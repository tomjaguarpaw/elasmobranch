{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE PartialTypeSignatures     #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE LambdaCase                #-}

module Main (module Main) where

import qualified Control.Concurrent
import qualified Data.IORef
import qualified Data.Ord
import           Control.Monad.Trans.Resource (runResourceT)
import qualified Data.Map
import qualified Streaming as S
import qualified Streaming.Prelude as S
import qualified System.Environment
import qualified Git
import qualified Server
 
data Table a b = forall e f. (Show e, Show f, Ord e, Ord f)
               => Table (e -> a) (f -> a) [e] [f] (Data.Map.Map (e, f) b)

data TableCell = TableCell { tcColor  :: String
                           , tcString :: String
                           }

tableToHtml :: Monad m
            => Table String TableCell
            -> S.Stream (S.Of String) m ()
tableToHtml (Table fleft ftop lefts tops m) = do
  table $ do
    row $ do
      cell (TableCell "white" "")
      S.for (S.each tops) $ \top ->
        cell (TableCell "white" (ftop top))

    S.for (S.each lefts) $ \left -> do
      row $ do
        cell (TableCell "white" (fleft left))

        S.for (S.each tops) $ \top -> do
          case Data.Map.lookup (left, top) m of
                  Just tc  -> cell tc
                  Nothing -> error (show (top, left))

  where row s = S.yield "<tr>" >> s >> S.yield "</tr>"
        cell tc = S.yield ("<td style='background-color: "
                            ++ tcColor tc ++ "; text-align: right'>"
                            ++ tcString tc ++ "</td>")
        table s = S.yield "<table>" >> s >> S.yield "</table>"

-- "https://github.com/tomjaguarpaw/product-profunctors.git"

doRepo mmap statusTyped repoPath = Git.withClone repoPath $ \result -> case result of
  Left  err  -> do
    return (S.yield ("Couldn't clone " ++ repoPath))
  Right repo -> doRepoSuccess mmap statusTyped repo

branchPairs :: _ -> _ -> Git.Repo -> _
branchPairs mmap statusTyped repo = do
  branches <- Git.remoteBranches repo

  let branch_hashes = S.for (S.each branches) $ \branch -> do
        hash <- S.lift (Git.revParse repo branch)
        S.yield (branch, hash)

  bhm_ S.:> _ <- S.toList branch_hashes
  let bhm = S.each bhm_

  let checkit = (case mmap of
        Nothing   -> id
        Just mmap -> memoize mmap)
          (uncurry (Git.status repo))

  let totalRebasesToDo = length bhm_ * length bhm_
  count <- Data.IORef.newIORef 0

  let branchpairs :: S.Stream (S.Of _) IO ()
      branchpairs =
        S.for bhm $ \(branch1, hash1) -> do
          S.for bhm $ \(branch2, hash2) -> do
            exit <- S.lift $ do
              exit <- checkit (hash1, hash2)
              Data.IORef.modifyIORef count (+1)
              soFar <- Data.IORef.readIORef count
              statusTyped (CompletedRebasing soFar totalRebasesToDo)
              return exit
            S.yield ((branch1, branch2), exit)

  statusTyped (CompletedRebasing 0 totalRebasesToDo)
  l S.:> _ <- S.toList branchpairs

  let d = Data.Map.fromList l

  return (branches, d)

doRepoSuccess mmap statusTyped repo = do
  (branches, d) <- branchPairs mmap statusTyped repo

  let red    = "#ff0000"
      yellow = "#ccff00"
      green  = "#00ff00"
      grey   = "#cccccc"
      white  = "#ffffff"

      wastebasket  = "&#x1f5d1;"
      cross_mark   = "&#x274c;"
      warning_sign = "&#x26a0;"

      tc (Left Git.Conflicts) = TableCell red "&nbsp;"
      tc (Left Git.Clean)     = TableCell yellow "&nbsp;"
      tc (Right Data.Ord.GT)  = TableCell grey "&nbsp;"
      tc (Right Data.Ord.LT)  = TableCell green "&nbsp;"
      tc (Right Data.Ord.EQ)  = TableCell white "&nbsp;"

      table = Table (drop 7)
                    (take 3 . drop 7)
                    branchesWithMasterFirst
                    branchesWithMasterFirst
                    (fmap tc d)
        where branchesWithMasterFirst = master:branchesNotMaster
              master = "origin/master"
              branchesNotMaster = filter (/= master) branches

      list = do
        S.yield "<ul>"
        S.for (S.each branches) $ \branch ->
          let key = (branch, "origin/master")
              li (sym, s) = "<li>" ++ sym ++ " &mdash; " ++ s ++ "</li>"

          in case Data.Map.lookup key d of
            Nothing -> error ("Couldn't find key " ++ show key ++ " in "
                              ++ show (Data.Map.keys d))
            Just r  -> traverse (S.yield . li) $ case r of
              Right Data.Ord.GT ->
                Just (wastebasket, branch ++ " is behind master")
              Right Data.Ord.EQ -> Nothing
              Right Data.Ord.LT -> Nothing
              Left Git.Conflicts ->
                Just (cross_mark, branch ++ " conflicts with master")
              Left Git.Clean     ->
                Just (warning_sign, branch ++ " rebases cleanly on master")
        S.yield "</ul>"

      html = do
        S.yield "<html>"
        S.yield "<head><title>elasmobranch</title></head>"
        S.yield "<body>"
        S.yield submitAnother
        list
        S.yield "<p>"
        tableToHtml table
        S.yield "</p>"
        S.yield submitAnother
        S.yield "</body></html>"

  return html

mainOld :: IO ()
mainOld = do
  html <- doRepo Nothing
                 (const (return ()))
                 "file:///home/tom/Haskell/haskell-opaleye"
  runResourceT (S.writeFile "/tmp/foo.html" html)

data Status = Cloning
            | CompletedRebasing Int Int

sendStatus tmap myThreadId message =
  Data.IORef.modifyIORef tmap
                         (Data.Map.insert (show myThreadId) message)

readStatus tmap threadId = fmap (Data.Map.lookup threadId) (Data.IORef.readIORef tmap)

doRepoString mmap sendStatustmap path = do
  threadId <- Control.Concurrent.forkIO $ do
    myThreadId <- Control.Concurrent.myThreadId

    let status = sendStatustmap myThreadId

    let statusTyped = status . Left . \case
          CompletedRebasing n total ->
            show n ++ "/" ++ show total ++ " rebases done"
          Cloning ->
            "I am cloning the repo"

    statusTyped Cloning
    html <- doRepo (Just mmap) statusTyped path
    l S.:> _ <- S.toList html
    let htmlString = concat l

    status (Right htmlString)

  return ("<html><head><title>elasmobranch: Link to your report</title></head>"
          ++ "<body>"
          ++ submitAnother
          ++ "<p>The report for <code>"
          ++ path
          ++ "</code>"
          ++ " will appear at <a href='/thread?thread="
          ++ show threadId
          ++ "'>" ++ show threadId ++ "</a>"
          ++ "</p>"
          ++ "</body></html>")

submitAnother = "<p><a href='/'>&#x25c0; Submit another repo</a></p>"

doThread readStatus_ threadId = do
  mhtml <- readStatus_ threadId
  case mhtml of
    Nothing -> return ("<html>"
                       ++ "<head><title>Invalid report ID</title></head>"
                       ++ "<body>"
                       ++ submitAnother
                       ++ "<p>This doesn't appear to be a valid report ID</p>"
                       ++ "</body></html>")
    Just (Left s) -> return
                       ("<html>"
                        ++ "<head><meta http-equiv='refresh' content='5' >"
                        ++ "<title>elasmobranch: Waiting for report to be generated</title></head>"
                        ++ "<body>"
                        ++ submitAnother
                        ++ "<p>I haven't finished generating your report yet. "
                        ++ "I'll refresh every 5 seconds to check for it "
                        ++ "or you can do that manually.</p>"
                        ++ "<p>elasmobranch says \"" ++ s ++ "\"</p>"
                        ++ "</body></html>")
    Just (Right html) -> return html

main :: IO ()
main = do
  mmap <- Data.IORef.newIORef Data.Map.empty
  tmap <- Data.IORef.newIORef Data.Map.empty

  let sendStatus_ = sendStatus tmap
      readStatus_ = readStatus tmap

  Server.mainOn (doRepoString mmap sendStatus_) (doThread readStatus_)

mainCommandLine :: IO ()
mainCommandLine = do
  args <- System.Environment.getArgs

  putStrLn "Hello, my name is elasmobranch panic.  I'm here to help you."
  putStrLn "Don't worry!  We'll get out of this situation."
  putStrLn ""

  let directory = case args of
        []    -> return  "."
        [dir] -> return dir
        _     -> Left ("Give me zero arguments, or one argument which is the "
                       ++ "directory of the git repo in question")

  situation <- traverse Git.repoAtDirectory directory

  case situation of
    Right mRepo -> do
      case mRepo of
       Nothing   -> putStrLn "There's no git repo there"
       Just (Git.RADRepo _) -> putStrLn "It's a clean repo!"
       Just (Git.RADRepoDirty repo) -> do
         wip <- Git.what'sInProgress repo
         putStrLn $ case wip of
           Nothing         -> ("I guess you've got some normal changes. "
                               ++ "I don't see anything wrong here "
                               ++ "and I'm not trained to help you further.")
           Just Git.IPRebase   -> ("You're in a rebase conflict. "
                                   ++ "If you want to abort it do\n\n"
                                   ++ "    git rebase --abort")
           Just Git.IPMerge    -> ("You're in a merge conflict. "
                                   ++ "If you want to abort it do\n\n"
                                   ++ "    git merge --abort")
           Just Git.IPStashPop -> ("In a stash pop conflict. "
                                   ++ "If you want to abort it do\n\n"
                                   ++ "    git reset HEAD\n\n"
                                   ++ "and then to remove the popped changes "
                                   ++ "from your working copy do\n\n"
                                   ++ "    git checkout --patch\n\n"
                                   ++ "(The popped changes still exist "
                                   ++ "in the stash)")
           Just Git.IPCherryPick -> ("You're in a cherry-pick conflict. "
                                    ++ "If you want to abort it do\n\n"
                                    ++ "    git cherry-pick --abort")
    Left err -> putStrLn err

-- This is not at all thread safe
memoize :: Ord t
        => Data.IORef.IORef (Data.Map.Map t a)
        -> (t -> IO a)
        -> t -> IO a
memoize memomap f x = do
  map_ <- Data.IORef.readIORef memomap
  case Data.Map.lookup x map_ of
    Nothing -> do
      putStrLn "New"
      exit <- f x
      Data.IORef.writeIORef memomap (Data.Map.insert x exit map_)
      return exit
    Just exit -> do
      putStrLn "Existing"
      return exit
