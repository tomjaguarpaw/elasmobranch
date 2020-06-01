{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE PartialTypeSignatures     #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE LambdaCase                #-}

module ElasmobranchMain (module ElasmobranchMain) where

import qualified Control.Concurrent
import qualified Data.IORef
import qualified Data.Ord
import qualified Data.Map
import qualified Streaming as S
import qualified Streaming.Prelude as S
import qualified System.Environment
import qualified System.IO
import qualified Git
import qualified Server

data Table a b = forall e f. (Show e, Show f, Ord e, Ord f)
               => Table (e -> a) (f -> a) [e] [f] (Data.Map.Map (e, f) b)

data TableCell = TableCell { tcColor  :: String
                           , tcString :: String
                           }

emptyTableCell :: CompareResult -> TableCell
emptyTableCell x = TableCell (color x) "&nbsp;"

tag :: Monad m
    => String
    -> S.Stream (S.Of String) m a
    -> S.Stream (S.Of String) m ()
tag s x = S.yield ("<" ++ s ++ ">") >> x >> S.yield ("</" ++ s ++ ">")

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

  where row = tag "tr"
        cell tc = S.yield ("<td style='background-color: "
                            ++ tcColor tc ++ "; text-align: right'>"
                            ++ tcString tc ++ "</td>")
        table = tag "table"

type CompareHashes r = Git.Repo -> (Git.Hash, Git.Hash) -> IO r
type CompareHashes' = CompareHashes CompareResult
type CompareResult = Either (Git.RebaseStatus, Git.MergeStatus) Ordering

doRepo :: _
       -> CompareHashes'
       -> (Status -> IO a)
       -> String
       -> IO (S.Stream (S.Of String) IO ())
doRepo originBranchHashes mmap statusTyped repoPath = do
  statusTyped Cloning
  Git.withClone repoPath $ \result -> case result of
    Left err   -> return (S.yield ("Couldn't clone " ++ repoPath))
    Right repo -> doRepoSuccess originBranchHashes mmap statusTyped repo

doRepoSuccess :: _
              -> CompareHashes'
              -> (Status -> IO a)
              -> Git.Repo
              -> IO (S.Stream (S.Of String) IO ())
doRepoSuccess originBranchHashes mmap statusTyped repo = do
  bhm_ <- originBranchHashes repo
  t <- branchPairsFromHashes mmap statusTyped repo bhm_
  produceTable t

branchPairsFromHashes :: Ord branch
                      => CompareHashes r
                      -> (Status -> IO a)
                      -> Git.Repo
                      -> [(branch, Git.Hash)]
                      -> IO ([branch], Data.Map.Map (branch, branch) r)
branchPairsFromHashes checkit emitStatus repo bhm_ = do
  let bhm = S.each bhm_

  let branchpairs :: S.Stream (S.Of _) IO ()
      branchpairs = do
        let totalRebasesToDo = length bhm_ * length bhm_
        count <- S.lift (Data.IORef.newIORef 0)
        S.lift (emitStatus (CompletedRebasing 0 totalRebasesToDo))
        S.for bhm $ \(branch1, hash1) -> do
          S.for bhm $ \(branch2, hash2) -> do
            result <- S.lift $ do
              result <- checkit repo (hash1, hash2)
              Data.IORef.modifyIORef count (+1)
              soFar <- Data.IORef.readIORef count
              emitStatus (CompletedRebasing soFar totalRebasesToDo)
              return result
            S.yield ((branch1, branch2), result)

  l <- S.toList_ branchpairs

  let d = Data.Map.fromList l

  return (map fst bhm_, d)

branchPairs :: (Git.Repo -> (Git.Hash, Git.Hash) -> IO r)
            -> (Status -> IO a)
            -> Git.Repo
            -> IO ([Git.Branch], Data.Map.Map (Git.Branch, Git.Branch) r)
branchPairs checkit emitStatus repo = do
  bhm_ <- Git.originBranchHashes repo
  branchPairsFromHashes checkit emitStatus repo bhm_

tableKey :: Table String TableCell
tableKey = Table statusString
                 (const "Left to top")
                 allOfThem
                 [()]
                 (Data.Map.fromList
                   (map (\s -> ((s, ()), emptyTableCell s))
                        allOfThem))
  where allOfThem :: [CompareResult]
        allOfThem = S.runIdentity $ S.toList_ $ do
          S.for streamAll $ \i ->
            S.for streamAll $ \j ->
              S.yield (Left (i, j))
          S.for streamAll $ \i ->
            S.yield (Right i)

        streamAll :: (Monad m, Bounded a, Enum a) => S.Stream (S.Of a) m ()
        streamAll = S.each [minBound..maxBound]

statusString :: CompareResult -> String
statusString = \case
  Left (Git.Conflicts, Git.MConflicts) -> "Merge and rebase conflict"
  Left (Git.Conflicts, Git.MClean)     -> "Rebase conflict"
  Left (Git.Clean, Git.MConflicts)     -> "Merge conflict"
  Left (Git.Clean, Git.MClean)         -> "No conflict"
  Right Data.Ord.GT  -> "Behind"
  Right Data.Ord.LT  -> "Ahead of"
  Right Data.Ord.EQ  -> "Equal to"

data SimpleColor = Red
                 | Orange
                 | Yellow
                 | Green
                 | Grey
                 | White

simpleColorToHtml :: SimpleColor -> String
simpleColorToHtml = \case
  Red    -> "#ff0000"
  Orange -> "#ffcc00"
  Yellow -> "#ccff00"
  Green  -> "#00ff00"
  Grey   -> "#cccccc"
  White  -> "#ffffff"

color :: CompareResult -> String
color = simpleColorToHtml . \case
 Left (Git.Conflicts, Git.MConflicts) -> Red
 Left (Git.Conflicts, Git.MClean)     -> Orange
 Left (Git.Clean, Git.MConflicts)     -> Orange
 Left (Git.Clean, Git.MClean)         -> Yellow

 Right Data.Ord.GT  -> Grey
 Right Data.Ord.LT  -> Green
 Right Data.Ord.EQ  -> White

data Sign = Wastebasket
          | CrossMark
          | WarningSign

signToHtml :: Sign -> String
signToHtml = \case
  Wastebasket -> "&#x1f5d1;"
  CrossMark   -> "&#x274c;"
  WarningSign -> "&#x26a0;"

warning :: CompareResult -> Maybe (Sign, String -> String)
warning = \case
  Right Data.Ord.GT ->
    Just (Wastebasket, (++ " is behind master"))
  Right Data.Ord.EQ -> Nothing
  Right Data.Ord.LT -> Nothing
  Left (Git.Conflicts, Git.MConflicts) ->
    Just (CrossMark, (++ " conflicts with master"))
  Left (Git.Clean, Git.MConflicts) ->
    Just (CrossMark, (++ " rebases cleanly on master but does not merge"))
  Left (Git.Conflicts, Git.MClean) ->
    Just (CrossMark, (++ " merges cleanly into master but does not rebase"))
  Left (Git.Clean, Git.MClean) ->
    Just (WarningSign,
          (++ " merges cleanly into and rebases cleanly onto master"))

produceTable :: ([Git.Branch],
                 Data.Map.Map (Git.Branch, Git.Branch) CompareResult)
             -> IO (S.Stream (S.Of String) IO ())
produceTable (branches, d) = do
  let master = Git.Branch "origin/master"

      table = Table (drop 7 . Git.branchName)
                    (take 3 . drop 7 . Git.branchName)
                    branchesWithMasterFirst
                    branchesWithMasterFirst
                    (fmap emptyTableCell d)
        where branchesWithMasterFirst = master:branchesNotMaster
              branchesNotMaster = filter (/= master) branches

      ulOfWarnings = do
        S.yield "<ul>"
        S.for (S.each branches) $ \branch ->
          let key = (branch, master)
              li (sym, s) = "<li>" ++ signToHtml sym
                            ++ " &mdash; " ++ s ++ "</li>"

          in case Data.Map.lookup key d of
            Nothing -> error ("Couldn't find key " ++ show key ++ " in "
                              ++ show (Data.Map.keys d))
            Just r  -> S.for (S.each (warning r)) $ \(warningSymbol, message) ->
              S.yield (li (warningSymbol, message (Git.branchName branch)))
        S.yield "</ul>"

      html = tag "html" $ do
        tag "head" (tag "title" (S.yield "elasmobranch"))
        tag "body" $ do
          S.yield submitAnother
          ulOfWarnings
          tag "p" (tableToHtml table)
          tag "p" (tableToHtml tableKey)
        S.yield submitAnother

  return html

data Status = Cloning
            | CompletedRebasing Int Int
            deriving Show

sendStatus :: Show threadId
           => Data.IORef.IORef (Data.Map.Map String status)
           -> threadId
           -> status
           -> IO ()
sendStatus tmap myThreadId message =
  Data.IORef.modifyIORef tmap
                         (Data.Map.insert (show myThreadId) message)

readStatus :: Ord threadId
           => Data.IORef.IORef (Data.Map.Map threadId status)
           -> threadId
           -> IO (Maybe status)
readStatus tmap threadId = fmap (Data.Map.lookup threadId) (Data.IORef.readIORef tmap)

statusMessage :: Status -> String
statusMessage = \case
  CompletedRebasing n total -> show n ++ "/" ++ show total ++ " rebases done"
  Cloning -> "I am cloning the repo"

doRepoString :: CompareHashes'
             -> (Control.Concurrent.ThreadId -> Either Status String -> IO ())
             -> String
             -> IO String
doRepoString mmap sendStatustmap path = do
  threadId <- Control.Concurrent.forkIO $ do
    myThreadId <- Control.Concurrent.myThreadId

    let status = sendStatustmap myThreadId
        statusTyped = status . Left

    htmlStrings <- doRepo Git.originBranchHashes mmap statusTyped path
    htmlStringsList <- S.toList_ htmlStrings

    status (Right (concat htmlStringsList))

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

submitAnother :: String
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
                        ++ "<p>elasmobranch says \"" ++  statusMessage s ++ "\"</p>"
                        ++ "</body></html>")
    Just (Right html) -> return html

main :: IO ()
main = do
  mmap <- Data.IORef.newIORef Data.Map.empty
  tmap <- Data.IORef.newIORef Data.Map.empty

  let checkit :: CompareHashes'
      checkit = memoize mmap . uncurry . Git.status

  let sendStatus_ = sendStatus tmap
      readStatus_ = readStatus tmap

  Server.mainOn (doRepoString checkit sendStatus_) (doThread readStatus_)

mainLocal :: IO ()
mainLocal = do
  args <- System.Environment.getArgs

  let (repo, outfile) = case args of
        [repo, outfile] -> (repo, outfile)
        _               ->
          error "Need two args: repo to clone and file to write to"

  let originBranchHashes _ = Git.originBranchHashes (Git.Repo repo)

  html <- doRepo originBranchHashes
                 (\r -> uncurry (Git.status r))
                 (\r -> print r >> System.IO.hFlush System.IO.stdout)
                 repo

  let writeFile :: FilePath -> S.Stream (S.Of String) IO r -> IO r
      writeFile f = System.IO.withFile f System.IO.WriteMode . flip S.toHandle

  writeFile outfile html

mainCommandLine :: IO ()
mainCommandLine = do
  args <- System.Environment.getArgs

  putStrLn "Hello, my name is elasmobranch panic.  I'm here to help you."
  putStrLn "Don't worry!  We'll get out of this situation."
  putStrLn ""

  let directory :: Either String FilePath
      directory = case args of
        []    -> return  "."
        [dir] -> return dir
        _     -> Left ("Give me zero arguments, or one argument which is the "
                       ++ "directory of the git repo in question")

  inProgress <- traverse what'sInProgressAtFilePath directory
  let _ = inProgress :: Either String (Maybe (Maybe (Maybe Git.InProgress)))

  putStrLn $ case inProgress of
    Right x  -> radMessage x
    Left err -> err

what'sInProgressAtFilePath
  :: FilePath
  -> IO (Maybe (Maybe (Maybe Git.InProgress)))
what'sInProgressAtFilePath filepath = do
  mRepo <- Git.repoAtDirectory filepath
  traverse what'sInProgress mRepo
  where what'sInProgress :: Git.RepoAtDirectory
                         -> IO (Maybe (Maybe Git.InProgress))
        what'sInProgress = \case
          Git.RADRepo _         -> return Nothing
          Git.RADRepoDirty repo -> do
            wip <- Git.what'sInProgress repo
            return (Just wip)

radMessage :: Maybe (Maybe (Maybe Git.InProgress)) -> String
radMessage = \case
  Nothing               -> "There's no git repo there"
  Just Nothing          -> "It's a clean repo!"
  Just (Just Nothing)   ->
    "I guess you've got some normal changes.\n"
    ++ "I don't see anything wrong here\n"
    ++ "and I'm not trained to help you further."
  Just (Just (Just ip)) -> inProgressMessage ip

inProgressMessage :: Git.InProgress -> String
inProgressMessage = \case
  Git.IPRebase   ->
   "You're in a rebase conflict.  If you want to abort it do\n\n"
    ++ "    git rebase --abort"
  Git.IPMerge    ->
   "You're in a merge conflict. If you want to abort it do\n\n"
    ++ "    git merge --abort"
  Git.IPStashPop ->
    "You're in a stash pop conflict. If you want to abort it try\n\n"
    ++ "    git reset --merge\n\nIf that doesn't work then do\n\n"
    ++ "    git reset HEAD\n\nand then to remove the popped changes "
    ++ "from your working copy do\n\n    git checkout --patch\n\n"
    ++ "(The popped changes still exist in the stash)\n"
    ++ "If you have resolved the conflict and want to continue working\n"
    ++ "without committing these changes then 'git reset HEAD' is also\n"
    ++ "the correct thing to do."
  Git.IPCherryPick ->
    "You're in a cherry-pick conflict. If you want to abort it do\n\n"
    ++ "    git cherry-pick --abort"

-- This is not at all thread safe
memoize :: Ord t
        => Data.IORef.IORef (Data.Map.Map t a)
        -> (t -> IO a)
        -> t -> IO a
memoize memomap f x = do
  map_ <- Data.IORef.readIORef memomap
  case Data.Map.lookup x map_ of
    Nothing -> do
      exit <- f x
      Data.IORef.writeIORef memomap (Data.Map.insert x exit map_)
      return exit
    Just exit -> do
      return exit
