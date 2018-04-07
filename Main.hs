{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE PartialTypeSignatures     #-}
{-# LANGUAGE ExistentialQuantification #-}

import qualified Control.Concurrent
import qualified Data.IORef
import qualified Data.Ord
import           Control.Monad.Trans.Resource (runResourceT)
import qualified Data.Map
import qualified Streaming as S
import qualified Streaming.Prelude as S
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
                            ++ tcColor tc ++ ";'>" ++ tcString tc ++ "</td>")
        table s = S.yield "<table>" >> s >> S.yield "</table>"

-- "https://github.com/tomjaguarpaw/product-profunctors.git"

doRepo :: Monad m
  => Maybe _
  -> String
  -> IO (S.Stream (S.Of [Char]) m ())
doRepo mmap repoPath = Git.withClone repoPath $ \repo -> do
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

  let branchpairs :: S.Stream (S.Of _) IO ()
      branchpairs =
        S.for bhm $ \(branch1, hash1) -> do
          S.for bhm $ \(branch2, hash2) -> do
            exit <- S.lift $ checkit (hash1, hash2)
            S.yield ((branch1, branch2), exit)

  let result = branchpairs

  l S.:> _ <- S.toList result

  let d = Data.Map.fromList l

      red    = "#ff0000"
      yellow = "#ccff00"
      green  = "#00ff00"
      grey   = "#cccccc"
      white  = "#ffffff"

      tc (Left Git.Conflicts) = TableCell red "&nbsp;"
      tc (Left Git.Clean)     = TableCell yellow "&nbsp;"
      tc (Right Data.Ord.GT)  = TableCell grey "&nbsp;"
      tc (Right Data.Ord.LT)  = TableCell green "&nbsp;"
      tc (Right Data.Ord.EQ)  = TableCell white "&nbsp;"

      table = Table (drop 7)
                    (take 3 . drop 7)
                    branches
                    ("origin/master":branches)
                    (fmap tc d)

      list = do
        S.yield "<ul>"
        S.for (S.each branches) $ \branch ->
          let key = (branch, "origin/master")
          in case Data.Map.lookup key d of
            Nothing -> error ("Couldn't find key " ++ show key ++ " in "
                              ++ show (Data.Map.keys d))
            Just r  -> case r of
              Right _ -> return ()
              Left Git.Conflicts -> S.yield ("<li>"
                                             ++ "&#x274c;"
                                             ++ " &mdash; "
                                             ++ branch
                                             ++ " conflicts with master"
                                             ++ "</li>")
              Left Git.Clean     -> S.yield ("<li>"
                                             ++ "&#x26a0;"
                                             ++ " &mdash; "
                                             ++ branch
                                             ++ " rebases cleanly on master"
                                             ++ "</li>")
        S.yield "</ul>"

      html = do
        S.yield "<html>"
        S.yield "<body>"
        list
        S.yield "<p>"
        tableToHtml table
        S.yield "</p>"
        S.yield "</body></html>"

  return html

mainOld :: IO ()
mainOld = do
  html <- doRepo Nothing "file:///home/tom/Haskell/haskell-opaleye"
  runResourceT (S.writeFile "/tmp/foo.html" html)

doRepoString mmap tmap path = do
  threadId <- Control.Concurrent.forkIO $ do
    myThreadId <- Control.Concurrent.myThreadId

    Data.IORef.modifyIORef tmap (Data.Map.insert (show myThreadId) (Left ()))

    html <- doRepo (Just mmap) path
    l S.:> _ <- S.toList html
    let htmlString = concat l

    Data.IORef.modifyIORef tmap (Data.Map.insert (show myThreadId) (Right htmlString))

  return ("<html><head><title>elasmobranch: Link to your report</title></head>"
          ++ "<body><p>The report for <code>"
          ++ path
          ++ "</code>"
          ++ " will appear at <a href='/thread?thread="
          ++ show threadId
          ++ "'>" ++ show threadId ++ "</a>"
          ++ "</p></body></html>")

doThread tmap threadId = do
  mhtml <- fmap (Data.Map.lookup threadId) (Data.IORef.readIORef tmap)
  case mhtml of
    Nothing -> return ("<html>"
                       ++ "<head><title>Invalid report ID</title></head>"
                       ++ "<body>"
                       ++ "<p>This doesn't appear to be a valid report ID</p>"
                       ++ "</body></html>")
    Just (Left ()) -> return
                       ("<html>"
                        ++ "<head><meta http-equiv='refresh' content='5' >"
                        ++ "<title>Waiting for report to be generated</title></head>"
                        ++ "<body>"
                        ++ "<p>I haven't finished generating your report yet. "
                        ++ "I'll refresh every 5 seconds to check for it "
                        ++ "or you can do that manually.</p>"
                        ++ "</body></html>")
    Just (Right html) -> return html

main :: IO ()
main = do
  mmap <- Data.IORef.newIORef Data.Map.empty
  tmap <- Data.IORef.newIORef Data.Map.empty
  Server.mainOn (doRepoString mmap tmap) (doThread tmap)

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
