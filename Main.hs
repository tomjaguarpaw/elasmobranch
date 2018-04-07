{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE PartialTypeSignatures     #-}
{-# LANGUAGE ExistentialQuantification #-}

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

doRepo mmap repoPath = Git.withClone repoPath $ \repo -> do
  putStrLn $ if Git.test
    then "Tests passed"
    else "OH NO MY TESTS FAILED!!!"

  branches <- Git.remoteBranches repo

  let branch_hashes = S.for (S.each branches) $ \branch -> do
        hash <- S.lift (Git.revParse repo branch)
        S.yield (branch, hash)

  bhm_ S.:> _ <- S.toList branch_hashes
  let bhm = S.each bhm_

  let checkit hash1 hash2 = let f = Git.status repo hash1 hash2 in
        case mmap of
          Nothing -> f
          Just map -> do
              -- This is not at all thread safe
                  map_ <- Data.IORef.readIORef map
                  case Data.Map.lookup (hash1, hash2) map_ of
                    Nothing -> do
                      putStrLn "Storing in the map"
                      exit <- f
                      Data.IORef.writeIORef map
                                            (Data.Map.insert (hash1, hash2)
                                                             exit
                                                             map_)
                      return exit
                    Just exit -> do
                      putStrLn "It's already in the map!"
                      return exit

  let branchpairs :: S.Stream (S.Of _) IO ()
      branchpairs =
        S.for bhm $ \(branch1, hash1) -> do
          S.for bhm $ \(branch2, hash2) -> do
            exit <- S.lift $ checkit hash1 hash2
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

doRepoString mmap path = do
  html <- doRepo mmap path
  l S.:> _ <- S.toList html
  return (concat l)

main :: IO ()
main = do
  mmap <- Data.IORef.newIORef Data.Map.empty
  Server.mainOn (doRepoString (Just mmap))
