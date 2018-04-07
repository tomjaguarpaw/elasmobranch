{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE PartialTypeSignatures     #-}
{-# LANGUAGE ExistentialQuantification #-}

import qualified Data.Ord
import           Control.Monad.Trans.Resource (runResourceT)
import qualified Data.Map
import qualified Streaming as S
import qualified Streaming.Prelude as S
import qualified Git
 
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

main :: IO ()
main = Git.withClone "/home/tom/Haskell/haskell-opaleye" $ \repo -> do
  putStrLn $ if Git.test
    then "Tests passed"
    else "OH NO MY TESTS FAILED!!!"

  branches <- fmap (take 8 . drop 1) (Git.remoteBranches repo)
  print branches

  let branch_hashes = S.for (S.each branches) $ \branch -> do
        hash <- S.lift (Git.revParse repo branch)
        S.lift (print (branch, hash))
        S.yield (branch, hash)

  bhm_ S.:> _ <- S.toList branch_hashes
  let bhm = S.each bhm_

  let branchpairs :: S.Stream (S.Of _) IO ()
      branchpairs =
        S.for bhm $ \(branch1, hash1) -> do
          S.for bhm $ \(branch2, hash2) -> do
            exit <- S.lift (Git.status repo hash1 hash2)

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
          case Data.Map.lookup (branch, "origin/master") d of
            Nothing -> error ("Couldn't find branch " ++ branch)
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
        list
        S.yield "<p>"
        tableToHtml table
        S.yield "</p>"
        S.yield "</html>"

  print d
  runResourceT (S.writeFile "/tmp/foo.html" html)


  return ()
