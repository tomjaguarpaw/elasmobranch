{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE PartialTypeSignatures     #-}
{-# LANGUAGE ExistentialQuantification #-}

import           Control.Monad.Trans.Resource (runResourceT)
import qualified Data.Map
import qualified Streaming as S
import qualified Streaming.Prelude as S
import qualified Git
 
data Table a b = forall e. (Show e, Ord e)
               => Table (e -> a) (e -> a) [e] (Data.Map.Map (e, e) b)

data TableCell = TableCell { tcColor  :: String
                           , tcString :: String
                           }

tableToHtml :: Monad m
            => Table String TableCell
            -> S.Stream (S.Of String) m ()
tableToHtml (Table fleft ftop es m) = do
  table $ do
    row $ do
      cell (TableCell "white" "")
      S.for (S.each es) $ \top ->
        cell (TableCell "white" (ftop top))

    S.for (S.each es) $ \left -> do
      row $ do
        cell (TableCell "white" (fleft left))

        S.for (S.each es) $ \top -> do
          case Data.Map.lookup (top, left) m of
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

  branches <- fmap (take 5 . drop 1) (Git.remoteBranches repo)
  print branches

  let branch_hashes = S.for (S.each branches) $ \branch -> do
        hash <- S.lift (Git.revParse branch)
        S.yield (branch, hash)

  let branchpairs :: S.Stream (S.Of _) IO ()
      branchpairs =
        S.for branch_hashes $ \(branch1, hash1) -> do
          S.for branch_hashes $ \(branch2, hash2) -> do
            exit <- S.lift (Git.status repo hash1 hash2)

            S.yield ((branch1, branch2), exit)

  let result = branchpairs

  l S.:> _ <- S.toList result
  mapM_ print l

  let d = Data.Map.fromList l

      tc (Left Git.Conflicts)  = TableCell "#ff0000" "&nbsp;"
      tc (Left Git.Clean)  = TableCell "#ccff00" "&nbsp;"
      tc (Right _) = TableCell "#00ff00" "&nbsp;"

      table = Table (drop 7) (take 3 . drop 7) branches (fmap tc d)

      html = do
        S.yield "<html>"
        S.yield "<p>"
        tableToHtml table
        S.yield "</p>"
        S.yield "</html>"

  print d
  runResourceT (S.writeFile "/tmp/foo.html" html)


  return ()
