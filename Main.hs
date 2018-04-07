{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE PartialTypeSignatures     #-}
{-# LANGUAGE ExistentialQuantification #-}

import qualified Data.Ord

import           Control.Monad.Trans.Resource (runResourceT)
import           Control.Applicative (empty)
import           Data.Maybe (fromJust)
import qualified Data.Map
import qualified Data.List as L
import qualified Data.Traversable as T
import qualified System.IO.Temp
import qualified System.Process
import qualified System.Directory
import qualified System.Exit
import qualified Streaming as S
import qualified Streaming.Prelude as S
 
tempDirectory = System.IO.Temp.withSystemTempDirectory ""

proc x y = System.Process.readProcessWithExitCode x y ""

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
      cell "white" ""
      S.for (S.each es) $ \top ->
        cell "white" (ftop top)

    S.for (S.each es) $ \left -> do
      row $ do
        cell "white" (fleft left)

        S.for (S.each es) $ \top -> do
          case Data.Map.lookup (top, left) m of
                  Just (TableCell c t)  -> cell c t
                  Nothing -> error (show (top, left))

  where row s = S.yield "<tr>" >> s >> S.yield "</tr>"
        cell c s = S.yield ("<td style='background-color: "
                            ++ c ++ ";'>" ++ s ++ "</td>")
        table s = S.yield "<table>" >> s >> S.yield "</table>"

originBranches :: String -> [String]
originBranches out = tail (flip fmap (lines out) $ \originBranch -> drop 2 originBranch)

data Hash = Hash String deriving Show

-- FIXME: Check for error
revParse :: String -> IO Hash
revParse branch = do
  (_, out, _) <- proc "git" ["rev-parse", branch]
  return (Hash (take (length out - 1) out))

-- gitRebase x y
--
-- Rebase x onto y
canRebaseOnto :: Hash -> Hash -> IO _
canRebaseOnto (Hash hash) (Hash onto) = do
  (exit, out, err) <- proc "git" ["rebase", hash, onto]
  status <- case exit of
    System.Exit.ExitSuccess     -> return Clean
    System.Exit.ExitFailure 128 -> do
      proc "git" ["rebase", "--abort"]
      return Conflicts
    System.Exit.ExitFailure a   -> do
      error ("Did not expect git rebase to return " ++ show a)

  putStrLn out
  putStrLn err

  return status

isAncestorOf :: Hash -> Hash -> IO Bool
isAncestorOf (Hash potentialAncestor) (Hash potentialDescendant) = do
  (exitStatus, _, _) <- proc "git" [ "merge-base"
                                   , "--is-ancestor"
                                   , potentialAncestor
                                   , potentialDescendant
                                   ]
    
  return $ case exitStatus of
    System.Exit.ExitSuccess   -> True
    System.Exit.ExitFailure 1 -> False
    System.Exit.ExitFailure a ->
      error ("Didn't expect git merge-base --is-ancestor "
              ++ "to return " ++ show a)

compareHash :: Hash -> Hash -> IO (Maybe Ordering)
compareHash hash1 hash2 = do
  a <- hash1 `isAncestorOf` hash2
  b <- hash2 `isAncestorOf` hash1

  return $ case (a, b) of
    (True, True)   -> Just Data.Ord.EQ
    (True, False)  -> Just Data.Ord.GT
    (False, True)  -> Just Data.Ord.LT
    (False, False) -> Nothing


data RebaseStatus = Conflicts | Clean deriving Show

status :: Hash -> Hash -> IO (Either RebaseStatus Ordering)
status hash1 hash2 = do
  mord <- compareHash hash1 hash2

  case mord of
    Just ord -> return (Right ord)
    Nothing  -> fmap Left (canRebaseOnto hash1 hash2)

test :: Bool
test = originBranches "  origin/HEAD -> origin/master\n  origin/master\n  origin/partial-type-signatures"
       == ["origin/master", "origin/partial-type-signatures"]


main = tempDirectory $ \temp -> do
  putStrLn $ if test
    then "Tests passed"
    else "OH NO MY TESTS FAILED!!!"

  -- FIXME test for failure
  (_, _, _) <- proc "git" [ "clone"
--                          , "--depth=1"
--                          , "--no-single-branch"
--                          , "https://github.com/tomjaguarpaw/product-profunctors.git"
                          , "/home/tom/Haskell/haskell-opaleye"
                          , temp
                          ]

  System.Directory.setCurrentDirectory temp

  (_, out, err) <- proc "git" ["branch", "--remote"]
  putStrLn out
  putStrLn err

  let branches = take 4 (drop 1 (originBranches out))
  print branches

  let branch_hashes = S.for (S.each branches) $ \branch -> do
        hash <- S.lift (revParse branch)
        S.yield (branch, hash)

  let branchpairs :: S.Stream (S.Of _) IO ()
      branchpairs =
        S.for branch_hashes $ \(branch1, hash1) -> do
          S.for branch_hashes $ \(branch2, hash2) -> do
            exit <- S.lift (hash1 `status` hash2)

            S.yield ((branch1, branch2), exit)

  let result = branchpairs

  l S.:> _ <- S.toList result
  mapM_ print l

  let d = Data.Map.fromList l

      tc (Left Conflicts)  = TableCell "#ff0000" "&nbsp;"
      tc (Left Clean)  = TableCell "#ccff00" "&nbsp;"
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
