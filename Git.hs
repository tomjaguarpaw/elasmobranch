module Git where

import qualified Data.Ord
import qualified System.IO.Temp
import qualified System.Process
import qualified System.Exit

-- Utils

proc :: FilePath
     -> [String]
     -> Maybe FilePath
     -> IO (System.Exit.ExitCode, String, String)
proc program arguments dir =
  System.Process.readCreateProcessWithExitCode createProcess stdin
  where createProcess = (System.Process.proc program arguments)
                          { System.Process.cwd = dir }
        stdin = ""

-- Git

data Hash = Hash String deriving (Show, Eq, Ord)
data Repo = Repo String deriving Show

-- FIXME test for failure
withClone :: String
          -> (Either String Repo -> IO a)
          -> IO a
withClone repo f = do
  System.IO.Temp.withSystemTempDirectory "" $ \temp -> do
    (exitCode, _, err) <- proc "git" [ "clone"
--                          , "--depth=1"
--                          , "--no-single-branch"
-- If we shallow clone then we need to
--         git fetch --unshallow origin master
                                     , repo
                                     , temp
                                     ]
                            Nothing
    case exitCode of
      System.Exit.ExitFailure _ -> f (Left err)
      System.Exit.ExitSuccess   -> f (Right (Repo temp))

remoteBranches :: Repo -> IO [String]
remoteBranches (Repo repo) = do
  (_, out, _) <- proc "git" ["branch", "--remote"] (Just repo)
  return (originBranches out)

originBranches :: String -> [String]
originBranches out = tail (flip fmap (lines out) $ \originBranch -> drop 2 originBranch)

-- FIXME: Check for error
revParse :: Repo -> String -> IO Hash
revParse (Repo repo) branch = do
  (exit, out, err) <- proc "git" ["rev-parse", branch] (Just repo)
  case exit of
    System.Exit.ExitSuccess   -> return (Hash (take (length out - 1) out))
    System.Exit.ExitFailure _ -> error err

status :: Repo -> Hash -> Hash -> IO (Either RebaseStatus Ordering)
status repo hash1 hash2 = do
  mord <- compareHash repo hash1 hash2

  case mord of
    Just ord -> return (Right ord)
    Nothing  -> fmap Left (canRebaseOnto repo hash1 hash2)

-- gitRebase hash x y
--
-- Rebase x onto y
canRebaseOnto :: Repo -> Hash -> Hash -> IO RebaseStatus
canRebaseOnto (Repo repo) (Hash hash) (Hash onto) = do
  (exit, _, _) <- proc "git" ["rebase", onto, hash] (Just repo)
  status_ <- case exit of
    System.Exit.ExitSuccess     -> return Clean
    System.Exit.ExitFailure 128 -> do
      _ <- proc "git" ["rebase", "--abort"] (Just repo)
      return Conflicts
    System.Exit.ExitFailure a   -> do
      error ("Did not expect git rebase to return " ++ show a)

  return status_

isAncestorOf :: Repo -> Hash -> Hash -> IO Bool
isAncestorOf (Repo repo) (Hash potentialAncestor) (Hash potentialDescendant) = do
  (exitStatus, _, err) <- proc "git" [ "merge-base"
                                     , "--is-ancestor"
                                     , potentialAncestor
                                     , potentialDescendant
                                     ]
                                     (Just repo)
    
  return $ case exitStatus of
    System.Exit.ExitSuccess   -> True
    System.Exit.ExitFailure 1 -> False
    System.Exit.ExitFailure a ->
      error ("Didn't expect git merge-base --is-ancestor "
              ++ "to return " ++ show a ++ "\n" ++ err)

compareHash :: Repo -> Hash -> Hash -> IO (Maybe Ordering)
compareHash repo hash1 hash2 = do
  a <- hash1 `isAncestorOf_` hash2
  b <- hash2 `isAncestorOf_` hash1

  return $ case (a, b) of
    (True, True)   -> Just Data.Ord.EQ
    (True, False)  -> Just Data.Ord.GT
    (False, True)  -> Just Data.Ord.LT
    (False, False) -> Nothing
  where isAncestorOf_ = isAncestorOf repo


data RebaseStatus = Conflicts | Clean deriving Show

-- Test

test :: Bool
test = originBranches "  origin/HEAD -> origin/master\n  origin/master\n  origin/partial-type-signatures"
       == ["origin/master", "origin/partial-type-signatures"]
