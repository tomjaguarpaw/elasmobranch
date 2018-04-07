module Git where

import qualified Data.Ord
import qualified System.IO.Temp
import qualified System.Process
import qualified System.Directory
import qualified System.Exit

-- Utils

proc :: FilePath
     -> [String]
     -> IO (System.Exit.ExitCode, String, String)
proc x y = System.Process.readProcessWithExitCode x y ""

-- Git

data Hash = Hash String deriving Show
data Repo = Repo String deriving Show

-- FIXME test for failure
withClone :: String
          -> (Repo -> IO a)
          -> IO a
withClone repo f = do
  System.IO.Temp.withSystemTempDirectory "" $ \temp -> do
    (_, _, _) <- proc "git" [ "clone"
--                          , "--depth=1"
--                          , "--no-single-branch"
                            , repo
                            , temp
                            ]
    f (Repo temp)

remoteBranches :: Repo -> IO [String]
remoteBranches (Repo repo) =
  System.Directory.withCurrentDirectory repo $ do
    (_, out, _) <- proc "git" ["branch", "--remote"]
    return (originBranches out)

originBranches :: String -> [String]
originBranches out = tail (flip fmap (lines out) $ \originBranch -> drop 2 originBranch)

-- FIXME: Check for error
revParse :: Repo -> String -> IO Hash
revParse (Repo repo) branch = do
  System.Directory.withCurrentDirectory repo $ do
  (exit, out, err) <- proc "git" ["rev-parse", branch]
  case exit of
    System.Exit.ExitSuccess   -> return (Hash (take (length out - 1) out))
    System.Exit.ExitFailure _ -> error err

status :: Repo -> Hash -> Hash -> IO (Either RebaseStatus Ordering)
status repo hash1 hash2 = do
  mord <- compareHash repo hash1 hash2

  case mord of
    Just ord -> return (Right ord)
    Nothing  -> fmap Left (canRebaseOnto repo hash1 hash2)

-- gitRebase x y
--
-- Rebase x onto y
canRebaseOnto :: Repo -> Hash -> Hash -> IO RebaseStatus
canRebaseOnto (Repo repo) (Hash hash) (Hash onto) =
  System.Directory.withCurrentDirectory repo $ do
  (exit, out, err) <- proc "git" ["rebase", hash, onto]
  status_ <- case exit of
    System.Exit.ExitSuccess     -> return Clean
    System.Exit.ExitFailure 128 -> do
      _ <- proc "git" ["rebase", "--abort"]
      return Conflicts
    System.Exit.ExitFailure a   -> do
      error ("Did not expect git rebase to return " ++ show a)

  return status_

isAncestorOf :: Repo -> Hash -> Hash -> IO Bool
isAncestorOf (Repo repo) (Hash potentialAncestor) (Hash potentialDescendant) =
  System.Directory.withCurrentDirectory repo $ do
  (exitStatus, _, err) <- proc "git" [ "merge-base"
                                     , "--is-ancestor"
                                     , potentialAncestor
                                     , potentialDescendant
                                     ]
    
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
