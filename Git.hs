module Git where

import qualified Data.Ord
import qualified System.Directory
import qualified System.IO.Temp
import qualified System.Process
import qualified System.Exit

import qualified Streaming as S
import qualified Streaming.Prelude as S

-- Utils

procC :: FilePath
      -> [String]
      -> (System.Exit.ExitCode -> Maybe (String -> String -> IO r))
      -> Maybe FilePath
      -> IO r
procC program arguments f dir = do
  (exitCode, out, err) <- proc program arguments dir
  case f exitCode of
      Nothing ->
        return (error ("Didn't expect '" ++ program ++ unwords arguments
                        ++ " to return " ++ show exitCode))
      Just g  -> g out err

proc :: FilePath
     -> [String]
     -> Maybe FilePath
     -> IO (System.Exit.ExitCode, String, String)
proc program arguments dir =
  System.Process.readCreateProcessWithExitCode createProcess stdin
  where createProcess = (System.Process.proc program arguments)
                          { System.Process.cwd = dir }
        stdin = ""

procEnv :: FilePath
        -> [String]
        -> Maybe FilePath
        -> Maybe [(String, String)]
        -> IO (System.Exit.ExitCode, String, String)
procEnv program arguments dir env =
  System.Process.readCreateProcessWithExitCode createProcess stdin
  where createProcess = (System.Process.proc program arguments)
                          { System.Process.cwd = dir
                          , System.Process.env = env
                          }
        stdin = ""

-- Git

data Branch = Branch String deriving (Show, Eq, Ord)
data Hash = Hash String deriving (Show, Eq, Ord)
data Repo = Repo String deriving Show
data RepoDirty = RepoDirty String deriving Show

branchName :: Branch -> String
branchName (Branch branch) = branch

data RepoAtDirectory = RADRepo Repo
                     | RADRepoDirty RepoDirty

-- FIXME test for failure
withClone :: String
          -> (Either String Repo -> IO a)
          -> IO a
withClone repo f = do
  System.IO.Temp.withSystemTempDirectory "" $ \temp -> do
    let env = Just [("GIT_TERMINAL_PROMPT", "0")]
    --- ^^ https://serverfault.com/a/665959

    (exitCode, _, err) <- procEnv "git" [ "clone"
--                          , "--depth=1"
--                          , "--no-single-branch"
-- If we shallow clone then we need to
--         git fetch --unshallow origin master
                                     , repo
                                     , temp
                                     ]
                            Nothing
                            env
    -- We don't actually care about to whom to attribute patches when
    -- git does a rebase or a merge but it seems that it can die without
    -- this information, making it look like all branches are
    -- incompatible.  Perhaps this should be moved into the merge and
    -- rebase commands themselves.
    Git.proc "git" ["config", "user.email", "elasmobranch@example.com"] (Just temp)
    Git.proc "git" ["config", "user.name", "Elasmobranch"] (Just temp)

    case exitCode of
      System.Exit.ExitFailure _ -> f (Left err)
      System.Exit.ExitSuccess   -> f (Right (Repo temp))

repoAtDirectory :: FilePath
                -> IO (Maybe RepoAtDirectory)
repoAtDirectory path = do
  (exitCode, out, _) <- proc "git" ["rev-parse", "--show-toplevel"] (Just path)
  let dir = stripLast out
  case exitCode of
    System.Exit.ExitSuccess     -> do
        (exitCode2, _, _) <- proc "git" ["diff-index", "--quiet", "HEAD", "--"] (Just dir)
        case exitCode2 of
          System.Exit.ExitSuccess   -> return (Just (RADRepo (Repo dir)))
          System.Exit.ExitFailure 1 -> return (Just (RADRepoDirty (RepoDirty dir)))
          System.Exit.ExitFailure a -> error ("Didn't expect git diff-index "
                                              ++ "--quiet HEAD -- to return "
                                              ++ show a)
    System.Exit.ExitFailure 128 -> return Nothing
    System.Exit.ExitFailure a   -> error ("Didn't expect git rev-parse "
                                          ++ "--show-toplevel to return "
                                          ++ show a)

data InProgress = IPRebase
                | IPMerge
                | IPStashPop
                | IPCherryPick

what'sInProgress :: RepoDirty
                 -> IO (Maybe InProgress)
what'sInProgress (RepoDirty dir) = do
  let doesGitPathExist s = System.Directory.doesPathExist (dir ++ "/.git/" ++ s)

  mergeHeadExists      <- doesGitPathExist "MERGE_HEAD"
  mergeModeExists      <- doesGitPathExist "MERGE_MODE"
  mergeMsgExists       <- doesGitPathExist "MERGE_MSG"
  rebaseApplyExists    <- doesGitPathExist "rebase-apply"
  cherryPickHeadExists <- doesGitPathExist "CHERRY_PICK_HEAD"

  let canCheckoutHEAD_ = do
        (exitCode, _, _) <- proc "git" ["checkout", "HEAD"] (Just dir)
        case exitCode of
          System.Exit.ExitSuccess   -> return True
          System.Exit.ExitFailure 1 -> return False
          System.Exit.ExitFailure a -> error ("Didn't expect git rev-parse "
                                              ++ "--show-toplevel to return "
                                              ++ show a)

  case ((mergeHeadExists, mergeModeExists, mergeMsgExists),
                 rebaseApplyExists,
                 cherryPickHeadExists) of
   ((True,  True,  True),  False, False) -> return (Just IPMerge)
   ((False, False, False), True,  False) -> return (Just IPRebase)
   ((False, False, True),  False, True)  -> return (Just IPCherryPick)
   ((False, False, False), False, False) -> do
       -- We mustn't try to checkout HEAD if we're in a merge or
       -- rebase because if the merge or rebase has been completely
       -- fixed up (but not yet committed) the in-progress fix up will
       -- be destroyed.  Therefore we only try to checkout HEAD if
       -- from the conflict markers we deduce that it's safe to do so.
       canCheckoutHEAD <- canCheckoutHEAD_
       return $ if canCheckoutHEAD
                then Nothing
                else Just IPStashPop
   unexpected -> error ("Unexpected combination of conflict markers: " ++ show unexpected)

originBranchHashes :: Repo -> IO [(Branch, Hash)]
originBranchHashes repo = do
  branches <- Git.remoteBranches repo

  let branch_hashes = S.for (S.each branches) $ \branch -> do
        hash <- S.lift (Git.revParse repo branch)
        S.yield (branch, hash)

  bhm_ <- S.toList_ branch_hashes

  return bhm_

remoteBranches :: Repo -> IO [Branch]
remoteBranches (Repo repo) = do
  (_, out, _) <- proc "git" ["branch", "--remote"] (Just repo)
  return (map Branch (originBranches out))

originBranches :: String -> [String]
originBranches = filter (not . startsWith "origin/HEAD ")
                 . fmap (\originBranch -> drop 2 originBranch)
                 . lines
  where startsWith start target = take (length start) target == start

-- Not technically accurate
stripNewline :: String -> String
stripNewline = stripLast

stripLast :: String -> String
stripLast s = take (length s - 1) s

-- FIXME: Check for error
--
-- TODO: We could expand this to being able to parse general
-- revisions, as in gitrevisions(7)
revParse :: Repo -> Branch -> IO Hash
revParse (Repo repo) (Branch branch) = do
  (exit, out, err) <- proc "git" ["rev-parse", branch] (Just repo)
  case exit of
    System.Exit.ExitSuccess   -> return (Hash (stripLast out))
    System.Exit.ExitFailure _ -> error err

status :: Repo -> Hash -> Hash -> IO (Either (RebaseStatus, MergeStatus) Ordering)
status repo hash1 hash2 = do
  mord <- compareHash repo hash1 hash2

  case mord of
    Just ord -> return (Right ord)
    Nothing  -> do
      rebaseStatus <- canRebaseOnto repo hash1 hash2
      mergeStatus  <- canMergeInto  repo hash1 hash2

      return (Left (rebaseStatus, mergeStatus))

-- git rebase hash x y
--
-- Rebase x onto y
canRebaseOnto :: Repo -> Hash -> Hash -> IO RebaseStatus
canRebaseOnto (Repo repo) (Hash hash) (Hash onto) = do
  (exit, _, _) <- proc "git" ["rebase", onto, hash] (Just repo)
  status_ <- case exit of
    System.Exit.ExitSuccess     -> return Clean
    System.Exit.ExitFailure 1 -> do
      _ <- proc "git" ["rebase", "--abort"] (Just repo)
      return Conflicts
    System.Exit.ExitFailure 128 -> do
      _ <- proc "git" ["rebase", "--abort"] (Just repo)
      return Conflicts
    System.Exit.ExitFailure a   -> do
      error ("Did not expect git rebase to return " ++ show a
            ++ " with git rebase " ++ onto ++ " " ++ hash)

  return status_

-- TODO: This is very similar to canRebaseOnto
canMergeInto :: Repo -> Hash -> Hash -> IO MergeStatus
canMergeInto (Repo repo) (Hash hash) (Hash into) = do
  proc "git" ["checkout", into] (Just repo)
  (exit, _, _) <- proc "git" ["merge", hash] (Just repo)
  status_ <- case exit of
    System.Exit.ExitSuccess     -> return MClean
    System.Exit.ExitFailure 1 -> do
      _ <- proc "git" ["merge", "--abort"] (Just repo)
      return MConflicts
    System.Exit.ExitFailure 128 -> do
      _ <- proc "git" ["merge", "--abort"] (Just repo)
      return MConflicts
    System.Exit.ExitFailure a   -> do
      error ("Did not expect git merge to return " ++ show a
            ++ " with git merge " ++ into ++ " " ++ hash)

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


data RebaseStatus = Conflicts | Clean deriving (Show, Eq, Ord, Bounded, Enum)
data MergeStatus = MConflicts | MClean deriving (Show, Eq, Ord, Bounded, Enum)

-- Test

test :: Bool
test = originBranches "  origin/100\n  origin/HEAD -> origin/master\n  origin/master\n  origin/partial-type-signatures"
       == ["origin/100", "origin/master", "origin/partial-type-signatures"]
