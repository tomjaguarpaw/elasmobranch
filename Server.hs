{-# LANGUAGE PartialTypeSignatures #-}

module Server where

import qualified Data.String
import qualified Data.Char
import           Control.Monad.IO.Class (liftIO)
import qualified Happstack.Lite as HL
import           Happstack.Server.RqData as HL

data HTMLString = HTMLString String

instance HL.ToMessage HTMLString where
  toContentType _ = Data.String.fromString "text/html; charset=UTF-8"
  toMessage (HTMLString s) = HL.toMessage s

mainOn :: (String -> IO String)
       -> (String -> IO String)
       -> IO ()
mainOn genHtml genThread =
  do HL.serve (Just HL.defaultServerConfig { HL.port = 12382 })
              (myApp genHtml genThread)

myApp :: (String -> IO String)
      -> (String -> IO String)
      -> HL.ServerPart HL.Response
myApp genHtml genThread =
  HL.msum [ HL.dir "repo" (repo genHtml)
          , HL.dir "thread" (thread genThread)
          , HL.dir "vsts" vsts
          , index
          ]

repo :: (String -> IO String)
     -> HL.ServerPart HL.Response
repo genHtml = do
  HL.method HL.GET
  repoPath <- HL.look "repo"

  html <- liftIO (genHtml repoPath)

  HL.ok (HL.toResponse (HTMLString html))

thread genThread = do
  HL.method HL.GET
  thread <- HL.look "thread"

  html <- liftIO (genThread thread)

  HL.ok (HL.toResponse (HTMLString html))

vsts = do
  HL.method HL.GET
  repo <- HL.look "repo"
  pat  <- HL.look "pat"

  let repoStripped = dropWhile Data.Char.isSpace repo
      startsWithHttps = take 8 repoStripped == "https://"
      horribleHackUrl = drop 8 repoStripped
      repoWithPat = "https://" ++ pat ++ "@" ++ horribleHackUrl
      redirectTo = "/repo?repo=" ++ repoWithPat

  if startsWithHttps
  then HL.seeOther redirectTo (HL.toResponse ())
  else HL.ok (HL.toResponse (HTMLString "Your URL didn't seem to start with https://"))

index :: HL.ServerPart HL.Response
index = do
  HL.method HL.GET

  let html = ("<html>"
              ++ "<head><title>elasmobranch</title></head>"
              ++ "<body>"
              ++ "<h1>elasmobranch</h1>"
              ++ "<p>Submit the URL to a git repository "
              ++ "and I'll tell you about the branch status</p>"
              ++ "<form action='/repo' method='GET'>"
              ++ "<p><input type='text' name='repo' size='100'></p>"
              ++ "<p><input type='submit'></p>"
              ++ "</form>"
              ++ "<h2>or if you want to do a VSTS repository</h2>"
              ++ "<form action='/vsts' method='GET'>"
              ++ "<p>Repository HTTPS URL</p>"
              ++ "<p><input type='text' name='repo' size='100'></p>"
              ++ "<p>Token</p>"
              ++ "<p><input type='password' name='pat' size='100'></p>"
              ++ "<p><input type='submit'></p>"
              ++ "</form>"
              ++ "<p><ul><li>"
              ++ "You can get the repository URL by clicking on 'Clone' in the "
              ++ "top right of your VSTS 'Code' page.</li>"
              ++ "<li>You can get a personal "
              ++ "access token by clicking on 'Create a Personal access token' in "
              ++ "the credentials setting of the same 'Clone' dialog.  elasmobranch "
              ++ "does not store the personal access token and you can revoke it "
              ++ "any time in the VSTS security settings page.</li></p>"
              ++ "</body></html>")

  HL.ok (HL.toResponse (HTMLString html))
