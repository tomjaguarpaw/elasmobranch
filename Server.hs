{-# LANGUAGE PartialTypeSignatures #-}

module Server where

import qualified Data.String
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

myApp genHtml genThread =
  HL.msum [ HL.dir "repo" (repo genHtml)
          , HL.dir "thread" (thread genThread)
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

index :: HL.ServerPart HL.Response
index = do
  HL.method HL.GET

  let html = ("<html>"
              ++ "<head><title>Elasmobranch</title></head>"
              ++ "<body>"
              ++ "<h1>Elasmobranch</h1>"
              ++ "<p>Submit the URL to a git repository "
              ++ "and I'll tell you about the branch status</p>"
              ++ "<form action='/repo' method='GET'>"
              ++ "<input type='text' name='repo'>"
              ++ "<input type='submit'>"
              ++ "</form>"
              ++ "</body></html>")

  HL.ok (HL.toResponse (HTMLString html))
