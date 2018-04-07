module Server where

import qualified Data.String
import           Control.Monad.IO.Class (liftIO)
import qualified Happstack.Lite as HL
import           Happstack.Server.RqData as HL

data HTMLString = HTMLString String

instance HL.ToMessage HTMLString where
  toContentType _ = Data.String.fromString "text/html; charset=UTF-8"
  toMessage (HTMLString s) = HL.toMessage s

mainOn :: (String -> IO String) -> IO ()
mainOn genHtml = do HL.serve (Just HL.defaultServerConfig { HL.port = 12382 })
                             (myApp genHtml)

myApp :: (String -> IO String)
      -> HL.ServerPart HL.Response
myApp genHtml = HL.msum [ HL.dir "repo" (repo genHtml) ]

repo :: (String -> IO String)
     -> HL.ServerPart HL.Response
repo genHtml = do
  HL.method HL.GET
  repoPath <- HL.look "repo"
  liftIO (putStrLn "Here2")
  
  html <- liftIO (genHtml repoPath)

  HL.ok (HL.toResponse (HTMLString html))
