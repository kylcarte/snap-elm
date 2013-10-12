{-# LANGUAGE OverloadedStrings #-}

module Snap.Elm where

------------------------------------------------------------------------------
import           Control.Applicative
import           Control.Monad
import           Control.Monad.IO.Class
import           Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as C8
import           Data.Maybe (fromMaybe)
import           Data.Monoid
import qualified Data.Text as T
import           Snap.Core
import           Snap.Util.FileServe
------------------------------------------------------------------------------
import qualified Language.Elm as Elm
import           System.Exit
import           System.FilePath
import           System.Process
------------------------------------------------------------------------------

data ElmOptions = ElmOptions
  { elmRuntimeURI  :: ByteString
  , elmRuntimePath :: FilePath
  , elmCachePath   :: FilePath
  }

defaultElmOptions :: IO ElmOptions
defaultElmOptions = mkElmOptions
  "static/js/elm-runtime.js"
  Nothing
  Nothing

mkElmOptions :: ByteString -> Maybe FilePath -> Maybe FilePath -> IO ElmOptions
mkElmOptions uri mr mc = ElmOptions
  <$> pure uri
  <*> maybe Elm.runtime pure mr
  <*> pure (fromMaybe "elm-cache" mc)

serveElm :: MonadSnap m => ElmOptions -> FilePath -> m ()
serveElm opts fp = when (takeExtension fp == ".elm") $ do
  let args = [ "--make" 
             , "--runtime=" ++ C8.unpack (elmRuntimeURI opts)
             , "--cache-dir=" ++ elmCachePath opts
             , fp
             ]
  (ec,out,err) <- liftIO $ readProcessWithExitCode "elm" args ""
  case ec of
    ExitFailure _ -> writeText $ "Failed to build .elm file: " <> T.unlines (map T.pack [out,err])
    ExitSuccess   -> do
      serveFile ("build" </> replaceExtension fp "html")

serveElmRuntime :: MonadSnap m => ElmOptions -> (ByteString, m ())
serveElmRuntime opts =
  (elmRuntimeURI opts, serveFile $ elmRuntimePath opts)

