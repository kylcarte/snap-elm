{-# LANGUAGE OverloadedStrings #-}

{-|

This module provides a few functions for conveniently serving
Elm files through the Snap web framework. Any changes made to
the served files will be reflected in the browser upon a refresh.

The easiest way to get started is to use the default ElmOptions:

> app = makeSnaplet ... $ do
>     opts <- defaultElmOptions
>     ...
>     addRoutes $ routes opts
>     ...

Then, provide routes to the Elm runtime, and to any Elm files
you wish to serve.

> routes opts =
>     [ ("/elm", serveElm opts "static/elm/test.elm")
>     , ...
>     , serveElmRuntime opts
>     ]

Additionally, you can customize the URI of the Elm runtime,
the file path to the Elm runtime, or the paths to the
directores that Elm will use to build and cache the compiled files.

> app = makeSnaplet ... $ do
>     opts <- mkElmOptions
>               "route/to/use/for/runtime.js"
>               (Just "/my/own/local/file/for/the/actual/runtime.js")
>               (Just "/tmp/location/for/build/dir")
>               (Just "/tmp/location/for/cache/dir")
>     ...
>     addRoutes $ routes opts
>     ...

|-}

module Snap.Elm where

------------------------------------------------------------------------------
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

-- | A set of options to coordinate the serving of Elm files and runtime.
data ElmOptions = ElmOptions
  { elmRuntimeURI  :: ByteString
  , elmRuntimePath :: FilePath
  , elmBuildPath   :: FilePath
  , elmCachePath   :: FilePath
  }

-- | The default set of options for serving Elm files.
--   This will use "static/js/elm-runtime.js" as the URI
--   for the Elm runtime, so you should use a custom route
--   if the route conflicts with another, for some reason.
defaultElmOptions :: MonadIO m => m ElmOptions
defaultElmOptions = mkElmOptions
  "static/js/elm-runtime.js"
  Nothing
  Nothing
  Nothing

-- | Construct a custom set of options.
mkElmOptions :: MonadIO m
  => ByteString     -- ^ Route at which to serve Elm runtime
  -> Maybe FilePath -- ^ 'FilePath' to custom Elm runtime
  -> Maybe FilePath -- ^ 'FilePath' to custom build directory
  -> Maybe FilePath -- ^ 'FilePath' to custom cache directory
  -> m ElmOptions
mkElmOptions uri mr mb mc = do
  rt <- maybe (liftIO Elm.runtime) return mr
  let bp = fromMaybe "elm-build" mb
  let cp = fromMaybe "elm-cache" mc
  return $ ElmOptions uri rt bp cp

-- | Serve an Elm file. The 'ElmOptions' argument can be
--   constructed at the initialization of your app.
serveElm :: MonadSnap m => ElmOptions -> FilePath -> m ()
serveElm opts fp = when (takeExtension fp == ".elm") $ do
  let args = [ "--make" 
             , "--runtime="   ++ runtimeURI
             , "--build-dir=" ++ buildPath
             , "--cache-dir=" ++ cachePath
             , fp
             ]
  (ec,out,err) <- liftIO $ readProcessWithExitCode "elm" args ""
  case ec of
    ExitFailure _ -> writeText $ T.unlines
                       [ "Failed to build Elm file (" <> T.pack fp <> "):"
                       , T.pack out
                       , T.pack err
                       ]
    ExitSuccess   -> serveFile (buildPath </> replaceExtension fp "html")
  where
  buildPath   = elmBuildPath opts
  cachePath   = elmCachePath opts
  runtimeURI  = C8.unpack $ elmRuntimeURI opts

-- | A route handler for the Elm runtime. If given the 'ElmOptions' used
--   by 'serveElm', it will place the runtime at the route the Elm file
--   will expect, as per the <script src=".../runtime.js"> element included
--   in the compiled file's <head> section.
serveElmRuntime :: MonadSnap m => ElmOptions -> (ByteString, m ())
serveElmRuntime opts =
  (elmRuntimeURI opts, serveFile $ elmRuntimePath opts)

