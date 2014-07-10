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
>     [ ("/elm", serveElmFile opts "static/elm/test.elm")
>     , ...
>     , serveElmRuntime opts
>     ]

Additionally, you can customize the URI of the Elm runtime,
the file path to the Elm runtime, or the paths to the
directores that Elm will use to build and cache the compiled files.

> app = makeSnaplet ... $ do
>     opts <- setElmBuildPath "/tmp/elm" <$>
>             setElmVerbose True <$>
>             defaultElmOptions
>     ...
>     addRoutes $ routes opts
>     ...

The 'FilePath's supplied to setElm{Source,Build,Cache}Path can be
relative or absolute.

-}

module Snap.Elm
  ( ElmOptions (..)
  , defaultElmOptions
  , setElmVerbose
  , setElmRuntimeURI
  , setElmRuntimePath
  , setElmSourcePath
  , setElmBuildPath
  , setElmCachePath
  , serveElmFile
  , serveElmDirectory
  , serveElmRuntime
  ) where

import           Control.Monad
import           Control.Monad.IO.Class
import           Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as C8
import           Data.Monoid
import qualified Data.Text as T
import qualified Data.Text.IO as T
import           Snap.Core
import           Snap.Util.FileServe
import qualified Elm.Internal.Paths as ElmPath
import           System.Directory
import           System.Exit
import           System.FilePath
import           System.Process

import Data.String

-- | A set of options to coordinate the serving of Elm files and runtime.
data ElmOptions = ElmOptions
  { elmIsVerbose   :: Bool
  , elmRuntimeURI  :: ByteString
  , elmRuntimePath :: FilePath
  , elmSourcePath  :: FilePath
  , elmBuildPath   :: FilePath
  , elmCachePath   :: FilePath
  }

-- | The default set of options for serving Elm files.
-- The values are as follows (IO aside):
--
-- > ElmOptions
-- >   { elmIsVerbose   = False
-- >   , elmRuntimeURI  = "/static/js/elm-runtime.js"
-- >   , elmRuntimePath = <Language.Elm.runtime>
-- >   , elmSourcePath  = "."
-- >   , elmBuildPath   = "elm-build"
-- >   , elmCachePath   = "elm-cache"
-- >   }
--
defaultElmOptions :: MonadIO m => m ElmOptions
defaultElmOptions = return ElmOptions
  { elmIsVerbose = False
  , elmRuntimeURI = "/static/js/elm-runtime.js"
  , elmRuntimePath = ElmPath.runtime
  , elmSourcePath = "."
  , elmBuildPath = "elm-build"
  , elmCachePath = "elm-cache"
  }

-- | Tell Elm to be verbose (print all executed commands and their output
-- to stdout), or quiet (print nothing).
setElmVerbose :: Bool -> ElmOptions -> ElmOptions
setElmVerbose v opts = opts { elmIsVerbose = v }

-- | Set the URI at which to serve the Elm runtime JS file.
setElmRuntimeURI :: ByteString -> ElmOptions -> ElmOptions
setElmRuntimeURI uri opts = opts { elmRuntimeURI = uri }

-- | Set the 'FilePath' to some custom Elm runtime.
setElmRuntimePath :: FilePath -> ElmOptions -> ElmOptions
setElmRuntimePath rt opts = opts { elmRuntimePath = rt }

-- | Set the directory to look for .elm files.
-- This allows for the @elm@ binary to properly find
-- local Elm modules.
setElmSourcePath :: FilePath -> ElmOptions -> ElmOptions
setElmSourcePath src opts = opts { elmSourcePath = src }

-- | Set the directory to use for storing the compiled .html
-- that @elm@ produces.
setElmBuildPath :: FilePath -> ElmOptions -> ElmOptions
setElmBuildPath bld opts = opts { elmBuildPath = bld }

-- | Set the directory to use for storing the .elmi and .elmo
-- files that @elm@ produces.
setElmCachePath :: FilePath -> ElmOptions -> ElmOptions
setElmCachePath cch opts = opts { elmCachePath = cch }

-- | Compile and serve an Elm file.
serveElmFile :: MonadSnap m => ElmOptions -> FilePath -> m ()
serveElmFile opts fp = when (takeExtension fp == ".elm") $ do
  mBin <- liftIO $ findExecutable "elm"
  case mBin of
    Nothing -> elmError fp "No executable 'elm' in PATH"
    Just bin -> do
      cd <- liftIO getCurrentDirectory
      let runtimeURI = C8.unpack        (elmRuntimeURI opts)
      let sourcePath = makeAbsolutePath (elmSourcePath opts) cd
      let buildPath  = makeAbsolutePath (elmBuildPath  opts) cd
      let cachePath  = makeAbsolutePath (elmCachePath  opts) cd
      let args = [ "--make"
                 , "--set-runtime="   ++ runtimeURI
                 , "--build-dir=" ++ buildPath
                 , "--cache-dir=" ++ cachePath
                 , fp
                 ]

      ifVerbose $ liftIO $ do
        putStrLn "Elm:"
        putStrLn $ "  $ cd " ++ elmSourcePath opts
        putStrLn $ unwords $ ("  $ " <> bin) : args

      (_,hOut,hErr,pid) <- liftIO $ runInteractiveProcess bin args
                             (Just sourcePath)
                             Nothing
      out <- liftIO $ T.hGetContents hOut
      err <- liftIO $ T.hGetContents hErr
      ec  <- liftIO $ waitForProcess pid
      ifVerbose $ liftIO $ T.putStrLn $ indent out

      case ec of
        ExitFailure _ -> elmError fp $ indent $ T.unlines [ out , "" , err ]
        ExitSuccess   -> serveFile $ buildPath </> replaceExtension fp "html"
  where
  ifVerbose = when $ elmIsVerbose opts

  makeAbsolutePath :: FilePath -> FilePath -> FilePath
  makeAbsolutePath p cd = case p of
    ""    -> cd
    "."   -> cd
    '/':_ -> p
    _     -> cd </> p

-- | Serve a directory of Elm files.
--
-- For example, a list of routes could contain:
--
-- > routes opts =
-- >   [ ...
-- >   , serveElmDirectory opts "/elm"
-- >   ]
--
-- In this example, if the ElmOptions contained @static/elm@
-- as the sourcePath, the route @elm/file.elm@ would
-- be handled by @serveElmFile "file.elm"@, run with the working directory
-- @static/elm@.
serveElmDirectory :: MonadSnap m
  => ElmOptions
  -> ByteString -- ^ Route for serving directory.
  -> (ByteString, m ())
serveElmDirectory opts d = (uri,handler)
  where
  param = "file"
  uri
    | C8.null d        = "/:" <> param
    | C8.last d == '/' = d <> ":" <> param
    | otherwise        = d <> "/:" <> param
  handler = do
    mf <- getParam param
    case mf of
      Nothing -> return ()
      Just f  -> serveElmFile opts . C8.unpack $ f

-- | A route handler for the Elm runtime. If given the 'ElmOptions' used
-- by 'serveElmFile', it will place the runtime at the route the Elm file
-- will expect, as per the @\<script src=".../runtime.js">@ element included
-- in the compiled file's @\<head>@ section.
serveElmRuntime :: MonadSnap m => ElmOptions -> (ByteString, m ())
serveElmRuntime opts =
  ( elmRuntimeURI opts
  , serveFile $ elmRuntimePath opts
  )

elmError :: MonadSnap m => FilePath -> T.Text -> m ()
elmError fp msg = writeText $ T.unlines
  [ "Failed to build Elm file (" <> T.pack fp <> "):"
  , indent msg
  ]

indent :: T.Text -> T.Text
indent = T.unlines . map (T.replicate n " " <>) . T.lines
  where
  n = 2

