--
-- Copyright 2017, Data61
-- Commonwealth Scientific and Industrial Research Organisation (CSIRO)
-- ABN 41 687 119 230.
--
-- This software may be distributed and modified according to the terms of
-- the BSD 2-Clause license. Note that NO WARRANTY is provided.
-- See "LICENSE_BSD2.txt" for details.
--
-- @TAG(DATA61_BSD)
--

import Distribution.Simple
import Distribution.PackageDescription
import System.Environment
import System.IO.Error (IOError)
import Control.Exception (catch)

main = do
  maxIRQs <- getEnv "CONFIG_CAPDL_LOADER_MAX_IRQS" `catch` handler
  defaultMainWithHooks simpleUserHooks {  preBuild = preBuildHook maxIRQs }

  where
    handler :: IOError -> IO String
    handler _ = return "256"

    preBuildHook :: String -> Args -> a -> IO HookedBuildInfo
    preBuildHook maxIRQs _ _ = do
      let buildinfo = emptyBuildInfo {
                         cppOptions = ["-DCONFIG_CAPDL_LOADER_MAX_IRQS=" ++ maxIRQs]
                      }
      return (Nothing, [("parse-capDL", buildinfo)])
