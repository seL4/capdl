--
-- Copyright 2014, NICTA
--
-- This software may be distributed and modified according to the terms of
-- the BSD 2-Clause license. Note that NO WARRANTY is provided.
-- See "LICENSE_BSD2.txt" for details.
--
-- @TAG(NICTA_BSD)
--

import Distribution.Simple
import Distribution.PackageDescription
import System.Environment

main = do
  maxIRQs <- getEnv "CONFIG_CAPDL_LOADER_MAX_IRQS"
  defaultMainWithHooks simpleUserHooks {  preBuild = preBuildHook maxIRQs }

preBuildHook :: String -> Args -> a -> IO HookedBuildInfo
preBuildHook maxIRQs _ _ = do
  let buildinfo = emptyBuildInfo {
               cppOptions = ["-DCONFIG_CAPDL_LOADER_MAX_IRQS=" ++ maxIRQs]
             }
  return (Nothing, [("parse-capDL", buildinfo)])
