--
-- Copyright 2020, Data61, CSIRO (ABN 41 687 119 230)
--
-- SPDX-License-Identifier: BSD-2-Clause
--

module Main where

import CapDL.Parser
import CapDL.DumpParser
import CapDL.PrintUtils
import CapDL.ParserUtils (emptyMaps)
import CapDL.Model
import CapDL.MakeModel
import CapDL.PrintModel
import CapDL.State
import CapDL.PrintDot
import CapDL.PrintXml
import CapDL.PrintIsabelle
import CapDL.PrintC
import CapDL.STCC

import System.Environment
import System.Exit (exitFailure)
import System.FilePath
import System.IO
import System.Posix (rename, removeLink)
import Text.ParserCombinators.Parsec
import Control.Exception (bracketOnError)
import Control.Monad
import Control.Monad.Writer
import Data.Maybe
import System.Console.GetOpt
import Data.String.Utils
import qualified Data.Map as Map
import qualified Data.Yaml as Yaml
import qualified Text.PrettyPrint as PP

data Options = Options {
    optOutputIsabelle :: Maybe String,
    optOutputXml :: Maybe String,
    optOutputDot :: Maybe String,
    optOutputCSpec :: Maybe String,
    optDynamicAllocCSpec :: Bool,
    optOutputText :: Maybe String,
    optOutputAnalysis :: Maybe String,
    optDumpAST :: Maybe String,
    optObjectSizeFile :: Maybe String
    }

-- Default options.
defaultOptions :: Options
defaultOptions = Options {
    optOutputIsabelle = Nothing,
    optOutputXml = Nothing,
    optOutputDot = Nothing,
    optOutputCSpec = Nothing,
    optDynamicAllocCSpec = True,
    optOutputText = Nothing,
    optOutputAnalysis = Nothing,
    optDumpAST = Nothing,
    optObjectSizeFile = Nothing
    }

--
-- Options our program supports.
--
options = [
    Option ['i'] ["isabelle"]
        (ReqArg (\arg -> \o -> o {optOutputIsabelle = Just arg }) "FILE")
        "output isabelle to FILE",

    Option ['d'] ["dot"]
        (ReqArg (\arg -> \o -> o {optOutputDot = Just arg }) "FILE")
        "output dot to FILE",

    Option ['c'] ["code"]
        (ReqArg (\arg -> \o -> o {optOutputCSpec = Just arg}) "FILE")
        "output C initialiser source to FILE",

    Option [] ["code-dynamic-alloc"]
        (NoArg (\o -> o {optDynamicAllocCSpec = True}))
        "assume dynamic allocation for C initialiser (default)",

    Option [] ["code-static-alloc"]
        (NoArg (\o -> o {optDynamicAllocCSpec = False}))
        "assume static allocation for C initialiser (must have untyped covers)",

    Option ['x'] ["xml"]
        (ReqArg (\arg -> \o -> o {optOutputXml = Just arg }) "FILE")
        "output XML to FILE",

    Option ['t'] ["text"]
        (ReqArg (\arg -> \o -> o {optOutputText = Just arg }) "FILE")
        "output text to FILE",

    Option ['a'] ["analysis"]
        (ReqArg (\arg o -> o {optOutputAnalysis = Just arg }) "FILE")
        "perform analysis and output cap leak and info flow .dot files and capDL text",

    Option [] ["dump-ast"]
        (ReqArg (\arg o -> o {optDumpAST = Just arg}) "FILE")
        "dump internal AST",

    Option [] ["object-sizes"]
        (ReqArg (\arg o -> o {optObjectSizeFile = Just arg}) "FILE")
        "YAML file containing kernel object sizes. Required for --code-dynamic-alloc and --isabelle"
  ]

--
-- Usage information
--
usageHeader = "usage: parse-capDL [options] <input file>"

--
-- The result of "getOpt" returns a list of functions that transforms
-- a "Options", returning a new "Options" object with the option applied. This
-- fucnction applies all these generated option functions together.
--
processOpts :: [Options -> Options] -> Options
processOpts [] = defaultOptions
processOpts (action:actions) = action (processOpts actions)

genparseFromFile :: GenParser Char s a -> s -> String -> IO (Either ParseError a)
genparseFromFile p st fname = do
    input <- readFile fname
    return $ runParser p st fname input

isDump :: String -> IO Bool
isDump fname = do
    input <- openFile fname ReadMode
    first <- hGetLine input
    hClose input
    if strip first == "-- Dump"
     then return True
     else return False

genObjectSizeMap :: Map.Map String Word -> ObjectSizeMap
genObjectSizeMap m =
    Map.fromList [ (koType, sz)
                 | (n, sz) <- Map.toList m, Just koType <- [Map.lookup n names] ]
    where names = Map.fromList
                      [ ("seL4_Slot",                CNode_T)
                      , ("seL4_TCBObject",           TCB_T)
                      , ("seL4_EndpointObject",      Endpoint_T)
                      , ("seL4_NotificationObject",  Notification_T)
                      , ("seL4_ASID_Pool",           ASIDPool_T)
                      , ("seL4_RTReplyObject",       RTReply_T)
                      , ("seL4_VCPU",                VCPU_T)
                      , ("seL4_PageTableObject",     PT_T)
                      , ("seL4_PageDirectoryObject", PD_T)
                      , ("seL4_AARCH64_PGD",         PGD_T)
                      , ("seL4_AARCH64_PUD",         PUD_T)
                      , ("seL4_IOPageTableObject",   IOPT_T)
                      , ("seL4_X64_PDPT",            PDPT_T)
                      , ("seL4_X64_PML4",            PML4_T)
                      , ("seL4_SchedContextObject",  SC_T)
                      , ("seL4_IOPorts",             IOPorts_T)
                      , ("seL4_IODevice",            IODevice_T)
                      , ("seL4_ARMIODevice",         ARMIODevice_T)
                      , ("seL4_IRQ",                 IrqSlot_T)
                      , ("seL4_IOAPICIRQ",           IOAPICIrqSlot_T)
                      , ("seL4_MSIIRQ",              MSIIrqSlot_T)
                      , ("seL4_ARMIRQ",              ARMIrqSlot_T)
                      , ("seL4_ARMSID",              ARMSID_T)
                      , ("seL4_ARMCB",               ARMCB_T)
                      , ("seL4_ARMSMC",              ARMSMC_T)
                      , ("seL4_ARMSGI_Signal",       ARMSGISignal_T)
                      ]

-- Abort with an error message if 'isFullyAllocated' fails.
assertIsFullyAllocated :: (PP.Doc -> PP.Doc) -> ObjectSizeMap -> ObjMap Word -> CoverMap -> IO ()
assertIsFullyAllocated wrapMessage sizeMap objs untypedCovers =
  case isFullyAllocated sizeMap objs untypedCovers of
    Right () -> return ()
    Left (msg, badObjs) -> do
      hPutStrLn stderr . PP.render . wrapMessage $
        PP.text (msg ++ ":") PP.$+$
        -- TODO: maybe limit number of badObjs printed
        PP.nest 2 (PP.vcat $ PP.text . CapDL.PrintUtils.showID <$> badObjs)
      exitFailure

main = do
    -- Parse command line arguments.
    args <- getArgs
    let (actions, nonOpts, msgs) = getOpt RequireOrder options args
    when (length msgs > 0) $
        error (concat msgs ++ usageInfo usageHeader options)
    when (length nonOpts < 1) $
        error ("input file not specified\n" ++ usageInfo usageHeader options)
    when (length nonOpts > 1) $
        error ("unrecognised arguments: " ++ unwords nonOpts ++ "\n" ++ usageInfo usageHeader options)
    let opt = foldr ($) defaultOptions actions

    -- Print option names that satisfy a condition
    let gatherOptions = Data.String.Utils.join " and " . map snd . filter fst

    let whyNeedObjectSizes = gatherOptions
          [ (isJust (optOutputIsabelle opt), "--isabelle")
          , (isJust (optOutputCSpec opt) && optDynamicAllocCSpec opt, "--code-dynamic-alloc") ]
    when (not (null whyNeedObjectSizes) && isNothing (optObjectSizeFile opt)) $
        error $ "--object-sizes file is required for " ++ whyNeedObjectSizes

    -- Parse the file.
    let inputFile = nonOpts !! 0
    dump <- isDump inputFile
    res <- if dump
             then genparseFromFile capDLDumpModule emptyMaps inputFile
             else genparseFromFile capDLModule emptyMaps inputFile

    -- Parse object sizes file, if available.
    objSizeMap <- case optObjectSizeFile opt of
        Nothing -> return Map.empty
        Just f -> do yParse <- Yaml.decodeFileEither f
                     case yParse of
                         Left err -> error $ "failed to parse object sizes file " ++ show f ++ "\n"
                                             ++ show err
                         Right m -> return $ genObjectSizeMap m

    -- Get the parse result (or show an error if it failed).
    res <- case res of
        Left e -> error (show e)
        Right t -> return t
    let ((m, i, c), mmLog) = runWriter (makeModel res)
    when (not (PP.isEmpty mmLog)) $ hPrint stderr mmLog

    -- `--isabelle` requires a statically allocated spec, so check this now.
    -- NB: we don't check this for `--code-static-alloc`, because we might not
    --     have objSizeMap for that mode.
    let whyNeedStaticAlloc = gatherOptions
          [ (isJust (optOutputIsabelle opt), "--isabelle") ]
        prefix = "A statically allocated spec is required for " ++ whyNeedStaticAlloc
    when (not (null whyNeedStaticAlloc)) $
        assertIsFullyAllocated (PP.text prefix PP.$+$) objSizeMap (objects m) (untypedCovers m)

    let writeFile' "-" s = putStr s
        writeFile' f   s = bracketOnError
            (do (tempF, handle) <- uncurry openTempFile (splitFileName f)
                hClose handle -- ignore for now
                return tempF)
            removeLink
            (\tempF -> do writeFile tempF s
                          rename tempF f)

    let (valid, log) = runWriter (checkModel m)
    if valid
     then do
        -- Output model in any requested format.
        let optActions = [(optOutputIsabelle, \f -> writeFile' f $ show $ printIsabelle f objSizeMap m),
                          (optOutputXml,      \f -> writeFile' f $ show $ printXml inputFile m),
                          (optOutputDot,      \f -> writeFile' f $ show $ printDot inputFile m),
                          (optOutputCSpec,    \f -> let allocType
                                                          | optDynamicAllocCSpec opt = DynamicAlloc objSizeMap
                                                          | otherwise = StaticAlloc
                                                    in writeFile' f $ show $
                                                       printC allocType m i c),
                          (optOutputText,     \f -> writeFile' f $ show $ pretty m),
                          (optOutputAnalysis, \f -> do (leakDot, flowDot, newM) <- leakMatrix m
                                                       writeFile (f ++ "-leak.dot") leakDot
                                                       writeFile (f ++ "-flow.dot") flowDot
                                                       writeFile (f ++ "-saturation.txt") $ show $ pretty newM),

                          (optDumpAST,        \f -> writeFile' f $ show m)]
            condDoOpt (projection, action) = maybe (return ()) action (projection opt)
        mapM_ condDoOpt optActions
     else do
            putStr $ show log
            error("Failed to check model")
