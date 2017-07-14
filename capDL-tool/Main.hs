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

module Main where

import CapDL.Parser
import CapDL.DumpParser
import CapDL.ParserUtils (emptyMaps)
import CapDL.MakeModel
import CapDL.PrintModel
import CapDL.State
import CapDL.PrintDot
import CapDL.PrintXml
import CapDL.PrintIsabelle
import CapDL.PrintC
import CapDL.STCC

import System.Environment
import System.IO
import Text.ParserCombinators.Parsec
import Control.Monad
import Control.Monad.Writer
import System.Console.GetOpt
import Data.String.Utils

data Options = Options {
    optOutputIsabelle :: Maybe String,
    optOutputXml :: Maybe String,
    optOutputDot :: Maybe String,
    optOutputHeader :: Maybe String,
    optOutputText :: Maybe String,
    optOutputAnalysis :: Maybe String
    }

-- Default options.
defaultOptions :: Options
defaultOptions = Options {
    optOutputIsabelle = Nothing,
    optOutputXml = Nothing,
    optOutputDot = Nothing,
    optOutputHeader = Nothing,
    optOutputText = Nothing,
    optOutputAnalysis = Nothing
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
        (ReqArg (\arg -> \o -> o {optOutputHeader = Just arg}) "FILE")
        "output C initialiser source to FILE",

    Option ['x'] ["xml"]
        (ReqArg (\arg -> \o -> o {optOutputXml = Just arg }) "FILE")
        "output XML to FILE",

    Option ['t'] ["text"]
        (ReqArg (\arg -> \o -> o {optOutputText = Just arg }) "FILE")
        "output text to FILE",

    Option ['a'] ["analysis"]
        (ReqArg (\arg o -> o {optOutputAnalysis = Just arg }) "FILE")
        "perform analysis and output cap leak and info flow .dot files and capDL text"
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

    -- Parse the file.
    let inputFile = nonOpts !! 0
    dump <- isDump inputFile
    res <- if dump
             then genparseFromFile capDLDumpModule emptyMaps inputFile
             else genparseFromFile capDLModule emptyMaps inputFile

    -- Get the parse result (or show an error if it failed).
    res <- case res of
        Left e -> error (show e)
        Right t -> return t
    let (m, i, c) = makeModel res

    let (valid, log) = runWriter (checkModel m)
    if valid
     then do
        -- Output model in any requested format.
        let optActions = [(optOutputIsabelle, \f -> writeFile f $ show $ printIsabelle f m),
                          (optOutputXml,      \f -> writeFile f $ show $ printXml inputFile m),
                          (optOutputDot,      \f -> writeFile f $ show $ printDot inputFile m),
                          (optOutputHeader,   \f -> writeFile f $ show $ printC m i c),
                          (optOutputText,     \f -> writeFile f $ show $ pretty m),
                          (optOutputAnalysis, \f -> do (leakDot, flowDot, newM) <- leakMatrix m
                                                       writeFile (f ++ "-leak.dot") leakDot
                                                       writeFile (f ++ "-flow.dot") flowDot
                                                       writeFile (f ++ "-saturation.txt") $ show $ pretty newM)]
            condDoOpt (projection, action) = maybe (return ()) action (projection opt)
        mapM_ condDoOpt optActions
     else do
            putStr $ show log
            error("Failed to check model")
