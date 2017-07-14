{-# LANGUAGE FlexibleContexts #-}
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

module CapDL.Matrix(
                    Matrix
                   ,isConnected
                   ,connect
                   ,newEmptyMatrix
                   ,printMatrix
                   ,printDotMatrix
                   ,showDotMatrix
                   ,getSize
                   ,disconnect
                   ) where

import Data.Array.IO

import Prelude ()
import Prelude.Compat

import Data.Map (Map)
import qualified Data.Map as Map

import Control.Monad

import Data.Char

newtype Matrix = Matrix (IOUArray (Int,Int) Bool)

{- This function takes a matrix and prints a literal table representation
with 0 for False and 1 for True, should be for debugging only -}
printMatrix :: Matrix -> IO()
printMatrix (Matrix m) =
  do ((minx, miny), (maxx, maxy)) <- getBounds m
     forM_ [minx .. maxx] $ \i ->
       forM_ [miny .. maxy] $ \j ->
         do elt <- readArray m (i,j)
            putStr $ (if elt then "1" else "0")
                     ++ if j == maxy then "\n" else " "

{- This function takes a matrix and a map of labels for
the nodes being represented by the matrix and prints
a .dot file representation of the graph.

This is hacky and only works for the specific 'names' used
in the -}

printDotMatrix :: Matrix -> Map Int (String, Maybe Word)  -> IO ()
printDotMatrix (Matrix m) nameMap =
  do putStrLn "digraph {"
     ((min, _), (_, max)) <- getBounds m
     forM_ [min .. max] $ \i ->
       do let iobjID = Map.findWithDefault (e i) i nameMap
          putStrLn $ cf $ fst iobjID
          forM_ [i .. max] $ \j ->
            do outb <- readArray m (i,j)
               inb <- readArray m (j,i)
               let iname = fst (iobjID) ++ maybe "" show (snd (iobjID))
                   jobjID = Map.findWithDefault (e j) j nameMap
                   jname = fst (jobjID) ++ maybe "" show (snd (jobjID))
               case (inb,outb) of
                 (False, False) -> return ()
                 (False, True) -> putStrLn $ cf iname ++ " -> " ++ cf jname ++ ";"
                 (True, False) -> putStrLn $ cf jname ++ " -> " ++ cf iname ++ ";"
                 (True, True) -> putStrLn $ cf iname ++ " -> " ++ cf jname ++ "[dir=both];"
     putStrLn "}"
    where
    e x = error ("Can't find name for object " ++ show x)
    cf = filter (\c -> isDigit c || isAlpha c)

showDotMatrix :: Matrix -> Map Int (String, Maybe Word)  -> IO String
showDotMatrix (Matrix m) nameMap =
  do
     ((min, _), (_, max)) <- getBounds m
     rows <- forM [min .. max] $ \i ->
       do let iobjID = Map.findWithDefault (e i) i nameMap
          cells <- forM [i .. max] $ \j ->
            do outb <- readArray m (i,j)
               inb <- readArray m (j,i)
               let iname = fst (iobjID) ++ maybe "" show (snd (iobjID))
                   jobjID = Map.findWithDefault (e j) j nameMap
                   jname = fst (jobjID) ++ maybe "" show (snd (jobjID))
               return $ case (inb,outb) of
                 (False, False) -> ""
                 (False, True) -> cf iname ++ " -> " ++ cf jname ++ ";\n"
                 (True, False) -> cf jname ++ " -> " ++ cf iname ++ ";\n"
                 (True, True) -> cf iname ++ " -> " ++ cf jname ++ "[dir=both];\n"
          return $ (cf (fst iobjID)) ++ "\n" ++ (concat cells)
     return $ "digraph {\n" ++ (concat rows) ++ "}\n"
    where
      e x = error ("Can't find name for object " ++ show x)
      cf = filter (\c -> isDigit c || isAlpha c)

isConnected :: Matrix -> Int -> Int -> IO Bool
isConnected (Matrix m) x y = readArray m (x,y)

{- Sets (x,y) to True -}
connect :: Matrix -> Int -> Int -> IO ()
connect (Matrix m) x y = writeArray m (x,y) True

{- Sets (x,y) to False -}
disconnect :: Matrix -> Int -> Int -> IO ()
disconnect (Matrix m) x y = writeArray m (x,y) False

{- Returns a new empty matrix -}
newEmptyMatrix :: Int -> IO Matrix
newEmptyMatrix n = liftM Matrix $ newArray ((0,0),(n-1,n-1)) False

{- Returns the dimensions of the matrix -}
getSize (Matrix m) = getBounds m
