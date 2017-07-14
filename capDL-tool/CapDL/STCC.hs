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

module CapDL.STCC(
                  somefn
                 ,transitiveClosure
                 ,leakMatrix
                 ) where

import CapDL.Matrix
import CapDL.Model

import Control.Monad.Compat
import Data.Map ( Map )
import qualified Data.Map as Map

import Data.Set ( Set )
import qualified Data.Set as Set

import Prelude ()
import Prelude.Compat

type ObjDictionary = Map ObjID Int

projectTCB capset =
  Set.fromList [capObj | (TCBCap capObj) <- Set.toList capset]
projectCNode capset =
  Set.fromList [capObj | (CNodeCap capObj _ _) <- Set.toList capset]
projectEndpoint capset =
  Set.fromList $ concat [[(r, capObj) | r <- Set.toList rights] | (EndpointCap capObj _ rights) <- Set.toList capset]
projectAsync capset =
  Set.fromList $ concat [[(r, capObj) | r <- Set.toList rights] | (NotificationCap capObj _ rights) <- Set.toList capset]
projectFrame capset =
  Set.fromList $ concat [[(r, capObj) | r <- Set.toList rights] | (FrameCap capObj rights Nothing True Nothing) <- Set.toList capset]

somefn :: Model Word -> IO ()
somefn (Model _ objects _ _ _) =
  do matrix <- newEmptyMatrix $ length tcbIDs
     leaksTos matrix objDict tcbDirectCaps
     warshall matrix
     _ <- saturateCSpaces matrix objDict cspaces
     return ()
  where
    tcbIDs = [objID | (objID, (TCB {})) <- Map.toList objects]
    cspaces = zip tcbIDs $ map (getCSpace objects) tcbIDs
    tcbDirectCaps = directTCBConnections cspaces
    objDict = Map.fromList (zip tcbIDs [0..])

transitiveClosure :: Model Word -> IO (Model Word)
transitiveClosure model@(Model _ objects _ _ _) =
  do matrix <- newEmptyMatrix $ length tcbIDs
     leaksTos matrix objDict tcbDirectCaps
     warshall matrix
     newcspaces <- saturateCSpaces matrix objDict cspaces
     let cspaceUpdates = zipWith f newcspaces cspaces
         newModel = updateModel model cspaceUpdates
     return newModel
  where
    tcbIDs = [objID | (objID, (TCB {})) <- Map.toList objects]
    cspaces = zip tcbIDs $ map (getCSpace objects) tcbIDs
    tcbDirectCaps = directTCBConnections cspaces
    objDict = Map.fromList (zip tcbIDs [0..])
    f (objID, cspace) (_, cspace') = (objID, cspace' `Set.difference` cspace)

leakMatrix :: Model Word -> IO (String, String, Model Word)
leakMatrix model@(Model _ objects _ _ _) =
  do authMatrix <- newEmptyMatrix $ mSize
     leaksTos authMatrix objDict tcbDirectCaps
     warshall authMatrix

     newcspaces <- saturateCSpaces authMatrix objDict cspaces
     let cspaceUpdates = zipWith f newcspaces cspaces
         newModel = updateModel model cspaceUpdates

     infoMatrix <- newEmptyMatrix $ mSize
     authSharingFlows authMatrix infoMatrix
     flowsTos infoMatrix objDict tcbDirectFlows
     warshall infoMatrix

     removeReflexive infoMatrix
     leakDot <- showDotMatrix authMatrix objDict'
     flowDot <- showDotMatrix infoMatrix objDict'
     return (leakDot, flowDot, newModel)
  where
    f (objID, cspace) (_, cspace') = (objID, cspace' `Set.difference` cspace)
    sndUnion (tcbID, set) (_, set') = (tcbID, Set.union set set')
    tcbIDs = [objID | (objID, (TCB {})) <- Map.toList objects]
    mSize = length tcbIDs
    cspaces = [(tcbID, Set.fromList $ getCaps objects tcbID) | tcbID <- tcbIDs]
    vspaces = [(tcbID, Set.fromList $ getFrames objects tcbID) | tcbID <- tcbIDs]
    tcbDirectCaps = directTCBConnections cspaces
    tcbDirectFlows = directTCBFlows (zipWith sndUnion vspaces cspaces)
    objDict = Map.fromList (zip tcbIDs [0..])
    objDict' = Map.fromList (zip [0..] tcbIDs)

updateModel :: Model Word -> [(ObjID, Set Cap)] -> Model Word
updateModel (Model arch objects irqNode cdt untypedCovers) tcbs =
    Model arch (foldr f objects tcbs) irqNode cdt untypedCovers
  where
    f (tcb, cspace) objects =
      let rootCNodeID = rootCNodeOf objects tcb
          e = error "cannot find root CNode in updateModel"
          (CNode slots x) = Map.findWithDefault e rootCNodeID objects
          maxKey = maximum $ Map.keys $ slots
          newKeys = map (maxKey +) [1..]
          newSlots = foldr (\(k,a) -> Map.insert k a) slots (zip newKeys $ Set.toList cspace)
      in Map.insert rootCNodeID (CNode newSlots x) objects
    rootCNodeOf objects tcb =
      let e = error "cannot find tcbID in updateModel" in
      case (Map.findWithDefault e tcb objects) of
        (TCB slots _ _ _ _) -> head [capObj | (CNodeCap capObj _ _) <- (map snd (Map.toList slots))]
        _ -> error "non-tcb ID in updateModel"

saturateCSpaces :: Matrix -> ObjDictionary -> [(ObjID, Set Cap)] -> IO [(ObjID, Set Cap)]
saturateCSpaces _ _ [] = return []
saturateCSpaces matrix objDict (tcb : tcbs) =
  do head <- foldM f tcb tcbs
     newtail <- mapM ((flip f) tcb) tcbs
     rest <- saturateCSpaces matrix objDict newtail
     return $ head : rest
  where
    f (tcbID, cspace) (tcbID', cspace') =
      do
        b <- isLeak matrix objDict tcbID' tcbID
        if b
          then
            let acc = cspace `Set.union` cspace' in acc `seq` return (tcbID, acc)
          else
            return (tcbID, cspace)

directTCBConnections :: [(ObjID, Set Cap)] -> [(ObjID, ObjID)]
directTCBConnections [] = []
directTCBConnections ((tcbID, cspace) : tcbs) =
  foldr f [] tcbs ++ directTCBConnections tcbs
    where
      f (tcbID', cspace') acc =
        acc
        ++ let endpointSet = projectEndpoint cspace
               endpointSet' = projectEndpoint cspace'
               shareCSpace = sharesCSpace (projectCNode cspace) (projectCNode cspace')
               haveTCB = tcbID' `Set.member` projectTCB cspace
               hadByTCB = tcbID `Set.member` projectTCB cspace'
               outEndpointCon = grantReadOverlap endpointSet endpointSet'
               inEndpointCon = grantReadOverlap endpointSet' endpointSet
               outCon = shareCSpace || haveTCB || outEndpointCon
               inCon = shareCSpace || hadByTCB || inEndpointCon
           in case (inCon,outCon) of
             (False, False) -> []
             (False, True)  -> [(tcbID, tcbID')]
             (True, False)  -> [(tcbID', tcbID)]
             (True, True)   -> [(tcbID, tcbID'), (tcbID', tcbID)]

directTCBFlows :: [(ObjID, Set Cap)] -> [(ObjID, ObjID)]
directTCBFlows [] = []
directTCBFlows ((tcbID,  capSet) : tcbs) =
  foldr f [] tcbs  ++ directTCBFlows tcbs
    where
      f (tcbID', capSet') acc =
        acc
        ++ let endpointSet = projectEndpoint capSet
               endpointSet' = projectEndpoint capSet'
               frameSet = projectFrame capSet
               frameSet' = projectFrame capSet'
               asyncSet = projectAsync capSet
               asyncSet' = projectAsync capSet'

               endpointFlow = writeReadOverlap endpointSet endpointSet'
                              || writeReadOverlap endpointSet' endpointSet
               frameFlowOut = writeReadOverlap frameSet frameSet'
               frameFlowIn = writeReadOverlap frameSet' frameSet
               asyncFlowOut = writeReadOverlap asyncSet asyncSet'
               asyncFlowIn = writeReadOverlap asyncSet' asyncSet

               outCon = endpointFlow || frameFlowOut || asyncFlowOut
               inCon = endpointFlow || frameFlowIn || asyncFlowIn
           in case (inCon,outCon) of
             (False, False) -> []
             (False, True)  -> [(tcbID, tcbID')]
             (True, False)  -> [(tcbID', tcbID)]
             (True, True)   -> [(tcbID, tcbID'), (tcbID', tcbID)]

{- True if the first set of rights contains a grant to an object
and the second set of rights contains a read to that object -}
grantReadOverlap :: Set (Rights, ObjID) -> Set (Rights, ObjID) -> Bool
grantReadOverlap cSet cSet' =
  let grants = Set.fromList [objID | (Grant, objID) <- Set.toList cSet]
      reads = Set.fromList [objID | (Read, objID) <- Set.toList cSet']
  in not $ Set.null $ grants `Set.intersection` reads

{- True if the first set of rights contains a write to an object
and the second set of rights contains a read to that object -}
writeReadOverlap :: Set (Rights, ObjID) -> Set (Rights, ObjID) -> Bool
writeReadOverlap cSet cSet' =
  let writes = Set.fromList [objID | (Write, objID) <- Set.toList cSet]
      reads = Set.fromList [objID | (Read, objID) <- Set.toList cSet']
  in not $ Set.null $ writes `Set.intersection` reads

{- True if two sets of CNode objIDs intersect -}
sharesCSpace :: Set ObjID -> Set ObjID -> Bool
sharesCSpace cspace1 cspace2 = not $ Set.null $ cspace1 `Set.intersection` cspace2

warshall :: Matrix -> IO ()
warshall matrix =
  do ((min, _), (max, _)) <- getSize matrix
     forM_ [min..max] $ \k ->
       forM_ [min..max] $ \i ->
         forM_ [min..max] $ \j ->
         do mij <- isConnected matrix i j
            mik <- isConnected matrix i k
            mkj <- isConnected matrix k j
            when (mij || (mik && mkj)) $ connect matrix i j

authSharingFlows :: Matrix -> Matrix -> IO ()
authSharingFlows fromMatrix toMatrix =
  do ((min, _), (max, _)) <- getSize fromMatrix
     forM_ [min..max] $ \i ->
       forM_ [min..max] $ \j ->
       do x <- isConnected fromMatrix i j
          when x $ connect toMatrix i j >> connect toMatrix j i

removeReflexive :: Matrix -> IO ()
removeReflexive matrix =
  do ((min, _), (max, _)) <- getSize matrix
     forM_ [min..max] $ \i ->
       disconnect matrix i i

flowsTos = leaksTos

leaksTos matrix objDict leaks =
  forM_ leaks $ \(objID, objID') ->
    leaksTo matrix objDict objID objID'

{- Write to matrix that objID leaks to objID' -}
leaksTo matrix objDict objID objID' = connect matrix (indexOf objID) (indexOf objID')
  where
       indexOf x =
         Map.findWithDefault
           (error $ "died in leaksTo trying to find key " ++ show x)
           x objDict

isLeak matrix objDict objID objID' = isConnected matrix (indexOf objID) (indexOf objID')
  where
    indexOf x = Map.findWithDefault
           (error $ "died in isLeak trying to find key " ++ show x)
           x objDict

getCSpace :: ObjMap Word -> ObjID -> Set Cap
getCSpace objects tcbID = Set.fromList $ getCaps objects tcbID

{- Returns all the caps in a TCB's CSpace -}
getCaps :: ObjMap Word -> ObjID -> [Cap]
getCaps objects tcbID =
  case Map.findWithDefault e1 tcbID objects of
    TCB slots _ _ _ _ ->
      let cNodeCaps = [cap | (_, cap@(CNodeCap {})) <- Map.toList slots]
      in concat $ map (getCaps' objects Set.empty) cNodeCaps
    _ -> e2
  where
    e1 = error "cannot find tcbID in cnodesFrom"
    e2 = error "objID given in cnodesFrom is not TCB"

getCaps' :: ObjMap Word -> Set ObjID -> Cap -> [Cap]
getCaps' objects visited cap =
  case cap of
    CNodeCap {}
      | capObj cap `Set.member` visited -> []
      | otherwise -> cap : getCaps'' objects visited (capObj cap)
    _ -> [cap]

getCaps'' :: ObjMap Word -> Set ObjID -> ObjID -> [Cap]
getCaps'' objects visited cnodeID
  | cnodeID `Set.member` visited = []
  | otherwise =
         case Map.findWithDefault e cnodeID objects of
           CNode slots _ ->
             let caps = map snd $ Map.toList slots
                 newVisited = cnodeID `Set.insert` visited
             in concat $ map (getCaps' objects newVisited) caps
           kObj@(_) -> error ("non-CNode objID passed into getCaps'': " ++ show kObj)
    where
      e = error "could not find rootID in objects map in cSpaceFrom'"

getFrames :: ObjMap Word -> ObjID -> [Cap]
getFrames objects tcbID =
  case Map.findWithDefault e1 tcbID objects of
    TCB slots _ _ _ _ ->
      let pdCaps = [cap | (_, cap@(PDCap _ _)) <- Map.toList slots]
      in concat $ map (getFrames' objects Set.empty) pdCaps
    _ -> e2
  where
    e1 = error "cannot find tcbID in getFrames"
    e2 = error "object ID given in getFrames is not TCB"

getFrames' :: ObjMap Word -> Set ObjID -> Cap -> [Cap]
getFrames' objects visited cap =
  case cap of
    PDCap capObj _
      | capObj `Set.member` visited -> []
      | otherwise -> cap : getFrames'' objects visited capObj
    PTCap capObj _
      | capObj `Set.member` visited -> []
      | otherwise -> cap : getFrames'' objects visited capObj
    FrameCap _ _ _ _ _ ->
      [cap]
    _ -> []

getFrames'' :: ObjMap Word -> Set ObjID -> ObjID -> [Cap]
getFrames'' objects visited objID
  | objID `Set.member` visited = []
  | otherwise =
    case Map.findWithDefault e objID objects of
      PD slots ->
        let caps = map snd $ Map.toList slots
            newVisited = objID `Set.insert` visited
        in concat $ map (getFrames' objects newVisited) caps
      PT slots ->
        let caps = map snd $ Map.toList slots
            newVisited = objID `Set.insert` visited
        in concat $ map (getFrames' objects newVisited) caps
      _ -> []
  where
    e = error "could not find objID in objects map in getFrames''"
