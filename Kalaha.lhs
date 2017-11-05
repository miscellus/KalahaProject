---
toc: 1
numbersections: true
geometry: margin=2.5cm
title: Programming Languages (Project 1)
author: Full Name (sdulogin16)
date: November 13, 2017
abstract: |
    The goal of this project is ...
---

\newpage

The Kalaha game with parameters $(n,m)$
====

\begin{code}
module Kalaha where
import Data.List
import Data.Ord
import Debug.Trace

mtrace :: Show a => String -> a -> a
mtrace str arg = trace ('<':str ++ ": " ++ show arg ++ ">\n") arg

type PitCount   = Int
type StoneCount = Int
data Kalaha     = Kalaha PitCount StoneCount deriving (Show, Read, Eq)

type KPos       = Int
type KState     = [Int]
type Player     = Bool

(alice, bob) = (False, True)

getPlayerOffset :: PitCount -> Player -> KPos
getPlayerOffset pitCount False  = 0
getPlayerOffset pitCount True   = pitCount+1

getPlayerStore :: PitCount -> Player -> KPos
getPlayerStore pitCount player = (getPlayerOffset pitCount player) + pitCount
\end{code}


The function `startStateImpl`
----

\begin{code}
startStateImpl :: Kalaha -> KState
startStateImpl (Kalaha pitCount stoneCount) = side ++ side
  where side = (replicate pitCount stoneCount) ++ [0]
\end{code}


The function `dropThenTake`
----


\begin{code}
dropThenTake :: Int -> Int -> [a] -> [a]
dropThenTake toDrop toTake list = take toTake (drop toDrop list)
\end{code}

The function `movesImpl`
----

\begin{code}
movesImpl :: Kalaha -> Player -> KState -> [KPos]
movesImpl game@(Kalaha pitCount _) player state =
  [index | (index, value) <- indexedPits, value > 0]
  where
    indexedPits = dropThenTake playerOffset pitCount indexedState
    indexedState = zip [0..] state
    playerOffset = getPlayerOffset pitCount player
\end{code}

The function `valueImpl`
----

\begin{code}
valueImpl :: Kalaha -> KState -> Double
valueImpl (Kalaha pitCount _) state = fromIntegral ( (value bob) - (value alice) )
  where value player = state !! (getPlayerStore pitCount player)
\end{code}

The function `moveImpl`
----

\begin{code}

moveImpl :: Kalaha -> Player -> KState -> KPos -> (Player, KState)
moveImpl (Kalaha pitCount _) player startState startPosition =
  (nextPlayer, stateAfterSowingAndLastMoveHandling)
  where
    -- Grab the stones initially
    (grabbedStones, stateAfterGrab) = getAndModifyElem (const 0) startPosition startState

    -- Skip full roundtrips
    -- Remember to save the last stone
    (numRoundtrips, stonesToSowe) = (grabbedStones-1) `quotRem` numSpotsToDropIn
    stateAfterRoundtrips = modifyElem (+(-numRoundtrips)) opponentStore $ map (+numRoundtrips) stateAfterGrab

    -- Rest of the sowing
    -- (nextPlayer, stateAfterLastStone) = putDownLastStone $ sowe stateAfterRoundtrips startPosition stonesToSowe
    (nextPlayer, stateAfterLastStone) = putDownLastStone $ iterate soweStep (stateAfterRoundtrips, firstDropSpot) !! stonesToSowe
      where soweStep (s, p) = (dropStone p s, getNextPosition p)
            firstDropSpot = getNextPosition startPosition

    -- If it was the last move (a whole side of pits got empty),
    -- move the remaining stones to the store on the same side
    stateAfterSowingAndLastMoveHandling = applyCond playersGetWhatTheyHaveLeft wasLastMove stateAfterLastStone
      where wasLastMove = or $ map (all (==0) . init) [aliceHalf, bobHalf]
            (aliceHalf, bobHalf) = splitAt (getPlayerOffset pitCount bob) stateAfterLastStone
  
    -- Indicies of the stores
    ownStore = getPlayerStore pitCount player
    opponentStore = getPlayerStore pitCount (not player)

    -- Common expressions and for conveyance of intent
    numSpots = 2*pitCount+2
    numSpotsToDropIn = numSpots-1
    dropStone = modifyElem (+1)

    putDownLastStone :: (KState, KPos) -> (Player, KState)
    putDownLastStone (state, position)
        | position == ownStore                = (player, statePutDownLastStone)
        | state !! position == 0 && onOwnSide = (not player, stateWinStonesOpposite)
        | otherwise                           = (not player, statePutDownLastStone)
        where
          statePutDownLastStone = dropStone position state
          onOwnSide = position < ownStore && position >= (ownStore - pitCount)
          stateWinStonesOpposite = modifyElem (+(stonesOpposite+1)) ownStore stateOppositeStonesTaken
            where
              (stonesOpposite, stateOppositeStonesTaken) = getAndModifyElem (const 0) otherSidePit state
              otherSidePit = numSpots - 2 - position -- NOTE: 'position' must (and should) be a pit

    getNextPosition :: KPos -> KPos
    getNextPosition position = (position + offset) `mod` numSpots
        where offset = if position == opponentStore then 2 else 1

    playersGetWhatTheyHaveLeft :: KState -> KState 
    playersGetWhatTheyHaveLeft state = foldl' f zeroed $ zip [0..] state
      where
        f accState (index, stones) = modifyElem (+stones) nextStore accState
          where nextStore = index - (index `mod` (pitCount+1)) + pitCount
        zeroed = map (const 0) state

\end{code}

The function `showGameImpl`
----


\begin{code}
showGameImpl :: Kalaha -> KState -> String
showGameImpl (Kalaha pitCount stoneCount) state =
  unlines $ map unwords [line1, line2, line3]
  where
    ((aliceSide, aliceKalaha), (bobSide, bobKalaha)) =
          applyToBothInPair (splitAt pitCount)
        $ splitAt (pitCount+1)
        $ map (leftPad ' ' maxLength . show) state

    line1 = emptySlot : (reverse $ bobSide)
    line2 = bobKalaha ++ (replicate pitCount emptySlot) ++ aliceKalaha
    line3 = emptySlot : (aliceSide)

    maxLength = length . show $ 2*pitCount*stoneCount
    emptySlot = replicate maxLength ' '
\end{code}

*Supporting Functions*:

\begin{code}

-- Replaces the element at index 'index' with the function 'op' applied to that element
-- returning the modified list
modifyElem :: (a -> a) -> Int -> [a] -> [a]
modifyElem _ _ [] = []
modifyElem op 0 (x:xs) = op x : xs
modifyElem op index (x:xs) = x : modifyElem op (index - 1) xs 

-- Replaces the element at index 'index' with the function 'op' applied to that element
-- returning a tuple of the original element and the modified list
getAndModifyElem :: (a -> a) -> Int -> [a] -> (a, [a])
getAndModifyElem op index list =
    case uncons r of
      Nothing      -> error "Index out of range"
      Just (x, xs) -> (x, l ++ (op x : xs))
    where
      (l, r) = splitAt index list

-- Pads a string on the left side with 'c' if it is shorter than length 'toLength'
leftPad :: Char -> Int -> String -> String
leftPad c toLength str = replicate padLength c ++ str
  where
    padLength = max 0 (toLength - (length str))

-- Conditionally applies a function to a value
applyCond :: (a -> a) -> Bool -> a -> a
applyCond function condition value = if condition then function value else value

applyToPair :: ((a -> b), (c -> d)) -> (a, c) -> (b, d)
applyToPair (f1, f2) (l, r) = (f1 l, f2 r)
applyToBothInPair f = applyToPair (f, f)
applyToSnd f = applyToPair (id, f)

\end{code}


Trees
====

\begin{code}
data Tree m v  = Node v [(m,Tree m v)] deriving (Eq)

testTree =
  Node 3 [
    (0, Node 4 [
        (0, Node 5 []), (1, Node 6 []), (2, Node 7 [])
    ])
    ,(1, Node 9 [
        (0, Node 10 [])
    ])]

instance (Show m, Show v) => Show (Tree m v) where
  show t = unlines $ draw t
    where
      draw :: (Show m, Show v) => Tree m v -> [String]
      draw (Node value children) = show value : drawSubTrees children
        where
          drawSubTrees [] = []
          drawSubTrees [(branchID, t)] = let (x:xs) = draw t in
              "|" : shift "`- " "    " (('{': show branchID ++ "} => " ++ x) : xs)
          drawSubTrees ((branchID, t):ts) = let (x:xs) = draw t in
              "|" : shift "+- " "|   " (('{': show branchID ++ "} => " ++ x) : xs) ++ drawSubTrees ts

          shift first other = zipWith (++) (first : repeat other)

\end{code}

The function `takeTree`
----

\begin{code}
takeTree :: Int -> Tree m v -> Tree m v
takeTree 0 (Node value _) = Node value []
takeTree _ node@(Node _ []) = node
takeTree depth (Node value subTrees) = Node value $ map f subTrees
  where f (m, subTree) = (m, takeTree (depth - 1) subTree)
\end{code}

The Minimax algorithm
====
\begin{code}
data Game s m = Game {
    startState    :: s,
    showGame      :: s -> String,
    move          :: Player -> s -> m -> (Player,s),
    moves         :: Player -> s -> [m],
    value         :: Player -> s -> Double}

kalahaGame :: Kalaha -> Game KState KPos
kalahaGame k = Game {
    startState = startStateImpl k,
    showGame   = showGameImpl k,
    move       = moveImpl k,
    moves      = movesImpl k,
    value      = const (valueImpl k)}

startTree :: Game s m -> Player -> Tree m (Player,Double) 
startTree game player = tree game (player, startState game)
    
testKalahaTree = startTree (kalahaGame (Kalaha 2 2)) False
\end{code}
The function `tree`
----

    
\begin{code}

tree :: Game s m -> (Player, s) -> Tree m (Player, Double)
tree game (player, state) = Node (player, theValue) (map nodeForMove theMoves)
  where
    theValue = value game player state
    theMoves = moves game player state
    nodeForMove aMove = (aMove, tree game outcomeOfMove)
      where
        outcomeOfMove = move game player state aMove
\end{code}
The function `minimax`
----

    
\begin{code}
minimax :: Tree m (Player, Double) -> (Maybe m, Double)
minimax (Node (_, theValue) []) = (Nothing, theValue) 
minimax (Node (player, _) branches) = applyToPair (Just, snd) $ chooseBestBy (comparing (snd.snd)) subResults
  where chooseBestBy = if player then maximumBy else minimumBy
        subResults = map (applyToSnd minimax) branches

\end{code}

The function `minimaxAlphaBeta`
----

\begin{code}
type AlphaBeta = (Double,Double)

minimaxAlphaBeta :: AlphaBeta -> Tree m (Player, Double) -> (Maybe m, Double)
minimaxAlphaBeta = undefined
\end{code}

Testing and sample executions
====