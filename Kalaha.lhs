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
dropThenTake d t = take t . drop d
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
moveImpl (Kalaha pitCount stoneCount) player startState startPosition =
  applyToSnd gameEndingMoveCleanup  $  dropLastStone  $  sowes !! stonesToSowe
  where
    -- Grab the stones initially
    (grabbedStones, stateAfterGrab) = grabStones startPosition startState

    -- Skip full roundtrips
    -- Remember to save the last stone
    (numRoundtrips, stonesToSowe) = (grabbedStones-1) `quotRem` (boardSize-1)
    stateAfterRoundtrips = modifyElem (+(-numRoundtrips)) opponentStore $ map (+numRoundtrips) stateAfterGrab

    -- Rest of the sowing
    
    
    sowes = iterate sowe (stateAfterRoundtrips, getNextPosition startPosition)
      where sowe (s, p) = (dropStone p s, getNextPosition p)
    
    -- Common expressions and for conveyance of intent
    boardSize     = 2*pitCount+2
    ownStore      = getPlayerStore pitCount player
    opponentStore = getPlayerStore pitCount (not player)
    dropStone     = modifyElem (+1)
    grabStones    = getAndModifyElem (const 0)

    gameEndingMoveCleanup :: KState -> KState
    gameEndingMoveCleanup state
      | not wasLastMove = state
      |     wasLastMove = (replicate pitCount 0) ++ aliceFinalScore : (replicate pitCount 0) ++ [bobFinalScore]
      where
        -- It was the last move if a whole side of pits got empty,
        wasLastMove = (False, False) /= (applyToBothInPair (all(==0) . init) $ splitAt (pitCount+1) state)
        aliceFinalScore = sum $ take (pitCount+1) state
        bobFinalScore = (2*pitCount*stoneCount - aliceFinalScore) -- Since we have a constant number of stones total

    dropLastStone :: (KState, KPos) -> (Player, KState)
    dropLastStone (state, position)
        | position == ownStore                = (player, dropStone position state)
        | state !! position == 0 && onOwnSide = (not player, stateWinStonesOpposite)
        | otherwise                           = (not player, dropStone position state)
        where
          statePutDownLastStone = dropStone position state
          onOwnSide = position < ownStore && position >= (ownStore - pitCount)
          stateWinStonesOpposite = modifyElem (+(stonesOpposite+1)) ownStore stateOppositeStonesTaken
            where
              (stonesOpposite, stateOppositeStonesTaken) = grabStones otherSidePit state
              otherSidePit = boardSize - 2 - position -- NOTE: 'position' must (and should) be a pit

    getNextPosition :: KPos -> KPos
    getNextPosition p = toNext $ applyCond toNext (toNext p == opponentStore) p
      where toNext = (`mod` boardSize).(+1)

specificMoves = let g = Kalaha 6 6 in [
    moveImpl g False [6,6,6,6,6,6,0,6,6,6,6,6,6,0] 0 == (False,[0,7,7,7,7,7,1,6,6,6,6,6,6,0]),
    moveImpl g False [0,7,7,7,7,7,1,6,6,6,6,6,6,0] 3 == (True,[0,7,7,0,8,8,2,7,7,7,7,6,6,0]),
    moveImpl g True [0,7,7,0,8,8,2,7,7,7,7,6,6,0] 9 == (False,[1,8,8,0,8,8,2,7,7,0,8,7,7,1]),
    moveImpl g False [1,8,8,0,8,8,2,7,7,0,8,7,7,1] 1 == (True,[1,0,9,1,9,9,3,8,8,1,8,7,7,1]),
    moveImpl g True [1,0,9,1,9,9,3,8,8,1,8,7,7,1] 10 == (False,[2,1,10,2,10,9,3,8,8,1,0,8,8,2]),
    moveImpl g False [2,1,10,2,10,9,3,8,8,1,0,8,8,2] 4 == (True,[3,2,10,2,0,10,4,9,9,2,1,9,9,2]),
    moveImpl g True [3,2,10,2,0,10,4,9,9,2,1,9,9,2] 9 == (False,[3,2,10,2,0,10,4,9,9,0,2,10,9,2]),
    moveImpl g False [3,2,10,2,0,10,4,9,9,0,2,10,9,2] 3 == (True,[3,2,10,0,1,11,4,9,9,0,2,10,9,2]),
    moveImpl g True [3,2,10,0,1,11,4,9,9,0,2,10,9,2] 7 == (False,[4,3,11,0,1,11,4,0,10,1,3,11,10,3]),
    moveImpl g False [4,3,11,0,1,11,4,0,10,1,3,11,10,3] 5 == (True,[5,4,12,0,1,0,8,1,11,0,4,12,11,3]),
    moveImpl g True [5,4,12,0,1,0,8,1,11,0,4,12,11,3] 7 == (False,[5,4,12,0,1,0,8,0,12,0,4,12,11,3]),
    moveImpl g False [5,4,12,0,1,0,8,0,12,0,4,12,11,3] 1 == (True,[5,0,13,1,2,0,9,0,12,0,4,12,11,3]),
    moveImpl g True [5,0,13,1,2,0,9,0,12,0,4,12,11,3] 8 == (False,[6,1,14,2,3,0,9,0,0,1,5,13,12,6]),
    moveImpl g False [6,1,14,2,3,1,9,1,0,1,5,13,12,4] 5 == (False,[6,1,14,2,3,0,10,1,0,1,5,13,12,4]),
    moveImpl g False [6,1,14,2,3,0,10,1,0,1,5,13,12,4] 0 == (False,[0,2,15,3,4,1,11,1,0,1,5,13,12,4]),
    moveImpl g False [0,2,15,3,4,1,11,1,0,1,5,13,12,4] 5 == (False,[0,2,15,3,4,0,12,1,0,1,5,13,12,4]),
    moveImpl g False [0,2,15,3,4,0,12,1,0,1,5,13,12,4] 3 == (False,[0,2,15,0,5,1,13,1,0,1,5,13,12,4]),
    moveImpl g False [0,2,15,0,5,1,13,1,0,1,5,13,12,4] 5 == (False,[0,2,15,0,5,0,14,1,0,1,5,13,12,4]),
    moveImpl g False [0,2,15,0,5,0,14,1,0,1,5,13,12,4] 2 == (True,[1,3,1,2,7,1,15,2,1,2,6,14,13,4]),
    moveImpl g True [1,3,1,2,7,1,15,2,1,2,6,14,13,4] 12 == (False,[0,4,2,3,8,2,15,3,2,3,7,15,0,8]),
    moveImpl g False [0,4,2,3,8,2,15,3,2,3,7,15,0,8] 3 == (False,[0,4,2,0,9,3,16,3,2,3,7,15,0,8]),
    moveImpl g False [0,4,2,0,9,3,16,3,2,3,7,15,0,8] 4 == (True,[0,4,2,0,0,4,19,4,3,4,8,16,0,8]),
    moveImpl g True [0,4,2,0,0,4,19,4,3,4,8,16,0,8] 9 == (True,[0,4,2,0,0,4,19,4,3,0,9,17,1,9]),
    moveImpl g True [0,4,2,0,0,4,19,4,3,0,9,17,1,9] 12 == (True,[0,4,2,0,0,4,19,4,3,0,9,17,0,10]),
    moveImpl g True [0,4,2,0,0,4,19,4,3,0,9,17,0,10] 11 == (False,[2,6,3,1,1,5,19,5,4,1,10,1,2,12]),
    moveImpl g False [2,6,3,1,1,5,19,5,4,1,10,1,2,12] 0 == (True,[0,7,4,1,1,5,19,5,4,1,10,1,2,12]),
    moveImpl g True [0,7,4,1,1,5,19,5,4,1,10,1,2,12] 8 == (False,[0,7,4,1,1,5,19,5,0,2,11,2,3,12]),
    moveImpl g False [0,7,4,1,1,5,19,5,0,2,11,2,3,12] 2 == (False,[0,7,0,2,2,6,20,5,0,2,11,2,3,12]),
    moveImpl g False [0,7,0,2,2,6,20,5,0,2,11,2,3,12] 1 == (True,[0,0,1,3,3,7,21,6,1,2,11,2,3,12]),
    moveImpl g True [0,0,1,3,3,7,21,6,1,2,11,2,3,12] 11 == (True,[0,0,1,3,3,7,21,6,1,2,11,0,4,13]),
    moveImpl g True [0,0,1,3,3,7,21,6,1,2,11,0,4,13] 7 == (True,[0,0,1,3,3,7,21,0,2,3,12,1,5,14]),
    moveImpl g True [0,0,1,3,3,7,21,0,2,3,12,1,5,14] 10 == (False,[1,1,2,4,4,8,21,1,3,4,0,2,6,15]),
    moveImpl g False [1,1,2,4,4,8,21,1,3,4,0,2,6,15] 5 == (True,[2,1,2,4,4,0,22,2,4,5,1,3,7,15]),
    moveImpl g True [2,1,2,4,4,0,22,2,4,5,1,3,7,15] 8 == (False,[2,1,2,4,4,0,22,2,0,6,2,4,8,15]),
    moveImpl g False [2,1,2,4,4,0,22,2,0,6,2,4,8,15] 3 == (True,[2,1,2,0,5,1,23,3,0,6,2,4,8,15]),
    moveImpl g True [2,1,2,0,5,1,23,3,0,6,2,4,8,15] 9 == (False,[3,2,2,0,5,1,23,3,0,0,3,5,9,16]),
    moveImpl g False [3,2,2,0,5,1,23,3,0,0,3,5,9,16] 5 == (False,[3,2,2,0,5,0,24,3,0,0,3,5,9,16]),
    moveImpl g False [3,2,2,0,5,0,24,3,0,0,3,5,9,16] 4 == (True,[3,2,2,0,0,1,25,4,1,1,3,5,9,16]),
    moveImpl g True [3,2,2,0,0,1,25,4,1,1,3,5,9,16] 10 == (True,[3,2,2,0,0,1,25,4,1,1,0,6,10,17]),
    moveImpl g True [3,2,2,0,0,1,25,4,1,1,0,6,10,17] 9 == (False,[3,2,0,0,0,1,25,4,1,0,0,6,10,20]),
    moveImpl g False [3,2,0,0,0,1,25,4,1,0,0,6,10,20] 5 == (False,[3,2,0,0,0,0,26,4,1,0,0,6,10,20]),
    moveImpl g False [3,2,0,0,0,0,26,4,1,0,0,6,10,20] 1 == (True,[3,0,1,0,0,0,27,4,1,0,0,6,10,20]),
    moveImpl g True [3,0,1,0,0,0,27,4,1,0,0,6,10,20] 8 == (False,[3,0,1,0,0,0,27,4,0,0,0,6,10,21]),
    moveImpl g False [3,0,1,0,0,0,27,4,0,0,0,6,10,21] 2 == (True,[3,0,0,0,0,0,28,4,0,0,0,6,10,21]),
    moveImpl g True [3,0,0,0,0,0,28,4,0,0,0,6,10,21] 7 == (False,[3,0,0,0,0,0,28,0,1,1,1,7,10,21]),
    moveImpl g False [3,0,0,0,0,0,28,0,1,1,1,7,10,21] 0 == (True,[0,1,1,0,0,0,30,0,1,0,1,7,10,21]),
    moveImpl g True [0,1,1,0,0,0,30,0,1,0,1,7,10,21] 11 == (False,[1,2,2,1,1,0,30,0,1,0,1,0,11,22]),
    moveImpl g False [1,2,2,1,1,0,30,0,1,0,1,0,11,22] 3 == (True,[1,2,2,0,2,0,30,0,1,0,1,0,11,22]),
    moveImpl g True [1,2,2,0,2,0,30,0,1,0,1,0,11,22] 8 == (False,[1,2,2,0,2,0,30,0,0,0,1,0,11,23]),
    moveImpl g False [1,2,2,0,2,0,30,0,0,0,1,0,11,23] 1 == (True,[1,0,3,0,2,0,31,0,0,0,1,0,11,23]),
    moveImpl g True [1,0,3,0,2,0,31,0,0,0,1,0,11,23] 10 == (False,[1,0,3,0,2,0,31,0,0,0,0,0,11,24]),
    moveImpl g False [1,0,3,0,2,0,31,0,0,0,0,0,11,24] 4 == (False,[1,0,3,0,0,1,32,0,0,0,0,0,11,24]),
    moveImpl g False [1,0,3,0,0,1,32,0,0,0,0,0,11,24] 5 == (False,[1,0,3,0,0,0,33,0,0,0,0,0,11,24]),
    moveImpl g False [1,0,3,0,0,0,33,0,0,0,0,0,11,24] 2 == (True,[1,0,0,1,1,0,34,0,0,0,0,0,11,24]),
    moveImpl g True [1,0,0,1,1,0,34,0,0,0,0,0,11,24] 12 == (False,[2,1,0,2,2,1,34,1,1,1,0,0,0,27]),
    moveImpl g False [2,1,0,2,2,1,34,1,1,1,0,0,0,27] 5 == (False,[2,1,0,2,2,0,35,1,1,1,0,0,0,27]),
    moveImpl g False [2,1,0,2,2,0,35,1,1,1,0,0,0,27] 4 == (False,[2,1,0,2,0,1,36,1,1,1,0,0,0,27]),
    moveImpl g False [2,1,0,2,0,1,36,1,1,1,0,0,0,27] 5 == (False,[2,1,0,2,0,0,37,1,1,1,0,0,0,27]),
    moveImpl g False [2,1,0,2,0,0,37,1,1,1,0,0,0,27] 3 == (True,[2,1,0,0,1,0,39,0,1,1,0,0,0,27]),
    moveImpl g True [2,1,0,0,1,0,39,0,1,1,0,0,0,27] 8 == (False,[2,1,0,0,1,0,39,0,0,2,0,0,0,27]),
    moveImpl g False [2,1,0,0,1,0,39,0,0,2,0,0,0,27] 1 == (True,[2,0,0,0,1,0,40,0,0,2,0,0,0,27]),
    moveImpl g True [2,0,0,0,1,0,40,0,0,2,0,0,0,27] 9 == (False,[2,0,0,0,1,0,40,0,0,0,1,0,0,28]),
    moveImpl g False [2,0,0,0,1,0,40,0,0,0,1,0,0,28] 0 == (True,[0,0,0,0,0,0,44,0,0,0,0,0,0,28])
  ]

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
applyToFst f = applyToPair (f, id)
applyToSnd f = applyToPair (id, f)

getBestBy :: (b -> a) -> (a -> a -> Bool) -> [b] -> b
getBestBy _ _ [] = error "Cannot get element from empty list"
getBestBy t betterThan (x:xs) = foldl f x xs
  where f best next = if t next `betterThan` t best then next else best

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
takeTree 0 (Node v _) = Node v []
takeTree _ (Node v []) = Node v []
takeTree d (Node v ts) = Node v $ map (applyToSnd $ takeTree (d - 1)) ts
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
minimax (Node (_, value) []) = (Nothing, value) 
minimax (Node (maximizer, _) branches) = 
    applyToPair (Just, snd)
  $ getBestBy (snd.snd) preference
  $ map (applyToSnd minimax) branches
  where preference = if maximizer then (>) else (<)

\end{code}

The function `minimaxAlphaBeta`
----

\begin{code}

type AlphaBeta = (Double,Double)

-- Alpha: best already explored option along the path to the root for the maximizer
-- Beta: best already explored option along the path to the root for the minimizer
minimaxAlphaBeta :: AlphaBeta -> Tree m (Player, Double) -> (Maybe m, Double)
minimaxAlphaBeta (a, b) (Node (maximizer, value) branches) = 
  case branches of
    [] -> (Nothing, value)
    [(m, onlyOneSubTree)] -> (Just m, snd $ minimaxAlphaBeta (a, b) onlyOneSubTree)
    ((m1, firstSubTree):restOfBranches) ->
      let
        result1@(_, v1) = (Just m1, snd $ minimaxAlphaBeta (a, b) firstSubTree)
        ((a', b'), preference)
          | maximizer = ((max a v1, b), (>))
          | otherwise = ((a, min b v1), (<))
      in if b' <= a'
          then result1
          else getBestBy snd preference [result1, minimaxAlphaBeta (a', b') (Node (maximizer, value) restOfBranches)]
\end{code}

Testing and sample executions
====