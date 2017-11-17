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

-- Because I feel sorry for Alice and Bob,
-- they never get to just sit down together and play a nice game of Kalaha;
-- somehow, they are always trying to communicate long distance
-- while being eavesdropped upon, or having their messages manipulated.
-- Well not this time!
(alice, bob) = (False, True)
\end{code}


The function `startStateImpl`
----

\begin{code}
startStateImpl :: Kalaha -> KState
startStateImpl (Kalaha pitCount stoneCount) = side ++ side
  where side = (replicate pitCount stoneCount) ++ [0]
\end{code}

The function `movesImpl`
----

\begin{code}
movesImpl :: Kalaha -> Player -> KState -> [KPos]
movesImpl (Kalaha pitCount _) player state =
  [index | (index, value) <- indexedPlayerSide, value > 0]
  where
    indexedPlayerSide = take pitCount . drop playerOffset $ zip [0..] state
    playerOffset = if player then pitCount+1 else 0
\end{code}

The function `valueImpl`
----

\begin{code}
valueImpl :: Kalaha -> KState -> Double
valueImpl (Kalaha pitCount _) state = fromIntegral $ score bob - score alice
  where score player = state !! ( pitCount + if player then pitCount+1 else 0 )
\end{code}

The function `moveImpl`
----

\begin{code}
moveImpl :: Kalaha -> Player -> KState -> KPos -> (Player, KState)
moveImpl (Kalaha pitCount stoneCount) player startState startPosition =
  let
    (grabbedStones, stateAfterGrab) = grabStones startPosition startState
    (numRoundtrips, stonesToSowe)   = (grabbedStones-1) `quotRem` (boardSize-1)
    stateAfterRoundtrips            = addRoundtrips numRoundtrips stateAfterGrab
    sowes                           = iterate sowe (stateAfterRoundtrips, positionAfter startPosition)
  in
    applyToSnd cleanupIfGameOver $ dropLastStone $ sowes !! stonesToSowe
  where    
    -- Common expressions and/or for conveying intent
    boardSize     = 2*pitCount+2
    (ownStore, opponentStore) = onCondition (player == bob) swapTuple $ (pitCount, 2*pitCount+1)
    dropStone     = modifyAtIndex (+1)
    grabStones    = getAndModifyAtIndex (const 0)

    addRoundtrips :: Int -> KState -> KState
    addRoundtrips n = modifyAtIndex (subtract n) opponentStore . map (+n)

    positionAfter :: KPos -> KPos
    positionAfter = head . filter (/= opponentStore) . tail . iterate ((`mod` boardSize).(+1))

    sowe :: (KState, KPos) -> (KState, KPos)
    sowe (state, position) = (dropStone position state, positionAfter position)
    
    cleanupIfGameOver :: KState -> KState
    cleanupIfGameOver state = if gameOver
      then (replicate pitCount 0) ++ aliceFinalScore : (replicate pitCount 0) ++ [bobFinalScore]
      else state
      where
        -- It was the last move if a whole side of pits got empty,
        gameOver = (False, False) /= (applyToBothInPair (all(==0) . init) $ splitAt (pitCount+1) state)
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
        stateWinStonesOpposite = modifyAtIndex (+(stonesOpposite+1)) ownStore stateOppositeStonesTaken
          where
            (stonesOpposite, stateOppositeStonesTaken) = grabStones otherSidePit state
            otherSidePit = boardSize - 2 - position -- NOTE: 'position' must (and should) be a pit
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

\end{code}

Show instance for Tree derived from similar implementation in Data.Tree module.
It is used solely for debugging.
NB: I have shared this implementation with some of my peers, so it might show up elsewhere.
\begin{code}
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
  $ getBestBy (preference `on` (snd.snd))
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
      let r1@(_, v1) = (Just m1, snd $ minimaxAlphaBeta (a, b) firstSubTree)
          ((a', b'), preference) | maximizer = ((max a v1, b), (>))
                                 | otherwise = ((a, min b v1), (<))
      in if b' <= a'
          then r1
          else getBestBy (preference `on` snd) [r1, minimaxAlphaBeta (a', b') $ Node (maximizer, value) restOfBranches]
\end{code}

Helper Functions
====

\begin{code}
on :: (a -> a -> b) -> (c -> a) -> c -> c -> b
on f t = \x y -> f (t x) (t y)

\end{code}

Replaces the element at index 'index' with the function 'op' applied to that element
returning a tuple of the original element and the modified list
\begin{code}
getAndModifyAtIndex :: (a -> a) -> Int -> [a] -> (a, [a])
getAndModifyAtIndex op index list =
  case splitAt index list of
      (list, []) -> error "Index out of range"
      (ys, x:xs) -> (x, ys ++ (op x : xs))
\end{code}

Same as 'getAndModifyAtIndex' but only returns the modified list.
\begin{code}
modifyAtIndex :: (a -> a) -> Int -> [a] -> [a]
modifyAtIndex o i l = snd $ getAndModifyAtIndex o i l
\end{code}

Pads a string on the left side with 'c' if it is shorter than length 'toLength'.
\begin{code}
leftPad :: Char -> Int -> String -> String
leftPad c toLength str = replicate padLength c ++ str
  where padLength = max 0 (toLength - (length str))
\end{code}

Makes a function only do something when a condition holds.
\begin{code}
onCondition :: Bool -> (a -> a) -> (a -> a)
onCondition False _ = id
onCondition _ f = f
\end{code}

Miscellaneous functions facilitating function application in tuples.
\begin{code}
applyToPair :: ((a -> b), (c -> d)) -> (a, c) -> (b, d)
applyToPair (f1, f2) (l, r) = (f1 l, f2 r)
applyToBothInPair f = applyToPair (f, f)
applyToFst f = applyToPair (f, id)
applyToSnd f = applyToPair (id, f)
swapTuple (a,b) = (b,a)
\end{code}

Function that takes the *first*, best element in a list by comparing each
element to the best found-so-far element according to a binary comparison
operator that returns whether one is preferred over the other.
\begin{code}
getBestBy :: (a -> a -> Bool) -> [a] -> a
getBestBy _ [] = error "Cannot get element from empty list"
getBestBy cmp (x:xs) = foldl f x xs
  where f best next | next `cmp` best = next
                    | otherwise       = best
\end{code}

Testing and sample executions
====

```
Prelude> :l KalahaTest
[1 of 2] Compiling Kalaha           ( Kalaha.lhs, interpreted )
[2 of 2] Compiling Main             ( KalahaTest.hs, interpreted )
Ok, modules loaded: Main, Kalaha.
*Main> main
=== prop_startStateLen from KalahaTest.hs:33 ===
+++ OK, passed 100 tests.

=== prop_startStateSum from KalahaTest.hs:37 ===
+++ OK, passed 100 tests.

=== prop_startStateValue from KalahaTest.hs:41 ===
+++ OK, passed 100 tests.

=== prop_startStateSymmetric from KalahaTest.hs:47 ===
+++ OK, passed 100 tests.

=== prop_valueSymmetric from KalahaTest.hs:51 ===
+++ OK, passed 100 tests.

=== prop_movesSymmetric from KalahaTest.hs:58 ===
+++ OK, passed 100 tests.

=== prop_moveSymmetric from KalahaTest.hs:63 ===
+++ OK, passed 100 tests.

=== prop_moveDoesntChangeSum from KalahaTest.hs:74 ===
+++ OK, passed 100 tests.

=== prop_specificMoves from KalahaTest.hs:83 ===
+++ OK, passed 1 tests.

True
*Main> :l GameStrategiesTestOrg
[1 of 2] Compiling Kalaha           ( Kalaha.lhs, interpreted )
[2 of 2] Compiling Main             ( GameStrategiesTestOrg.hs, interpreted )
Ok, modules loaded: Main, Kalaha.
*Main> main
=== prop_minimaxPicksWinningStrategyForNim from GameStrategiesTestOrg.hs:42 ===
+++ OK, passed 100 tests.

=== prop_alphabetaSolutionShouldEqualMinimaxSolution from GameStrategiesTestOrg.hs:46 ===
+++ OK, passed 100 tests.

=== prop_boundedAlphabetaIsOptimalForNim from GameStrategiesTestOrg.hs:50 ===
+++ OK, passed 100 tests.

=== prop_pruningShouldBeDoneCorrectlyForStaticTestTrees from GameStrategiesTestOrg.hs:56 ===
+++ OK, passed 1 tests.

True
*Main>
```