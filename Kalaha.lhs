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

myTrace arg = traceShow arg arg

type PitCount   = Int
type StoneCount = Int
data Kalaha     = Kalaha PitCount StoneCount deriving (Show, Read, Eq)

type KPos       = Int
type KState     = [Int]
type Player     = Bool

(alice, bob) = (False, True)

getPlayerOffset :: PitCount -> Player -> Int
getPlayerOffset pitCount player = if player == alice then 0 else pitCount + 1

getPlayerStore :: PitCount -> Player -> Int
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
dropThenTake n m xs = take m (drop n xs)
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

modifyElement :: (a -> a) -> Int -> [a] -> ([a], a)
modifyElement modifyOp index list =
    case uncons r of
        Nothing      -> error "Index out of range"
        Just (x, xs) -> (l ++ (modifyOp x : xs), x)
    where
        (l, r) = splitAt index list

putStones :: KState -> KPos -> StoneCount -> KState
putStones state position amount = fst (modifyElement (+amount) position state)

moveImpl :: Kalaha -> Player -> KState -> KPos -> (Player, KState)
moveImpl (Kalaha pitCount _) player startState startPosition =
    sowe stateAfterGrab (getNextPosition startPosition) stones
    where
        (stateAfterGrab, stones) = modifyElement (const 0) startPosition startState

        sowe :: KState -> KPos -> StoneCount -> (Player, KState)
        sowe state position 1 =
            if position == ownStore
            then (player, stateFinalStone)
            else
                if stoneCountInLastPit > 0
                then (not player, stateFinalStone)
                else (not player, stateOpponentStonesPutInOwnStore)
            where
                ownStore = getPlayerStore pitCount player
                stoneCountInLastPit = state !! position
                stateFinalStone = putStones state position 1
                stateOpponentStonesPutInOwnStore =
                    putStones stateOpponentStonesTaken ownStore (stonesTakenFromOpponent+1)
                    where
                        (stateOpponentStonesTaken, stonesTakenFromOpponent) = modifyElement (const 0) otherSidePit state
                        otherSidePit = 2*pitCount - position

        sowe state position stonesLeft =
            sowe nextState nextPosition (stonesLeft-1)
            where
                nextState = putStones state position 1
                nextPosition = getNextPosition position

        getNextPosition :: KPos -> KPos
        getNextPosition position = head $ dropWhile (== opponentStore) $ tail $ iterate toNextIndex position
            where
                toNextIndex = (`mod` (2*pitCount + 2)).(+1)
                opponentStore = getPlayerStore pitCount (not player)
                        

\end{code}


The function `showGameImpl`
----


\begin{code}
showGameImpl :: Kalaha -> KState -> String
showGameImpl (Kalaha pitCount stoneCount) state =
    unlines $ map unwords [line1, line2, line3]
    where
        (aliceHalf, bobHalf) = splitAt (getPlayerOffset pitCount bob) $ map (padToMaxLength.show) state
        line1 = emptySlot : (reverse $ init bobHalf)
        line2 = bobKalaha : (replicate pitCount emptySlot) ++ [aliceKalaha]
        line3 = emptySlot : (init aliceHalf)
        aliceKalaha = last aliceHalf
        bobKalaha = last bobHalf

        maxLength = length $ show $ 2*pitCount*stoneCount
        emptySlot = replicate maxLength ' '

        padToMaxLength :: String -> String
        padToMaxLength str = replicate padLength ' ' ++ str
            where
                padLength = max 0 (maxLength - (length str))
\end{code}


Trees
====

\begin{code}
data Tree m v  = Node v [(m,Tree m v)] deriving (Eq, Show)
\end{code}

The function `takeTree`
----

\begin{code}
takeTree :: Int -> Tree m v -> Tree m v
takeTree = undefined
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
startTree g p = tree g (p, startState g)
    
\end{code}
The function `tree`
----

    
\begin{code}
tree      :: Game s m -> (Player, s) -> Tree m (Player, Double)
tree = undefined
    
\end{code}
The function `minimax`
----

    
\begin{code}
minimax   :: Tree m (Player, Double) -> (Maybe m, Double)
minimax = undefined
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
