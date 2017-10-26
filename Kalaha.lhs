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

type PitCount   = Int
type StoneCount = Int
data Kalaha     = Kalaha PitCount StoneCount deriving (Show, Read, Eq)

type KPos       = Int
type KState     = [Int]
type Player     = Bool

(alice, bob) = (False, True)

getPlayerOffset :: PitCount -> Player -> Int
getPlayerOffset pitCount player =
    case player of
        alice   -> 0
        bob     -> pitCount+1

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
dropThenTake n m xs = take n (drop m xs)
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
modifyElement modifyOp 0 (x:xs) = ((modifyOp x) : xs, x)
modifyElement modifyOp index (x:xs) = x : modifyElement modifyOp (index - 1) xs

putStones :: KState -> KPos -> StoneCount -> KState
putStones state position amount = fst (modifyElement (+amount) position state)

moveImpl :: Kalaha -> Player -> KState -> KPos -> (Player, KState)
moveImpl game@(Kalaha pitCount stoneCount) player startState startPosition =
    sowe player stateAfterGrab startPosition stones
    where
        (stateHead, stones : stateTail) = splitAt startPosition startState
        stateAfterGrab = stateHead ++ (0 : stateTail)

        sowe :: Player -> KState -> KPos -> StoneCount -> (Player, KState)
        sowe player state position 1 =
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
                    modifyElement (+(stonesOppositeSide+1)) ownStore state'
                    where
                        (state', stonesOppositeSide) = modifyElement (const 0) otherSidePit state
                        otherSidePit = 2*(pitCount - position)
        sowe player state position stonesLeft =
            sowe player nextState nextPosition (stonesLeft-1)
            where
                nextPosition = getNextPosition position player
                nextState = putStones state position 1

        getNextPosition :: KPos -> Player -> KPos
        getNextPosition position player
            | nextPosition == opponentStore = getNextPosition nextPosition player
            | otherwise                     = nextPosition
            where
                nextPosition = wrapIndex (position + 1)
                opponentStore = getPlayerStore pitCount (not player)

        wrapIndex :: Int -> Int
        wrapIndex index = index `mod` (2*pitCount + 2)

\end{code}


The function `showGameImpl`
----


\begin{code}
showGameImpl :: Kalaha -> KState -> String
showGameImpl g@(Kalaha n m) xs = undefined
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
