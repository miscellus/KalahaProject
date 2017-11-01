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

getPlayerOffset :: PitCount -> Player -> KPos
getPlayerOffset pitCount player = if player == alice
    then 0
    else pitCount + 1

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

condApply :: Bool -> (a -> a) -> a -> a
condApply condition function value = if condition then function value else value

modifyElement :: (a -> a) -> Int -> [a] -> ([a], a)
modifyElement modifyOp index list =
    case uncons r of
        Nothing      -> error "Index out of range"
        Just (x, xs) -> (l ++ (modifyOp x : xs), x)
    where
        (l, r) = splitAt index list

putStones :: KState -> KPos -> StoneCount -> KState
putStones state position amount = fst (modifyElement (+amount) position state)

leftPadWithSpaces :: Int -> String -> String
leftPadWithSpaces toLength str = replicate padLength ' ' ++ str
    where
        padLength = max 0 (toLength - (length str))

moveImpl :: Kalaha -> Player -> KState -> KPos -> (Player, KState)
moveImpl (Kalaha pitCount _) player startState startPosition =
    (playerAfterSowing, condApply wasLastMove playersGetWhatTheyHaveLeft stateAfterSowing)
    where
        wasLastMove = or $ map (all (==0) . init) [aliceHalf, bobHalf]
            where (aliceHalf, bobHalf) = splitAt (getPlayerOffset pitCount bob) stateAfterSowing
        (playerAfterSowing, stateAfterSowing) = sowe stateAfterGrab (getNextPosition startPosition) initialStones
        (stateAfterGrab, initialStones) = modifyElement (const 0) startPosition startState

        sowe :: KState -> KPos -> StoneCount -> (Player, KState)
        sowe state position 1
            | position == ownStore                = (player, statePutDownLastStone)
            | state !! position == 0 && onOwnSide = (not player, stateWinStonesOpposite)
            | otherwise                           = (not player, statePutDownLastStone)
            where
                statePutDownLastStone = putStones state position 1
                onOwnSide = position < ownStore && position >= (ownStore - pitCount)
                ownStore = getPlayerStore pitCount player
                stateWinStonesOpposite = putStones state' ownStore (opponentStones+1)
                    where
                        (state', opponentStones) = modifyElement (const 0) otherSidePit state
                        otherSidePit = 2*pitCount - position -- 'position' must (and should) be a pit

        sowe state position stonesLeft =
            sowe nextState nextPosition (stonesLeft-1)
            where
                nextState = putStones state position 1
                nextPosition = getNextPosition position

        getNextPosition :: KPos -> KPos
        getNextPosition position = head $ dropWhile (== opponentStore) $ tail $ iterate toNextIndex position
            where
                toNextIndex = (`mod` (2*halfSize)).(+1)
                opponentStore = getPlayerStore pitCount (not player)

        playersGetWhatTheyHaveLeft :: KState -> KState 
        playersGetWhatTheyHaveLeft state = foldl' f zeroed $ zip [0..] state
            where
                f accState (index, stones) = fst $ modifyElement (+stones) nextStore accState
                    where nextStore = (index `div` halfSize)*halfSize + pitCount
                zeroed = map (const 0) state

        halfSize = pitCount+1

\end{code}

The function `showGameImpl`
----


\begin{code}
showGameImpl :: Kalaha -> KState -> String
showGameImpl (Kalaha pitCount stoneCount) state =
    unlines $ map unwords [line1, line2, line3]
    where
        (aliceHalf, bobHalf) = splitAt (getPlayerOffset pitCount bob) $ map ((leftPadWithSpaces maxLength).show) state
        line1 = emptySlot : (reverse $ init bobHalf)
        line2 = bobKalaha : (replicate pitCount emptySlot) ++ [aliceKalaha]
        line3 = emptySlot : (init aliceHalf)
        aliceKalaha = last aliceHalf
        bobKalaha = last bobHalf

        maxLength = length $ show $ 2*pitCount*stoneCount
        emptySlot = replicate maxLength ' '
\end{code}


Trees
====

\begin{code}
data Tree m v  = Node v [(m,Tree m v)] deriving (Eq, Show)

testTree = Node 3 [
    (0, Node 4 [
        (0, Node 5 []), (1, Node 6 []), (2, Node 7 [])
    ])
    ,(1, Node 9 [
        (0, Node 10 [])
    ])]

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