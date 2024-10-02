{-# LANGUAGE PatternGuards #-}

module Main (main) where

import Data.Int (Int32)
import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Game
import Data.WAVE


main :: IO ()
main
 = do   let state = State Nothing []
        play    (InWindow "draw function left to right" (1000, 1000) (0,0))
                white 100 state
                (fst . makePicture) handleEvent stepWorld


data State
        = State (Maybe Path)
                [Picture]


type Segment    = ((Float, Float), (Float, Float))


makePicture :: State -> (Picture, [(Float, Float)])
makePicture (State m xs)
        = (Pictures (maybe xs (\x -> Line x : xs) m), (\(Line x) -> x) $ head xs)

handleEvent :: Event -> State -> State
handleEvent event state
        | EventMotion (x, y)    <- event
        , State (Just ps) ss    <- state
        = State (Just $ scanl1 (\(a,b) (c,d) -> if a<c then (a,b) else (c,d)) ((x, y):ps)) ss

        | EventKey (MouseButton LeftButton) Down _ pt@(x,y) <- event
        , State Nothing ss <- state
        = State (Just [pt])
                []

        | EventKey (MouseButton LeftButton) Up _ pt@(x,y) <- event
        , State (Just ps) ss <- state
        = State Nothing
                (Line (pt:ps) : ss)

        | otherwise
        = state


stepWorld :: Float -> State -> State
stepWorld _ = id