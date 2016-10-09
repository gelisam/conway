module Main where

import Control.Concurrent

import Conway


-- construct an animation based on Conway's Game of Life.

glider :: ZZ Char
glider = fromList [" #     ",
                   "  #    ",
                   "###    ",
                   "       ",
                   "       "]

glider_animation :: [ZZ Char]
glider_animation = life_animation glider


-- display such an animation.

clear :: IO ()
clear = putStr "\x1B[2J\x1B[;H"

display_animation :: [ZZ Char] -> IO ()
display_animation = mapM_ $ \screen -> do
                      clear
                      threadDelay 100000
                      mapM_ putStrLn $ toList screen

animate :: ZZ Char -> IO ()
animate = display_animation . life_animation


-- display the glider animation.

main :: IO ()
main = animate glider
