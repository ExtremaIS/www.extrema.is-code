\# Literate Haskell Example

Executables are implemented using a `Main` module that exposes a function
named `main`.

> module Main (main) where

The `main` function is run when the program is executed.

> main :: IO ()
> main = putStrLn "Hello!"

This simple example just prints "Hello!" to the screen.
