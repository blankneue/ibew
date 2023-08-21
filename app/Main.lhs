\chapter{Executing Ibew}

\begin{code}
{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import App
import Configuration

import Dhall
import System.Environment

main :: IO ()
main = let c = Configuration 8080
               "accounts.sqlite3" 10
               "messages.sqlite3" 10
               "c.pem" "k.pem"
       in do
        a <- getArgs
        if null a
        then run c
        else do
          b <- inputFile auto $ head a
          run b
\end{code}
