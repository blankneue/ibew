\begin{code}
{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import App
import Configuration

main :: IO ()
main = run $ Configuration
  8080
  "accounts.sqlite3" 10
  "messages.sqlite3" 10
  "c.pem" "k.pem"
\end{code}
