\begin{code}
module Main (main) where

import Lib

main :: IO ()
main = do
  let dbfile = "./messages.sqlite3"
  initDB dbfile
  startApp dbfile "" ""
\end{code}
