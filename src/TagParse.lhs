\chapter{Parsing Headmate Tags}

\begin{code}
{-# LANGUAGE OverloadedStrings #-}

module TagParse (checkMessage) where

import Prelude hiding (length,take)

import Data.Text

checkHead :: Text -> Text -> Bool
checkHead a b = (take (length b) a) == b

checkTail :: Text -> Text -> Bool
checkTail a b = (takeEnd (length b) a) == b

checkLength :: Text -> (Text, Text) -> Bool
checkLength a (b, c) = length a > (length b + length c)

checkMessage :: Text -> (Text, Text) -> Bool
checkMessage a (b, c) = and [checkLength a (b, c)
                            ,checkHead a b
                            ,checkTail a c]
\end{code}
