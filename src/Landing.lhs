\begin{code}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}

module Landing where

import Servant
import Servant.HTML.Blaze
import Text.Blaze
import Text.Blaze.Html5

data Landing = Landing
instance ToMarkup Landing where
  toMarkup l = docTypeHtml $ do
    Text.Blaze.Html5.head $ do
      title "Ibew Tupa!!!"
    body $ do
      p "Hello world"

type LANDING = Get '[HTML] Landing

landing :: Server LANDING
landing = return Landing
\end{code}
