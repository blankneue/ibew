\begin{code}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}

module Endpoints.Landing (LANDING, Landing, landing) where

import Servant
import Servant.HTML.Blaze
import Text.Blaze
import Text.Blaze.Html5 as H
import Text.Blaze.Html5.Attributes as A

data Landing = Landing
instance ToMarkup Landing where
  toMarkup _ = docTypeHtml $ do
    H.head $ do
      H.title "tupa"
    body $ do
      a ! href "app" $ "Sign In"
      br
      a ! href "register" $ "Sign Up"

type LANDING = Get '[HTML] Landing

landing :: Server LANDING
landing = return Landing
\end{code}
