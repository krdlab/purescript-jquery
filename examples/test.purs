module Main where

import Data.Either
import Prelude
import Data.Foreign
import Control.Monad.Eff
import qualified Control.Monad.JQuery as J
import Debug.Trace

main = J.ready $ do
  -- Get the document body
  b <- J.body

  -- Create a text box
  div <- J.create "<div>"
  input <- J.create "<input>"
  "Your Name: " `J.appendText` div
  input `J.append` div
  div `J.append` b

  -- Create a paragraph to display a greeting
  greeting <- J.create "<p>"
  { color: "red" } `J.css` greeting
  greeting `J.append` b

  -- Listen for change events on the text box
  flip (J.on "change") input $ \_ _ -> do
    Right name <- parseForeign read <$> J.getValue input
    trace $ "Name changed to " ++ name
    J.setText ("Hello, " ++ name) greeting

  -- Run ajax
  response <- J.create "<pre>"
  response `J.append` b
  J.ajax "http://httpbin.org/get" { _type: "GET", dataType: "json" }
    >>= J.done (\dat _ _ -> J.clear response >>= J.setText (stringify dat))
    >>= J.fail (\_ _ err -> trace err)

foreign import stringify
  "function stringify(json) { \
  \  return function() { \
  \    return JSON.stringify(json); \
  \  }; \
  \}" :: forall j. { | j } -> String

