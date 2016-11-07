
module Main where

import           Reflex.Dom

import           Data.Time.Clock (getCurrentTime)

import qualified Widget
import qualified Timer
import qualified Tea

main :: IO ()
main = do
  tStart <- getCurrentTime
  mainWidgetWithHead (Widget.headElement "timers") $
    elClass "div" "container" $
      elClass "div" "row" $ do
        elClass "div" "six columns" $ do
          el "h1" $ text "Pomodorooooooo"
          Timer.timer tStart
          pure ()
        elClass "div" "six columns" $ do
          el "h1" $ text "Tea Time"
          Tea.timer tStart
          pure ()
