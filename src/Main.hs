
module Main where

import           Reflex
import           Reflex.Dom

import           Data.Decimal
import           Data.Time.Clock (getCurrentTime)

import qualified Widget
import qualified Timer
import qualified Tea

main :: IO ()
main = do
  tStart <- getCurrentTime
  mainWidgetWithHead (Widget.headElement "timer") $
    elClass "div" "container" $
      elClass "div" "row" $ do
        elClass "div" "six columns" $ do
          Timer.timer tStart
          pure ()
        elClass "div" "six columns" $ do
          Tea.timer tStart
          pure ()
