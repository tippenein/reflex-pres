{-# LANGUAGE RecursiveDo #-}

module Timer where

import           Reflex
import           Reflex.Dom

import           Data.Decimal
import           Data.Time.Clock (UTCTime)

import Widget

data TimerEvent
  = Start Decimal -- starting countdown
  | Break Decimal
  | TimerTick Decimal

timeInput :: (Read a, MonadWidget t m ) => String -> m (Event t a)
timeInput placeholder = Widget.readableInput $ def
    & attributes .~ constDyn (mconcat [ "placeholder" =: placeholder
                                      , "class" =: "timer-input"
                                      ])

timer :: MonadWidget t m => UTCTime -> m ()
timer t0 = el "div" $ do
  let increment = 1.0 -- this one's a decimal
  tick <- tickLossy 1.0 t0 -- this one's a nominal diff time

  start <- button "Start"
  take_a_break <- button "Take a Break"

  countdownFrom <- timeInput "pomodoro time"
  countdownDyn <- holdDyn 25.0 countdownFrom

  breakAmount <- timeInput "chill time"
  breakDyn <- holdDyn 5.0 breakAmount

  rec let events = leftmost
            [ (\(curr, _) -> TimerTick curr) <$> attachDyn (constDyn 0.0) tick
            , (\(from, _) -> Start from) <$> attachDyn countdownDyn start
            , (\(from, _) -> Break from) <$> attachDyn breakDyn take_a_break
            ]

      remaining <- foldDyn (\ev curr -> case ev of
              TimerTick _whatever ->
                if curr - increment >= 0
                then curr - increment
                else curr
              Start limit' -> limit'
              Break b -> b
          ) (0.0 :: Decimal) events

      elapsedText <- mapDyn show remaining
      _ <- widgetHold elapsedWidget $ dynText elapsedText <$ start
  pure ()

elapsedWidget :: MonadWidget t m => m ()
elapsedWidget = el "p" $ text ""

