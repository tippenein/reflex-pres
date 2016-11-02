{-# LANGUAGE RecursiveDo #-}

module Tea where

import           Reflex
import           Reflex.Dom

import           Data.Decimal
import           Data.Time.Clock (UTCTime)

import qualified Widget

data Tea
  = Green
  | Black
  | White
  | Other Decimal
  | TimerTick Decimal

whiteDef :: Decimal
whiteDef = 2.0

greenDef :: Decimal
greenDef = 3.0

blackDef :: Decimal
blackDef = 4.0

  -- styledButton title color = elAttr' "Green (3 min)" $ def & ("class" =: "")
timer :: MonadWidget t m => UTCTime -> m ()
timer t0 = el "div" $ do
  let increment = 1.0 -- this one's a decimal
  tick <- tickLossy 1.0 t0 -- this one's a nominal diff time

  white <- button "White (2 min)"
  green <- Widget.buttonWith "Green (3 min)" ("class" =: "green")
  black <- button "Black (4 min)"
  other <- button "Custom Time"

  otherTime <- Widget.readableInput $ def
    & attributes .~ constDyn (mconcat ["placeholder" =: "custom"])
  otherDyn <- holdDyn (2.5 :: Decimal) otherTime

  rec let events = leftmost
            [ (\(curr, _) -> TimerTick curr) <$> attachDyn (constDyn 0.0) tick
            , (\(curr, _) -> Other curr) <$> attachDyn otherDyn other
            , Green <$ green
            , Black <$ black
            , White <$ white
            ]

      remaining <- foldDyn (\ev curr -> case ev of
              TimerTick _whatever ->
                if curr - increment >= 0
                then curr - increment
                else curr
              Green -> greenDef
              Black -> blackDef
              White -> whiteDef
              Other t -> t
          ) (0.0 :: Decimal) events

      elapsedText <- mapDyn show remaining
      let startEvent = leftmost [green, black, other]
      _ <- widgetHold elapsedWidget $ dynText elapsedText <$ startEvent
  pure ()

elapsedWidget :: MonadWidget t m => m ()
elapsedWidget = el "p" $ text ""
