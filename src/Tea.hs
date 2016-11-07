{-# LANGUAGE RecursiveDo #-}

module Tea where

import Data.Decimal
import Data.Time.Clock (UTCTime)
import Reflex
import Reflex.Dom

import qualified Widget

whiteDef :: Decimal
whiteDef = 2.0

greenDef :: Decimal
greenDef = 3.0

blackDef :: Decimal
blackDef = 4.0

data Unit = Minute | Second deriving (Eq, Show)

data Tea
  = Green
  | Black
  | White
  | Other Decimal

data Action
  = Start Tea
  | TimerTick
  | UnitChange Unit

data Model
  = Model
  { currentTime :: Decimal
  , unit :: Unit }

increment :: Decimal
increment = 1.0 -- this one's a decimal

initialModel :: Model
initialModel = Model { currentTime = 0, unit = Minute}

convert :: Model -> Decimal -> Decimal
convert m t = case unit m of
  Minute -> t * 60
  Second -> t

setTime :: Model -> Tea -> Decimal
setTime m (Other t) = convert m t
setTime m Green = convert m greenDef
setTime m Black = convert m blackDef
setTime m White = convert m whiteDef

update :: Action -> Model -> Model
update (UnitChange u) m = m { unit = u }
update (Start tea) m = m { currentTime = setTime m tea }
update TimerTick m = m { currentTime = t }
  where
    t = if currentTime m - increment >= 0
        then currentTime m - increment
        else currentTime m

timer :: MonadWidget t m => UTCTime -> m ()
timer t0 = elClass "div" "row" $ do
  tick <- tickLossy 1.0 t0 -- 1.0 is a nominal diff time

  white <- Widget.buttonWith "White (2 min)" ("class" =: "button white huge")
  green <- Widget.buttonWith "Green (3 min)" ("class" =: "button green huge")
  black <- Widget.buttonWith "Black (4 min)" ("class" =: "button black huge")

  otherTime <- Widget.readableInput $ def
    & attributes .~ constDyn (mconcat ["placeholder" =: "custom"])
  other <- button "Custom Time"
  otherDyn <- holdDyn (2.5 :: Decimal) otherTime

  rec let events = leftmost
            [ TimerTick <$ tick
            , (\(curr, _) -> Start (Other curr)) <$> attachDyn otherDyn other
            , Start Green <$ green
            , Start Black <$ black
            , Start White <$ white
            , UnitChange <$> updated unitChange
            ]
      let startEvent = leftmost [green, black, white, other]

      dynModel <- foldDyn update initialModel events

      elapsedText <- mapDyn showTime dynModel

      unitE <- Widget.radio "unit" ["minute", "second"]
      unitChange <- holdDyn Minute $ Second <$ leftmost unitE

      elClass "div" "row" $ do
        _ <- widgetHold elapsedWidget $ dynText elapsedText <$ startEvent
        pure ()

  pure ()


elapsedWidget :: MonadWidget t m => m ()
elapsedWidget = el "p" $ text ""

showTime :: Model -> String
showTime model = case unit model of
  Second -> show (currentTime model) ++ "seconds remaining"
  Minute ->
    m ++ "m" ++ s ++ "s remaining"
    where
      m = show $ fst (quotRem d' 60)
      s = show $ snd (quotRem d' 60)
      d' = ceiling (currentTime model) :: Int
