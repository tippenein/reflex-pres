{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

module Tea where

import Control.Monad (replicateM_)
import Data.List 
import Data.Monoid ((<>))
import Control.Lens hiding (view)
import Data.Time.Clock (UTCTime)
import Reflex
import Prelude hiding (div)
import Reflex.Dom

import qualified Widget

whiteDef :: Float
whiteDef = 2.0

greenDef :: Float
greenDef = 3.0

blackDef :: Float
blackDef = 4.0

data Unit
  = Minute
  | Second
  deriving (Eq, Show)

data Tea
  = Green
  | Black
  | White
  | Other Float
  deriving (Show, Eq)

data Action
  = Start Tea
  | TimerTick
  | UnitChange

data Model
  = Model
  { currentTime :: Float
  , _unit :: Unit
  , chosenTea :: Maybe Tea
  , _lightness :: Float
  , done :: Bool
  } deriving (Show)

makeLenses ''Model

increment :: Float
increment = 1.0 -- this one's a decimal

initialModel :: Model
initialModel
  = Model
  { currentTime = 0
  , _unit = Minute
  , chosenTea = Nothing
  , _lightness = 100
  , done = False
  }

convert :: Model -> Float -> Float
convert m t = case _unit m of
  Minute -> t * 60
  Second -> t
setTime :: Model -> Tea -> Float
setTime m (Other t) = convert m t
setTime m Green = convert m greenDef
setTime m Black = convert m blackDef
setTime m White = convert m whiteDef

flipUnit :: Unit -> Unit
flipUnit Minute = Second
flipUnit Second = Minute

-- lowest 
lightnessRate :: Tea -> Float
lightnessRate Green = equalPortion 58 greenDef
lightnessRate Black = equalPortion 40 blackDef
lightnessRate White = equalPortion 88 whiteDef
lightnessRate ( Other c ) = equalPortion 58 c

equalPortion :: Float -> Float -> Float
equalPortion a b = a / (b * 60)

teaColor :: Model -> String
teaColor m = case (chosenTea m, ceiling $ _lightness m) of
  ( Just White, lightness) ->
    "background: " ++ hsl 92 57 lightness -- min 88
  ( Just Green , lightness) ->
    "background: " ++ hsl 92 57 lightness -- min 58
  ( Just Black, lightness) ->
    "background: " ++ hsl 38 54 lightness -- min 40
  ( _, lightness) -> "background: " ++ hsl 38 54 lightness

hsl :: Int -> Int -> Int -> String
hsl h s l = "hsl(" ++ inner ++ ")"
  where
    inner = join "," [show h, s',l']
    s' = show s ++ "%"
    l' = show l ++ "%"


loading :: MonadWidget t m => m ()
loading = text "loading..."

showMaybeConcat :: Maybe Tea -> String -> String -> String
showMaybeConcat Nothing _ def' = def'
showMaybeConcat ( Just a ) c _ = show a ++ c

elapsedWidget :: MonadWidget t m => Dynamic t Model -> m ()
elapsedWidget m  =
  el "header" $ do
    t <- mapDyn (\m' -> showMaybeConcat (chosenTea m') " tea will be ready in " "select a steep time") m
    dynText t
    dynText =<< mapDyn showTime m

statusWidget :: MonadWidget t m => Dynamic t Model -> m ()
statusWidget model = do
  attrs <- mapDyn (\m ->
        "class" =: "six columns tea-block" <>
        "style" =: teaColor m) model

  div "row" $
    elDynAttr "div" attrs $ do
      _ <- elapsedWidget model
      pure ()
  pure ()

div :: MonadWidget t m => String -> m a -> m a
div = elClass "div"

update :: Action -> Model -> Model
update UnitChange m = m { _unit = flipUnit (_unit m) }
update (Start tea) m =
  initialModel { currentTime = setTime m tea
               , chosenTea = Just tea
               }
update TimerTick m = 
  let t = currentTime m - increment
  in
    if t >= 0
    then 
      m { currentTime = t, _lightness = _lightness m - rate }
    else
      m { done = True }
    where
      rate = case chosenTea m of
        Nothing -> 0
        Just j -> lightnessRate j

view :: MonadWidget t m
     => UTCTime
     -> Dynamic t Model
     -> m (Event t Action)
view t0 model = elClass "div" "row" $ do
  tick <- tickLossy 1.0 t0 -- 1.0 is a nominal diff time

  rec
      statusWidget model
      (white,green,black) <- timerContainer $ do
        w <- Widget.buttonWith "White (2 min)" ("class" =: "button white huge")
        g <- Widget.buttonWith "Green (3 min)" ("class" =: "button green huge")
        b <- Widget.buttonWith "Black (4 min)" ("class" =: "button black huge")
        pure (w, g, b)

      (other, otherDyn) <- timerContainer $ do
        otherTime <- Widget.readableInput $ def
          & attributes .~ constDyn (mconcat ["placeholder" =: "custom"])
        o <- button "Custom Time"
        od <- holdDyn (2.5 :: Float) otherTime
        pure ( o, od )
      let startEvent = leftmost [green, black, white, other]

      minuteBox <- checkbox True $ Widget.checkboxAttrs "unit" "minute"
      dynText =<< mapDyn (show . _unit) model
      let unitToggle = _checkbox_change minuteBox

  pure $ leftmost
      [ TimerTick <$ tick
      , (\(v,_) -> Start (Other v)) <$> attachDyn otherDyn other
      , Start Green <$ green
      , Start Black <$ black
      , Start White <$ white
      , UnitChange <$ unitToggle
      ]

bodyElement :: MonadWidget t m => UTCTime -> m ()
bodyElement tStart =
  mainContainer $ do
    rec changes <- view tStart model
        model <- foldDyn update initialModel changes

        t <- mapDyn showTime model
        _ <- elDynHtml' "title" t
    pure ()

timerContainer :: MonadWidget t m => m a -> m a
timerContainer body =
  elClass "div" "row" $
    elClass "div" "twelve columns" $ do
      r <- body
      pure r

mainContainer :: MonadWidget t m => m () -> m ()
mainContainer body = do
  _ <- elClass "div" "container" $
    elClass "div" "row" $
      elClass "div" "twelve columns" $ do
        el "h1" $ text "Tea Time"
        body
  pure ()


showTime :: Model -> String
showTime model = case _unit model of
  Second -> show (currentTime model) ++ " seconds"
  Minute ->
    m ++ ":" ++ s
    where
      m = show $ fst (quotRem d' 60)
      s = show $ snd (quotRem d' 60)
      d' = ceiling (currentTime model) :: Int

join = intercalate
