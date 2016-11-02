{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE FlexibleContexts #-}

module Widget where

import           Reflex
import           Reflex.Dom

import           Data.Map         (Map)
import qualified Data.Map         as Map
import           Data.Maybe
import           Data.Monoid
import           Data.Time.Clock  (UTCTime)
import           Data.Time.Format (defaultTimeLocale, parseTimeM)

import Data.Text
import           Text.Read        (readMaybe)

type PageTitle = String

headElement :: MonadWidget t m => PageTitle -> m ()
headElement title = do
  el "title" (text title)
  styleSheet "css/style.css"
  styleSheet "https://maxcdn.bootstrapcdn.com/font-awesome/4.6.3/css/font-awesome.min.css"
  styleSheet "http://fonts.googleapis.com/css?family=Lato"
  styleSheet "https://cdnjs.cloudflare.com/ajax/libs/skeleton/2.0.4/skeleton.min.css"
  where
    styleSheet _link = elAttr "link" (Map.fromList [
          ("rel", "stylesheet")
        , ("type", "text/css")
        , ("href", _link)
      ]) $ pure ()

readableInput :: (MonadWidget t m, Read a) => TextInputConfig t -> m (Event t a)
readableInput conf = do
    c <- textInput conf
    pure $ fmapMaybe readMaybe $ _textInput_input c


-- buttonWith :: DomBuilder t m => Text -> Map.Map String String -> m (Event t ())
buttonWith title attrs = do
  (e,_) <- elAttr' "button" attrs $ text title
  pure $ domEvent Click e

-- buttonWithDyn :: DomBuilder t m => Text -> Dynamic t (Map.Map String String) -> m (Event t ())
buttonWithDyn title attrs = do
  (e,_) <- elDynAttr' "button" attrs $ text title
  pure $ domEvent Click e

maybeButton :: MonadWidget t m
            => Dynamic t Bool -- ^ Is the button enabled?
            -> String -- ^ Static button label
            -> m (Event t ())
maybeButton enabled label = do
    attrs <- forDyn enabled $ \e -> monoidGuard (not e) $ "disabled" =: "disabled"
    (btn, _) <- elDynAttr' "button" attrs $ text label
    pure $ domEvent Click btn

datePicker :: MonadWidget t m
           => Dynamic t Bool -- ^ Widget enabled?
           -> m (Dynamic t (Maybe UTCTime))
datePicker enabled = do
    rec raw <- textInput $ def & textInputConfig_attributes .~ attrs
        date <- mapDyn (parseTimeM True defaultTimeLocale "%F") $ _textInput_value raw
        attrs <- dynCombine date enabled $ \d e ->
            monoidGuard (isNothing d) ("style" =: "color: red") <>
            monoidGuard (not e) ("disabled" =: "disabled")
    return date

selectableList :: (MonadWidget t m, Ord k)
               => Dynamic t (Maybe k)
               -- ^ Key of element that may be selected
               -> Dynamic t (Map k v)
               -- ^ Map of elements to be shown in the list
               -> (Dynamic t Bool -> Dynamic t v -> m (Event t a))
               -- ^ Action that renders a widget for an element. The element may fire events
               -> m (Event t k)
               -- ^ List fires events whenever an element is selected
selectableList selection elems mkEntry = do
    selectEntry <- listWithKey elems $ \k v -> do
        isSelected <- forDyn selection $ \s -> s == Just k
        fmap (const k) <$> mkEntry isSelected v
    switchPromptlyDyn <$> mapDyn (leftmost . Map.elems) selectEntry

monoidGuard :: Monoid a => Bool -> a -> a
monoidGuard p a = if p then a else mempty

dynCombine :: (Reflex t, MonadHold t m)
           => Dynamic t a -> Dynamic t b
           -> (a -> b -> c)
           -> m (Dynamic t c)
dynCombine a b f = combineDyn f a b

dynCombine3 :: (Reflex t, MonadHold t m)
            => Dynamic t a -> Dynamic t b -> Dynamic t c
            -> (a -> b -> c -> d)
            -> m (Dynamic t d)
dynCombine3 da db dc f = do
    dg <- combineDyn f da db
    combineDyn (\g c -> g c) dg dc
