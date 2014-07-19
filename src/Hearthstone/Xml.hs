{-# LANGUAGE OverloadedStrings #-}

module Hearthstone.Xml (
    names
  , xmlName
  , xmlCardSet
  , intCardType
  , parseCard
  , parseCards
  , tag
  , intTag
) where

import Control.Lens
import Text.Xml.Lens
import Text.XML
import Hearthstone.Card (Card(..))
import Hearthstone.CardType (CardType(..), toCardType)
import Data.Text (Text)
import Control.Applicative
import Data.Text.Read (decimal)

names :: Traversal' Document Text
names = xml...xmlName

tag n = node "Tag".attributed (ix "name" . only n)
intTag n = tag n.attr "value"._Just.readInt

readInt :: Fold Text Int
readInt = to decimal._Right._1

xmlName :: Traversal' Element Text
xmlName = tag "CardName".node "enUS".text

intCardType :: Fold Element Int
intCardType = intTag "CardType"

xmlCardSet :: Fold Element Int
xmlCardSet = intTag "CardSet"

parseCards :: Fold Element (Maybe Card)
parseCards = to parseCard

parseCard :: Element -> Maybe Card
parseCard e = do
  cardTypeId <- e ^? intCardType
  Card <$> (e ^? xmlName)
       <*> (e ^? attr "CardID"._Just)
       <*> (e ^? xmlCardSet)
       <*> Just cardTypeId
       <*> Just (toCardType cardTypeId)

{-
data Card = Card {
    _cardName :: !Text
  , _cardId   :: !Text
  , _cardSet  :: !Int
  , _cardType :: !Int
  }
-}

