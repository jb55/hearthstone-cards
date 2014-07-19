{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}

module Hearthstone.Card (
    Card(..)
  , pretty
) where

import Data.Text (Text)
import qualified Data.Text.Lazy as LT
import Control.Lens.TH
import Hearthstone.CardType (CardType)
import Data.Text.Format (format)
import Control.Applicative
import Control.Lens

data Card = Card {
    _cardName   :: !Text
  , _cardId     :: !Text
  , _cardSet    :: !Int
  , _cardTypeId :: !Int
  , _cardType   :: Maybe CardType
  }

makeLenses ''Card

pretty :: Card -> LT.Text
pretty c = format "Card\t{}\t{}\t{}\t{}\t{}" $ 
  ( c^.cardName
  , c^.cardId
  , c^.cardSet
  , c^.cardTypeId
  , c^?cardType.to show
  )
