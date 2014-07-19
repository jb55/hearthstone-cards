
module Hearthstone.CardType (
  CardType(..)
, toCardType
, fromCardType
) where

import Control.Lens

data CardType = CtMinion -- 4
              | CtSpell  -- 5
              deriving (Show, Eq)

fromCardType :: CardType -> Int
fromCardType CtMinion = 4
fromCardType CtSpell  = 5

toCardType :: Int -> Maybe CardType
toCardType 4 = Just CtMinion
toCardType 5 = Just CtSpell
toCardType _ = Nothing

