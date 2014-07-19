{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Applicative
import Control.Lens
import Data.Text (Text)
import Text.Xml.Lens
import Text.XML
import qualified Data.Text.Lazy.IO as T
import Data.Default (def)
import Data.ByteString (ByteString)
import Data.Monoid ((<>))
import Hearthstone.Xml
import Hearthstone.Card (pretty)

readXml :: IO (Maybe Document)
readXml = do
  cards <- T.readFile "/home/jb55/cards.xml"
  let parsed = parseText def cards
  return $ parsed ^? _Right

--entities src = do
--  parsed <- lift $ evalStateT (PA.parse parseXmlBytes) src
--  yield "<Entities>"
--  yield "</Entities>"

prettyCards = _Just . xml ... parseCards . _Just . to pretty

main = do 
  d <- readXml 
  let pcards = d ^.. prettyCards
  mapM T.putStrLn pcards
