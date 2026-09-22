{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}
module Prolog.Programming.Parser (
  parseConfig,
  parseSpec
  ) where

import Control.Monad                    (void)
import Control.Arrow                    ((>>>), (&&&))

import Data.List                        (isPrefixOf)

import Language.Prolog                  (terms, term)

import Text.Parsec
import Prolog.Programming.Types (TaskConfig (..), Spec, TreeStyle (..), Include (..), IncludeTask, IncludeHidden)
import Prolog.Programming.Helper (queryWithAnswers, statementToCheck, newPredDecl, localTimeout, negative, hidden, withTree, withTreeNegative)
import Data.Yaml (decodeEither', FromJSON (..), Value (..), withObject, (.:?), (.!=))
import qualified Data.ByteString.Char8 as BS (pack)
import qualified Data.Text as T (unpack)
import Data.Bifunctor (Bifunctor(..))

instance FromJSON TreeStyle where
  parseJSON (String "query") = pure QueryStyle
  parseJSON (String "resolution") = pure ResolutionStyle
  parseJSON _ = fail "Invalid value"

instance FromJSON IncludeTask where
  parseJSON (String "yes") = pure Yes
  parseJSON (String "filtered") = pure Filtered
  parseJSON (String "no") = pure $ No ()
  parseJSON _ = fail "Invalid value"

instance FromJSON IncludeHidden where
  parseJSON (String "yes") = pure Yes
  parseJSON (String "filtered") = pure Filtered
  parseJSON _ = fail "Invalid value"

instance FromJSON Spec where
  parseJSON (String v) = case parse parseSpec "(spec)" (T.unpack v) of
    Left err -> fail $ show err
    Right s -> pure s
  parseJSON _ = fail "Invalid value type"

instance FromJSON TaskConfig where
  parseJSON = withObject "TaskConfig" $ \v -> TaskConfig
    <$> v .:? "globalTimeout" .!= 10000
    <*> v .:? "treeStyle" .!= QueryStyle
    <*> v .:? "includeHiddenDefinitions" .!= Yes
    <*> v .:? "includeTaskDefinitions" .!= Yes
    <*> v .:? "allowListPatternMatching" .!= True
    <*> v .:? "showSWISHButton" .!= False
    <*> v .:? "specifications" .!= []


parseConfig :: String -> Either ParseError (TaskConfig, (String, String))
parseConfig = parse configuration "(config)"

configuration ::
  Parsec
    String
    ()
    ( TaskConfig,
      (String, String)
    )
configuration = do
  ls <- lines <$> anyChar `manyTill` eof
  let (rawCfg, rest) = first unlines $ breakWhen ("---" `isPrefixOf`) ls
  case decodeEither' (BS.pack rawCfg) of
    Left err -> fail $ show err
    Right taskCfg -> do
      let preds = bimap unlines unlines $ breakWhen ("---" `isPrefixOf`) rest
      pure (taskCfg,preds)


parseSpec :: Parsec String () Spec
parseSpec = try newPredDeclParser <|> specLine
  where
    specLine = ((\f g h i -> f . g . h . i)
                  <$> localTimeoutAnn
                  <*> negativeFlag
                  <*> withTreeFlag
                  <*> hiddenFlag) <*> do
        spaces
        q <- terms
        (do char ':' >> optional (char ' ')
            queryWithAnswers q . map (:[]) <$> terms)
         <|> pure (statementToCheck q)

    newPredDeclParser = do
        void $ string "new"
        spaces
        t <- term
        spaces
        void $ char ':'
        spaces
        desc <- many1 anyChar
        pure $ newPredDecl t desc

    localTimeoutAnn = option id $
      localTimeout . read
      <$> between (char '[') (char ']') (many1 digit) <* spaces

    negativeFlag = option id $ negative <$ char '-'

    hiddenFlag   = option id $
      char '!' >> hidden
      <$> option "" (try (between (char '(') (char ')') (many $ noneOf ")")))

    withTreeFlag = option id $ (char '@' >> return withTree)
                           <|> (char '#' >> return withTreeNegative)

breakWhen :: (a -> Bool) -> [a] -> ([a],[a])
breakWhen p = (takeWhile (not . p) &&& dropWhile (not . p)) >>> second (drop 1)
