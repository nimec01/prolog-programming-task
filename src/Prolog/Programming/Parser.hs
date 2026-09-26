{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Prolog.Programming.Parser (
  parseConfig,
  parseSpec,
) where

import Control.Monad (void, when)

import Data.List (isInfixOf, isPrefixOf)

import Language.Prolog (Term, term, terms)

import qualified Data.ByteString.Char8 as BS (pack)
import Data.Maybe (isJust)
import qualified Data.Text as T (unpack)
import Data.Yaml (FromJSON (..), Value (..), decodeEither', withObject, (.!=), (.:?))
import Data.Yaml.Aeson (Parser)
import Prolog.Programming.CodeAnalysis.Config (
  defaultCodeAnalysisConfig,
 )
import Prolog.Programming.CodeAnalysis.Types (
  CodeAnalysisConfig (..),
  CodeAnalysisRuleConfig (..),
  CutUsageConfig (..),
  SingletonVariablesConfig (..),
 )
import qualified Prolog.Programming.CodeAnalysis.Types as CA (Severity (..))
import Prolog.Programming.Types (
  Expection (..),
  Include (..),
  IncludeHidden,
  IncludeTask,
  Requirement (..),
  Spec (..),
  TaskConfig (..),
  Timeout (..),
  TreeStyle (..),
  Visibility (..),
  Visualize (..),
 )
import Text.Parsec

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
  parseJSON (String v) = case parse (parseSpec <* eof) "(spec)" (T.unpack v) of
    Left err -> fail $ show err
    Right s -> pure s
  parseJSON _ = fail "Invalid value type"

parseStatus :: Value -> Parser (CodeAnalysisRuleConfig ())
parseStatus (String "ignore") = pure Ignore
parseStatus (String "hint") = pure $ Detect CA.Hint ()
parseStatus (String "warn") = pure $ Detect CA.Warn ()
parseStatus (String "reject") = pure $ Detect CA.Error ()
parseStatus _ = fail "status must be one of: 'ignore', 'hint', 'warn', or 'reject'"

instance FromJSON SingletonVariablesConfig where
  parseJSON = withObject "SingletonVariablesConfig" $ \v -> do
    mStatus <- v .:? "status"

    status <- maybe (pure Ignore) parseStatus mStatus

    pure $ SingletonVariablesConfig status

instance FromJSON CutUsageConfig where
  parseJSON = withObject "CutUsageConfig" $ \v -> do
    mStatus <- v .:? "status"

    status <- maybe (pure Ignore) parseStatus mStatus

    msg <- v .:? "additionalMessage"

    when (status == Ignore && isJust msg) $
      fail "additionalMessage is only allowed to exist when status is not 'ignore'"

    pure $ CutUsageConfig (msg <$ status)

instance FromJSON CodeAnalysisConfig where
  parseJSON = withObject "CodeAnalysisConfig" $ \v ->
    CodeAnalysisConfig
      <$> v .:? "singletonVariables" .!= SingletonVariablesConfig Ignore
      <*> v .:? "cutUsage" .!= CutUsageConfig Ignore

instance FromJSON TaskConfig where
  parseJSON = withObject "TaskConfig" $ \v ->
    TaskConfig
      <$> v .:? "globalTimeout" .!= 10000
      <*> v .:? "treeStyle" .!= QueryStyle
      <*> v .:? "includeTaskDefinitions" .!= Yes
      <*> v .:? "includeHiddenDefinitions" .!= Yes
      <*> v .:? "allowListPatternMatching" .!= True
      <*> v .:? "showSWISHButton" .!= False
      <*> v .:? "codeAnalysis" .!= defaultCodeAnalysisConfig
      <*> v .:? "specifications" .!= []

parseConfig :: String -> Either ParseError (TaskConfig, String, (String, String))
parseConfig = parse (configuration <* eof) "(config)"

configuration
  :: Parsec
       String
       ()
       ( TaskConfig
       , String
       , (String, String)
       )
configuration = do
  ls <- lines <$> anyChar `manyTill` eof
  case breakWhen ("---" `isPrefixOf`) ls of
    (rawCfgLs : taskLs : optionalLs) -> case decodeEither' (BS.pack $ unlines rawCfgLs) of
      Left err -> fail $ show err
      Right taskCfg -> do
        (solution, hiddenPart) <- case optionalLs of
          [a, b] -> case map unlines [a, b] of
            [a', b']
              | "% SOLUTION" `isInfixOf` a' -> pure (a', b')
              | "% SOLUTION" `isInfixOf` b' -> pure (b', a')
            _ -> fail "Unable to find sample solution."
          [a] -> let a' = unlines a in if "% SOLUTION" `isInfixOf` a' then pure (a', "") else fail "Unable to find sample solution."
          xs
            | null xs -> fail "Unable to find sample solution."
            | otherwise -> fail "Provided more config sections than expected."

        let solutionStripped = unlines $ filter (not . isPrefixOf "% SOLUTION") $ lines solution

        pure (taskCfg, solutionStripped, (unlines taskLs, hiddenPart))
    _ -> fail "Config does not include the two required parts"

parseSpec :: Parsec String () Spec
parseSpec = try newPredDeclParser <|> specLine
  where
    specLine =
      ( (\f g h i -> f . g . h . i)
          <$> localTimeoutAnn
          <*> negativeFlag
          <*> withTreeFlag
          <*> hiddenFlag
      )
        <*> do
          spaces
          q <- terms
          ( do
              char ':' >> optional (char ' ')
              queryWithAnswers q . map (: []) <$> terms
            )
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

    localTimeoutAnn =
      option id $
        localTimeout . read
          <$> between (char '[') (char ']') (many1 digit)
          <* spaces

    negativeFlag = option id $ negative <$ char '-'

    hiddenFlag =
      option id $
        char '!'
          >> hidden
            <$> option "" (try (between (char '(') (char ')') description))

    withTreeFlag =
      option id $
        (char '@' >> return withTree)
          <|> (char '#' >> return withTreeNegative)

    description =
      try (between (char '"') (char '"') (descriptionMsg "\""))
        <|> try (between (char '\'') (char '\'') (descriptionMsg "'"))
        <|> descriptionMsg ")"

    descriptionMsg :: String -> Parsec String () String
    descriptionMsg end = many (noneOf end)

defaultOptions :: Requirement -> Spec
defaultOptions = Spec Visible DontShowTree PositiveResult GlobalTimeout

breakWhen :: (a -> Bool) -> [a] -> [[a]]
breakWhen _ [] = []
breakWhen p xs =
  let (before, after) = break p xs
  in before : case after of
       [] -> []
       (_ : xs') -> breakWhen p xs'

queryWithAnswers :: [Term] -> [[Term]] -> Spec
queryWithAnswers q as = defaultOptions $ QueryWithAnswers q as

statementToCheck :: [Term] -> Spec
statementToCheck ts = defaultOptions $ StatementToCheck ts

hidden :: String -> Spec -> Spec
hidden s spec = spec {specVisibility = Hidden s}

withTree :: Spec -> Spec
withTree spec = spec {specVisualize = ShowTree}

withTreeNegative :: Spec -> Spec
withTreeNegative = negative . withTree

newPredDecl :: Term -> String -> Spec
newPredDecl t s = defaultOptions $ NewPredDecl t s

negative :: Spec -> Spec
negative spec = spec {specExpection = NegativeResult}

localTimeout :: Int -> Spec -> Spec
localTimeout d spec = spec {specTimeout = LocalTimeout d}
