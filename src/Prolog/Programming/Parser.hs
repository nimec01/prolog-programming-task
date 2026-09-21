{-# LANGUAGE RecordWildCards #-}
module Prolog.Programming.Parser (
  parseConfig,
  ) where

import Prolog.Programming.TestSpec

import Control.Monad                    (forM, void)
import Control.Arrow                    ((>>>), (&&&), second)

import Data.List                        (isPrefixOf)
import Data.Maybe                       (catMaybes, fromMaybe)

import Language.Prolog                  (terms, term)

import Text.Parsec
import Prolog.Programming.CodeAnalysis.Types (CodeAnalysisConfig(..))
import qualified Prolog.Programming.CodeAnalysis.Types as CA (Severity(..))

parseConfig ::
  String ->
  Either
    ParseError
    ( TimeoutDuration,
      TreeStyle,
      IncludeTask,
      IncludeHidden,
      AllowListMatching,
      ShowSWISHButton,
      CodeAnalysisConfig,
      [Spec],
      (String, String)
    )
parseConfig = parse configuration "(config)"

configuration ::
  Parsec
    String
    ()
    ( TimeoutDuration,
      TreeStyle,
      IncludeTask,
      IncludeHidden,
      AllowListMatching,
      ShowSWISHButton,
      CodeAnalysisConfig,
      [Spec],
      (String, String)
    )
configuration = (\(d, st, it, ih, lm, sb, dc, xs) s -> (d, st, it, ih, lm, sb, dc, xs, s))
  <$> specification <*> sourceText

specification ::
  Parsec
    String
    ()
    ( TimeoutDuration,
      TreeStyle,
      IncludeTask,
      IncludeHidden,
      AllowListMatching,
      ShowSWISHButton,
      CodeAnalysisConfig,
      [Spec]
    )
specification = do
  lines' <- commentBlock
  timeoutStyleAndSpecs <- zip [1 :: Integer ..] lines' `forM` \t ->
    case parseSpecLine t of
      Right spec -> return spec
      Left err   -> fail (show err)
  let TaskConfig {..} = partitionSpecLine $ catMaybes timeoutStyleAndSpecs
  pure
    ( fromMaybe 10000 mTimeout,
      fromMaybe QueryStyle mStyle,
      fromMaybe Yes mIncTask,
      fromMaybe Yes mIncHidden,
      fromMaybe True mListMatch,
      fromMaybe False mSWISHButton,
      CodeAnalysisConfig
        { noSingletonVariables = mNoSingletonVariables
        , allowCutUsage = fromMaybe True mAllowCutUsage
        },
      specifications
    )
  where
    parseSpecLine :: (Integer, String) -> Either ParseError (Maybe SpecLine)
    parseSpecLine (i, s) = parse (
        ((Nothing <$ commentLine)
         <|> (Just <$> ((TimeoutSpec <$> try globalTimeout)
                        <|> TreeStyleSpec <$> try treeStyle
                        <|> IncludeHiddenSpec <$> try includeHidden
                        <|> IncludeTaskSpec <$> try includeTask
                        <|> ListMatchSpec <$> try allowListMatching
                        <|> ShowsSWISHButtonSpec <$> try showSWISHButton
                        <|> NoSingletonVariablesSpec <$> try noSingletonVariablesP
                        <|> AllowCutUsageSpec <$> try allowCutUsageP
                        <|> TestSpec <$> (try newPredDeclParser <|> specLine)))
        ) <* eof)
        ("Specification line " ++ show i) s

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

    includeHidden = do
      void $ string "Include hidden definitions:"
      spaces
      Yes <$ string "yes" <|> Filtered <$ string "filtered"

    includeTask = do
      void $ string "Include task definitions:"
      spaces
      Yes <$ string "yes" <|> Filtered <$ string "filtered" <|> No () <$ string "no"

    globalTimeout = do
      void $ string "Global timeout:"
      spaces
      read <$> many1 digit

    treeStyle = do
      void $ string "Tree style:"
      spaces
      QueryStyle <$ string "query" <|> ResolutionStyle <$ string "resolution"

    allowListMatching = do
      void $ string "Allow list pattern matching:"
      spaces
      True <$ string "yes" <|> False <$ string "no"

    showSWISHButton = do
      void $ string "Show SWISH button:"
      spaces
      True <$ string "yes" <|> False <$ string "no"

    noSingletonVariablesP = do
      void $ string "Detect Singleton Variables:"
      spaces
      problemSeverity

    allowCutUsageP = do
      void $ string "Allow usage of cuts:"
      spaces
      True <$ string "yes" <|> False <$ string "no"

    problemSeverity =
      CA.Hint <$ string "hint"
        <|> CA.Warn <$ string "warn"
        <|> CA.Error <$ string "error"

    localTimeoutAnn = option id $
      localTimeout . read
      <$> between (char '[') (char ']') (many1 digit) <* spaces

    negativeFlag = option id $ negative <$ char '-'

    hiddenFlag   = option id $
      char '!' >> hidden
      <$> option "" (try (between (char '(') (char ')') (many $ noneOf ")")))

    withTreeFlag = option id $ (char '@' >> return withTree)
                           <|> (char '#' >> return withTreeNegative)

commentLine :: Parsec String u String
commentLine = spaces >> char '%' >> many anyChar

commentBlock :: Parsec String u [String]
commentBlock = do
  let startMarker =            string "/* "
  let separator   = endOfLine >> string " * "
  let endMarker   = endOfLine >> string " */"
  let content     = many $ notFollowedBy separator >> notFollowedBy endMarker >> anyToken
  between startMarker endMarker $ content `sepBy` try separator

sourceText :: Parsec String u (String, String)
sourceText = do
  ls <- lines <$> anyChar `manyTill` eof
  let (visiblePart, hiddenPart) = breakWhen ("---" `isPrefixOf`) ls
  return (unlines visiblePart, unlines hiddenPart)

breakWhen :: (a -> Bool) -> [a] -> ([a],[a])
breakWhen p = (takeWhile (not . p) &&& dropWhile (not . p)) >>> second (drop 1)
