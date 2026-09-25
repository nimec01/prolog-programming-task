{-# LANGUAGE RecordWildCards #-}

module Prolog.Programming.CodeAnalysis
  ( checkForProblems,
    displayProblems,
  )
where

import Data.Text.Lazy (pack)
import Language.Prolog (Program, consultString)
import Prolog.Programming.CodeAnalysis.Config (configuredRules, defaultCodeAnalysisConfig)
import Prolog.Programming.CodeAnalysis.Types
  ( CodeAnalysisConfig (..),
    Problem (..),
    Severity (Error),
    WithSeverity (..),
  )
import Text.PrettyPrint.Leijen.Text (Doc, brackets, string, vsep, (<$$>))

testCheck :: String -> IO [WithSeverity Problem]
testCheck code = case consultString code of
  Left err -> do
    print err
    pure []
  Right prog -> pure $ checkForProblems defaultCodeAnalysisConfig prog

checkForProblems :: CodeAnalysisConfig -> Program -> [WithSeverity Problem]
checkForProblems cfg = concatMap (\c -> concatMap (traverse ($ c)) $ configuredRules cfg)

displayProblems :: [WithSeverity Problem] -> Either Doc Doc
displayProblems pbs =
  cons $
    vsep $
      map
        ( \WithSeverity {..} ->
            padEnd 30 "-" (brackets $ string $ pack $ show severity) <$$> problemDisplay value
        )
        pbs
  where
    cons = if any ((== Error) . severity) pbs then Left else Right

padEnd :: Int -> String -> Doc -> Doc
padEnd maxWidth filler x = x <> mconcat (replicate (max 0 (maxWidth - width x)) $ string $ pack filler)

width :: Doc -> Int
width doc = length $ show doc
