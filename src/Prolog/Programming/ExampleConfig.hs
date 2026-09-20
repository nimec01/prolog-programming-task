{-# LANGUAGE QuasiQuotes #-}

module Prolog.Programming.ExampleConfig where

import Prolog.Programming.Data (Config (..))
import qualified Text.RawString.QQ as RS (r)

exampleConfig :: Config
exampleConfig =
  Config
    [RS.r|/* Tree style: query
 * Include task definitions: filtered
 * CodeAnalysis rules: error:RestrictCutUsage , warn:NoUnusedVariables
 */
/* As in the lecture, we deal with "Peano numbers" now, i.e., natural numbers
 * represented with constant symbol null/0 and successor function symbol s/1.
 *
 * Complete the following definition of the recursive Prolog predicate isEven/1.
 *
 * isEven(A) should hold if, and only if, A, interpreted as a natural number,
 * is divisible by 2.
 */

/* For example, the following query should succeed:
 *
 * ?- isEven(s(s(s(s(null))))).
 */


  |]
