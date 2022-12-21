{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}

{-# OPTIONS_GHC -Wno-missing-signatures #-}
module LexerTests (tests) where

import           Prelude
import           Data.Text (Text)
import qualified Data.Text as T

import Nix.Lexer
import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.TH

case_constant = do
  assertLex "234" [TInt 234]
  assertLex "true" [TText TId "true"]
  assertLex "false" [TText TId "false"]
  assertLex "true-foo" [TText TId "true-foo"]
  assertLex "false-bar" [TText TId "false-bar"]
  assertLex "./. ./+-_/cdef/09ad+- /abc ../abc <abc> <../cdef> a//b rec+def/cdef"
          [ TText TPath "./."
          , TText TPath "./+-_/cdef/09ad+-"
          , TText TPath "/abc"
          , TText TPath "../abc"
          , TText TSPath "abc"
          , TText TSPath "../cdef"
          , TText TId "a", TTk TUpdate, TText TId "b"
          , TText TPath "rec+def/cdef"
          ]
  -- assertParseText "a/b//c/def//<g> < def/d" $ ...
  -- But meanwhile...
  -- nix-repl> a/b//c/def//<g> < def/d
  -- error: path 'a/b/' has a trailing slash
  -- So, do I need to throw error from lexer? Or what?
  assertLex "a'b/c a/b 4/2"
    [ TText TId "a'b"
    , TText TPath "/c"
    , TText TPath "a/b"
    , TText TPath "4/2" ]
  assertLex "a:a http://foo.bar a+de+.adA+-:%%%ads%5asdk&/ rec+def:c"
    [ TText TUri "a:a"
    , TText TUri "http://foo.bar"
    , TText TUri "a+de+.adA+-:%%%ads%5asdk&/"
    , TText TUri "rec+def:c" ]


case_simple_let = do
  assertLex "let a = 4; in a"
    [ TTk TLet, TText TId "a", TChar '=', TInt 4 , TChar ';', TTk TIn, TText TId "a" ]
  assertLex "let a = 4 in a"
    [ TTk TLet, TText TId "a", TChar '=', TInt 4, TTk TIn, TText TId "a" ]

case_identifier_special_chars = do
  assertLex "_a" [TText TId "_a"]
  assertLex "a_b" [TText TId "a_b"]
  assertLex "a'b" [TText TId "a'b"]
  assertLex "a''b" [TText TId "a''b"]
  assertLex "a-b" [TText TId "a-b"]
  assertLex "a--b" [TText TId "a--b"]
  assertLex "a12a" [TText TId "a12a"]
  assertLex ".a" [TChar '.', TText TId "a"]
  assertLex "'a" [TChar '\'', TText TId "a"]

case_string_escape = do
  assertLex "\"\\n\\t\\\\\"" [TChar '"', TText TStr "\n\t\\", TChar '"']
  assertLex "\" \\\" \\' \"" [TChar '"', TText TStr " \" ' ", TChar '"']

case_indented_string = do
  assertLex "''a''" [TTk TIndStrOpen, TText TIndStr "a", TTk TIndStrClose]
  assertLex "''\n  foo\n  bar''" [TTk TIndStrOpen, TText TIndStr "  foo\n  bar", TTk TIndStrClose]
  assertLex "''        ''" [TTk TIndStrOpen, TTk TIndStrClose]
  assertLex "'''''''" [TTk TIndStrOpen, TText TIndStr "''", TTk TIndStrClose]

case_indented_string_escape = do
  assertLex "'' ''\\n ''\\t ''\\\\ \\ \\n ' ''' ''"
    [ TTk TIndStrOpen, TText TIndStr "\n", TText TIndStr " "
    , TText TIndStr "\t", TText TIndStr " ", TText TIndStr "\\"
    , TText TIndStr " \\ \\n ' ", TText TIndStr "''", TText TIndStr " "
    , TTk TIndStrClose ]

tests :: TestTree
tests = $testGroupGenerator

-- Utility

assertLex :: Text -> [Token] -> Assertion
assertLex txt expected = case alexScanTokens txt of
  Right actual ->
    let msg = "When lexing `" ++ T.unpack txt ++ "'"
    in assertEqual msg expected actual
  Left errorOffset ->
    let prefix = "Unexpected error lexing `"
        caret = replicate (length prefix + errorOffset) ' ' ++ "^"
        rest = "' at offset " ++ show errorOffset ++ "\n" ++ caret
    in assertFailure (prefix ++ T.unpack txt ++ rest)

assertFailAt :: Text -> Int -> Assertion
assertFailAt txt o = case alexScanTokens txt of
  Right actual -> assertFailure $ "Unexpected success lexing `" ++ T.unpack txt ++ "'\nLexed tokens: " ++ show actual
  Left errorOffset ->
    let msg = "Unexpected error lexing `" ++ T.unpack txt ++ "'\nExpected error at offset " ++ show o ++ " not " ++ show errorOffset
    in assertEqual msg o errorOffset
