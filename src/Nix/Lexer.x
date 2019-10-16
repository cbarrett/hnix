{
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -funbox-strict-fields #-}
module Nix.Lexer ( Token(..), Tk(..), Ts(..), Stack
                 , AlexInput(..), alexScan, AlexReturn(..)
                 ) where
                   -- TODO replace token datatypes with combinators

import           Data.Bits (shiftR, (.&.))
import           Data.ByteString (ByteString)
import qualified Data.ByteString as B
import           Data.Char (ord)
import           Data.List.NonEmpty (NonEmpty(..), (<|))
import qualified Data.List.NonEmpty as NE
import           Data.Maybe (fromJust)
import           Data.Text (Text)
import qualified Data.Text as T
import           Data.Word (Word8)

}

%action "AlexAction"

$pseg  = [a-zA-Z0-9\.\_\-\+]

@any   = . | \n
@id    = [a-zA-Z\_] [a-zA-Z0-9\_\'\-]*
@int   = [0-9]+
@float =
  -- all floats one and greater
  (( [1-9] [0-9]* \. [0-9]* )
  -- or decimal, with optional zero
  |( 0?  \. [0-9]+ ))
  -- exponent
  ([Ee] [\+\-]? [0-9]+)?
@path  = $pseg* ( \/ $pseg+ )+ \/?
@hpath = \~ ( \/ $pseg+ )+ \/?
@spath = \< $pseg+ ( \/ $pseg+ )* \>
@uri   =
  [a-zA-Z] [a-zA-Z0-9\+\-\.]* \: [a-zA-Z0-9\%\/\?\:\@\&\=\+\$\,\-\_\.\!\~\*\']+

tokens :-
  if          { tk TIf }
  then        { tk TThen }
  else        { tk TElse }
  assert      { tk TAssert }
  with        { tk TWith }
  let         { tk TLet }
  in          { tk TIn }
  rec         { tk TRec }
  inherit     { tk TInherit }
  or          { tk TOrKW }
  \.\.\.      { tk TEllipsis }

  \=\=        { tk TEq }
  \!\=        { tk TNeq }
  \<\=        { tk TLeq }
  \>\=        { tk TGeq }
  \&\&        { tk TAnd }
  \|\|        { tk TOr }
  \-\>        { tk TImpl }
  \/\/        { tk TUpdate }
  \+\+        { tk TConcat }

  @id    { text TId }

  @int   { \txt s -> (TInt . read . T.unpack $ txt, s) }
  @float { \txt s -> (TFloat . read . fixup . T.unpack $ txt, s)
                 -- In Haskell, `read ".5"` throws
           where fixup ('.':rest) = "0." <> rest
                 fixup xs = xs
         }

  -- TODO other strings

  @path  { text TPath }
  @hpath { text THPath }
  @spath { text TSPath }
  @uri   { text TUri }

  [\ \t\r\n]+ ; -- eat up whitespace
  \# [^\r\n]* ; -- single-line comments
  \/ \* ([^\*] | \*+ [^\*\/])* \*+ \/ ; -- long comments

  @any   { \txt s -> (TChar $ T.head txt, s) }

{
-- Helper token data types
data Tk = TIf | TThen | TElse | TAssert | TWith | TLet | TIn | TRec
  | TInherit | TOrKW | TEllipsis | TEq | TNeq | TLeq | TGeq | TAnd | TOr
  | TImpl | TUpdate | TConcat | TDollarCurly | TIndStrOpen | TIndStrClose
  deriving (Show, Eq, Ord)
data Ts = TId | TPath | THPath | TSPath | TUri
  deriving (Show, Eq, Ord)
-- Data type representing each case of token data we can have
data Token = TTk !Tk
  | TChar !Char | TInt !Int | TFloat !Float
  | TText !Ts !Text
  deriving (Show, Eq, Ord)

-- Action helpers

-- Stack of start codes
type Stack = NonEmpty Int
-- Text of the match and the stack (see %action, above)
type AlexAction = Text -> Stack -> (Token, Stack)

-- Helpers for constructors of Token (besides TInt and TFloat)
tk :: Tk -> AlexAction
tk t _ s = (TTk t, s)
char :: Char -> AlexAction
char c _ s = (TChar c, s)
text :: Ts -> AlexAction
text ts txt s = (TText ts txt, s)
-- Managing the stack (use with Data.Function.&)
push :: Int -> AlexAction -> AlexAction
push sc act = \txt s -> act txt (sc <| s)
pop :: AlexAction -> AlexAction
pop act = \txt s -> let (_, rest) = NE.uncons s
                    in act txt (fromJust rest)

-- Required by Alex
-- Alex doesn't care what this type is; we have to provide it
data AlexInput = AI {
    charBuffer :: ByteString  -- Buffer of current character's bytes
  , input :: Text  -- Rest of the input
  , offset :: !Int  -- Number of characters scanned so far
} deriving (Show, Eq, Ord)

-- For debugging
instance IsString AlexInput where
  fromString = alexInput . T.pack
    where alexInput txt = AI{ input=txt, offset=0, charBuffer=B.empty }

-- Unused optional feature
alexInputPrevChar = undefined

-- Return just one UTF-8 byte of the input, plus the
-- rest of the input, or Nothing if eof
alexGetByte :: AlexInput -> Maybe (Word8, AlexInput)
alexGetByte AI{..} =  -- Adds input, offset and charBuffer to scope
  -- Check buffer
  case B.uncons charBuffer of
    -- Give first byte, if present
    Just (b, rest) -> Just (b,  AI{ input, offset, charBuffer=rest })
    -- Otherwise, buffer is empty, fetch next character from input
    Nothing -> case T.uncons input of
      -- (Unless we've reached eof)
      Nothing -> Nothing
      Just (c, rest) ->
            -- Fill buffer with UTF-8 bytes of `c`
            -- Split out the first byte from the rest
        let (b, cb) = fromJust (B.uncons (utf8Encode c))
            -- Give first byte and new state
        in Just (b, AI{ input=rest, offset=offset + 1, charBuffer=cb })

-- Provided by Alex:
-- alexScan :: AlexInput      -- Current input
--  -> Int                    -- Start code
--  -> AlexReturn AlexAction  -- Parser action to construct a Token

-- Copied from Alex's built-in wrappers (public domain)
utf8Encode :: Char -> ByteString
utf8Encode = B.pack . map fromIntegral . go . ord
 where
  go oc
   | oc <= 0x7f       = [oc]

   | oc <= 0x7ff      = [ 0xc0 + (oc `shiftR` 6)
                        , 0x80 + oc .&. 0x3f
                        ]

   | oc <= 0xffff     = [ 0xe0 + (oc `shiftR` 12)
                        , 0x80 + ((oc `shiftR` 6) .&. 0x3f)
                        , 0x80 + oc .&. 0x3f
                        ]
   | otherwise        = [ 0xf0 + (oc `shiftR` 18)
                        , 0x80 + ((oc `shiftR` 12) .&. 0x3f)
                        , 0x80 + ((oc `shiftR` 6) .&. 0x3f)
                        , 0x80 + oc .&. 0x3f
                        ]


}
