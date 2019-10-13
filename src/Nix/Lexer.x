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
module Nix.Lexer ( TokenStream, tokenStream
                 , Token(..), Tk(..), Ts(..) ) where
                   -- TODO replace with combinators

import           Data.Bits (shiftR, (.&.))
import           Data.ByteString (ByteString)
import qualified Data.ByteString as B
import           Data.Char (ord)
import           Data.Functor
import qualified Data.List as L
import           Data.List.NonEmpty (NonEmpty(..), (<|))
import qualified Data.List.NonEmpty as NE
import           Data.Maybe (fromJust, fromMaybe)
import           Data.Proxy
import           Data.Text (Text)
import qualified Data.Text as T
import           Data.Word (Word8)
import qualified Text.Megaparsec as P

}

%action "AlexAction"

@any   = . | \n
@int   = [0-9]+
@float =
  (([1-9] [0-9]* \. [0-9]*) -- all floats one and greater
  |(0? \. [0-9]+))          -- or optional zero, with decimal
  ([Ee] [\+\-]? [0-9]+)?    -- exponent, like scientific notation

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

  -- TODO identifiers

  @int   { \txt s -> (TInt . read . T.unpack $ txt, s) }
  @float { \txt s -> (TFloat . read . fixup . T.unpack $ txt, s)
                 -- In Haskell, `read ".5"` throws
           where fixup ('.':rest) = "0." <> rest
                 fixup xs = xs
         }

  -- TODO other strings

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
data AlexInput = AlexInput {
    charBuffer :: ByteString  -- Buffer of current character's bytes
  , input :: Text  -- Rest of the input
  , offset :: !Int  -- Number of characters scanned so far
} deriving (Show)

-- For debugging
alexInput :: Text -> AlexInput
alexInput txt = AlexInput { input=txt, offset=0, charBuffer=B.empty }

-- For debugging
instance IsString AlexInput where
  fromString = alexInput . T.pack

-- Unused optional feature
alexInputPrevChar = undefined

-- Return just one UTF-8 byte of the input, plus the
-- rest of the input, or Nothing if eof
alexGetByte :: AlexInput -> Maybe (Word8, AlexInput)
alexGetByte AlexInput{..} =  -- Adds input, offset and charBuffer to scope
  -- Check buffer
  case B.uncons charBuffer of
    -- Give first byte, if present
    Just (b, rest) -> Just (b,  AlexInput{ input, offset, charBuffer=rest })
    -- Otherwise, buffer is empty, fetch next character from input
    Nothing -> case T.uncons input of
      -- (Unless we've reached eof)
      Nothing -> Nothing
      Just (c, rest) ->
            -- Fill buffer with UTF-8 bytes of `c`
            -- Split out the first byte from the rest
        let (b, cb) = fromJust (B.uncons (utf8Encode c))
            -- Give first byte and new state
        in Just (b, AlexInput{ input=rest, offset=offset + 1, charBuffer=cb })

-- Provided by Alex:
-- alexScan :: AlexInput      -- Current input
--  -> Int                    -- Start code
--  -> AlexReturn AlexAction  -- Parser action to construct a Token

-- Megaparsec allows for parsers over custom streams;
-- ours captures the inputs to alexScan and our AlexAction.
data TokenStream = TS {
    input :: Text  -- Rest of the input
  , offset :: !Int  -- Number of characters scanned so far
  , stack :: !Stack  -- Stack of start codes
} deriving (Show)

-- Public constructor
tokenStream :: Text -> TokenStream
tokenStream txt = TS { input=txt, offset=0, stack=0:|[] }

-- For debugging
instance IsString TokenStream where
  fromString = tokenStream . T.pack

-- Megaparsec type class for inputs consumed by parsers.
-- So, parsers can be written in terms of tokens rather than characters.
instance P.Stream TokenStream where
  type Token TokenStream = Nix.Lexer.Token
  type Tokens TokenStream = [Token]
  -- Boilerplate for unused features.
  tokenToChunk Proxy = pure
  tokensToChunk Proxy = id
  chunkToTokens Proxy = id
  chunkLength Proxy = length
  chunkEmpty Proxy = null

  -- Megaparsec documentation:
  -- Extract a single token from the stream. Return Nothing if the
  -- stream is empty.
  take1_ :: TokenStream -> Maybe (Token, TokenStream)
  take1_ =
     \case TS{..}  -- Add input, offset and stack to scope
                   -- Initialize character buffer for alexGetByte
            -> let ai = AlexInput{charBuffer = B.empty, ..}
                   -- Loop until we find a Token or Nothing
               in loop ai stack
    where loop ai@AlexInput{input} stack =
            -- Ask Alex to scan one token (with top start code from the stack)
            case alexScan ai (NE.head stack) of
              -- There is no token if we're eof or have errored
              AlexEOF -> Nothing
              AlexError _ -> Nothing
              -- Skip (e.g. whitespace) by looping with updated AlexInput
              AlexSkip ai' _ -> loop ai' stack
              -- A token has been found! Also we have the rest of the input
              -- and the number of characters scanned so far (offset)
              AlexToken AlexInput{input=i', offset} len act ->
                    -- Take `len` chars from the old input (before we scanned)
                let txt = T.take len input
                    -- Run the lexer action, updating the stack of start codes
                    (token, stack') = act txt stack
                    -- Save input, offset and stack, and return the token
                in Just (token, TS{input=i', offset, stack=stack'})

  -- Megaparsec documentation:
  -- `takeN_ n s` should try to extract a chunk of length `n`, or if the
  -- stream is too short, the rest of the stream. Valid implementation
  -- should follow the rules:
  --
  --     * If the requested length `n` is 0 (or less), Nothing should
  --       never be returned, instead `Just ([], s)` should be returned,
  --       where [] stands for the empty chunk, and `s` is the original
  --       stream (second argument).
  --     * If the requested length is greater than 0 and the stream is
  --       empty, Nothing should be returned indicating end of input.
  --     * In other cases, take chunk of length `n` (or shorter if the
  --       stream is not long enough) from the input stream and return the
  --       chunk along with the rest of the stream.
  --
  takeN_ :: Int -> TokenStream -> Maybe ([Token], TokenStream)
  takeN_ n ts
      -- Base case as promised in the first bullet point above
    | n <= 0    = Just ([], ts)
    | otherwise = do
        -- Delegate to take1_ to find a token (or Nothing, for bullet #2)
        (token, ts') <- P.take1_ ts
        -- Decrement n and loop (ignoring Nothing as per final bullet)
        let (rest, ts'') = fromMaybe ([], ts) $ P.takeN_ (n - 1) ts'
        -- Output list and new stream
        return (token : rest, ts'')

  -- Megaparsec documentation:
  -- Extract chunk of the stream taking tokens while the supplied
  -- predicate returns 'True'. Return the chunk and the rest of the stream.
  --
  -- For many types of streams, the method allows for significant
  -- performance improvements, although it is not strictly necessary from
  -- conceptual point of view.
  --
  takeWhile_ :: (Token -> Bool) -> TokenStream -> ([Token], TokenStream)
  takeWhile_ p ts =
    case P.take1_ ts of
      -- Base case: Nothing in stream
      Nothing -> ([], ts)
      Just (token, ts')
          -- If p is False on token, continue on
        | not (p token) -> P.takeWhile_ p ts'
                                        -- Loop to get rest
        | otherwise -> let (rest, ts'') = P.takeWhile_ p ts'
                       in (token : rest, ts'')

  -- Megaparsec documentation:
  -- Pretty-print non-empty stream of tokens. This function is also used
  -- to print single tokens (represented as singleton lists).
  showTokens :: Proxy TokenStream -> NonEmpty Token -> String
  showTokens Proxy ne = L.intercalate " " . NE.toList $ stringPretty . NE.fromList <$> (ne <&> \case
    TTk k -> tkPretty k
    TChar c -> charPretty c
    TInt i -> show i
    TFloat d -> show d
    TText s txt -> ttextPretty s txt)

  -- Megaparsec uses these to pretty print error messages.
  -- Mostly copied from internal Megaparsec helpers (FreeBSD license).

  -- Megaparsec documentation:
  -- Given an offset o and initial PosState, adjust the state in such a way that it starts at the offset.
  --
  -- Return three values (in order):
  --
  -- SourcePos which the given offset o points to.
  -- String representing the line on which the given offset o is located. The line should satisfy a number of conditions that are described below.
  -- The updated PosState which can be in turn used to locate another offset o' given that o' >= o.
  --
  -- The String representing the offending line in input stream should satisfy the following:
  --
  -- It should adequately represent location of token at the offset of interest, that is, character at sourceColumn of the returned SourcePos should correspond to the token at the offset o.
  -- It should not include the newline at the end.
  -- It should not be empty, if the line happens to be empty, it should be replaced with the string "<empty line>".
  -- Tab characters should be replaced by appropriate number of spaces, which is determined by the pstateTabWidth field of PosState.
  reachOffset :: Int -> P.PosState TokenStream
              -> (P.SourcePos, String, P.PosState TokenStream)
  reachOffset o P.PosState{..} =
    let post = snd . fromJust $ P.takeN_ (o - pstateOffset) pstateInput
        pre  = T.take (offset' post - offset' pstateInput) (input' pstateInput)
        (spos, f) = T.foldl' go (pstateSourcePos, id) pre
        sameLine  = P.sourceLine spos == P.sourceLine pstateSourcePos
        addPrefix xs = if sameLine then pstateLinePrefix ++ xs else xs
        emptyLine xs = if xs == "" then "<empty line>" else xs
        line = emptyLine . (expandTab pstateTabWidth) . addPrefix
             . f . T.unpack . T.takeWhile (/= '\n') $ input' post
    in (spos, line, P.PosState {
         pstateInput = post
       , pstateOffset = max pstateOffset o
       , pstateSourcePos = spos
       , pstateTabWidth = pstateTabWidth
       , pstateLinePrefix = addPrefix (f "")
    })
    where input' ts = input (ts :: TokenStream)
          offset' ts = offset (ts :: TokenStream)
          go (apos, g) ch =
            let P.SourcePos n l c = apos
                c' = P.unPos c
                w  = P.unPos pstateTabWidth
            in if | ch == '\n' -> (P.SourcePos n (l <> P.pos1) P.pos1, id)
                  | ch == '\t' ->
                     (P.SourcePos n l (P.mkPos $ c' + w - ((c' - 1) `rem` w))
                    , g . (ch :))
                  | otherwise -> (P.SourcePos n l (c <> P.pos1), g . (ch :))

  -- Megaparsec documentation:
  -- A version of reachOffset that may be faster because it doesn't need to fetch the line at which the given offset in located.
  reachOffsetNoLine :: Int -> P.PosState TokenStream
                    -> (P.SourcePos, P.PosState TokenStream)
  reachOffsetNoLine o P.PosState{..} =
    let post = snd . fromJust $ P.takeN_ (o - pstateOffset) pstateInput
        spos = T.foldl' go pstateSourcePos (input' pstateInput)
    in (spos, P.PosState {
         pstateInput = post
       , pstateOffset = max pstateOffset o
       , pstateSourcePos = spos
       , pstateTabWidth = pstateTabWidth
       , pstateLinePrefix = pstateLinePrefix
    })
    where input' ts = input (ts :: TokenStream)
          go (P.SourcePos n l c) ch =
            let c' = P.unPos c
                w  = P.unPos pstateTabWidth
            in if | ch == '\n' -> P.SourcePos n (l <> P.pos1) P.pos1
                  | ch == '\t' ->
                     P.SourcePos n l (P.mkPos $ c' + w - ((c' - 1) `rem` w))
                  | otherwise -> P.SourcePos n l (c <> P.pos1)

-- Utility

tkPretty :: Tk -> String
tkPretty = \case
  TIf -> "if" ; TThen -> "then" ; TElse -> "else" ; TAssert -> "assert"
  TWith -> "with" ; TLet -> "let" ; TIn -> "in" ; TRec -> "rec"
  TInherit -> "inherit" ; TOrKW -> "or" ; TEllipsis -> "..." ; TEq -> "=="
  TNeq -> "!=" ; TLeq -> "<=" ; TGeq -> ">=" ; TAnd -> "&&" ; TOr -> "||"
  TImpl -> "->" ; TUpdate -> "//" ; TConcat -> "++" ; TDollarCurly -> "${"
  TIndStrOpen -> "''" ; TIndStrClose -> "''"

ttextPretty :: Ts -> Text -> String
ttextPretty THPath txt = '~' : T.unpack txt
ttextPretty TSPath txt = "<" <> T.unpack txt <> ">"
ttextPretty _      txt = T.unpack txt

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

-- Copied from Megaparesc (FreeBSD license)

-- `stringPretty s` returns pretty representation of string `s`. This is
-- used when printing string tokens in error messages.

stringPretty :: NonEmpty Char -> String
stringPretty (x:|[])      = charPretty x
stringPretty ('\r':|"\n") = "crlf newline"
stringPretty xs           = "\"" <> concatMap f (NE.toList xs) <> "\""
  where
    f ch =
      case charPretty' ch of
        Nothing     -> [ch]
        Just pretty -> "<" <> pretty <> ">"

-- `charPretty ch` returns user-friendly string representation of given
-- character `ch`, suitable for using in error messages.

charPretty :: Char -> String
charPretty ' ' = "space"
charPretty ch = fromMaybe ("'" <> [ch] <> "'") (charPretty' ch)

-- If the given character has a pretty representation, return that,
-- otherwise Nothing. This is an internal helper.

charPretty' :: Char -> Maybe String
charPretty' = \case
  '\NUL' -> Just "null"
  '\SOH' -> Just "start of heading"
  '\STX' -> Just "start of text"
  '\ETX' -> Just "end of text"
  '\EOT' -> Just "end of transmission"
  '\ENQ' -> Just "enquiry"
  '\ACK' -> Just "acknowledge"
  '\BEL' -> Just "bell"
  '\BS'  -> Just "backspace"
  '\t'   -> Just "tab"
  '\n'   -> Just "newline"
  '\v'   -> Just "vertical tab"
  '\f'   -> Just "form feed"
  '\r'   -> Just "carriage return"
  '\SO'  -> Just "shift out"
  '\SI'  -> Just "shift in"
  '\DLE' -> Just "data link escape"
  '\DC1' -> Just "device control one"
  '\DC2' -> Just "device control two"
  '\DC3' -> Just "device control three"
  '\DC4' -> Just "device control four"
  '\NAK' -> Just "negative acknowledge"
  '\SYN' -> Just "synchronous idle"
  '\ETB' -> Just "end of transmission block"
  '\CAN' -> Just "cancel"
  '\EM'  -> Just "end of medium"
  '\SUB' -> Just "substitute"
  '\ESC' -> Just "escape"
  '\FS'  -> Just "file separator"
  '\GS'  -> Just "group separator"
  '\RS'  -> Just "record separator"
  '\US'  -> Just "unit separator"
  '\DEL' -> Just "delete"
  '\160' -> Just "non-breaking space"
  _      -> Nothing

-- Replace tab characters with given number of spaces.

expandTab
  :: P.Pos
  -> String
  -> String
expandTab w' = go 0
  where
    go 0 []        = []
    go 0 ('\t':xs) = go w xs
    go 0 (x:xs)    = x : go 0 xs
    go n xs        = ' ' : go (n - 1) xs
    w              = P.unPos w'

-- End Megaparsec
}
