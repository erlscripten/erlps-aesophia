module AesoScan where

import Control.Monad
import Control.Monad.Except
import Control.Monad.State
import Data.Array
import Data.Either
import Data.Maybe
import Erlang.Type
import Prelude

import Control.Monad.Error.Class (catchError)
import Data.BigInt as DBI
import Data.Char as Char
import Data.String (CodePoint, codePointAt)
import Data.String as CodePoint
import Data.String as Str
import Data.String.CodePoints as CodePoints
import Data.String.Regex (search)
import Data.String.Unsafe as Str
import Erlang.Builtins as BIF

type Pos = {line :: Int, column :: Int}

type LexerState =
  { input :: String
  , output :: ErlangTerm
  , line :: Int
  , column :: Int
  }

initLexerState :: String -> LexerState
initLexerState i =
  { input: i
  , output: ErlangEmptyList
  , line: 0
  , column: 0
  }

data LexerError = LexerError String Pos

type Lexer = StateT LexerState (Except LexerError)

runLex :: forall a. Lexer a -> String -> Either LexerError ErlangTerm
runLex l i = runExcept (evalStateT (l *> output) (initLexerState i))

output :: Lexer ErlangTerm
output = gets $ \s -> BIF.lists__reverse__2 [s.output, ErlangEmptyList]


peek :: Lexer CodePoint
peek = get >>= \s -> case Str.codePointAt 0 s.input of
  Nothing -> fail "Unexpected EOF"
  Just cp -> pure cp

pop :: Lexer CodePoint
pop = do
  c <- peek
  _ <- case Str.singleton c of
    "\n" -> modify $ \s -> s{line = s.line + 1, column = 0, input = Str.drop 1 s.input}
    _ -> modify $ \s -> s{column = s.column + 1, input = Str.drop 1 s.input}
  pure c

pushToken :: ErlangTerm -> Lexer Unit
pushToken tok = modify (\s -> s{output = ErlangCons tok s.output}) *> pure unit

fail :: forall a. String -> Lexer a
fail e = pos >>= \p -> get >>= \st -> throwError $ LexerError e p

codePoint :: CodePoint -> Lexer CodePoint
codePoint cp = do
  c0 <- pop
  when (c0 /= cp)
    (fail $ "Expected " <>
     Str.singleton cp <>
     " got " <>
     Str.singleton c0
    )
  pure cp

char :: Char -> Lexer CodePoint
char = codePoint <<< CodePoints.codePointFromChar


string :: String -> Lexer String
string s0 = go s0 where
  go s = case Str.codePointAt 0 s of
    Nothing -> pure s0
    Just cp -> codePoint cp *> go (Str.drop 1 s)

choice :: forall a. Array (Lexer a) -> Lexer a
choice choices = tryAt 0 where
  tryAt n = case index choices n of
    Nothing -> fail "Out of choices"
    Just parser -> catchError parser (\_ -> tryAt (n + 1))

many :: forall a. Lexer a -> Lexer (Array a)
many p = choice [p >>= \x -> map (cons x) (some p), pure []]

some :: forall a. Lexer a -> Lexer (Array a)
some p = do
  x <- p
  map (cons x) (many p)

whitespace :: Lexer CodePoint
whitespace = choice [char ' ', char '\n']

skipWhitespaces :: Lexer Unit
skipWhitespaces = many whitespace *> pure unit

skipUntil :: forall a. Lexer a -> Lexer a
skipUntil p =
  choice [p, pop *> skipUntil p]

optional :: forall a. Lexer a -> Lexer (Maybe a)
optional p = catchError (map Just p) (\_ -> pure Nothing)

void :: forall a. Lexer a -> Lexer Unit
void p = p *> pure unit

skipLineComment :: Lexer Unit
skipLineComment = void $ optional (string "//" <* skipUntil (char '\n'))

skipBlockComment :: Lexer Unit
skipBlockComment = void $ optional (string "/*" <* skipUntil (string "*/"))

skipBloat :: Lexer Unit
skipBloat = skipWhitespaces *> skipBlockComment *> skipLineComment

eof :: Lexer Unit
eof = get >>= \st -> if Str.null st.input then pure unit else fail "Expected EOF"


pos :: Lexer Pos
pos = gets $ \st -> {line: st.line, column: st.column}

withPos :: forall a. Lexer a -> Lexer {res :: a, pos :: Pos}
withPos p = do
  ps <- pos
  x <- p
  pure {res: x, pos: ps}

named :: forall a. String -> Lexer a -> Lexer a
named name p = do
  l <- pos
  catchError p (\_ -> throwError $ LexerError ("Expected " <> name) l)

assert :: forall a. (a -> Boolean) -> Lexer a -> Lexer a
assert pred p = do
  x <- p
  if pred x then pure x else fail "Invalid token"

lowLetter :: Lexer CodePoint
lowLetter = named "lower case letter" $
  assert (\cp -> cp >= CodePoints.codePointFromChar 'a' && cp <= CodePoints.codePointFromChar 'z') pop

upLetter :: Lexer CodePoint
upLetter = named "upper case letter" $
  assert (\cp -> cp >= CodePoints.codePointFromChar 'A' && cp <= CodePoints.codePointFromChar 'Z') pop

letter :: Lexer CodePoint
letter = choice [upLetter, lowLetter]

digit :: Lexer CodePoint
digit = named "digit" $
  assert (\cp -> cp >= CodePoints.codePointFromChar '0' && cp <= CodePoints.codePointFromChar '9') pop

alphaNum :: Lexer CodePoint
alphaNum = choice [letter, digit]

idChar :: Lexer CodePoint
idChar = choice [alphaNum, char '_']

lId :: Lexer String
lId = named "lower case ID" $
      map Str.fromCodePointArray $ do
        i <- lowLetter
        rest <- many idChar
        pure (cons i rest)

uId :: Lexer String
uId = named "upper case ID" $
      map Str.fromCodePointArray $ do
        i <- upLetter
        rest <- many idChar
        pure (cons i rest)

unsigned :: Lexer DBI.BigInt
unsigned = named "unsigned int" $ do
  digits <- map Str.fromCodePointArray (some digit)
  case DBI.fromString digits of
    Nothing -> fail "Number parse"
    Just i -> pure i

signed :: Lexer DBI.BigInt
signed = named "signed int" $ do
  minus <- optional (char '-')
  num <- unsigned
  case minus of
    Nothing -> pure num
    Just _ -> pure (-num)

escaped :: Lexer CodePoint
escaped = do
  c <- pop
  if c == CodePoints.codePointFromChar '\\'
  then do
     c1 <- pop
     case findMap (\ {l: l, r: r} ->
            if CodePoint.codePointFromChar l == c1
            then Just (CodePoints.codePointFromChar r) else Nothing)
        [{l:'"',  r:'"'}
        {l:'\\', r:'\\'}
        {l:'n',  r:'\n'}
        {l:'r',  r:'\r'}
        {l:'t',  r:'\t'}] of
         Just x -> pure x

        -- 'b',  '\b'
        -- 'e',  '\e'
        -- 'f',  '\f'
        -- 'v',  '\v'
    else pure c
