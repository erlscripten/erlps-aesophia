module Aeso.Scan where

import Prelude

import Control.Monad.Except (Except, catchError, runExcept, throwError)
import Control.Monad.State (StateT, evalStateT, get, gets, modify)
import Data.Array (cons, findMap, index)
import Data.BigInt as DBI
import Data.Either (Either(..))
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Char as Char
import Data.String (CodePoint)
import Data.String (codePointAt, drop, fromCodePointArray, null, singleton, toCodePointArray) as Str
import Data.String as CodePoint
import Data.String.CodePoints as CodePoints
import Erlang.Binary as BIN
import Erlang.Builtins as BIF
import Erlang.Exception as EXC
import Erlang.Helpers as H
import Erlang.Type (ErlangFun, ErlangTerm(..), arrayToErlangList)

type Pos = {line :: Int, column :: Int}

newtype LexerState = LexerState
  { input :: String
  , output :: ErlangTerm
  , line :: Int
  , column :: Int
  }
instance showLexerState :: Show LexerState where
  show (LexerState s) = show s


initLexerState :: String -> LexerState
initLexerState i = LexerState
  { input: i
  , output: ErlangEmptyList
  , line: 0
  , column: 0
  }

data LexerError = LexerError String Pos
instance showLexerError :: Show LexerError where
  show = displayLexError
instance eqLexerError :: Eq LexerError where
  eq (LexerError e1 loc1) (LexerError e2 loc2) = e2 == e1 && loc1 == loc2

displayLexError :: LexerError -> String
displayLexError (LexerError msg {line, column}) =
  "Scan error: " <> msg <> " at " <> show line <> ":" <> show column

type Lexer = StateT LexerState (Except LexerError)

runLex :: forall a. Lexer a -> String -> Either LexerError a
runLex l i = runExcept (evalStateT l (initLexerState i))

output :: Lexer ErlangTerm
output = gets $ \(LexerState s) -> BIF.lists__reverse__2 [s.output, ErlangEmptyList]


peek :: Lexer CodePoint
peek = get >>= \(LexerState s) -> case Str.codePointAt 0 s.input of
  Nothing -> fail "Unexpected EOF"
  Just cp -> pure cp

pop :: Lexer CodePoint
pop = do
  c <- peek
  _ <- case Str.singleton c of
    "\n" -> modify $ \(LexerState s) -> LexerState s{line = s.line + 1, column = 0, input = Str.drop 1 s.input}
    _ -> modify $ \(LexerState s) -> LexerState s{column = s.column + 1, input = Str.drop 1 s.input}
  pure c

pushToken :: ErlangTerm -> Lexer Unit
pushToken tok = modify (\(LexerState s) -> LexerState s{output = ErlangCons tok s.output}) *> pure unit

fail :: forall a. String -> Lexer a
fail e = do
  p <- pos
  throwError $ LexerError e p

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
many p = choice
         [ do
              x <- p
              map (cons x) (many p)
         , pure []
         ]

some :: forall a. Lexer a -> Lexer (Array a)
some p = do
  x <- p
  map (cons x) (many p)

whitespace :: Lexer CodePoint
whitespace = choice [char ' ', char '\n']

skipUntil :: forall a. Lexer a -> Lexer a
skipUntil p = choice [p, do
                         _ <- pop
                         skipUntil p
                     ]

optional :: forall a. Lexer a -> Lexer (Maybe a)
optional p = catchError (map Just p) (\_ -> pure Nothing)

void :: forall a. Lexer a -> Lexer Unit
void p = do
  _ <- p
  pure unit

lineComment :: Lexer Unit
lineComment = void (string "//" <* skipUntil (choice [void $ char '\n', eof]))

blockComment :: Lexer Unit
blockComment = void (string "/*" <* skipUntil (string "*/"))

skipBloat :: Lexer Unit
skipBloat = void $ many (choice [void whitespace, blockComment, lineComment])

lex :: forall a. Lexer a -> Lexer a
lex p = do
  x <- p
  skipBloat
  pure x

eof :: Lexer Unit
eof = get >>= \(LexerState st) -> if Str.null st.input then pure unit else fail "Expected EOF"


pos :: Lexer Pos
pos = gets $ \(LexerState st) -> {line: st.line, column: st.column}

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

unsignedHex :: Lexer DBI.BigInt
unsignedHex = named "unsigned hex int" $ do
  void $ string "0x"
  digits <- map Str.fromCodePointArray (some digit)
  case DBI.fromBase 16 digits of
    Nothing -> fail "Number parse"
    Just i -> pure i

signedHex :: Lexer DBI.BigInt
signedHex = named "signed hex int" $ do
  minus <- optional (char '-')
  num <- unsignedHex
  case minus of
    Nothing -> pure num
    Just _ -> pure (-num)

escaped :: Char -> Lexer CodePoint
escaped close = do
  c <- peek
  if c == CodePoints.codePointFromChar '"'
    then fail "Closed context"
    else void pop
  if c == CodePoints.codePointFromChar '\\'
  then named "escaped character" $ do
     c1 <- pop
     case findMap (\ {l: l, r: r} ->
            if CodePoint.codePointFromChar l == c1
            then Just (CodePoints.codePointFromChar r) else Nothing)
        [ {l:'"',  r:'"'}
        , {l:'\\', r:'\\'}
        , {l:'n',  r:'\n'}
        , {l:'r',  r:'\r'}
        , {l:'t',  r:'\t'}
        , {l:'b',  r:fromMaybe '0' $ Char.fromCharCode 8}
        , {l:'e',  r:fromMaybe '0' $ Char.fromCharCode 27}
        , {l:'f',  r:fromMaybe '0' $ Char.fromCharCode 12}
        , {l:'v',  r:fromMaybe '0' $ Char.fromCharCode 11}
        ] of
         Just x -> pure x
         Nothing -> fail $ "Unknown control character " <> Str.singleton c1
  else pure c

stringExpr :: Lexer String
stringExpr = do
  _ <- char '"'
  chars <- many $ escaped '"'
  _ <- char '"'
  pure (Str.fromCodePointArray chars)


charExpr :: Lexer CodePoint
charExpr = do
  _ <- char '\''
  c <- escaped '\''
  _ <- char '\''
  pure c

operatorChar :: Lexer CodePoint
operatorChar = named "operator character" $
  choice $ map char
  [ '=', '!', '<', '>', '+', '\\', '-', '*'
  , '/', ':', '&', '|', '?', '~', '@', '^']

operator :: Lexer String
operator = map Str.fromCodePointArray $ some operatorChar

symbolChar :: Lexer CodePoint
symbolChar = named "symbol character" $
  choice $ map char ['(', ')', '[', ']', '{', '}', ',', '.', '|', ';', '_']


-----------------------------------------------

consumeToken :: Lexer Unit
consumeToken = do
  p <- pos
  let epos = ErlangTuple [ErlangInt $ DBI.fromInt p.line, ErlangInt $ DBI.fromInt p.column]
  tok <- lex $ choice
         [ do
              c <- charExpr
              pure $ ErlangTuple
                [ErlangAtom "char", epos, ErlangInt $ DBI.fromInt $ H.codePointToInt c]
         , do
              s <- stringExpr
              pure $ ErlangTuple
                [ErlangAtom "string", epos, ErlangBinary $ BIN.fromFoldable $ map H.codePointToInt $ Str.toCodePointArray s]
         , do
           i <- choice [signed, signedHex]
           pure $ ErlangTuple
             [ErlangAtom "int", epos, ErlangInt i]
         , do
              k <- choice $ map string
                   ["contract", "include", "let", "switch", "type", "record", "datatype", "if", "elif", "else", "function",
                    "stateful", "payable", "true", "false", "mod", "public", "entrypoint", "private", "indexed", "namespace"]
              pure $ ErlangTuple [ErlangAtom k, epos]
         , do
              qual <- some (uId <* char '.')
              i <- lId
              pure $ ErlangTuple
                [ ErlangAtom "qid", epos
                , arrayToErlangList $ map H.make_string (qual <> [i])
                ]
         , do
              qual <- some (uId <* char '.')
              i <- uId
              pure $ ErlangTuple
                [ ErlangAtom "qcon", epos
                , arrayToErlangList $ map H.make_string (qual <> [i])
                ]
         , do
              _ <- char '\''
              i <- lId
              pure $ ErlangTuple
                [ ErlangAtom "tvar", epos
                , H.make_string ("'" <> i)
                ]
         , do
              i <- lId
              pure $ ErlangTuple
                [ ErlangAtom "id", epos
                , H.make_string i
                ]
         , do
              i <- uId
              pure $ ErlangTuple
                [ ErlangAtom "con", epos
                , H.make_string i
                ]
         , do
              op <- operator
              pure $ ErlangTuple [ErlangAtom op, epos]
         , do
              op <- symbolChar
              pure $ ErlangTuple [ErlangAtom $ Str.singleton op, epos]
         ]
  pushToken tok

sophia :: Lexer Unit
sophia = do
  skipBloat
  _ <- many consumeToken
  eof

runLexSophia :: String -> ErlangTerm
runLexSophia inp =
  case runLex (sophia *> output) inp of
    Right l -> ErlangTuple [ErlangAtom "ok", l]
    Left (LexerError msg p) ->
      ErlangTuple
      [ErlangAtom "error",
       ErlangTuple [
         ErlangTuple [ErlangInt $ DBI.fromInt p.line, ErlangInt $ DBI.fromInt p.column],
         ErlangAtom msg--"scan_error"
         ]
      ]

erlps__scan__1 :: ErlangFun
erlps__scan__1 [estr] | Just str <- H.erlangListToString estr = runLexSophia str
erlps__scan__1 [_] = EXC.badarg unit
erlps__scan__1 args = EXC.badarity (ErlangFun 1 erlps__scan__1) args
