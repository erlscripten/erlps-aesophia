module Aeso.Scan where

import Erlang.Type
import Erlang.Exception as EXC
import Node.Buffer (Buffer)
import Data.Maybe
import Data.BigInt as DBI
import Erlang.Unicode as Unicode

import Prelude

foreign import lexImpl
  :: (String -> ErlangTerm) -- atom constructor
  -> (Array ErlangTerm -> ErlangTerm) -- tuple constructor
  -> (Array ErlangTerm -> ErlangTerm) -- list constructor
  -> (Int -> ErlangTerm) -- int constructor
  -> (String -> ErlangTerm) -- string constructor
  -> (Buffer -> ErlangTerm) -- bytes constructor
  -> String -- input
  -> Array ErlangTerm

runLex :: String -> ErlangTerm
runLex input =
  let tokens = lexImpl
         ErlangAtom
         ErlangTuple
         toErl
         (DBI.fromInt >>> ErlangInt)
         toErl
         ErlangBinary
         input
  in ErlangTuple [ErlangAtom "ok", toErl tokens]

erlps__scan__1 :: ErlangFun
erlps__scan__1 [estr] | Just str <- fromErl estr = runLex str
erlps__scan__1 [_] = EXC.badarg unit
erlps__scan__1 args = EXC.badarity (ErlangFun 1 erlps__scan__1) args

erlps__utf8_encode__1 :: ErlangFun
erlps__utf8_encode__1 [cs] = Unicode.erlps__characters_to_binary__1 [cs]
erlps__utf8_encode__1 args = EXC.badarity (ErlangFun 1 erlps__utf8_encode__1) args
