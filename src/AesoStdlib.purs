module Aeso.Stdlib(erlps__stdlib_include_path__0) where
{-
This file has been autogenerated
DO NOT EDIT - Your changes WILL be overwritten
Use this code at your own risk - the authors are just a mischievous raccoon and a haskell devote
Erlscripten v0.2.0
-}

import Prelude
import Data.BigInt as DBI
import Data.Array as DA
import Data.Maybe as DM
import Data.Map as Map
import Data.Tuple as DT
import Erlang.Builtins as BIF
import Erlang.Binary as BIN
import Erlang.Helpers
import Erlang.Exception as EXC
import Erlang.Type
import Partial.Unsafe (unsafePartial)


erlps__stdlib_include_path__0 :: ErlangFun
erlps__stdlib_include_path__0 [] =
  let   
    head_1 =
      toErl "aesophia/priv"
  in let head_4 = toErl "stdlib"
  in
    BIF.do_remote_fun_call "Filename" "erlps__join__1"
      [ErlangCons head_1 (ErlangCons head_4 ErlangEmptyList)]
erlps__stdlib_include_path__0 args =
  EXC.badarity (ErlangFun 0 (\ _ -> ErlangAtom "purs_tco_sucks"))
    args