module Aeso.Syntax(erlps__get_ann__1, erlps__get_ann__2,
                   erlps__get_ann__3, erlps__set_ann__2,
                   erlps__qualify__2) where
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


erlps__get_ann__1 :: ErlangFun
erlps__get_ann__1 [node_0] | isETuple node_0 =
  let arg_1 = toErl 2
  in BIF.erlang__element__2 [arg_1, node_0]
erlps__get_ann__1 [ann_0] | isEList ann_0 = ann_0
erlps__get_ann__1 [arg_1] = EXC.function_clause unit
erlps__get_ann__1 args =
  EXC.badarity (ErlangFun 1 erlps__get_ann__1) args

erlps__set_ann__2 :: ErlangFun
erlps__set_ann__2 [ann1_0, node_1] | isETuple node_1 =
  let arg_2 = toErl 2
  in BIF.erlang__setelement__3 [arg_2, node_1, ann1_0]
erlps__set_ann__2 [ann1_0, ann_1] | isEList ann_1 = ann1_0
erlps__set_ann__2 [arg_2, arg_3] = EXC.function_clause unit
erlps__set_ann__2 args =
  EXC.badarity (ErlangFun 2 erlps__set_ann__2) args

erlps__get_ann__2 :: ErlangFun
erlps__get_ann__2 [key_0, node_1] =
  let arg_3 = erlps__get_ann__1 [node_1]
  in
    BIF.do_remote_fun_call "Proplists" "erlps__get_value__2"
      [key_0, arg_3]
erlps__get_ann__2 [arg_5, arg_6] = EXC.function_clause unit
erlps__get_ann__2 args =
  EXC.badarity (ErlangFun 2 erlps__get_ann__2) args

erlps__get_ann__3 :: ErlangFun
erlps__get_ann__3 [key_0, node_1, default_2] =
  let arg_4 = erlps__get_ann__1 [node_1]
  in
    BIF.do_remote_fun_call "Proplists" "erlps__get_value__3"
      [key_0, arg_4, default_2]
erlps__get_ann__3 [arg_7, arg_8, arg_9] =
  EXC.function_clause unit
erlps__get_ann__3 args =
  EXC.badarity (ErlangFun 3 erlps__get_ann__3) args

erlps__qualify__2 :: ErlangFun
erlps__qualify__2 [(ErlangTuple [(ErlangAtom "con"), ann_0,
                                 n_1]),
                   x_2]
  =
  let
    arg_3 =
      ErlangTuple
        [ErlangAtom "qcon", ann_0, ErlangCons n_1 ErlangEmptyList]
  in erlps__qualify__2 [arg_3, x_2]
erlps__qualify__2 [(ErlangTuple [(ErlangAtom "qcon"), _, ns_0]),
                   (ErlangTuple [(ErlangAtom "con"), ann_1, c_2])]
  =
  let
    tup_el_5 =
      BIF.erlang__op_append [ns_0, ErlangCons c_2 ErlangEmptyList]
  in ErlangTuple [ErlangAtom "qcon", ann_1, tup_el_5]
erlps__qualify__2 [(ErlangTuple [(ErlangAtom "qcon"), _, ns_0]),
                   (ErlangTuple [(ErlangAtom "id"), ann_1, x_2])]
  =
  let
    tup_el_5 =
      BIF.erlang__op_append [ns_0, ErlangCons x_2 ErlangEmptyList]
  in ErlangTuple [ErlangAtom "qid", ann_1, tup_el_5]
erlps__qualify__2 [arg_10, arg_11] = EXC.function_clause unit
erlps__qualify__2 args =
  EXC.badarity (ErlangFun 2 erlps__qualify__2) args