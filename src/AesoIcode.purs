module Aeso.Icode(erlps__new__1, erlps__pp__1,
                  erlps__set_name__2, erlps__set_namespace__2,
                  erlps__set_payable__2, erlps__enter_namespace__2,
                  erlps__get_namespace__1, erlps__in_main_contract__1,
                  erlps__qualify__2, erlps__set_functions__2,
                  erlps__map_typerep__2, erlps__option_typerep__1,
                  erlps__get_constructor_tag__2) where
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


erlps__pp__1 :: ErlangFun
erlps__pp__1 [icode_0] =
  let arg_1 = toErl "~p~n"
  in
    BIF.do_remote_fun_call "Erlang.Io" "erlps__format__2"
      [arg_1, ErlangCons icode_0 ErlangEmptyList]
erlps__pp__1 [arg_5] = EXC.function_clause unit
erlps__pp__1 args =
  EXC.badarity (ErlangFun 1 (\ _ -> ErlangAtom "purs_tco_sucks"))
    args

erlps__new__1 :: ErlangFun
erlps__new__1 [options_0] =
  let    val_11 = toErl ""
  in let val_13 = erlps__new_env__0 []
  in let val_14 = ErlangTuple [ErlangAtom "tuple", ErlangEmptyList]
  in let val_17 = ErlangTuple [ErlangAtom "tuple", ErlangEmptyList]
  in let val_20 = erlps__builtin_types__0 []
  in let val_21 = ErlangMap Map.empty
  in let val_22 = erlps__builtin_constructors__0 []
  in
    ErlangMap
      (Map.fromFoldable
         [DT.Tuple (ErlangAtom "contract_name") val_11,
          DT.Tuple (ErlangAtom "functions") ErlangEmptyList,
          DT.Tuple (ErlangAtom "env") val_13,
          DT.Tuple (ErlangAtom "state_type") val_14,
          DT.Tuple (ErlangAtom "event_type") val_17,
          DT.Tuple (ErlangAtom "types") val_20,
          DT.Tuple (ErlangAtom "type_vars") val_21,
          DT.Tuple (ErlangAtom "constructors") val_22,
          DT.Tuple (ErlangAtom "options") options_0,
          DT.Tuple (ErlangAtom "payable") (ErlangAtom "false")])
erlps__new__1 [arg_25] = EXC.function_clause unit
erlps__new__1 args =
  EXC.badarity (ErlangFun 1 (\ _ -> ErlangAtom "purs_tco_sucks"))
    args

erlps__builtin_types__0 :: ErlangFun
erlps__builtin_types__0 [] =
  let   
    word_2 =
      ErlangFun 1
        (let
           lambda_0 [(ErlangEmptyList)] = ErlangAtom "word"
           lambda_0 [arg_1] = EXC.function_clause unit
           lambda_0 args = EXC.badarity (ErlangFun 1 lambda_0) args
         in lambda_0)
  in let key_3 = toErl "bool"
  in let key_4 = toErl "int"
  in let key_5 = toErl "char"
  in let key_6 = toErl "bits"
  in let key_7 = toErl "string"
  in let key_8 = toErl "address"
  in let key_9 = toErl "hash"
  in let key_10 = toErl "unit"
  in let key_11 = toErl "signature"
  in let key_12 = toErl "oracle"
  in let key_13 = toErl "oracle_query"
  in let key_14 = toErl "list"
  in let key_15 = toErl "option"
  in let key_16 = toErl "map"
  in let head_18 = toErl "Chain"
  in let head_20 = toErl "ttl"
  in let
    val_26 =
      ErlangFun 1
        (let
           lambda_27 [(ErlangEmptyList)] = ErlangAtom "string"
           lambda_27 [arg_28] = EXC.function_clause unit
           lambda_27 args = EXC.badarity (ErlangFun 1 lambda_27) args
         in lambda_27)
  in let
    val_31 =
      ErlangFun 1
        (let
           lambda_32 [(ErlangEmptyList)] =
             ErlangTuple [ErlangAtom "tuple", ErlangEmptyList]
           lambda_32 [arg_33] = EXC.function_clause unit
           lambda_32 args = EXC.badarity (ErlangFun 1 lambda_32) args
         in lambda_32)
  in let
    val_36 =
      ErlangFun 1
        (let
           lambda_37 [(ErlangEmptyList)] =
             ErlangTuple
               [ErlangAtom "tuple",
                ErlangCons (ErlangAtom "word")
                  (ErlangCons (ErlangAtom "word") ErlangEmptyList)]
           lambda_37 [arg_38] = EXC.function_clause unit
           lambda_37 args = EXC.badarity (ErlangFun 1 lambda_37) args
         in lambda_37)
  in let
    val_45 =
      ErlangFun 1
        (let
           lambda_46 [(ErlangCons _ (ErlangCons _ (ErlangEmptyList)))] =
             ErlangAtom "word"
           lambda_46 [arg_47] = EXC.function_clause unit
           lambda_46 args = EXC.badarity (ErlangFun 1 lambda_46) args
         in lambda_46)
  in let
    val_48 =
      ErlangFun 1
        (let
           lambda_49 [(ErlangCons _ (ErlangCons _ (ErlangEmptyList)))] =
             ErlangAtom "word"
           lambda_49 [arg_50] = EXC.function_clause unit
           lambda_49 args = EXC.badarity (ErlangFun 1 lambda_49) args
         in lambda_49)
  in let
    val_51 =
      ErlangFun 1
        (let
           lambda_52 [(ErlangCons a_54 (ErlangEmptyList))] =
             ErlangTuple [ErlangAtom "list", a_54]
           lambda_52 [arg_53] = EXC.function_clause unit
           lambda_52 args = EXC.badarity (ErlangFun 1 lambda_52) args
         in lambda_52)
  in let
    val_57 =
      ErlangFun 1
        (let
           lambda_58 [(ErlangCons a_60 (ErlangEmptyList))] =
             ErlangTuple
               [ErlangAtom "variant",
                ErlangCons ErlangEmptyList
                  (ErlangCons (ErlangCons a_60 ErlangEmptyList)
                     ErlangEmptyList)]
           lambda_58 [arg_59] = EXC.function_clause unit
           lambda_58 args = EXC.badarity (ErlangFun 1 lambda_58) args
         in lambda_58)
  in let
    val_69 =
      ErlangFun 1
        (let
           lambda_70 [(ErlangCons k_72 (ErlangCons v_73 (ErlangEmptyList)))]
             =
             erlps__map_typerep__2 [k_72, v_73]
           lambda_70 [arg_71] = EXC.function_clause unit
           lambda_70 args = EXC.badarity (ErlangFun 1 lambda_70) args
         in lambda_70)
  in let
    val_76 =
      ErlangFun 1
        (let
           lambda_77 [(ErlangEmptyList)] =
             ErlangTuple
               [ErlangAtom "variant",
                ErlangCons (ErlangCons (ErlangAtom "word") ErlangEmptyList)
                  (ErlangCons (ErlangCons (ErlangAtom "word") ErlangEmptyList)
                     ErlangEmptyList)]
           lambda_77 [arg_78] = EXC.function_clause unit
           lambda_77 args = EXC.badarity (ErlangFun 1 lambda_77) args
         in lambda_77)
  in
    ErlangMap
      (Map.fromFoldable
         [DT.Tuple key_3 word_2, DT.Tuple key_4 word_2,
          DT.Tuple key_5 word_2, DT.Tuple key_6 word_2,
          DT.Tuple key_7 val_26, DT.Tuple key_8 word_2,
          DT.Tuple key_9 word_2, DT.Tuple key_10 val_31,
          DT.Tuple key_11 val_36, DT.Tuple key_12 val_45,
          DT.Tuple key_13 val_48, DT.Tuple key_14 val_51,
          DT.Tuple key_15 val_57, DT.Tuple key_16 val_69,
          DT.Tuple
            (ErlangCons head_18 (ErlangCons head_20 ErlangEmptyList))
            val_76])
erlps__builtin_types__0 args =
  EXC.badarity (ErlangFun 0 (\ _ -> ErlangAtom "purs_tco_sucks"))
    args

erlps__builtin_constructors__0 :: ErlangFun
erlps__builtin_constructors__0 [] =
  let    head_1 = toErl "RelativeTTL"
  in let head_4 = toErl "FixedTTL"
  in let head_7 = toErl "None"
  in let head_10 = toErl "Some"
  in let val_12 = toErl 0
  in let val_13 = toErl 1
  in let val_14 = toErl 0
  in let val_15 = toErl 1
  in
    ErlangMap
      (Map.fromFoldable
         [DT.Tuple (ErlangCons head_1 ErlangEmptyList) val_12,
          DT.Tuple (ErlangCons head_4 ErlangEmptyList) val_13,
          DT.Tuple (ErlangCons head_7 ErlangEmptyList) val_14,
          DT.Tuple (ErlangCons head_10 ErlangEmptyList) val_15])
erlps__builtin_constructors__0 args =
  EXC.badarity (ErlangFun 0 (\ _ -> ErlangAtom "purs_tco_sucks"))
    args

erlps__map_typerep__2 :: ErlangFun
erlps__map_typerep__2 [k_0, v_1] =
  ErlangTuple [ErlangAtom "map", k_0, v_1]
erlps__map_typerep__2 [arg_5, arg_6] = EXC.function_clause unit
erlps__map_typerep__2 args =
  EXC.badarity (ErlangFun 2 (\ _ -> ErlangAtom "purs_tco_sucks"))
    args

erlps__option_typerep__1 :: ErlangFun
erlps__option_typerep__1 [a_0] =
  ErlangTuple
    [ErlangAtom "variant",
     ErlangCons ErlangEmptyList
       (ErlangCons (ErlangCons a_0 ErlangEmptyList) ErlangEmptyList)]
erlps__option_typerep__1 [arg_9] = EXC.function_clause unit
erlps__option_typerep__1 args =
  EXC.badarity (ErlangFun 1 (\ _ -> ErlangAtom "purs_tco_sucks"))
    args

erlps__new_env__0 :: ErlangFun
erlps__new_env__0 [] = ErlangEmptyList
erlps__new_env__0 args =
  EXC.badarity (ErlangFun 0 (\ _ -> ErlangAtom "purs_tco_sucks"))
    args

erlps__set_name__2 :: ErlangFun
erlps__set_name__2 [name_0, icode_1] =
  BIF.maps__put__3 [ErlangAtom "contract_name", name_0, icode_1]
erlps__set_name__2 [arg_5, arg_6] = EXC.function_clause unit
erlps__set_name__2 args =
  EXC.badarity (ErlangFun 2 (\ _ -> ErlangAtom "purs_tco_sucks"))
    args

erlps__set_payable__2 :: ErlangFun
erlps__set_payable__2 [payable_0, icode_1] =
  BIF.maps__put__3 [ErlangAtom "payable", payable_0, icode_1]
erlps__set_payable__2 [arg_5, arg_6] = EXC.function_clause unit
erlps__set_payable__2 args =
  EXC.badarity (ErlangFun 2 (\ _ -> ErlangAtom "purs_tco_sucks"))
    args

erlps__set_namespace__2 :: ErlangFun
erlps__set_namespace__2 [ns_0, icode_1] =
  let
    mapExt_5 =
      ErlangMap (Map.singleton (ErlangAtom "namespace") ns_0)
  in BIF.maps__merge__2 [icode_1, mapExt_5]
erlps__set_namespace__2 [arg_7, arg_8] = EXC.function_clause unit
erlps__set_namespace__2 args =
  EXC.badarity (ErlangFun 2 (\ _ -> ErlangAtom "purs_tco_sucks"))
    args

erlps__enter_namespace__2 :: ErlangFun
erlps__enter_namespace__2 [ns_0, icode_3@(ErlangMap map_1)]
  | (DM.Just ns1_2) <-
      (Map.lookup (ErlangAtom "namespace") map_1) =
  let   
    val_6 =
      BIF.do_remote_fun_call "Aeso.Syntax" "erlps__qualify__2"
        [ns1_2, ns_0]
  in let
    mapExt_9 =
      ErlangMap (Map.singleton (ErlangAtom "namespace") val_6)
  in BIF.maps__merge__2 [icode_3, mapExt_9]
erlps__enter_namespace__2 [ns_0, icode_1] =
  let
    mapExt_5 =
      ErlangMap (Map.singleton (ErlangAtom "namespace") ns_0)
  in BIF.maps__merge__2 [icode_1, mapExt_5]
erlps__enter_namespace__2 [arg_7, arg_8] =
  EXC.function_clause unit
erlps__enter_namespace__2 args =
  EXC.badarity (ErlangFun 2 (\ _ -> ErlangAtom "purs_tco_sucks"))
    args

erlps__in_main_contract__1 :: ErlangFun
erlps__in_main_contract__1 [(ErlangMap map_0)]
  | (DM.Just main_2) <-
      (Map.lookup (ErlangAtom "contract_name") map_0)
  , (DM.Just (ErlangTuple [(ErlangAtom "con"), _, main_1])) <-
      (Map.lookup (ErlangAtom "namespace") map_0)
  , (main_2 == main_1) =
  ErlangAtom "true"
erlps__in_main_contract__1 [_icode_0] = ErlangAtom "false"
erlps__in_main_contract__1 [arg_1] = EXC.function_clause unit
erlps__in_main_contract__1 args =
  EXC.badarity (ErlangFun 1 (\ _ -> ErlangAtom "purs_tco_sucks"))
    args

erlps__get_namespace__1 :: ErlangFun
erlps__get_namespace__1 [icode_0] =
  BIF.do_remote_fun_call "Maps" "erlps__get__3"
    [ErlangAtom "namespace", icode_0, ErlangAtom "false"]
erlps__get_namespace__1 [arg_4] = EXC.function_clause unit
erlps__get_namespace__1 args =
  EXC.badarity (ErlangFun 1 (\ _ -> ErlangAtom "purs_tco_sucks"))
    args

erlps__qualify__2 :: ErlangFun
erlps__qualify__2 [x_0, icode_1] =
  let case_2 = erlps__get_namespace__1 [icode_1]
  in
    case case_2 of
      (ErlangAtom "false") -> x_0
      ns_4 ->
        BIF.do_remote_fun_call "Aeso.Syntax" "erlps__qualify__2"
          [ns_4, x_0]
erlps__qualify__2 [arg_7, arg_8] = EXC.function_clause unit
erlps__qualify__2 args =
  EXC.badarity (ErlangFun 2 (\ _ -> ErlangAtom "purs_tco_sucks"))
    args

erlps__set_functions__2 :: ErlangFun
erlps__set_functions__2 [newfuns_0, icode_1] =
  BIF.maps__put__3 [ErlangAtom "functions", newfuns_0, icode_1]
erlps__set_functions__2 [arg_5, arg_6] = EXC.function_clause unit
erlps__set_functions__2 args =
  EXC.badarity (ErlangFun 2 (\ _ -> ErlangAtom "purs_tco_sucks"))
    args

erlps__get_constructor_tag__2 :: ErlangFun
erlps__get_constructor_tag__2 [name_0, (ErlangMap map_1)]
  | (DM.Just constructors_2) <-
      (Map.lookup (ErlangAtom "constructors") map_1) =
  let
    case_3 =
      BIF.do_remote_fun_call "Maps" "erlps__get__3"
        [name_0, constructors_2, ErlangAtom "undefined"]
  in
    case case_3 of
      (ErlangAtom "undefined") ->
        let
          arg_7 = ErlangTuple [ErlangAtom "undefined_constructor", name_0]
        in BIF.erlang__error__1 [arg_7]
      tag_10 -> tag_10
erlps__get_constructor_tag__2 [arg_11, arg_12] =
  EXC.function_clause unit
erlps__get_constructor_tag__2 args =
  EXC.badarity (ErlangFun 2 (\ _ -> ErlangAtom "purs_tco_sucks"))
    args