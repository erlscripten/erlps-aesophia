module Aeso.Code.Errors(erlps__format__1, erlps__pos__1) where
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


erlps__format__1 :: ErlangFun
erlps__format__1 [(ErlangTuple [(ErlangAtom "last_declaration_must_be_main_contract"),
                                decl_2@(ErlangTuple [kind_0, _,
                                                     (ErlangTuple [(ErlangAtom "con"),
                                                                   _, c_1]),
                                                     _])])]
  =
  let   
    arg_3 =
      toErl
        "Expected a main contract as the last declaration instead of the ~p \'~s\'\n"
  in let
    msg_9 =
      BIF.do_remote_fun_call "Io.Lib" "erlps__format__2"
        [arg_3, ErlangCons kind_0 (ErlangCons c_1 ErlangEmptyList)]
  in let arg_10 = erlps__pos__1 [decl_2]
  in erlps__mk_err__2 [arg_10, msg_9]
erlps__format__1 [(ErlangTuple [(ErlangAtom "missing_init_function"),
                                con_0])]
  =
  let   
    arg_1 = toErl "Missing init function for the contract \'~s\'.\n"
  in let head_3 = erlps__pp_expr__1 [con_0]
  in let
    msg_6 =
      BIF.do_remote_fun_call "Io.Lib" "erlps__format__2"
        [arg_1, ErlangCons head_3 ErlangEmptyList]
  in let
    cxt_7 =
      toErl
        "The \'init\' function can only be omitted if the state type is \'unit\'.\n"
  in let arg_8 = erlps__pos__1 [con_0]
  in erlps__mk_err__3 [arg_8, msg_6, cxt_7]
erlps__format__1 [(ErlangTuple [(ErlangAtom "missing_definition"),
                                id_0])]
  =
  let    arg_1 = toErl "Missing definition of function \'~s\'.\n"
  in let head_3 = erlps__pp_expr__1 [id_0]
  in let
    msg_6 =
      BIF.do_remote_fun_call "Io.Lib" "erlps__format__2"
        [arg_1, ErlangCons head_3 ErlangEmptyList]
  in let arg_7 = erlps__pos__1 [id_0]
  in erlps__mk_err__2 [arg_7, msg_6]
erlps__format__1 [(ErlangTuple [(ErlangAtom "parameterized_state"),
                                decl_0])]
  =
  let    msg_1 = toErl "The state type cannot be parameterized.\n"
  in let arg_2 = erlps__pos__1 [decl_0]
  in erlps__mk_err__2 [arg_2, msg_1]
erlps__format__1 [(ErlangTuple [(ErlangAtom "parameterized_event"),
                                decl_0])]
  =
  let    msg_1 = toErl "The event type cannot be parameterized.\n"
  in let arg_2 = erlps__pos__1 [decl_0]
  in erlps__mk_err__2 [arg_2, msg_1]
erlps__format__1 [(ErlangTuple [(ErlangAtom "invalid_entrypoint"),
                                why_0, ann_1,
                                (ErlangTuple [(ErlangAtom "id"), _, name_2]),
                                thing_3])]
  =
  let   
    what_5 =
      case why_0 of
        (ErlangAtom "higher_order") ->
          toErl "higher-order (contains function types)"
        (ErlangAtom "polymorphic") ->
          toErl "polymorphic (contains type variables)"
        something_else -> EXC.case_clause something_else
  in let
    things_22 =
      case thing_3 of
        (ErlangTuple [(ErlangAtom "argument"), x_7, t_8]) ->
          let    arg_9 = toErl "argument\n~s\n"
          in let head_11 = erlps__pp_typed__2 [x_7, t_8]
          in
            BIF.do_remote_fun_call "Io.Lib" "erlps__format__2"
              [arg_9, ErlangCons head_11 ErlangEmptyList]
        (ErlangTuple [(ErlangAtom "result"), t_15]) ->
          let    arg_16 = toErl "return type\n~s\n"
          in let arg_19 = toErl 2
          in let head_18 = erlps__pp_type__2 [arg_19, t_15]
          in
            BIF.do_remote_fun_call "Io.Lib" "erlps__format__2"
              [arg_16, ErlangCons head_18 ErlangEmptyList]
        something_else -> EXC.case_clause something_else
  in let
    bad_32 =
      case thing_3 of
        (ErlangTuple [(ErlangAtom "argument"), _, _]) ->
          let arg_24 = toErl "has a ~s type"
          in
            BIF.do_remote_fun_call "Io.Lib" "erlps__format__2"
              [arg_24, ErlangCons what_5 ErlangEmptyList]
        (ErlangTuple [(ErlangAtom "result"), _]) ->
          let arg_28 = toErl "is ~s"
          in
            BIF.do_remote_fun_call "Io.Lib" "erlps__format__2"
              [arg_28, ErlangCons what_5 ErlangEmptyList]
        something_else -> EXC.case_clause something_else
  in let arg_33 = toErl "The ~sof entrypoint \'~s\' ~s.\n"
  in let
    msg_41 =
      BIF.do_remote_fun_call "Io.Lib" "erlps__format__2"
        [arg_33,
         ErlangCons things_22
           (ErlangCons name_2 (ErlangCons bad_32 ErlangEmptyList))]
  in
    case why_0 of
      (ErlangAtom "polymorphic") ->
        let    arg_43 = erlps__pos__1 [ann_1]
        in let
          arg_46 =
            toErl
              "Use the FATE backend if you want polymorphic entrypoints.\n"
        in erlps__mk_err__3 [arg_43, msg_41, arg_46]
      (ErlangAtom "higher_order") ->
        let arg_47 = erlps__pos__1 [ann_1]
        in erlps__mk_err__2 [arg_47, msg_41]
      something_else -> EXC.case_clause something_else
erlps__format__1 [(ErlangTuple [(ErlangAtom "cant_compare_type_aevm"),
                                ann_0, op_1, type_2])]
  =
  let   
    cond_3 =
      BIF.lists__member__2
        [op_1,
         ErlangCons (ErlangAtom "==")
           (ErlangCons (ErlangAtom "!=") ErlangEmptyList)]
  in let
    stringandtuple_11 =
      case cond_3 of
        (ErlangAtom "true") ->
          let
            lcRet_10 =
              toErl "- type string\n- tuple or record of word type\n"
          in ErlangCons lcRet_10 ErlangEmptyList
        _ -> ErlangEmptyList
  in let
    arg_12 =
      toErl
        "Cannot compare values of type\n~s\nThe AEVM only supports \'~s\' on values of\n- word type (int, bool, bits, address, oracle(_, _), etc)\n~s"
  in let arg_15 = toErl 2
  in let head_14 = erlps__pp_type__2 [arg_15, type_2]
  in let
    msg_22 =
      BIF.do_remote_fun_call "Io.Lib" "erlps__format__2"
        [arg_12,
         ErlangCons head_14
           (ErlangCons op_1 (ErlangCons stringandtuple_11 ErlangEmptyList))]
  in let
    cxt_23 =
      toErl "Use FATE if you need to compare arbitrary types.\n"
  in let arg_24 = erlps__pos__1 [ann_0]
  in erlps__mk_err__3 [arg_24, msg_22, cxt_23]
erlps__format__1 [(ErlangTuple [(ErlangAtom "invalid_aens_resolve_type"),
                                ann_0, t_1])]
  =
  let   
    arg_2 =
      toErl
        "Invalid return type of AENS.resolve:\n~s\nIt must be a string or a pubkey type (address, oracle, etc).\n"
  in let arg_5 = toErl 2
  in let head_4 = erlps__pp_type__2 [arg_5, t_1]
  in let
    msg_8 =
      BIF.do_remote_fun_call "Io.Lib" "erlps__format__2"
        [arg_2, ErlangCons head_4 ErlangEmptyList]
  in let arg_9 = erlps__pos__1 [ann_0]
  in erlps__mk_err__2 [arg_9, msg_8]
erlps__format__1 [(ErlangTuple [(ErlangAtom "unapplied_contract_call"),
                                contract_0])]
  =
  let   
    arg_1 =
      toErl
        "The AEVM does not support unapplied contract call to\n~s\n"
  in let arg_4 = toErl 2
  in let head_3 = erlps__pp_expr__2 [arg_4, contract_0]
  in let
    msg_7 =
      BIF.do_remote_fun_call "Io.Lib" "erlps__format__2"
        [arg_1, ErlangCons head_3 ErlangEmptyList]
  in let cxt_8 = toErl "Use FATE if you need this.\n"
  in let arg_9 = erlps__pos__1 [contract_0]
  in erlps__mk_err__3 [arg_9, msg_7, cxt_8]
erlps__format__1 [(ErlangTuple [(ErlangAtom "unapplied_builtin"),
                                id_0])]
  =
  let   
    arg_1 = toErl "The AEVM does not support unapplied use of ~s.\n"
  in let arg_4 = toErl 0
  in let head_3 = erlps__pp_expr__2 [arg_4, id_0]
  in let
    msg_7 =
      BIF.do_remote_fun_call "Io.Lib" "erlps__format__2"
        [arg_1, ErlangCons head_3 ErlangEmptyList]
  in let cxt_8 = toErl "Use FATE if you need this.\n"
  in let arg_9 = erlps__pos__1 [id_0]
  in erlps__mk_err__3 [arg_9, msg_7, cxt_8]
erlps__format__1 [(ErlangTuple [(ErlangAtom "invalid_map_key_type"),
                                why_0, ann_1, type_2])]
  =
  let    arg_3 = toErl "Invalid map key type\n~s\n"
  in let arg_6 = toErl 2
  in let head_5 = erlps__pp_type__2 [arg_6, type_2]
  in let
    msg_9 =
      BIF.do_remote_fun_call "Io.Lib" "erlps__format__2"
        [arg_3, ErlangCons head_5 ErlangEmptyList]
  in let
    cxt_11 =
      case why_0 of
        (ErlangAtom "polymorphic") ->
          toErl
            "Map keys cannot be polymorphic in the AEVM. Use FATE if you need this.\n"
        (ErlangAtom "function") ->
          toErl "Map keys cannot be higher-order.\n"
        something_else -> EXC.case_clause something_else
  in let arg_12 = erlps__pos__1 [ann_1]
  in erlps__mk_err__3 [arg_12, msg_9, cxt_11]
erlps__format__1 [(ErlangTuple [(ErlangAtom "invalid_oracle_type"),
                                why_0, what_1, ann_2, type_3])]
  =
  let   
    whys_5 =
      case why_0 of
        (ErlangAtom "higher_order") ->
          toErl "higher-order (contain function types)"
        (ErlangAtom "polymorphic") ->
          toErl "polymorphic (contain type variables)"
        something_else -> EXC.case_clause something_else
  in let arg_6 = toErl "Invalid oracle type\n~s\n"
  in let arg_9 = toErl 2
  in let head_8 = erlps__pp_type__2 [arg_9, type_3]
  in let
    msg_12 =
      BIF.do_remote_fun_call "Io.Lib" "erlps__format__2"
        [arg_6, ErlangCons head_8 ErlangEmptyList]
  in let arg_13 = toErl "The ~s type must not be ~s.\n"
  in let
    cxt_19 =
      BIF.do_remote_fun_call "Io.Lib" "erlps__format__2"
        [arg_13, ErlangCons what_1 (ErlangCons whys_5 ErlangEmptyList)]
  in let arg_20 = erlps__pos__1 [ann_2]
  in erlps__mk_err__3 [arg_20, msg_12, cxt_19]
erlps__format__1 [(ErlangTuple [(ErlangAtom "higher_order_state"),
                                (ErlangTuple [(ErlangAtom "type_def"), ann_0, _,
                                              _, state_1])])]
  =
  let    arg_2 = toErl "Invalid state type\n~s\n"
  in let arg_5 = toErl 2
  in let head_4 = erlps__pp_type__2 [arg_5, state_1]
  in let
    msg_8 =
      BIF.do_remote_fun_call "Io.Lib" "erlps__format__2"
        [arg_2, ErlangCons head_4 ErlangEmptyList]
  in let
    cxt_9 =
      toErl
        "The state cannot contain functions in the AEVM. Use FATE if you need this.\n"
  in let arg_10 = erlps__pos__1 [ann_0]
  in erlps__mk_err__3 [arg_10, msg_8, cxt_9]
erlps__format__1 [(ErlangTuple [(ErlangAtom "var_args_not_set"),
                                expr_0])]
  =
  let    arg_1 = erlps__pos__1 [expr_0]
  in let
    arg_3 = toErl "Could not deduce type of variable arguments list"
  in let lop_5 = toErl "When compiling "
  in let rop_6 = erlps__pp_expr__1 [expr_0]
  in let arg_4 = BIF.erlang__op_append [lop_5, rop_6]
  in erlps__mk_err__3 [arg_1, arg_3, arg_4]
erlps__format__1 [(ErlangTuple [(ErlangAtom "found_void"),
                                ann_0])]
  =
  let    arg_1 = erlps__pos__1 [ann_0]
  in let arg_3 = toErl "Found a void-typed value."
  in let
    arg_4 =
      toErl
        "`void` is a restricted, uninhabited type. Did you mean `unit`?"
  in erlps__mk_err__3 [arg_1, arg_3, arg_4]
erlps__format__1 [err_0] =
  let    arg_2 = toErl 0
  in let arg_3 = toErl 0
  in let
    arg_1 =
      BIF.do_remote_fun_call "Aeso.Errors" "erlps__pos__2"
        [arg_2, arg_3]
  in let arg_5 = toErl "Unknown error: ~p\n"
  in let
    arg_4 =
      BIF.do_remote_fun_call "Io.Lib" "erlps__format__2"
        [arg_5, ErlangCons err_0 ErlangEmptyList]
  in erlps__mk_err__2 [arg_1, arg_4]
erlps__format__1 [arg_9] = EXC.function_clause unit
erlps__format__1 args =
  EXC.badarity (ErlangFun 1 erlps__format__1) args

erlps__pos__1 :: ErlangFun
erlps__pos__1 [ann_0] =
  let   
    file_4 =
      BIF.do_remote_fun_call "Aeso.Syntax" "erlps__get_ann__3"
        [ErlangAtom "file", ann_0, ErlangAtom "no_file"]
  in let arg_7 = toErl 0
  in let
    line_8 =
      BIF.do_remote_fun_call "Aeso.Syntax" "erlps__get_ann__3"
        [ErlangAtom "line", ann_0, arg_7]
  in let arg_11 = toErl 0
  in let
    col_12 =
      BIF.do_remote_fun_call "Aeso.Syntax" "erlps__get_ann__3"
        [ErlangAtom "col", ann_0, arg_11]
  in
    BIF.do_remote_fun_call "Aeso.Errors" "erlps__pos__3"
      [file_4, line_8, col_12]
erlps__pos__1 [arg_16] = EXC.function_clause unit
erlps__pos__1 args =
  EXC.badarity (ErlangFun 1 erlps__pos__1) args

erlps__pp_typed__2 :: ErlangFun
erlps__pp_typed__2 [e_0, t_1] =
  let    arg_3 = toErl 2
  in let arg_8 = toErl 2
  in let
    arg_5 =
      BIF.erlang__make_fun__3
        [ErlangAtom "prettypr", ErlangAtom "beside", arg_8]
  in let
    arg_9 = BIF.do_remote_fun_call "Prettypr" "erlps__empty__0" []
  in let
    head_11 =
      BIF.do_remote_fun_call "Aeso.Pretty" "erlps__expr__1" [e_0]
  in let arg_15 = toErl " : "
  in let
    head_14 =
      BIF.do_remote_fun_call "Prettypr" "erlps__text__1" [arg_15]
  in let
    head_17 =
      BIF.do_remote_fun_call "Aeso.Pretty" "erlps__type__1" [t_1]
  in let
    arg_4 =
      BIF.do_remote_fun_call "Lists" "erlps__foldr__3"
        [arg_5, arg_9,
         ErlangCons head_11
           (ErlangCons head_14 (ErlangCons head_17 ErlangEmptyList))]
  in let
    arg_2 =
      BIF.do_remote_fun_call "Prettypr" "erlps__nest__2" [arg_3, arg_4]
  in BIF.do_remote_fun_call "Prettypr" "erlps__format__1" [arg_2]
erlps__pp_typed__2 [arg_20, arg_21] = EXC.function_clause unit
erlps__pp_typed__2 args =
  EXC.badarity (ErlangFun 2 erlps__pp_typed__2) args

erlps__pp_expr__1 :: ErlangFun
erlps__pp_expr__1 [e_0] =
  let arg_1 = toErl 0
  in erlps__pp_expr__2 [arg_1, e_0]
erlps__pp_expr__1 [arg_3] = EXC.function_clause unit
erlps__pp_expr__1 args =
  EXC.badarity (ErlangFun 1 erlps__pp_expr__1) args

erlps__pp_expr__2 :: ErlangFun
erlps__pp_expr__2 [n_0, e_1] =
  let   
    arg_4 =
      BIF.do_remote_fun_call "Aeso.Pretty" "erlps__expr__1" [e_1]
  in let
    arg_2 =
      BIF.do_remote_fun_call "Prettypr" "erlps__nest__2" [n_0, arg_4]
  in BIF.do_remote_fun_call "Prettypr" "erlps__format__1" [arg_2]
erlps__pp_expr__2 [arg_6, arg_7] = EXC.function_clause unit
erlps__pp_expr__2 args =
  EXC.badarity (ErlangFun 2 erlps__pp_expr__2) args

erlps__pp_type__2 :: ErlangFun
erlps__pp_type__2 [n_0, t_1] =
  let   
    arg_4 =
      BIF.do_remote_fun_call "Aeso.Pretty" "erlps__type__1" [t_1]
  in let
    arg_2 =
      BIF.do_remote_fun_call "Prettypr" "erlps__nest__2" [n_0, arg_4]
  in BIF.do_remote_fun_call "Prettypr" "erlps__format__1" [arg_2]
erlps__pp_type__2 [arg_6, arg_7] = EXC.function_clause unit
erlps__pp_type__2 args =
  EXC.badarity (ErlangFun 2 erlps__pp_type__2) args

erlps__mk_err__2 :: ErlangFun
erlps__mk_err__2 [pos_0, msg_1] =
  let
    arg_4 =
      BIF.do_remote_fun_call "Lists" "erlps__flatten__1" [msg_1]
  in
    BIF.do_remote_fun_call "Aeso.Errors" "erlps__new__3"
      [ErlangAtom "code_error", pos_0, arg_4]
erlps__mk_err__2 [arg_6, arg_7] = EXC.function_clause unit
erlps__mk_err__2 args =
  EXC.badarity (ErlangFun 2 erlps__mk_err__2) args

erlps__mk_err__3 :: ErlangFun
erlps__mk_err__3 [pos_0, msg_1, cxt_2] =
  let   
    arg_5 =
      BIF.do_remote_fun_call "Lists" "erlps__flatten__1" [msg_1]
  in let
    arg_7 =
      BIF.do_remote_fun_call "Lists" "erlps__flatten__1" [cxt_2]
  in
    BIF.do_remote_fun_call "Aeso.Errors" "erlps__new__4"
      [ErlangAtom "code_error", pos_0, arg_5, arg_7]
erlps__mk_err__3 [arg_9, arg_10, arg_11] =
  EXC.function_clause unit
erlps__mk_err__3 args =
  EXC.badarity (ErlangFun 3 erlps__mk_err__3) args