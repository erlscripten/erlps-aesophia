module Aeso.Utils(erlps__scc__1) where
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


erlps__scc__1 :: ErlangFun
erlps__scc__1 [graph_0] =
  let    arg_5 = erlps__reverse_graph__1 [graph_0]
  in let arg_4 = erlps__dff__1 [arg_5]
  in let arg_3 = erlps__postorder__1 [arg_4]
  in let
    arg_2 =
      BIF.do_remote_fun_call "Lists" "erlps__reverse__1" [arg_3]
  in let trees_7 = erlps__dfs__2 [graph_0, arg_2]
  in let
    decode_29 =
      ErlangFun 1
        (let
           lambda_8 [t_10] =
             let case_11 = erlps__postorder__1 [t_10]
             in
               case case_11 of
                 (ErlangCons i_13 (ErlangEmptyList)) ->
                   let   
                     arg_16 =
                       BIF.do_remote_fun_call "Maps" "erlps__get__3"
                         [i_13, graph_0, ErlangEmptyList]
                   in let case_14 = BIF.lists__member__2 [i_13, arg_16]
                   in
                     case case_14 of
                       (ErlangAtom "true") ->
                         ErlangTuple
                           [ErlangAtom "cyclic",
                            ErlangCons i_13 ErlangEmptyList]
                       (ErlangAtom "false") ->
                         ErlangTuple [ErlangAtom "acyclic", i_13]
                       something_else -> EXC.case_clause something_else
                 is_26 -> ErlangTuple [ErlangAtom "cyclic", is_26]
           lambda_8 [arg_9] = EXC.function_clause unit
           lambda_8 args = EXC.badarity (ErlangFun 1 lambda_8) args
         in lambda_8)
  in
    BIF.do_remote_fun_call "Lists" "erlps__map__2"
      [decode_29, trees_7]
erlps__scc__1 [arg_32] = EXC.function_clause unit
erlps__scc__1 args =
  EXC.badarity (ErlangFun 1 (\ _ -> ErlangAtom "purs_tco_sucks"))
    args

erlps__dff__1 :: ErlangFun
erlps__dff__1 [graph_0] =
  let arg_2 = BIF.maps__keys__1 [graph_0]
  in erlps__dfs__2 [graph_0, arg_2]
erlps__dff__1 [arg_4] = EXC.function_clause unit
erlps__dff__1 args =
  EXC.badarity (ErlangFun 1 (\ _ -> ErlangAtom "purs_tco_sucks"))
    args

erlps__dfs__2 :: ErlangFun
erlps__dfs__2 [graph_0, vs_1] =
  let    arg_3 = ErlangMap Map.empty
  in let
    matchExpr_7 =
      erlps__dfs__4 [graph_0, arg_3, vs_1, ErlangEmptyList]
  in
    case matchExpr_7 of
      (ErlangTuple [_, trees_6]) -> trees_6
      _ -> EXC.badmatch matchExpr_7
erlps__dfs__2 [arg_8, arg_9] = EXC.function_clause unit
erlps__dfs__2 args =
  EXC.badarity (ErlangFun 2 (\ _ -> ErlangAtom "purs_tco_sucks"))
    args

erlps__dfs__4 :: ErlangFun
erlps__dfs__4 [_graph_0, visited_1, (ErlangEmptyList), trees_2] =
  let
    tup_el_4 =
      BIF.do_remote_fun_call "Lists" "erlps__reverse__1" [trees_2]
  in ErlangTuple [visited_1, tup_el_4]
erlps__dfs__4 [graph_0, visited_1, (ErlangCons v_2 vs_3),
               trees_4]
  =
  let case_5 = BIF.maps__is_key__2 [v_2, visited_1]
  in
    case case_5 of
      (ErlangAtom "true") ->
        erlps__dfs__4 [graph_0, visited_1, vs_3, trees_4]
      (ErlangAtom "false") ->
        let   
          mapExt_17 = ErlangMap (Map.singleton v_2 (ErlangAtom "true"))
        in let arg_13 = BIF.maps__merge__2 [visited_1, mapExt_17]
        in let matchExpr_22 = erlps__dfs1__3 [graph_0, arg_13, v_2]
        in
          case matchExpr_22 of
            (ErlangTuple [visited1_20, tree_21]) ->
              erlps__dfs__4
                [graph_0, visited1_20, vs_3, ErlangCons tree_21 trees_4]
            _ -> EXC.badmatch matchExpr_22
      something_else -> EXC.case_clause something_else
erlps__dfs__4 [arg_29, arg_30, arg_31, arg_32] =
  EXC.function_clause unit
erlps__dfs__4 args =
  EXC.badarity (ErlangFun 4 (\ _ -> ErlangAtom "purs_tco_sucks"))
    args

erlps__dfs1__3 :: ErlangFun
erlps__dfs1__3 [graph_0, visited_1, v_2] =
  let   
    ws_6 =
      BIF.do_remote_fun_call "Maps" "erlps__get__3"
        [v_2, graph_0, ErlangEmptyList]
  in let
    matchExpr_13 =
      erlps__dfs__4 [graph_0, visited_1, ws_6, ErlangEmptyList]
  in
    case matchExpr_13 of
      (ErlangTuple [visited1_11, trees_12]) ->
        let tup_el_15 = ErlangTuple [v_2, trees_12]
        in ErlangTuple [visited1_11, tup_el_15]
      _ -> EXC.badmatch matchExpr_13
erlps__dfs1__3 [arg_18, arg_19, arg_20] =
  EXC.function_clause unit
erlps__dfs1__3 args =
  EXC.badarity (ErlangFun 3 (\ _ -> ErlangAtom "purs_tco_sucks"))
    args

erlps__postorder__1 :: ErlangFun
erlps__postorder__1 [tree_0@(ErlangTuple [_, _])] =
  erlps__postorder__1 [ErlangCons tree_0 ErlangEmptyList]
erlps__postorder__1 [trees_0] | isEList trees_0 =
  erlps__postorder__2 [trees_0, ErlangEmptyList]
erlps__postorder__1 [arg_3] = EXC.function_clause unit
erlps__postorder__1 args =
  EXC.badarity (ErlangFun 1 (\ _ -> ErlangAtom "purs_tco_sucks"))
    args

erlps__postorder__2 :: ErlangFun
erlps__postorder__2 [(ErlangEmptyList), acc_0] = acc_0
erlps__postorder__2 [(ErlangCons (ErlangTuple [v_0,
                                               trees1_1]) trees_2),
                     acc_3]
  =
  let tail_7 = erlps__postorder__2 [trees_2, acc_3]
  in erlps__postorder__2 [trees1_1, ErlangCons v_0 tail_7]
erlps__postorder__2 [arg_10, arg_11] = EXC.function_clause unit
erlps__postorder__2 args =
  EXC.badarity (ErlangFun 2 (\ _ -> ErlangAtom "purs_tco_sucks"))
    args

erlps__from_edges__2 :: ErlangFun
erlps__from_edges__2 [is_0, es_1] =
  let   
    arg_2 =
      ErlangFun 2
        (let
           lambda_3 [(ErlangTuple [i_6, j_7]), g_8] =
             let
               arg_10 =
                 ErlangFun 1
                   (let
                      lambda_11 [js_13] =
                        BIF.do_remote_fun_call "Lists" "erlps__umerge__2"
                          [ErlangCons j_7 ErlangEmptyList, js_13]
                      lambda_11 [arg_12] = EXC.function_clause unit
                      lambda_11 args = EXC.badarity (ErlangFun 1 lambda_11) args
                    in lambda_11)
             in
               BIF.do_remote_fun_call "Maps" "erlps__update_with__4"
                 [i_6, arg_10, ErlangCons j_7 ErlangEmptyList, g_8]
           lambda_3 [arg_4, arg_5] = EXC.function_clause unit
           lambda_3 args = EXC.badarity (ErlangFun 2 lambda_3) args
         in lambda_3)
  in let
    arg_23 =
      flmap
        (\ lc_26 ->
           let lcRet_27 = ErlangTuple [lc_26, ErlangEmptyList]
           in ErlangCons lcRet_27 ErlangEmptyList)
        is_0
  in let arg_22 = BIF.maps__from_list__1 [arg_23]
  in
    BIF.do_remote_fun_call "Lists" "erlps__foldl__3"
      [arg_2, arg_22, es_1]
erlps__from_edges__2 [arg_31, arg_32] = EXC.function_clause unit
erlps__from_edges__2 args =
  EXC.badarity (ErlangFun 2 (\ _ -> ErlangAtom "purs_tco_sucks"))
    args

erlps__reverse_graph__1 :: ErlangFun
erlps__reverse_graph__1 [g_0] =
  let    arg_1 = BIF.maps__keys__1 [g_0]
  in let lcSrc_4 = BIF.maps__to_list__1 [g_0]
  in let
    arg_3 =
      flmap
        (\ lc_8 ->
           case lc_8 of
             (ErlangTuple [i_6, js_7]) ->
               flmap
                 (\ lc_11 ->
                    let lcRet_12 = ErlangTuple [lc_11, i_6]
                    in ErlangCons lcRet_12 ErlangEmptyList)
                 js_7
             _ -> ErlangEmptyList)
        lcSrc_4
  in erlps__from_edges__2 [arg_1, arg_3]
erlps__reverse_graph__1 [arg_15] = EXC.function_clause unit
erlps__reverse_graph__1 args =
  EXC.badarity (ErlangFun 1 (\ _ -> ErlangAtom "purs_tco_sucks"))
    args