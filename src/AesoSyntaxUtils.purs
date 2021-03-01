module Aeso.Syntax.Utils(erlps__used_ids__1,
                         erlps__used_types__2, erlps__used__1) where
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


erlps__fold__4 :: ErlangFun
erlps__fold__4 [alg_3@(ErlangTuple [(ErlangAtom "alg"), zero_0,
                                    plus_1, scoped_2]),
                fun_4, k_5, x_6]
  =
  let   
    sum_13 =
      ErlangFun 1
        (let
           lambda_7 [xs_9] =
             BIF.do_remote_fun_call "Lists" "erlps__foldl__3"
               [plus_1, zero_0, xs_9]
           lambda_7 [arg_8] = EXC.function_clause unit
           lambda_7 args = EXC.badarity (ErlangFun 1 lambda_7) args
         in lambda_7)
  in let
    same_21 =
      ErlangFun 1
        (let
           lambda_14 [a_16] = erlps__fold__4 [alg_3, fun_4, k_5, a_16]
           lambda_14 [arg_15] = EXC.function_clause unit
           lambda_14 args = EXC.badarity (ErlangFun 1 lambda_14) args
         in lambda_14)
  in let
    decl_29 =
      ErlangFun 1
        (let
           lambda_22 [d_24] =
             erlps__fold__4 [alg_3, fun_4, ErlangAtom "decl", d_24]
           lambda_22 [arg_23] = EXC.function_clause unit
           lambda_22 args = EXC.badarity (ErlangFun 1 lambda_22) args
         in lambda_22)
  in let
    type_37 =
      ErlangFun 1
        (let
           lambda_30 [t_32] =
             erlps__fold__4 [alg_3, fun_4, ErlangAtom "type", t_32]
           lambda_30 [arg_31] = EXC.function_clause unit
           lambda_30 args = EXC.badarity (ErlangFun 1 lambda_30) args
         in lambda_30)
  in let
    expr_45 =
      ErlangFun 1
        (let
           lambda_38 [e_40] =
             erlps__fold__4 [alg_3, fun_4, ErlangAtom "expr", e_40]
           lambda_38 [arg_39] = EXC.function_clause unit
           lambda_38 args = EXC.badarity (ErlangFun 1 lambda_38) args
         in lambda_38)
  in let
    bindexpr_53 =
      ErlangFun 1
        (let
           lambda_46 [p_48] =
             erlps__fold__4 [alg_3, fun_4, ErlangAtom "bind_expr", p_48]
           lambda_46 [arg_47] = EXC.function_clause unit
           lambda_46 args = EXC.badarity (ErlangFun 1 lambda_46) args
         in lambda_46)
  in let
    bindtype_61 =
      ErlangFun 1
        (let
           lambda_54 [t_56] =
             erlps__fold__4 [alg_3, fun_4, ErlangAtom "bind_type", t_56]
           lambda_54 [arg_55] = EXC.function_clause unit
           lambda_54 args = EXC.badarity (ErlangFun 1 lambda_54) args
         in lambda_54)
  in let
    top_65 =
      BIF.erlang__apply__2
        [fun_4, ErlangCons k_5 (ErlangCons x_6 ErlangEmptyList)]
  in let
    rec_410 =
      case x_6 of
        (ErlangCons a_67 as_68) ->
          let   
            arg_69 =
              BIF.erlang__apply__2 [same_21, ErlangCons a_67 ErlangEmptyList]
          in let
            arg_72 =
              BIF.erlang__apply__2 [same_21, ErlangCons as_68 ErlangEmptyList]
          in
            BIF.erlang__apply__2
              [scoped_2, ErlangCons arg_69 (ErlangCons arg_72 ErlangEmptyList)]
        (ErlangTuple [(ErlangAtom "contract"), _, _, ds_76]) ->
          BIF.erlang__apply__2 [decl_29, ErlangCons ds_76 ErlangEmptyList]
        (ErlangTuple [(ErlangAtom "namespace"), _, _, ds_79]) ->
          BIF.erlang__apply__2 [decl_29, ErlangCons ds_79 ErlangEmptyList]
        (ErlangTuple [(ErlangAtom "type_def"), _, i_82, _, d_83]) ->
          let   
            arg_84 =
              BIF.erlang__apply__2
                [bindtype_61, ErlangCons i_82 ErlangEmptyList]
          in let
            arg_87 =
              BIF.erlang__apply__2 [decl_29, ErlangCons d_83 ErlangEmptyList]
          in
            BIF.erlang__apply__2
              [plus_1, ErlangCons arg_84 (ErlangCons arg_87 ErlangEmptyList)]
        (ErlangTuple [(ErlangAtom "fun_decl"), _, _, t_91]) ->
          BIF.erlang__apply__2 [type_37, ErlangCons t_91 ErlangEmptyList]
        (ErlangTuple [(ErlangAtom "letval"), _, p_94, e_95]) ->
          let   
            arg_96 =
              BIF.erlang__apply__2
                [bindexpr_53, ErlangCons p_94 ErlangEmptyList]
          in let
            arg_99 =
              BIF.erlang__apply__2 [expr_45, ErlangCons e_95 ErlangEmptyList]
          in
            BIF.erlang__apply__2
              [scoped_2, ErlangCons arg_96 (ErlangCons arg_99 ErlangEmptyList)]
        (ErlangTuple [(ErlangAtom "letfun"), _, f_103, xs_104, t_105,
                      e_106]) ->
          let   
            head_108 =
              BIF.erlang__apply__2
                [bindexpr_53, ErlangCons f_103 ErlangEmptyList]
          in let
            head_112 =
              BIF.erlang__apply__2 [type_37, ErlangCons t_105 ErlangEmptyList]
          in let
            arg_117 =
              BIF.erlang__op_append [xs_104, ErlangCons e_106 ErlangEmptyList]
          in let
            head_116 =
              BIF.erlang__apply__2
                [expr_45, ErlangCons arg_117 ErlangEmptyList]
          in
            BIF.erlang__apply__2
              [sum_13,
               ErlangCons
                 (ErlangCons head_108
                    (ErlangCons head_112 (ErlangCons head_116 ErlangEmptyList)))
                 ErlangEmptyList]
        (ErlangTuple [(ErlangAtom "fun_clauses"), _, _, t_125,
                      cs_126]) ->
          let   
            head_128 =
              BIF.erlang__apply__2 [type_37, ErlangCons t_125 ErlangEmptyList]
          in let
            tail_131 =
              flmap
                (\ lc_134 ->
                   let
                     lcRet_135 =
                       BIF.erlang__apply__2
                         [decl_29, ErlangCons lc_134 ErlangEmptyList]
                   in ErlangCons lcRet_135 ErlangEmptyList)
                cs_126
          in
            BIF.erlang__apply__2
              [sum_13,
               ErlangCons (ErlangCons head_128 tail_131) ErlangEmptyList]
        (ErlangTuple [(ErlangAtom "alias_t"), t_139]) ->
          BIF.erlang__apply__2 [type_37, ErlangCons t_139 ErlangEmptyList]
        (ErlangTuple [(ErlangAtom "record_t"), fs_142]) ->
          BIF.erlang__apply__2 [type_37, ErlangCons fs_142 ErlangEmptyList]
        (ErlangTuple [(ErlangAtom "variant_t"), cs_145]) ->
          BIF.erlang__apply__2 [type_37, ErlangCons cs_145 ErlangEmptyList]
        (ErlangTuple [(ErlangAtom "field_t"), _, _, t_148]) ->
          BIF.erlang__apply__2 [type_37, ErlangCons t_148 ErlangEmptyList]
        (ErlangTuple [(ErlangAtom "constr_t"), _, _, ts_151]) ->
          BIF.erlang__apply__2 [type_37, ErlangCons ts_151 ErlangEmptyList]
        (ErlangTuple [(ErlangAtom "fun_t"), _, named_154, args_155,
                      ret_156]) ->
          BIF.erlang__apply__2
            [type_37,
             ErlangCons
               (ErlangCons named_154
                  (ErlangCons args_155 (ErlangCons ret_156 ErlangEmptyList)))
               ErlangEmptyList]
        (ErlangTuple [(ErlangAtom "app_t"), _, t_165, ts_166]) ->
          BIF.erlang__apply__2
            [type_37, ErlangCons (ErlangCons t_165 ts_166) ErlangEmptyList]
        (ErlangTuple [(ErlangAtom "tuple_t"), _, ts_171]) ->
          BIF.erlang__apply__2 [type_37, ErlangCons ts_171 ErlangEmptyList]
        (ErlangTuple [(ErlangAtom "named_arg_t"), _, _, t_174, e_175]) ->
          let   
            arg_176 =
              BIF.erlang__apply__2 [type_37, ErlangCons t_174 ErlangEmptyList]
          in let
            arg_179 =
              BIF.erlang__apply__2 [expr_45, ErlangCons e_175 ErlangEmptyList]
          in
            BIF.erlang__apply__2
              [plus_1, ErlangCons arg_176 (ErlangCons arg_179 ErlangEmptyList)]
        (ErlangTuple [(ErlangAtom "lam"), _, args_183, e_184]) ->
          let   
            arg_185 =
              BIF.erlang__apply__2
                [bindexpr_53, ErlangCons args_183 ErlangEmptyList]
          in let
            arg_188 =
              BIF.erlang__apply__2 [expr_45, ErlangCons e_184 ErlangEmptyList]
          in
            BIF.erlang__apply__2
              [scoped_2,
               ErlangCons arg_185 (ErlangCons arg_188 ErlangEmptyList)]
        (ErlangTuple [(ErlangAtom "if"), _, a_192, b_193, c_194]) ->
          BIF.erlang__apply__2
            [expr_45,
             ErlangCons
               (ErlangCons a_192
                  (ErlangCons b_193 (ErlangCons c_194 ErlangEmptyList)))
               ErlangEmptyList]
        (ErlangTuple [(ErlangAtom "switch"), _, e_203, alts_204]) ->
          BIF.erlang__apply__2
            [expr_45,
             ErlangCons
               (ErlangCons e_203 (ErlangCons alts_204 ErlangEmptyList))
               ErlangEmptyList]
        (ErlangTuple [(ErlangAtom "app"), _, a_211, as_212]) ->
          BIF.erlang__apply__2
            [expr_45, ErlangCons (ErlangCons a_211 as_212) ErlangEmptyList]
        (ErlangTuple [(ErlangAtom "proj"), _, e_217, _]) ->
          BIF.erlang__apply__2 [expr_45, ErlangCons e_217 ErlangEmptyList]
        (ErlangTuple [(ErlangAtom "tuple"), _, as_220]) ->
          BIF.erlang__apply__2 [expr_45, ErlangCons as_220 ErlangEmptyList]
        (ErlangTuple [(ErlangAtom "list"), _, as_223]) ->
          BIF.erlang__apply__2 [expr_45, ErlangCons as_223 ErlangEmptyList]
        (ErlangTuple [(ErlangAtom "list_comp"), _, y_226,
                      (ErlangEmptyList)]) ->
          BIF.erlang__apply__2 [expr_45, ErlangCons y_226 ErlangEmptyList]
        (ErlangTuple [(ErlangAtom "list_comp"), a_229, y_230,
                      (ErlangCons (ErlangTuple [(ErlangAtom "comprehension_bind"),
                                                i_231, e_232]) r_233)]) ->
          let   
            arg_234 =
              BIF.erlang__apply__2 [expr_45, ErlangCons e_232 ErlangEmptyList]
          in let
            arg_238 =
              BIF.erlang__apply__2
                [bindexpr_53, ErlangCons i_231 ErlangEmptyList]
          in let
            arg_242 =
              ErlangTuple [ErlangAtom "list_comp", a_229, y_230, r_233]
          in let
            arg_241 =
              BIF.erlang__apply__2
                [expr_45, ErlangCons arg_242 ErlangEmptyList]
          in let
            arg_237 =
              BIF.erlang__apply__2
                [scoped_2,
                 ErlangCons arg_238 (ErlangCons arg_241 ErlangEmptyList)]
          in
            BIF.erlang__apply__2
              [plus_1, ErlangCons arg_234 (ErlangCons arg_237 ErlangEmptyList)]
        (ErlangTuple [(ErlangAtom "list_comp"), a_250, y_251,
                      (ErlangCons (ErlangTuple [(ErlangAtom "comprehension_if"),
                                                _, e_252]) r_253)]) ->
          let   
            arg_254 =
              BIF.erlang__apply__2 [expr_45, ErlangCons e_252 ErlangEmptyList]
          in let
            arg_258 =
              ErlangTuple [ErlangAtom "list_comp", a_250, y_251, r_253]
          in let
            arg_257 =
              BIF.erlang__apply__2
                [expr_45, ErlangCons arg_258 ErlangEmptyList]
          in
            BIF.erlang__apply__2
              [plus_1, ErlangCons arg_254 (ErlangCons arg_257 ErlangEmptyList)]
        (ErlangTuple [(ErlangAtom "list_comp"), a_265, y_266,
                      (ErlangCons d_268@(ErlangTuple [(ErlangAtom "letval"), _,
                                                      pat_267, _]) r_269)]) ->
          let   
            arg_270 =
              BIF.erlang__apply__2 [decl_29, ErlangCons d_268 ErlangEmptyList]
          in let
            arg_274 =
              BIF.erlang__apply__2
                [bindexpr_53, ErlangCons pat_267 ErlangEmptyList]
          in let
            arg_278 =
              ErlangTuple [ErlangAtom "list_comp", a_265, y_266, r_269]
          in let
            arg_277 =
              BIF.erlang__apply__2
                [expr_45, ErlangCons arg_278 ErlangEmptyList]
          in let
            arg_273 =
              BIF.erlang__apply__2
                [scoped_2,
                 ErlangCons arg_274 (ErlangCons arg_277 ErlangEmptyList)]
          in
            BIF.erlang__apply__2
              [plus_1, ErlangCons arg_270 (ErlangCons arg_273 ErlangEmptyList)]
        (ErlangTuple [(ErlangAtom "list_comp"), a_286, y_287,
                      (ErlangCons d_289@(ErlangTuple [(ErlangAtom "letfun"), _,
                                                      f_288, _, _,
                                                      _]) r_290)]) ->
          let   
            arg_291 =
              BIF.erlang__apply__2 [decl_29, ErlangCons d_289 ErlangEmptyList]
          in let
            arg_295 =
              BIF.erlang__apply__2
                [bindexpr_53, ErlangCons f_288 ErlangEmptyList]
          in let
            arg_299 =
              ErlangTuple [ErlangAtom "list_comp", a_286, y_287, r_290]
          in let
            arg_298 =
              BIF.erlang__apply__2
                [expr_45, ErlangCons arg_299 ErlangEmptyList]
          in let
            arg_294 =
              BIF.erlang__apply__2
                [scoped_2,
                 ErlangCons arg_295 (ErlangCons arg_298 ErlangEmptyList)]
          in
            BIF.erlang__apply__2
              [plus_1, ErlangCons arg_291 (ErlangCons arg_294 ErlangEmptyList)]
        (ErlangTuple [(ErlangAtom "typed"), _, e_307, t_308]) ->
          let   
            arg_309 =
              BIF.erlang__apply__2 [expr_45, ErlangCons e_307 ErlangEmptyList]
          in let
            arg_312 =
              BIF.erlang__apply__2 [type_37, ErlangCons t_308 ErlangEmptyList]
          in
            BIF.erlang__apply__2
              [plus_1, ErlangCons arg_309 (ErlangCons arg_312 ErlangEmptyList)]
        (ErlangTuple [(ErlangAtom "record"), _, fs_316]) ->
          BIF.erlang__apply__2 [expr_45, ErlangCons fs_316 ErlangEmptyList]
        (ErlangTuple [(ErlangAtom "record"), _, e_319, fs_320]) ->
          BIF.erlang__apply__2
            [expr_45, ErlangCons (ErlangCons e_319 fs_320) ErlangEmptyList]
        (ErlangTuple [(ErlangAtom "map"), _, e_325, fs_326]) ->
          BIF.erlang__apply__2
            [expr_45, ErlangCons (ErlangCons e_325 fs_326) ErlangEmptyList]
        (ErlangTuple [(ErlangAtom "map"), _, kvs_331]) ->
          let
            arg_332 =
              flmap
                (\ lc_336 ->
                   case lc_336 of
                     (ErlangTuple [key_334, val_335]) ->
                       let
                         lcRet_337 =
                           BIF.erlang__apply__2
                             [expr_45,
                              ErlangCons
                                (ErlangCons key_334
                                   (ErlangCons val_335 ErlangEmptyList))
                                ErlangEmptyList]
                       in ErlangCons lcRet_337 ErlangEmptyList
                     _ -> ErlangEmptyList)
                kvs_331
          in
            BIF.erlang__apply__2 [sum_13, ErlangCons arg_332 ErlangEmptyList]
        (ErlangTuple [(ErlangAtom "map_get"), _, a_345, b_346]) ->
          BIF.erlang__apply__2
            [expr_45,
             ErlangCons (ErlangCons a_345 (ErlangCons b_346 ErlangEmptyList))
               ErlangEmptyList]
        (ErlangTuple [(ErlangAtom "map_get"), _, a_353, b_354, c_355]) ->
          BIF.erlang__apply__2
            [expr_45,
             ErlangCons
               (ErlangCons a_353
                  (ErlangCons b_354 (ErlangCons c_355 ErlangEmptyList)))
               ErlangEmptyList]
        (ErlangTuple [(ErlangAtom "block"), _, ss_364]) ->
          BIF.erlang__apply__2 [expr_45, ErlangCons ss_364 ErlangEmptyList]
        (ErlangTuple [(ErlangAtom "field"), _, lv_367, e_368]) ->
          BIF.erlang__apply__2
            [expr_45,
             ErlangCons (ErlangCons lv_367 (ErlangCons e_368 ErlangEmptyList))
               ErlangEmptyList]
        (ErlangTuple [(ErlangAtom "field"), _, lv_375, _, e_376]) ->
          BIF.erlang__apply__2
            [expr_45,
             ErlangCons (ErlangCons lv_375 (ErlangCons e_376 ErlangEmptyList))
               ErlangEmptyList]
        (ErlangTuple [(ErlangAtom "arg"), _, y_383, t_384]) ->
          let   
            arg_385 =
              BIF.erlang__apply__2
                [bindexpr_53, ErlangCons y_383 ErlangEmptyList]
          in let
            arg_388 =
              BIF.erlang__apply__2 [type_37, ErlangCons t_384 ErlangEmptyList]
          in
            BIF.erlang__apply__2
              [plus_1, ErlangCons arg_385 (ErlangCons arg_388 ErlangEmptyList)]
        (ErlangTuple [(ErlangAtom "case"), _, p_392, e_393]) ->
          let   
            arg_394 =
              BIF.erlang__apply__2
                [bindexpr_53, ErlangCons p_392 ErlangEmptyList]
          in let
            arg_397 =
              BIF.erlang__apply__2 [expr_45, ErlangCons e_393 ErlangEmptyList]
          in
            BIF.erlang__apply__2
              [scoped_2,
               ErlangCons arg_394 (ErlangCons arg_397 ErlangEmptyList)]
        (ErlangTuple [(ErlangAtom "proj"), _, _]) -> zero_0
        (ErlangTuple [(ErlangAtom "map_get"), _, e_401]) ->
          BIF.erlang__apply__2 [expr_45, ErlangCons e_401 ErlangEmptyList]
        (ErlangTuple [(ErlangAtom "named_arg"), _, _, e_404]) ->
          BIF.erlang__apply__2 [expr_45, ErlangCons e_404 ErlangEmptyList]
        _ ->
          case alg_3 of
            (ErlangTuple arr_409) | (DM.Just field_408) <-
                                      (arr_409 DA.!! 1) ->
              field_408
            _ -> EXC.badrecord (ErlangAtom "alg")
  in let
    fun_413 =
      case alg_3 of
        (ErlangTuple arr_416) | (DM.Just field_415) <-
                                  (arr_416 DA.!! 2) ->
          field_415
        _ -> EXC.badrecord (ErlangAtom "alg")
  in
    BIF.erlang__apply__2
      [fun_413, ErlangCons top_65 (ErlangCons rec_410 ErlangEmptyList)]
erlps__fold__4 [arg_417, arg_418, arg_419, arg_420] =
  EXC.function_clause unit
erlps__fold__4 args =
  EXC.badarity (ErlangFun 4 erlps__fold__4) args

erlps__used_ids__1 :: ErlangFun
erlps__used_ids__1 [e_0] =
  let lcSrc_1 = erlps__used__1 [e_0]
  in
    flmap
      (\ lc_4 ->
         case lc_4 of
           (ErlangTuple [(ErlangTuple [(ErlangAtom "term"),
                                       (ErlangCons x_3 (ErlangEmptyList))]),
                         _]) ->
             ErlangCons x_3 ErlangEmptyList
           _ -> ErlangEmptyList)
      lcSrc_1
erlps__used_ids__1 [arg_6] = EXC.function_clause unit
erlps__used_ids__1 args =
  EXC.badarity (ErlangFun 1 erlps__used_ids__1) args

erlps__used_types__2 :: ErlangFun
erlps__used_types__2 [_currentns_1@(ErlangCons top_0 (ErlangEmptyList)),
                      t_2]
  =
  let   
    f_12 =
      ErlangFun 1
        (let
           lambda_3 [(ErlangTuple [(ErlangTuple [(ErlangAtom "type"),
                                                 (ErlangCons x_5 (ErlangEmptyList))]),
                                   _])]
             =
             ErlangCons x_5 ErlangEmptyList
           lambda_3 [(ErlangTuple [(ErlangTuple [(ErlangAtom "type"),
                                                 (ErlangCons top1_8 (ErlangCons x_9 (ErlangEmptyList)))]),
                                   _])]
             | weakEq top1_8 top_0 =
             ErlangCons x_9 ErlangEmptyList
           lambda_3 [_] = ErlangEmptyList
           lambda_3 [arg_4] = EXC.function_clause unit
           lambda_3 args = EXC.badarity (ErlangFun 1 lambda_3) args
         in lambda_3)
  in let arg_14 = erlps__used__1 [t_2]
  in
    BIF.do_remote_fun_call "Lists" "erlps__flatmap__2" [f_12, arg_14]
erlps__used_types__2 [arg_16, arg_17] = EXC.function_clause unit
erlps__used_types__2 args =
  EXC.badarity (ErlangFun 2 erlps__used_types__2) args

erlps__entity_alg__0 :: ErlangFun
erlps__entity_alg__0 [] =
  let   
    isbound_9 =
      ErlangFun 1
        (let
           lambda_0 [(ErlangTuple [k_2, _])] =
             BIF.lists__member__2
               [k_2,
                ErlangCons (ErlangAtom "bound_term")
                  (ErlangCons (ErlangAtom "bound_type") ErlangEmptyList)]
           lambda_0 [arg_1] = EXC.function_clause unit
           lambda_0 args = EXC.badarity (ErlangFun 1 lambda_0) args
         in lambda_0)
  in let
    unbind_12 =
      ErlangFun 1
        (let
           lambda_10 [(ErlangAtom "bound_term")] = ErlangAtom "term"
           lambda_10 [(ErlangAtom "bound_type")] = ErlangAtom "type"
           lambda_10 [arg_11] = EXC.function_clause unit
           lambda_10 args = EXC.badarity (ErlangFun 1 lambda_10) args
         in lambda_10)
  in let
    remove_20 =
      ErlangFun 2
        (let
           lambda_13 [keys_16, map_17] =
             BIF.do_remote_fun_call "Maps" "erlps__without__2"
               [keys_16, map_17]
           lambda_13 [arg_14, arg_15] = EXC.function_clause unit
           lambda_13 args = EXC.badarity (ErlangFun 2 lambda_13) args
         in lambda_13)
  in let
    scoped_54 =
      ErlangFun 2
        (let
           lambda_21 [xs_24, ys_25] =
             let    lcSrc_26 = BIF.maps__keys__1 [xs_24]
             in let
               bound_34 =
                 flmap
                   (\ lc_29 ->
                      let
                        cond_30 =
                          BIF.erlang__apply__2
                            [isbound_9, ErlangCons lc_29 ErlangEmptyList]
                      in
                        case cond_30 of
                          (ErlangAtom "true") ->
                            ErlangCons lc_29 ErlangEmptyList
                          _ -> ErlangEmptyList)
                   lcSrc_26
             in let
               bound1_44 =
                 flmap
                   (\ lc_38 ->
                      case lc_38 of
                        (ErlangTuple [tag_36, x_37]) ->
                          let   
                            tup_el_40 =
                              BIF.erlang__apply__2
                                [unbind_12, ErlangCons tag_36 ErlangEmptyList]
                          in let lcRet_39 = ErlangTuple [tup_el_40, x_37]
                          in ErlangCons lcRet_39 ErlangEmptyList
                        _ -> ErlangEmptyList)
                   bound_34
             in let
               others_48 =
                 BIF.erlang__apply__2
                   [remove_20,
                    ErlangCons bound1_44 (ErlangCons ys_25 ErlangEmptyList)]
             in let
               arg_49 =
                 BIF.erlang__apply__2
                   [remove_20,
                    ErlangCons bound_34 (ErlangCons xs_24 ErlangEmptyList)]
             in BIF.maps__merge__2 [arg_49, others_48]
           lambda_21 [arg_22, arg_23] = EXC.function_clause unit
           lambda_21 args = EXC.badarity (ErlangFun 2 lambda_21) args
         in lambda_21)
  in let tup_el_56 = ErlangMap Map.empty
  in let arg_60 = toErl 2
  in let
    tup_el_57 =
      BIF.erlang__make_fun__3
        [ErlangAtom "maps", ErlangAtom "merge", arg_60]
  in
    ErlangTuple [ErlangAtom "alg", tup_el_56, tup_el_57, scoped_54]
erlps__entity_alg__0 args =
  EXC.badarity (ErlangFun 0 erlps__entity_alg__0) args

erlps__used__1 :: ErlangFun
erlps__used__1 [d_0] =
  let   
    kind_3 =
      ErlangFun 1
        (let
           lambda_1 [(ErlangAtom "expr")] = ErlangAtom "term"
           lambda_1 [(ErlangAtom "bind_expr")] = ErlangAtom "bound_term"
           lambda_1 [(ErlangAtom "type")] = ErlangAtom "type"
           lambda_1 [(ErlangAtom "bind_type")] = ErlangAtom "bound_type"
           lambda_1 [arg_2] = EXC.function_clause unit
           lambda_1 args = EXC.badarity (ErlangFun 1 lambda_1) args
         in lambda_1)
  in let
    ns_10 =
      ErlangFun 1
        (let
           lambda_4 [xs_6] =
             let
               tup_el_8 =
                 BIF.do_remote_fun_call "Lists" "erlps__droplast__1" [xs_6]
             in ErlangTuple [ErlangAtom "namespace", tup_el_8]
           lambda_4 [arg_5] = EXC.function_clause unit
           lambda_4 args = EXC.badarity (ErlangFun 1 lambda_4) args
         in lambda_4)
  in let
    notbound_21 =
      ErlangFun 1
        (let
           lambda_11 [(ErlangTuple [(ErlangTuple [tag_13, _]), _])] =
             let
               op_arg_14 =
                 BIF.lists__member__2
                   [tag_13,
                    ErlangCons (ErlangAtom "bound_term")
                      (ErlangCons (ErlangAtom "bound_type") ErlangEmptyList)]
             in BIF.erlang__not__1 [op_arg_14]
           lambda_11 [arg_12] = EXC.function_clause unit
           lambda_11 args = EXC.badarity (ErlangFun 1 lambda_11) args
         in lambda_11)
  in let arg_23 = erlps__entity_alg__0 []
  in let
    arg_24 =
      ErlangFun 2
        (let
           lambda_25 [k_28, (ErlangTuple [(ErlangAtom "id"), ann_29, x_30])]
             =
             let   
               tup_el_32 =
                 BIF.erlang__apply__2 [kind_3, ErlangCons k_28 ErlangEmptyList]
             in let
               key_31 = ErlangTuple [tup_el_32, ErlangCons x_30 ErlangEmptyList]
             in ErlangMap (Map.singleton key_31 ann_29)
           lambda_25 [k_39,
                      (ErlangTuple [(ErlangAtom "qid"), ann_40, xs_41])]
             =
             let   
               tup_el_43 =
                 BIF.erlang__apply__2 [kind_3, ErlangCons k_39 ErlangEmptyList]
             in let key_42 = ErlangTuple [tup_el_43, xs_41]
             in let
               key_47 =
                 BIF.erlang__apply__2 [ns_10, ErlangCons xs_41 ErlangEmptyList]
             in
               ErlangMap
                 (Map.fromFoldable
                    [DT.Tuple key_42 ann_40, DT.Tuple key_47 ann_40])
           lambda_25 [k_52,
                      (ErlangTuple [(ErlangAtom "con"), ann_53, x_54])]
             =
             let   
               tup_el_56 =
                 BIF.erlang__apply__2 [kind_3, ErlangCons k_52 ErlangEmptyList]
             in let
               key_55 = ErlangTuple [tup_el_56, ErlangCons x_54 ErlangEmptyList]
             in ErlangMap (Map.singleton key_55 ann_53)
           lambda_25 [k_63,
                      (ErlangTuple [(ErlangAtom "qcon"), ann_64, xs_65])]
             =
             let   
               tup_el_67 =
                 BIF.erlang__apply__2 [kind_3, ErlangCons k_63 ErlangEmptyList]
             in let key_66 = ErlangTuple [tup_el_67, xs_65]
             in let
               key_71 =
                 BIF.erlang__apply__2 [ns_10, ErlangCons xs_65 ErlangEmptyList]
             in
               ErlangMap
                 (Map.fromFoldable
                    [DT.Tuple key_66 ann_64, DT.Tuple key_71 ann_64])
           lambda_25 [_, _] = ErlangMap Map.empty
           lambda_25 [arg_26, arg_27] = EXC.function_clause unit
           lambda_25 args = EXC.badarity (ErlangFun 2 lambda_25) args
         in lambda_25)
  in let
    arg_22 = erlps__fold__4 [arg_23, arg_24, ErlangAtom "decl", d_0]
  in let xs_78 = BIF.maps__to_list__1 [arg_22]
  in
    BIF.do_remote_fun_call "Lists" "erlps__filter__2"
      [notbound_21, xs_78]
erlps__used__1 [arg_81] = EXC.function_clause unit
erlps__used__1 args =
  EXC.badarity (ErlangFun 1 erlps__used__1) args