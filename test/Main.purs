module Test.Main where

import Aeso.Compiler (erlps__from_string__2)
import Aeso.Aci.Tests (erlps__aci_test___0, erlps__simple_aci_test___0)
import Aeso.Calldata.Tests (erlps__calldata_aci_test___0, erlps__calldata_test___0)
import Aeso.Compiler.Tests (erlps__simple_compile_test___0, erlps__validation_test___0)
import Aeso.Parser.Tests (erlps__simple_contracts_test___0)
import Aeso.Scan.Tests (erlps__all_tokens_test___0, erlps__empty_contract_test___0)
import Effect.Aff (launchAff_)
import Erlang.Type (ErlangTerm(..), fromErl, toErl)
import Partial.Unsafe (unsafePartial)
import Prelude (Unit, bind, discard, pure, unit, ($))

import Data.Map as Map
import Data.Maybe as M
import Effect (Effect)
import Erlang.Binary as BIN
import Erlang.TestUtil (exec, ok, testExecOk)
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual)
import Test.Spec.Reporter.Console (consoleReporter)
import Test.Spec.Runner (runSpec)


run_eunit :: Partial => ErlangTerm -> Spec Unit
run_eunit ErlangEmptyList = pure unit
run_eunit (ErlangTuple [ErlangAtom "foreach", before, after, tests]) =
  run_eunit tests
run_eunit (ErlangCons (ErlangTuple [ename, ErlangFun 0 testfun]) rest)
  | M.Just name <- fromErl ename = do
    it name do
      testExecOk ok testfun []
    run_eunit rest
run_eunit (ErlangCons _ rest) = run_eunit rest
run_eunit wtf = it "wtf:" (shouldEqual wtf ErlangEmptyList)

main :: Effect Unit
main = unsafePartial $
    launchAff_ $ runSpec [consoleReporter] do
      describe "Compare some hardcoded examples" do
        it "Constant returner" do
          let contract =
                "contract Const =\n  entrypoint f() = 123\n"
          ErlangTuple
            [ ErlangAtom "ok"
            , ErlangTuple
              [ErlangAtom "ok", ErlangMap m]] <- exec erlps__from_string__2
               [ toErl contract
               , ErlangCons
                 (ErlangTuple [ErlangAtom "backend", ErlangAtom "fate"])
                 ErlangEmptyList
               ]
          let M.Just (ErlangBinary bytecode) = Map.lookup (ErlangAtom "byte_code") m
          [158,254,68,214,68,31,0,55,0,55,0,26,14,130,63,1,3,63,
           254,224,190,252,97,0,55,0,7,1,3,111,59,147,47,2,17,68,
           214,68,31,17,105,110,105,116,17,224,190,252,97,5,102,
           130,47,0] `shouldEqual` BIN.toArray bytecode

      describe "AesoAciTests" do
         describe "simple aci test" do
           run_eunit (erlps__simple_aci_test___0 [])
         describe "aci test" do
           run_eunit (erlps__aci_test___0 [])
      describe "AesoCalldataTests" do
         describe "calldata test" do
           run_eunit (erlps__calldata_test___0 [])
         describe "calldata aci test" do
           run_eunit (erlps__calldata_aci_test___0 [])
      describe "AesoCompilerTests" do
        describe "simlpe compile test" do
          run_eunit (erlps__simple_compile_test___0 [])
        describe "validation test" do
          run_eunit (erlps__validation_test___0 [])
      describe "AesoParserTests" do
        describe "simple contracts test" do
          run_eunit (erlps__simple_contracts_test___0 [])
      describe "AesoScanTests" do
        describe "empty contract test" do
          run_eunit (erlps__empty_contract_test___0 [])
        describe "all tokens test" do
          run_eunit (erlps__all_tokens_test___0 [])

