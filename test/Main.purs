module Test.Main where

import Aeso.Aci.Tests
import Aeso.Calldata.Tests
import Aeso.Compiler.Tests
import Aeso.Parser.Tests
import Aeso.Scan.Tests
import Data.Either
import Data.Lazy
import Data.Time.Duration
import Data.Traversable
import Effect.Aff hiding (error)
import Effect.Unsafe
import Erlang.Exception
import Erlang.Invoke
import Erlang.Type
import Partial.Unsafe
import Prelude
import Unsafe.Coerce

import Data.Array as A
import Data.BigInt as DBI
import Data.Maybe as M
import Data.String as Str
import Data.String.CodePoints as StrCP
import Data.Time.Duration (Milliseconds(..))
import Data.Tuple as T
import Effect (Effect)
import Effect.Aff.AVar as AVar
import Effect.Class (liftEffect)
import Effect.Console (log)
import Effect.Exception (catchException)
import Effect.Ref as Ref
import Erlang.Binary as BIN
import Erlang.Builtins as BIF
import Erlang.Helpers as H
import Test.Spec (Spec, describe, it, pending)
import Test.Spec.Assertions (shouldEqual, expectError)
import Test.Spec.Reporter.Console (consoleReporter)
import Test.Spec.Runner (runSpec)

-- BEWARE BE DRAGONS - I've lost too many hours debugging alternative helpers
-- If you think you can make a better wrapper which does not crash the testing infrastructure then please make a PR
-- If you can replace this helper with something better then please feel free to do so :)
exec_may_throw_aff :: ErlangFun -> Array ErlangTerm -> Aff ErlangTerm
exec_may_throw_aff fun args =
    let
      t = defer $ (\_ -> run_erlang fun args)
      f = defer $ (\_ -> ErlangAtom "error")
    in do
      v <- liftEffect (catchException (\_ -> pure f
                                      ) (pure t))
      pure $ force v

wololo_term :: Error -> ErlangTerm
wololo_term res = unsafeCoerce res

wololo_codepoint :: Partial => ErlangTerm -> StrCP.CodePoint
wololo_codepoint (ErlangInt res) = unsafeCoerce res

print_err (Right r) = show r
print_err (Left e) =
    case show e of
        "[object Object]" ->
            case (wololo_term e) of
                ErlangTuple [a,b,stack] ->
                    let
                        m1 = show a
                        m2 = show b
                        m3 = show stack
                    in
                        "[" <> m1 <> ", " <> m2 <> ", " <> m3 <> "]"
                r ->
                    show r
        r ->
            r

exec_may_throw :: ErlangFun -> Array ErlangTerm -> Aff ErlangTerm
exec_may_throw fun args = do
    res <- attempt $ exec_may_throw_aff fun args
    liftEffect $ log $ print_err res -- Uncomment for logs :)
    case res of
        Left _ -> pure make_err
        Right r -> pure $ make_ok r

lift_aff_to_erlang_process :: forall a. (Unit -> Aff a) -> Aff (T.Tuple ErlangTerm a)
lift_aff_to_erlang_process calc = do
        -- ONLY TOUCH THIS IF YOU KNOW WHAT YOU ARE DOING!!!!!
        -- THIS IS A DIRTY HACK TO "lift" an calculation in the Aff monad to an ErlangProcess from the GLOBAL scope
        res_channel <- AVar.empty
        pid_channel <- AVar.empty
        _ <- forkAff do
            packed_pid <- exec_may_throw BIF.erlang__spawn__1 [(
                ErlangFun 0 (\ _ -> let -- TODO: Fixme - the calculation should yield to the scheduler and only then we may launch the avar. We need a jump to FFI here :(
                    a = unsafePerformEffect $ launchAff_ (
                        do
                            res <- calc unit
                            AVar.put res res_channel
                        )
                    in
                       ErlangInt (DBI.fromInt 1)))]
            -- At this point we never yielded so the process MUST be alive
            pid <- unpack_ok packed_pid
            AVar.put pid pid_channel

        pid <- AVar.take pid_channel
        packed_is_alive <- exec_may_throw BIF.erlang__is_process_alive__1 [pid]
        (ErlangAtom "true") `shouldEqualOk` packed_is_alive

        res <- AVar.take res_channel

        delay (Milliseconds 1.0) -- force a context switch to cleanup the process :P
        packed_is_alive <- exec_may_throw BIF.erlang__is_process_alive__1 [pid]
        (ErlangAtom "false") `shouldEqualOk` packed_is_alive
        pure $ T.Tuple pid res

ok = ErlangAtom "ok"

unpack_ok :: ErlangTerm -> Aff ErlangTerm
unpack_ok (ErlangTuple [ok, term]) = pure term
unpack_ok e = do
    ErlangEmptyList `shouldEqual` e
    pure ErlangEmptyList

assert_ok :: ErlangTerm -> Aff Unit
assert_ok (ErlangTuple [ok, _]) = pure unit
assert_ok e = do
    ErlangEmptyList `shouldEqual` e
    pure unit

make_ok term = ErlangTuple [ok, term]
make_err = ErlangAtom "error"
mkInt :: Int -> ErlangTerm
mkInt = DBI.fromInt >>> ErlangInt

shouldEqualOk a b = make_ok a `shouldEqual` b

run_eunit :: Partial => ErlangTerm -> Spec Unit
run_eunit ErlangEmptyList = pure unit
run_eunit (ErlangCons (ErlangTuple [ename, ErlangFun 0 testfun]) rest)
  | M.Just name <- H.erlangListToString ename = do
    it name do
      r <- exec_may_throw testfun []
      ok `shouldEqualOk` r
    run_eunit rest
run_eunit (ErlangCons _ rest) = run_eunit rest

main :: Effect Unit
main = unsafePartial $
    launchAff_ $ runSpec [consoleReporter] do
      -- describe "AesoAciTests" do
      --   describe "simple aci test" do
      --     run_eunit (erlps__simple_aci_test___0 [])
      --   describe "aci test" do
      --     run_eunit (erlps__aci_test___0 [])
      -- describe "AesoCalldataTests" do
      --   describe "calldata test" do
      --     run_eunit (erlps__calldata_test___0 [])
      --   describe "calldata aci test" do
      --     run_eunit (erlps__calldata_aci_test___0 [])
      describe "AesoCompilerTests" do
        describe "simlpe compile test" do
          run_eunit (erlps__simple_compile_test___0 [])
        describe "validation test" do
          run_eunit (erlps__validation_test___0 [])
      -- describe "AesoParserTests" do
      --   describe "simple contracts test" do
      --     run_eunit (erlps__simple_contracts_test___0 [])
      -- describe "AesoScanTests" do
      --   describe "empty contract test" do
      --     run_eunit (erlps__empty_contract_test___0 [])
      --   describe "all tokens test" do
      --     run_eunit (erlps__all_tokens_test___0 [])
