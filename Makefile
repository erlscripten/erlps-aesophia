ERLANG_PROJECT=aesophia

.PHONY: transpile transpile_test clean libs test sed fs

transpile: $(ERLANG_PROJECT)/_build
	./erlscripten -p $(ERLANG_PROJECT) -o . --skip-tests --omit aesophia/_build/default/lib/enacl --omit aesophia/_build/default/lib/eblake2
	spago build --purs-args "+RTS -I5 -w -A128M --"

transpile_test: $(ERLANG_PROJECT)
	./erlscripten -p $(ERLANG_PROJECT) -o . --omit $(ERLANG_PROJECT)/_build/default/lib/enacl --omit $(ERLANG_PROJECT)/_build/default/lib/aeserialization --omit $(ERLANG_PROJECT)/_build/default/lib/aebytecode --omit $(ERLANG_PROJECT)/_build/default/lib/eblake2 --omit $(ERLANG_PROJECT)/_build/default/lib/base58 --omit $(ERLANG_PROJECT)/_build/test/lib/aesophia/test/aeso_abi_tests.beam --omit $(ERLANG_PROJECT)/_build/default/lib/getopt/

bundle:
	purs bundle "output/*/*.js" -m Aeb.Aevm.Abi -m Aeb.Aevm.Data -m Aeb.Asm -m Aeb.Asm.Scan -m Aeb.Disassemble -m Aeb.Fate.Abi -m Aeb.Fate.Asm -m Aeb.Fate.Asm.Scan -m Aeb.Fate.Code -m Aeb.Fate.Data -m Aeb.Fate.Encoding -m Aeb.Fate.Maps -m Aeb.Fate.Opcodes -m Aeb.Fate.Ops -m Aeb.Fate.Pp -m Aeb.Heap -m Aeb.Memory -m Aeb.Opcodes -m Aeb.Primops -m Aefateasm -m Aeser.Api.Encoder -m Aeser.Chain.Objects -m Aeser.Contract.Code -m Aeserialization -m Aeser.Id -m Aeser.Rlp -m Aeso.Aci -m Aeso.Ast -m Aeso.Ast.Infer.Types -m Aeso.Ast.To.Fcode -m Aeso.Ast.To.Icode -m Aeso.Builtins -m Aeso.Code.Errors -m Aeso.Compiler -m Aeso.Errors -m Aeso.Fcode.To.Fate -m Aeso.Icode -m Aeso.Icode.To.Asm -m Aeso.Parse.Lib -m Aeso.Parser -m Aeso.Pretty -m Aeso.Scan -m Aeso.Stdlib -m Aeso.Syntax -m Aeso.Syntax.Utils -m Aeso.Utils -m Aeso.Vm.Decode -m Array -m Base58 -m Base64 -m Crypto -m Dict -m Eblake2 -m Enacl -m Epp -m Erlang.Binary -m Erlang.Builtins -m Erlang.Exception -m Erlang.Helpers -m Erlang.Invoke -m Erlang.Io -m Erlang.Ioserver -m Erlang.Type -m Erlang.Unicode -m Erlang.Utils -m Erl.Anno -m Erl.Internal -m Erl.Lint -m Erl.Parse -m Erl.Posix.Msg -m Erl.Pp -m Erl.Scan -m Ets -m File -m Filename -m Gb.Sets -m Gb.Trees -m Getopt -m Io.Lib -m Io.Lib.Format -m Io.Lib.Pretty -m Jsx -m Jsx.Config -m Jsx.Consult -m Jsx.Decoder -m Jsx.Encoder -m Jsx.Parser -m Jsx.To.Json -m Jsx.To.Term -m Jsx.Verify -m Lists -m Maps -m Math -m Orddict -m Ordsets -m Os -m Otp.Internal -m Prettypr -m Proplists -m Queue -m Sets -m String -m Unicode -m Unicode.Util -o dist/index.js
	echo -e "\n\nmodule.exports = PS" >> dist/index.js
	runhaskell gen_filesystem.hs aesophia/priv/stdlib > output/File/filemap.json
	NODE_OPTIONS=--max-old-space-size=4096 rollup -c

fs:
	mkdir -p output/File/node_modules
	runhaskell gen_filesystem.hs $(ERLANG_PROJECT)/test/contracts/{,code_errors} $(ERLANG_PROJECT)/priv/stdlib > output/File/filemap.json

sed:
	sed -n -f sed/lib_dir.sed test/AesoTestUtils.purs > /tmp/aesops
	mv /tmp/aesops test/AesoTestUtils.purs # for some reason -i doesn't work
	sed -n -f sed/version.sed src/AesoCompiler.purs > /tmp/aesops
	mv /tmp/aesops src/AesoCompiler.purs # for some reason -i doesn't work
	sed -n -f sed/priv_dir.sed src/AesoStdlib.purs > /tmp/aesops
	mv /tmp/aesops src/AesoStdlib.purs # for some reason -i doesn't work
	sed -n -f sed/is_dir.sed src/AesoParser.purs > /tmp/aesops
	mv /tmp/aesops src/AesoParser.purs # for some reason -i doesn't work

spago_test:
	spago test --purs-args "+RTS -I5 -w -A128M --"

spago_build:
	spago build --purs-args "+RTS -I5 -w -A128M --"

test: transpile_test libs sed spago_build fs spago_test


$(ERLANG_PROJECT)/_build:
	cd $(ERLANG_PROJECT); ./rebar3 compile; ./rebar3 eunit

libs:
	ln mocks/* src/ -f

clean:
	rm src/* -f
	rm test/Aeso* -f
	rm -rf aesophia/_build

nuke: clean
	rm output -rf
	rm .spago -rf
