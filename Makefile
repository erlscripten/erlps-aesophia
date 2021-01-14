ERLANG_PROJECT=aesophia

.PHONY: $(ERLANG_PROJECT) transpile transpile_test clean libs test sed fs

transpile: $(ERLANG_PROJECT)
	./erlscripten -p $(ERLANG_PROJECT) -o . --skip-tests --omit aesophia/_build/default/lib/enacl --omit aesophia/_build/default/lib/eblake2 --omit 
	spago build --purs-args "+RTS -I5 -w -A128M --"

transpile_test: $(ERLANG_PROJECT)
	./erlscripten -p $(ERLANG_PROJECT) -o . --omit $(ERLANG_PROJECT)/_build/default/lib/enacl --omit $(ERLANG_PROJECT)/_build/default/lib/aeserialization --omit $(ERLANG_PROJECT)/_build/default/lib/aebytecode --omit $(ERLANG_PROJECT)/_build/default/lib/eblake2 --omit $(ERLANG_PROJECT)/_build/default/lib/base58 --omit $(ERLANG_PROJECT)/_build/test/lib/aesophia/test/aeso_abi_tests.beam --omit $(ERLANG_PROJECT)/_build/default/lib/getopt/ -S "10:aeso_ast_to_icode:ast_body" -S "10:aeso_ast_to_icode:is_builtin_fun" -S "10:aeso_ast_to_icode:builtin_code" -S "5:jsx_config:parse_strict" -S "5:jsx_config:reduce_config"   -S "5:jsx_config:parse_config" -S "5:jsx_decoder:value"  -S "5:jsx_decoder:object"  -S "5:jsx_decoder:array"  -S "5:jsx_decoder:colon"  -S "5:jsx_decoder:key" -S "5:jsx_decoder:string" -S "5:jsx_decoder:count"  -S "5:jsx_decoder:unescape" -S "5:jsx_decoder:done" -S "5:jsx_decoder:maybe_done" -S "5:jsx_parser:value" -S "5:jsx_parser:object"  -S "5:jsx_parser:array"  -S "5:jsx_parser:colon"  -S "5:jsx_parser:key" -S "5:jsx_parser:string" -S "5:jsx_parser:count"  -S "5:jsx_parser:unescape" -S "5:jsx_parser:done" -S "5:jsx_parser:maybe_done" -S "5:jsx_parser:clean" -S "5:jsx_parser:maybe_replace"

fs:
	spago build --purs-args "+RTS -I5 -w -A128M --"
	mkdir -p output/File/node_modules
	runhaskell gen_filesystem.hs $(ERLANG_PROJECT)/test/contracts/{,reason,code_errors} > output/File/node_modules/filemap.json

sed:
	sed -n -f sed/lib_dir.sed test/AesoTestUtils.purs > /tmp/aesops
	mv /tmp/aesops test/AesoTestUtils.purs # for some reason -i doesn't work
	sed -n -f sed/version.sed src/AesoCompiler.purs > /tmp/aesops
	mv /tmp/aesops src/AesoCompiler.purs # for some reason -i doesn't work
spago_test:
	spago test --purs-args "+RTS -I5 -w -A128M --"

test: transpile_test fs libs sed spago_test


$(ERLANG_PROJECT):
	cd $(ERLANG_PROJECT); ./rebar3 compile; ./rebar3 eunit

libs:
	ln mocks/* src/ -f

clean:
	rm src/* -f
	rm test/Aeso* -f

nuke: clean
	rm output -rf
	rm .spago -rf
