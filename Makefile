ERLANG_PROJECT=aesophia

.PHONY: $(ERLANG_PROJECT) transpile transpile_test clean libs test sed fs

transpile: $(ERLANG_PROJECT)
	./erlscripten -p $(ERLANG_PROJECT) -o . --skip-tests --omit aesophia/_build/default/lib/enacl --omit aesophia/_build/default/lib/eblake2 --omit 
	spago build --purs-args "+RTS -I5 -w -A128M --"

transpile_test: $(ERLANG_PROJECT)
	./erlscripten -p $(ERLANG_PROJECT) -o . --omit $(ERLANG_PROJECT)/_build/default/lib/enacl --omit $(ERLANG_PROJECT)/_build/default/lib/aeserialization --omit $(ERLANG_PROJECT)/_build/default/lib/aebytecode --omit $(ERLANG_PROJECT)/_build/default/lib/eblake2 --omit $(ERLANG_PROJECT)/_build/default/lib/base58 --omit $(ERLANG_PROJECT)/_build/test/lib/aesophia/test/aeso_abi_tests.beam --omit $(ERLANG_PROJECT)/_build/default/lib/getopt/

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
