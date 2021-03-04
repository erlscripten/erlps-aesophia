var fs = require('fs');

require.extensions['.aes'] = function (module, filename) {
    module.exports = fs.readFileSync(filename, 'utf8');
};

const expect = require('chai').expect;
const PS = require("./dist/bundle.js");
const CSmall = 'contract Foo =\n    entrypoint init() = ()';
const CLarge = require("./staking_contract.aes");

const showTerm = PS['Data.Show'].show(PS['Erlang.Type'].showErlangTerm)
const erlBinToErlStr = (t) => PS['Erlang.Builtins'].erlang__binary_to_list__1([t])
const erlStrToErlBin = (t) => PS['Erlang.Builtins'].erlang__iolist_to_binary__1([t])

const erlStrToJSStr = (t) => PS['Erlang.Type'].fromErl(PS['Erlang.Type'].stringFromErlang)(t).value0
const erlBinToJSStr = (t) => erlStrToJSStr(erlBinToErlStr(t))

const jsStrToErlStr = PS['Erlang.Type'].toErl(PS['Erlang.Type'].stringToErlang)
const jsStrToErlBin = (t) => erlStrToErlBin(jsStrToErlStr(t))

const mkTuple = PS['Erlang.Type'].ErlangTuple.create
const mkAtom = PS['Erlang.Type'].ErlangAtom.create
const mkNil = PS['Erlang.Type'].ErlangEmptyList.value
const mkCons = (a, b) => PS['Erlang.Type'].ErlangCons.create(a)(b)


describe('Type conversions', () => {
 it('JS STR -> ERL STR -> JS STR', () => {
        let a = 'hello world!';
        expect(a).to.equal(erlStrToJSStr(jsStrToErlStr(a)));
    });
 it('JS STR -> ERl BIN -> JS STR', () => {
        let a = 'hello world!';
        expect(a).to.equal(erlBinToJSStr(jsStrToErlBin(a)));
    });
});

function compileTest(src, opts, e) {
    const compiled = PS['Aeso.Compiler'].erlps__from_string__2([jsStrToErlStr(src), opts])
    if (compiled instanceof PS['Erlang.Type'].ErlangTuple && compiled.value0[0].value0 == "ok")
    {
        const compiled1 = PS['Aeser.Contract.Code'].erlps__serialize__1([compiled.value0[1]])
        const bytecode = PS['Aeser.Api.Encoder'].erlps__encode__2([PS['Erlang.Type'].ErlangAtom.create('contract_bytearray'), compiled1])
        expect(erlBinToJSStr(bytecode)).to.equal(e)
    } else {
        console.log(showTerm(compiled))
        console.log("Compilation of contract failed!")
        expect(1).to.equal(2)
    }
}

describe('Compiles', () => {
 it('Small contract on AEVM', () => {
        compileTest(CSmall, mkNil, 'cb_+QKNRgOgIsE3h90pkyczQKCXfbRy5G8LTfhuqH8oKz1KZ9bE3GX5Ac/5AcyguclW8osxSan1mHqlBfPaGyIJzFc5I0AGK7bBvZ+fmeqEaW5pdAC4YAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAgAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAP//////////////////////////////////////////7kBQAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAgAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAMAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAYAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAACgAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAMAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAABQAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAEA//////////////////////////////////////////8AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA///////////////////////////////////////////uI9iAAA5YgAAWZGAgFF/uclW8osxSan1mHqlBfPaGyIJzFc5I0AGK7bBvZ+fmeoUYgAAhFdQYAEZUQBbYAAZWWAgAZCBUmAgkANgA4FSkFlgAFFZUmAAUmAA81tgAIBSYADzW1lZYCABkIFSYCCQA2AAGVlgIAGQgVJgIJADYAOBUoFSkFZbUIKRUFBiAABhVoU0LjMuMADpOQ0f')
    });
 it('Small contract on FATE', () => {
        compileTest(CSmall, mkCons(mkTuple([mkAtom("backend"), mkAtom("fate")]), mkNil), 'cb_+E5GA6AiwTeH3SmTJzNAoJd9tHLkbwtN+G6ofygrPUpn1sTcZcCikf5E1kQfADcANwAaDoI/AQM/jC8BEUTWRB8RaW5pdIIvAIU0LjMuMADUibLd')
    });
 it('Large contract on AEVM', () => {
        // TODO: Stack explosion
        //compileTest(CLarge, mkNil, '')
        expect(1).to.equal(2)
    });
 it('Large contract on FATE', () => {
        // TODO: Stack explosion
        // compileTest(CLarge, mkCons(mkTuple([mkAtom("backend"), mkAtom("fate")]), mkNil), '')
        expect(1).to.equal(2)
    });
});

describe('Aci', () => {
    it('Generates ACI on FATE', () => {
        const opts = mkCons(mkTuple([mkAtom("backend"), mkAtom("fate")]), mkNil)
        const r = PS['Aeso.Aci'].erlps__contract_interface__3([mkAtom('json'), jsStrToErlStr(CSmall), opts])
        if (r instanceof PS['Erlang.Type'].ErlangTuple && r.value0[0].value0 == "ok")
        {
            const aci = r.value0[1];
            console.log(JSON.parse(erlBinToJSStr(PS['Jsx'].erlps__encode__1([aci]))))

        } else {
            console.log(showTerm(r))
            console.log("ACI generation for contract failed")
            expect(1).to.equal(2)
        }
    });
});
