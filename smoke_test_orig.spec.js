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
         compileTest(CLarge, mkCons(mkTuple([mkAtom("backend"), mkAtom("fate")]), mkNil), 'cb_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ZUVsZWN0aW9uLnNldF9sZWFkZXIRaHbW31kuTGlzdEludGVybmFsLmZsYXRfbWFwEWsdoz55LlNpbXBsZUVsZWN0aW9uLmdldF9jYW5kaWRhdGVzEW+ykeUVLl45OTERcZ3RyjlkZWZhdWx0X2NvbmZpZxF2qyUXTWdldF9jb21wdXRlZF9sZWFkZXIReKq0wUEuTGlzdC5wYXJ0aXRpb25fEXy4/mQZLl4xMDAzEX7+9ZqhLlNpbXBsZUVsZWN0aW9uLmV4dHJhY3RfcmlwZV93aXRoZHJhd2FscxGHq6ckKWdldF9sZWFkZXIRiZ7s2TVkZXBvc2l0X3N0YWtlEY0kaXM9cmV0cmFjdGVkX3N0YWtlEZkp31J5LlNpbXBsZUVsZWN0aW9uLmRlY3JlYXNlX3N0YWtlEZtRzOh9LlNpbXBsZUVsZWN0aW9uLmNob29zZV9ieV9wb3dlchGgAot4JS5MaXN0Lm1hcBGgnx6AJS5QYWlyLnNuZBGjuFuthS5TaW1wbGVFbGVjdGlvbi5wcm90b2NvbF9yZXN0cmljdBGkutPMFS5eOTkyEaW0RnBdLlNpbXBsZUVsZWN0aW9uLnZhbHVhdGURrfZcOxkuXjEwMDYRsh2pCGkuU2ltcGxlRWxlY3Rpb24uaGFzaF9yYW5nZRHKUt7/FS5eOTk3EePzPAglLlBhaXIuZnN0EfEyrhkZcHVuaXNoEfVvNQZBcmVxdWVzdF93aXRoZHJhdxH13MfeNXN0YWtlZF90b2tlbnMR9j4H+RkuXjEwMDUR9utyzSkuTGlzdC5maW5kEfwIDR0pLkxpc3Quc29ydBH/0jlKFS5eOTk5gi8AhTQuMy4wAfUcbGc=')
        //expect(1).to.equal(2)
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


