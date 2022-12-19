const PS = require('../temp/bundle.js');
const { readFile } = require('fs/promises');

/* Pretty printer of elang terms */
const showTerm = PS['Data.Show'].show(PS['Erlang.Type'].showErlangTerm);

/* Erl -> Erl conversions */
const erlBinToErlStr = (t) => PS['Erlang.Builtins'].erlang__binary_to_list__1([t]);
const erlStrToErlBin = (t) => PS['Erlang.Builtins'].erlang__iolist_to_binary__1([t]);
const erlMapToErlList = (t) => PS['Erlang.Builtins'].maps__to_list__1([t]);

/* Erl -> JS casts */
const erlStrToJSStr = (t) => PS['Erlang.Type'].fromErl(PS['Erlang.Type'].stringFromErlang)(t).value0;
const erlBinToJSStr = (t) => erlStrToJSStr(erlBinToErlStr(t));

/* JS -> Erl casts */
const jsStrToErlStr = PS['Erlang.Type'].toErl(PS['Erlang.Type'].stringToErlang);
const jsStrToErlBin = (t) => erlStrToErlBin(jsStrToErlStr(t));

/* Constructors for erlang types */
const erlTuple = PS['Erlang.Type'].ErlangTuple;
const erlCons = PS['Erlang.Type'].ErlangCons;
const erlNil = PS['Erlang.Type'].ErlangEmptyList;
const erlAtom = PS['Erlang.Type'].ErlangAtom;
const erlBin = PS['Erlang.Type'].ErlangBinary;
const erlMap = PS['Erlang.Type'].ErlangMap;
const erlFloat = PS['Erlang.Type'].ErlangFloat;
const erlInt = PS['Erlang.Type'].ErlangInt;
const mkTuple = erlTuple.create;
const mkAtom = PS['Erlang.Type'].ErlangAtom.create;
const mkNil = PS['Erlang.Type'].ErlangEmptyList.value;
const mkCons = (a, b) => PS['Erlang.Type'].ErlangCons.create(a)(b);
const jsArrayToErlList = (t) => PS['Erlang.Builtins'].erlang__tuple_to_list__1([mkTuple(t)]);

/* Convert an erlang JSON to a JS JSON */
const erlJSONToJsJSON = (t) => {
  if (t instanceof erlNil) return [];
  if (t instanceof erlCons) {
    const r = [];
    while (!(t instanceof erlNil)) {
      r.push(erlJSONToJsJSON(t.value0));
      t = t.value1;
    }
    return r;
  }
  if (t instanceof erlMap) {
    t = erlMapToErlList(t);
    const r = {};
    while (!(t instanceof erlNil)) {
      let k = t.value0.value0[0];
      if (k instanceof erlAtom) k = k.value0;
      else if (k instanceof erlBin) k = erlBinToJSStr(k);
      else if (k instanceof erlInt) k = PS['Erlang.Utils'].bigIntToInt(k.value0).value0;
      else {
        console.log(k);
        throw new Error('Invalid Key');
      }
      const v = t.value0.value0[1];
      r[k] = erlJSONToJsJSON(v);
      t = t.value1;
    }
    return r;
  }
  if (t instanceof erlBin) return erlBinToJSStr(t);
  if (t instanceof erlAtom) {
    if (t.value0 === 'true') return true;
    if (t.value0 === 'false') return false;
    return t.value0;
  }
  if (t instanceof erlFloat) return t.value0;
  if (t instanceof erlInt) {
    return PS['Erlang.Utils'].bigIntToInt(t.value0).value0;
  }
  console.log(t);
  throw new Error('Invalid value');
};

async function getCompilerVersion(options = {}) {
  const r = PS['Aeso.Compiler'].erlps__version__0([]);
  if (r instanceof erlTuple && r.value0[0].value0 === 'ok') {
    return erlBinToJSStr(r.value0[1]);
  }
  console.log(showTerm(r));
  throw new Error('Failed to get compiler version');
}

function erlpsPrepareCompilerOption(
  { backend = 'fate', filesystem = {} } = {},
) {
  let erlfs = mkNil;
  for (const [key, value] of Object.entries(filesystem)) {
    erlfs = mkCons(mkTuple([jsStrToErlStr(key), erlStrToErlBin(jsStrToErlStr(value))]), erlfs);
  }
  const filemap = PS['Erlang.Builtins'].maps__from_list__1([erlfs]);
  return mkCons(
    mkTuple([mkAtom('backend'), mkAtom(backend)]),
    mkCons(mkTuple([mkAtom('include'), mkTuple([mkAtom('explicit_files'), filemap])]), mkNil),
  );
}

async function compileContractAPI(code, options = {}) {
  const copts = erlpsPrepareCompilerOption(options)
  const compiled = PS['Aeso.Compiler'].erlps__from_string__2([jsStrToErlStr(code), copts]);
  if (compiled instanceof erlTuple && compiled.value0[0].value0 === 'ok') {
    const compiled1 = PS['Aeser.Contract.Code'].erlps__serialize__1([compiled.value0[1]]);
    const bytecode = PS['Aeser.Api.Encoder'].erlps__encode__2([mkAtom('contract_bytearray'), compiled1]);
    return erlBinToJSStr(bytecode);
  }
  console.log(showTerm(compiled));
  throw new Error('Failed to compile contract');
}

async function run() {
  const version = await getCompilerVersion();
  if (version !== '4.3.0') throw new Error();

  const contract = await readFile('./src/Test.aes', 'utf8');
  const bytecode = await compileContractAPI(contract);
  const expectedBytecode = 'cb_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ZBE6QiThVXRlc3RfcHJpbWl0aXZlc190dXBsZRE9BFdGNXRlc3RfaW50X3R5cGURQLhEBCF0ZXN0X2ludBFBufYfZXRlc3Rfb3JhY2xlX3F1ZXJ5X2FkZHJlc3MRRNZEHxFpbml0EUU+O7k9dGVzdF9zdHJpbmdfbWFwEUfOd+JBdGVzdF9yZWNvcmRzX21hcBFKXBE9RXRlc3RfcmVjb3Jkc19saXN0EUssvmspdGVzdF9lbXB0eRFLwdBRJXRlc3RfbGlzdBFMrKn+LXRlc3RfcmVjb3JkEU11ISA9dGVzdF9sb25nX3R1cGxlEVA1fAlFdGVzdF90ZW1wbGF0ZV9tYXARW3QdbCl0ZXN0X3R1cGxlEV0TH4w9dGVzdF9uZXN0ZWRfbWFwEWJaC9c1dGVzdF92YXJpYW50cxFlpeAPLUNoYWluLmV2ZW50EWdBkGEldGVzdF91bml0EXWpeiZRdGVzdF9uZXN0ZWRfdmFyaWFudHMRdyWNfDF0ZXN0X2J5dGVzMzIRewibU010ZXN0X2FkZHJlc3NfcmVjb3JkEX3l4jEtdGVzdF9ldmVudHMRg6J2CyF0ZXN0X3R0bBGExpeGNXRlc3RfbGliX3R5cGURj5rNEU10ZXN0X29yYWNsZV9hZGRyZXNzEZCNt1RJdGVzdF9uZXN0ZWRfcmVjb3JkEZy8Xjw9dGVzdF9zaW5nbGVfaW50EaELxYgldGVzdF9ib29sEagK9D9JdGVzdF90ZW1wbGF0ZV90eXBlEa5rRDtRdGVzdF90ZW1wbGF0ZV9yZWNvcmQRruUsPEl0ZXN0X3RlbXBsYXRlX21hemURtPhwcTV0ZXN0X29wdGlvbmFsEbbuQYYldGVzdF9iaXRzEcEat6lZdGVzdF90ZW1wbGF0ZV92YXJpYW50cxHLN316PXRlc3Rfc2ltcGxlX21hcBHYGGWQSXRlc3Rfb3B0aW9uX3JlY29yZBHgYyOEUXRlc3RfYWNjb3VudF9hZGRyZXNzEeQoKR5FdGVzdF9uZXN0ZWRfdHVwbGUR6LzL90l0ZXN0X2NvbXBsZXhfdHVwbGUR8MwrlS10ZXN0X3N0cmluZ4IvAIU0LjMuMAAbgPWP';
  if (bytecode !== expectedBytecode) throw new Error();

  console.log('Looks ok');
  process.exit();
}
run();
