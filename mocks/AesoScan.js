'use static';
var Lexer = require('flex-js');
var lexer = new Lexer();

exports.lexer = lexer;

function lex(make_atom,
             make_tuple,
             make_list,
             make_int,
             make_string,
             make_bin,
             input
            ) {
    lexer.setSource(input);
    lexer.make_atom   = make_atom;
    lexer.make_tuple  = make_tuple;
    lexer.make_list  = make_list;
    lexer.make_int    = make_int;
    lexer.make_string = make_string;
    lexer.make_bin    = make_bin;
    lexer.col = 1;
    lexer.line = 1;
    lexer.tokens = [];
    lexer.lex();
    return lexer.tokens;
}

exports.lexImpl =
    function(make_atom) {
        return function (make_tuple) {
            return function (make_list) {
                return function (make_int) {
                    return function (make_string) {
                        return function (make_bin) {
                            return function (input) {
                                return lex(make_atom,
                                           make_tuple,
                                           make_list,
                                           make_int,
                                           make_string,
                                           make_bin,
                                           input
                                          );
                            };
                        };
                    };
                };
            };
        };
    };

lexer.addLocRule = function(rule, hook) {
    lexer.addRule(rule, (lexer) => {
        var lines = lexer.text.split("\n");
        hook(lexer);
        if(lines) {
            lexer.line += lines.length - 1;
            if(lines.length == 1) {
                lexer.col += lines[0].length;
            } else {
                lexer.col = lines[lines.length - 1].length + 1;
            }
        }
    });
};

// options
lexer.setIgnoreCase(false);

// definitions
lexer.addDefinition('CON', /[A-Z][a-zA-Z0-9_]*/);
lexer.addDefinition('INT', /[0-9]+(_[0-9]+)*/);
lexer.addDefinition('HEX', /0x[0-9a-fA-F]+(_[0-9a-fA-F]+)*/);
lexer.addDefinition('BYTES', /#[0-9a-fA-F]+(_[0-9a-fA-F]+)*/);
lexer.addDefinition('WS', /[\n\t\r\0\ ]+/);
lexer.addDefinition('ID', /[a-z_][a-zA-Z0-9_']*/);
lexer.addDefinition('TVAR', /'[a-z_][a-zA-Z0-9_']*/);
lexer.addDefinition('QID', /([A-Z][a-zA-Z0-9_]*\.)+[a-z_][a-zA-Z0-9_']*/);
lexer.addDefinition('QCON', /([A-Z][a-zA-Z0-9_]*\.)+[A-Z][a-zA-Z0-9_]*/);
lexer.addDefinition('OP', /[=!<>+\\\-*:&|?~@^]*/);
lexer.addDefinition('CHAR', /'([^'\\]|(\\.))'/);
lexer.addDefinition('STRING', /\"([^\"\\]|(\\.))*\"/);

keywords = ["contract", "include", "let", "switch", "type", "record", "datatype", "if", "elif", "else", "function",
            "stateful", "payable", "true", "false", "mod", "public", "entrypoint", "private", "indexed", "namespace"];


// rules
lexer.addLocRule('/*', function (lexer) {
    var depth = 1;
    do {
        var char = lexer.input();
        var nextChar;
        if (char === '*') {
            nextChar = lexer.input();
            if (nextChar === '/') {
                depth--;
            }
        } else if (char === '/') {
            nextChar = lexer.input();
            if (nextChar === '*') {
                depth++;
            }
        }
    } while (char !== '' && depth !== 0);
});
lexer.addLocRule(/\/\/(?:(?!\n).)*\n/, function(lexer){});
lexer.addLocRule(/{WS}/, function(lexer){});
lexer.addLocRule(/(\.\.)|[,\.;()\[\]{}]/, symbol);
lexer.addLocRule(/{CHAR}/,   token("char",   parse_char));
lexer.addLocRule(/{STRING}/, token("string", parse_string));
lexer.addLocRule(/{HEX}/,    token("hex",    parse_hex));
lexer.addLocRule(/{INT}/,    token("int",    parse_int));
lexer.addLocRule(/{BYTES}/,  token("bytes",  parse_bytes));
lexer.addLocRule(/{QID}/,   token("qid", parse_qual));
lexer.addLocRule(/{QCON}/,  token("qcon", parse_qual));
lexer.addLocRule(/{TVAR}/,  token_raw("tvar"));
lexer.addLocRule(/{CON}/, token_raw("con"));
lexer.addLocRule(/{ID}/, parse_id_or_keyword);
lexer.addLocRule(/{OP}/, symbol);
lexer.addLocRule(/[/]/, symbol);  // for some reason this needs to be handled separately. let us pray that no operator using this char will be introduced...

function symbol(lexer) {
    lexer.tokens.push(
        lexer.make_tuple([
            lexer.make_atom(lexer.text),
            lexer.make_tuple([
                lexer.make_int(lexer.line),
                lexer.make_int(lexer.col)
            ])
        ]));
}

function token_raw(tag) {
    return function(lexer) {
        lexer.tokens.push(
            lexer.make_tuple([
                lexer.make_atom(tag),
                lexer.make_tuple([
                    lexer.make_int(lexer.line),
                    lexer.make_int(lexer.col)
                ]),
                lexer.make_string(lexer.text)
            ]));
    };
}

function token(tag, parse) {
    return function(lexer) {
        lexer.tokens.push(
            lexer.make_tuple([
                lexer.make_atom(tag),
                lexer.make_tuple([
                    lexer.make_int(lexer.line),
                    lexer.make_int(lexer.col)
                ]),
                parse(lexer)
            ]));
    };
}

function parse_id_or_keyword(lexer) {
    if(keywords.indexOf(lexer.text) >= 0) {
        symbol(lexer);
    } else {
        token_raw("id")(lexer);
    }
}

function parse_qual(lexer) {
    return lexer.make_list(
        lexer.text.split(".").map((i) => lexer.make_string(i))
    );
}

function parse_string(lexer) {
    var s = lexer.text;
    if(s.length >= 2) {
        var s1 = s
            .substring(1, s.length - 1)
            .replaceAll("\\\\", String.fromCharCode(92))
            .replaceAll("\\\"", String.fromCharCode(34))
            .replaceAll("\\n", String.fromCharCode(10))
            .replaceAll("\\t", String.fromCharCode(9))
            .replaceAll("\\r", String.fromCharCode(13))
            .replaceAll("\\b", String.fromCharCode(8))
            .replaceAll("\\e", String.fromCharCode(27))
            .replaceAll("\\f", String.fromCharCode(12))
            .replaceAll("\\v", String.fromCharCode(11));
        var escstr = encodeURIComponent(s1);
        var binstr = escstr.replace(/%([0-9A-F]{2})/g, function(match, p1) {
            return String.fromCharCode('0x' + p1);
        });
        var ua = new Uint8Array(binstr.length);
        Array.prototype.forEach.call(binstr, function (ch, i) {
            ua[i] = ch.charCodeAt(0);
        });
        var buf = Buffer.alloc(ua.length);
        for (var i = 0; i < ua.length; ++i) {
            buf[i] = ua[i];
        }
        return lexer.make_bin(buf);
    } else throw(Error("lexer: bad string: " + s));
}

function parse_char(lexer) {
    var s = lexer.text;
    if(s.length == 3) {
        return lexer.make_int(s.charCodeAt(1));
    } else if(s.length == 4 && s[1] === '\\') {
        switch(s[2]) {
        case "\\": return lexer.make_int(92);
        case "\"": return lexer.make_int(34);
        case "n": return lexer.make_int(10);
        case "t": return lexer.make_int(9);
        case "r": return lexer.make_int(13);
        case "b": return lexer.make_int(8);
        case "e": return lexer.make_int(27);
        case "f": return lexer.make_int(12);
        case "v": return lexer.make_int(11);
        }
        throw(Error("lexer: bad escaped char: " + s));
    } else throw(Error("lexer: bad char: " + s));
}

function parse_hex(lexer) {
    return lexer.make_int(parseInt(lexer.text.replaceAll("_", ""), 16));
}

function parse_int(lexer) {
    return lexer.make_int(parseInt(lexer.text.replaceAll("_", "")));
}

function parse_bytes(lexer) {
    var numtext = lexer.text.substring(1, lexer.text.length).replaceAll("_", "");
    var digits = Math.ceil(numtext.length / 2);

    var n = BigInt("0x" + numtext);

    const a = [];
    a.unshift(Number(n & BigInt(255)));
    while (n >= BigInt(256)) {
        n = n >> BigInt(8);
        a.unshift(Number(n & BigInt(255)));
    }

    var ab = new Uint8Array(a).buffer;
    var buf = Buffer.alloc(digits);
    var view = new Uint8Array(ab);
    for (var i = 0; i < view.length; ++i) {
        buf[i + digits - view.length] = view[i];
    }
    return lexer.make_bin(buf);
}
