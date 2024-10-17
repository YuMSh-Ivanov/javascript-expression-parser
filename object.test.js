"use strict";

const util = require("node:util");
const {Const, Variable, Add, Subtract, Multiply, Divide, Negate, RMS, Sin, Cos, Pi, parsePrefix, "expression_error_type" : ExpressionError} = require("./object");

const sfc32 = require("./random-sfc32");
const rng = sfc32(0xB16B00B5, 0xCAFEBABE, 0xDEADBEEF, 0xF0CACC1A);


const numbersStr = ["0.0", "-0.0",
                    "1", "-1",
                    "1000", "-1000",
                    "0.001", "-0.001",
                    "Number.MAX_SAFE_INTEGER", "Number.MIN_SAFE_INTEGER", "Number.MAX_SAFE_INTEGER + 1", "Number.MIN_SAFE_INTEGER - 1",
                    "Number.MAX_VALUE", "-Number.MAX_VALUE",
                    "Number.MIN_VALUE", "-Number.MIN_VALUE",
                    "Number.EPSILON", "-Number.EPSILON",
                    "Number.POSITIVE_INFINITY", "Number.NEGATIVE_INFINITY", "Number.NaN"];

const numbers = numbersStr.map(eval);

const variables = ["x", "y", "z"];

test("Is expression_error_type an Error?", () => {
    capture("expression_error_type should inherit Error", () => {
        expect(ExpressionError.prototype).toBeInstanceOf(Error);
    });
});

function escapeWS(s) {
    let ans = "";
    for (const ch of s.replaceAll('\t', '\\t').replaceAll('\v', '\\v').replaceAll('\f', '\\f').replaceAll('\n', '\\n').replaceAll('\r', '\\r').replaceAll('\xA0', '\\xA0')) {
        if (ch.charCodeAt() >= 0x2000 && ch.charCodeAt() <= 0x200A
                || ch.charCodeAt() == 0x202F
                || ch.charCodeAt() == 0x205F
                || ch.charCodeAt() == 0x3000
                || ch.charCodeAt() == 0xFEFF
                || ch.charCodeAt() == 0x2028
                || ch.charCodeAt() == 0x2029
                || ch.charCodeAt() == 0x1680) {
            ans += '\\u' + ch.charCodeAt().toString(16);
        } else {
            ans += ch;
        }
    }
    return ans;
}

function capture(msg, checks) {
    try {
        return checks();
    } catch (err) {
        err.message += "\n" + msg;
        throw err;
    }
}


function testExpression(actualStr, evaluate, repr, name, inf) {
    const actual = eval(actualStr);
    capture(`(${escapeWS(actualStr)}).constructor is wrong class`, () => {
        expect(actual.constructor).toBe(name);
    });
    capture(`Object.getPrototypeOf(${escapeWS(actualStr)}) is not equal to class' prototype property`, () => {
        expect(Object.getPrototypeOf(actual)).toBe(name.prototype);
    })
    capture(`Testing evaluate of ${escapeWS(actualStr)}`, () => {
        for (const x of numbers) {
            for (const y of numbers) {
                for (const z of numbers) {
                    capture(`At point [${x}, ${y}, ${z}]`, () => expect(actual.evaluate(x, y, z)).toBe(evaluate(x, y, z)));
                }
            }
        }
    });
    capture(`Testing prefix of ${escapeWS(actualStr)}`, () => {
        expect(actual.prefix()).toBe(repr);
    });

    capture(`Testing infix of ${escapeWS(actualStr)}`, () => {
        expect(actual.infix()).toBe(inf);
    });
}

const log_cases = {};

function expectThrow(action, log = undefined) {
    if (log !== undefined) {
        expect(() => {
            try {
                action();
            } catch (e) {
                if (e instanceof ExpressionError) {
                    log_cases[log] = e.name + "{" + e.message + "}"
                }
                throw e;
            }
        }).toThrow(ExpressionError);
    } else {
        expect(action).toThrow(ExpressionError);
    }
}

function creationExpectThrown(actualStr) {
    capture(`Attempting to call ${actualStr} [expecting an error]`, () => {
        expectThrow(() => eval(actualStr), actualStr);
    });
}

function evaluationExpectThrow(actualStr, args) {
    let actual;
    capture(`Testing evaluation of ${actualStr}`, () => {
        actual = eval(actualStr);
    })
    for (let xyz of args) {
        capture(`Attempting to evaluate ${actualStr} at point [${xyz.join(', ')}] [expecting an error]`, () => {
            expect(() => {
                try {
                    actual.evaluate(...xyz);
                } catch (e) {
                    if (e instanceof ExpressionError) {
                        log_cases[`(${actualStr}).evaluate(${xyz.join(', ')})`] = `${e.name}{${e.message}}`;
                    }
                    throw e;
                }
            }).toThrow(ExpressionError);
        });
    }
}

test("constants", () => {
    for (let i = 0; i < numbers.length; i++) {
        testExpression(`new Const(${numbersStr[i]})`, (x, y, z) => numbers[i], numbers[i].toString(), Const, numbers[i].toString());
    }
});

test("constants create throw", () => {
    creationExpectThrown("new Const()");
    creationExpectThrown("new Const(null)");
    creationExpectThrown("new Const('hello')");
    creationExpectThrown("new Const({})");
    creationExpectThrown("new Const('1')");
    creationExpectThrown("new Const([1])");
    creationExpectThrown("new Const(1, 2)");
});

test("variables", () => {
    testExpression("new Variable('x')", (x, y, z) => x, 'x', Variable, 'x');

    testExpression("new Variable('y')", (x, y, z) => y, 'y', Variable, 'y');

    testExpression("new Variable('z')", (x, y, z) => z, 'z', Variable, 'z');
});

test("variables create throw", () => {
    creationExpectThrown("new Variable()");
    creationExpectThrown("new Variable('t')");
    creationExpectThrown("new Variable(123)");
    creationExpectThrown("new Variable(['x'])");
    creationExpectThrown("new Variable('yyy')");
    creationExpectThrown("new Variable('x', 'y')");
});

test("simple", () => {
    testExpression("new Add(new Const(3), new Variable('y'))", (x, y, z) => 3 + y, "(+ 3 y)", Add, "(3 + y)");

    testExpression("new Subtract(new Const(Number.MAX_SAFE_INTEGER), new Variable('z'))", (x, y, z) => Number.MAX_SAFE_INTEGER - z, `(- ${Number.MAX_SAFE_INTEGER} z)`, Subtract, `(${Number.MAX_SAFE_INTEGER} - z)`);

    testExpression("new Multiply(new Variable('x'), new Variable('z'))", (x, y, z) => x * z, "(* x z)", Multiply, "(x * z)");

    testExpression("new Divide(new Variable('z'), new Variable('y'))", (x, y, z) => z / y, "(/ z y)", Divide, "(z / y)");

    testExpression("new Negate(new Variable('x'))", (x, y, z) => -x, "(negate x)", Negate, "-(x)");
});

const rms = (...args) => Math.sqrt(args.map(a => a * a).reduce((a, b) => a + b, 0) / args.length)
const pi = Math.PI;
const cos = Math.cos;
const sin = Math.sin;

test("simple modification (var-args)", () => {
    testExpression("new Multiply(new Const(5))", (x, y, z) => 5, "(* 5)", Multiply, "(5)");
    testExpression("new Add(new Variable('y'))", (x, y, z) => y, "(+ y)", Add, "(y)");
    testExpression("new Multiply(new Variable('z'), new Variable('z'), new Variable('y'), new Variable('x'), new Const(777))", (x, y, z) => z * z * y * x * 777, "(* z z y x 777)", Multiply, "(z * z * y * x * 777)");
    testExpression("new Add(new Variable('y'), new Const(45), new Add(new Variable('x'), new Const(-3)), new Negate(new Variable('z')))", (x, y, z) => y + 45 + (x + -3) + -z, "(+ y 45 (+ x -3) (negate z))", Add, "(y + 45 + (x + -3) + -(z))");
    testExpression("new Add(new Multiply(new Const(-13), new Const(0.5), new Variable('y')), new Variable('x'), new Negate(new Add(new Variable('z'))), new Const(-15))", (x, y, z) => (-13 * 0.5 * y) + x + -(z) + -15, "(+ (* -13 0.5 y) x (negate (+ z)) -15)", Add, "((-13 * 0.5 * y) + x + -((z)) + -15)")
});

test("simple modification (operations)", () => {
    testExpression("new Pi()", (x, y, z) => Math.PI, "(PI)", Pi, "pi");
    testExpression("new Cos(new Pi())", (x, y, z) => -1, "(cos (PI))", Cos, "cos(pi)");
    testExpression("new Sin(new Divide(new Pi(), new Variable('z')))", (x, y, z) => Math.sin(Math.PI / z), "(sin (/ (PI) z))", Sin, "sin((pi / z))");
    testExpression("new RMS(new Variable('x'), new Add(new Variable('z'), new Const(-3)), new Const(4))", (x, y, z) => rms(x, (z + -3), 4), "(rms x (+ z -3) 4)", RMS, "rms(x, (z + -3), 4)");
});

test("simple create throw", () => {
    creationExpectThrown("new Add()");
    creationExpectThrown("new Subtract('x')");
    creationExpectThrown("new Multiply(123)");
    creationExpectThrown("new Divide(new Const(1), new Const(2), new Const(3))");
    creationExpectThrown("new Negate(new Variable('x'), new Const(-5))");
    creationExpectThrown("new Pi(new Const(4))");
    creationExpectThrown("new Cos()");
    creationExpectThrown("new Sin(new Variable('x'), new Const(11))");
    creationExpectThrown("new RMS({})");
});

test("evaluate throw", () => {
    for (const exprs of ["new Const(100)", "new Variable('x')", "new Negate(new Add(new Const(4), new Variable('z')))"]) {
        evaluationExpectThrow(exprs, [[], [1, 2], [, 4, 5], [14, 5, [2]], [null, 0, -5], [14, '14', 14], [0, 1, 2, 3]]);
    }
});

const operations = [["Add",      "+",      " + ", "",    -1],
                    ["Subtract", "-",      " - ", "",    2],
                    ["Multiply", "*",      " * ", "",   -1],
                    ["Divide",   "/",      " / ", "",    2],
                    ["Negate",   "negate", "",    "-",   1],
                    ["RMS",      "rms",    ", ",  "rms", -1],
                    ["Sin",      "sin",    "",    "sin", 1],
                    ["Cos",      "cos",    "",    "cos", 1],
                    ["Pi",       "PI",     "",    "pi",  0]];

function generateRandomTest(depth) {
    const r = depth > 0 ? Math.floor(rng() * (operations.length + 2)) : Math.floor(rng() * 2) + operations.length;
    if (r === operations.length) {
        const c = rng() * 200001 - 100000;
        return {a : `new Const(${c})`, e : c.toString(), s : c.toString(), c : "Const"};
    } else if (r === operations.length + 1) {
        const v = variables[Math.floor(rng() * variables.length)];
        return {a : `new Variable('${v}')`, e : v, s : v, c : "Variable"};
    }
    const {0 : oper, 1 : name, 2 : inf, 3 : pref, 4 : acnt1} = operations[r];
    const acnt = acnt1 !== -1 ? acnt1 : 1 + Math.floor(rng() * 6);
    const args = Array.from({length : acnt}, () => generateRandomTest(depth - 1));
    const impl = args.length === 0 ? pref : `${pref}(${args.map(t => t.e).join(inf)})`;
    return {c : oper, a : `new ${oper}(${args.map(t => t.a)})`, e : impl, s : `(${name}${args.map(t => " " + t.s).join('')})`};
}

function randomTest(depth, count) {
    test("random with depth " + depth, () => {
        for (let i = 0; i < count; i++) {
            const {a : actual, e : expected, s : repr, c : name} = generateRandomTest(depth)
            testExpression(actual, eval(`(x, y, z) => ${expected}`), repr, eval(name), expected);
        }
    })
}

randomTest(2, 5)
randomTest(5, 5)
randomTest(7, 5)

test("parse simple", () => {
    testExpression("parsePrefix('1')", (x, y, z) => 1, "1", Const, "1");
    testExpression("parsePrefix('y')", (x, y, z) => y, "y", Variable, "y");
    testExpression("parsePrefix('(+ z .2)')", (x, y, z) => z + .2, "(+ z 0.2)", Add, "(z + 0.2)");
    testExpression("parsePrefix('(* y -5.5 x -Infinity)')", (x, y, z) => y * -5.5 * x * -Infinity, "(* y -5.5 x -Infinity)", Multiply, "(y * -5.5 * x * -Infinity)");
    testExpression("parsePrefix('(- (sin (PI)) (cos (negate x)))')", (x, y, z) => sin(pi) - cos(-x), "(- (sin (PI)) (cos (negate x)))", Subtract, "(sin(pi) - cos(-(x)))");
});

const ws = "\t\v\f \xA0\u1680\u2000\u2001\u2002\u2003\u2004\u2005\u2006\u2007\u2008\u2009\u200A\u202F\u205F\u3000\uFEFF\n\r\u2028\u2029"

test("parse simple (WS)", () => {
    testExpression(escapeWS("parsePrefix('\u200010.67\u2028')"), (x, y, z) => 10.67, "10.67", Const, "10.67");
    testExpression(
        "parsePrefix('(rms(-(PI)(/ 49932.19346581935 76977.54319403274))(*(negate 91965.27912336565)))')",
        (x, y, z) => rms(pi - (49932.19346581935 / 76977.54319403274), -91965.27912336565),
        "(rms (- (PI) (/ 49932.19346581935 76977.54319403274)) (* (negate 91965.27912336565)))",
        RMS,
        "rms((pi - (49932.19346581935 / 76977.54319403274)), (-(91965.27912336565)))"
    );
    testExpression(
        escapeWS("parsePrefix(' \t(\v\f/\xA0\u1680(\u2000\u2001*\u2002\u2003(\u2004\u2005+\u2006\u200753963.54993002652\u2008\u2009)\u200A\u202F)\u205F\u3000(\uFEFF\n-\r\u2028(\u2029\u2002*\n\r18027.53764423495\u205F\f)\r\u2001z\u2003\u2002)\u2028\uFEFF)\u2008\u2008')"),
        (x, y, z) => 53963.54993002652 / (18027.53764423495 - z),
        "(/ (* (+ 53963.54993002652)) (- (* 18027.53764423495) z))",
        Divide,
        "(((53963.54993002652)) / ((18027.53764423495) - z))"
    );
});

function rngWS(min, ran) {
    const cnt = Math.floor(rng() * ran + min);
    return Array.from({length : cnt}, () => ws[Math.floor(rng() * ws.length)]).join('');
}

function preWS(s1) {
    if (s1[0] === '(') {
        return rngWS(0, 3) + s1;
    } else {
        return rngWS(1, 2) + s1;
    }
}

function generateRandomParseTest(depth) {
    const r = depth > 0 ? Math.floor(rng() * (operations.length + 2)) : Math.floor(rng() * 2) + operations.length;
    if (r === operations.length) {
        const c = rng() * 200001 - 100000;
        return {e : c.toString(), s : c.toString(), c : "Const", s1 : c.toString()};
    } else if (r === operations.length + 1) {
        const v = variables[Math.floor(rng() * variables.length)];
        return {e : v, s : v, c : "Variable", s1 : v};
    }
    const {0 : oper, 1 : name, 2 : inf, 3 : pref, 4 : acnt1} = operations[r];
    const acnt = acnt1 !== -1 ? acnt1 : 1 + Math.floor(rng() * 6);
    const args = Array.from({length : acnt}, () => generateRandomParseTest(depth - 1));
    const impl = args.length === 0 ? pref : `${pref}(${args.map(t => t.e).join(inf)})`;
    return {c : oper, e : impl, s : `(${name}${args.map(t => " " + t.s).join('')})`, s1 : `(${name}${args.map(t => preWS(t.s1)).join('')})`};
}

function randomParseTest(depth, count) {
    test("parse random with depth " + depth, () => {
        for (let i = 0; i < count; i++) {
            const {e : expected, s : repr, c : name, s1 : toparse} = generateRandomParseTest(depth)
            testExpression(escapeWS(`parsePrefix('${rngWS(0, 3)}${toparse}${rngWS(0, 3)}')`), eval(`(x, y, z) => ${expected}`), repr, eval(name), expected);
        }
    })
}

randomParseTest(2, 5);
randomParseTest(3, 5);
randomParseTest(5, 4);
randomParseTest(7, 2);

test("parse throw", () => {
    creationExpectThrown("parsePrefix(null)");
    creationExpectThrown("parsePrefix(1)");
    creationExpectThrown("parsePrefix(['1'])");
    creationExpectThrown("parsePrefix(['(+ 1 2)'])");

    creationExpectThrown("parsePrefix(' ')");
    creationExpectThrown("parsePrefix('.')");
    creationExpectThrown("parsePrefix('-.')");
    creationExpectThrown("parsePrefix('--1')");
    creationExpectThrown("parsePrefix('123hello')");
    creationExpectThrown("parsePrefix('@')");
    creationExpectThrown("parsePrefix('(')");
    creationExpectThrown("parsePrefix(')')");

    creationExpectThrown("parsePrefix('+')");
    creationExpectThrown("parsePrefix('negate')");
    creationExpectThrown("parsePrefix('PI')");

    creationExpectThrown("parsePrefix('()')");
    creationExpectThrown("parsePrefix('(x)')");
    creationExpectThrown("parsePrefix('(1)')");
    creationExpectThrown("parsePrefix('(-1 y)')");

    creationExpectThrown("parsePrefix('(+)')");
    creationExpectThrown("parsePrefix('(-)')");
    creationExpectThrown("parsePrefix('(/ 15)')");
    creationExpectThrown("parsePrefix('(negate 10 20 30)')");
    creationExpectThrown("parsePrefix('(rms)')");
    creationExpectThrown("parsePrefix('(sin)')");
    creationExpectThrown("parsePrefix('(PI 1)')");

    creationExpectThrown("parsePrefix('(pi)')");
    creationExpectThrown("parsePrefix('(Pi)')");

    creationExpectThrown("parsePrefix('(PI')");
    creationExpectThrown("parsePrefix('(- 1 x')");
    creationExpectThrown("parsePrefix('- 1 x)')");
    creationExpectThrown("parsePrefix('(/ 2 z))')");
    creationExpectThrown("parsePrefix('(* 2 y @)')");
    creationExpectThrown("parsePrefix('(@ 1 2)')");
    creationExpectThrown("parsePrefix('(+ (cos 1)')");
    creationExpectThrown("parsePrefix('(rms (cos 1)) 15)')");
    creationExpectThrown("parsePrefix('(+ negate 1)')");
})

afterAll(() => {
    console.log(Object.entries(log_cases).map(([key, value]) => key + ": " + value).join('\n'));
})
