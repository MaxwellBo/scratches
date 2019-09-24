/**
 * ALL LINES TYPECHECK UNLESS STATED OTHERWISE
 */

// #############################################################################

let _true: true = true
let _false: false = false
let _0: 0 = 0
let _literal: 'literal' = 'literal'

let _never: never // empty set, ⊥
let _any: any // set of everything, ⊤

//         { true } ∪ { false }
let _trueFalse: true | false = true

// a: T = b where b is of type U
// is equivalent to
// U ⊂ T
let _boolean: boolean = _true
_boolean = _trueFalse 
_boolean = _0 // does not typecheck

// extends is equivalent to the subset operator
const _1In1And2: 1 extends (1 | 2) ? true : false = true
const _2In1:     1 extends (2)     ? true : false = false

type Subset<A, B> = [A] extends [B] ? true : false
// what's with the [A] and [B]?

// `T extends U ? X : Y` with the type argument `A | B | C` for `T` is resolved as 
// `(A extends U ? X : Y) | (B extends U ? X : Y) | (C extends U ? X : Y)`

// type Subset<A, B> = A extends B ? true : false
// would cause `Subset<2 | 3, 3>` to resolve to `boolean`, because while 2 is not assignable to 3, 3 is, hence `true | false` aka `boolean`
const _23IsSubsetOf123: Subset<2 | 3, 1 | 2 | 3> = true
const _23IsSubsetOf1: Subset<2 | 3, 1> = false
const _23IsSubsetOf2: Subset<2 | 3, 2> = false

const _asdfIfSubsetOfString: Subset<string, 'asdf'> = false
const _stringIsSubsetOfAsdf: Subset<'asdf', string> = true

const _asdfListIsSubsetOfStringList: Subset<'asdf'[], string[]> = true
const _stringListIsSubsetOfAsdfList: Subset<string[], 'asdf'[]> = false

type T12 = 1 | 2
type T23 =     2 | 3

type T2 = T12 & T23


type TAny = 1 | any
type T1 = 1 | never

type TAnyAsWell = 1 & any // WTF?, this is `any`, not `1`
type TNever = 1 & never


// { true }
let _testTrueTrueIntersection: true & true = true
_testTrueIntersection = false // doesn't typecheck


// {}
let _testTrueFalseIntersection: true & false = _never
_testTrueFalseIntersection = true // doesn't typecheck
_testTrueFalseIntersection = false // doesn't typecheck

const _isTrueIntersection: true extends true & true ? true : false = true
const _isFalseIntersection: true extends true & false ? true : false = false

type And<A, B> = true extends A & B ? true : false

const _testTT: And<true, true> = true
const _testTF: And<true, false> = false
const _testFF: And<false, false> = false
const _testFT: And<false, true> = false

// type Equals<A, B> = true extends (A extends B ? true : false) & (B extends A ? true : false) ? true : false
type Equal<A, B> = And<Subset<A, B>, Subset<B, A>>

const _testBooleanEquality: Equal<boolean, true | false> = true
const _testIntersectionInequality: Equal<1 | 2, 2 | 3> = false

// #############################################################################
// Interfaces are equivalent to type functions

type StringToString<A extends string> = string

interface IStringToString {
    [a: string]: string
}

type StringToNumber<A extends string> = number

interface IStringToNumber {
    [a: string]: number
}

type StringToStringOrNumber<T extends string> = StringToString<T> | StringToNumber<T>

type IStringToStringOrNumber = IStringToNumber | IStringToString

let _stringOrNumber: IStringToStringOrNumber['arbitary parameter']
_stringOrNumber = 'string'
_stringOrNumber = 0

let _AlsoStringOrNumber: StringToStringOrNumber<'arbitary parameter'>
_AlsoStringOrNumber = 'string'
_AlsoStringOrNumber = 0

interface NumberCodec {
    0: 'zero',
    'zero': 0
}

const _zeroString: NumberCodec[0] = 'zero';
const _zeroNumber: NumberCodec['zero'] = 0
const _roundtrip:  NumberCodec[NumberCodec['zero']] = 'zero'
