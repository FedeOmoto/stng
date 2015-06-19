import pegs, strutils, bignum, token, ../base/obj

let numberPeg = peg"({^{\-}?{\d\d?}{r}{[0-9A-Z]+}})/({^{{\-}?{\d+}{{\.}\d+}?}{{{{e}{\-/\+}?}/{s}}{\d+}}?})"
var matches1: array[0 .. 11, string]
var matches2: array[0 .. 11, string]
var matches3: array[0 .. 11, string]
var matches4: array[0 .. 11, string]
let numberLen1 = "-12.3s4".matchLen(numberPeg, matches1)
let numberLen2 = "12.3s4".matchLen(numberPeg, matches2)
let numberLen3 = "-12s4".matchLen(numberPeg, matches3)
let numberLen4 = "12s4".matchLen(numberPeg, matches4)

echo repr(matches1)
echo repr(matches2)
echo repr(matches3)
echo repr(matches4)
