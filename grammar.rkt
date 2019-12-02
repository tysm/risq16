#lang brag

program    : [expression] (NEWLINE [expression])*
expression : nop | op

nop        : "nop"
op         : add | addu | sub | subu
           | div | divu | mul | mulu
           | and | or | xor | shl
           | shr | seq | sne | sgt
           | slt | sge | sle | brz
           | bnz | lw | sw | input
           | output

add        : "add" reg operand
addu       : "addu" reg operand
sub        : "sub" reg operand
subu       : "subu" reg operand
div        : "div" reg operand
divu       : "divu" reg operand
mul        : "mul" reg operand
mulu       : "mulu" reg operand
and        : "and" reg operand
or         : "or" reg operand
xor        : "xor" reg operand
shl        : "shl" reg operand
shr        : "shr" reg operand

seq        : "seq" reg operand
sne        : "sne" reg operand
sgt        : "sgt" reg operand
slt        : "slt" reg operand
sge        : "sge" reg operand
sle        : "sle" reg operand
brz        : "brz" reg operand
bnz        : "bnz" reg operand

lw         : "lw" reg OPENBRACKET operand CLOSEBRACKET
sw         : "sw" reg OPENBRACKET operand CLOSEBRACKET

input      : "input" reg
output     : "output" reg

reg        : REG

operand    : int | reg
int        : INT