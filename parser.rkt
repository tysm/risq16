#lang brag

risq-program   : [risq-line] (/NEWLINE [risq-line])*

risq-line      : [risq-label-def] [risq-opcode]

risq-label-def : ID /COLON

@risq-opcode    : risq-nop | risq-add | risq-addu | risq-sub | risq-subu
                | risq-div | risq-divu |risq-mul | risq-mulu
                | risq-and | risq-or | risq-xor | risq-shl
                | risq-shr | risq-set | risq-seq | risq-sne | risq-sgt
                | risq-slt | risq-sge | risq-sle | risq-brz
                | risq-bnz | risq-b | risq-br | risq-c | risq-cr
                | risq-lw | risq-sw | risq-input | risq-output

risq-nop        : /"nop"
risq-add        : /"add" risq-reg risq-operand
risq-addu       : /"addu" risq-reg risq-operand
risq-sub        : /"sub" risq-reg risq-operand
risq-subu       : /"subu" risq-reg risq-operand
risq-div        : /"div" risq-reg risq-operand
risq-divu       : /"divu" risq-reg risq-operand
risq-mul        : /"mul" risq-reg risq-operand
risq-mulu       : /"mulu" risq-reg risq-operand
risq-and        : /"and" risq-reg risq-operand
risq-or         : /"or" risq-reg risq-operand
risq-xor        : /"xor" risq-reg risq-operand
risq-shl        : /"shl" risq-reg risq-operand
risq-shr        : /"shr" risq-reg risq-operand

risq-set        : /"set" risq-reg risq-operand
risq-seq        : /"seq" risq-reg risq-operand
risq-sne        : /"sne" risq-reg risq-operand
risq-sgt        : /"sgt" risq-reg risq-operand
risq-slt        : /"slt" risq-reg risq-operand
risq-sge        : /"sge" risq-reg risq-operand
risq-sle        : /"sle" risq-reg risq-operand
risq-brz        : /"brz" risq-reg risq-label
risq-bnz        : /"bnz" risq-reg risq-label
risq-b          : /"b" risq-label
risq-c          : /"c" risq-label
risq-br         : /"br" risq-reg
risq-cr         : /"cr" risq-reg

risq-lw         : /"lw" risq-reg /OPENBRACKET risq-operand /CLOSEBRACKET
risq-sw         : /"sw" risq-reg /OPENBRACKET risq-operand /CLOSEBRACKET

risq-input      : /"input" risq-reg
risq-output     : /"output" risq-reg

risq-reg        : REG
risq-int        : INT
risq-label      : ID
@risq-operand   : risq-int | risq-reg