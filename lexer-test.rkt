#lang br

(require "lexer.rkt" brag/support rackunit)

(define (lex str)
  (apply-port-proc risq16-lexer str))

(check-equal? (lex "") empty)
(check-equal?
 (lex " ")
 (list (srcloc-token (token " " #:skip? #t)
                     (srcloc 'string 1 0 1 1))))

(check-equal?
 (lex "nop")
 (list (srcloc-token (token "nop" "nop") (srcloc 'string 1 0 1 3))))

(check-equal?
 (lex "add")
 (list (srcloc-token (token "add" "add") (srcloc 'string 1 0 1 3))))

(check-equal?
 (lex "addu")
 (list (srcloc-token (token "addu" "addu") (srcloc 'string 1 0 1 4))))

(check-equal?
 (lex "sub")
 (list (srcloc-token (token "sub" "sub") (srcloc 'string 1 0 1 3))))

(check-equal?
 (lex "subu")
 (list (srcloc-token (token "subu" "subu") (srcloc 'string 1 0 1 4))))

(check-equal?
 (lex "div")
 (list (srcloc-token (token "div" "div") (srcloc 'string 1 0 1 3))))

(check-equal?
 (lex "divu")
 (list (srcloc-token (token "divu" "divu") (srcloc 'string 1 0 1 4))))

(check-equal?
 (lex "mul")
 (list (srcloc-token (token "mul" "mul") (srcloc 'string 1 0 1 3))))

(check-equal?
 (lex "mulu")
 (list (srcloc-token (token "mulu" "mulu") (srcloc 'string 1 0 1 4))))

(check-equal?
 (lex "and")
 (list (srcloc-token (token "and" "and") (srcloc 'string 1 0 1 3))))

(check-equal?
 (lex "or")
 (list (srcloc-token (token "or" "or") (srcloc 'string 1 0 1 2))))

(check-equal?
 (lex "xor")
 (list (srcloc-token (token "xor" "xor") (srcloc 'string 1 0 1 3))))

(check-equal?
 (lex "shl")
 (list (srcloc-token (token "shl" "shl") (srcloc 'string 1 0 1 3))))

(check-equal?
 (lex "shr")
 (list (srcloc-token (token "shr" "shr") (srcloc 'string 1 0 1 3))))

(check-equal?
 (lex "seq")
 (list (srcloc-token (token "seq" "seq") (srcloc 'string 1 0 1 3))))

(check-equal?
 (lex "sne")
 (list (srcloc-token (token "sne" "sne") (srcloc 'string 1 0 1 3))))

(check-equal?
 (lex "sgt")
 (list (srcloc-token (token "sgt" "sgt") (srcloc 'string 1 0 1 3))))

(check-equal?
 (lex "slt")
 (list (srcloc-token (token "slt" "slt") (srcloc 'string 1 0 1 3))))

(check-equal?
 (lex "sge")
 (list (srcloc-token (token "sge" "sge") (srcloc 'string 1 0 1 3))))

(check-equal?
 (lex "sle")
 (list (srcloc-token (token "sle" "sle") (srcloc 'string 1 0 1 3))))

(check-equal?
 (lex "brz")
 (list (srcloc-token (token "brz" "brz") (srcloc 'string 1 0 1 3))))

(check-equal?
 (lex "bnz")
 (list (srcloc-token (token "bnz" "bnz") (srcloc 'string 1 0 1 3))))

(check-equal?
 (lex "lw")
 (list (srcloc-token (token "lw" "lw") (srcloc 'string 1 0 1 2))))

(check-equal?
 (lex "sw")
 (list (srcloc-token (token "sw" "sw") (srcloc 'string 1 0 1 2))))