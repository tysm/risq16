#lang br

(require brag/support)

(define-lex-abbrev nop "nop")
(define-lex-abbrev op (:or "add" "addu" "sub" "subu" "div"
                           "divu" "mul" "mulu" "and" "or"
                           "xor" "shl" "shr" "seq" "sne"
                           "sgt" "slt" "sge" "sle" "brz"
                           "bnz" "lw" "sw" "input" "output"))

(define-lex-abbrev reg (:or (:seq "r" (char-set "0123456789")) (:seq "r1" (char-set "012345")) (:seq (char-set "sf") "p")))
(define-lex-abbrev int (:seq (:? "-") (:+ (char-set "0123456789"))))

(define risq16-lexer
  (lexer-srcloc
   ["\n" (token 'NEWLINE lexeme)]
   ["[" (token 'OPENBRACKET lexeme)]
   ["]" (token 'CLOSEBRACKET lexeme)]
   [whitespace (token lexeme #:skip? #t)]
   [(:or nop op) (token lexeme lexeme)]
   [reg (token 'REG lexeme)]
   [int (token 'INT (string->number lexeme))]))

(provide risq16-lexer)