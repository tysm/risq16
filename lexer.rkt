#lang br

(require brag/support)

(define-lex-abbrev op (:or "nop" "add" "addu" "sub" "subu" "div"
                           "divu" "mul" "mulu" "and" "or"
                           "xor" "shl" "shr" "set" "seq" "sne"
                           "sgt" "slt" "sge" "sle" "brz"
                           "b" "br" "c" "cr"
                           "bnz" "lw" "sw" "input" "output" "outputu"))

(define-lex-abbrev reg (:or (:seq "r" (char-set "0123456789")) (:seq "r1" (char-set "012345")) "sp"))
(define-lex-abbrev int (:seq (:? "-") (:+ (char-set "0123456789"))))

(define risq16-lexer
  (lexer-srcloc
   ["\n" (token 'NEWLINE lexeme)]
   [whitespace (token lexeme #:skip? #t)]
   [(from/to ";" "\n") (token lexeme #:skip? #t)]
   ["[" (token 'OPENBRACKET lexeme)]
   ["]" (token 'CLOSEBRACKET lexeme)]
   [":" (token 'COLON lexeme)]
   [op (token lexeme lexeme)]
   [reg (token 'REG (if (equal? "sp" lexeme) 15
                        (string->number (substring lexeme 1))))]
   [(:seq alphabetic (:* (:or alphabetic numeric "_")))
    (token 'ID (string->symbol lexeme))]
   [int (let ([value (string->number lexeme)])
          (if (and (>= value -32768) (<= value 65535)) (token 'INT value)
              (error (format "error in line ~a: int ~a does not fit in 16 bits" "TODO line-idx" value))))]))

(provide risq16-lexer)