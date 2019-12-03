#lang br/quicklang
(require "parser.rkt" "tokenizer.rkt")

(define (read-syntax path port)
  (define parse-tree (parse path (make-tokenizer port path)))
  (strip-bindings
   #`(module risq16-mod "expander.rkt"
       #,parse-tree)))

(provide read-syntax)
;(module+ reader
;  (provide read-syntax))