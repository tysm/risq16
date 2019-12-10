#lang br/quicklang

(require "parser.rkt" "tokenizer.rkt")

(define (read-syntax path port)
  (define parse-tree (parse path (make-tokenizer port path)))
  (strip-bindings
   #`(module risq16-mod risq16/expander #,parse-tree)))

(provide read-syntax)
