#lang br/quicklang

(define (zip l1 l2)
  (if (or (null? l1) (null? l2))
      '()
      (cons (list (car l1) (car l2))
            (zip  (cdr l1) (cdr l2)))))

(define-macro (extract-label (risq-line LINE-DATA ...)); #'(list 1))
  (pattern-case #'(LINE-DATA ...)
                [((risq-label-def LABELNAME) REST ...) #'(list (symbol->string (quote LABELNAME)))]
                [_ #'(list "")]))

(define-macro (build-label-list LINE ...)
  #'(let* ([label-list-noindex (append (extract-label LINE) ...)]
           [label-list (zip label-list-noindex (range 0 (length label-list-noindex)))])
      (filter
       (lambda (label-pair)
         (non-empty-string? (first label-pair)))
       label-list)))

(define-macro (extract-lambda (risq-line LINE-DATA ...))
  (pattern-case #'(LINE-DATA ...)
                [((risq-label-def LABELNAME)) #'(risq-nop)]
                [((risq-label-def LABELNAME) OPERATION) #'OPERATION]
                [(OPERATION) #'OPERATION]
                [() #'(risq-nop)]))

(define-macro (build-lambda-list LINE ...)
  #'(list (extract-lambda LINE) ...))

(define-macro (risq-module-begin (risq-program LINE ...))
    #'(#%module-begin
       (define label-table (apply hasheqv (build-label-list LINE ...)))
       (define line-func-list (build-lambda-list LINE ...))
       (display line-func-list)
       (display label-table)))
       ;(void (run line-func-list label-table))))

(provide (rename-out [risq-module-begin #%module-begin]))

(define (run line-func-list label-table) (list 1 2 3))
;(define (run line-func-list label-table)
;  (define line-func-vec (list->vector line-func-list))
;  (for/fold ([line-idx 0])
;            ([i (in-naturals)]
;             #:break (>= line-idx (vector-length line-vec)))
;    (define line-func (vector-ref line-func-list line-idx))
;    (line-func)
;    (add1 line-idx)))
;

(provide risq-nop)
(define-macro (risq-nop) #'(lambda () (void)))

(provide risq-add)
(define-macro (risq-add DST SRC) #'(lambda () (void)))
