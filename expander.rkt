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
       (void (run line-func-list label-table))))

(provide (rename-out [risq-module-begin #%module-begin]))

(provide run)
(define (run line-func-list label-table)
  (define line-func-vec (list->vector line-func-list))
  (for/fold ([line-idx 0] [mem (make-vector 4 0)] [regs (make-vector 16 0)]
                          #:result (void))
            ([i (in-naturals)]
             #:break (>= line-idx (vector-length line-func-vec)))
    (define line-func (vector-ref line-func-vec line-idx))
    (apply values (add1 line-idx) (line-func mem regs))))


(provide risq-nop)
(define-macro (risq-nop) #'(lambda (mem regs) (list mem regs)))

(provide risq-add)
(define-macro (risq-add (risq-reg DST-REG-ID) SRC)
  #'(lambda (mem regs)
      (update-reg mem regs DST-REG-ID
                  (+
                   (read-reg mem regs DST-REG-ID)
                   ((operand-reader SRC) mem regs)))))

(provide risq-output)
(define-macro (risq-output (risq-reg SRC-REG-ID))
  #'(lambda (mem regs)
      (displayln (read-reg mem regs SRC-REG-ID))
      (list mem regs)))

(provide operand-reader)
(define-macro (operand-reader OPERAND)
  (pattern-case #'OPERAND
                [(risq-int INTVAL) #'(lambda (mem regs) INTVAL)]
                [(risq-reg REGID) #'(lambda (mem regs) (read-reg reg REGID))]))

(provide read-reg)
(define (read-reg mem regs reg-id) (vector-ref regs reg-id))

(provide update-reg)
(define (update-reg mem regs reg-id new-value)
  (let ([new-regs (vector-copy regs)])
    (vector-set! new-regs reg-id new-value)
    (list mem new-regs)))
