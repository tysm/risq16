#lang br/quicklang

(struct branch-labelname-signal (val))
(struct branch-index-signal (val))
(struct call-labelname-signal (val))
(struct call-index-signal (val))

(define (boolean->integer boolean) (if boolean 1 0))

(define (zip l1 l2)
  (if (or (null? l1) (null? l2))
      '()
      (cons (list (car l1) (car l2))
            (zip  (cdr l1) (cdr l2)))))

(define (unpack-pairs list-of-pairs)
  (if (null? list-of-pairs)
      '()
      (cons
       (first (car list-of-pairs))
       (cons
        (second (car list-of-pairs))
        (unpack-pairs (cdr list-of-pairs))))))

(define-macro (extract-label (risq-line LINE-DATA ...))
  (pattern-case #'(LINE-DATA ...)
                [((risq-label-def LABELNAME) REST ...) #'(list (symbol->string (quote LABELNAME)))]
                [_ #'(list "")]))

(define-macro (build-label-list LINE ...)
  #'(let* ([label-list-noindex (append (extract-label LINE) ...)]
           [label-list (zip label-list-noindex (range 0 (length label-list-noindex)))])
      (unpack-pairs
       (filter
        (lambda (label-pair)
         (non-empty-string? (first label-pair)))
        label-list))))

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
       (define label-table (apply hash (build-label-list LINE ...)))
       (define line-func-list (build-lambda-list LINE ...))
       (void (run line-func-list label-table))))

(provide (rename-out [risq-module-begin #%module-begin]))

(provide run)
(define (run line-func-list label-table)
  (define line-func-vec (list->vector line-func-list))
  (for/fold ([line-idx 0]
             [mem (make-vector 65535 0)]
             [regs (vector 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 65535)]
             #:result (void))
            ([i (in-naturals)]
             #:break (>= line-idx (vector-length line-func-vec)))
    (define line-func (vector-ref line-func-vec line-idx))
    (with-handlers
        ([branch-labelname-signal? (lambda (signal)
                           (define label-name (branch-labelname-signal-val signal))
                           (if (hash-has-key? label-table label-name)
                               (values (hash-ref label-table label-name) mem regs)
                               (error (format "error in line ~a: label ~a not found" (add1 line-idx) label-name))))]
         [branch-index-signal? (lambda (signal)
                           (define new-line-idx (branch-index-signal-val signal))
                           (if (< new-line-idx (vector-length line-func-vec))
                               (values new-line-idx mem regs)
                               (error (format "error in line ~a: line ~a not found" (add1 line-idx) new-line-idx))))]
         [call-labelname-signal? (lambda (signal)
                           (define label-name (call-labelname-signal-val signal))
                           (if (hash-has-key? label-table label-name)
                               (apply values (hash-ref label-table label-name) (store-into-stack mem regs line-idx))
                               (error (format "error in line ~a: label ~a not found" (add1 line-idx) label-name))))]
         [call-index-signal? (lambda (signal)
                               (define new-line-idx (call-index-signal-val signal))
                               (if (< new-line-idx (vector-length line-func-vec))
                                   (apply values new-line-idx (store-into-stack mem regs line-idx))
                                   (error (format "error in line ~a: line ~a not found" (add1 line-idx) new-line-idx))))])
        (apply values (add1 line-idx) (line-func mem regs)))))


(provide risq-nop)
(define-macro (risq-nop) #'(lambda (mem regs) (list mem regs)))

(provide risq-add)
(define-macro (risq-add (risq-reg DST-REG-ID) SRC)
  #'(lambda (mem regs)
      (update-reg mem regs DST-REG-ID
                  (+
                   (read-reg mem regs DST-REG-ID)
                   ((operand-reader SRC) mem regs)))))

(provide risq-sub)
(define-macro (risq-sub (risq-reg DST-REG-ID) SRC)
  #'(lambda (mem regs)
      (update-reg mem regs DST-REG-ID
                  (-
                   (read-reg mem regs DST-REG-ID)
                   ((operand-reader SRC) mem regs)))))

(provide risq-div)
(define-macro (risq-div (risq-reg DST-REG-ID) SRC)
  #'(lambda (mem regs)
      (update-reg mem regs DST-REG-ID
                  (floor
                   (/
                    (read-reg mem regs DST-REG-ID)
                    ((operand-reader SRC) mem regs))))))

(provide risq-mul)
(define-macro (risq-mul (risq-reg DST-REG-ID) SRC)
  #'(lambda (mem regs)
      (update-reg mem regs DST-REG-ID
                  (floor
                   (*
                    (read-reg mem regs DST-REG-ID)
                    ((operand-reader SRC) mem regs))))))

(provide risq-and)
(define-macro (risq-and (risq-reg DST-REG-ID) SRC)
  #'(lambda (mem regs)
      (update-reg mem regs DST-REG-ID
                  (bitwise-and
                   (read-reg mem regs DST-REG-ID)
                   ((operand-reader SRC) mem regs)))))

(provide risq-or)
(define-macro (risq-or (risq-reg DST-REG-ID) SRC)
  #'(lambda (mem regs)
      (update-reg mem regs DST-REG-ID
                  (bitwise-ior
                   (read-reg mem regs DST-REG-ID)
                   ((operand-reader SRC) mem regs)))))

(provide risq-xor)
(define-macro (risq-xor (risq-reg DST-REG-ID) SRC)
  #'(lambda (mem regs)
      (update-reg mem regs DST-REG-ID
                  (bitwise-xor
                   (read-reg mem regs DST-REG-ID)
                   ((operand-reader SRC) mem regs)))))

(provide risq-shl)
(define-macro (risq-shl (risq-reg DST-REG-ID) SRC)
  #'(lambda (mem regs)
      (update-reg mem regs DST-REG-ID
                  (arithmetic-shift
                   (read-reg mem regs DST-REG-ID)
                   ((operand-reader SRC) mem regs)))))

(provide risq-shr)
(define-macro (risq-shr (risq-reg DST-REG-ID) SRC)
  #'(lambda (mem regs)
      (update-reg mem regs DST-REG-ID
                  (arithmetic-shift
                   (read-reg mem regs DST-REG-ID)
                   (- ((operand-reader SRC) mem regs))))))


(provide risq-set)
(define-macro (risq-set (risq-reg DST-REG-ID) SRC)
  #'(lambda (mem regs)
      (update-reg mem regs DST-REG-ID ((operand-reader SRC) mem regs))))

(provide risq-seq)
(define-macro (risq-seq (risq-reg DST-REG-ID) SRC)
  #'(lambda (mem regs)
      (update-reg mem regs DST-REG-ID
                  (boolean->integer
                   (eq?
                    (read-reg mem regs DST-REG-ID)
                    ((operand-reader SRC) mem regs))))))

(provide risq-sne)
(define-macro (risq-sne (risq-reg DST-REG-ID) SRC)
  #'(lambda (mem regs)
      (update-reg mem regs DST-REG-ID
                  (boolean->integer
                   (not (eq?
                         (read-reg mem regs DST-REG-ID)
                         ((operand-reader SRC) mem regs)))))))

(provide risq-sgt)
(define-macro (risq-sgt (risq-reg DST-REG-ID) SRC)
  #'(lambda (mem regs)
      (update-reg mem regs DST-REG-ID
                  (boolean->integer
                   (>
                    (read-reg mem regs DST-REG-ID)
                    ((operand-reader SRC) mem regs))))))

(provide risq-slt)
(define-macro (risq-slt (risq-reg DST-REG-ID) SRC)
  #'(lambda (mem regs)
      (update-reg mem regs DST-REG-ID
                  (boolean->integer
                   (<
                    (read-reg mem regs DST-REG-ID)
                    ((operand-reader SRC) mem regs))))))

(provide risq-sge)
(define-macro (risq-sge (risq-reg DST-REG-ID) SRC)
  #'(lambda (mem regs)
      (update-reg mem regs DST-REG-ID
                  (boolean->integer
                   (>=
                    (read-reg mem regs DST-REG-ID)
                    ((operand-reader SRC) mem regs))))))

(provide risq-sle)
(define-macro (risq-sle (risq-reg DST-REG-ID) SRC)
  #'(lambda (mem regs)
      (update-reg mem regs DST-REG-ID
                  (boolean->integer
                   (<=
                    (read-reg mem regs DST-REG-ID)
                    ((operand-reader SRC) mem regs))))))

(provide risq-brz)
(define-macro (risq-brz (risq-reg REG-ID) (risq-label LABELNAME))
  #'(lambda (mem regs)
      (define label-name (symbol->string (quote LABELNAME)))
      (if (eq? (read-reg mem regs REG-ID) 0)
          (raise (branch-labelname-signal label-name))
          (list mem regs))))

(provide risq-bnz)
(define-macro (risq-bnz (risq-reg REG-ID) (risq-label LABELNAME))
  #'(lambda (mem regs)
      (define label-name (symbol->string (quote LABELNAME)))
      (if (not (eq? (read-reg mem regs REG-ID) 0))
          (raise (branch-labelname-signal label-name))
          (list mem regs))))

(provide risq-b)
(define-macro (risq-b (risq-label LABELNAME))
  #'(lambda (mem regs)
      (define label-name (symbol->string (quote LABELNAME)))
      (raise (branch-labelname-signal label-name))))

(provide risq-c)
(define-macro (risq-c (risq-label LABELNAME))
  #'(lambda (mem regs)
      (define label-name (symbol->string (quote LABELNAME)))
      (raise (call-labelname-signal label-name))))

(provide risq-br)
(define-macro (risq-br (risq-reg REG-ID))
  #'(lambda (mem regs)
      (raise (branch-index-signal (read-reg mem regs REG-ID)))))

(provide risq-cr)
(define-macro (risq-cr (risq-label LABELNAME))
  #'(lambda (mem regs)
      (raise (call-index-signal (read-reg mem regs REG-ID)))))  


(provide risq-lw)
(define-macro (risq-lw (risq-reg DST-REG-ID) SRC)
  #'(lambda (mem regs)
      (update-reg mem regs DST-REG-ID
                  (read-mem mem regs ((operand-reader SRC) mem regs)))))

(provide risq-sw)
(define-macro (risq-sw (risq-reg DST-REG-ID) SRC)
  #'(lambda (mem regs)
      (update-mem mem regs ((operand-reader SRC) mem regs)
                  (read-reg mem regs DST-REG-ID))))


(provide risq-input)
(define-macro (risq-input (risq-reg DST-REG-ID))
  #'(lambda (mem regs)
      (define input-value (read))
      (update-reg mem regs DST-REG-ID
                  (if (integer? input-value) input-value 0))))

(provide risq-output)
(define-macro (risq-output (risq-reg SRC-REG-ID))
  #'(lambda (mem regs)
      (displayln (read-reg mem regs SRC-REG-ID))
      (list mem regs)))


(provide operand-reader)
(define-macro (operand-reader OPERAND)
  (pattern-case #'OPERAND
                [(risq-int INTVAL) #'(lambda (mem regs) INTVAL)]
                [(risq-reg REGID) #'(lambda (mem regs) (read-reg mem regs REGID))]))


(define (integer->int16 int)
  (bitwise-and int (string->number "1111111111111111" 2)))

(define (int16->integer int16)
  (if (zero? (bitwise-and int16 (string->number "1000000000000000" 2)))
      int16
      (bitwise-ior (arithmetic-shift -1 16) int16)))


(provide read-reg)
(define (read-reg mem regs reg-id) (int16->integer (vector-ref regs reg-id)))

(provide update-reg)
(define (update-reg mem regs reg-id new-value)
  (let ([new-regs (vector-copy regs)])
    (vector-set! new-regs reg-id (integer->int16 new-value))
    (list mem new-regs)))

(provide read-mem)
(define (read-mem mem regs id)
  (int16->integer
   (bitwise-ior
    (arithmetic-shift (vector-ref mem id) 8)
    (vector-ref mem (+ id 1)))))

(provide update-mem)
(define (update-mem mem regs id new-value)
  (let ([new-mem (vector-copy mem)]
        [new-value (integer->int16 new-value)])
    (vector-set! new-mem id (arithmetic-shift new-value -8))
    (vector-set! new-mem (+ id 1) (bitwise-and new-value (string->number "11111111" 2)))
    (list new-mem regs)))

(provide store-into-stack)
(define (store-into-stack mem regs value) (list mem regs)) ; TODO r15 <- r15 - 2; [r15] <- value