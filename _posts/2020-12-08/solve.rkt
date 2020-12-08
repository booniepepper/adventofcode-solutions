#!/usr/bin/env racket
#lang racket

(define the-code
    (make-hash (sequence->list (sequence-map (lambda (e i) (list i (first e) (string->number (second e))))
        (in-indexed (filter-map (lambda (s) (and (non-empty-string? s) (string-split s " ")))                                                   (file->lines "input")))))))

(define (compute-line add acc code seen)
    (if (set-member? seen add)
        (error "Attempted to traverse to already seen address: ~a" add)
        (let* ([seen (set-add seen add)]
               [line (hash-ref code add)]
               [op (first line)]
               [n (second line)])
            (printf "~a: ~a ~a | ~a\n" add op n acc)
            (case op
                [("nop") (compute-line (add1 add) acc       code seen)]
                [("acc") (compute-line (add1 add) (+ acc n) code seen)]
                [("jmp") (compute-line (+ add n)  acc       code seen)]))))

(compute-line 0 0 the-code (set))

