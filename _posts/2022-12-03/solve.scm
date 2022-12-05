#!/usr/bin/env -S csi -script

(import (chicken file posix) (chicken io) (chicken string))

(define the-lines
    (read-lines (open-input-file* (file-open "input" (+ open/rdonly)))))

(define (score-of-char c)
    (define alpha "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ")
    (define (go i)
        (if (substring=? c alpha 0 i 1)
            (+ i 1)
            (go (+ i 1))))
    (go 0))

(define (find-common-char line)
    (define halves (string-chop line (/ (string-length line) 2)))
    (define a (car halves))
    (define b (cadr halves))
    (define (go i j)
        (if (substring=? a b i j 1)
            (substring a i (+ i 1))
            (if (< j (string-length b))
                (go i (+ j 1))
                (go (+ i 1) 0))))
    (score-of-char (go 0 0)))

(print (foldl + 0 (map find-common-char the-lines)))
