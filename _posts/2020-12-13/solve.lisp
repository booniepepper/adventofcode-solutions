#!/usr/bin/env -S sbcl --script

(with-open-file (input "input")
  (let ((timestamp-raw (read-line input))
        (busses-raw (substitute #\SPACE #\, (read-line input))))
    (defvar timestamp (parse-integer timestamp-raw))
    (defvar busses
      (remove 0 (read-from-string
                  (concatenate 'string "(" (substitute #\0 #\x busses-raw) ")"))))))

(defun next-after-timestamp (n)
    (list (- n (mod timestamp n)) n))
(defun find-best-next (busses)
  (reduce (lambda (a b) (if (< (car b) (car a)) b a))
          (map 'list #'next-after-timestamp busses)))
(defun find-weird-thing (delay-bus)
  (let ((delay (car delay-bus))
        (bus-id (cadr delay-bus)))
    (* bus-id delay)))

;;; Part 1 solution
(format t "~a~%" (find-weird-thing (find-best-next busses)))

