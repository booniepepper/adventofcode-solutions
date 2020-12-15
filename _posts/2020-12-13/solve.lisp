#!/usr/bin/env -S sbcl --script

(with-open-file (input "input")
  (let ((timestamp-raw (read-line input))
        (busses-raw (substitute #\SPACE #\, (read-line input))))
    (defvar timestamp (parse-integer timestamp-raw))
    (defvar busses
      (read-from-string
                  (concatenate 'string "(" (substitute #\0 #\x busses-raw) ")")))))

(defun next-after-timestamp (n)
    (list (- n (mod timestamp n)) n))
(defun find-best-next (busses)
  (reduce (lambda (a b) (if (< (car b) (car a)) b a))
          (map 'list #'next-after-timestamp (remove 0 busses))))

;;; Part 1 solution
(let* ((best-next-buss (find-best-next busses))
       (bus-id (cadr best-next-buss))
       (delay (car best-next-buss))
       (weird-product (* bus-id delay)))
  (format t "Product of bus id (~a) and delay (~a): ~a~%" 
          bus-id delay weird-product))

