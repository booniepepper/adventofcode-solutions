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

;;; Helpers
(defun range (end &key (start 0) (step 1))
  (loop for i from start below end by step
        collect i))
(defun range-inclusive (end &key (start 0) (step 1))
  (range (+ 1 end) :start start :step step))
(defun zip (as bs)
  (map 'list #'list as bs))
(defun enumerate (as)
  (zip (range (length as)) as))

;;; Period hacking.
; I'm not amazing at maths. This is sort of based on my understanding of this:
; https://math.stackexchange.com/questions/2218763/how-to-find-lcm-of-two-numbers-when-one-starts-with-an-offset#2218777
(defun find-period (a-pair b-pair)
  (let ((a (car a-pair))
        (step (cadr a-pair))
        (b (cadr b-pair)) 
        (offset (car b-pair)))
    (loop for i fixnum from a by step
          when (= 0 (mod (+ i offset) b)) return (list i (lcm step b)))))

(defun find-multi-period (busses)
  (let* ((enum-busses (remove-if
                        (lambda (pair) (= 0 (cadr pair)))
                        (enumerate busses)))
         (start (cadar enum-busses))
         (rest (sort (cdr enum-busses) #'> :key #'cadr)))
    (reduce #'find-period rest :initial-value (list start start))))

;;; Part 2 solution
(format t "The time when it all comes together: ~a~%"
        (car (find-multi-period busses)))

