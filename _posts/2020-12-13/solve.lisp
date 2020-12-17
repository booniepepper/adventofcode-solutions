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

(defun range (end &key (start 0) (step 1))
  (loop for i from start below end by step
        collect i))
(defun range-inclusive (end &key (start 0) (step 1))
  (range (+ 1 end) :start start :step step))
(defun zip (as bs)
  (map 'list #'list as bs))
(defun enumerate (as)
  (zip (range (length as)) as))

;;; Early solution
(defun perfect-offset? (a bs)
  (reduce (lambda (a? b)
            ;(format t "comparing: a?: ~a, b: ~a~%" a? b)
            (if (or (= 0 (cadr b))
                    (and a?
                         (= 0 (mod (+ a? (car b)) (cadr b)))))
              a?
              nil))
          bs
          :initial-value a))
(defun find-perfect-offset (as)
  (loop for i fixnum from 0 by (cadadr as)
        ;do (format t "~a: ~a~%" i (perfect-offset? i as))
        when (perfect-offset? i as) collect i
        until (perfect-offset? i as)))

(defun find-perfect-offset (as)
  (loop for i fixnum from 0 by (cadar as)
        ;do (format t "~a: ~a~%" i (perfect-offset? i as))
        when (perfect-offset? i as) collect i
        until (perfect-offset? i as)))

;;; Second attempt
(format t "lcm: ~a~%" (reduce #'lcm (remove 0 (map 'list #'cadr (enumerate busses)))))
(defun find-perfect-offset-v2 (busses)
  (let* ((enum-busses (remove-if (lambda (pair) (= 0 (cadr pair))) (enumerate busses)))
         (max-pair (reduce
                     (lambda (a-pair b-pair)
                       (if (> (cadr b-pair) (cadr a-pair))
                         b-pair
                         a-pair))
                     enum-busses
                     :initial-value (list -1 -1)))
         (start (- (reduce #'lcm (remove 0 busses)) (car max-pair)))
         (step (cadr max-pair)))
    (format t "start: ~a, step: ~a, enum-busses: ~a~%" start step enum-busses)
    (loop for i fixnum downfrom start by step
          ;do (format t "i: ~a~%" i)
          when (perfect-offset? i enum-busses) collect i
          until (perfect-offset? i enum-busses))))

;;; TODO: Learn. There's something intuitive about GCD/LCM here but
;;; I couldn't conjure it up. Take some time to understand this:
;;; https://math.stackexchange.com/questions/2218763/how-to-find-lcm-of-two-numbers-when-one-starts-with-an-offset#2218777

;;; Period hacking
(defun find-period (a-pair b-pair)
  (let ((a (car a-pair))
        (step (cadr a-pair))
        (b (cadr b-pair))
        (offset (car b-pair)))
    (loop for i fixnum from a by step
          do (format t "(b: ~a, offset: ~a) i: ~a (i+offset: ~a, (i+offset)%b: ~a)~%" b offset i (+ i offset) (mod (+ i offset) b))
          when (= 0 (mod (+ i offset) b)) return (list i (lcm step b)))))


(format t "Period hacking (expect (18 45)): ~a~%" (find-period '(9 9) '(-3 15)))
(format t "Period hacking (expect (120 570)): ~a~%" (find-period '(30 30) '(-6 38)))

(defvar enum-busses (remove-if
               (lambda (pair) (= 0 (cadr pair)))
               (enumerate busses)))
(format t "Period hacking: ~a~%" (find-period (list (cadar enum-busses) (cadar enum-busses)) (cadr enum-busses)))

; broken (format t "SERIOUS Period hacking: ~a~%" (reduce #'find-period (cdr enum-busses) :initial-value (cadar enum-busses)))

(defun find-perfect-offset-v3 (busses)
  (let* ((enum-busses (remove-if
                          (lambda (pair) (= 0 (cadr pair)))
                          (enumerate busses)))
         (start (cadar enum-busses))
         (rest (sort (cdr enum-busses) #'> :key #'cadr)))
    (format t "busses: ~a, enum-busses: ~a, rest: ~a~%" busses enum-busses rest)
    (reduce #'find-period rest :initial-value (list start start))))
;(format t "SERIOUS EXTRA Period hacking: ~a~%" (find-perfect-offset-v3 busses))

;;; Part 2 (still hacking on it)
(format t "~a~%"
        (find-perfect-offset (enumerate '(17 0 13 19))))
(format t "~a~%"
        (find-perfect-offset-v2 '(17 0 13 19)))
(format t "~a~%"
        (find-perfect-offset-v3 '(17 0 13 19)))

(format t "~a~%"
        (find-perfect-offset (enumerate '(67 7 59 61))))
(format t "~a~%"
        (find-perfect-offset-v2 '(67 7 59 61)))
(format t "~a~%"
        (find-perfect-offset-v3 '(67 7 59 61)))

(format t "~a~%"
        (find-perfect-offset (enumerate '(67 0 7 59 61))))
(format t "~a~%"
        (find-perfect-offset-v2 '(67 0 7 59 61)))
(format t "~a~%"
        (find-perfect-offset-v3 '(67 0 7 59 61)))

(format t "~a~%"
        (find-perfect-offset (enumerate '(67 7 0 59 61))))
(format t "~a~%"
        (find-perfect-offset-v2 '(67 7 0 59 61)))
(format t "~a~%"
        (find-perfect-offset-v3 '(67 7 0 59 61)))

(format t "~a~%"
        (find-perfect-offset (enumerate '(1789 37 47 1889))))
(format t "~a~%"
        (find-perfect-offset-v2 '(1789 37 47 1889)))
(format t "~a~%"
        (find-perfect-offset-v3 '(1789 37 47 1889)))

(format t "~a~%"
        (find-perfect-offset-v3 busses))

