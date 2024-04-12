;;; :FILE-CREATED <Timestamp: #{2011-04-15T21:28:06-04:00Z}#{11155} - by MON>
;;; :FILE unicly/unicly-timings.lisp
;;; ==============================


;;; ==============================
;; Most of the loops included below are built to readily swap in
;; with the equivalent uuid.lisp function
;;
;; Substitute in the various corresponding operations to see how uuid and unicly
;; compare e.g.:
;;  uuid:uuid-to-byte-array vs. unicly:get-namespace-bytes
;;
;; :TODO Either wrap this into dedicated timing functions or add print
;; statements before each call indicating the nature of the timing performed and
;; its purpose.
;;
;; :TODO Add comparison timings for unicly vs. uuid where applicable.
;;
;;; ==============================

(in-package #:cl-user)
;; (in-package #:unicly)


#| 
 Uncomment the section below to run the timings code following this block:

 (defpackage #:unicly-timings (:use #:common-lisp))

 (in-package #:unicly-timings)

 (defvar *random-chars*  ; works with SBCL / CLISP
   (make-array 282 :element-type 'character :initial-contents
               (loop 
                  for ascii upfrom 33 below 127 ;; (* (- 127 33) 3) =>282 ;
                  for latin upfrom 161
                  for higher-latin upfrom 7680
                  collect (code-char ascii)
                  nconc (list (code-char latin) (code-char higher-latin)))))
 
 ;; :SOURCE PCL Chapter 23 p 305
 (defun nshuffle-vector (vector)
   (declare ((simple-array *) vector))
   (loop for idx downfrom (1- (length vector)) to 1
      for other = (random (1+ idx))
      do (unless (= idx other)
           (rotatef (aref vector idx) (aref vector other))))
   vector)

 (defun make-random-inverted-number-array ()
   (let ((arr (make-array 320))
         (inv-cons '((128  #XFF)
                     (64   #XFFFF)
                     (48   #XFFFFFF)
                     (32   #XFFFFFFFF)
                     (24   #XFFFFFFFFFFFF)
                     (16   #XFFFFFFFFFFFFFFFF)
                     (8    #XFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF))))
     (loop 
        with idx = -1  
        for (a  b) in inv-cons
        do (loop
              repeat a
              for x = (random b)
             ;;collect x)))             ;
              do  (setf (aref arr (incf idx)) x))
        finally (return (nshuffle-vector arr)))))

 (defun make-random-char-array (n)
   (loop 
      with rnd-array = (make-array n :element-type 'character :initial-element #\Nul)
      with rnd-char-bnd = (1- (length *random-chars*))
      for rnd-char = (aref *random-chars* (random rnd-char-bnd))
      for idx upfrom 0 below n
      do (setf (aref rnd-array idx) rnd-char)
      finally (return  rnd-array)))

 (defun make-random-string (&optional str-len)
   (declare ((or null (integer 0 *)) str-len))
   (loop 
      with randlen = (or (and str-len (not (zerop str-len)) str-len)
                         (loop 
                            for i = (random 16)
                            until (plusp i)
                            finally (return i)))
      with str = (make-array randlen :element-type 'character :initial-element #\Nul)
      with rnd-str = (make-random-char-array randlen)
      for put from 0 below randlen
      do (setf (aref str put) (aref rnd-str put))
      finally (return str)))

 (defun make-ascii-alphabet-for-random-string ()
   (loop 
      for x from 97 below 123
      for y from 65 below 91 
      nconc (list (code-char x) (code-char y)) into alpha
      finally (return (make-array 52 :element-type 'character :initial-contents alpha))))

 (defvar *ascii-chars-for-random* (make-ascii-alphabet-for-random-string))

 (defun make-random-length-ascii-string ()
   (declare (special *ascii-chars-for-random*))
   (let ((ascii-chars *ascii-chars-for-random*))
     (declare ((simple-array character (52)) ascii-chars))
     (loop
        repeat (random 53)
        for x = (schar ascii-chars (random 52)) collect x into rand
        finally (return (make-array (length rand) :element-type 'character :initial-contents rand)))))

 (defun generic-gc ()
   #+sbcl (sb-ext:gc :full t)
   #+clisp (ext:gc))

 (vardoc '*random-chars*
         "An array of 282 characters for use with `make-random-char-array' and `make-random-string'.~%~@
Array contains the ASCII chars in the range 33,127~%~@
The latin-1 chars in the range 161,255~%~@
UTF-8 chars in the range 7680,7774~%~@
:EXAMPLE~%
 \(aref *random-chars* 0\)~%~@
:SEE-ALSO `<XREF>'.~%")

 (fundoc 'make-random-char-array
             "Return an array of length N containing random characters selected from `*random-chars*'.~%~@
:EXAMPLE~%
 \(make-random-char-array 3\)~%
 \(make-random-char-array 8\)~%~@
:SEE-ALSO `make-random-string'.~%")

 (fundoc 'make-random-string
         "Return a string of up to sixteen random characters from the value of `*random-chars*'.~%~@
Optional arg STR-LEN is a positive integer value. When ommitted defaults to 16.~%~@
Strings generated as if by `make-random-char-array' ~%~@
:EXAMPLE~%
 \(loop repeat 3 collect \(make-random-string\)\)~%~@
:SEE-ALSO `<XREF>'.~%")

 (fundoc 'make-random-inverted-number-array
         "Return array of 320 randomly selected integers with a distribution inverted
over the byte size of the most-significant number in the following set:~%~@
 \(128 255\)   
 \(64  65535\)
 \(48  16777215\)
 \(32  4294967295\)
 \(24  281474976710655\)
 \(16  18446744073709551615\)
 \(8   340282366920938463463374607431768211455\)~%~@
IOW, for the range 0,255 select 128 integers at random for the range 0,65535
select 64 integers at random etc.~%~@
No effort is made to guarantee the returned array will not contain duplicated entries.
Return value is shuffled as if by `mon:nshuffle-vector'.~%~@
:EXAMPLE~%~@
 { ... <EXAMPLE> ... } ~%~@
:SEE-ALSO `<XREF>'.~%")


|#


;;; ==============================
;;; :UNICLY-TIMINGS
;;; ==============================
(defconstant +tt--size+
  #+sbcl  #xf4240    ; 1mil
  #+clisp #x2710)   ; 10k

;; (defconstant +tt--size+ 10000)



(defparameter *tt--rnd* (make-array +tt--size+))

;; (array-dimensions *tt--rnd*)

(loop 
   for x from 0 below +tt--size+
   do (setf (aref *tt--rnd* x) (make-random-string 36)))




;; Clisp -- Real time: 24.94456 sec.
;; SBCL  -- 0.197 seconds of real time
(generic-gc)
(time
 (loop 
    for x across *tt--rnd*
    do (unicly::make-v5-uuid unicly::*uuid-namespace-dns* x)))

(generic-gc)
(progn 
  (loop for x from 0 below +tt--size+
     do (setf (aref *tt--rnd* x) 
              (unicly::uuid-to-bit-vector (unicly::make-v5-uuid unicly::*uuid-namespace-dns* (make-random-string 36)))))
  (aref *tt--rnd* 
        (1- +tt--size+)))

;;; ==============================
;; compare uuid-to-bit-vector-eql with cl:equal

(generic-gc)
(time 
 (loop 
    for x from 0 below (1- +tt--size+)
    for y = (abs (lognot x))
    count (unicly::uuid-bit-vector-eql (aref *tt--rnd* x) (aref *tt--rnd* y)) into cnt
    finally (return  cnt)))

(generic-gc)
(time 
 (loop 
    for x from 0 below (1- +tt--size+)
    for y = (abs (lognot x))
    count (equal (aref *tt--rnd* x) (aref *tt--rnd* y)) into cnt
    finally (return  cnt)))

;;; ==============================
;; timing with sxhash of string
(progn 
  (setq  *tt--rnd* (make-array +tt--size+ :element-type 'string ))
  (aref *tt--rnd* (1- +tt--size+)))

(loop 
   for x from 0 below +tt--size+ 
   do (setf (aref *tt--rnd* x) 
            (unicly::uuid-print-bytes nil (unicly::make-v5-uuid unicly::*uuid-namespace-dns* (make-random-string 36))))
   finally (return (aref *tt--rnd* (1- +tt--size+))))

(generic-gc)
(time (loop for x across *tt--rnd* do (sxhash x)))

;;; ==============================
;;; timing with sxhash of bit-vector
(defparameter *tt--rnd2* (make-array +tt--size+))

(loop
   for x from 0 below +tt--size+
   do (setf (aref *tt--rnd2* x)
            (unicly::uuid-to-bit-vector (unicly::make-v5-uuid unicly::*uuid-namespace-dns* (make-random-string 36))))
   finally (return (aref *tt--rnd2* (1- +tt--size+))))

(generic-gc)
(time (loop for x across *tt--rnd2* do (sxhash x)))

;; (setq *tt--rnd2* nil)

;;; ==============================
;; timing make-v5-uuid
(generic-gc)
(time
 (loop 
    for x from 0 below +tt--size+
    do (unicly::make-v5-uuid unicly::*uuid-namespace-dns* (aref *tt--rnd* x))))

;; timing uuid-get-namespace-bytes/make-v5-uuid
(generic-gc)
(time
 (dotimes (i +tt--size+) 
   (unicly::uuid-get-namespace-bytes (unicly::make-v5-uuid unicly::*uuid-namespace-dns* (aref *tt--rnd* i)))))

;; timing uuid-get-namespace-bytes/make-v5-uuid

(generic-gc)
(time
 (dotimes (i +tt--size+) 
   (unicly::uuid-get-namespace-bytes (unicly::make-v3-uuid unicly::*uuid-namespace-dns* (aref *tt--rnd* i)))))

;;; ==============================
;; The timing differences between `make-v5-uuid'/`make-v3-uuid' are negligible but MD5 allocates 1/3 less
;; timing 1mil make-v5-uuid

(generic-gc)
(time
 (dotimes (i +tt--size+) 
   (unicly::make-v5-uuid unicly::*uuid-namespace-dns* (aref *tt--rnd* i))))
;;
;; timing 1mil make-v3-uuid

(generic-gc)
(time
 (dotimes (i +tt--size+) 
   (unicly::make-v3-uuid unicly::*uuid-namespace-dns* (aref *tt--rnd* i))))

;;; ==============================
;; There is little additional overhead associated with the uuid-to-bit-vector
;; conversion once the UUID is allocated.
;; timing 1mil uuid-to-bit-vector/make-v5-uuid

(generic-gc)
(time
 (dotimes (i +tt--size+) 
   (unicly::uuid-to-bit-vector (unicly::make-v5-uuid unicly::*uuid-namespace-dns* (aref *tt--rnd* i)))))

;; timing uuid-to-bit-vector/make-v3-uuid
(generic-gc)
(time
 (dotimes (i +tt--size+) 
   (unicly::uuid-to-bit-vector (unicly::make-v3-uuid unicly::*uuid-namespace-dns* (aref *tt--rnd* i)))))

;; same without an aref lookup
(generic-gc)
(time
 (dotimes (i +tt--size+) 
   (unicly::uuid-get-namespace-bytes (unicly::make-v5-uuid unicly::*uuid-namespace-dns* (make-random-string 36)))))

(generic-gc)
(time
 (dotimes (i +tt--size+) 
   (unicly::uuid-to-bit-vector (unicly::make-v5-uuid unicly::*uuid-namespace-dns* (make-random-string 36)))))

;;; ==============================
;;; EOF
