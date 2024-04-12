;;; :FILE-CREATED <Timestamp: #{2011-08-17T15:58:07-04:00Z}#{11333} - by MON>
;;; :FILE unicly/unicly-byte-arrays.lisp
;;; ==============================


(in-package #:unicly)

(declaim (inline uuid-byte-array-16-zeroed))
(defun uuid-byte-array-16-zeroed ()
  (declare (optimize (speed 3)))
  (the uuid-byte-array-16
    (make-array (the uuid-bit-vector-16-length 16) :element-type 'uuid-ub8 :initial-element 0)))

;; (uuid-get-namespace-bytes  (uuid-princ-to-string (make-v5-uuid *uuid-namespace-dns* "bubba")))
;; (uuid-get-namespace-bytes (make-uuid-from-string "eea1105e-3681-5117-99b6-7b2b5fe1f3c7"))
;; ,----
;; | x86-32 converting 1mil v4 UUIDs pre-cached in an array of 1mil elts in
;; | consistently around 1.5 seconds real time:
;; | 1.489 seconds of real time [ 0.143 seconds GC time, and 1.339 seconds non-GC time. ]
;; | 99.53% CPU 4,454,788,950 processor cycles
;; | 183,996,952 bytes consed
;; `----
(defun uuid-get-namespace-bytes (uuid)
  (declare (type unique-universal-identifier uuid)
           (inline uuid-byte-array-16-zeroed  %unique-universal-identifier-null-p)
           (optimize (speed 3)))
  (when (%unique-universal-identifier-null-p uuid)
    (return-from uuid-get-namespace-bytes (the uuid-byte-array-16 (uuid-byte-array-16-zeroed))))
  (the uuid-byte-array-16
    (with-slots (%uuid_time-low %uuid_time-mid %uuid_time-high-and-version
                                %uuid_clock-seq-and-reserved %uuid_clock-seq-low %uuid_node)
        uuid
      (declare (type uuid-ub32 %uuid_time-low)
               (type uuid-ub16 %uuid_time-mid %uuid_time-high-and-version)
               (type uuid-ub8  %uuid_clock-seq-and-reserved %uuid_clock-seq-low)
               (type uuid-ub48 %uuid_node)
               (inline uuid-disassemble-ub48 uuid-disassemble-ub32 uuid-disassemble-ub16))
      (make-array 16
                  :element-type 'uuid-ub8
                  :initial-contents (multiple-value-call #'list
                                      (the (values uuid-ub8 uuid-ub8 uuid-ub8 uuid-ub8 &optional)
                                        (uuid-disassemble-ub32 %uuid_time-low))
                                      (the (values uuid-ub8 uuid-ub8 &optional)
                                        (uuid-disassemble-ub16 %uuid_time-mid))
                                      (the (values uuid-ub8 uuid-ub8 &optional)
                                        (uuid-disassemble-ub16 %uuid_time-high-and-version))
                                      %uuid_clock-seq-and-reserved
                                      %uuid_clock-seq-low
                                      (the (values uuid-ub8 uuid-ub8 uuid-ub8 uuid-ub8 uuid-ub8 uuid-ub8 &optional)
                                        (uuid-disassemble-ub48 %uuid_node)))))))

;;; ==============================
;; :NOTE UNICLY:UUID-GET-NAMESPACE-BYTES is equivalent to
;; UUID:UUID-TO-BYTE-ARRAY we provide it here for congruence.
;; :SEE Bottom of file for our variation of the original definition.
(eval-when (:load-toplevel :execute)
  (setf (fdefinition 'uuid-to-byte-array)
        (fdefinition 'uuid-get-namespace-bytes)))


;;; ==============================
;; :TODO Finish `uuid-byte-array-version'
;; :SEE ironclad:ub16ref/be for a fetcher to grab only the relevant portion of
;; the `uuid-byte-array-16'.
;; :SEE `uuid-request-integer'
;;  (uuid-request-integer <UUID-BYTE-ARRAY-16> <VERSION-BITS-OFFSET> <VERSION-BITS-LENGTH>)
;;
;; (defun uuid-byte-array-version (uuid-byte-array)
;;   (declare (uuid-byte-array-16 uuid-byte-array))
;;   (let ((version-bits
;;          (uuid-request-integer <UUID-BYTE-ARRAY-16> <VERSION-BITS-OFFSET> <VERSION-BITS-LENGTH>)))
;;      { ... }
;;      ))

;;; ==============================
;; :NOTE Following modelled after `ironclad::octets-to-integer'
;; :SEE :FILE ironclad/src/public-key/public-key.lisp
;; It originally had the following signature:
;; octets-to-integer (octet-vec &key (start 0) end (big-endian t) n-bits)
;; The BIG-ENDIAN key above refers to whether (aref uuid-ba-16 0) represents the LSB or MSB.
;; Objects of type `uuid-byte-array-16' should always have their MSB at 0.
(defun uuid-byte-array-16-to-integer (uuid-ba-16)
  (declare (type uuid-byte-array-16 uuid-ba-16)
           (optimize (speed 3)))
  (uuid-byte-array-16-check-type uuid-ba-16)
  (do ((j 0 (1+ j))
       (sum 0))
      ;; ((>= j end) sum)
      ((>= j 16) sum)
    (setf sum (+ (aref uuid-ba-16 j) (ash sum 8)))))

;; :NOTE Following adapted from `ironclad::integer-to-octets'
;; :SEE :FILE ironclad/src/public-key/public-key.lisp
(defun uuid-integer-128-to-byte-array (uuid-integer)
  (let ((octet-vec (make-array 16 :element-type 'uuid-ub8)))
    (declare (type uuid-byte-array-16 octet-vec))
    (loop
       for i from 15 downto 0
       for index from 0
       ;; do (setf (aref octet-vec index) (ldb (byte 8 (ash i 3)) uuid-integer))
       do (setf (aref octet-vec index) (ldb (byte 8 (* i 8)) uuid-integer))
       finally (return octet-vec))))


;;; ==============================
;; :NOTE the weird loop in the return value of the dotimes form is to accomodate
;; situations where the top bits of the class `unique-universal-identifier' are
;; such that the uuid has an integer representation with `cl:integer-length'
;; less than 120 and we need to pad the array. On current system this will
;; happen for 1 in 200 invocations of `make-v4-uuid's and we end up with
;; something like this:
;; (integer-length 169114161898150076209418180205435926)
;; Following example will illustrate the problem, remove the loop in return of
;; `cl:dotimes' to play:
;;
;; (let ((diff '()))
;;   (dotimes (i 1000 diff)
;;     (let* ((uuid  (make-v4-uuid))
;;            (ba     (uuid-to-byte-array uuid))
;;            (bv-int (uuid-bit-vector-to-integer (uuid-to-bit-vector uuid)))
;;            (int-ba-2 (tt--number-byte-array.2  bv-int))
;;            (inner-diff '()))
;;       (unless (equalp ba int-ba-2)
;;         (push (list :!ba/int-ba-2 ba int-ba-2 (uuid-princ-to-string uuid)) inner-diff))
;;       (unless (null inner-diff)
;;         (push inner-diff diff)))))
;;
;; :NOTE Now using code adapted from ironclad::integer-to-octets instead.
;;
;; (defun uuid-integer-128-to-byte-array (uuid-integer)
;;   (declare (uuid-ub128 uuid-integer)
;;            (optimize (speed 3)))
;;   (when (zerop uuid-integer)
;;     (return-from uuid-integer-128-to-byte-array (uuid-byte-array-16-zeroed)))
;;   (let* ((octet-count (nth-value 0 (truncate (+ (integer-length uuid-integer) 7) 8)))
;;          (bit-count   (ash octet-count 3))
;;          (ba-out      (uuid-byte-array-16-zeroed))
;;          (chk-byte   '()))
;;     (declare (uuid-byte-array-16 ba-out))
;;     (dotimes (cnt 16
;;               (if (evenp octet-count)
;;                   (the uuid-byte-array-16 ba-out)
;;                   (loop
;;                      with offset = ba-out
;;                      with new = (the uuid-byte-array-16 (uuid-byte-array-16-zeroed))
;;                      for x across offset
;;                      for y from 1 below 16
;;                      do (setf (aref new y) x)
;;                      finally (return (the uuid-byte-array-16 new)))))
;;       (setf chk-byte (- bit-count (ash (1+ cnt) 3)))
;;       (if (minusp chk-byte)
;;           (setf (aref ba-out cnt)
;;                 (ldb (byte 8 0) uuid-integer))
;;           (setf (aref ba-out cnt)
;;                 (ldb (byte 8 chk-byte) uuid-integer))))))
;;; ==============================

;;; ==============================


;; Local Variables:
;; indent-tabs-mode: nil
;; show-trailing-whitespace: t
;; mode: lisp-interaction
;; package: unicly
;; End:


;;; ==============================
;;; EOF
