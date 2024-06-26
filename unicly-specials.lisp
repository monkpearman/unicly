;;; :FILE-CREATED <Timestamp: #{2011-04-14T12:00:53-04:00Z}#{11154} - by MON>
;;; :FILE unicly/unicly-specials.lisp
;;; ==============================


(in-package #:unicly)

;;; ==============================
;;; :UNICLY-VARIABLES
;;; ==============================

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defvar *random-state-uuid* (make-random-state t))

(defvar *uuid-null-uuid* nil)

;;; ==============================
;; :NOTE Following are bound to their RFC4122 Appendix C. defaults at loadtime in:
;; :FILE unicly-loadtime-bind.lisp

(defparameter *uuid-namespace-dns*
  ;; (make-uuid-from-string "6ba7b810-9dad-11d1-80b4-00c04fd430c8")
  nil)

(defparameter *uuid-namespace-url*
  ;; (make-uuid-from-string "6ba7b811-9dad-11d1-80b4-00c04fd430c8")
  nil)

(defparameter *uuid-namespace-oid*
  ;; (make-uuid-from-string "6ba7b812-9dad-11d1-80b4-00c04fd430c8")
  nil)

(defparameter *uuid-namespace-x500*
  ;; (make-uuid-from-string "6ba7b814-9dad-11d1-80b4-00c04fd430c8")
  nil)
)

;; (defparameter *uuid-v1-imitation-seed* (make-v4-uuid))
;; (uuid-version-uuid *uuid-namespace-dns*) (make-v5-uuid *uuid-namespace-oid*)

;; Why do we defmacro defconstant here?
(defmacro defconst (name value &optional doc)
  `(defconstant ,name (if (boundp ',name) (symbol-value ',name) ,value)
     ,@(when doc (list doc))))

;; (defconst +uuid-null-string+  "00000000-0000-0000-0000-000000000000")
(defconst +uuid-null-string+
  ;; (type-of +uuid-null-string+)   => (SIMPLE-ARRAY CHARACTER (36))
  ;; (constantp +uuid-null-string+) => T
  (make-array 36
              :element-type 'character
              :initial-contents "00000000-0000-0000-0000-000000000000"))

(defvar *uuid-allow-null-like-namespace-args* nil)

(defvar *uuid-allow-empty-string-name-args* nil)

;;; ==============================


;; Local Variables:
;; indent-tabs-mode: nil
;; show-trailing-whitespace: t
;; mode: lisp-interaction
;; package: unicly
;; End:

;;; ==============================
;;; EOF
