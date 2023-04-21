(in-package #:sdf/base)

(defvar *check* NIL) ;; enable expensive consistency checks

(define-condition sdf-break (error)
  ())

;; like BREAK, but signals an error so it can be ignored from callers
(defun ebreak (datum &rest arguments)
  (with-simple-restart (continue "Continue from ebreak")
    (apply #'error datum arguments)))

