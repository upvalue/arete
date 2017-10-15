(define MyBox (register-record-type "box" 1 0))

(define box-instance (make-record MyBox))

(record-set! MyBox box-instance 0 "success")

(print (record-ref MyBox box-instance 0))

(set-record-type-printer! MyBox
  (lambda (b)
    "success"))

(print box-instance)

(set-record-type-applicator! MyBox (lambda (b) "success"))

(print (box-instance))
