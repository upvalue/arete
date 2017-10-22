(define MyBox (register-record-type "box" 1 0 '(field) #f))

(define box-instance (make-record MyBox))

(record-set! MyBox box-instance 0 "success")

(print (record-ref MyBox box-instance 0))

#|
(set-record-type-printer! MyBox
  (lambda (b)
    "success"))

(print box-instance)
|#

;(set-record-type-apply! MyBox (lambda (b) "success"))

;(print (box-instance))

(define MyBox2 (register-record-type "box2" 1 0 '(field) MyBox))

(define box2-instance (make-record MyBox2))

(if (record-isa? box2-instance MyBox) (print "success"))

(record-set! MyBox2 box2-instance 0 "success")
(record-set! MyBox2 box2-instance 1 "success")

(print (record-ref MyBox2 box2-instance 0))
(print (record-ref MyBox2 box2-instance 1))

(if (eq? (record-isa? box-instance MyBox2) #f) (print "success"))

(if (record-isa? box2-instance MyBox2) (print "success"))

;(print (record-isa? box2-instance MyBox))

;(record-ref MyBox box2-instance 0)
