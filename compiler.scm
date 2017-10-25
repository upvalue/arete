;; compiler.scm - Arete bytecode compiler

;; DONE Closures (via upvalues)
;; TODO define
;; TODO set!
;; DONE if
;; DONE and/or
;; TODO Check lambda body
;; TODO Error reporting
;; TODO 

;; OpenFn is a record representing a function in the process of being compiled.

;; Note: Internal structure is manipulated by openfn_to_procedure in builtins.cpp and that must be updated
;; if anything is rearranged here

(define-record OpenFn
  ;; A name or description for a function
  name ;; 0
  ;; A vector of instructions (symbols and fixnums)
  insns ;; 1
  ;; A vector of constants
  constants ;; 2
  ;; A vector of source-locations,  
  sources  ;; 3
  ;; Current stack size of the function
  stack-size ;; 4
  ;; Number of local variables in the function
  local-count ;; 5
  ;; Maximum stack size
  stack-max ;; 6
  ;; The function this function was defined in, or #f for toplevel functions
  parent ;; 7
  ;; Environment, table mapping symbols to Var structures
  env ;; 8
  ;; Minimum arguments to function
  min-arity ;; 9
  ;; Maximum arguments to function; ignored if var-arity is #t
  max-arity ;; 10
  ;; #t if function is variable arity
  var-arity ;; 11
  ;; Depth of the function
  depth ;; 12
  ;; If this is a closure, this will be a vector of upvalue locations e.g.
  ;; #(#t 0 #f 0) = capture local variable 0 from the calling function, capture enclosed value 0 from the calling
  ;; functions closure
  closure ;; 13
  ;; Free variable count
  free-variable-count ;; 14
  ;; Vector of free variables
  free-variables ;; 15
  ;; Labels
  labels ;; 16
  ;; #t if this is top-level code and variables are therefore global
  toplevel? ;; 17
  )

(set! OpenFn/make
  (let ((make OpenFn/make))
    (lambda (name)
      (make name (make-vector) (make-vector) (make-vector) 0 0 0 #f (make-table) 0 0 0 0 #f 0 #f (make-table) #f))))

(define-record Var
  idx
  name
  ;; If #t, this is a reference to a free variable
  upvalue?
  ;; If not #f, index in f.fn->upvalues. Mutually exclusive with upvalue?
  free-variable-id)

(set! Var/make
  (let ((make Var/make))
    (lambda (id name)
      (make id name #f #f))))

(define (compiler-log fn . rest)
  (when (or (eq? (top-level-value 'compiler-log) #t))
    (display "arete:cc: ")
    (let loop ((i 0))
      (unless (fx= i (OpenFn/depth fn))
        (display ">")
        (loop (fx+ i 1))))

    (apply pretty-print rest)))

(define (register-constant fn const)
  ;; Extreme optimization: linear search for duplicate constants
  (let ((constants (OpenFn/constants fn)))
    (let loop ((i 0))
      (if (eq? i (vector-length constants))
        (begin
          (vector-append! constants const)
          (fx- (vector-length constants) 1))
        (if (eq? (vector-ref constants i) const)
          i
          (loop (fx+ i 1)))))))

;; This converts symbols into their equivalent fixnums
(define (insn->byte fn insn)
  (cond
    ((fixnum? insn) insn)
    ;; Replace labels with their location
    ((gensym? insn) (table-ref (OpenFn/labels fn) insn))
    (else 
      (case insn
        (push-constant 1)
        (global-get 2)
        (global-set 3)
        (return 4)
        (apply 5)
        (apply-tail 6)
        (local-get 7)
        (local-set 8)
        (upvalue-get 9)
        (upvalue-set 10)
        (close-over 11)
        (jump 12)
        (jump-if-false 13)
        (pop 14)
        (else (raise 'compile "unknown named instruction" (list insn)))))))

;; Adjust the stack size while recalculating the stack max if necessary
(define (fn-adjust-stack fn size)
  (define new-size (fx+ (OpenFn/stack-size fn) size))
  (compiler-log fn "stack +=" size "=" new-size)
  (OpenFn/stack-size! fn new-size)
  (OpenFn/stack-max! fn (max new-size (OpenFn/stack-max fn)))
)

;; emit takes a named instruction and determines the stack effect of it based on its argument
(define (emit fn . insns)
  (fn-adjust-stack fn 
    (case (car insns)
      ;; A note: jump does not actually pop the stack, but because it's always generated after a jump-if-false
      ;; expression and conditionally evaluated, saying it does makes it simpler to check the stack size
      ((jump pop) -1)
      ((words close-over return) 0)
      ((push-constant global-get local-get upvalue-get) 1)
      ((upvalue-set local-set global-set) 2)
      ;; Only pops stack if argument is 1
      ((jump-if-false) (if (fx= (list-ref insns 2) 1) -1 0))
      ;; Remove arguments from stack, but push a single result
      ((apply apply-tail) (fx- (cadr insns)))
      (else (raise 'compile (print-string "unknown instruction" (car insns)) (list fn (car insns) insns)))
    ))

  ;; Stack size sanity check
  (when (fx< (OpenFn/stack-size fn) 0)
    (raise 'compile "stack underflow" (list fn insns (OpenFn/stack-size fn))))

  (for-each
    (lambda (insn) 
      ;; Words isn't a real instruction, just an argument to emit that means we don't need to check
      ;; stack size
      (unless (eq? insn 'words)
        (vector-append! (OpenFn/insns fn) insn)))
    insns))

(define (compile-constant fn x)
  (emit fn 'push-constant (register-constant fn x)))

(define (fn-push-source fn x)
  (aif (list-get-source x)
    (vector-append! (OpenFn/sources fn) it)))

(define (compile-apply fn x tail?)
  (define stack-check #f)
  (define argc (length (cdr x)))

  (compiler-log fn "compiling application" x)
  ;; (print) => OP_GLOBAL_GET 'print OP_APPLY 0
  (compile-expr fn (car x) (and (eq? argc 1) tail?))

  (set! stack-check (OpenFn/stack-size fn))

  (for-each-i
    (lambda (i x)
      (compile-expr fn x (and (eq? argc (fx- i 1)) tail?)))
    (cdr x))

  (unless (eq? (fx- (OpenFn/stack-size fn) argc) stack-check)
    (raise 'compile "stack size does not reflect function arguments" (list fn x)))

  (emit fn (if tail? 'apply-tail 'apply) (length (cdr x)))
  )

;; This is the free-variable handling, it's necessarily somewhat complex

;; It works like this: when a reference to a free-variable is encountered, this free variable is copied into the
;; "closure" vector of each function in the call chain between the function where the variable was defined (and where
;; it is a local variable). This "closure" field is used to generate the close-over instruction, whose arguments tell
;; the virtual machine to save these free variables either from its locals array (if it was where the free variable
;; occurred, or from its closure (and so on up the line).

;; These free variables are saved in the form of Upvalues, heap-allocated values which point at the locals array of a 
;; function until it returns, at which point they are "converted" and function essentially as pointers. 

;; We also have to keep track of the amount of free variables in each function, because a special array is allocated on
;; the stack to store these Upvalues during function execution.
(define (register-free-variable fn x)
  ;; Go up through function stack, adding variable X to environment as necessary
  (unless (OpenFn/closure fn)
    (OpenFn/closure! fn (make-vector)))

  (let ((parent-fn (OpenFn/parent fn))
        (closure (OpenFn/closure fn)))

    (if (OpenFn/toplevel? parent-fn)
      (raise 'compile "register-free-variable reached toplevel somehow" (list parent-fn)))

    (aif (table-ref (OpenFn/env parent-fn) x)
      ;; If this was successful, we've found either a function that has already captured this free variable or the
      ;; function where it was defined
      (let ((var (Var/make 0 x)))
        (compiler-log fn (OpenFn/name fn) "registered free variable" x)
        ;; Calculate index in closure from closure length
        ;(Var/idx! var (fx/ (vector-length closure) 2))
        ;(print closure)

        (Var/idx! var (vector-length closure))
        ;(Var/idx! var (fx/ (vector-length closure) 2))
        ;; This is an upvalue
        (Var/upvalue?! var #t)
        ;; Add to function environment
        (table-set! (OpenFn/env fn) x var)

        #;(when (Var/free-variable-id it)
          (raise 'compile "duplicate free variable" (list it)))


        ;; If this is a free variable and it hasn't been noted as such, do so now
        (unless (or (Var/upvalue? it) (Var/free-variable-id it))
          (OpenFn/free-variable-count! parent-fn (fx+ (OpenFn/free-variable-count parent-fn) 1))
          (Var/free-variable-id! it (fx- (OpenFn/free-variable-count parent-fn) 1)))
        ;; Append to closure
        (vector-append! closure it))
      (register-free-variable parent-fn x))))

(define (fn-lookup fn x)
  (let loop ((search-fn fn))
    (if (eq? search-fn #f)
      (cons 'global x)
      (aif (table-ref (OpenFn/env search-fn) x)
        (if (and (eq? fn search-fn) (not (Var/upvalue? it)))
          (cons 'local (Var/idx it))
          ;; This is an upvalue
          (if (eq? fn search-fn)
            ;; This upvalue has already been added to the closure
            (begin
              (cons 'upvalue (Var/idx it)))
            (begin
              (register-free-variable fn x)
              (fn-lookup fn x))))
        (loop (OpenFn/parent search-fn))))))

(define (compile-identifier fn x)
  (define result (fn-lookup fn x))

  (compiler-log fn "compiling identifier" result)

  (case (car result)
    (local (emit fn 'local-get (cdr result)))
    (global (emit fn 'global-get (register-constant fn x)))
    (upvalue (emit fn 'upvalue-get (cdr result)))
    (else (raise 'compiler ":(" (list x))))
)

;; This function gives us the number of proper arguments to a function
(define (args-length x)
  (if (or (identifier? x) (null? x))
    0
    (let loop ((rest (cdr x))
               (i 1))
      (if (or (null? rest) (not (pair? rest)))
        i
        (loop (cdr rest) (fx+ i 1))))))

(define (compile-lambda fn x)
  ;; Create a new OpenFn with fn as a parent
  (define sub-fn (OpenFn/make (gensym 'lambda)))
  (define args (cadr x))
  (define arg-len (args-length args))
  (define varargs (or (not (list? args)) (identifier? args)))

  (compiler-log sub-fn (OpenFn/name sub-fn))
  (compiler-log fn "parent of " (OpenFn/name sub-fn) " is " (OpenFn/name fn))

  ;; Most of the complexity of this function is just setting up the fields of OpenFn
  (OpenFn/parent! sub-fn fn)
  ;; Note the depth of the function
  (OpenFn/depth! sub-fn (fx+ (OpenFn/depth fn) 1))


  ;; Calculate arity
  (OpenFn/min-arity! sub-fn arg-len)
  (OpenFn/max-arity! sub-fn arg-len)
  (OpenFn/var-arity! sub-fn (or (not (list? args)) (identifier? args)))

  ;; Calculate local count
  (OpenFn/local-count! sub-fn (if (identifier? args) 1 (fx+ arg-len (if (OpenFn/var-arity sub-fn) 1 0))))

  (if (identifier? args)
    (raise 'compile "can't handle varargs" (list x)))

  ;; Here we seed the environment with the lambda's arguments
  (for-each-i
    (lambda (i x)
      (table-set! (OpenFn/env sub-fn) x (Var/make i x))
      #;(OpenFn/local-count! sub-fn (fx+ i 1)))
    args)

  ;; Compile the lambda's body
  (compile sub-fn (cddr x))

  ;(when (top-level-value '*compiler-log*) (pretty-print sub-fn))
  ;; And it's finally done
  (compile-finish sub-fn)

  ;; Now emit an argument to push this function onto the stack
  (emit fn 'push-constant (register-constant fn (OpenFn->procedure sub-fn)))

  ;; Finally, if this function encloses free variables, we emit an instruction
  ;; That will create a closure at runtime out of the compiled function
  (aif (OpenFn/closure sub-fn)
    (begin
      (emit fn 'close-over (vector-length it))
      (let loop ((i 0))
        (unless (eq? i (vector-length it))
          (let ((var (vector-ref it i)))
            (if (Var/upvalue? var)
              (emit fn 'words 1 (Var/idx var))
              (emit fn 'words 0 (Var/free-variable-id var))))
          (loop (fx+ i 1))))))
)

(define (compile-define fn x tail?)
  (define name (cadr x))

  (compile-expr fn (list-ref x 2) tail?)

  (if (OpenFn/toplevel? fn)
    (begin
      (emit fn 'push-constant (register-constant fn name))
      (emit fn 'global-set 0))
    (let ((var (Var/make (OpenFn/local-count fn) name)))
      ;; Register this variable as part of the environment
      (table-set! (OpenFn/env fn) name var)
      (OpenFn/local-count! fn (fx+ (OpenFn/local-count fn) 1))
      (emit fn 'local-set (Var/idx var))))
)

(define (compile-set! fn x tail?)
  (define name (cadr x))
  (define result (fn-lookup fn name))

  (compile-expr fn (list-ref x 2) tail?)

  (case (car result)
    (global
      (begin
        (emit fn 'push-constant (register-constant fn name))
        (emit fn 'global-set 1)))
    (local
      (emit fn 'local-set (cdr result)))
    (upvalue
      (emit fn 'upvalue-set (cdr result))))
)

;; Compiling expressions with conditional evaluation is a little tricky, because we need to jump to a location in the
;; bytecode, but we don't know where it is yet.

;; To deal with this, we emit labels in the form of gensym'd symbols
;; So something like (if #t "true" "false")
;; Might become something like

;; push-constant #t jump-if-false #:label0 push-constant "true" jump #:label1 push-constant "false" #:label0

(define (register-label fn label)
  (let ((labels (OpenFn/labels fn)))
    (compiler-log fn "added label" label "@" (vector-length (OpenFn/insns fn)))
    (table-set! labels label (vector-length (OpenFn/insns fn)))))

(define (compile-if fn x tail?)
  (define condition (cadr x))
  (define then-branch (list-ref x 2))
  (define else-branch (and (fx= (length x) 4) (list-ref x 3)))
  (define then-branch-end (gensym 'if-else))
  (define else-branch-end (gensym 'if-end))

  (compile-expr fn condition #f)

  ;(print 'jump-if-false then-branch-end)
  (emit fn 'jump-if-false then-branch-end 1)
  (compile-expr fn then-branch tail?)
  ;(pretty-print fn)

  (emit fn 'jump else-branch-end)
  (register-label fn then-branch-end)

  (compile-expr fn else-branch tail?)

  (register-label fn else-branch-end))

;; With ands, we generate a push-constant #f at the end of the expression, then a series of jump-if-falses that jump
;; to that push-constant if evaluation fails. The final jump-if-false has an argument of 0, meaning that the successful
;; (true) condition will be left on the stack.

(define (compile-and fn x tail?)
  (define and-fail-pop (gensym 'and-fail-pop))
  (define and-fail (gensym 'and-fail))
  (define and-end (gensym 'and-end))
  (define x-len (length x))
  (define argc (fx- x-len 2))

  ;; (and 1 2 3)
  ;; jump-if-false needs to drop all conditions except for the last one
  ;; then we need a pop instruction

  (if (fx= x-len 1)
    (emit fn 'push-constant (register-constant fn #t))
    (begin
      (for-each-i 
        (lambda (i x)
          ;; Emit a jump-if-false for each expression
          ;; If it's at the expression
          ;(print i argc)
          (compile-expr fn x (and tail? (fx= i argc)))
          (emit fn 'jump-if-false (if (fx= i argc) and-fail-pop and-fail) (if (fx= i argc) 0 1)))
        (cdr x))

      (emit fn 'jump and-end)
      (register-label fn and-fail-pop)

      ;; Manually adjust stack size: and will always result in one value being pushed on the stack after it's 
      ;; evaluated, whether successful or not.
      (OpenFn/stack-size! fn (fx+ (OpenFn/stack-size fn) 1))

      (emit fn 'pop)
      (register-label fn and-fail)

      (emit fn 'push-constant (register-constant fn #f))
      (register-label fn and-end)


    )
  )
)

(define (compile-quote fn x)
  (emit fn 'push-constant (register-constant fn (cadr x))))

(define (compile-begin fn x tail?)
  (define x-len (fx- (length x) 2))
  (for-each-i 
    (lambda (i x)
      (compile-expr fn x (and tail? (fx= i x-len))))
    (cdr x)))

(define (compile-special-form fn x type tail?)
  (compiler-log fn "compiling special form" type x)
  (case type
    (lambda (compile-lambda fn x))
    (define (compile-define fn x tail?))
    (set! (compile-set! fn x tail?))
    (if (compile-if fn x tail?))
    (and (compile-and fn x tail?))
    (or (compile-or fn x tail?))
    (quote (compile-quote fn x))
    (begin (compile-begin fn x tail?))
    (else (raise 'compile "unknown special form" (list x)))))


(define (special-form x)
  (when (rename? x)
    (if (rename-env x) 
      (raise 'compile "compiler encountered non-toplevel rename" (list x)))

    (set! x (rename-expr x)))

  (if (memq x '(lambda define set! if begin and or quote)) x #f))

(define (compile-expr fn x tail?)
  (compiler-log fn "compiling expr" x)
  (cond
    ((self-evaluating? x) (compile-constant fn  x))
    ((identifier? x) (compile-identifier fn  x))
    ((list? x)
     (aif (special-form (car x))
       (compile-special-form fn x it tail?)
       (compile-apply fn x tail?)))
    (else (raise 'compile "don't know how to compile expression" (list x))))
  fn)

(define (compile fn body)
  (define end (fx- (length body) 1))
  (compiler-log fn "compiling body" body)
  (for-each-i
    (lambda (i x)
      (compile-expr fn x (fx= i end)))
    body)

  (emit fn 'return)

  fn)

(define (compile-finish fn)
  (table-for-each 
    (lambda (_ var)
      (when (Var/free-variable-id var)
        (unless (OpenFn/free-variables fn)
          (OpenFn/free-variables! fn (make-vector)))

        (compiler-log fn "has free variable" (Var/name var) " at local idx " (Var/idx var))

        (vector-append! (OpenFn/free-variables fn) (Var/idx var))))
    (OpenFn/env fn))


  (compiler-log fn fn)

  (let loop ((i 0))
    (unless (fx= i (vector-length (OpenFn/insns fn)))
      (vector-set! (OpenFn/insns fn) i (insn->byte fn (vector-ref (OpenFn/insns fn) i)))
      (loop (fx+ i 1)))))

(define (compile-toplevel body)
  (define fn (OpenFn/make 'vm-toplevel))

  (OpenFn/toplevel?! fn #t)

  (compile fn body)
  (compile-finish fn)


  (OpenFn->procedure fn))

