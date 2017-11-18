;; compiler.scm - Arete bytecode compiler

;; If you have a function with ten parameters, you probably missed a few.

;; - Alan Perlis.

;; DONE Closures (via upvalues)
;; DONE define
;; DONE set!
;; DONE if
;; DONE and/or
;; TODO Check lambda body
;; TODO Proper error reporting

;; TODO. All we need to implement display closures would be a simple analysis pass to check set! on variables.
;; This would remove a pointer dereference for variables which are not set!, which might actually be a decent boost
;; given the naive way certain expressions (e.g. (let loop ())) introduce free variables

;; TODO: Optimization

;; TODO: Figure out how to get more detailed about function names and preserve them in stack traces.
;; eg (define (myfn) (let loop () #t))
;; an error in let-loop should give more information than it does.

;; Compiler parameters

;; If #t, will generate opcodes in place of functions like +, -, etc.

;; Most basic inlining
;; ((lambda (x) x) 5)
;; How do we do this simply? We have to gensym x somehow. There has to be an analysis pass that generates information
;; about variables, whether they are set! or not.

;; ((lambda (x) x) #t)
;; ((lambda (<Binding original: 'x name: '#:x0 mutated: #f>) x) #t)
;; ((lambda (<Binding original: 'x name: '#:x0 mutated: #f>) '#:x0) #t)

;; Then fn-lookup will have to use this new Binding struct.

;; lookup X and replace with NAME

;; We could do a simple analysis pass like over code, replacing introduced bindings (in define, and in lambda args)
;; with a binding structure like this. Without building a full AST or intermediate language

;; This would enable (1) display closures and (2) simple inlining, because each binding will now have a unique gensym'd
;; name and knowledge about mutation.

;; Really, we'd just like to inline lambda in the CAR position. That should provide a good speedup by optimizing let
;; and let* expressions.

;; (let loop () #t)
;; Can this be optimized as well? Right now it expands to (lambda () (define loop (...)) (lambda () body)....
;; We could transform it to something with OP_LOCAL_SET 0 to a function's own adress.

(set-top-level-value! 'COMPILER-VM-PRIMITIVES #t)

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
  ;; function body after analysis pass
  body ;; 18
  )

(define %OpenFn/make OpenFn/make)

(set! OpenFn/make
  (lambda (name)
    (%OpenFn/make name (make-vector) (make-vector) (make-vector) 0 0 0 #f (make-table) 0 0 0 0 #f 0 #f (make-table)
                  #f #f)))

;; Although this is called Var, it might be more proper to think of it as a Binding that can be propagated through the
;; call stack

;; For example, in the expression (lambda (a) (lambda () (lambda () a)))
;; When A is accessed from the third lambda expression, a Var will be added to it and the one above marking it as an
;; upvalue

(define-record Var
  idx
  name
  ;; If #t, this is a reference to a free variable
  upvalue?
  ;; If not #f, index in f.fn->upvalues. Mutually exclusive with upvalue?
  free-variable-id
  ;; #t if variable is ever set!
  mutated?
  ;; OpenFn/depth the variable was defined at.
  scope
  )

(define %Var/make Var/make)
(set! Var/make
  (lambda (id name) (%Var/make id name #f #f #f #f)))

(define AVar/make
  (lambda (id name fn)
    (%Var/make id name #f #f #f (OpenFn/depth fn))))

(define (compiler-log fn . rest)
  (when (not (eq? (top-level-value 'COMPILER-LOG) unspecified))
    (display "arete:cc:")
    (let loop ((i 0))
      (unless (fx= i (OpenFn/depth fn))
        (display ">")
        (loop (fx+ i 1))))

    (display " ")

    (apply pretty-print rest)))

;; This function registers the source of a particular expression, by creating a vector with the offset of its code
;; and its source code information (see SourceLocation in arete.hpp).
(define (register-source fn cell)
  (aif (list-get-source cell)
    (begin
      (vector-append! (OpenFn/sources fn) (vector-length (OpenFn/insns fn)))
      (let loop ((i 0)
                 (sources (OpenFn/sources fn)))
        (if (fx= i (vector-length it))
          #t
          (begin
            (vector-append! sources (vector-ref it i))
            (loop (fx+ i 1) sources)))))))

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
        ;; See vm.cpp for details
        (push-constant 1)
        (push-immediate 2)
        (pop 3)
        (global-get 4)
        (global-set 5)
        (local-get 6)
        (local-set 7)
        (upvalue-get 8)
        (upvalue-set 9)
        (close-over 10)
        (apply 11)
        (apply-tail 12)
        (return 13)
        (jump 14)
        (jump-if-false 15)
        (jump-if-true 16)

        (+ 17)
        (- 18)
        (< 19)
        (car 20)
        (list-ref 21)
        (eq? 22)

        (else (raise 'compile-internal "unknown named instruction" (list insn)))))))

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
      ((jump pop global-set eq? list-ref) -1)
      ((words close-over return car) 0)
      ((push-immediate push-constant global-get local-get upvalue-get) 1)
      ((upvalue-set local-set) 2) ;; TODO: Omitting the two here causes a horrific expansion error
      ;; Only pops stack if argument is 1
      ((jump-if-false jump-if-true) (if (fx= (list-ref insns 2) 1) -1 0))
      ;; Variable microcode: Remove arguments from stack, and re-use one of the argument slots to push results
      ((+ - <) (fx+ (fx- (cadr insns)) 1))
      ;; Remove arguments from stack, but push a single result in the place of the function on the stack
      ((apply apply-tail) (fx- (cadr insns)))
      (else (raise 'compile-internal (print-string "unknown instruction" (car insns)) (list fn (car insns) insns)))
    ))

  ;; Stack size sanity check
  (when (fx< (OpenFn/stack-size fn) 0)
    (raise 'compile-internal "stack underflow" (list insns (OpenFn/stack-size fn) fn)))

  (for-each
    (lambda (insn) 
      ;; Words isn't a real instruction, just an argument to emit that means we don't need to check
      ;; stack size
      (unless (eq? insn 'words)
        (vector-append! (OpenFn/insns fn) insn)))
    insns))

(define (compile-constant fn x)
  (if (or (fixnum? x) (boolean? x))
    (emit fn 'push-immediate (value-bits x))
    (emit fn 'push-constant (register-constant fn x)))
)

(define (fn-push-source fn x)
  (aif (list-get-source x)
    (vector-append! (OpenFn/sources fn) it)))

(define (alist->table alist)
  (let ((table (make-table)))
    (let loop ((cell (car alist))
               (rest (cdr alist)))
      (table-set! table (car cell) cell)
      (if (null? rest)
        table
        (loop (car rest) (cdr rest))))))

(define primitive-table
  (alist->table '(
    ;; name min-argc max-argc variable-arity
    (+ 1 1 #t)
    (- 1 1 #t)
    (car 1 1 #f)
    (eq? 2 2 #f)
    (list-ref 2 2 #f)
  ))
)

(define (compile-apply fn x tail?)
  (define stack-check #f)
  (define argc (length (cdr x)))
  (define primitive #f)
  ;; If true, argument count will be emitted after the primitive thing
  (define primitive-args #f)
  ;; strip renames at this point
  (define kar (if (rename? (car x)) (rename-expr (car x)) (car x)))

  (compiler-log fn "compiling application" x)

  ;; Compiling specific opcodes
  ;; +, -, etc
  (aif (and (eq? (top-level-value 'COMPILER-VM-PRIMITIVES) #t)
            (symbol? kar)
            (eq? (car (fn-lookup fn kar x)) 'global)
            (table-ref primitive-table kar))
    (begin
      ;; Check primitive function arguments at compile-time when possible
      (let ((min-argc (list-ref it 1))
            (max-argc (list-ref it 2))
            (var-argc (list-ref it 3))
            (argc (length (cdr x))))
        (when (< argc min-argc)
          (raise-source x 'compile (print-string "function call" (car x) "requires at least" min-argc "arguments but only got" argc) (list x )))

        (when (and (not var-argc) (> argc max-argc))
          (raise-source x 'compile (print-string "function call" (car x) "expects at most" min-argc "arguments but got" argc) (list x )))

        (when var-argc
          (set! primitive-args #t))

      (set! primitive it)))
    (compile-expr fn (car x) x #f))

  (set! stack-check (OpenFn/stack-size fn))

  (for-each-i
    (lambda (i sub-x)
      (compile-expr fn sub-x (list-tail x (fx+ i 1)) #f))
    (cdr x))

  ;; Stack size sanity check
  ;; Except for toplevel functions
  (unless (or (OpenFn/toplevel? fn) (eq? (fx- (OpenFn/stack-size fn) argc) stack-check))
    (raise 'compile-internal (print-string "expected function stack size" (OpenFn/stack-size fn)
                                           "to match 0 + function arguments" stack-check) (list fn x)))


  (if primitive
    (begin
      (if primitive-args
        (emit fn (car primitive) (length (cdr x)))
        (emit fn (car primitive)))
    )
    (emit fn (if tail? 'apply-tail 'apply) (length (cdr x))))
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
      (raise 'compile-internal "register-free-variable reached toplevel somehow" (list parent-fn)))

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
          (Var/free-variable-id! it (OpenFn/free-variable-count parent-fn))
          (OpenFn/free-variable-count! parent-fn (fx+ (OpenFn/free-variable-count parent-fn) 1))
          (unless (OpenFn/free-variables parent-fn)
            (OpenFn/free-variables! parent-fn (make-vector)))

          ;; Append to vector of free variables
          (vector-append! (OpenFn/free-variables parent-fn) (Var/idx it))

          (compiler-log fn "noting free variable" it))
        ;; Append to closure
        (vector-append! closure it))
      (register-free-variable parent-fn x))))

(define (fn-lookup fn x src)
  (let loop ((search-fn fn))
    (if (eq? search-fn #f)
      (cons 'global x)
      (if (OpenFn/toplevel? search-fn)
        (begin
          (unless (or (top-level-bound? x) (table-ref (OpenFn/env search-fn) x))
            (print-source src "Warning: reference to undefined global variable" x))
          (cons 'global x))
        (aif (table-ref (OpenFn/env search-fn) x)
          (if (and (eq? fn search-fn) (not (Var/upvalue? it)))
            (cons 'local (Var/idx it))
            ;; This is an upvalue
            (if (eq? fn search-fn)
              ;; This upvalue has already been added to the closure
              (begin
                (compiler-log fn "found existing upvalue" it)
                (cons 'upvalue (Var/idx it)))
              (begin
                (register-free-variable fn x)
                (fn-lookup fn x src))))
          (loop (OpenFn/parent search-fn)))))))

(define (compile-identifier fn x src)
  (if (rename? x)
    (set! x (rename-expr x)))
  (define result (fn-lookup fn x src))

  (compiler-log fn "compiling identifier" result)

  (case (car result)
    (local (emit fn 'local-get (cdr result)))
    (global (emit fn 'global-get (register-constant fn x)))
    (upvalue (emit fn 'upvalue-get (cdr result)))
    (else (raise 'compile-internal ":(" (list x))))
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

  ;; Check what the value of sub-fn is during a recompiled compile-lambda call
  ; (print sub-fn)

  (compiler-log sub-fn (OpenFn/name sub-fn))

  (when fn
    (compiler-log fn "parent of " (OpenFn/name sub-fn) " is " (OpenFn/name fn)))

  ;; Most of the complexity of this function is just setting up the fields of OpenFn
  (OpenFn/parent! sub-fn fn)

  ;; Note the depth of the function
  
  (OpenFn/depth! sub-fn (if fn (fx+ (OpenFn/depth fn) 1) 0))

  ;; Calculate arity
  (OpenFn/min-arity! sub-fn arg-len)
  (OpenFn/max-arity! sub-fn arg-len)
  (OpenFn/var-arity! sub-fn varargs)

  ;; Calculate argument count
  (OpenFn/local-count! sub-fn (if (identifier? args) 1 (fx+ arg-len (if (OpenFn/var-arity sub-fn) 1 0))))

  (unless (null? args)
    (if (identifier? args)
      ;; (lambda args args)
      (table-set! (OpenFn/env sub-fn) args (Var/make 0 args))

      ;; Handle arguments, including varargs
      (let loop ((rest (cdr args))
                 (item (car args))
                 (i 0))
        (table-set! (OpenFn/env sub-fn) item (Var/make i item))
        (unless (null? rest)

          (if (pair? rest)
            (loop (cdr rest) (car rest) (+ i 1))
            ;; We've hit the varargs
            (begin
              (table-set! (OpenFn/env sub-fn) rest (Var/make (+ i 1) rest))))))))

  ;; Compile the lambda's body
  (compile sub-fn (cddr x))

  ;(pretty-print sub-fn)
  ;(when (top-level-value '*compiler-log*) (pretty-print sub-fn))
  ;; And it's finally done
  (compile-finish sub-fn)

  ;; Now emit an argument to push this function onto the stack
  (when fn
    (let ((procedure (OpenFn->procedure sub-fn)))
      (emit fn 'push-constant (register-constant fn procedure))

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
      procedure))

  sub-fn
)

(define (compile-define fn x tail?)
  (define name (cadr x))
  (define var #f)

  (unless (OpenFn/toplevel? fn)
    (set! var (Var/make (OpenFn/local-count fn) name))
    (table-set! (OpenFn/env fn) name var)
    (OpenFn/local-count! fn (fx+ (OpenFn/local-count fn) 1)))

  (let ((result (compile-expr fn (list-ref x 2) (list-tail x 2) tail?)))
    (when (procedure? result)
      (set-vmfunction-name! result name)))

  (if (OpenFn/toplevel? fn)
    (begin
      (emit fn 'global-set 0 (register-constant fn name)))
    (begin
      (emit fn 'local-set (Var/idx var))))
)

(define (compile-set! fn x src tail?)
  (define name (cadr x))
  (define result (fn-lookup fn name src))

  (compile-expr fn (list-ref x 2) (list-tail x 2) tail?)

  (case (car result)
    (global
      (begin
        (emit fn 'global-set 1 (register-constant fn name))))
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
  (define else-branch (if (fx= (length x) 4) (list-ref x 3)))
  (define then-branch-end (gensym 'if-else))
  (define else-branch-end (gensym 'if-end))

  (compile-expr fn condition (list-tail x 1) #f)

  (emit fn 'jump-if-false then-branch-end 1)
  (compile-expr fn then-branch (list-tail x 2) tail?)

  ;; Something side-effecty happened in a value context so we'll push unspecified on the stack
  ;; e.g. (if #t (set! x #t))
  (if (eq? (OpenFn/stack-size fn) 0)
    (emit fn 'push-immediate (value-bits unspecified)))

  (emit fn 'jump else-branch-end)
  (register-label fn then-branch-end)

  (compile-expr fn else-branch (if (fx= (length x) 4) (list-tail x 3) #f) tail?)

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
    (compile-constant fn #t) 
    (begin
      (for-each-i 
        (lambda (i sub-x)
          ;; Emit a jump-if-false for each expression
          ;; If it's at the expression
          ;(print i argc)
          (compile-expr fn sub-x (list-tail x (fx+ i 1)) (and tail? (fx= i argc)))
          (emit fn 'jump-if-false (if (fx= i argc) and-fail-pop and-fail) (if (fx= i argc) 0 1)))
        (cdr x))

      (emit fn 'jump and-end)
      (register-label fn and-fail-pop)

      ;; Manually adjust stack size: and will always result in one value being pushed on the stack after it's 
      ;; evaluated, whether successful or not.
      (OpenFn/stack-size! fn (fx+ (OpenFn/stack-size fn) 1))

      (emit fn 'pop)
      (register-label fn and-fail)

      (compile-constant fn #f)
      (register-label fn and-end)
    )
  )
)

;; (or a b c)
;; (if a a (if b b (if c c #f)))
(define (compile-or fn x tail?)
  (define x-len (length x))
  (if (fx= x-len 1)
    (compile-constant fn #f)

    (let ((or-success (gensym 'or-success))
          (or-fail-pop (gensym 'or-fail-pop))
          (or-fail (gensym 'or-fail))
          (argc (fx- x-len 2)))
      (for-each-i
        (lambda (i sub-x)
          (compile-expr fn sub-x (list-tail x (fx+ i 1)) (and tail? (fx= i argc)))
          (emit fn 'jump-if-true or-success 0)
          (if (not (fx= i argc)) (emit fn 'pop)))
        (cdr x))

      (emit fn 'jump or-success)
      (register-label fn or-fail-pop)

      (compile-constant fn #f)
      (register-label fn or-success)
    )

  )
)

(define (compile-quote fn x)
  (compile-constant fn (cadr x)))

;; Compiling a begin is fairly simple -- just need to make sure the last expression is considered a tail call.
(define (compile-begin fn x tail?)
  (define x-len (fx- (length x) 2))
  (for-each-i 
    (lambda (i sub-x)
      (compile-expr fn sub-x (list-tail x i) (and tail? (fx= i x-len))))
    (cdr x)))

(define (compile-special-form fn x src type tail?)
  (compiler-log fn "compiling special form" type x)
  (case type
    (lambda (compile-lambda fn x))
    ;; TODO: Define and set cannot have tail-applications currently, because the global-set instructions are
    ;; generated after them.
    ;; So something like (set! var (fn)) should not destroy the stack frame.
    (define (compile-define fn x #f))
    (set! (compile-set! fn x src #f))
    (if (compile-if fn x tail?))
    (and (compile-and fn x tail?))
    (or (compile-or fn x tail?))
    (quote (compile-quote fn x))
    (begin (compile-begin fn x tail?))
    (else (raise 'compile-internal "unknown special form" (list x)))))

(define (special-form x)
  (when (rename? x)
    (if (rename-env x) 
      (raise 'compile-internal "compiler encountered non-toplevel rename" (list x)))

    (set! x (rename-expr x)))

  (if (memq x '(lambda define set! if begin and or quote)) x #f))

(define (compile-expr fn x src tail?)
  (compiler-log fn "compiling expr" x)
  (aif (and src (list-get-source src))
    (begin
      (compiler-log fn "expr source:" it)
      (register-source fn src))
  )
  (cond
    ((self-evaluating? x) (compile-constant fn x))
    ((identifier? x) (compile-identifier fn x src))
    ((list? x)
     (aif (special-form (car x))
       (compile-special-form fn x src it tail?)
       (compile-apply fn x tail?)))
    (else (raise 'compile-internal "don't know how to compile expression" (list x)))))

;; Compiler entry point; compiles a list of expressions and adds them to a function
(define (compile fn body)
  (define end (fx- (length body) 1))
  (compiler-log fn "compiling body" body)
  (for-each-i
    (lambda (i x)
      (register-source fn (list-tail body i))
      ;; No tail calls in toplevel programs, so that something like
      ;; (define x (vm-function)) at REPL will work.
      (compile-expr fn x (list-tail body i) (and (not (OpenFn/toplevel? fn)) (fx= i end)))
      #;(if (OpenFn/toplevel? fn)
        (emit fn 'pop))
    )

    body)

  (emit fn 'return)

  fn)

;; Compiler finisher -- converts symbolic VM instructions into actual fixnums
(define (compile-finish fn)
  (compiler-log fn fn)

  (let loop ((i 0))
    (unless (fx= i (vector-length (OpenFn/insns fn)))
      (vector-set! (OpenFn/insns fn) i (insn->byte fn (vector-ref (OpenFn/insns fn) i)))
      (loop (fx+ i 1)))))

(define (scan-defines fn body)
  (for-each
    (lambda (x)
      ;; It is possible for #<unspecified> 
      ;; to occur in toplevel bodies because it is returned by the expander.
      (unless (eq? x unspecified)
        (if (and (pair? x) (env-compare #f (car x) 'define))
          (let ((var (Var/make 0 (cadr x))))
            (compiler-log fn "registering global variable" (cadr x))
            (table-set! (OpenFn/env fn) (cadr x) var))
          (if (and (pair? x) (env-compare #f (car x) 'begin))
            (scan-defines fn (cdr x))))))
    body))

;; This is where most expressions will enter the compiler, it creates a function on-the-fly and returns it for
;; execution
(define (compile-toplevel body)
  (define fn (OpenFn/make 'vm-toplevel))

  (OpenFn/toplevel?! fn #t)

  (scan-defines fn body)

  #;(for-each
    (lambda (x)
      ;; It is possible for #<unspecified> 
      ;; to occur in toplevel bodies because it is returned by the expander.
      (unless (eq? x unspecified)
        (if (and (pair? x) (eq? (car x) 'define))
          (let ((var (Var/make 0 (cadr x))))
            (compiler-log fn "registering global variable" (cadr x))
            (table-set! (OpenFn/env fn) (cadr x) var)))))
    body)

  (compile fn body)
  (compile-finish fn)

  ;(print "toplevel stack size" (OpenFn/stack-size fn))

  (let ((result (OpenFn->procedure fn)))
    #;(set-vmfunction-log! result #t)
    result))

;; Analysis pass

;; What this does:

;; ((lambda (a)
;;   ((lambda (b)
;;     (set! a 5)
;;     (+ a b)) 10)) 5)

;; Becomes

;; (#<OpenFn env: (#<Binding name: #:a47 mutated: #t>)
;;   (#<OpenFn env: (#<Binding name: #:b47 mutated: #f>) body:
;;      (set! #<Binding name: #:a47 mutated: #t>))
;;      etc...

;; This allows the compiler to (a) inline these functions without causing a name conflict and (b) generate cheaper
;; OP_CLOSURE_REF instructions in the case that a variable is never mutated.

#|
(define (special-form=? x type)
  (and (pair? x) (rename? (car x)) (eq? (rename-env (car x)) #f) (eq? (rename-expr (car x)) type)))

;; (define a 5)
;; ((lambda (b) (+ a b))
;; Analysis pass does this
;; (#<OpenFn body: (+ #<Binding name: a depth: 0> #<Binding name: b depth: 1>)
;; When this is encountered during the code generation pass in the CAR position
;; We add the bindings from the sub-function to the function, and generate the code directly in that function.

;; A binding of depth 0 is a global, a binding of depth equal to current function is a local...

;; When inlining, we have to adjust all Vars which are shared in the source code, right?

;; Increase local-count by arguments of inlined function.
;; Adjust all variables > current location

;; This is the free-variable handling, it's necessarily somewhat complex

;; ((lambda (a) a) 5)

;; #<Binding name: a idx: 0>

;; (can we just replace function env entirely while compiling the body?)
;; ((lambda (a) a) 5)
;; ((lambda (a) (lambda (b) (+ a b)) 5) 5)

;; ((lambda (a) 5) 5)
;; Inline a function in the CAR. We compile it backwards: evaluate and local-set the arguments, then compile the
;; function body as though it were there.

;; What makes this hard: name conflicts. Our current upvalue handling is also built into the name lookup process.

;; Considering a name itself equivalent to a location is the source of some confusion

(define (analyze-lookup fn x src)
  (let loop ((search-fn fn))
    (if (eq? search-fn #f)
      x
      (if (OpenFn/toplevel? search-fn)
        (begin
          (unless (or (top-level-bound? x) (table-ref (OpenFn/env search-fn) x))
            (print-source src "Warning: reference to undefined global variable" x))
          x)
        (aif (table-ref (OpenFn/env search-fn) x)
          it
          (loop (OpenFn/parent search-fn)))))))

;; By the time compiler reaches VARs, the inlining decision is already made.
;; how does this effect upvalues and upvalue generation?

(define (analyze-expr fn x)
  (cond
    ((self-evaluating? x) x)
    ((identifier? x)
     (analyze-lookup fn x #f))

    ((special-form=? x 'define)
     (if (OpenFn/toplevel? fn)
       x
       (let* ((name (cadr x)) (var (AVar/make (OpenFn/local-count fn) name fn)))
         (table-set! (OpenFn/env fn) name var)
         (OpenFn/local-count! fn (fx+ (OpenFn/local-count fn) 1))
         (list-source x (car x) var (cddr x)))))

    ((special-form=? x 'set!)
     (let ((var (analyze-lookup fn (cadr x) #f)))
       (Var/mutated?! var #t)
       (cons-source x (car x) (cons-source x var (cddr x)))))
     
    ((special-form=? x 'lambda)
     (let* ((sub-fn (OpenFn/make (gensym 'lambda)))
            (args (cadr x))
            (arg-len (args-length args))
            (varargs (or (not (list? args)) (identifier? args))))
        (OpenFn/parent! sub-fn fn)
        (OpenFn/depth! sub-fn (if fn (fx+ (OpenFn/depth fn) 1) 0))
        (OpenFn/min-arity! sub-fn arg-len)
        (OpenFn/max-arity! sub-fn arg-len)
        (OpenFn/var-arity! sub-fn varargs)

        ;; Calculate argument count
        (OpenFn/local-count! sub-fn (if (identifier? args) 1 (fx+ arg-len (if (OpenFn/var-arity sub-fn) 1 0))))

        (unless (null? args)
          (if (identifier? args)
            ;; (lambda args args)
            (table-set! (OpenFn/env sub-fn) args (Var/make 0 args))

            ;; Handle arguments, including varargs
            (let loop ((rest (cdr args))
                       (item (car args))
                       (i 0))
              (table-set! (OpenFn/env sub-fn) item (AVar/make i item sub-fn ))
              (unless (null? rest)

                (if (pair? rest)
                  (loop (cdr rest) (car rest) (+ i 1))
                  ;; We've hit the varargs
                  (begin
                    (table-set! (OpenFn/env sub-fn) rest (AVar/make (+ i 1) rest sub-fn))))))))

        (OpenFn/body! sub-fn (analyze-body sub-fn (cddr x)))

        sub-fn))
    (else 
      (map (lambda (sub-x) (analyze-expr fn sub-x)) x)
      )))

(define (analyze-body fn body)
  (map (lambda (sub-x) (analyze-expr fn sub-x)) body))

(define (analyze-toplevel body)
  (define fn (OpenFn/make 'vm-toplevel))
  (OpenFn/toplevel?! fn #t)

  (scan-defines fn body)

  (OpenFn/body! fn (analyze-body fn body))

  fn)
|#

;; A copying append that uses source information
(define (append-source src lst elt)
  (let loop ((lst lst))
    (if (pair? lst)
      (cons-source src (car lst) (loop (cdr lst)))
      elt)
  )
)

;; Recompiles a toplevel function that's been defined under the interpreter
(define (recompile-function name)
  (let* ((oldfn (top-level-value name))
         (fn-name (function-name oldfn))
         (fn-body (function-body oldfn))
         (macro? (env-syntax? #f name))
         ;; TODO Could just save the original arguments list in the Function; it doesn't really matter
         ;; if they are oversized as they won't exist during normal execution in any case
         (fn-proper-args (or (function-arguments oldfn) '()))
         (fn-rest-arguments (function-rest-arguments oldfn))
         ;; Reconstruct a valid arguments list.
         (fn-args 
           (begin
             (if (and (not (null? fn-proper-args)) fn-rest-arguments)
               (append-source fn-body fn-proper-args fn-rest-arguments)
               (if (null? fn-proper-args)
                 (or fn-rest-arguments '())
                 fn-proper-args))))

         #;(fn (OpenFn/make fn-name))
         (fn-expr (append (list-source fn-body 'lambda fn-args) fn-body))
         
         ;(fn-expr (list-source fn-body 'lambda fn-args (car fn-body)))
         (fn-exxxpr
           ;; Unexpanded boot functions which use COND
           ;; need to be expanded.
           (if (memq fn-name '(module-import-eval expand-apply expand env-lookup env-define env-compare env-resolve expand-module-decl))
             (expand fn-expr #f)
             fn-expr))
         )

    (let ((fn (compile-lambda #f fn-exxxpr)))
      (OpenFn/name! fn fn-name)
      (set-top-level-value! name (OpenFn->procedure fn))
      (when macro?
        (set-function-macro-bit! (top-level-value name))))))

(define (time-function str cb)
  (let ((time-start (current-millisecond)))
    (cb)
    (let ((time-end (current-millisecond)))
      (print ";;" str)
      (print ";;" (- time-end time-start) "ms elapsed"))))

;; n.b. it's possible that bugs could lurk here, if the order of function recompilation is important
;; (it should not be). because top-level-for-each just iterates over the symbol table in whatever order it
;; happens to exist

;; we could also list them out by hand, but that seems kind of tedious, doesn't it?

(define (pull-up-bootstraps)
  (time-function "bootstrapped!"
    (lambda () 
      (print ";; compiled"
      (top-level-for-each
        (lambda (k v)

          (if (and (eq? (value-type v) 13) (not (memq k '(define-record))))
            (begin
              ;(print ";; compiling" k)
              (let ((is-macro (env-syntax? #f k)))
                (recompile-function k)
                )
              #t
              
              ))))
      "functions")
)))

(set-top-level-value! 'compiler compile-toplevel)

