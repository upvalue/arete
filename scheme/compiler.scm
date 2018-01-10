;; compiler.scm - Arete bytecode compiler

;; If you have a function with ten parameters, you probably missed a few.

;; - Alan Perlis.

;; TODO. All we need to implement display closures would be a simple analysis pass to check set! on variables.
;; This would remove a pointer dereference for variables which are not set!, which might actually be a decent boost
;; given the naive way certain expressions (e.g. (let loop ())) introduce free variables

;; Compiler parameters

;; If true, generate opcodes for things like +. Currently fairly inflexible and means procedures like + can't be
;; redefined (which they probably shouldn't be allowed to in any case)
(set-top-level-value! 'COMPILER-VM-PRIMITIVES #t)

;; If true, warn about undefined variables
(set-top-level-value! 'COMPILER-WARN-UNDEFINED #t)

;; Inline applications of functions in the CAR position e.g. ((lambda (a) #t) #t)
;; Assumes that the expander has made all variable names unique.
(set-top-level-value! 'COMPILER-INLINE-CAR #t)

;; Use destination-driven code generation to remove jumps-to-jumps and jumps-to-returns
(set-top-level-value! 'COMPILER-CONTROL-DESTINATION #t)

(define (compiler-inline-car) (eq? (top-level-value 'COMPILER-INLINE-CAR) #t))
(define (compiler-ddcg) (eq? (top-level-value 'COMPILER-CONTROL-DESTINATION) #t))

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
    (%OpenFn/make name (make-vector) (make-vector) (make-vector) 0 0 0 #f (make-table) 0 0 #f 0 #f 0 #f (make-table)
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

;; Print a list of INSNs with labels added in pairs. Note: does not work with multiple labels at same spot
(define (print-insns fn)
  (let* ((insns (OpenFn/insns fn))
         (insns-limit (vector-length insns))
         (labels (table-map (lambda (k v) (list v k)) (OpenFn/labels fn))))

    (let loop ((i 0) (lst '()))
      (if (fx= i insns-limit)
        (print (reverse lst))
        (let ((insn (vector-ref insns i)))
          (loop (fx+ i 1) (aif (assq i labels) (cons insn (cons (cdr it) lst)) (cons insn lst))))))))

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
      ;; Note the location of the instruction
      (vector-append! (OpenFn/sources fn) (vector-length (OpenFn/insns fn)))
      ;; Now insert the return value of list-get-source (a vector with four values) into the source vector
      (let loop ((i 0)
                 (sources (OpenFn/sources fn)))
        (unless (fx= i (vector-length it))
          (vector-append! sources (vector-ref it i))
          (loop (fx+ i 1) sources))))))

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

;; Build tables correlating instructions to bytes and vice versa
(define insns-to-bytes (make-table))
;(define bytes-to-insns (make-table))
;; TODO make this a vector.

;; Note: order is important
(define insn-list
  '(bad
    push-constant push-immediate pop
    global-get global-set local-get local-set upvalue-get upvalue-set
    close-over
    apply apply-tail
    return 
    jump jump-when jump-when-pop jump-unless
    argc-eq argc-gte arg-optional argv-rest
    + - < car cdr list-ref not eq? fx< fx+ fx-))

;; Static labels
(define static-labels '(past-optionals))

(let loop ((i 0) (lst insn-list))
  (table-set! insns-to-bytes (car lst) i)
  (unless (null? (cdr lst))
    (loop (fx+ i 1) (cdr lst))))

;; This converts symbols into their equivalent fixnums
(define (insn->byte fn insn)
  (cond
    ((fixnum? insn) insn)
    ;; Replace labels with their location
    ((or (gensym? insn) (memq insn static-labels)) (table-ref (OpenFn/labels fn) insn))
    (else 
      (aif (table-ref insns-to-bytes insn)
        it
        (raise 'compile-internal "unknown named instruction" (list insn))))))

;; Adjust the stack size while recalculating the stack max if necessary
(define (fn-adjust-stack fn size)
  (define new-size (fx+ (OpenFn/stack-size fn) size))
  (compiler-log fn "stack +=" size "=" new-size)
  (OpenFn/stack-size! fn new-size)
  (OpenFn/stack-max! fn (max new-size (OpenFn/stack-max fn)))
)

;; Build a table of stack effects
(define stack-effects
  (let ((table (make-table))
        (lst '(
               (-1 pop jump-when-pop global-set eq? list-ref local-set upvalue-set fx< fx- fx+)
               (0 jump jump-when argc-optional jump-unless words close-over return car cdr not argc-eq argc-gte argv-rest arg-optional)
               (1 push-immediate push-constant global-get local-get upvalue-get)
               )))
    (let loop ((elt (car lst)) (rest (cdr lst)))
      (let loop2 ((insns (cdr elt)))
        (table-set! table (car insns) (car elt))
        (unless (null? (cdr insns))
          (loop2 (cdr insns))))
      (unless (null? rest)
        (loop (car rest) (cdr rest))))
    table))

;; emit takes a named instruction and determines the stack effect of it based on its argument
(define (emit fn . insns)
  (compiler-log fn insns)

  (fn-adjust-stack fn 
    (aif (table-ref stack-effects (car insns))
      it
      (case (car insns)
        ;; Variable microcode: Remove arguments from stack, and re-use one of the argument slots to push results
        ((+ - <) (fx+ (fx- 0 (cadr insns)) 1))
        ;; Remove arguments from stack, but push a single result in the place of the function on the stack
        ((apply apply-tail) (fx- 0 (cadr insns)))
        (else (raise 'compile-internal (print-string "unknown instruction" (car insns)) (list fn (car insns) insns))))
    ))

  ;; Stack size sanity check
  (when (fx< (OpenFn/stack-size fn) 0)
    (raise 'compile-internal "stack underflow" (list insns (OpenFn/stack-size fn) fn)))

  (for-each1
    (lambda (insn) 
      ;; Words isn't a real instruction, just an argument to emit that means we don't need to check
      ;; stack size
      (unless (eq? insn 'words)
        (vector-append! (OpenFn/insns fn) insn)))
    insns))

(define (compile-constant fn x)
  (if (or (fixnum? x) (boolean? x) (eq? x unspecified))
    (emit fn 'push-immediate (value-bits x))
    (emit fn 'push-constant (register-constant fn x)))
)

;; Stack management

;; This will pop a value in the case that evaluation has happened in a side-effecty context
;; e.g. in the case of (begin (set! x #t) 2) we don't want to pop the first "argument" which results in no stack changes
;; in the case of (begin x 2) we want to pop "x" as it does nothing
(define (maybe-pop fn x original-stack)
  (if (fx= (OpenFn/stack-size fn) (fx+ original-stack 1))
    (emit fn 'pop)
    (if (not (fx= (OpenFn/stack-size fn) original-stack))
      (raise-source x 'compiler-internal "expected expression to push only one value onto the stack" (list (OpenFn/stack-size fn) original-stack x)))))

(define (fn-push-source fn x)
  (aif (list-get-source x)
    (vector-append! (OpenFn/sources fn) it)))

(define (alist->table alist)
  (if (null? alist)
    (make-table)
    (let ((table (make-table)))
      (let loop ((cell (car alist))
                 (rest (cdr alist)))
        (table-set! table (car cell) cell)
        (if (null? rest)
          table
          (loop (car rest) (cdr rest)))))))

(define primitive-table
  (alist->table '(
    ;; name min-argc max-argc variable-arity
    #|
    (list-ref 2 2 #f)
    |#
    (< 2 2 #t)
    (+ 0 0 #t)
    (- 1 1 #t)
    (car 1 1 #f)
    (cdr 1 1 #f)
    (not 1 1 #f)
    (eq? 2 2 #f)
    (fx+ 2 2 #f)
    (fx- 2 2 #f)
    (fx< 2 2 #f)
  ))
)

(define (apply-inlinable? fn x)
  (and (compiler-inline-car)
       (not (OpenFn/toplevel? fn))
       (pair? (car x))
       (eq? (rename-strip (caar x)) 'lambda)
       (list? (cadar x))))

(define (compile-inline-call fn x tail?)
  ;; inlining a function call:
  ;; subsume all arguments as locals
  ;; compile all arguments as local-sets
  ;; compile body
  (compiler-log fn "inlining function call" x)

  (let ((args (cadar x)) (locals (OpenFn/local-count fn)))
    (when (not (fx= (length (cdr x)) (length args)))
      (raise-source x 'compile (print-string "inline procedure application expected" (length args) "arguments but got" (length (cdr x))) (list x)))

    (unless (null? args)
      ;; TODO check args length
      (let loop ((i 0) (item (car args)) (rest (cdr args)))

        ;(compiler-log fn "inlining function arg" (car args))
        (table-set! (OpenFn/env fn) item (Var/make (fx+ locals i) item))

        (compile-expr fn (list-ref x (fx+ i 1)) #f (list-tail x i) #f)
        (emit fn 'local-set (fx+ locals i))

        (unless (null? rest)
          (loop (fx+ i 1) (car rest) (cdr rest)))))
    (OpenFn/local-count! fn (fx+ locals (length (cdr x)))))

  ;(pretty-print fn)

  (let ((body (cons (make-rename #f 'begin) (cddar x))))

    (scan-local-defines fn body)
    (compile-expr fn body #f (cdar x) tail?)))

(define (compile-apply fn x tail?)
  (define stack-check #f)
  (define argc (length (cdr x)))
  (define primitive #f)
  ;; If true, argument count will be emitted after the primitive thing
  (define primitive-args #f)
  ;; strip renames at this point
  (when (rename? (car x))
    (raise-source x 'compile-internal "rename encountered by compiler; this should never happen" (list x)))
  (define kar (if (rename? (car x)) (rename-expr (car x)) (car x)))

  (compiler-log fn "compiling application" x)

  (if (apply-inlinable? fn x)
    (begin
      (compile-inline-call fn x tail?))
    (let ()
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
        (compile-expr fn (car x) #f x #f))

      (set! stack-check (OpenFn/stack-size fn))

      ;; Extract an argument list 
      (define argument-list
        (if (and (pair? (car x)) (eq? (rename-strip (caar x)) 'lambda))
          (and (pair? (cadar x)) (cadar x))
          #f))

      (define argument-list-length (if (and argument-list (list? argument-list)) (length argument-list) #f))

      (for-each-i
        (lambda (i sub-x)
          (define result (compile-expr fn sub-x #f (list-tail x (fx+ i 1)) #f))
          (when (and argument-list argument-list-length)
            (if (fx= i argument-list-length)
              (print-source x "Inline function application appears to have too many arguments")
              (if (vmfunction? result)
                (set-function-name! result (list-ref argument-list i)))

              
              )))
        (cdr x))

      ;; Stack size sanity check
      ;; Except for toplevel functions
      (unless (or (OpenFn/toplevel? fn) (eq? (fx- (OpenFn/stack-size fn) argc) stack-check))
        (raise 'compile-internal (print-string "expected function stack size" (OpenFn/stack-size fn)
                                               "to match 0 + function arguments" stack-check) (list fn x)))


      (if primitive
        (begin
          (if primitive-args
            (if (and (eq? (car primitive) '+) (null? (cdr x)))
              (compile-constant fn 0)
              (emit fn (car primitive) (length (cdr x))))
            (emit fn (car primitive))
            
          )
        )
        (emit fn (if tail? 'apply-tail 'apply) (length (cdr x))))
      )))

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

        (Var/idx! var (vector-length closure))

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
  (if (symbol-qualified? x)
    (cons 'global x)
    (let loop ((search-fn fn))
      (if (eq? search-fn #f)
        (cons 'global x)
        (if (OpenFn/toplevel? search-fn)
          (begin
            #;(unless (or (top-level-bound? x) (table-ref (OpenFn/env search-fn) x) (not (top-level-value 'COMPILER-WARN-UNDEFINED)))
              (print-source src "Warning: reference to undefined variable" (symbol-dequalify x)))
            (cons 'global x))
          (aif (table-ref (OpenFn/env search-fn) x)
            (begin
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
              )
            (begin
              (loop (OpenFn/parent search-fn)))))))))

(define (compile-identifier fn x src)
  (if (rename? x)
    (begin
      ;; how do we resolve a rename to its appropriate expansion-time location...can we do this with the information
      ;; we have, or do we need to add some kind of annotation?
      (print "stripping rename!" x)
      (set! x (rename-expr x))))
  (define result (fn-lookup fn x src))

  (compiler-log fn "compiling identifier" result)

  (case (car result)
    (local (emit fn 'local-get (cdr result)))
    (global (emit fn 'global-get (register-constant fn x)))
    (upvalue (emit fn 'upvalue-get (cdr result)))
    (else (raise 'compile-internal ":(" (list x))))
)

(define (OpenFn/define! fn name)
  (let ((var (Var/make (OpenFn/local-count fn) name)))
    (table-set! (OpenFn/env fn) name var)
    (OpenFn/local-count! fn (fx+ (OpenFn/local-count fn) 1))
    var))

(define (scan-local-defines-expr fn x)
  (when (pair? x)
    (cond
      ((eq? (rename-strip (car x)) 'define)
       (OpenFn/define! fn (cadr x)))
      ((eq? (rename-strip (car x)) 'quote) #f)
      ((eq? (rename-strip (car x)) 'lambda) #f)
      (else
        (for-each-improper (lambda (expr) (scan-local-defines-expr fn expr)) x)))))

(define (scan-local-defines fn body)
  (for-each-improper
    (lambda (x)
      (scan-local-defines-expr fn x))
    body)
 )

;; Processes an arguments list. Defines all argument names as local variables, sets up function arity and local count
;; appropriately.

(define (process-argument-list! fn x)
  (define arg-i 0)
  (define seen-optional #f)

  (cond
    ((null? x) #t)
    ((identifier? x)
     (begin
       (OpenFn/var-arity! fn #t)
       (OpenFn/define! fn x)))
    (else
      (begin
        (for-each-improper
          (lambda (a)
            (cond
              ((or (identifier? a) (pair? a))
               (let ((name (if (identifier? a) a (car a))))
                 (unless seen-optional
                   (OpenFn/min-arity! fn (fx+ arg-i 1)))

                 (OpenFn/max-arity! fn (fx+ arg-i 1))
                 (set! arg-i (fx+ arg-i 1))
                 (OpenFn/define! fn name)))

              ((eq? a #!optional)
               (set! seen-optional #t)
               )
              ((memq a '(#!keys #!key #!rest)
                (raise-source a 'compiler (print-string a "not supported yet") (list a))))
              (else
                (raise-source x 'compiler-internal "unexpected argument list item" (lsit a)))))
          x)

        ;; Handle improper list rest argument
        (unless (list? x)
          (OpenFn/var-arity! fn #t)
          ;; Remove rest argument from arity.
          (when (fx= (OpenFn/min-arity fn) (OpenFn/max-arity fn))
            (OpenFn/min-arity! fn (fx- (OpenFn/min-arity fn) 1)))
          (OpenFn/max-arity! fn (fx- (OpenFn/max-arity fn) 1)))))
    
    )
)

;; Optional argument evaluation:
;; ($optional 5)
;; ($optional 6)
;; ($optional 7)
;; if argc < current optional index (given as wordcode operand), evaluate the stuff on the right; otherwise, jump
;; to the body of the function (given by label optionals)

(define (compile-lambda fn x)
  ;; Create a new OpenFn with fn as a parent
  (define sub-fn (OpenFn/make (gensym 'lambda)))
  (define args (cadr x))
  (compiler-log sub-fn (OpenFn/name sub-fn))
  (when fn
    (compiler-log fn "parent of" (OpenFn/name sub-fn) "is" (OpenFn/name fn)))

  ;; Most of the complexity of this function is just setting up the fields of OpenFn
  (OpenFn/parent! sub-fn fn)

  ;; Note the depth of the function
  (OpenFn/depth! sub-fn (if fn (fx+ (OpenFn/depth fn) 1) 0))

  ;; Emit prologue argument checking/processing instructions

  (process-argument-list! sub-fn args)
  ;; If this function has simple arity (no optional or rest arguments), we use argc-eq. Otherwise, we use argc-gte,
  ;; followed by argv-rest
  (if (and (fx= (OpenFn/min-arity sub-fn) (OpenFn/max-arity sub-fn)) (not (OpenFn/var-arity sub-fn)))
    (emit sub-fn 'argc-eq (OpenFn/min-arity sub-fn))
    (begin
      (emit sub-fn 'argc-gte (OpenFn/min-arity sub-fn))
      ;; We generate argv-rest after the argc checks only for functions which don't take optional arguments
      ;; Otherwise it will be generated after $label-rest-optionals
      (when (and (OpenFn/var-arity sub-fn) (fx= (OpenFn/min-arity sub-fn) (OpenFn/max-arity sub-fn)))
        (emit sub-fn 'argv-rest))))

  (scan-local-defines sub-fn (cddr x))
  ;; Compile the lambda's body
  (compile sub-fn (cddr x))

  ;; And it's finally done
  (compile-finish sub-fn)

  (let ((procedure (OpenFn->procedure sub-fn)))
    ;; This can be called without a higher function in the case that we're bootstrapping and recompiling the whole
    ;; heap. If so, we'll just return the compiled procedure, if not, we'll emit some code to push this function
    ;; onto the stack and to create a closure, if necessary
    (when fn
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
              (loop (fx+ i 1)))))))
      procedure)
)

(define (compile-define fn x tail?)
  (define name (cadr x))
  (define var (table-ref (OpenFn/env fn) name))

  ;; Detect function names
  (let ((result (compile-expr fn (list-ref x 2) #f (list-tail x 2) tail?)))
    (when (procedure? result)
      (begin
        (set-function-name! result (symbol-dequalify name)))))

  (if (OpenFn/toplevel? fn)
    (begin
      (emit fn 'global-set 0 (register-constant fn name)))
    (begin
      (emit fn 'local-set (Var/idx var))))
)

(define (compile-set! fn x src tail?)
  (define name (cadr x))
  (define result (fn-lookup fn name src))

  (let ((fn (compile-expr fn (list-ref x 2) #f (list-tail x 2) tail?)))
    (if (vmfunction? fn)
      (set-function-name! fn name)))

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

 
;; Work in progress: destination driven code generation

;; This passes around a "control destination" which can be jumped to directly.
;; if cd is #t, jumps will be replaced by a return. For example
;; ((lambda () (if #t #t #f)) can return directly rather than jumping to a RETURN at the end of the if-expression
;; if cd is a single gensym, jumps can go to that, for example
;; (if #t (if #t 1 2) 3)
;; the jump after 1 can go directly to the end of the toplevel if expression

;; TODO pairs of labels so that things like (if (and 1 2 3)) can jump to parts of if expressions based on
;; their values

;; This vastly reduces the amount of jumps-to-jumps generated, and completely reduces the amount of jumps-to-returns
;; Unfortunately, this seems to have negligible impact on performance :(

;; Emit an unconditional jump instruction
(define (emit-jump fn cd label)
  (if (compiler-ddcg)
    (cond 
      ((gensym? cd)
       (emit fn 'jump label))
      ((eq? cd #f)
       (emit fn 'jump label))
      ((eq? cd #t)
       (emit fn 'return))
      (else (raise 'compiler-internal "unknown control destination" (list cd))))
    (emit fn 'jump label)))
      
(define (control-destination cd label)
  (if (and (compiler-ddcg) (gensym? cd))
    (begin
      ;(print "skipping to control destination" cd)
      cd
    )
  label))

(define (compile-if fn x cd tail?)
  (define condition (cadr x))
  (define then-branch (list-ref x 2))
  (define else-branch (if (fx= (length x) 4) (list-ref x 3)))
  (define then-branch-end (gensym 'if-else))
  (define else-branch-end (gensym 'if-end))

  ;(print "control destination" cd)

  (define check-stack (OpenFn/stack-size fn))
  (compile-expr fn condition #f (list-tail x 1) #f)

  (emit fn 'jump-when-pop then-branch-end)

  (set! check-stack (OpenFn/stack-size fn))
  (compile-expr fn then-branch (control-destination cd else-branch-end) (list-tail x 2) tail?)

  (emit-jump fn cd else-branch-end)
  (register-label fn then-branch-end)

  (compile-expr fn else-branch else-branch-end (if (fx= (length x) 4) (list-tail x 3) #f) tail?)

  (register-label fn else-branch-end)

  ;; Manually adjust stack for conditional evaluation
  (OpenFn/stack-size! fn (fx- (OpenFn/stack-size fn) 1))

  )

;; Short circuiting evaluation expressions: AND/OR.
;; These jump to the end upon meeting their condition (false for and, true for or)
;; Otherwise, they pop the result and evaluate the next expression.
(define (compile-condeval type fn x tail?)
  (if (null? (cdr x))
    (compile-constant fn (eq? type 'and)) ;; (and) => #t, (or) => #f
    (let ((x-len (fx- (length x) 2))
          (condeval-end (gensym 'condeval-end))
          (jmp (if (eq? type 'and) 'jump-when 'jump-unless)))
      (for-each-i
        (lambda (i sub-x)
          (unless (fx= i 0)
            (emit fn 'pop))
          (compile-expr fn sub-x #f (list-tail x (fx+ i 1)) (and tail? (fx= i x-len)))
          (unless (fx= i x-len)
            (emit fn jmp condeval-end)))
        (cdr x))

      (register-label fn condeval-end))))

(define (compile-quote fn x) (compile-constant fn (cadr x)))

;; Compiling a begin is fairly simple -- just need to make sure the last expression is considered a tail call.
(define (compile-begin fn x tail?)
  (define x-len (fx- (length x) 2))
  (if (null? (cdr x))
    (emit fn 'push-immediate (value-bits unspecified))
    (for-each-i 
      (lambda (i sub-x)
        (define stack-size (OpenFn/stack-size fn))
        ;(print sub-x (OpenFn/stack-size fn))
        (compile-expr fn sub-x #f (list-tail x i) (and tail? (fx= i x-len)))
        ;(print sub-x (OpenFn/stack-size fn))
        ;; Expression is here for side-effects. Pop it.
        (unless (fx= i x-len)
          (maybe-pop fn sub-x stack-size))
      )
      (cdr x))))

(define (compile-optional fn x)
  (emit fn 'arg-optional (cadr x) 'past-optionals)
  (compile-expr fn (caddr x) #f x #f)
  (emit fn 'local-set (cadr x))
  )

(define (compile-special-form fn x cd src type tail?)
  (compiler-log fn "compiling special form" type x)
  (case type
    (lambda (compile-lambda fn x))
    ;; TODO: Define and set cannot have tail-applications currently, because the global-set instructions are
    ;; generated after them.
    ;; So something like (set! var (fn)) should not destroy the stack frame.
    (define (compile-define fn x #f))
    (set! (compile-set! fn x src #f))
    (if (compile-if fn x (if tail? #t cd) tail?))
    (and (compile-condeval 'and fn x tail?))
    (or (compile-condeval 'or fn x tail?))
    (quote (compile-quote fn x))
    (begin (compile-begin fn x tail?))

    ;; Expander-generated builtins
    ($optional (compile-optional fn x))
    ($label-past-optionals
      (begin
        (register-label fn 'past-optionals)
        (when (OpenFn/var-arity fn)
          (emit fn 'argv-rest))))
    (else
      (begin
        (raise 'compile-internal "unknown special form" (list type (car x)))))))

(define (special-form x)
  (when (rename? x)
    (if (rename-env x) 
      (raise 'compile-internal "compiler encountered non-toplevel rename" (list x)))

    (set! x (rename-expr x)))

  (if (memq x '(lambda define set! if begin and or quote $optional $label-past-optionals)) x #f))

(define (compile-expr fn x cd src tail?)
  (and #f #f)
  (compiler-log fn "compiling expr" x)


  (aif (and src (list-get-source src))
    (begin
      (compiler-log fn "expr source:" it)
      (register-source fn src))
  )

  (define check-stack (OpenFn/stack-size fn))

  (define result
    (cond
      ((self-evaluating? x) (compile-constant fn x))
      ((identifier? x) (compile-identifier fn x src))
      ((list? x)
       (aif (special-form (car x))
         (begin
           (compile-special-form fn x cd src it tail?))
         (compile-apply fn x tail?)))
      (else (raise 'compile-internal "don't know how to compile expression" (list x)))))

  ;; We'll push UNSPECIFIED on the stack in case this expression happens in a context where a value must be consumed
  ;; such as (if #t (set! x #t) (set! x #f)) etc
  (if (fx= check-stack (OpenFn/stack-size fn))
    (compile-constant fn unspecified)
    (unless (fx= check-stack (fx- (OpenFn/stack-size fn) 1))
      (raise 'compile-internal "expression resulted in stack growing by more than 1" (list x))))



  result)

;; Compiler entry point; compiles a list of expressions and adds them to a function
(define (compile fn body)
  (define end (fx- (length body) 1))
  (compiler-log fn "compiling body" body)
  (for-each-i
    (lambda (i x)
      (register-source fn (list-tail body i))
      ;; No tail calls in toplevel programs, so that something like
      ;; (define x (vm-function)) at REPL will work.
      (compile-expr fn x #f (list-tail body i) (and (not (OpenFn/toplevel? fn)) (fx= i end)))
    )
    body)

  (emit fn 'return)

  fn)

;; Compiler finisher -- converts symbolic VM instructions into actual fixnums
(define (compile-finish fn)
  (compiler-log fn fn)
  ;(print-insns fn)

  (let loop ((i 0))
    (unless (fx= i (vector-length (OpenFn/insns fn)))
      (vector-set! (OpenFn/insns fn) i (insn->byte fn (vector-ref (OpenFn/insns fn) i)))
      (loop (fx+ i 1)))))

;; This is where most expressions will enter the compiler, it creates a function on-the-fly and returns it for
;; execution
(define (compile-toplevel body)
  (define fn (OpenFn/make (aif (source-name body) (string->symbol (string-append "toplevel:" it)) 'vm-toplevel)))

  (OpenFn/toplevel?! fn #t)

  (compile fn (list body))
  (compile-finish fn)

  (let ((result (OpenFn->procedure fn)))
    result))

;; A copying append that uses source information
(define (append-source src lst elt)
  (let loop ((lst lst))
    (if (pair? lst)
      (cons-source src (car lst) (loop (cdr lst)))
      elt)
  )
)

;; Recompiles a function that's been defined under the interpreter
(define (recompile-function oldfn)
  (let* ((fn-name (function-name oldfn))
         (fn-body (function-body oldfn))
         (fn-source (if (list-get-source fn-body) fn-body (car fn-body)))
         (is-macro? (macro? oldfn))
         ;; TODO Could just save the original arguments list in the Function; it doesn't really matter
         ;; if they are oversized as they won't exist during normal execution in any case
         (fn-proper-args (or (function-arguments oldfn) '()))
         (fn-rest-arguments (function-rest-arguments oldfn))
         ;; Reconstruct a valid arguments list.
         (fn-args 
           (begin
             (if (and (not (null? fn-proper-args)) fn-rest-arguments)
               (append-source fn-source fn-proper-args fn-rest-arguments)
               (if (null? fn-proper-args)
                 (or fn-rest-arguments '())
                 fn-proper-args))))

         #;(fn (OpenFn/make fn-name))
         (fn-expr (append-source fn-source (list-source fn-body 'lambda fn-args) fn-body))

         ;(fn-expr (list-source fn-body 'lambda fn-args (car fn-body)))
         (fn-exxxpr
           ;; Unexpanded boot functions which use COND
           ;; need to be expanded.
           (if (memq fn-name '(expand-argument-list parse-next-argument parse-arguments-list module-import-eval expand-apply expand env-lookup env-define env-compare env-resolve expand-module-decl))
             (expand-toplevel fn-expr #f)
             fn-expr))
         )

    (let ((fn (compile-lambda #f fn-exxxpr)))
      (when fn-name
        (set-function-name! fn fn-name))
      (when is-macro?
        (set-function-macro-env! fn (function-env oldfn))
        (set-function-macro-bit! fn))
      fn)
  ))

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
      (top-level-for-each
        (lambda (k v)
          ;; This is pretty shoddy, but since the module system currently just creates duplicates of every function
          ;; defined before it is installed, we'll compile all functions in the module system.

          (if (and (eq? (value-type v) 13))
            (begin
              (let ((is-macro (env-syntax? #f k)))
                (let ((vmf (recompile-function v)))
                  (set-top-level-value! k vmf)
                  (if (and (symbol-qualified? k) (and (eq? (top-level-value (symbol-dequalify k)) unspecified)))
                    (set-top-level-value! (symbol-dequalify k) vmf)
                    #t))
                )

              ))))

      ;; Fascinating: moving this table-for-each above the other really changes the speed of bootstrapping
      ;; There's probably an optimal order in which to recompile the system for bootstrapping speed, getting the 
      ;; compile functions first

      ;; Recompile macros, which have not been defined at the toplevel.
      (table-for-each
        (lambda (k v)
          (if (eq? (value-type v) 13)
            (let ((vmf (recompile-function v)))
              (table-set! (top-level-value '*core-module*) k vmf)
              (table-set! (top-level-value '*user-module*) k vmf)
              )))
        (top-level-value '*core-module*))

      ;; update certain functions hw
      (set-top-level-value! 'expander expand-toplevel)
      (set-top-level-value! 'compiler compile-toplevel)

)))

(expand-import (top-level-value '*user-module*) '(arete))

;; Turn over default execution to the (user) module
(set-top-level-value! '*push-module* (top-level-value '*user-module*))
(set-top-level-value! '*current-module* (top-level-value '*user-module*))
(set-top-level-value! 'compiler compile-toplevel)

