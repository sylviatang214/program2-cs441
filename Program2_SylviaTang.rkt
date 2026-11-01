#lang racket

(require racket/match)

;; =============================================================================
;; 1. SCANNER / LEXER
;; =============================================================================

(struct token (type value line) #:transparent)

(define (tokenize input-str)
  (define len (string-length input-str))
  (define idx 0)
  (define line 1)

  (define (peek)
    (if (< idx len) (string-ref input-str idx) #\nul))

  (define (advance)
    (let ([ch (peek)])
      (set! idx (add1 idx))
      (when (char=? ch #\newline)
        (set! line (add1 line)))
      ch))

  (define (skip-whitespace-and-comments)
    (cond
      [(>= idx len) (void)]
      [(char-whitespace? (peek))
       (advance)
       (skip-whitespace-and-comments)]
      [(and (char=? (peek) #\-)
            (< (add1 idx) len)
            (char=? (string-ref input-str (add1 idx)) #\-))
       (advance) (advance)
       (let loop ()
         (when (and (< idx len) (not (char=? (peek) #\newline)))
           (advance)
           (loop)))
       (skip-whitespace-and-comments)]
      [else (void)]))

  (define tokens '())
  (define (emit type val)
    (set! tokens (cons (token type val line) tokens)))

  (let main-loop ()
    (skip-whitespace-and-comments)
    (when (< idx len)
      (let ([ch (peek)])
        (cond
          ;; Double-character operators
          [(and (char=? ch #\=) (< (add1 idx) len) (char=? (string-ref input-str (add1 idx)) #\>))
           (advance) (advance) (emit 'ARROW "=>")]
          [(and (char=? ch #\=) (< (add1 idx) len) (char=? (string-ref input-str (add1 idx)) #\=))
           (advance) (advance) (emit 'RELOP "==")]
          [(and (char=? ch #\/) (< (add1 idx) len) (char=? (string-ref input-str (add1 idx)) #\=))
           (advance) (advance) (emit 'RELOP "/=")]
          [(and (char=? ch #\<) (< (add1 idx) len) (char=? (string-ref input-str (add1 idx)) #\=))
           (advance) (advance) (emit 'RELOP "<=")]
          [(and (char=? ch #\>) (< (add1 idx) len) (char=? (string-ref input-str (add1 idx)) #\=))
           (advance) (advance) (emit 'RELOP ">=")]

          ;; Single-character operators & delimiters
          [(char=? ch #\<) (advance) (emit 'RELOP "<")]
          [(char=? ch #\>) (advance) (emit 'RELOP ">")]
          [(char=? ch #\+) (advance) (emit 'ADDOP "+")]
          [(char=? ch #\-) (advance) (emit 'ADDOP "-")]
          [(char=? ch #\~) (advance) (emit 'ADDOP "~")]
          [(char=? ch #\*) (advance) (emit 'MULOP "*")]
          [(char=? ch #\/) (advance) (emit 'MULOP "/")]

          [(char=? ch #\{) (advance) (emit 'LBRACE "{")]
          [(char=? ch #\}) (advance) (emit 'RBRACE "}")]
          [(char=? ch #\() (advance) (emit 'LPAREN "(")]
          [(char=? ch #\)) (advance) (emit 'RPAREN ")")]
          [(char=? ch #\[) (advance) (emit 'LBRACKET "[")]
          [(char=? ch #\]) (advance) (emit 'RBRACKET "]")]
          [(char=? ch #\;) (advance) (emit 'SEMI ";")]
          [(char=? ch #\:) (advance) (emit 'COLON ":")]
          [(char=? ch #\,) (advance) (emit 'COMMA ",")]

          ;; String literals: `...`
          [(char=? ch #\`)
           (advance)
           (define chars '())
           (let str-loop ()
             (cond
               [(>= idx len) (error 'lexer "Unterminated string literal at line ~a" line)]
               [(char=? (peek) #\`)
                (advance)
                (emit 'STRING_LIT (list->string (reverse chars)))]
               [else
                (set! chars (cons (advance) chars))
                (str-loop)]))]

          ;; Numbers
          [(char-numeric? ch)
           (define num-str (string (advance)))
           (let int-loop ()
             (when (and (< idx len) (char-numeric? (peek)))
               (set! num-str (string-append num-str (string (advance))))
               (int-loop)))
           (if (and (< idx len)
                    (char=? (peek) #\.)
                    (< (add1 idx) len)
                    (char-numeric? (string-ref input-str (add1 idx))))
               (begin
                 (set! num-str (string-append num-str (string (advance))))
                 (let real-loop ()
                   (when (and (< idx len) (char-numeric? (peek)))
                     (set! num-str (string-append num-str (string (advance))))
                     (real-loop)))
                 (emit 'REAL_LIT (string->number num-str)))
               (emit 'INT_LIT (string->number num-str)))]

          ;; Identifiers and words
          [(char-alphabetic? ch)
           (define id-str (string (advance)))
           (let id-loop ()
             (when (and (< idx len) (or (char-alphabetic? (peek)) (char-numeric? (peek))))
               (set! id-str (string-append id-str (string (advance))))
               (id-loop)))
           (cond
             [(member id-str '("yes" "no" "maybe"))
              (emit 'BOOL_LIT (string->symbol id-str))]
             [(string=? id-str "not")
              (emit 'NOT id-str)]
             [(member id-str '("or" "~"))
              (emit 'ADDOP id-str)]
             [(string=? id-str "and")
              (emit 'MULOP id-str)]
             [(member id-str '("int" "real" "bool" "string"))
              (emit 'TYPE (string->symbol id-str))]
             [else
              (emit 'IDENTIFIER id-str)])]

          [else
           (error 'lexer "Unexpected character '~a' at line ~a" ch line)]))
      (main-loop)))

  (reverse (cons (token 'EOF "EOF" line) tokens)))

;; =============================================================================
;; 2. RECURSIVE DESCENT PARSER
;; =============================================================================

(define (parse token-list)
  (define tokens (list->vector token-list))
  (define pos 0)

  (define (current-token) (vector-ref tokens pos))
  (define (peek-type) (token-type (current-token)))
  (define (peek-val) (token-value (current-token)))

  (define (peek-next-type)
    (if (< (add1 pos) (vector-length tokens))
        (token-type (vector-ref tokens (add1 pos)))
        'EOF))

  (define (peek-next-val)
    (if (< (add1 pos) (vector-length tokens))
        (token-value (vector-ref tokens (add1 pos)))
        ""))

  (define (match? type) (eq? (peek-type) type))

  (define (match-keyword? kw)
    (and (eq? (peek-type) 'IDENTIFIER)
         (string=? (peek-val) kw)))

  (define (consume expected-type)
    (let ([tok (current-token)])
      (if (eq? (token-type tok) expected-type)
          (begin (set! pos (add1 pos)) tok)
          (error 'compiler "Syntax Error: Expected token ~a, found ~a ('~a') at line ~a"
                 expected-type (token-type tok) (token-value tok) (token-line tok)))))

  (define (consume-keyword kw)
    (let ([tok (current-token)])
      (if (and (eq? (token-type tok) 'IDENTIFIER) (string=? (token-value tok) kw))
          (begin (set! pos (add1 pos)) tok)
          (error 'compiler "Syntax Error: Expected keyword '~a', found ~a ('~a') at line ~a"
                 kw (token-type tok) (token-value tok) (token-line tok)))))

  (define (parse-identifier)
    (token-value (consume 'IDENTIFIER)))

  (define (parse-program)
    (consume-keyword "program")
    (define id (parse-identifier))
    (consume 'LBRACE)
    (define decls (parse-declaration-list))
    (define stmts (parse-statement-list))
    (when (match? 'RBRACE)
      (consume 'RBRACE))
    `(program ,id ,decls ,stmts))

  (define (parse-declaration-list)
    (define vars (parse-var-decl-list))
    (define procs (parse-proc-decl-list))
    `(declarations (vars ,@vars) (routines ,@procs)))

  (define (parse-var-decl-list)
    (if (match-keyword? "decl")
        (cons (parse-var-declaration) (parse-var-decl-list))
        '()))

  ;; Trailing semicolon is optional for variable declarations
  (define (parse-var-declaration)
    (consume-keyword "decl")
    (define ids (parse-identifier-list))
    (consume-keyword "as")
    (define t (token-value (consume 'TYPE)))
    (when (match? 'SEMI)
      (consume 'SEMI))
    `(decl-var ,ids ,t))

  (define (parse-identifier-list)
    (define id (parse-identifier))
    (if (match? 'COMMA)
        (begin (consume 'COMMA) (cons id (parse-identifier-list)))
        (list id)))

  (define (parse-proc-decl-list)
    (if (match-keyword? "routine")
        (cons (parse-proc-declaration) (parse-proc-decl-list))
        '()))

  (define (parse-proc-declaration)
    (consume-keyword "routine")
    (define id (parse-identifier))
    (define params (parse-formal-params))
    (consume-keyword "returns")
    (define ret-type (token-value (consume 'TYPE)))
    (consume 'LBRACE)
    (define decls (parse-declaration-list))
    (define stmts (parse-statement-list))
    (consume 'RBRACE)
    `(routine ,id ,params ,ret-type ,decls ,stmts))

  (define (parse-formal-params)
    (consume 'LPAREN)
    (if (match? 'RPAREN)
        (begin (consume 'RPAREN) '())
        (let ([params (parse-param-list)])
          (consume 'RPAREN)
          params)))

  (define (parse-param-list)
    (define p (parse-param))
    (if (match? 'COMMA)
        (begin (consume 'COMMA) (cons p (parse-param-list)))
        (list p)))

  (define (parse-param)
    (define id (parse-identifier))
    (consume 'COLON)
    (define t (token-value (consume 'TYPE)))
    `(param ,id ,t))

  (define (parse-statement-list)
    (if (or (match? 'RBRACE) (match? 'EOF))
        '()
        (let* ([is-block? (or (and (match-keyword? "choose") (eq? (peek-next-type) 'LBRACKET))
                              (and (match-keyword? "loop")   (string=? (peek-next-val) "until")))]
               [stmt (parse-statement)])
          (cond
            [is-block?
             (when (match? 'SEMI) (consume 'SEMI))]
            [else
             (when (match? 'SEMI) (consume 'SEMI))])
          (cons stmt (parse-statement-list)))))

  (define (parse-statement)
    (cond
      [(and (match-keyword? "choose") (eq? (peek-next-type) 'LBRACKET))
       (parse-conditional-stmt)]

      [(and (match-keyword? "loop") (string=? (peek-next-val) "until"))
       (parse-loop-stmt)]

      [(match-keyword? "yield")
       (parse-return-stmt)]

      [(match-keyword? "invoke")
       (parse-call-or-assign-call-stmt)]

      [(or (match? 'IDENTIFIER)
           (match? 'INT_LIT)
           (match? 'REAL_LIT)
           (match? 'BOOL_LIT)
           (match? 'STRING_LIT)
           (match? 'LPAREN)
           (match? 'NOT))
       (parse-assignment-or-expr-stmt)]

      [else '(empty-stmt)]))

  (define (parse-conditional-stmt)
    (consume-keyword "choose")
    (consume 'LBRACKET)
    (define cond-expr (parse-expression))
    (consume 'RBRACKET)
    (consume-keyword "match")
    (consume 'LBRACE)
    (define match-stmts (parse-statement-list))
    (consume 'RBRACE)
    (if (match-keyword? "otherwise")
        (begin
          (consume-keyword "otherwise")
          (consume 'LBRACE)
          (let ([otherwise-stmts (parse-statement-list)])
            (consume 'RBRACE)
            `(choose ,cond-expr (then ,@match-stmts) (otherwise ,@otherwise-stmts))))
        `(choose ,cond-expr (then ,@match-stmts))))

  (define (parse-loop-stmt)
    (consume-keyword "loop")
    (consume-keyword "until")
    (consume 'LBRACKET)
    (define test-expr (parse-expression))
    (consume 'RBRACKET)
    (consume-keyword "do")
    (consume 'LBRACE)
    (define body-stmts (parse-statement-list))
    (consume 'RBRACE)
    `(loop-until ,test-expr ,@body-stmts))

  (define (parse-return-stmt)
    (consume-keyword "yield")
    (define val (parse-expression))
    `(yield ,val))

  (define (parse-call-or-assign-call-stmt)
    (consume-keyword "invoke")
    (define id (parse-identifier))
    (define params (parse-actual-params))
    (if (match? 'ARROW)
        (begin
          (consume 'ARROW)
          (let ([target (parse-identifier)])
            `(assign-call (call ,id ,params) ,target)))
        `(call ,id ,params)))

  (define (parse-actual-params)
    (consume 'LPAREN)
    (if (match? 'RPAREN)
        (begin (consume 'RPAREN) '())
        (let ([args (parse-expression-list)])
          (consume 'RPAREN)
          args)))

  (define (parse-expression-list)
    (define expr (parse-expression))
    (if (match? 'COMMA)
        (begin
          (consume 'COMMA)
          (cons expr (parse-expression-list)))
        (list expr)))

  (define (parse-assignment-or-expr-stmt)
    (define expr (parse-expression))
    (if (match? 'ARROW)
        (begin
          (consume 'ARROW)
          (let ([target (parse-identifier)])
            `(assign ,expr ,target)))
        expr))

  (define (parse-expression)
    (define left (parse-simple-expr))
    (if (match? 'RELOP)
        (let* ([op (token-value (consume 'RELOP))]
               [right (parse-simple-expr)])
          `(relop ,op ,left ,right))
        left))

  (define (parse-simple-expr)
    (define left (parse-term))
    (if (match? 'ADDOP)
        (let* ([op (token-value (consume 'ADDOP))]
               [right (parse-simple-expr)])
          `(addop ,op ,left ,right))
        left))

  (define (parse-term)
    (define left (parse-factor))
    (if (match? 'MULOP)
        (let* ([op (token-value (consume 'MULOP))]
               [right (parse-term)])
          `(mulop ,op ,left ,right))
        left))

  (define (parse-factor)
    (cond
      [(match? 'IDENTIFIER) `(id ,(parse-identifier))]
      [(match? 'INT_LIT)    `(int ,(token-value (consume 'INT_LIT)))]
      [(match? 'REAL_LIT)   `(real ,(token-value (consume 'REAL_LIT)))]
      [(match? 'BOOL_LIT)   `(bool ,(token-value (consume 'BOOL_LIT)))]
      [(match? 'STRING_LIT) `(string ,(token-value (consume 'STRING_LIT)))]
      [(match? 'NOT)
       (consume 'NOT)
       `(not ,(parse-factor))]
      [(match? 'LPAREN)
       (consume 'LPAREN)
       (define expr (parse-expression))
       (consume 'RPAREN)
       expr]
      [else
       (error 'compiler "Syntax Error: Unexpected token ~a ('~a') at line ~a"
              (token-type (current-token))
              (token-value (current-token))
              (token-line (current-token)))]))

  (parse-program))

;; =============================================================================
;; 3. INTERPRETER (WITH 3-VALUED LOGIC)
;; =============================================================================

(struct return-signal (value))

(define (make-env [parent #f])
  (mcons (make-hash) parent))

(define (env-lookup env var-name)
  (let loop ([curr env])
    (if (not curr)
        (error 'runtime "Variable '~a' is undefined" var-name)
        (let ([table (mcar curr)])
          (if (hash-has-key? table var-name)
              (hash-ref table var-name)
              (loop (mcdr curr)))))))

(define (env-set! env var-name val)
  (let loop ([curr env])
    (if (not curr)
        (hash-set! (mcar env) var-name val)
        (let ([table (mcar curr)])
          (if (hash-has-key? table var-name)
              (hash-set! table var-name val)
              (loop (mcdr curr)))))))

(define (env-declare! env var-name [val 0])
  (hash-set! (mcar env) var-name val))

;; Three-valued logic helpers: 'yes, 'no, 'maybe
(define (tri-not v)
  (cond
    [(eq? v 'yes) 'no]
    [(eq? v 'no) 'yes]
    [else 'maybe]))

(define (tri-and a b)
  (cond
    [(or (eq? a 'no) (eq? b 'no)) 'no]
    [(and (eq? a 'yes) (eq? b 'yes)) 'yes]
    [else 'maybe]))

(define (tri-or a b)
  (cond
    [(or (eq? a 'yes) (eq? b 'yes)) 'yes]
    [(and (eq? a 'no) (eq? b 'no)) 'no]
    [else 'maybe]))

(define (eval-expr expr env procs)
  (match expr
    [`(int ,n) n]
    [`(real ,n) (exact->inexact n)]
    [`(string ,s) s]
    [`(bool ,b) b]
    [`(id ,name) (env-lookup env name)]

    [`(not ,sub)
     (let ([v (eval-expr sub env procs)])
       (if (symbol? v) (tri-not v) (not v)))]

    [`(relop ,op ,left ,right)
     (let ([l (eval-expr left env procs)]
           [r (eval-expr right env procs)])
       (match op
         ["==" (if (equal? l r) 'yes 'no)]
         ["/=" (if (not (equal? l r)) 'yes 'no)]
         ["<"  (if (< l r) 'yes 'no)]
         ["<=" (if (<= l r) 'yes 'no)]
         [">"  (if (> l r) 'yes 'no)]
         [">=" (if (>= l r) 'yes 'no)]))]

    [`(addop ,op ,left ,right)
     (let ([l (eval-expr left env procs)]
           [r (eval-expr right env procs)])
       (match op
         ["+" (+ l r)]
         ["-" (- l r)]
         ["~" (string-append (~a l) (~a r))]
         ["or" (tri-or l r)]))]

    [`(mulop ,op ,left ,right)
     (let ([l (eval-expr left env procs)]
           [r (eval-expr right env procs)])
       (match op
         ["*" (* l r)]
         ["/" (/ l r)]
         ["and" (tri-and l r)]))]

    [else (error 'runtime "Cannot evaluate expr: ~a" expr)]))

(define (exec-statement stmt env procs)
  (match stmt
    [`(assign ,expr ,target)
     (let ([val (eval-expr expr env procs)])
       (env-set! env target val))]

    [`(assign-call (call ,routine-name ,arg-exprs) ,target)
     (let ([ret-val (exec-call routine-name arg-exprs env procs)])
       (env-set! env target ret-val))]

    [`(call ,routine-name ,arg-exprs)
     (exec-call routine-name arg-exprs env procs)]

    [`(yield ,expr)
     (raise (return-signal (eval-expr expr env procs)))]

    [`(loop-until ,test-expr ,@body)
     (let loop ()
       (unless (eq? (eval-expr test-expr env procs) 'yes)
         (for ([s body]) (exec-statement s env procs))
         (loop)))]

    [`(choose ,cond-expr (then ,@then-stmts) (otherwise ,@else-stmts))
     (if (eq? (eval-expr cond-expr env procs) 'yes)
         (for ([s then-stmts]) (exec-statement s env procs))
         (for ([s else-stmts]) (exec-statement s env procs)))]

    [`(choose ,cond-expr (then ,@then-stmts))
     (when (eq? (eval-expr cond-expr env procs) 'yes)
       (for ([s then-stmts]) (exec-statement s env procs)))]

    [`(empty-stmt) (void)]
    [else (void)]))

(define (exec-call routine-name arg-exprs env procs)
  (let ([routine-def (hash-ref procs routine-name #f)])
    (unless routine-def
      (error 'runtime "Routine '~a' not found" routine-name))
    (match routine-def
      [`(routine ,_ ,params ,_ (declarations (vars ,@local-vars) (routines ,@local-procs)) ,body-stmts)
       (define local-env (make-env env))
       (define arg-vals (map (lambda (e) (eval-expr e env procs)) arg-exprs))

       (for ([p params] [val arg-vals])
         (match p [`(param ,p-name ,_) (env-declare! local-env p-name val)]))

       (for ([vdecl local-vars])
         (match vdecl
           [`(decl-var ,ids ,_)
            (for ([id ids]) (env-declare! local-env id 0))]))

       (with-handlers ([return-signal? (lambda (sig) (return-signal-value sig))])
         (for ([s body-stmts])
           (exec-statement s local-env procs))
         0)])))

(define (run-program ast)
  (match ast
    [`(program ,prog-name (declarations (vars ,@global-vars) (routines ,@routine-decls)) ,stmts)
     (define global-env (make-env #f))
     (define procs (make-hash))

     (for ([vdecl global-vars])
       (match vdecl
         [`(decl-var ,ids ,t)
          (for ([id ids])
            (env-declare! global-env id
                          (match t
                            ['string ""]
                            ['bool 'no]
                            ['real 0.0]
                            [_ 0])))]))

     (for ([r routine-decls])
       (match r
         [`(routine ,name ,_ ,_ ,_ ,_)
          (hash-set! procs name r)]))

     (for ([s stmts])
       (exec-statement s global-env procs))

     (displayln (format "=== Execution Result for Program '~a' ===" prog-name))
     (for ([(var val) (in-hash (mcar global-env))])
       (printf "  ~a = ~v~n" var val))]))

;; =============================================================================
;; 4. COMPILE AND RUN
;; =============================================================================

(define (compile-and-run source-str)
  (with-handlers ([exn:fail? (lambda (e)
                               (displayln (exn-message e))
                               #f)])
    (define tokens (tokenize source-str))
    (define ast (parse tokens))
    (displayln "Compilation Successful: OK")
    (run-program ast)
    #t))

(define (compile-file path)
  (if (file-exists? path)
      (compile-and-run (file->string path))
      (error 'compile-file "File not found: ~a" path)))

;; File path using forward slashes to avoid Windows backslash escape issues
(define test-file-path
  "C:/Users/sylvi/OneDrive/Desktop/School/CS 441/Program 2 Test Files/Testfile-1.txt")

(define (run-test-file path)
  (printf "Loading and executing: ~a~n" path)
  (if (file-exists? path)
      (let ([source (file->string path)])
        (compile-and-run source))
      (eprintf "Error: Could not locate file at '~a'~n" path)))

;; Execute the test file
(run-test-file test-file-path)