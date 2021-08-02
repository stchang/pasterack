#lang racket
(require syntax/stx syntax/parse)
(require "pasterack-utils.rkt")

;; parsing utility functions used by pasterack.org

(provide (all-defined-out))

;; lang regexp patterns
(define hashlang-pat #px"^\\#lang ([\\w/\\-\\+]+)\\s*(.*)")
(define weblang-pat #px"^web-server.*")
(define scribblelang-pat #px"^scribble/.*")
(define htdplang-pat #px"^lang/htdp-(.*)")
(define TRlang-pat #px"^typed/racket.*")
(define plai-pat #px"^plai.*")

(define (hashlang? code)
  (define in (open-input-string code))
  (begin0 (read-language in (lambda () #f)) (close-input-port in)))

;; Returns #t if str has "#lang" somewhere.
(define (has-hashlang? str)
  (regexp-match "#lang" str))

;; ie maps htdp/bsl -> lang/htdp-beginner
(define (htdplang->modulename lang)
  (match (cadr (regexp-match htdplang-pat lang))
    ["bsl"  "lang/htdp-beginner"]
    ["bsl+" "lang/htdp-beginner-abbr"]
    ["isl"  "lang/htdp-intermediate"]
    ["isl+" "lang/htdp-intermediate-lambda"]
    ["asl"  "lang/htdp-advanced"]
    [_ "racket"]))

;; returns two string values, one for lang and one for the rest of the program
(define (hashlang-split code)
  (match (regexp-match hashlang-pat code)
    [(list _ lang rst) (values lang rst)]
    [_ (values "racket" code)]))

(define (scribble-lang? lang) (regexp-match scribblelang-pat lang))
(define (htdp-lang? lang) (regexp-match htdplang-pat lang))
(define (TR-lang? lang) (regexp-match TRlang-pat lang))
(define (web-lang? lang) (regexp-match weblang-pat lang))
(define (plai-lang? lang) (regexp-match plai-pat lang))

;; htdp form patterns
(define provide-pat #px"^\\(provide (.*)\\)$")
(define require-pat #px"^\\(require (.*)\\)$")
(define define-pat #px"^\\(define(.*)\\)$")
(define check-pat #px"^\\(check-(.*)\\)$")

(define (require-datum? e) (get-require-spec e))
(define (provide-datum? e) (regexp-match provide-pat (to-string/s e)))
(define (define-datum? e) (regexp-match define-pat (to-string e)))
(define (check-datum? e) (regexp-match check-pat (to-string e)))
(define (get-require-spec e) (regexp-match require-pat (to-string/s e)))

;; for now, only accept certain forms
;; (ie reject strings)
(define (valid-req? r) 
  (or (symbol? r)
      (and (pair? r)
	   (let ([form (car r)])
	     (define (symeq? x) (eq? x form))
	     (or 
	      (and (ormap symeq? '(only-in except-in rename-in))
		   (valid-req? (second r)))
	      (and (ormap symeq? '(prefix-in))
		   (valid-req? (third r)))
	      (and (ormap symeq? '(combine-in))
		   (andmap valid-req? (cdr r))))))))

(define (not-htdp-expr? e) (or (require-datum? e) (provide-datum? e)
                               (check-datum? e) (define-datum? e)))

;; wont work if s has a #lang line
;; returns list of datums
(define (string->datums s)
  (with-handlers ([exn:fail? (lambda () null)])
    (with-input-from-string s (lambda () (for/list ([e (in-port)]) e)))))

;; stx predicates
(define (not-expr? d [out (current-output-port)])
  (with-handlers ([exn:fail:syntax? (lambda (e) (displayln (exn-message e)) #t)])
    (define expanded (expand-to-top-form d))
    (define hd (and (stx-pair? expanded)
                    ;; not identifier always means %#app, %#datum, or %#top (?)
                    ;; ie, an expression?
                    (identifier? (stx-car expanded))
                    (stx-car expanded)))
    ;; (fprintf out "expanded: ~a\n" (syntax->datum expanded))
    ;; (fprintf out "hd: ~a\n"  hd)
    (and hd
         ;; check for begin
      (or (and (free-identifier=? hd #'begin)
               (for/and ([s (stx->list (stx-cdr (expand d)))])
                 (not-expr? s out)))
          (and
           ;; (when (or (free-identifier=? hd #'define-syntaxes)
           ;;           (free-identifier=? hd #'begin-for-syntax)
           ;;           (free-identifier=? hd #'#%require))
           ;;   (eval d))
           (for/or ([form
                     (syntax->list
                      ;; ok to do define-values from interactions prompt
                      ;; (but set! must be classified same as define-values)
                      #'(module module* begin-for-syntax
                       #%provide #%require define-syntaxes))])
             (free-identifier=? hd form)))))))

;; stx utils
(define (get-module-lang stx)
  (syntax-parse stx #:datum-literals (module)
    [(module _:id lang . mod-beg)
     #'lang]
    [_ #'racket]))
(define (require-stx? stx)
  (syntax-parse stx #:datum-literals (require)
    [(require . _) #t]
    [_ #f]))
;; get-module-reqs : Syntax -> (List Syntax)
(define (get-module-reqs stx)
  (append*
   (for/list ([e (get-module-bodys stx)] #:when (require-stx? e))
     (stx->list (stx-cdr e)))))

;; get-module-bodys : Syntax -> (List Syntax)
(define (get-module-bodys stx)
  (syntax-parse stx
    [(_ name:id lang (mod-beg body ...))
     (stx->list #'(body ...))]
    [_ empty]))
