#lang racket
(require syntax/stx)
(require "pasterack-utils.rkt")

;; parsing utility functions used by pasterack.org

(provide (all-defined-out))

;; lang regexp patterns
(define hashlang-pat #px"^\\#lang ([\\w/\\-\\+]+)\\s*(.*)")
(define weblang-pat #px"^web-server.*")
(define scribblelang-pat #px"^scribble/.*")
(define htdplang-pat #px"^htdp/(.*)")
(define TRlang-pat #px"^typed/racket.*")

(define (hashlang? code)
  (define in (open-input-string code))
  (begin0 (read-language in (lambda () #f)) (close-input-port in)))

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

;; htdp form patterns
(define provide-pat #px"^\\(provide (.*)\\)$")
(define require-pat #px"^\\(require (.*)\\)$")
(define define-pat #px"^\\(define(.*)\\)$")
(define check-pat #px"^\\(check-(.*)\\)$")

(define (require-datum? e) (get-require-spec e))
(define (provide-datum? e) (regexp-match provide-pat (to-string e)))
(define (define-datum? e) (regexp-match define-pat (to-string e)))
(define (check-datum? e) (regexp-match check-pat (to-string e)))
(define (get-require-spec e) (regexp-match require-pat (to-string e)))

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
    (fprintf out "expanded: ~a\n" (syntax->datum expanded))
    (fprintf out "hd: ~a\n"  hd)
    (and hd
         ;; check for begin
      (or (and (free-identifier=? hd #'begin)
               (for/and ([s (syntax->list (stx-cdr (expand d)))])
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