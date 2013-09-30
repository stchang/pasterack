#lang racket

(require web-server/servlet
         web-server/dispatch)
(require xml xml/path)
(require racket/system racket/runtime-path)
(require redis)
(provide/contract (start (request? . -> . response?)))

(define-runtime-path tmp-html-file "test.html")
(define-runtime-path tmp-scrbl-file "test.scrbl")
(define-runtime-path htdocs-dir "htdocs")
(define-runtime-path here ".")

(define +++ string-append)

(define pastebin-url "http://www.pasterack.org/")
(define paste-url-base (+++ pastebin-url "pastes/"))
(define racket-docs-url "http://docs.racket-lang.org/")
(define racket-lang-url "http://racket-lang.org")
(define racket-logo-url "http://racket-lang.org/logo.png")

(define (mk-link url txt)
  `(a ((href ,url) (onclick ,(+++ "top.location.href=\"" url "\""))) ,txt))

(define (write-scribble-file code)
  (with-output-to-file tmp-scrbl-file
    (lambda ()
      (printf (+++ "#lang scribble/manual\n"
                   "@(require (for-label racket))\n"
                   "@(require scribble/eval)\n"
                   "@(define the-eval (make-base-eval))\n"
;                   "@codeblock[#:line-numbers 0]{\n~a}")
                   "@codeblock{\n~a}\n"
                   "@interaction-eval-show[#:eval the-eval ~a]")
              code code))
    #:mode 'text
    #:exists 'replace))
(define (compile-scribble-file code)
  (write-scribble-file code)
  (system (+++ "/home/stchang/pltpkg/racket/bin/scribble --html +m "
;               "++xref-in setup/xref load-collections-xref "
               "--redirect-main " racket-docs-url " "
               "--dest " (path->string here) " "
               (path->string tmp-scrbl-file))))


(define (serve-home request)
  (define (response-generator embed/url)
    (response/xexpr
     `(html
       (head (title "PasteRack: The Racket pastebin."))
       (body
        (center
         (img ((src ,racket-logo-url)))
         (h1 "PasteRack: The "
             ,(mk-link racket-lang-url "Racket") " pastebin.")
         (form ((action ,(embed/url process-paste)) (method "post"))
               (textarea ((rows "20") (cols "79") (name "paste")))
               (br)
               (input ((type "submit") (value "Submit Paste")))))))))
  (send/suspend/dispatch response-generator))

(define (mk-rand-str)
  (bytes->string/utf-8 (list->bytes (for/list ([n 4]) (+ 49 (random 9))))))
(define (fresh-str)
  (let loop ()
    (define str (mk-rand-str))
    (if (EXISTS str) (loop) str)))

(define (process-paste request)
  (define bs (request-bindings request))
  (cond
   [(exists-binding? 'paste bs)
    (define str (fresh-str))
    (define paste-url (+++ paste-url-base str))
    (SET str (extract-binding/single 'paste bs))
    (response/xexpr
     `(html ()
        (head ()
          (script () ,(+++ "top.location.href=\"" paste-url "\"")))
        (body ())))]
   [else
    (response/xexpr
     `(html ()
        (head ())
        (body () "ERROR" ,(mk-link pastebin-url "Go Back"))))]))

(define (serve-paste request pastenum)
  (define code (GET/str pastenum))
  (cond
   [(not code)
    (response/xexpr
     `(html() (head ())
        (body ()
         ,(format "Paste # ~a doesn't exist." pastenum) (br)
         ,(mk-link pastebin-url "Go Back"))))]
   [else
    (compile-scribble-file code)
    (define main-div
      (car (filter
            (lambda (d) (equal? "main" (se-path* '(div #:class) d)))
            (se-path*/list '(div)
              (xml->xexpr (document-element
                           (with-input-from-file tmp-html-file read-xml)))))))
    (define paste-url (string-append paste-url-base pastenum))
    (response/xexpr
     `(html ()
        (head ()
          (meta ((content "text-html; charset=utf-8")
                 (http-equiv "content-type")))
          (title ())
          (link ((href "/scribble.css")       (rel "stylesheet")
                 (title "default")            (type "text/css")))
          (link ((href "/racket.css")         (rel "stylesheet")
                 (title "default")            (type "text/css")))
          (link ((href "/scribble-style.css") (rel "stylesheet")
                 (title "default")            (type "text/css")))
        (script ((src "/scribble-common.js")  (type "text/javascript"))))
      (body
       ((id "scribble-racket-lang-org"))
       "Paste # " (a ((href ,paste-url)) ,pastenum)
       (div ((class "maincolumn"))
        ,(match main-div
           [`(div ((class "main")) ,ver (p () ,code ,res))
            `(div ((class "main")) (p () ,code (div "=>") ,res))])))))]))
        ;; ,(cons (car main-div) (cons (cadr main-div) (cdddr main-div)))))))]))
         ;; (div ((class "main"))
         ;;   (blockquote ((class "SCodeFlow"))
         ;;    ,(se-path* '(blockquote) doc-xexpr)))))))]))

(define-values (do-dispatch mk-url)
  (dispatch-rules
   [("") serve-home]
   [("pastes" (string-arg)) serve-paste]
   #;[else serve-home]))

(current-redis-connection (connect))

(define (start request) (do-dispatch request))

(require web-server/servlet-env)
(serve/servlet start
               #:launch-browser? #f
               #:quit? #f
               #:listen-ip #f
               #:port 8000
               #:extra-files-paths (list htdocs-dir)
               #:servlet-path "/"
               #:servlet-regexp #rx".*")
