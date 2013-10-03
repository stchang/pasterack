#lang racket

(require web-server/servlet web-server/dispatch)
(require xml xml/path)
(require racket/system racket/runtime-path)
(require redis data/ring-buffer)
(require "pasterack-utils.rkt")
(provide/contract (start (request? . -> . response?)))

(define-runtime-path htdocs-dir "htdocs")
(define-runtime-path here ".")
(define-runtime-path tmp-dir "tmp")

(define pastebin-url "http://www.pasterack.org/")
(define paste-url-base (++ pastebin-url "pastes/"))
(define racket-docs-url "http://docs.racket-lang.org/")
(define racket-lang-url "http://racket-lang.org")
(define racket-logo-url "http://racket-lang.org/logo.png")

(define (mk-paste-url paste-num) (++ paste-url-base paste-num))

;; the top.location breaks out of the current frame
(define (mk-link url txt)
  `(a ((href ,url) (onclick ,(++ "top.location.href=\"" url "\""))) ,txt))

(define (fresh-str)
  (let loop () (define str (mk-rand-str)) (if (EXISTS str) (loop) str)))

(define sample-pastes
  '("4557" ; Sierpinski
    "9545" ; div1
    "3516" ; circles (test require)
    "3289" ; Greek letters
    "2531" ; lazy fib
    "7747" ; set bang (test multi-expr, no #lang)
    "2417" ; scribble syntax
    "9425" ; big bang (test 2 requires on 1 line)
    "9265" ; typed/racket
    "8937" ; datalog
    "2979" ; test limits, and forms in racket but not racket/base
    "7169" ; racket/gui
    "5352" ; test 2 specs in 1 require
    "1216" ; another typed/racket
    "6813" ; ffi
    "5752" ; bs ipsum (as text)
    ))

(define NUM-RECENT-PASTES 32)
(define recent-pastes (empty-ring-buffer NUM-RECENT-PASTES))
(for ([p sample-pastes]) (ring-buffer-push! recent-pastes p))

;; lang regexp patterns
(define hashlang-pat #px"^\\#lang ([\\w/-]+)\\s*(.*)")
(define weblang-pat #px"^web-server.*")
(define scribblelang-pat #px"^scribble/.*")
(define htdplang-pat #px"^htdp/.*")
(define TRlang-pat #px"^typed/racket.*")
(define require-pat #px"^\\(require (.*)\\)$")
(define (hashlang? code)
  (define in (open-input-string code))
  (begin0 (read-language in (const #f)) (close-input-port in)))
;; returns two string values, one for lang and one for the rest of the program
(define (hashlang-split code)
  (match (regexp-match hashlang-pat code)
    [(list _ lang rst) (values lang rst)]
    [_ (values "racket" code)]))
(define (scribble-lang? lang) (regexp-match scribblelang-pat lang))
(define (htdp-lang? lang) (regexp-match htdplang-pat lang))
(define (TR-lang? lang) (regexp-match TRlang-pat lang))
(define (web-lang? lang) (regexp-match weblang-pat lang))
(define (require-datum? e) (get-require-spec e))
(define (get-require-spec e) (regexp-match require-pat (to-string e)))

;; returns generated pastenum
(define (write-codeblock-scrbl-file code)
  (define tmp-name (mk-rand-str))
  (define tmp-scrbl-file (build-path tmp-dir (++ tmp-name ".scrbl")))
  (define-values (lang code-no-lang) (hashlang-split code))
  (define lang-lst
    (cond [(scribble-lang? lang) (list "racket" lang)]
          [(htdp-lang? lang) (list "racket")]
          [(TR-lang? lang) (list "racket")]
          [(web-lang? lang) (list "web-server" "web-server/http")]
          [else (list lang)]))
  (define reqs   
    (with-handlers ([exn:fail? (const null)])  ;; read fail = non-sexp syntax
      (with-input-from-string code-no-lang
        (lambda () (for/list ([e (in-port)] #:when (require-datum? e))
                     (second (get-require-spec e)))))))
  (with-output-to-file tmp-scrbl-file
    (lambda () (printf
      (++ "#lang scribble/manual\n"
          "@(require (for-label " (string-join (append lang-lst reqs)) "))\n"
          "@codeblock|{\n~a}|")
      code))
    #:mode 'text
    #:exists 'replace)
  tmp-name)
(define (write-eval-scrbl-file code)
  ; parse out #lang if it's there, otherwise use racket
  (define-values (lang code-no-lang) (hashlang-split code))
  (define tmp-name (mk-rand-str))
  (define tmp-scrbl-file (build-path tmp-dir (++ tmp-name ".scrbl")))
  (with-output-to-file tmp-scrbl-file
    (lambda ()
      (printf
       (++ "#lang scribble/manual\n"
           "@(require scribble/eval racket/sandbox)\n"
           "@(define-namespace-anchor anchor)\n"
           "@(define the-eval\n"
           "   (call-with-trusted-sandbox-configuration\n"
           "    (lambda ()\n"
           "      (parameterize ([sandbox-output 'string]\n"
           "                     [sandbox-error-output 'string]\n"
           "                     [sandbox-propagate-breaks #f]\n"
           "                     [sandbox-namespace-specs "
                                   "(cons "
                            "(lambda () (namespace-anchor->namespace anchor)) "
                            "'(racket/pretty file/convertible))]\n"
           "                    [sandbox-path-permissions '([exists \"/\"])]\n"
           "                    [sandbox-eval-limits '(8 64)])\n"
           "        (let ([e (make-evaluator '" lang ")])\n"
           "            (call-in-sandbox-context e\n"
           "              (lambda ()\n"
           "                (current-print (dynamic-require 'racket/pretty "
                                             "'pretty-print-handler))))\n"
           "          e)))))\n"
           "@interaction[#:eval the-eval\n~a]")
       code-no-lang))
      #:mode 'text
      #:exists 'replace)
  tmp-name)

(define (compile-scrbl-file/get-html name)
  (and
   (system (++ "/home/stchang/pltpkg/racket/bin/scribble --html +m "
               "--redirect-main " racket-docs-url " "
               "--dest " (path->string tmp-dir) " "
               (path->string (build-path tmp-dir (++ name ".scrbl")))))
   (with-input-from-file (build-path tmp-dir (++ name ".html")) port->bytes)))
(define (compile-eval-scrbl-file/get-html name)
  (and
   (system (++ "/home/stchang/pltpkg/racket/bin/scribble --html "
               "--dest " (path->string tmp-dir) " "
               (path->string (build-path tmp-dir (++ name ".scrbl")))))
   (with-input-from-file (build-path tmp-dir (++ name ".html")) port->bytes)))

(define (generate-paste-html code)
  (compile-scrbl-file/get-html (write-codeblock-scrbl-file code)))
(define (generate-eval-html code)
  (compile-eval-scrbl-file/get-html (write-eval-scrbl-file code)))

(define google-analytics-script
  (++ "var _gaq = _gaq || [];\n"
      "_gaq.push(['_setAccount', 'UA-44480001-1']);\n"
      "_gaq.push(['_trackPageview']);\n"
      "(function() {\n"
      "var ga = document.createElement('script'); "
      "ga.type = 'text/javascript'; ga.async = true;\n"
      "ga.src = ('https:' == document.location.protocol "
      "? 'https://ssl' : 'http://www') + '.google-analytics.com/ga.js';\n"
      "var s = document.getElementsByTagName('script')[0];"
      "s.parentNode.insertBefore(ga, s);\n"
      "})();"))

(define (serve-home request)
  (define (response-generator embed/url)
    (response/xexpr
     `(html
       (head
        (title "PasteRack: The Racket pastebin.")
        (script ((type "text/javascript")) ,google-analytics-script))
       (body ((style "margin-top:20px"))
         (div ((style "margin-left:5px;position:relative;float:left;margin-right:-10em"))
           (h4 "Total pastes: " ,(number->string (DBSIZE)))
           (h4 "Sample pastes:")
           (table ((style "margin-top:-15px"))
           ,@(for/list ([pnum sample-pastes])
               (define name (bytes->string/utf-8 (HGET pnum 'name)))
               `(tr (td ,(mk-link (mk-paste-url pnum) pnum))
                    (td ((style "width:1px"))) (td ,name))))
           (h4 "Recent pastes:")
           (table ((style "margin-top:-15px"))
           ,@(reverse
              (for/list ([pnum recent-pastes] #:when pnum)
                (define name (bytes->string/utf-8 (HGET pnum 'name)))
                `(tr (td ,(mk-link (mk-paste-url pnum) pnum))
                     (td ((style "width:1px"))) (td ,name))))))
       (div 
        (center
         (img ((src ,racket-logo-url)))
         (h1 ,(mk-link pastebin-url "PasteRack") ": The "
             ,(mk-link racket-lang-url "Racket") " pastebin.")
         (form ((action ,(embed/url process-paste)) (method "post"))
               (table (tr
                 (td (input ((type "text") (name "name") (size "60"))))
                 (td "(paste title)")))
               (textarea ((rows "32") (cols "80") (name "paste")))
               (br)
               (table (tr (td ((style "width:10em")))
               (td ((style "width:8em"))
                   (input ((type "submit") (value "Submit Paste"))))
               (td (input ((type "checkbox") (name "astext") (value "off")))
               " Submit as text only"))))))
         (div ((style "width:10em;position:relative;float:right")))))))
  (send/suspend/dispatch response-generator))

(define (process-paste request)
  (define bs (request-bindings request))
  (cond
   [(exists-binding? 'paste bs)
    (define paste-num (fresh-str))
    (define paste-name (extract-binding/single 'name bs))
    (define pasted-code (extract-binding/single 'paste bs))
    (define html-res
      (if (exists-binding? 'astext bs) #f (generate-paste-html pasted-code)))
    (define paste-html-str (or html-res pasted-code))
    (define eval-html-str (and html-res (generate-eval-html pasted-code)))
    (define paste-url (mk-paste-url paste-num))
    (ring-buffer-push! recent-pastes paste-num)
    (SET/hash paste-num (hash 'name paste-name
                              'code paste-html-str
                              'eval (or eval-html-str "")
                              'time (get-time/iso8601)))
    (response/xexpr
     `(html ()
        (head ()
          (script () ,(++ "top.location.href=\"" paste-url "\"")))
        (body ())))]
   [else
    (response/xexpr
     `(html ()
        (head ())
        (body () "ERROR: bad paste" ,(mk-link pastebin-url "Go Back"))))]))

(define (get-main-div html-bytes)
  (with-handlers ([exn:fail? (lambda (x) (bytes->string/utf-8 html-bytes))])
    (car (filter
          (lambda (d) (equal? "main" (se-path* '(div #:class) d)))
          (se-path*/list '(div)
            (xml->xexpr (document-element
                         (with-input-from-bytes html-bytes read-xml))))))))

(define (serve-paste request pastenum)
  (define retrieved-paste-hash (GET/hash pastenum #:map-key bytes->symbol))
  (cond
   [(equal? (hash) retrieved-paste-hash)
    (response/xexpr
     `(html() (head ())
        (body ()
         ,(format "Paste # ~a doesn't exist." pastenum) (br)
         ,(mk-link pastebin-url "Go Back"))))]
   [else
    (match-define
     (hash-table ('name paste-name) ('code code-html)
                 ('eval eval-html)  ('time time-str)) retrieved-paste-hash)
    (define code-main-div (get-main-div code-html))
    (define eval-main-div (get-main-div eval-html))
    (define paste-url (string-append paste-url-base pastenum))
    (response/xexpr
     `(html ()
        (head ()
          (meta ((content "text-html; charset=utf-8")
                 (http-equiv "content-type")))
          (title)
          (link ((href "/scribble.css")       (rel "stylesheet")
                 (title "default")            (type "text/css")))
          (link ((href "/racket.css")         (rel "stylesheet")
                 (title "default")            (type "text/css")))
          (link ((href "/scribble-style.css") (rel "stylesheet")
                 (title "default")            (type "text/css")))
          (script ((src "/scribble-common.js")  (type "text/javascript")))
          (script ,(++ "top.document.title=\"Paste" pastenum ":"
                       (bytes->string/utf-8 paste-name) "\"")))
      (body ()
       (div ((style "margin-left:10px;position:relative;float:left"))
         (table ((cellspacing "0") (cellpadding "0"))
           (tr (td ,(mk-link pastebin-url "PasteRack.org")))
           (tr (td ((height "10px"))))
           (tr (td "Paste # " (a ((href ,paste-url)) ,pastenum)))
           (tr (td ((colspan "3")) (small ,(bytes->string/utf-8 time-str))))))
       (div ((style "margin-top:-15px") (class "maincolumn"))
         (h4 ((style "font-family:sans-serif"))
             ,(bytes->string/utf-8 paste-name))
         (br)
        ,(match code-main-div
           [`(div ((class "main")) ,ver ,body)
            `(div ((class "main"))
              ,body
              (p "=>")
              ,(match eval-main-div
                 [`(div ((class "main")) ,ver
                     (blockquote ,attr1 (table ,attr2 . ,results)))
                  `(blockquote ,attr1 (table ,attr2 .
                  ,(filter
                    identity
                    (map
                    (lambda (x)
                      (match x
                        ;; single-line evaled expr (with ">" prompt), skip
                        [`(tr () (td () (span ((class "stt")) ">" " ") . ,rst))
                         #f]
                        ;; multi-line evaled expr
                        [`(tr () (td ()
                            (table ((cellspacing "0")
                                    (class "RktBlk"))
                              (tr () (td () (span ((class "stt")) ">" " ")
                                         . ,rst1)) . ,rst))) #f]
                        ;; void result, skip
                        [`(tr () (td () (table ,attr (tr () (td ()))))) #f]
                        ;; fix filename in image link
                        [`(tr () (td () (p () (img
                            ((alt "image") ,height
                             (src ,filename) ,width)))))
                         ;; rename file to avoid future clashes
                         (define rxmatch
                           (regexp-match #px"^(pict|\\d+)\\_*(\\d+)*\\.png"
                                         filename))
                         (unless rxmatch
                           (error "scribble made non-pict.png ~a" filename))
                         (match-define (list _ base offset) rxmatch)
                         (define new-file
                           (++ pastenum (if offset (++ "_" offset) "") ".png"))
                         (define curr-file-path (build-path tmp-dir filename))
                         (define new-file-path (build-path tmp-dir new-file))
                         (unless (file-exists? new-file-path)
                           (copy-file curr-file-path new-file-path)
                           (delete-file curr-file-path))
                         `(tr () (td () (p () (img
                            ((alt "image") ,height
                             (src ,(++ "/" new-file)) ,width)))))]
                        [x x]))
                    results))))]
                 [_ `(div (pre ,eval-main-div))]))]
           [_ `(div (pre ,code-main-div))])))))]))

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
               #:extra-files-paths (list htdocs-dir tmp-dir)
               #:servlet-path "/"
               #:servlet-regexp #rx".*")
