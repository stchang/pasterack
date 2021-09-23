#lang racket

(require web-server/servlet web-server/dispatch
         web-server/http/request-structs)
(require xml xml/path net/url net/uri-codec json "recaptcha.rkt"
         "spam.rkt")
(require racket/system racket/runtime-path syntax/modread)
(require redis data/ring-buffer lang-file/read-lang-file)
(require "pasterack-utils.rkt" "pasterack-parsing-utils.rkt"
         "pasterack-test-cases.rkt" "filter-pastes.rkt")

(provide/contract (start (request? . -> . response?)))

(define-runtime-path htdocs-dir "htdocs")
(define-runtime-path here-dir ".")
(define-runtime-path tmp-dir "tmp")

;(define pastebin-url "http://162.243.38.241:8000/")
(define pastebin-url "http://pasterack.org/")
;(define pastebin-url "http://143.198.140.118/")
(define paste-url-base (++ pastebin-url "pastes/"))
(define racket-docs-url "http://docs.racket-lang.org/")
(define racket-lang-url "http://racket-lang.org")
(define racket-logo-url "http://racket-lang.org/logo.png")
(define racket-irc-url "https://botbot.me/freenode/racket/")

(define scrbl-exe "/home/pasterack/racket82/bin/scribble")

(define PASTE-TITLE-DISPLAY-LEN 32) ; limit length of displayed title

(define (mk-paste-url paste-num) (++ paste-url-base paste-num))

;(define (mk-link url txt) `(a ((href ,url)) ,txt))

(define (fresh-str)
  (with-redis-connection
   (let loop () (define str (mk-rand-str)) (if (EXISTS str) (loop) str))))

;; logging
(define log-file (build-path here-dir "pasterack.log"))
(define log-port (open-output-file log-file #:mode 'text #:exists 'append))

(unless (getenv "PLT_TR_NO_OPTIMIZE")
  (putenv "PLT_TR_NO_OPTIMIZE" "1"))

(define sample-pastes
  '("8953" ; Sierpinski
    "5563" ; Greek letters
    "4837" ; lazy fib
    "3259" ; scribble syntax
    "8314" ; check-expect
    "7435" ; #lang htdp/bsl + 2htdp/image
    "3883" ; echo serv, test limits, and forms in racket but not racket/base
    "7658" ; typed/racket
    "97561"; plot
    "29314"; fish pict
    ))

(define sample-pastes-htmls
  (let ([ns (with-redis-connection
             (do-MULTI (for ([p sample-pastes]) (send-cmd 'HGET p 'name))))])
    (for/list ([name/bytes ns] [pnum sample-pastes])
      (define name (bytes->string/utf-8 name/bytes))
      `(tr (td ,(mk-link (mk-paste-url pnum) pnum))
           (td ((style "width:1px"))) (td ,name)))))

(define NUM-RECENT-PASTES 16)
(define recent-pastes (empty-ring-buffer NUM-RECENT-PASTES))
(for ([p test-cases]) (ring-buffer-push! recent-pastes p))

(define TR-bad-ids
  (++ "#%module-begin with-handlers lambda λ #%top-interaction for for* "
      "define default-continuation-prompt-tag struct case-lambda let-values "
      "letrec-values for*/product let let* letrec define-struct for*/lists "
      "for*/hasheqv let/cc do for/and for/sum for/hasheq for/lists for*/and "
      "for*/hasheq for*/vector for/or for/hasheqv for*/last for*/or for/last "
      "for*/sum for/first for*/fold for/product for/hash for*/list let/ec "
      "for/list for/vector for*/hash for/fold for*/first let*-values"))
(define plai-bad-ids "#%module-begin provide")

;; returns generated pastenum
(define (write-codeblock-scrbl-file code modu pnum)
  (define tmp-scrbl-file (build-path tmp-dir pnum (++ pnum "code.scrbl")))
  (define mod
    (with-handlers
      ([exn:fail?
        (lambda (e)
          ; most likely missing #lang
          (eprintf "~a\n\nCheck that paste includes #lang?" (exn-message e))
          #'(module m racket/base (#%module-begin)))])
      (check-module-form modu 'pasterack (++ "paste " pnum))))
  (define lang (get-module-lang mod))
  (define reqs   
    (with-handlers ([exn:fail? (const null)])  ;; read fail = non-sexp syntax
      (get-module-reqs mod)))
  (define valid-reqs 
    (string-join 
     (map to-string/s
	  (filter 
	   valid-req? 
	   (map syntax->datum reqs)))))
  (with-output-to-file tmp-scrbl-file
    (lambda () (printf
      (++ "#lang scribble/manual\n"
          "@(require racket/require)\n"
          "@(require (for-label "
          ;; when required id is also in lang, favor require (with subtract-in)
          (++ "(only-meta-in 0 "
              valid-reqs " "
              "(subtract-in (combine-in "
              (symbol->string (syntax->datum lang))
              ")"
              " (combine-in " valid-reqs ")))")
          "))\n"
          "@codeblock|{\n~a}|")
      code))
    #:mode 'text
    #:exists 'replace))
(define (write-eval-scrbl-file code mod pnum)
  (define lang (symbol->string (syntax->datum (get-module-lang mod))))
  (define tmp-scrbl-file (build-path tmp-dir pnum (++ pnum "eval.scrbl")))
  (define out (current-output-port))
  (with-output-to-file tmp-scrbl-file
    (lambda ()
      (cond
       ;; htdp lang only --------------------------------------------------
       [(htdp-lang? lang)
        ;; separate code into exprs and other things
        ;; - exprs get evaled by interaction (ow pictures dont work)
        ;; - other top-level defs get included in make-module-evaluator
        ;; because they are not allowed in interactions
        (define-values (code-defs/checks code-exprs)
          (partition not-htdp-expr? (map syntax->datum (get-module-bodys mod))))
      (printf
       (++ "#lang scribble/manual\n"
           "@(require scribble/eval racket/sandbox)\n"
           "@(define-namespace-anchor anchor)\n"
           "@(define the-eval\n"
           "      (parameterize ([sandbox-output 'string]\n"
           "                     [sandbox-error-output 'string]\n"
           "                     [sandbox-propagate-breaks #f]\n"
           "                     [sandbox-namespace-specs "
                                   "(cons "
                            "(lambda () (namespace-anchor->namespace anchor)) "
                            "'(racket/pretty file/convertible))]\n"
           "                    [sandbox-path-permissions "
                         "'([read \"/home/pasterack/pasterack/tmp/\"]\n"
                         ;; images seem to need access to the prefs file
                           "[read \"/home/pasterack/.racket/\"]\n"
                           "[read \"/home/pasterack/.config/\"]\n" ; prefs moved here? (v8?)
                           ;; 2htdp/image performs exists? checks on libpng
                           "[exists \"/\"])]\n"
           "                    [sandbox-eval-limits '(20 128)])\n"
           "        (let ([e (make-module-evaluator "
           "'(module m " lang
           " (require test-engine/racket-tests) "
           (string-join (map to-string/s code-defs/checks))
           " (test))"
           ")])\n"
           "            (call-in-sandbox-context e\n"
           "              (lambda ()\n"
           "                (current-print (dynamic-require 'racket/pretty "
                                             "'pretty-print-handler))))\n"
           "          e)))\n"
           "@interaction[#:eval the-eval\n"
           (string-join (map to-string/s code-exprs))
           " (test)]"))]
       ;; non htdp lang --------------------------------------------------
       [else
        (define datums (map syntax->datum (get-module-bodys mod)))
;        (for ([d datums]) (fprintf out "~a\n" d))
        (define-values (mod-datums expr-datums)
          (parameterize ([current-namespace (make-base-namespace)])
            (eval `(require ,(string->symbol lang)))
            (partition (lambda (d) (not-expr? d out)) datums)))
        ;; (fprintf out "mod datums: ~a\n" (string-join (map to-string/s mod-datums)))
        ;; (fprintf out "expr datums: ~a\n" (string-join (map to-string/s expr-datums)))
        (display
;        (printf
         (++ "#lang scribble/manual\n"
             "@(require scribble/eval racket/sandbox)\n"
             "@(define-namespace-anchor anchor)\n"
             "@(define the-eval\n"
             "      (parameterize ([sandbox-output 'string]\n"
             "                     [sandbox-error-output 'string]\n"
             "                     [sandbox-propagate-breaks #f]\n"
             "                     [sandbox-namespace-specs "
                               "(cons "
                            "(lambda () (namespace-anchor->namespace anchor)) "
                            "'(racket/pretty file/convertible))]\n"
             "                    [sandbox-path-permissions "
                         "'([read \"/home/pasterack/pasterack/tmp/\"]"
                           "[read \"/home/pasterack/.racket/\"]\n"
                           "[read \"/home/pasterack/.config/\"]\n" ; prefs moved here? (v8?)
                           ;; 2htdp/image performs exists? checks on libpng
                           "[exists \"/\"])]\n"
             "                     [sandbox-eval-limits '(20 128)])\n"
;           "        (let ([e (make-evaluator '" lang ")])\n"
           "        (let ([e (make-module-evaluator "
           "'(module m " lang "\n"
           (string-join (map to-string/s mod-datums))
           "))])\n"
           "            (call-in-sandbox-context e\n"
           "              (lambda ()\n"
           "                (current-print (dynamic-require 'racket/pretty "
                                             "'pretty-print-handler))))\n"
           "          e)))\n"
       ;;     "@interaction[#:eval the-eval\n~a]")
       ;; code-no-lang)]))
           "@interaction[#:eval the-eval\n(void)\n"
           (string-join (map to-string/s expr-datums))
           "]")
       )]))
      #:mode 'text
      #:exists 'replace))

(define (compile-scrbl-file/get-html pnum)
  (define new-tmpdir (build-path tmp-dir pnum))
  (define scrbl-file (build-path new-tmpdir (++ pnum "code.scrbl")))
  (define html-file (build-path new-tmpdir (++ pnum "code.html")))
  (and (system (++ scrbl-exe " --html "
                   "+m --redirect-main " racket-docs-url " "
                   "--dest " (path->string new-tmpdir) " "
                   (path->string scrbl-file)))
        (with-input-from-file html-file port->bytes)))
(define (compile-eval-scrbl-file/get-html pnum)
  (define new-tmpdir (build-path tmp-dir pnum))
  (define scrbl-file (build-path new-tmpdir (++ pnum "eval.scrbl")))
  (define html-file (build-path new-tmpdir (++ pnum "eval.html")))
  (and (system (++ scrbl-exe " --html "
                   "--dest " (path->string new-tmpdir) " "
               (path->string scrbl-file)))
       (with-input-from-file html-file port->bytes)))

;; files/directories layout ---------------------------------------------------
;; web-server files are in htdocs/
;; each paste creates a directory tmp/<pastenum>
;; - scrbl for code is tmp/<pastenum>/<pastenum>code.scrbl
;; - compiled code is  tmp/<pastenum>/<pastenum>code.html
;; -- if code scrbl file couldn't be compiled, then error is in
;;    tmp/<pastenum>/<pastenum>code.err
;; - scrbl for eval is tmp/<pastenum>/<pastenum>eval.scrbl
;; - compiled eval is  tmp/<pastenum>/<pastenum>eval.html
;; -- if eval results in 1 pict: tmp/<pastenum>/pict.png
;; -- if eval results in n picts: tmp/<pastenum>/pict_1.png through pict_n.png

;; generate-paste-html : String Syntax String -> HTML
;; code = pasted code
;; mod = `read`ed pasted code
(define (generate-paste-html code mod pastenum)
  (define paste-dir (build-path tmp-dir pastenum))
  (unless (directory-exists? paste-dir) (make-directory paste-dir))
  ;; read.err contains errs before the compile, eg missing #lang
  ;; needed otherwise extraneous scribble warnings will be displayed
  (define read-err (open-output-file (build-path paste-dir (++ pastenum "read.err"))))
  (parameterize ([current-error-port read-err])
    (write-codeblock-scrbl-file code mod pastenum))
  (close-output-port read-err)
  (define err (open-output-file (build-path paste-dir (++ pastenum "code.err"))))
  (parameterize ([current-error-port err])
    (begin0 (compile-scrbl-file/get-html pastenum)
            (close-output-port err))))
(define (generate-eval-html code mod pastenum)
  ;; should check that tmp/pastenum dir exists here
  (write-eval-scrbl-file code mod pastenum)
  (compile-eval-scrbl-file/get-html pastenum))

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
(define twitter-script
  (++ "!function(d,s,id){"
      "var js,fjs=d.getElementsByTagName(s)[0],"
      "p=/^http:/.test(d.location)?'http':'https';"
      "if(!d.getElementById(id)){"
      "js=d.createElement(s);js.id=id;"
      "js.src=p+'://platform.twitter.com/widgets.js';"
      "fjs.parentNode.insertBefore(js,fjs);}}"
      "(document, 'script', 'twitter-wjs');"))

(define codemirror-script
  "var codeMirror = CodeMirror.fromTextArea(document.getElementById(\"paste\"),\
     { lineNumbers : true, matchBrackets : true, theme: \"neat\" }
   );")

(define droidsansmono-css/x
  '(link ([type "text/css"] [rel "stylesheet"]
          [href "http://fonts.googleapis.com/css?family=Droid+Sans+Mono"])))
(define ptsans-css/x
  '(link ([type "text/css"] [rel "stylesheet"]
          [href "http://fonts.googleapis.com/css?family=PT+Sans"])))
(define scrbl-css/x
  '(link ([type "text/css"] [rel "stylesheet"]
          [href "http://pasterack.org/scribble.css"])))
(define scrbl-style-css/x
  '(link ([type "text/css"] [rel "stylesheet"]
          [href "http://pasterack.org/scribble-style.css"])))
(define rkt-css/x
  '(link ([type "text/css"] [rel "stylesheet"]
          [href "http://pasterack.org/racket.css"])))


;; generate SUBMIT button image
;; (require images/icons/control)
;; (require images/icons/style)
;; (require pict racket/draw)
;; (send 
;;  (pict->bitmap
;;   (cc-superimpose
;;    (bitmap (record-icon #:color (make-object color% 64 64 255) #:height 64
;;                         #:material glass-icon-material))
;;    (bitmap (play-icon #:color light-metal-icon-color #:height 32
;;                       #:material metal-icon-material))))
;;  save-file "htdocs/submit.png" 'png)

(define-syntax-rule (~~ prop ...) (string-join (list prop ...) ";"))

;; ----------------------------------------------------------------------------
;; serve home -----------------------------------------------------------------
(define (serve-home request #:title [title ""]
                            #:content [content "#lang racket"]
                            #:fork-from [fork-from ""]
                            #:status [status ""])
  (define (response-generator embed/url)
    (response/xexpr
     `(html ([style ,(~~ "background-image:url('/plt-back.1024x768.png')"
                         "background-attachment:fixed"
                         "background-size:cover")])
       ;; head ----------------------------------------------------------------
       (head
;        (title "PasteRack (MIRROR): A Racket-evaluating pastebin")
        (title "PasteRack: A Racket-evaluating pastebin")
        (script ([type "text/javascript"]) ,google-analytics-script)
        (script ([src "https://www.google.com/recaptcha/api.js"]))
	,droidsansmono-css/x ,ptsans-css/x
        ;; expects a codemirror.js script and its scheme mode in htdocs
        (script ([src "/codemirror.js"] [type "text/javascript"]))
        (link ((rel "stylesheet") (href "/codemirror.css")))
        (link ((rel "stylesheet") (href "/neat.css")))
        (style ,(string-append ".CodeMirror { text-align: left; background: #FFFFF0;"
                               " font-size: 15px; height: 35em;"
                               " font-family: Droid Sans Mono, monospace;"
                               " border: thin gray inset; width: 50em; }"))
	)
       ;; body ----------------------------------------------------------------
       (body ((style "font-family:'PT Sans',sans-serif"))
;       (h1 "MIRROR")
        ;; left --------------------------------------------------------------
        (div ((style ,(~~ "position:absolute;left:1em;top:2em"
                          "width:12em"
                          "font-size:95%")))
          (h4 "Total pastes: " ,(number->string (DBSIZE)))
          (h4 "Sample pastes:")
          (table ((style "margin-top:-15px;font-size:95%"))
                 ,@sample-pastes-htmls)
          (h4 "Recent pastes:")
          (table ((style "margin-top:-15px;font-size:95%"))
          ,@(reverse
             (with-redis-connection
               (for/list ([pnum recent-pastes] #:when pnum
                          #:when (HGET/str pnum 'name))
                (define name (HGET/str pnum 'name))
                (define trunc-name
                  (string-truncate name PASTE-TITLE-DISPLAY-LEN))
                `(tr (td ,(mk-link (mk-paste-url pnum) pnum))
                     (td ((style "width:1px"))) (td ,trunc-name)))))))
        ;; middle ------------------------------------------------------------
        (div ((style ,(~~ "position:absolute;left:14em;top:2em")))
         (center
          (img ((src ,racket-logo-url)))
          (h2 ,(mk-link pastebin-url "PasteRack")
              ": An evaluating pastebin for "
              ,(mk-link racket-lang-url "Racket") (small " (v" ,(version) ")"))
          (form ([action ,(embed/url check-paste)] [method "post"])
            (div 
              (input ([type "text"] [name "name"] [size "60"] [value ,title]
                      [style ,(~~ "background-color:#FFFFF0"
                                  "border:inset thin"
                                  "font-size:105%"
                                  "font-family:'PT Sans',sans-serif")]))
              (span ([style "font-size:90%"]) " (paste title)"))
            (br)
            (textarea ([id "paste"] [name "paste"]) ,content)
            ;; run script after textarea is evaluated
            (script ([type "text/javascript"]) ,codemirror-script)
            (input ([type "hidden"] [name "fork-from"] [value ,fork-from]))
            (br)
            (table (tr
              ;; submit button -------------
              (td ((style "width:5em"))
                  (input ([type "image"] [alt "Submit Paste and Run"]
                          [src "/submit.png"])))))
            (span ,status)
            (br)
            (span "Paste must be a valid #lang program.")
            (br)
            (span ,(if (string=? "" fork-from) ""
                       `(span "Forked from paste # " 
                         ,(mk-link 
                              (++ paste-url-base fork-from) fork-from))))
            (br)
            (div ([class "g-recaptcha"] 
                  [data-sitekey "6LdM0wYTAAAAAJPls_eNV28XvCRMeaf1cDoAV4Qx"])
                 "Please check the box:"))
         (br)(br)(br)
         ;; middle bottom (part of middle) ------------------------------------
         (div ([style "font-size:small;color:#808080"])
           "Powered by " ,(mk-link racket-lang-url "Racket") ". "
           "View "
           ,(mk-link "https://github.com/stchang/pasterack" "source") "."
           " Report issues or suggestions "
           ,(mk-link "https://github.com/stchang/pasterack/issues" "here") ". "
            "Inspired by "
            ,(mk-link "https://github.com/samth/paste.rkt" "paste.rkt") "."
            )
         ))
         ))))
  (send/suspend/dispatch response-generator))

(define (serve-home/compat request content [title #f])
  (if title
      (serve-home request #:title title
                          #:content content)
      (serve-home request #:content content)))

(define (check-paste request)
  (define bs (request-bindings request))
  (define name (extract-binding/single 'name bs))
  (define as-text? (exists-binding? 'astext bs))
  (define captcha-token (extract-binding/single 'g-recaptcha-response bs))
  (define paste-content (extract-binding/single 'paste bs))
  (define fork-from (extract-binding/single 'fork-from bs))
  (define lang/#f ; ban pastes that are not valid #lang program
    (call-with-input-string paste-content read-lang))
  (define-values (status headers captcha-success-in)
    (http-sendrecv/url 
      (string->url "https://www.google.com/recaptcha/api/siteverify")
      #:method "POST"
      #:data (alist->form-urlencoded
               (list (cons 'secret RECAPTCHA-SECRET)
                     (cons 'response captcha-token)
                     (cons 'remoteip (request-client-ip request))))
      #:headers '("Content-Type: application/x-www-form-urlencoded")))
  (define captcha-success?
    (hash-ref (read-json captcha-success-in) 'success #f))
  ;; very basic spam filter TODO: move check to client-side?
  (if (and lang/#f
           captcha-success?
           (not (contains-banned? name))
           (not (contains-banned? paste-content)))
      (process-paste request as-text?)
      (serve-home request 
                  #:title name
                  #:content paste-content
                  #:fork-from fork-from
                  #:status '(span "Invalid paste: possibly captcha failed or invalid #lang program."))))

(define (process-paste request [as-text? #f])
  (define bs (request-bindings request))
  (cond
   [(exists-binding? 'paste bs)
    (define paste-num (fresh-str))
    (define paste-name (extract-binding/single 'name bs))
    (define pasted-code (extract-binding/single 'paste bs))
    (define fork-from (extract-binding/single 'fork-from bs))
    (define mod-stx
      (with-handlers ([exn:fail?
                       (lambda (e)
                         ; either scribble will properly report read error,
                         ; eg, unbalanced parens,
                         ; or #lang missing and check-module-form will catch,
                         ; so just return dummy module to continue
                         #'(module m racket/base (#%module-begin)))])
        (call-with-input-string pasted-code read-lang-module)))
    (define html-res
      (if as-text? #f (generate-paste-html pasted-code mod-stx paste-num)))
    (define paste-html-str (or html-res pasted-code))
    (define read-err-str
      (if as-text? ""
          (with-input-from-file
            (build-path tmp-dir paste-num (++ paste-num "read.err"))
            port->string)))
    ;; html-res = #f means "as text" or scrbl compile fail (ie, read fail)
    ;; only eval on typeset compile success and no other errs
    (define eval-html-str
      (if (and html-res (empty-string? read-err-str))
          (generate-eval-html pasted-code mod-stx paste-num)
          ;; if not, use read error as output,
          ;; unless as-text was explicitly checked
          (cond [as-text? #f]
                [(non-empty-string? read-err-str) read-err-str]
                [else
                 (with-input-from-file
                   (build-path tmp-dir paste-num (++ paste-num "code.err"))
                   port->string)])))
    (define paste-url (mk-paste-url paste-num))
    (ring-buffer-push! recent-pastes paste-num)
    (define tm-str (get-time/iso8601))
    (SET/hash paste-num (hash 'name paste-name
                              'code pasted-code
                              'code-html paste-html-str
                              'eval-html (or eval-html-str "")
                              'time tm-str
                              'fork-from fork-from
                              'views 0))
    (fprintf log-port "~a\t~a\t~a\t~a\n"
             tm-str paste-num paste-name (request-client-ip request))
    (redirect-to paste-url permanently)]
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

(define (serve-raw-paste request pastenum)
  (define retrieved-paste-hash
    (with-redis-connection
     (when (HEXISTS pastenum 'views) (HINCRBY pastenum 'views 1))
     (GET/hash pastenum #:map-key bytes->symbol)))
  (match retrieved-paste-hash
    [(hash-table ('name paste-name) ('code code) ('code-html code-html)
                 ('eval-html eval-html) ('time time-str)
                 ('fork-from fork-from) ('views views))
     (response/xexpr (bytes->string/utf-8 code) #:mime-type #"text/plain; charset=utf-8")]
   [_
    (response/xexpr
     `(html() (head (title "Paste not found"))
        (body ()
         ,(format "Paste # ~a doesn't exist." pastenum) (br)
         ,(mk-link pastebin-url "Go Back"))))]))

(define (serve-paste request pastenum)
  (define retrieved-paste-hash
    (with-redis-connection
     (when (HEXISTS pastenum 'views) (HINCRBY pastenum 'views 1))
     (GET/hash pastenum #:map-key bytes->symbol)))
  (cond
   [(equal? (hash) retrieved-paste-hash)
    (response/xexpr
     `(html() (head (title "Paste not found"))
        (body ()
         ,(format "Paste # ~a doesn't exist." pastenum) (br)
         ,(mk-link pastebin-url "Go Back"))))]
   [else
    (define-values (name code code-html eval-html time-str fork-from views)
      (match retrieved-paste-hash
        [(hash-table ('name paste-name) ('code code) ('code-html code-html)
                     ('eval-html eval-html) ('time time-str)
                     ('fork-from fork-from) ('views views))
         (values (bytes->string/utf-8 paste-name)
                 (bytes->string/utf-8 code)
                 code-html eval-html
                 (bytes->string/utf-8 time-str)
                 (bytes->string/utf-8 fork-from)
                 (bytes->string/utf-8 views))]
        ;; old record layouts
        [(hash-table ('name paste-name) ('code code-html)
                     ('eval eval-html)  ('time time-str))
         (values (bytes->string/utf-8 paste-name)
                 "" code-html eval-html
                 (bytes->string/utf-8 time-str) "" "")]))
    (define code-main-div (get-main-div code-html))
    (define eval-main-div (get-main-div eval-html))
    (define paste-url (string-append paste-url-base pastenum))

    ;; move-image-file: html -> html
    ;; '(img ((alt "image") ,height (src ,filename) ,width))
    ;;     =>
    ;; '(img ((alt "image") ,height (src ,new-filename) ,width))
    ;; side effect: moves pict file from tmp dir to permanent location in htdocs
    (define (move-image-file filename height width
                             [style '(style "")])
      ;; rename file to avoid future clashes
      (define rxmatch
        (regexp-match #px"^(pict|\\d+)\\_*(\\d+)*\\.png"
                      filename))
      (unless rxmatch
        (error "scribble made non-pict.png ~a" filename))
      (match-define (list _ base offset) rxmatch)
      (define new-file
        (++ pastenum (if offset (++ "_" offset) "") ".png"))
      (define curr-file-path
        (build-path tmp-dir pastenum filename))
      (define new-file-path
        (build-path htdocs-dir new-file))
      (unless (file-exists? new-file-path)
        (copy-file curr-file-path new-file-path)
        (delete-file curr-file-path))
      `(img ((alt "image")
         ,height (src ,(++ pastebin-url new-file)) ,style ,width)))
    ;; should be a flat list of elems, even for nested lists
    (define (move-image-files lst)
      (for/list ([elem lst])
        (match elem
          ;; 611 added a "style" field
          [`(img ((alt "image") ,height (src ,filename) ,style ,width))
           (move-image-file filename height width style)]
          [`(img ((alt "image") ,height (src ,filename) ,width))
           (move-image-file filename height width)]
          [x x])))
    (define main-html
      (match code-main-div
           [`(div ((class "main")) ,ver
               (blockquote ((class "SCodeFlow"))
                 (table ,table-params . ,rows)))
            (define new-rows
              (map
               (lambda (r)
                 (match r
                   [`(tr () (td () . ,rst))
                    `(li (span ((style "font-family:'Droid Sans Mono',monospace;font-size:125%")) . ,rst))]
                   [_ r]))
               rows))
;            `(div ;((class "main"))
            `(div ([style ,(~~ "font-family:'Droid Sans Mono',monospace"
                              "background-color:transparent")])
          ;    (blockquote ;((class "SCodeFlow"))
                 (ol ((start "0")(style "font-size:70%;color:#A0A0A0"))
                   . ,new-rows)
              (p "=>")
              ,(match eval-main-div
                 [`(div ((class "main")) ,ver
                     (blockquote ,attr1 (table ,attr2 . ,results)))
;                  `(blockquote ,attr1 (table ,attr2 .
                  `(blockquote (table ([style ,(~~ "font-size:90%"
                                                   "table-layout:fixed"
                                                   "width:100%"
                                                   "word-wrap:break-word")]) .
                  ,(filter
                    identity
                    (map ; either rewrites html or produces #f to be filtered
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
                        ;; rewrite filename in image link (1st case):
                        ;; html of img output (for pict) has changed (in 611?)
                        ;; new "style" field added, so handle as separate case
                        [`(tr () (td () (p ()
                            (img ((alt "image")
                              ,height (src ,filename) ,style ,width)))))
                         ;; renames file to avoid future clashes
                         ;; and rewrites html with new filename
                         `(tr () (td () (p ()
                           ,(move-image-file filename height width style))))]
                        ;; fix filename in image link (2nd case)
                        ;; (this was the only case before 611)
                        [`(tr () (td () (p ()
                            (img ((alt "image") ,height (src ,filename) ,width)))))
                         ;; renames file to avoid future clashes
                         ;; and rewrites html with new filename
                         `(tr () (td () (p ()
                           ,(move-image-file filename height width))))]
                        ;; list(s) of images
                        [`(tr () (td () (p ()
                            (span ((class "RktRes")) "'(") . ,rst)))
                         `(tr () (td () (p ()
                            (span ((class "RktRes")) "'(")
                            ,@(move-image-files rst))))]
                        ;; nested table
                        [`(tr () (td () (table ,attrs . ,rows)))
                         `(tr () (td () (table ([style ,(~~ "font-size:95%"
                                                            "table-layout:fixed"
                                                            "width:100%"
                                                            "word-wrap:break-word")])
                                               . ,rows)))]
                        [x x]))
                    results))))]
                 [_ `(div (pre ,eval-main-div))]))]
           [_ `(div (pre ,code-main-div)
                    ,(if (string=? eval-main-div "") ""
                         `(span (p "=>") (pre ,eval-main-div)))
                    )]))
    (serve-home #:content code #:title name #:fork-from pastenum
    (send/suspend
      (lambda (home-url)
    (response/xexpr
     `(html ([style ,(~~ "background-image:url('/plt-back.1024x768.png')"
                         "background-attachment:fixed"
                         "background-size:cover")])
        (head ()
          (meta ((content "text-html; charset=utf-8")
                 (http-equiv "content-type")))
          (title ,(++ "Paste # " pastenum ": " name))
	  ,scrbl-css/x ,rkt-css/x ,scrbl-style-css/x
	  ,droidsansmono-css/x ,ptsans-css/x
          (script ((src "/scribble-common.js")  (type "text/javascript")))
          (script ,twitter-script))
      (body ([style ,(~~ "font-family:'PT Sans',sans-serif"
                         "background-color:transparent")])
       ;; left ----------------------------------------------------------------
       (div ([style "position:absolute;left:1em;top:2em"])
         (table ([cellspacing "0"] [cellpadding "0"])
           (tr (td ,(mk-link pastebin-url "PasteRack.org")))
           (tr (td ((height "10px"))))
           (tr (td "Paste # " (a ((href ,paste-url)) ,pastenum)))
           (tr (td ([colspan "3"] [style "font-size:90%"]) ,time-str))
           (tr (td ,(if (string=? "" fork-from) ""
                             `(span (br) "Forked from paste # "
                             ,(mk-link (++ paste-url-base fork-from) fork-from)
                               "."))))
           (tr (td 
               ,(if (string=? "" code) ""
                    `(span (br) (a ([href ,home-url]) "Fork") " as a new paste."))))
           (tr (td ,(if (string=? "" views) ""
                             `(span (br) "Paste viewed " ,views " time"
                              ,(if (string=? "1" views) "." "s.")))))
           (tr (td (br)
           (a ([href "https://twitter.com/share"][class "twitter-share-button"]
               [data-related "racketlang"][data-dnt "true"]) "Tweet")))
           (tr (td (br) "Embed:"))
           (tr (td (textarea ([rows "2"][cols "16"])
                     ,(xexpr->string scrbl-css/x)
                     ,(xexpr->string rkt-css/x)
                     ,(xexpr->string droidsansmono-css/x)
                     ,(xexpr->string main-html))))))
       ;; middle --------------------------------------------------------------
       (div ((style "position:absolute;left:14em"))
        ,(if (string=? name "") '(br) `(h4 ,name))
        ,main-html)))))) )]))

(define (serve-search request searchpat)
  (response/xexpr
   `(html () (head (title "Search results: " searchpat))
      (body ()
        (table ()
               . ,(for/list ([k (KEYS "*")]
                           #:when (and (string=? (TYPE k) "hash")
                                       (HEXISTS k 'code) ; valid paste
                                       (let ([paste-contents (HGET/str k 'code)]
                                             [paste-name (HGET/str k 'name)])
                                         (or (contains-pat? searchpat paste-contents)
                                             (contains-pat? searchpat paste-name)))))
                 (define pnum (bytes->string/utf-8 k))
                 `(tr (td ,(mk-link (mk-paste-url pnum) pnum))
                      (td ((style "width:1px"))) (td ,(HGET/str k 'name)))))))))

(define (serve-tests request)
  (define test-cases-htmls
    (let ([ns (with-redis-connection
                (do-MULTI (for ([p test-cases]) (send-cmd 'HGET p 'name))))])
      (for/list ([name/bytes ns] [pnum test-cases])
        (define name (bytes->string/utf-8 name/bytes))
        `(tr (td ,(mk-link (mk-paste-url pnum) pnum))
             (td ((style "width:1px"))) (td ,name)))))
  (response/xexpr
   `(html ([style ,(~~ "background-image:url('/plt-back.1024x768.png')"
                       "background-attachment:fixed"
                       "background-size:cover")])
      ;; head ----------------------------------------------------------------
      (head
       (title "PasteRack: Test Cases")
       (link ([type "text/css"] [rel "stylesheet"]
              [href "http://fonts.googleapis.com/css?family=PT+Sans"]))
       (link ([type "text/css"] [rel "stylesheet"]
              [href "http://fonts.googleapis.com/css?family=Droid+Sans+Mono"])))
      ;; body ----------------------------------------------------------------
      (body ((style "font-family:'PT Sans',sans-serif"))
            (div ((style ,(~~ "position:absolute;left:1em;top:2em"
                              "width:20em"
                              "font-size:95%")))
                 (h4 "Test Cases:")
                 (table ((style "margin-top:-15px;font-size:95%"))
                        ,@test-cases-htmls))))))

(require "plt-bacon.rkt")
(define-values (do-dispatch mk-url)
  (dispatch-rules
   [("") serve-home]
   [("paste" (string-arg)) serve-home/compat]
   [("paste" (string-arg) (string-arg)) serve-home/compat]
   [("pastes" (string-arg) "raw") serve-raw-paste]
   [("pastes" (string-arg)) serve-paste]
   [("search" (string-arg)) serve-search]
   [("tests") serve-tests]
   [("bacon") serve-bacon]
   #;[else serve-home]))


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
