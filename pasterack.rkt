#lang racket

(require web-server/servlet web-server/dispatch
         web-server/http/request-structs)
(require xml xml/path)
(require racket/system racket/runtime-path)
(require redis data/ring-buffer)
(require "pasterack-utils.rkt" "pasterack-parsing-utils.rkt")

;; irc bot
(require racket-irc/irc/main)
(require racket/async-channel)

(provide/contract (start (request? . -> . response?)))

(define-runtime-path htdocs-dir "htdocs")
(define-runtime-path here ".")
(define-runtime-path tmp-dir "tmp")

(define pastebin-url "http://www.pasterack.org/")
(define paste-url-base (++ pastebin-url "pastes/"))
(define racket-docs-url "http://docs.racket-lang.org/")
(define racket-lang-url "http://racket-lang.org")
(define racket-logo-url "http://racket-lang.org/logo.png")
(define racket-irc-url "https://botbot.me/freenode/racket/")

(define (mk-paste-url paste-num) (++ paste-url-base paste-num))

(define (mk-link url txt) `(a ((href ,url)) ,txt))

(define (fresh-str)
  (with-redis-connection
   (let loop () (define str (mk-rand-str)) (if (EXISTS str) (loop) str))))

;; logging
(define log-file (build-path tmp-dir "pasterack.log"))
(define log-port (open-output-file log-file #:mode 'text #:exists 'append))

;; irc bot
(define-values (connection ready)
  (irc-connect "card.freenode.net" 6667 "pasterack" "pasterack" "pasterack.org"))
(sync ready)
(define irc-channels '("#racktest"))
(for ([chan irc-channels]) (irc-join-channel connection chan))

(define sample-pastes
  '("8953" ; Sierpinski
    "5563" ; Greek letters
    "4837" ; lazy fib
    "1989" ; set bang (test multi-expr, no #lang)
    "3259" ; scribble syntax
    "5238" ; big bang (test 2 requires on 1 line)
    "3883" ; echo serv, test limits, and forms in racket but not racket/base
    "7658" ; typed/racket
    "9269" ; type error
    "2277" ; checkerboard
    "5873")) ; plot
;    "7489")) ; bad syntax
(define sample-pastes-htmls
  (let ([ns (with-redis-connection
             (do-MULTI (for ([p sample-pastes]) (send-cmd 'HGET p 'name))))])
    (for/list ([name/bytes ns] [pnum sample-pastes])
      (define name (bytes->string/utf-8 name/bytes))
      `(tr (td ,(mk-link (mk-paste-url pnum) pnum))
           (td ((style "width:1px"))) (td ,name)))))

(define NUM-RECENT-PASTES 16)
(define recent-pastes (empty-ring-buffer NUM-RECENT-PASTES))
(for ([p sample-pastes]) (ring-buffer-push! recent-pastes p))

(define TR-bad-ids
  (++ "#%module-begin with-handlers lambda λ #%top-interaction for for* "
      "define default-continuation-prompt-tag"))

;; returns generated pastenum
(define (write-codeblock-scrbl-file code pnum)
  (define tmp-scrbl-file (build-path tmp-dir pnum (++ pnum "code.scrbl")))
  (define-values (lang code-no-lang) (hashlang-split code))
  (define lang-lst
    (cond [(scribble-lang? lang) (list "racket" lang)]
          [(htdp-lang? lang) (list (htdplang->modulename lang))]
          [(TR-lang? lang) (list)]
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
          "@(require racket/require)\n"
          "@(require (for-label "
          (cond
           [(TR-lang? lang)
            (++ "(except-in typed/racket " TR-bad-ids ")\n"
                "(only-meta-in 0 (only-in typed/racket " TR-bad-ids "))\n")]
            [else ""])
          (cond
           [(htdp-lang? lang)
            (++ (car lang-lst) " (subtract-in (combine-in "
                (string-join reqs) ") " (car lang-lst) ")")]
           [else (string-join (append lang-lst reqs))])
          "))\n"
          "@codeblock|{\n~a}|")
      code))
    #:mode 'text
    #:exists 'replace))
(define (write-eval-scrbl-file code pnum)
  ; parse out #lang if it's there, otherwise use racket
  (define-values (lang code-no-lang) (hashlang-split code))
  (define tmp-scrbl-file (build-path tmp-dir pnum (++ pnum "eval.scrbl")))
  (with-output-to-file tmp-scrbl-file
    (lambda ()
      (cond
       [(htdp-lang? lang)
        ;; separate code into exprs and other things
        ;; - exprs get evaled by interaction (ow pictures dont work)
        ;; - other top-level defs get included in make-module-evaluator
        ;; because they are not allowed in interactions
        (define-values (code-defs/checks code-exprs)
          (partition not-htdp-expr? (string->datums code-no-lang)))
        (define lang-name (htdplang->modulename lang))
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
           "                    [sandbox-path-permissions '([read \"/\"])]\n"
           "                    [sandbox-eval-limits '(20 128)])\n"
           "        (let ([e (make-module-evaluator "
           "'(module m " lang-name
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
       ;; no htdp lang
       [else
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
             "                    [sandbox-path-permissions '([read \"/\"])]\n"
             "                     [sandbox-eval-limits '(20 128)])\n"
           "        (let ([e (make-evaluator '" lang ")])\n"
           "            (call-in-sandbox-context e\n"
           "              (lambda ()\n"
           "                (current-print (dynamic-require 'racket/pretty "
                                             "'pretty-print-handler))))\n"
           "          e)))\n"
           "@interaction[#:eval the-eval\n~a]")
       code-no-lang)]))
      #:mode 'text
      #:exists 'replace))

(define (compile-scrbl-file/get-html pnum)
  (define new-tmpdir (build-path tmp-dir pnum))
  (define err (open-output-file (build-path new-tmpdir (++ pnum "code.err"))))
  (define scrbl-file (build-path new-tmpdir (++ pnum "code.scrbl")))
  (define html-file (build-path new-tmpdir (++ pnum "code.html")))
  (and (parameterize ([current-error-port err])
         (begin0 (system (++ "/home/stchang/pltpkg/racket/bin/scribble --html "
                             "+m --redirect-main " racket-docs-url " "
                             "--dest " (path->string new-tmpdir) " "
                             (path->string scrbl-file)))
                 (close-output-port err)))
       (with-input-from-file html-file port->bytes)))
(define (compile-eval-scrbl-file/get-html pnum)
  (define new-tmpdir (build-path tmp-dir pnum))
  (define scrbl-file (build-path new-tmpdir (++ pnum "eval.scrbl")))
  (define html-file (build-path new-tmpdir (++ pnum "eval.html")))
  (and (system (++ "/home/stchang/pltpkg/racket/bin/scribble --html "
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

(define (generate-paste-html code pastenum)
  (define paste-dir (build-path tmp-dir pastenum))
  (unless (directory-exists? paste-dir) (make-directory paste-dir))
  (write-codeblock-scrbl-file code pastenum)
  (compile-scrbl-file/get-html pastenum))
(define (generate-eval-html code pastenum)
  ;; should check that tmp/pastenum dir exists here
  (write-eval-scrbl-file code pastenum)
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
        (title "PasteRack: A Racket-evaluating pastebin")
        (script ((type "text/javascript")) ,google-analytics-script)
        (link ([type "text/css"] [rel "stylesheet"]
               [href "http://fonts.googleapis.com/css?family=PT+Sans"]))
        (link ([type "text/css"] [rel "stylesheet"]
               [href "http://fonts.googleapis.com/css?family=Droid+Sans+Mono"])))
       ;; body ----------------------------------------------------------------
       (body ((style "font-family:'PT Sans',sans-serif"))
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
              (for/list ([pnum recent-pastes] #:when pnum)
                (define name (HGET/str pnum 'name))
                `(tr (td ,(mk-link (mk-paste-url pnum) pnum))
                     (td ((style "width:1px"))) (td ,name)))))))
        ;; middle ------------------------------------------------------------
        (div ((style ,(~~ "position:absolute;left:14em;top:2em")))
         (center
          (img ((src ,racket-logo-url)))
          (h2 ,(mk-link pastebin-url "PasteRack")
              ": An evaluating pastebin for "
              ,(mk-link racket-lang-url "Racket") ".")
          (form ((action ,(embed/url process-paste)) (method "post"))
            (div ([style "text-align:left"])
              (input ([type "text"] [name "name"] [size "60"] [value ,title]
                      [style ,(~~ "background-color:#FFFFF0"
                                  "border:inset thin"
                                  "font-size:105%"
                                  "font-family:'PT Sans',sans-serif")]))
              (span ([style "font-size:90%"]) " (paste title)"))
            (textarea ([style ,(~~ "font-family:'Droid Sans Mono',monospace"
                                   "background-color:#FFFFF0"
                                   "border:inset"
                                   "border-width:thin"
                                   "height:32em" "width:50em")]
                       [name "paste"]) ,content)
            (input ([type "hidden"] [name "fork-from"] [value ,fork-from]))
            (br)
            (table (tr
              (td ((style "width:12em")))
              ;; as-text checkbox ----------
              (td (input ([type "checkbox"] [name "astext"] [value "off"])))
              (td ((style "font-size:90%")) " Submit as text only")
              (td ((style "width:10px")))
              ;; submit button -------------
              (td ((style "width:5em"))
                  (input ([type "image"] [alt "Submit Paste and Run"]
                          [src "/submit.png"])))
              ;; paste to irc --------------
              (td ((style "font-size:90%"))
                  (input ([type "checkbox"] [name "irc"] [value "off"]))
                  (span " Alert "
                       ,(mk-link racket-irc-url "#racket")
                       " channel; your name/nick: ")
                  (input ([type "text"] [name "nick"] [size "10"]
                          [style ,(~~ "background-color:#FFFFF0"
                                      "border:inset thin"
                                      "font-size:105%"
                                      "font-family:'PT Sans',sans-serif")])))
              )
              (tr (td ([colspan "3"])) (td ([colspan "3"]) ,status))
              ;; status message
              (tr (td ([colspan "3"])) (td ([colspan "3"])
                      ,(if (string=? "" fork-from) ""
                          `(span "Forked from paste # " ,fork-from))))))
         (br)(br)(br)
         ;; middle bottom (part of middle) ------------------------------------
         (div ((style "font-size:small;color:#808080"))
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

(define (process-paste request)
  (define bs (request-bindings request))
  (cond
   [(exists-binding? 'paste bs)
    (define paste-num (fresh-str))
    (define paste-name (extract-binding/single 'name bs))
    (define pasted-code (extract-binding/single 'paste bs))
    (define fork-from (extract-binding/single 'fork-from bs))
    (define html-res
      (if (exists-binding? 'astext bs) #f
          (generate-paste-html pasted-code paste-num)))
    (define paste-html-str (or html-res pasted-code))
    (define eval-html-str
      (if html-res
          ;; eval only if able to read pasted code
          (generate-eval-html pasted-code paste-num)
          ;; if not, use read error as output,
          ;; unless as-text was explicitly checked
          (if (exists-binding? 'astext bs) #f
              (with-input-from-file
                (build-path tmp-dir paste-num (++ paste-num "code.err"))
                port->string))))
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
    (when (exists-binding? 'irc bs)
      (define nick (extract-binding/single 'nick bs))
      (for ([c irc-channels])
        (irc-send-message connection c
          (++ (if (string=? "" nick) "" (++ nick " pasted: "))
              (if (string=? "" paste-name) "" (++ paste-name ", "))
              paste-url))))
    (fprintf log-port "~a\t~a\t~a\t~a\n"
             tm-str paste-num paste-name (request-client-ip request))
    (response/xexpr
     `(html ()
        (head ()
          (script () ,(++ "location.href=\"" paste-url "\"")))
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

(define droidsansmono-css/x
  '(link ([type "text/css"] [rel "stylesheet"]
          [href "http://fonts.googleapis.com/css?family=Droid+Sans+Mono"])))
(define scrbl-css/x
  '(link ([type "text/css"] [rel "stylesheet"]
          [href "http://pasterack.org/scribble.css"])))
(define rkt-css/x
  '(link ([type "text/css"] [rel "stylesheet"]
          [href "http://pasterack.org/racket.css"])))
             
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
                         (define curr-file-path
                           (build-path tmp-dir pastenum filename))
                         (define new-file-path
                           (build-path htdocs-dir new-file))
                         (unless (file-exists? new-file-path)
                           (copy-file curr-file-path new-file-path)
                           (delete-file curr-file-path))
                         `(tr () (td () (p () (img
                            ((alt "image") ,height
                             (src ,(++ "http://pasterack.org/" new-file)) ,width)))))]
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
          (link ((href "/scribble.css")       (rel "stylesheet")
                 (title "default")            (type "text/css")))
          (link ((href "/racket.css")         (rel "stylesheet")
                 (title "default")            (type "text/css")))
          (link ((href "/scribble-style.css") (rel "stylesheet")
                 (title "default")            (type "text/css")))
          (link ([type "text/css"] [rel "stylesheet"]
                 [href "http://fonts.googleapis.com/css?family=PT+Sans"]))
          (link ([type "text/css"] [rel "stylesheet"]
                 [href "http://fonts.googleapis.com/css?family=Droid+Sans+Mono"]))
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

(define-values (do-dispatch mk-url)
  (dispatch-rules
   [("") serve-home]
   [("pastes" (string-arg)) serve-paste]
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
