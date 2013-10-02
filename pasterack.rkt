#lang racket

(require web-server/servlet
         web-server/dispatch)
(require xml xml/path)
(require racket/system racket/runtime-path)
(require redis
         data/ring-buffer)
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

(define NUM-RECENT-PASTES 10)
(define recent-pastes (empty-ring-buffer NUM-RECENT-PASTES))
;; initialize buffer with some pastes
(ring-buffer-push! recent-pastes "9965")
(ring-buffer-push! recent-pastes "3542")
(ring-buffer-push! recent-pastes "3414")
(ring-buffer-push! recent-pastes "5237")
(ring-buffer-push! recent-pastes "9647")
(ring-buffer-push! recent-pastes "5434")

;; returns output file name (as path), or #f on fail
(define (write-codeblock-scrbl-file code)
  (define tmp-name (mk-rand-str))
  (define tmp-scrbl-file (build-path tmp-dir (++ tmp-name ".scrbl")))
  (with-output-to-file tmp-scrbl-file
    (lambda () (printf (++ "#lang scribble/manual\n"
                           "@(require (for-label racket))\n"
                           "@codeblock{\n~a}")
                       code))
    #:mode 'text
    #:exists 'replace)
  tmp-name)
(define (write-eval-scrbl-file code)
  ;; parse out #lang if it's there
  (define lang-match (regexp-match #px"^\\#lang ([\\w/]+)\\s*(.*)" code))
  (define-values (code-no-lang lang)
    (match lang-match [(list _ lang rst) (values rst lang)]
                      [_ (values code "racket")]))
  (define tmp-name (mk-rand-str))
  (define tmp-scrbl-file (build-path tmp-dir (++ tmp-name ".scrbl")))
  (with-output-to-file tmp-scrbl-file
    (lambda ()
      (printf (++ "#lang scribble/manual\n"
                  "@(require scribble/eval)\n"
                  "@(define the-eval (make-base-eval"
                  (if (string=? "racket" lang) "" (++ " #:lang '" lang))
                  "))\n"
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
       (body
        (center
         (img ((src ,racket-logo-url)))
         (h1 ,(mk-link pastebin-url "PasteRack") ": The "
             ,(mk-link racket-lang-url "Racket") " pastebin.")
         (form ((action ,(embed/url process-paste)) (method "post"))
               (textarea ((rows "20") (cols "79") (name "paste")))
               (br)
               (input ((type "submit") (value "Submit Paste"))))
         (br)
         (h3 "Total pastes: " ,(number->string (DBSIZE)))
         (h3 "Recent pastes:")
         ,@(apply append
             (reverse
              (for/list ([pnum recent-pastes] #:when pnum)
                (list (mk-link (mk-paste-url pnum) pnum) '(br))))))))))
  (send/suspend/dispatch response-generator))

(define (process-paste request)
  (define bs (request-bindings request))
  (cond
   [(exists-binding? 'paste bs)
    (define paste-num (fresh-str))
    (define pasted-code (extract-binding/single 'paste bs))
    (define html-res (generate-paste-html pasted-code))
    (define paste-html-str (or html-res pasted-code))
    (define eval-html-str (and html-res (generate-eval-html pasted-code)))
    (define paste-url (mk-paste-url paste-num))
    (ring-buffer-push! recent-pastes paste-num)
    (SET/hash paste-num (hash 'code paste-html-str
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
    (match-define (hash-table ('code code-html)
                              ('eval eval-html)
                              ('time time-str)) retrieved-paste-hash)
    (define code-main-div (get-main-div code-html))
    (define eval-main-div (get-main-div eval-html))
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
      (body ()
       (div () ,(mk-link pastebin-url "Paste")
            " # " (a ((href ,paste-url)) ,pastenum) (br)
            (small ,(bytes->string/utf-8 time-str)) (br))
       (div ((class "maincolumn"))
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
                      ;; (printf "~v\n" x)
                      ;; (flush-output)
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
                 [_ `(div ,eval-main-div)]))]
           [_ `(div ,code-main-div)])))))]))

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
               #:extra-files-paths (list tmp-dir htdocs-dir)
               #:servlet-path "/"
               #:servlet-regexp #rx".*")
