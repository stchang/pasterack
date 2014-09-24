#lang racket
(require 
  web-server/servlet web-server/servlet-env
  web-server/formlets web-server/formlets/servlet
  net/url
  graph)

;; scrape data ----------------------------------------------------------------
(define PLT-PUBS-URL (string->url "http://www.ccs.neu.edu/racket/pubs/"))
(define neu-pubs-port (get-pure-port PLT-PUBS-URL))
;(define neu-pubs-port (open-input-file "plt-pubs.html"))

(define name-pat "([A-Z][a-z\\-]+\\s?)+")
(define word-pat "([A-Za-z\\-]+\\s?)+")
(define names-pat (string-append "(" name-pat ",\\s)+" name-pat))
(define title-pat (string-append "(?<=<cite>)\\s+(" word-pat ")+"))
(define authors-px 
  (pregexp 
   (string-append "(?<=:|<div>)\\s*" names-pat ".+?" title-pat)))

(define matches (regexp-match* authors-px neu-pubs-port))

;; authors+title : [Listof author-string ... title-string]
(define authors+title
  (for/list ([authors matches])
    (define as+title 
      (string-split (string-trim (bytes->string/utf-8 authors)) #px",\\s+"))
    (define last-auth+title 
      (car (reverse as+title)))
    (define first-authors 
      (reverse (cdr (reverse as+title))))
    (define last-auth+title-match 
      (regexp-split #px"\\s+<br />|\\s+<cite>" last-auth+title))
    (define as+t
      (append first-authors 
              (list (first last-auth+title-match)
                    (string-trim (car (reverse last-auth+title-match))))))
    as+t))

;; populate graph -------------------------------------------------------------
(define PLT-GRAPH (unweighted-graph/undirected null))
(define-edge-property PLT-GRAPH papers)

(for ([as+t authors+title])
  (define authors (cdr (reverse as+t)))
  (define title (car (reverse as+t)))
  (for* ([auth1 authors]
         [auth2 authors]
         #:unless (string=? auth1 auth2))
    (define papers-curr (papers auth1 auth2 #:default null))
    (add-edge! PLT-GRAPH auth1 auth2)
    (papers-set! auth1 auth2 (cons title papers-curr))))

;; print to stdout ------------------------------------------------------------
#;(define (plt-bacon auth erdos bacon)
  (define erdos-path (fewest-vertices-path PLT-GRAPH auth erdos))
  (define bacon-path (fewest-vertices-path PLT-GRAPH auth bacon))
  ;; print erdos path
  (for ([a1 erdos-path]
        [a2 (cdr erdos-path)])
    (printf "~a co-authored with ~a:\n" a1 a2)
    (for ([p (papers a1 a2)])
      (printf "  ~a\n" p)))
  (define erdos-num (sub1 (length erdos-path)))
  (printf "\n** ~a's ~a-number is: ~a\n\n" auth erdos erdos-num)
  ;; print bacon path
  (for ([a1 bacon-path]
        [a2 (cdr bacon-path)])
    (printf "~a co-authored with ~a:\n" a1 a2)
    (for ([p (papers a1 a2)])
      (printf "  ~a\n" p)))
  (define bacon-num (sub1 (length bacon-path)))
  (printf "\n** ~a's ~a-number is: ~a\n\n" auth bacon bacon-num)
  (printf "## ~a's ~a-~a-number is: ~a\n"
          auth erdos bacon
          (+ erdos-num bacon-num)))
  
;; html output ----------------------------------------------------------------
(define (plt-bacon-html auth erdos bacon)
  (define erdos-path (fewest-vertices-path PLT-GRAPH auth erdos))
  (define bacon-path (fewest-vertices-path PLT-GRAPH auth bacon))
  (define erdos-num (sub1 (length erdos-path)))
  (define bacon-num (sub1 (length bacon-path)))
  `(table
    (tr "Computed "
        (i ,auth)
        "'s "
        (b ,erdos) "-" (b ,bacon) " number:")
    (tr (br) (hr))
    (tr
     ;; print erdos path
     ,@(for/list ([a1 erdos-path]
                  [a2 (cdr erdos-path)])
         `(table (tr (i ,(format "~a" a1))
                     " co-authored with "
                     (i ,(format "~a" a2))
                     ":")
                 (tr (ul
                 ,@(for/list ([p (papers a1 a2)])
                     `(li ,(format "~a" p))))))))
    (tr "** "
        (i ,(format "~a" auth))
        "'s "
        (b ,(format "~a" erdos))
        "-number is: "
        (b ,(format "~a" erdos-num)))
    (tr (br) (hr))
    (tr (br))
    (tr
    ;  ;; print bacon path
    ,@(for/list ([a1 bacon-path]
                 [a2 (cdr bacon-path)])
         `(table (tr (i ,(format "~a" a1))
                     " co-authored with "
                     (i ,(format "~a" a2))
                     ":")
                (tr (ul
                     ,@(for/list ([p (papers a1 a2)])
                         `(li ,(format "~a" p))))))))
    (tr "** "
        (i ,(format "~a" auth))
        "'s "
        (b ,(format "~a" bacon))
        "-number is: "
        (b ,(format "~a" bacon-num)))
    (tr (br) (hr))
    (tr (br))
    (tr
        "## "
        (i ,(format "~a" auth))
        "'s "
        (b ,(format "~a-~a" erdos bacon))
        "-number is: "
        (b ,(format "~a" (+ erdos-num bacon-num))))
    (tr (br) (hr))))

;;-----------------------------------------------------------------------------
;; web server front end

(define author-choices 
  (sort
   (filter-not 
    (λ (v) (regexp-match #px"and\\s|b>|Felleisen\\." v))
    (get-vertices PLT-GRAPH))
   string<?))
(define author-formlet
  (formlet*
   `(div
     (div "Author Name: "
          ,{(select-input
             author-choices
             #:selected? (lambda (x) (string=? x "Chang")))
            . =>* . author})
     (div "\"Bacon\": "
          ,{(select-input
             author-choices
             #:selected? (lambda (x) (string=? x "Felleisen")))
            . =>* . bacon})
     (div "\"Erdos\": "
          ,{(select-input
             author-choices
             #:selected? (lambda (x) (string=? x "Flatt")))
            . =>* . erdos})
     (div ,{(submit "Compute!") . =>* . res}))
   ;(list author erdos bacon)))
   ;; Q: Why is author etc a list?
   (let ([response-gen
          (λ (embed/url)
            (response/xexpr
             `(html
               (title "Results")
               (body (h1 "Results")
                     (div ,(plt-bacon-html (car author) (car bacon) (car erdos)))
                     (br) (br)
                     (a ([href ,(embed/url serve-bacon)]) "Start Again")))))])
     (send/suspend/dispatch response-gen))))
   
;(define (start request) (serve-bacon request))

(provide serve-bacon)
(define (serve-bacon request)
  (define (response-generator embed/url)
    (response/xexpr
     `(html
       (head (title "PLT Bacon"))
       (body (h1 "PLT Bacon")
             (img ([src "plt-bacon.png"]))
             ,(embed-formlet embed/url author-formlet)))))
  (send/suspend/dispatch response-generator))


#;(serve/servlet start
               #:launch-browser? #t
               #:quit? #f
               #:listen-ip #f
               #:port 8000)
