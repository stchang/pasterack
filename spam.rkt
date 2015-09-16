#lang racket/base

;; Rudimentary spam detection

(require racket/contract
         racket/port
         memoize
         net/http-client
         xml
         xml/path)

(provide (contract-out [check-ip (-> string? any)]))

(define blacklist-host "api.stopforumspam.org")

;; Returns #f if the lookup failed, if the response is malformed, or
;; if the IP doesn't appear. Return #t if the IP does appear.
;;
;; The result is memoized to avoid querying the server too often.
(define/memo (check-ip ip)
  (define-values (status headers contents)
    (http-sendrecv blacklist-host
                   (format "/api?ip=~a" ip)))
  (cond ;; only accept 200 OK
        [(regexp-match #"200 OK" status)
         (define response
           (string->xexpr (port->string contents)))
         (and response
              (equal? "yes" (se-path* '(response appears) response)))]
        [else #f]))
