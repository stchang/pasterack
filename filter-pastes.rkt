#lang racket
(require redis)

;; delete all non-hash vals (ie, not pastes)
(define (delete-nonpastes #:trial? [trial? #f])
  (for ([k (KEYS "*")])
    ; delete non-hashes
    (unless (string=? (TYPE k) "hash")
      (printf "deleting non-hash key: ~a\n" k)
      (unless trial? (DEL k)))
    ; delete hashes that are not pastes
    (unless (HEXISTS k 'code)
      (printf "deleting non-paste hash key: ~a\n" k)
      (unless trial? (DEL k)))))

; deletes pastes satisfying the given regexp pattern
(define (delete-pastes/pat pat #:trial? [trial? #f])
  (define count 0)
  (define ci-pat (pregexp (string-append "(?i:" pat ")")))
  (printf "searching for pastes with pattern: ~a" pat)
  (when trial? (printf " (trial)"))
  (printf "\n")
  (for ([k (KEYS "*")])
    (when (and (string=? (TYPE k) "hash") (HEXISTS k 'code)) ; valid paste
      (define paste-contents (HGET/str k 'code))
      (define paste-dir (build-path "tmp" (bytes->path k)))
      (when (regexp-match ci-pat paste-contents)
        (printf "deleting paste: ~a\n" k)
;        (displayln paste-contents)
        (when (directory-exists? paste-dir)
          (printf "... and deleting directory: ~a\n" paste-dir))
        (unless trial?
          (DEL k)
          (when (directory-exists? paste-dir)
            (delete-directory/files paste-dir)))
        (set! count (add1 count)))))
  (printf "deleted ~a pastes matching pattern ~a\n" count pat))

(define default-pats
  '("amex"
    "visa"
    "mastercard"
    "discover"
    "rapidgator"
    "turbobit"
    "bitcoin"
    "paypal"
    "Western Union"
    "Money Gram"
    "wmz"
    "cvv"
    "Web Money"
    "Perfect Money"
    "torrent"
    "hacked"
    "accounts"
    "premium"))

(module+ main
  (define trial-mode (make-parameter #t))
  (define pats
    (command-line
     #:once-each
     [("--delete") "Do the deletions (default is trial mode)"
      (trial-mode #f)]
     #:args args
     (if (null? args) default-pats args)))
  (for ([p pats])
    (delete-pastes/pat p #:trial? (trial-mode))))
