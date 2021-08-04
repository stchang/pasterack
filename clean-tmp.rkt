#lang racket
(require redis)

; delete tmp file that dont have existing paste
(define (clean-tmp tmp-dir htdoc-dir #:trial? [trial? #t])
  (define dir-count 0)
  (define file-count 0)
  (when trial? (displayln "(trial mode)"))
  ;; 1) dirs in tmp
  (for ([d (in-directory tmp-dir (lambda go-into-dirs? #f))]
        #:when (directory-exists? d)) ; dirs only for now
    (define paste-num
      (substring (path->string d) (add1 (string-length tmp-dir))))
    (unless (HEXISTS paste-num 'code)
      (printf "deleting tmp dir for paste ~a\n" paste-num)
      (set! dir-count (add1 dir-count))
      (unless trial? (delete-directory/files d))))
  ;; 2) files in tmp
  (for ([f (in-directory tmp-dir (lambda go-into-dirs? #f))]
        #:unless (directory-exists? f)
        #:when (regexp-match #px"/([0-9]+)" f))
    (define paste-num
      (cadr (regexp-match #px"/([0-9]+)" f)))
    (unless (HEXISTS paste-num 'code)
      (printf "deleting file ~a\n" f)
      (set! file-count (add1 file-count))
      (unless trial? (delete-file f))))
  ;; 3) files in ht-docs
  (for ([f (in-directory htdoc-dir)]
        #:when (and (equal? #".png" (path-get-extension f))
                    (regexp-match #px"/([0-9]+)" f)))
    (define paste-num
      (cadr (regexp-match #px"/([0-9]+)" f)))
    (unless (HEXISTS paste-num 'code)
      (printf "deleting file ~a\n" f)
      (set! file-count (add1 file-count))
      (unless trial? (delete-file f))))
  (printf "Deleted ~a dirs\n" dir-count)
  (printf "Deleted ~a files\n" file-count))
  

(module+ main
  (define trial-mode (make-parameter #t))
  (define tmp-dirs
    (command-line
     #:once-each
     [("--delete") "Do the deletions (default is trial mode)" (trial-mode #f)]
     #:args args
     (if (null? args) (list "tmp" "htdocs") args)))
  (apply clean-tmp tmp-dirs #:trial? (trial-mode)))
