#lang racket/base
(require irc racket/async-channel)
(provide pasterack-irc-connect irc-paste)

;; pasterack irc bot

(define MIRROR? #f)

(define FREENODE "chat.freenode.net")
(define PORT 6667)
(define NAME (if MIRROR? "pasteracktest" "pasterack"))


(define irc-channels (if MIRROR? '("#racktest") '("#racket")))

(define current-irc-connection #f)
(define current-irc-listener (thread void))
(define current-irc-monitor (thread void))
  

(define (irc-connect/internal)
  (define-values (irc-connection ready)
    (irc-connect FREENODE PORT NAME NAME NAME #:return-eof #t))
  (define achan (irc-connection-incoming irc-connection))
  (set! current-irc-connection irc-connection)
  (set! current-irc-listener
    (thread
        (lambda ()
          (let loop ()
            (unless (eof-object? (async-channel-get achan))
              (loop))))))
  ready)

;; creates an irc monitor thread
(define (pasterack-irc-connect)
  (set! current-irc-monitor
      (thread
          (lambda ()
            (let loop ()
              (when (thread-dead? current-irc-listener)
                (sync (irc-connect/internal))
                (join-channels))
              (sleep 60)
              (loop))))))

(define (join-channels)
  (for ([c irc-channels]) (irc-join-channel current-irc-connection c)))

(define (irc-paste msg)
  (for ([c irc-channels]) (irc-send-message current-irc-connection c msg)))
              
