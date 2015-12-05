#lang racket
(require "teleracket.rkt")

(define (run-bot proc)
  (define-values (updates get-updates-proc) (proc))
    (begin
      (map
       (λ (update)
           (let* ((message (hash-ref update 'message))
                  (user  (hash-ref message 'from))
                  (message-text (hash-ref message 'text))
                  (user-username (hash-ref user 'username)))
             (printf "<~a> ~a ~%" user-username  message-text)
             (cond
               ((and (> (string-length message-text) 5) (string=? (substring message-text 0 5) "echo "))
                (api-answer-message message (substring message-text 5)))))
         ) updates)
      (sleep 2)
      (run-bot get-updates-proc)))

(run-bot (λ () (api-get-updates)))