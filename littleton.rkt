#lang racket
(require racket/match
         "teleracket.rkt")

(struct ~matcher (id
                  match-proc
                  handle-proc))

(struct ~message (id
                  chat-id 
                  from-id
                  from-name
                  text))

(define bot-name (hash-ref (api-get-botinfo) 'username))

(define matcher-list
  (list (~matcher "coffee"
            (lambda (m) (string=? (~message-text m) "coffee"))
            (lambda (m) (api-send-message (~message-chat-id m) "☕️")))))

(define (run-bot proc matchers)
  (define-values (updates get-updates-proc) (proc))
    (begin (map
       (lambda (update)
           (let* ((message (hash-ref update 'message))
                  (user (hash-ref message 'from))
                  (m (~message (hash-ref message 'message_id)
                               (hash-ref (hash-ref message 'chat) 'id)
                               (hash-ref user 'id)
                               (hash-ref user 'username)
                               (if (hash-has-key? message 'text) (sanitize-command (hash-ref message 'text)) ""))))
             (printf "<~a> (~a) ~a ~%" (~message-from-name m) (~message-chat-id m) (~message-text m))
             (handle-message m matchers))
         ) updates)
      (sleep 2)
      (run-bot get-updates-proc matchers)))

(define (sanitize-command t)
  (cond 
    [(regexp-match? (regexp (string-append "/(.*?)@" bot-name)) t) (cadr (regexp-match #rx"/(.*?)@" t))]
    [(char=? #\/ (string-ref t 0)) (substring t 1)]
    [else t]))

(define (handle-message m matchers)
  (map
   (lambda (matcher)
     (if ((~matcher-match-proc matcher) m) ((~matcher-handle-proc matcher) m) '()))
   matchers))


(run-bot (lambda () (api-get-updates)) matcher-list)
