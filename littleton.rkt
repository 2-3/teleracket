#lang racket
(require "teleracket.rkt")

(struct ~matcher (id
                  match
                  handle)
        #:guard (lambda (id mt hn tn)
                  (values
                    id
                    (cond
                      [(string? mt) (lambda (m) (string=? (~message-text m) mt))]
                      [(regexp? mt) (lambda (m) (regexp-match? mt (~message-text m)))]
                      [(procedure? mt) mt]
                      [else (error tn "bad matcher value: ~a" mt)])
                    (cond
                      [(string? hn) (lambda (m) (api-send-message (~message-chat-id m) hn))]
                      [(and (regexp? mt) (procedure? hn) (arity=? hn 2)) (lambda (m) (hn m (regexp-match* mt)))]
                      [(procedure? hn) hn]
                      [else (error tn "bad matcher value: ~a" mt)]))))

(struct ~message (id
                  chat-id 
                  from-id
                  from-name
                  text))

(define bot-name (hash-ref (api-get-botinfo) 'username))

(define matcher-list
  (list (~matcher "coffee"
                  "coffee"
                  "☕️")))

(define (run-bot proc matchers)
  (define-values (updates get-updates-proc) (proc))
    (begin (map
       (lambda (update)
           (let* ((message (hash-ref update 'message))
                  (user (hash-ref message 'from))
                  (m (~message (hash-ref message 'message_id)
                               (hash-ref (hash-ref message 'chat) 'id)
                               (hash-ref user 'id)
                               (hash-ref user 'username '())
                               (sanitize-command (hash-ref message 'text "/")))))
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
     (if ((~matcher-match matcher) m) ((~matcher-handle matcher) m) '()))
   matchers))


(run-bot (lambda () (api-get-updates)) matcher-list)
