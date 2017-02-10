#lang racket
(require "teleracket.rkt")

(struct ~matcher (id
                  match-proc
                  handle-proc))

(define-syntax <-
  (syntax-rules ()
    [(<- hs a)         (hash-ref hs a)]
    [(<- hs a b)       (hash-ref (hash-ref hs a) b)]
    [(<- hs a b c ...) (<- (hash-ref (hash-ref hs a) b) c ...)]))

(define bot-name (<- (cdr (api-get-botinfo)) 'username))

(define (run-bot proc matchers)
  (define-values (status result get-updates-proc) (proc))
  (case status
    ('err
     (begin
       (printf "ERROR: [~a] ~a" (second result) (first result))
       (sleep 10)))
    ('ok 
     (map (λ (update)
             (let
              ((m (<- update 'message)))
               (printf "<~a> (~a) ~a ~%" (<- m 'from 'username) (<- m 'chat 'id) (<- m 'text))
               (handle-message (hash-set m 'text (sanitize-command (<- m 'text))) matchers)))
          result)))
  (run-bot get-updates-proc matchers))

(define (sanitize-command t)
  (cond 
    [(regexp-match? (regexp (string-append "/(.*?)@" bot-name)) t) (cadr (regexp-match #rx"/(.*?)@" t))]
    [(char=? #\/ (string-ref t 0)) (substring t 1)]
    [else t]))

(define (handle-message m matchers)
  (map
   (λ (matcher)
     (when ((~matcher-match-proc matcher) m) ((~matcher-handle-proc matcher) m)))
   matchers))

(run-bot (λ () (api-get-updates)) (list
                                      (~matcher "coffee"
                                        (λ (m) (string=? (<- m 'text) "coffee"))
                                        (λ (m) (api-send-message (<- m 'chat 'id) "☕️")))))
