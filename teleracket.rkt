#lang racket
(require net/http-client)
(require json)

(provide api-get-updates)

; see https://core.telegram.org/bots/api for information on how to get a token
(define bot-token "")
(define api-endpoint-host "api.telegram.org")
(define api-endpoint-uri (string-append "/bot" bot-token "/"))

(define (make-request-options options)
  (jsexpr->string options))

(define (api-request method action (options ""))
  (define-values (status header response)
    (http-sendrecv
     api-endpoint-host
     (string-append api-endpoint-uri action)
     #:ssl? #t
     #:data (make-request-options options)))
  (read-json response))

(define (api-get-request action (options ""))
  (api-request "GET" action options))

(define (api-post-request action (options ""))
  (api-request "POST" action options))

(define (make-get-updates-with-offset offset)
  (λ ((limit 100) (timeout 0)) (api-get-updates offset limit timeout)))

(define (api-get-updates offset (limit 100) (timeout 0))
  (let* ((response (api-get-request "getUpdates"))
         (updates (hash-ref response 'result))
         (next-offset (if (empty? updates) offset (hash-ref (last updates) 'update_id))))
    (values updates (make-get-updates-with-offset next-offset))))