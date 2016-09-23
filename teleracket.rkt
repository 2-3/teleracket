#lang racket
(require net/http-client)
(require json)

(provide
 api-get-updates
 api-get-botinfo
 api-send-message
 api-forward-message)

; see https://core.telegram.org/bots/api for information on how to get a token
(define bot-token "289593631:AAE58As13IneG2Lz49cPopbML1g2zk23MFE")
(define api-endpoint-host "api.telegram.org")
(define api-endpoint-uri (string-append "/bot" bot-token "/"))

(define (make-request-options options)
  (jsexpr->string options))

(define (api-request method action (options '()))
  (define-values (status header response)
    (http-sendrecv
     api-endpoint-host
     (string-append api-endpoint-uri action)
     #:ssl? #t
     #:data (make-request-options options)
     #:headers (list "Content-Type: application/json")))
  (read-json response))

(define (api-get-request action (options '()))
  (api-request "GET" action options))

(define (api-post-request action (options  '()))
  (api-request "POST" action options))

(define (api-get-updates (offset 0) (limit 100) (timeout 0))
  (let* ((options (hash 'offset offset 'limit limit 'timeout timeout))
         (response (api-get-request "getUpdates" options))
         (updates (hash-ref response 'result))
         (next-offset (+ 1 (if (empty? updates)
                         offset
                         (hash-ref (last updates) 'update_id)))))
    (values updates (make-get-updates-with-offset next-offset))))

(define (api-get-botinfo)
  (hash-ref (api-get-request "getMe") 'result))

(define (make-get-updates-with-offset offset)
  (λ ((limit 100) (timeout 0)) (api-get-updates offset limit timeout)))

(define (api-send-message chat-id text)
  (let* ((message-hash (hash 'chat_id chat-id 'text text))
         (response (api-post-request "sendMessage" message-hash)))
    response))

(define (api-forward-message to-chat-id from-chat-id message-id)
  (let* ((message-hash (hash 'chat_id to-chat-id 'from_chat_id from-chat-id 'message_id message-id))
         (response (api-post-request "forwardMessage" message-hash)))
    response))
