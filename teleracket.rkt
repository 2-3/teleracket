#lang racket
(require net/http-client
         json)

(provide
 api-get-updates
 api-get-botinfo
 api-send-message
 api-forward-message)

; see https://core.telegram.org/bots/api for information on how to get a token
(define bot-token "289593631:AAExfFvE5GcjawA3wEiWARhzrt47Ey7ZpbQ")
(define api-endpoint-host "api.telegram.org")
(define api-endpoint-uri (string-append "/bot" bot-token "/"))

(define (make-request-options options)
  (jsexpr->string options))

(define (api-request method action (options '()))
  (define-values (status header json-encoded-response)
    (http-sendrecv
     api-endpoint-host
     (string-append api-endpoint-uri action)
     #:ssl? #t
     #:data (make-request-options options)
     #:headers (list "Content-Type: application/json")))
  (define response (read-json json-encoded-response))
  (if (hash-ref response 'ok)
    (cons 'ok  (hash-ref response 'result))
    (cons 'err ((hash-ref response 'description) (hash-ref response 'error_code)))))

(define (api-get-request action (options '()))
  (api-request "GET" action options))

(define (api-post-request action (options  '()))
  (api-request "POST" action options))

(define (api-get-updates (offset 0) (limit 100) (timeout 0) (allowed-updates '()))
  (let* ((options (hash 'offset offset
                        'limit limit
                        'timeout timeout
                        'allowed-updates allowed-updates))
         (response (api-get-request "getUpdates" options)))
    (values response
          (make-get-updates-with-offset
            (case (car response)))
              ('ok  (+ 1 (hash-ref (last response) 'update_id)))
              ('err offset))))

(define (api-get-botinfo)
  (api-get-request "getMe"))

(define (make-get-updates-with-offset offset (default-limit 100) (default-timeout 0) (default-allowed-updates '()))
  (λ ((limit default-limit) (timeout default-timeout) (allowed-updates default-allowed-updates))
     (api-get-updates offset limit timeout allowed-updates)))

(define (api-send-message chat-id text)
  (let* ((message-hash (hash 'chat_id chat-id
                             'text text))
         (response (api-post-request "sendMessage" message-hash)))
    response))

(define (api-forward-message to-chat-id from-chat-id message-id)
  (let* ((message-hash (hash 'chat_id to-chat-id
                             'from_chat_id from-chat-id
                             'message_id message-id))
         (response (api-post-request "forwardMessage" message-hash)))
    response))
