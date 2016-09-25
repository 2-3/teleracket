#lang racket 
(require datalog)

(provide
  ~quote
  add-quote
  delete-quote
  query)

(define db-path "quotes.datalog")

(define quote-db (if (file-exists? db-path)
                   (read-db db-path)
                   (init-db db-path)))

(define (read-db path) (with-input-from-file path (lambda () (read-theory))))

(define (write-db path theory) (with-output-to-file path (lambda (write-theory theory))))

(define (init-db path)
  ((write-db path (make-theory))
   (read-db path)))

(define (commit-changes) (write-db db-path quote-db))

(struct ~quote (message-id
                from-id
                from-name
                text))


(define (add-quote message-id from-id from-name text)
  (datalog quote-db
           (! (owner message-id from-id))))

(define (query field value) '())
