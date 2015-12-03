#lang racket
(require "teleracket.rkt")

; simple bot that prints messages it receives
(define (run-bot proc)
  (let-values (((updates get-updates-proc) (proc)))
    (begin
      (map (λ (update) (println update)) updates)
      (sleep 10)
      (run-bot get-updates-proc))))

(run-bot (λ () (api-get-updates 0)))