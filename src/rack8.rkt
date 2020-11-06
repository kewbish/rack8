#lang racket

(define (readRom path)
  (define loaded '())
  (for ([line (file->bytes path)])
    (set! loaded (append loaded (list line))))
  (for ([line loaded])
    (printf "~X" line))  
)

(readRom "CONNECT4.ch8")
