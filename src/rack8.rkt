#lang racket

(struct chip8-state (memory [pc #:mutable] [stack #:mutable] regs [reg-i #:mutable]))

(define (read-rom path)
  (define loaded '())
  (for ([line (file->bytes path)])
    (set! loaded (append loaded (list line))))
  (for ([line loaded])
    (printf "~X" line))  
  loaded
)

(define (set-init path)
  (define rom-bytes (read-rom path))
  (define cur-state (chip8-state (make-bytes #x1000) #x200 '() (make-bytes 16) #x0))
cur-state)

(set-init "CONNECT4.ch8")
