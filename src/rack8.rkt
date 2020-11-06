#lang racket
(require srfi/25)

(struct chip8-state (memory [pc #:mutable] [stack #:mutable] registers [reg-i #:mutable]))

(define (read-rom path)
  (define loaded (file->bytes path))
  loaded
)

(define (set-init path)
  (define rom-bytes (read-rom path))
  (define state (chip8-state (make-bytes #x1000) #x200 '() (make-bytes 16) #x0))
  (bytes-copy! (chip8-state-memory state) #x200 rom-bytes)
  (bytes-copy! (chip8-state-memory state) 0 #"\360\220\220\220\360")
  (bytes-copy! (chip8-state-memory state) 5 #"\040\140\040\040\160")
  (bytes-copy! (chip8-state-memory state) 10 #"\360\020\360\200\360")
  (bytes-copy! (chip8-state-memory state) 15 #"\360\020\360\020\360")
  (bytes-copy! (chip8-state-memory state) 20 #"\220\220\360\020\020")
  (bytes-copy! (chip8-state-memory state) 25 #"\360\200\360\020\360")
  (bytes-copy! (chip8-state-memory state) 30 #"\360\200\360\220\360")
  (bytes-copy! (chip8-state-memory state) 35 #"\360\020\040\200\200")
  (bytes-copy! (chip8-state-memory state) 40 #"\360\220\360\220\360")
  (bytes-copy! (chip8-state-memory state) 45 #"\360\220\360\020\360")
  (bytes-copy! (chip8-state-memory state) 50 #"\360\220\360\220\220")
  (bytes-copy! (chip8-state-memory state) 55 #"\340\220\340\220\340")
  (bytes-copy! (chip8-state-memory state) 60 #"\360\200\200\200\360")
  (bytes-copy! (chip8-state-memory state) 65 #"\340\220\220\220\340")
  (bytes-copy! (chip8-state-memory state) 70 #"\360\200\360\200\360")
  (bytes-copy! (chip8-state-memory state) 75 #"\360\200\360\200\200")
state)

(define state (set-init "CONNECT4.ch8"))
(define memory (chip8-state-memory state))
(define graphics (make-vector 2048))
(define keys (make-bytes 16))

(set-init "CONNECT4.ch8")
