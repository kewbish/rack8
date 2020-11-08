#lang racket

(struct chip8-state (memory [pc #:mutable] [stack #:mutable] registers [reg-i #:mutable]))
(define (read-rom path) (define loaded (file->bytes path))
  loaded
)

(define (set-init path)
  (define rom-bytes (read-rom path))
  ; create a chip8 state, set most to empty vals
  (define state (chip8-state (make-bytes #x1000) #x200 '() (make-bytes 16) #x0))
  ; load the program and fontset into the memory
  (bytes-copy! (chip8-state-memory state) #x200 rom-bytes)
  (bytes-copy! (chip8-state-memory state) 50 #"\360\220\220\220\360")
  (bytes-copy! (chip8-state-memory state) 55 #"\040\140\040\040\160")
  (bytes-copy! (chip8-state-memory state) 60 #"\360\020\360\200\360")
  (bytes-copy! (chip8-state-memory state) 65 #"\360\020\360\020\360")
  (bytes-copy! (chip8-state-memory state) 70 #"\220\220\360\020\020")
  (bytes-copy! (chip8-state-memory state) 75 #"\360\200\360\020\360")
  (bytes-copy! (chip8-state-memory state) 80 #"\360\200\360\220\360")
  (bytes-copy! (chip8-state-memory state) 85 #"\360\020\040\200\200")
  (bytes-copy! (chip8-state-memory state) 90 #"\360\220\360\220\360")
  (bytes-copy! (chip8-state-memory state) 95 #"\360\220\360\020\360")
  (bytes-copy! (chip8-state-memory state) 100 #"\360\220\360\220\220")
  (bytes-copy! (chip8-state-memory state) 105 #"\340\220\340\220\340")
  (bytes-copy! (chip8-state-memory state) 110 #"\360\200\200\200\360")
  (bytes-copy! (chip8-state-memory state) 115 #"\340\220\220\220\340")
  (bytes-copy! (chip8-state-memory state) 120 #"\360\200\360\200\360")
  (bytes-copy! (chip8-state-memory state) 125 #"\360\200\360\200\200")
state)

; create state object, will be used thruo interpreting
(define state (set-init "CONNECT4.ch8"))

; memory -> definition and helper
(define memory (chip8-state-memory state))
(define (print-mem mem-state)
  (define counter 0)
  (define cells (bytes->list mem-state))
  (for ([i (in-range 2048)])
    (printf "[~a: ~x~x]" counter (list-ref cells counter) (list-ref cells (+ counter 1)))
    (if (= (modulo counter 20) 0) (printf " ~n") (printf " "))
    (set! counter (+ counter 2)))
)

; graphics -> 2d vector (have to do some interesting vector layering)
(define graphics (make-vector 64 (make-vector 32 0)))
; helper refs and sets, will get values
(define (2d-ref 2d-vec x y)
  (vector-ref (vector-ref 2d-vec x) y))
(define (2d-set! 2d-vec x y)
  (vector-set! (vector-ref 2d-vec x) y))
(define keys (make-bytes 16))

; stacks
(define (pop-stack)
  (match (chip8-state-stack state)
         ['() (error "rack8 - empty stack.")]
         [(cons h t)
          (set-chip8-state-stack! state t) h]))
(define (push-stack v)
  (set-chip8-state-stack! state cons(v (chip8-state-stack state))))

; registers and counters
(define (get-pc) (chip8-state-pc state))
(define (set-pc v) (set-chip8-state-pc! state v))
(define (incre-pc) (set-chip8-state-pc! state (+ (get-pc) 2)))
(define (get-reg n) (bytes-ref (chip8-state-registers state) n))
(define (set-reg n v) (bytes-set! (chip8-state-registers state) n v))
(define (get-regi) (bytes-ref (chip8-state-reg-i state)))
(define (set-regi v) (bytes-set! (set-chip8-state-reg-i! state v)))

; emulate one opcode
(define (cycle)
  (define inst (+ (* (bytes-ref memory (get-pc)) #x100) (bytes-ref memory (+ (get-pc) 1))))
  (match inst
         [224 (printf "CLEAR SCREEN")]
         [inst (printf "[~a: ~x|~a] " (get-pc) inst inst)])
  (incre-pc)
)

(for ([i 100]) (cycle))
