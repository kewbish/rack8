#lang racket

(struct chip8-state (memory [pc #:mutable] [stack #:mutable] registers [reg-i #:mutable]))
(define (read-rom path) (define loaded (file->bytes path)) loaded
)

(define (set-init path)
  (define rom-bytes (read-rom path))
  ; create a chip8 state, set most to empty vals
  (define state (chip8-state (make-bytes #x1000) #x200 '() (make-bytes 16) #x0))
  ; load the program and fontset into the memory
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
(define (incre-pc) (set-pc (+ (get-pc) 2)))
(define (get-reg n) (bytes-ref (chip8-state-registers state) n))
(define (set-reg n v) (bytes-set! (chip8-state-registers state) n v))
(define (get-regi) (chip8-state-reg-i state))
(define (set-regi v) (set-chip8-state-reg-i! state v))

; emulate one opcode
(define (cycle)
  ; we multiply by #x100 to shift it to the hundredths (of hex) and add the tens
  (define inst (+ (* (bytes-ref memory (get-pc)) #x100) (bytes-ref memory (+ 1 (get-pc)))))
  (define (masked mask) (bitwise-and inst mask))
  ; ? denotes a boolean returner
  (define (hex-op? mask value) (= (masked mask) value))
  ; variables
  (define nnn (masked #x0fff))
  (define kk (masked #x00ff))
  (define n (masked #xf000))
  ; have to divide the mask by 100 to get the ones we want, similarly 10 for one
  (define x (get-reg (/ (masked #x0f00) #x100)))
  (define y (get-reg (/ (masked #x00f0) #x10)))
  ; opcode cycle loop
  (cond
    [(= inst #x00e0) (printf "[CLEAR SCREEN]")]
    [(= inst #x00ee) (printf "[RETURN]")]
    [(and (= n #x0000) (not (= inst #x0000))) (printf "[CALL AT ~a]" nnn)]
    [(= n #x1000) (printf "[JUMP TO ADDRESS ~a]" nnn)]
    [(= n #x2000) (printf "[SUBROUTINE AT ~a]" nnn)]
    [(= n #x3000) (printf "[SKIP IF EQUAL ~a]" (= (get-reg x) kk))]
    [(= n #x4000) (printf "[SKIP IF INEQUAL ~a]" (not (= (get-reg x) kk)))]
    [(= n #x5000) (printf "[SKIP IF 2EQUAL ~a]" (= (get-reg x) (get-reg y)))]
    [(= n #x6000) (printf "[SET REG ~a]" x)]
    [(= n #x7000) (printf "[ADD ~a TO REG ~a]" kk x)]
    [else (printf "[~a: ~x|~a]" (get-pc) inst inst)])
  (printf " ")
  (incre-pc)
)

(for ([i 100]) (cycle))
