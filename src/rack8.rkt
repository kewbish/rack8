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
  (bytes-copy! (chip8-state-memory state) 0 (bytes #xF0 #x90 #x90 #x90 #xF0))
  (bytes-copy! (chip8-state-memory state) 5 (bytes #x20 #x60 #x20 #x20 #x70))
  (bytes-copy! (chip8-state-memory state) 10 (bytes #xF0 #x10 #xF0 #x80 #xF0))
  (bytes-copy! (chip8-state-memory state) 15 (bytes #xF0 #x10 #xF0 #x10 #xF0))
  (bytes-copy! (chip8-state-memory state) 20 (bytes #x90 #x90 #xF0 #x10 #x10))
  (bytes-copy! (chip8-state-memory state) 25 (bytes #xF0 #x80 #xF0 #x10 #xF0))
  (bytes-copy! (chip8-state-memory state) 30 (bytes #xF0 #x80 #xF0 #x90 #xF0))
  (bytes-copy! (chip8-state-memory state) 35 (bytes #xF0 #x10 #x20 #x40 #x40))
  (bytes-copy! (chip8-state-memory state) 40 (bytes #xF0 #x90 #xF0 #x90 #xF0))
  (bytes-copy! (chip8-state-memory state) 45 (bytes #xF0 #x90 #xF0 #x10 #xF0))
  (bytes-copy! (chip8-state-memory state) 50 (bytes #xF0 #x90 #xF0 #x90 #x90))
  (bytes-copy! (chip8-state-memory state) 55 (bytes #xE0 #x90 #xE0 #x90 #xE0))
  (bytes-copy! (chip8-state-memory state) 60 (bytes #xF0 #x80 #x80 #x80 #xF0))
  (bytes-copy! (chip8-state-memory state) 65 (bytes #xE0 #x90 #x90 #x90 #xE0))
  (bytes-copy! (chip8-state-memory state) 70 (bytes #xF0 #x80 #xF0 #x80 #xF0))
  (bytes-copy! (chip8-state-memory state) 75 (bytes #xF0 #x80 #xF0 #x80 #x80))
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

; timers
(struct timer ([value #:mutable] [last-set #:mutable]))
; these two are actual timer objects
(define delay-timer (timer 0 0))
(define sound-timer (timer 0 0))
; timer helper methods
(define (set-timer! timer time)
  (set-timer-value! timer time)
  (set-timer-last-set! timer (current-milliseconds)))
(define (tick-timer! timer time)
  (when (>= (- (current-milliseconds) (timer-last-set timer)) 1000)
    (begin
      (set-timer-value! timer (- (timer-value timer) 1))
      (set-timer-last-set! timer (current-milliseconds)))))
(define (tick-timers!)
  (tick-timer! delay-timer)
  (tick-timer! sound-timer))
(define (timer-active? timer) (> (timer-value timer) 0))

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
  (define ln (masked #x000f))
  ; have to divide the mask by 100 to get the ones we want, similarly 10 for one
  (define x (get-reg (/ (masked #x0f00) #x100)))
  (define y (get-reg (/ (masked #x00f0) #x10)))
  ; opcode cycle loop
  (match n
     [#x0000
      (cond
        [(= inst #x00e0) (printf "[CLEAR SCREEN]") (set! graphics (make-vector 64 (make-vector 32 0)))]
        [(= inst #x00ee) (printf "[RETURN]")]
        [(and (= n #x0000) (not (= inst #x0000))) (printf "[CALL AT ~a]" nnn)]
        [else (printf "[EMPTY]")])]
     [#x1000 (printf "[JUMP TO ADDRESS ~a]" nnn)]
     [#x2000 (printf "[SUBROUTINE AT ~a]" nnn)]
     [#x3000 (printf "[SKIP IF EQUAL ~a]" (= (get-reg x) kk))]
     [#x4000 (printf "[SKIP IF INEQUAL ~a]" (not (= (get-reg x) kk)))]
     [#x5000 (printf "[SKIP IF 2EQUAL ~a]" (= (get-reg x) (get-reg y)))]
     [#x6000 (printf "[SET REG ~a]" x)]
     [#x7000 (printf "[ADD ~a TO REG ~a]" kk x)]
     [#x8000
      (cond
        [(= ln 0) (printf "[SET ~a TO ~a]" x (get-reg y))]
        [(= ln 1) (printf "[OR ~a TO ~a|~a]" x (get-reg x) (get-reg y))]
        [(= ln 2) (printf "[AND ~a TO ~a&~a]" x (get-reg x) (get-reg y))]
        [(= ln 3) (printf "[XOR ~a TO ~a+~a]" x (get-reg x) (get-reg y))]
        [(= ln 4) (printf "[ADD ~a TO ~a+~a]" x (get-reg x) (get-reg y))]
        [(= ln 5) (printf "[SUB ~a TO ~a-~a]" x (get-reg x) (get-reg y))]
        [(= ln 6) (printf "[SHR ~a TO ~a]" x (get-reg x))]
        [(= ln 7) (printf "[SHL ~a TO ~a]" x (get-reg x))]
        [else (printf "[INVALID]")])]
     [#x9000 (printf "[SKIP NEXT ~a]" (not (= (get-reg x) (get-reg y))))]
     [#xA000 (printf "[SET I TO ~a]" nnn)]
     [#xB000 (printf "[JUMP TO ~a+~a ~a]" nnn (get-reg 0) (+ nnn (get-reg 0)))]
     [#xC000 (printf "[SET ~a TO RAND~a]}" x kk)]
     [#xD000 (printf "[DRAW ~ab at ~a, ~a]" ln (get-reg x) (get-reg y))]
     [#xE000
      (cond
        [(= kk #x9E) (printf "[SKIP IF KEY ~a DN]" x)]
        [(= kk #xA1) (printf "[SKIP IF KEY ~a UP]" x)]
        [else (printf "[INVALID]")])]
     [n (printf "[~a: ~x|~a]" (get-pc) inst inst)])
  (printf " ")
  (incre-pc)
)

(for ([i 100]) (cycle))
