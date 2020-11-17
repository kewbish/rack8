#lang racket
(require charterm)

(struct chip8-state (memory [pc #:mutable] [stack #:mutable] registers [reg-i #:mutable]))
(define (read-rom path)
  (define loaded (file->bytes path))
  loaded)

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
(define (2d-set! 2d-vec x y v)
  (vector-set! (vector-ref 2d-vec x) y v))
(define (graphics-print graphics)
  (define graphics-string (make-bytes 2048))
  (for ([j 64]) (for ([i 32])
                     (if (= (2d-ref graphics j i) 128)
                       (bytes-set! graphics-string (+ j (* i 64)) 77)
                       (bytes-set! graphics-string (+ j (* i 64)) 95))))
  (with-charterm (charterm-display graphics-string)))

; keys
(define keys (make-bytes 16))
(define key-map #hash((#\1 . 0) (#\2 . 1) (#\3 . 2) (#\4 . 3)
  (#\q . 4) (#\w . 5) (#\e . 6) (#\r . 7)
  (#\a . 8) (#\s . 9) (#\d . 10) (#\f . 11)
  (#\z . 12) (#\x . 13) (#\c . 14) (#\v . 15)
  ))

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

; screen display 
(with-charterm
 (charterm-clear-screen))

; emulate one opcode
(define (cycle)
  ; we multiply by #x100 to shift it to the hundredths (of hex) and add the tens
  (define inst (+ (* (bytes-ref memory (get-pc)) #x100) (bytes-ref memory (+ 1 (get-pc)))))
  (define (masked mask) (bitwise-and inst mask))
  ; variables
  (define nnn (masked #x0fff))
  (define kk (masked #x00ff))
  (define n (masked #xf000))
  (define ln (masked #x000f))
  ; have to divide the mask by 100 to get the ones we want, similarly 10 for one
  (define x (/ (masked #x0f00) #x100))
  (define y (/ (masked #x00f0) #x10))
  ; get-key loop
  (for ([i 16])
       (bytes-set! keys i 0))
  (with-charterm
    (define ckey (charterm-read-key #:timeout 0.000000001))
    (if (dict-has-key? key-map ckey)
      (bytes-set! keys 1 (dict-ref key-map ckey)) (void))
    )
  ; opcode cycle loop
  (match n
     [#x0000
      (cond
        [(= inst #x00e0) (with-charterm (charterm-display "[CLEAR SCREEN]")) (set! graphics (make-vector 64 (make-vector 32 0)))]
        [(= inst #x00ee) (with-charterm (charterm-display "[RETURN]"))]
        [(and (= n #x0000) (not (= inst #x0000))) (with-charterm (charterm-display "[CALL AT ~a]" nnn))]
        [else (with-charterm (charterm-display "[EMPTY]"))])]
     [#x1000 (with-charterm (charterm-display (format "[JUMP TO ADDRESS ~a]" nnn))) (set-pc nnn)]
     [#x2000 (with-charterm (charterm-display (format "[SUBROUTINE AT ~a]" nnn))) (push-stack (get-pc)) (set-pc nnn)]
     [#x3000 (with-charterm (charterm-display (format "[SKIP IF EQUAL ~a]" (= (get-reg x) kk))))
      (if (= (get-reg x) kk) (incre-pc) (void))]
     [#x4000 (with-charterm (charterm-display (format "[SKIP IF INEQUAL ~a]" (not (= (get-reg x) kk)))))
      (if (not (= (get-reg x) kk)) (incre-pc) (void))]
     [#x5000 (with-charterm (charterm-display (format "[SKIP IF 2EQUAL ~a]" (= (get-reg x) (get-reg y)))))
      (if (= (get-reg x) (get-reg y)) (incre-pc) (void))]
     [#x6000 (with-charterm (charterm-display (format "[SET REG ~a]" x))) (set-reg x kk)]
     [#x7000 (with-charterm (charterm-display (format "[ADD ~a TO REG ~a]" kk x))) (set-reg x (+ (get-reg x) kk))]
     [#x8000
      (cond
        [(= ln #x0) (with-charterm (charterm-display (format "[SET ~a TO ~a]" x (get-reg y)) (set-reg x (get-reg y))))]
        [(= ln #x1) (with-charterm (charterm-display (format "[OR ~a TO ~a|~a]" x (get-reg x) (get-reg y))))
                    (set-reg x (bitwise-ior (get-reg x) (get-reg y)))]
        [(= ln #x2) (with-charterm (charterm-display (format "[AND ~a TO ~a&~a]" x (get-reg x) (get-reg y))))
                    (set-reg x (bitwise-and (get-reg x) (get-reg y)))]
        [(= ln #x3) (with-charterm (charterm-display (format "[XOR ~a TO ~a+~a]" x (get-reg x) (get-reg y))))
                    (set-reg x (bitwise-xor (get-reg x) (get-reg y)))]
        [(= ln #x4) (with-charterm (charterm-display (format "[ADD ~a TO ~a+~a]" x (get-reg x) (get-reg y))))
                    (define added (+ (get-reg x) (get-reg y)))
                    (if (> added 255) (begin (set-reg #xF 1) (set! added 255)) (set-reg #xF 0))
                    (set-reg x added)]
        [(= ln #x5) (with-charterm (charterm-display (format "[SUB ~a TO ~a-~a]" x (get-reg x) (get-reg y))))
                    (define subbed (- (get-reg x) (get-reg y)))
                    (if (> (get-reg x) (get-reg y))
                      (begin (set-reg #xF 1) (set! subbed (modulo subbed 255)))
                      (set-reg #xF 0))
                    (set-reg x subbed)]
        [(= ln #x6) (with-charterm (charterm-display (format "[SHR ~a TO ~a]" x (get-reg x))))
                    (set-reg #xF (bitwise-and #x1 (get-reg y)))
                    (define shiftr (arithmetic-shift (get-reg y) (- 1)))
                    (set-reg x shiftr)]
        [(= ln #x7) (with-charterm (charterm-display (format "[SUBN ~a TO ~a]" x (get-reg x))))
                    (define subbed (- (get-reg y) (get-reg x)))
                    (if (> (get-reg y) (get-reg x)) 
                      (begin (set-reg #xF 1) (set! subbed (modulo subbed 255)))
                      (set-reg #xF 0))
                    (set-reg x subbed)]
        [(= ln #xE) (with-charterm (charterm-display (format "[SHL ~a WITH ~a]" x (get-reg x))))
                    (set-reg #xF (bitwise-and #x80 (get-reg y)))
                    (define shiftl (arithmetic-shift (get-reg y) 1))
                    (set-reg x shiftl)]
        [else (with-charterm (charterm-display "[INVALID]"))])]
     [#x9000 (with-charterm (charterm-display (format "[SKIP NEXT ~a]" (not (= (get-reg x) (get-reg y))))))
      (if (not (= (get-reg x) (get-reg y))) (incre-pc) (void))]
     [#xA000 (with-charterm (charterm-display (format "[SET I TO ~a]" nnn))) (set-regi nnn)]
     [#xB000 (with-charterm (charterm-display (format "[JUMP TO ~a+~a ~a]" nnn (get-reg #x0) (+ nnn (get-reg #x0)))))
      (set-pc (+ nnn (get-reg #x0)))]
     [#xC000 (with-charterm (charterm-display (format "[SET ~a TO RAND~a]}" x kk)))
      (define random (random 0 255))
      (set-reg x (bitwise-and random kk))]
     [#xD000 (with-charterm (charterm-display (format "[DRAW ~a at ~a, ~a]" ln (get-reg x) (get-reg y))))
      (for ([i ln]) (for ([j 8])
                         (2d-set! graphics
                         (+ (get-reg x) j) (+ (get-reg y) i)
                         (bitwise-xor (bytes-ref memory (+ (get-regi) i)) (2d-ref graphics j i)))))]
     [#xE000
      (cond
        [(= kk #x9E) (with-charterm (charterm-display (format "[SKIP IF KEY ~a DN]" x)))
                     (if (= (bytes-ref keys x) 1) (incre-pc) (void))]
        [(= kk #xA1) (with-charterm (charterm-display (format "[SKIP IF KEY ~a UP]" x)))
                     (if (= (bytes-ref keys x) 0) (incre-pc) (void))]
        [else (with-charterm (charterm-display "[INVALID]"))])]
     [#xF000
      (cond
        [(= kk #x07) (with-charterm (charterm-display (format "[SET ~a TIMER ~a]" x (timer-value delay-timer))))
                     (set-reg x (timer-value delay-timer))]
        [(= kk #x0A) (with-charterm (charterm-display (format "[WAIT KEY TO ~a]" x))
                                    (define key (charterm-read-key))
                                    (if (dict-has-key? key-map key)
                                        (set-reg x (dict-ref key-map key)) (void)))
                     ]
        [(= kk #x15) (with-charterm (charterm-display (format "[SET DELAY ~a]" (get-reg x))))
                     (set-timer! delay-timer (get-reg x))]
        [(= kk #x18) (with-charterm (charterm-display (format "[SET SOUND ~a]" (get-reg x))))
                     (set-timer! sound-timer (get-reg x))]
        [(= kk #x1E) (with-charterm (charterm-display (format "[ADD I ~a]" (get-reg x))))
                     (set-regi (+ (get-regi) (get-reg x)))]
        [(= kk #x29) (with-charterm (charterm-display (format "[SET I SPR ~a]" (get-reg x))))
                     (set-regi (* 5 (get-reg x)))]
        [(= kk #x33) (with-charterm (charterm-display (format "[BCD I ~a]" (get-reg x))))
                     (bytes-set! memory (get-regi) (/ (get-reg x) #x100))
                     (bytes-set! memory (+ 1 (get-regi)) (/ (get-reg x) #x010))
                     (bytes-set! memory (+ 2 (get-regi)) (/ (get-reg x) #x001))]
        [(= kk #x55) (with-charterm (charterm-display (format "[STORE I TO ~a]" x)))
                     (for ([i (+ x 1)])
                          (bytes-set! memory (+ (get-regi) i) (get-reg i)))]
        [(= kk #x65) (with-charterm (charterm-display (format "[READ FROM I TO ~a]" x)))
                     (for ([i (+ x 1)])
                          (set-reg i (bytes-ref memory (+ (get-regi) i))))]
        [else (with-charterm (charterm-display "[INVALID]"))])]
     [n (with-charterm (charterm-display "[~a: ~x|~a]" (get-pc) inst inst))])
  (with-charterm (charterm-clear-screen))
  (graphics-print graphics)
  (incre-pc)
)

(for ([i 100]) (cycle))
