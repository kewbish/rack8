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
(define graphics (for/vector ((i 32)) (make-vector 64 0)))
; helper refs and sets, will get values
(define (2d-ref 2d-vec x y)
  (vector-ref (vector-ref 2d-vec y) x))
(define (2d-set! 2d-vec x y v)
  (vector-set! (vector-ref 2d-vec y) x v))

; keys
(define keys (make-vector 16 #f))
(define key-map (make-hash '((#\1 . 0) (#\2 . 1) (#\3 . 2) (#\4 . 3)
  (#\q . 4) (#\w . 5) (#\e . 6) (#\r . 7)
  (#\a . 8) (#\s . 9) (#\d . 10) (#\f . 11)
  (#\z . 12) (#\x . 13) (#\c . 14) (#\v . 15)
  )))

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

; graphics helpers
(define (graphics-print graphics)
  (with-charterm
    (charterm-clear-screen)
    (for ([y 32])
         (let ([display-string (make-string 64 #\space)])
           (for ([x 64])
                (if (= (2d-ref graphics x y) 1)
                  (string-set! display-string x #\M) (void))
                (charterm-display display-string)))
         (charterm-newline))))
(define (graphics-dump graphics)
  (printf "~a" graphics))

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
  ; opcode cycle loop
  (match n
     [#x0000
      (cond
        [(= inst #x00e0) (printf "[CLEAR SCREEN] ") (set! graphics ((for/vector ((i 32)) (make-vector 64 0))))]
        [(= inst #x00ee) (printf "[RETURN] ")]
        [(and (= n #x0000) (not (= inst #x0000))) (printf "[CALL AT ~a]" nnn)]
        [else (with-charterm (charterm-display "[EMPTY]"))])]
     [#x1000 (printf (format "[JUMP TO ADDRESS ~a] " nnn)) (set-pc nnn)]
     [#x2000 (printf (format "[SUBROUTINE AT ~a] " nnn)) (push-stack (get-pc)) (set-pc nnn)]
     [#x3000 (printf (format "[SKIP IF EQUAL ~a] " (= (get-reg x) kk)))
      (if (= (get-reg x) kk) (incre-pc) (void))]
     [#x4000 (printf (format "[SKIP IF INEQUAL ~a)] " (not (= (get-reg x) kk))))
      (if (not (= (get-reg x) kk)) (incre-pc) (void))]
     [#x5000 (printf (format "[SKIP IF 2EQUAL ~a] " (= (get-reg x) (get-reg y))))
      (if (= (get-reg x) (get-reg y)) (incre-pc) (void))]
     [#x6000 (printf (format "[SET REG ~a] " x)) (set-reg x kk)]
     [#x7000 (printf (format "[ADD ~a TO REG ~a] " kk x)) (set-reg x (+ (get-reg x) kk))]
     [#x8000
      (cond
        [(= ln #x0) (printf (format "[SET ~a TO ~a] " x (get-reg y)) (set-reg x (get-reg y)))]
        [(= ln #x1) (printf (format "[OR ~a TO ~a|~a] " x (get-reg x) (get-reg y)))
                    (set-reg x (bitwise-ior (get-reg x) (get-reg y)))]
        [(= ln #x2) (printf (format "[AND ~a TO ~a&~a] " x (get-reg x) (get-reg y)))
                    (set-reg x (bitwise-and (get-reg x) (get-reg y)))]
        [(= ln #x3) (printf (format "[XOR ~a TO ~a+~a] " x (get-reg x) (get-reg y)))
                    (set-reg x (bitwise-xor (get-reg x) (get-reg y)))]
        [(= ln #x4) (printf (format "[ADD ~a TO ~a+~a] " x (get-reg x) (get-reg y)))
                    (define added (+ (get-reg x) (get-reg y)))
                    (if (> added 255) (begin (set-reg #xF 1) (set! added 255)) (set-reg #xF 0))
                    (set-reg x added)]
        [(= ln #x5) (printf (format "[SUB ~a TO ~a-~a] " x (get-reg x) (get-reg y)))
                    (define subbed (- (get-reg x) (get-reg y)))
                    (if (> (get-reg x) (get-reg y))
                      (begin (set-reg #xF 1) (set! subbed (modulo subbed 255)))
                      (set-reg #xF 0))
                    (set-reg x subbed)]
        [(= ln #x6) (printf (format "[SHR ~a TO ~a] " x (get-reg x)))
                    (set-reg #xF (bitwise-and #x1 (get-reg y)))
                    (define shiftr (arithmetic-shift (get-reg y) (- 1)))
                    (set-reg x shiftr)]
        [(= ln #x7) (printf (format "[SUBN ~a TO ~a] " x (get-reg x)))
                    (define subbed (- (get-reg y) (get-reg x)))
                    (if (> (get-reg y) (get-reg x)) 
                      (begin (set-reg #xF 1) (set! subbed (modulo subbed 255)))
                      (set-reg #xF 0))
                    (set-reg x subbed)]
        [(= ln #xE) (printf (format "[SHL ~a WITH ~a] " x (get-reg x)))
                    (set-reg #xF (bitwise-and #x80 (get-reg y)))
                    (define shiftl (arithmetic-shift (get-reg y) 1))
                    (set-reg x shiftl)]
        [else (printf "[INVALID] ")])]
     [#x9000 (printf (format "[SKIP NEXT ~a] " (not (= (get-reg x) (get-reg y)))))
      (if (not (= (get-reg x) (get-reg y))) (incre-pc) (void))]
     [#xA000 (printf (format "[SET I TO ~a] " nnn)) (set-regi nnn)]
     [#xB000 (printf (format "[JUMP TO ~a+~a ~a] " nnn (get-reg #x0) (+ nnn (get-reg #x0))))
      (set-pc (+ nnn (get-reg #x0)))]
     [#xC000 (printf (format "[SET ~a TO RAND~a] " x kk))
      (define random (random 0 255))
      (set-reg x (bitwise-and random kk))]
     [#xD000 (printf (format "[DRAW ~a at ~a, ~a] " ln (get-reg x) (get-reg y)))
             (set-reg #xF 0)
             (for ([row ln])
                  (let ([pixel (bytes-ref memory (+ (get-regi) row))])
                    (for ([col 8])
                         (let ([bit (bitwise-and (arithmetic-shift #b10000000 (- col)))])
                           (if (and (and (not (= bit 0)) (< (+ (get-reg x) col) 64)) (< (+ (get-reg y) row) 32))
                             (let ([graphics_coord (2d-ref graphics (+ (get-reg x) col) (+ (get-reg y) row))])
                               (if (= graphics_coord 1)
                                (set-reg #xF 1) (void))
                             (2d-set! graphics (+ (get-reg x) col) (+ (get-reg y) row) (bitwise-xor graphics_coord 1))) (void))))))]
     [#xE000
      (cond
        [(= kk #x9E) (printf (format "[SKIP IF KEY ~a DN] " x))
                     (let ([cur-key (charterm-read-key #:timeout 0.01)])
                       (if (hash-has-key? key-map cur-key)
                         (if (= (hash-ref key-map cur-key) (get-reg x))
                           (incre-pc) (void)) (void)))]
        [(= kk #xA1) (printf (format "[SKIP IF KEY ~a UP] " x))
                     (let ([cur-key (charterm-read-key #:timeout 0.01)])
                       (if (hash-has-key? key-map cur-key)
                         (if (not (= (hash-ref key-map cur-key) (get-reg x)))
                           (incre-pc) (void)) (void)))]
        [else (printf "[INVALID] ")])]
     [#xF000
      (cond
        [(= kk #x07) (printf (format "[SET ~a TIMER ~a] " x (timer-value delay-timer)))
                     (set-reg x (timer-value delay-timer))]
        [(= kk #x0A) (printf (format "[WAIT KEY TO ~a] " x))
                     (with-charterm (let ([cur-key (charterm-read-key)])
                       (set-reg x (hash-ref key-map cur-key))))]
        [(= kk #x15) (printf (format "[SET DELAY ~a] " (get-reg x)))
                     (set-timer! delay-timer (get-reg x))]
        [(= kk #x18) (printf (format "[SET SOUND ~a] " (get-reg x)))
                     (set-timer! sound-timer (get-reg x))]
        [(= kk #x1E) (printf (format "[ADD I ~a] " (get-reg x)))
                     (set-regi (+ (get-regi) (get-reg x)))]
        [(= kk #x29) (printf (format "[SET I SPR ~a] " (get-reg x)))
                     (set-regi (* 5 (get-reg x)))]
        [(= kk #x33) (printf (format "[BCD I ~a] " (get-reg x)))
                     (bytes-set! memory (get-regi) (/ (get-reg x) #x100))
                     (bytes-set! memory (+ 1 (get-regi)) (/ (get-reg x) #x010))
                     (bytes-set! memory (+ 2 (get-regi)) (/ (get-reg x) #x001))]
        [(= kk #x55) (printf (format "[STORE I TO ~a] " x))
                     (for ([i (+ x 1)])
                          (bytes-set! memory (+ (get-regi) i) (get-reg i)))]
        [(= kk #x65) (printf (format "[READ FROM I TO ~a] " x))
                     (for ([i (+ x 1)])
                          (set-reg i (bytes-ref memory (+ (get-regi) i))))]
        [else (printf "[INVALID] ")])]
     [n (printf "[~a: ~x|~a] " (get-pc) inst inst)])
  (incre-pc)
  (graphics-print graphics)
)

(for ([i 100]) (cycle))
