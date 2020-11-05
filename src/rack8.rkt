#lang racket

(define (readRom path)
  (define contents (file->lines path))
  (printf contents)
)

(readRom './rack8.rkt')
