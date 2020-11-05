#lang racket

(define (readRom path)
  (for ([line (file->bytes path)])
    (displayln line)))

(readRom "CONNECT4.ch8")
