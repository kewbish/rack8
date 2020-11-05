#lang racket

(define (readRom path)
  (for ([line (file->lines path)])
    (displayln line)))

(readRom "CONNECT4.ch8")
