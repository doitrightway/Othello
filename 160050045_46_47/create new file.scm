#lang racket
(define out (open-output-file "D:\\semester 2\\cs 152\\cs-152project\\160050045_46_47\\high-score.txt"
                        #:mode 'binary
                        #:exists 'replace))
(write (cons 0 0) out)
(close-output-port out)
;(with-output-to-file 