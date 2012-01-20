#lang racket
(require "vorbisfile.rkt")

(define m (open-vorbis-file "/home/michael/Music/Jan Morgenstern/Big Buck Bunny/01-Prelude.ogg"))

(displayln (string-join (vorbis-comments m) "\n"))
(newline)

(printf "This file is ~a seconds long\n" (vorbis-length-sec m))
(printf "At ~a sec\n" (vorbis-current-time m))

(define bitstream-ptr (make-bitstream-ptr))

(define buf (vorbis-read-bytes! m 1024 0 1 0 bitstream-ptr)) (vorbis-current-time m)
