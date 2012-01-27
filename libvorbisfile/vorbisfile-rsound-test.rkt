#lang racket/base
(require "rsound-compat.rkt"
         "vorbisfile.rkt"
         (planet clements/rsound)
         (planet clements/rsound/draw))

(displayln "Loading sound...")
(define o (open-vorbis-file "/tmp/siberia.ogg"))
(define s (vorbis->rsound o))
(displayln "Playing sound...")

(play s)
(sleep 10)
(collect-garbage)
(sleep 600)

