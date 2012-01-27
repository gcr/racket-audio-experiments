#lang racket/base
(require (planet gcr/vorbisfile)
         (planet gcr/vorbisfile/rsound-compat)
         (planet clements/rsound)
         (planet clements/rsound/draw))

(define filename (vector-ref (current-command-line-arguments) 0))

(displayln "Loading sound...")
(define o (open-vorbis-file filename))
(define s (vorbis->rsound o))
(displayln "Playing 30s of sound...")

(play s)
(sleep 30)

