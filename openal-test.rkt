#lang racket
(require "openal.rkt")

(define device (open-device #f))

(define context (create-context device))
(set-current-context context)
(define buffers (gen-buffers 1))
(printf "Buffers: ~a\n" buffers)

(define buffer (car buffers))

; simple square wave or something
(buffer-data buffer AL_FORMAT_STEREO8 #"ppppppppppppppppppppppppppppppppppppppppppppppppppppppppppppppppppppppppppppppppppppppppppppppppppppppppppppppppppppppppppppppppppppPppppppppppppppppppppppppppppppp" 44100)

(define sources (gen-sources 1))
(printf "Sources: ~a\n" sources)

(alSourcei (car sources) AL_BUFFER buffer)

(play-source (car sources))
(alSourcei (car sources) AL_LOOPING AL_TRUE)

(sleep 10)



(set-current-context #f)
(destroy-context! context)
(close-device! device)


