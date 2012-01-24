#lang setup/infotab
(define name "OpenAL Bindings")
(define blurb
  '("OpenAL is an audio library for Linux, Mac OSX, and Windows commonly used in games. This package provides some simple Racket bindings."))
(define primary-file "main.rkt")
(define categories '(media))
(define repositories '("4.x"))
(define scribblings '(("openal.scrbl" ())))
(define release-notes '((p "added documentation, "
                           "added 'stream-port-to-source'")))