#lang racket

(require "vorbisfile.rkt"
         (planet clements/rsound))

(provide (all-defined-out))

(define (vorbis->rsound vf)
  (define rs-bytes
    (port->bytes
     (make-vorbis-input-port vf 0 2 1)))
  ;; Sorry! Gotta load the entire sound into memory to ensure
  ;; correctness -- what if rsound calls our signal functions more
  ;; than once?
  (define (s16->real x)
    (/ (exact->inexact x) s16max/i))

  (define (left-sig fr)
    (s16->real
     (integer-bytes->integer rs-bytes #t #f
                             (* 4 fr)
                             (+ 2 (* 4 fr)))))

  (define (right-sig fr)
    (s16->real
     (integer-bytes->integer rs-bytes #t #f
                             (+ 2 (* 4 fr))
                             (+ 4 (* 4 fr)))))
  (define (mono-sig fr)
    (s16->real
     (integer-bytes->integer rs-bytes #t #f
                             (* 2 fr) (* 2 (add1 fr)))))
  (parameterize ([default-sample-rate (vorbis-frequency vf)])
    (case (vorbis-channels vf)
      [(1) (mono-signal->rsound (sub1 (vorbis-length-samples vf)) mono-sig)]
      [(2) (signals->rsound (sub1 (vorbis-length-samples vf))
                            left-sig
                            right-sig)]
      [else (raise-type-error 'vorbisfile->rsound "We only support files with 1 or 2 channels.")])))