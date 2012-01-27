#lang racket
(require (planet gcr/openal)
         (planet gcr/vorbisfile))

(define filename (vector-ref (current-command-line-arguments) 0))
(printf "Playing file ~a\n" filename)

;; Initialize OpenAL (see the OpenAL guide)
(define device (open-device #f))
(define context (create-context device))
(set-current-context context)

;; Open the file!
(define m (open-vorbis-file filename))

(printf "Rate: ~a Channels: ~a\n"
        (vorbis-frequency m)
        (vorbis-channels m))

;; To read the binary PCM data, we make a port that supplies us with
;; the binary decompressed data.
(define vorbis-binary (make-vorbis-input-port m 0 2 1))

;; Make our OpenAL source
(define source (car (gen-sources 1)))

;; Start streaming
(define stream-thread
  (stream-port-to-source vorbis-binary
                         source
                         AL_FORMAT_STEREO16
                         (vorbis-frequency m)))

;; Start playing
(play-source source)

;; Wait until we're fisished playing
(thread-wait stream-thread)

;; Clean up
(set-current-context #f)
(destroy-context! context)
(close-device! device)
