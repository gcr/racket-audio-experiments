#lang racket
(require (except-in "vorbisfile.rkt" define/native)
         (except-in "openal.rkt" define/native)
         data/queue)

;; Initialize OpenAL
(define device (open-device #f))
(define context (create-context device))
(set-current-context context)

;; Open vorbis file!
(define m (open-vorbis-file "/tmp/siberia.ogg"))

(printf "Rate: ~a Channels: ~a\n"
        (vorbis-frequency m)
        (vorbis-channels m))

(define NUM-BUFFERS 5)
(define BUFFER-SIZE (* 4096 8))

(define bitstream-ptr (make-bitstream-ptr))

;; Make our source
(define source (car (gen-sources 1)))

;; Make our buffer queues
(define buffer-queue (make-queue))
(for ([buffer (in-list (gen-buffers NUM-BUFFERS))])
     (let ([buf (vorbis-read-bytes-exact! m BUFFER-SIZE 0 2 1 bitstream-ptr)])
       (printf "Loading ~a bytes to buffer ~a\n" (bytes-length buf) buffer)
       (buffer-data buffer AL_FORMAT_STEREO16
                    buf
                    (vorbis-frequency m)))
     (source-queue-buffers! source (list buffer))
     (enqueue! buffer-queue buffer))

(printf "Source has ~a buffers\n" (source-buffers-queued source))
(printf "Source has ~a processed buffers\n" (source-buffers-processed source))
(displayln "Playing...")
(play-source source)

(printf "State: ~a\n" (source-source-state source))

(let loop ()
  (set-source-pitch! source
                     (+ 1.0
                        (/ (sin (/ (current-inexact-milliseconds) 1000))
                           50)))
  (if (zero? (source-buffers-processed source))
      (begin (sleep 0.1)
             (loop))
      (begin (displayln "Refilling buffer")
             ;; Take it out
             (let ([buffer (dequeue! buffer-queue)])
               (source-unqueue-buffers! source (list buffer))
               ;; Fill it up
               (let ([buf (vorbis-read-bytes-exact! m
                                                    BUFFER-SIZE
                                                    0 2 1
                                                    bitstream-ptr)])
                 (unless (eof-object? buf)
                   (buffer-data buffer AL_FORMAT_STEREO16
                                buf
                                (vorbis-frequency m))
                   ;; Put it back
                   (source-queue-buffers! source (list buffer))
                   (enqueue! buffer-queue buffer)
                   (displayln (vorbis-current-time m))
                   (loop)))))))

(printf "Ended\n")
(set-current-context #f)
(destroy-context! context)
(close-device! device)
