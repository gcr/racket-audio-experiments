#lang racket
(require (except-in "vorbisfile.rkt" define/native)
         (except-in "openal/main.rkt" define/native)
         data/queue)

(define filename (vector-ref (current-command-line-arguments) 0))
(printf "Playing ~a\n" filename)

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

;; Streaming through OpenAL requires that we have a queue of buffers.
;; When OpenAL finishes playing one buffer, we take it out of the queue,
;; decode the vorbis file into the buffer, and re-queue it.
(define NUM-BUFFERS 5)
(define BUFFER-SIZE (* 4096 8))

;; Make our OpenAL source
(define source (car (gen-sources 1)))

;; Make our initial buffer queues, filling them with data. There are
;; two queues here -- the internal queue maintained by OpenAL (which
;; we can't inspect) and this buffer-queue.
(define buffer-queue (make-queue))
(for ([buffer (in-list (gen-buffers NUM-BUFFERS))])
     ; Load data
     (buffer-data buffer AL_FORMAT_STEREO16
                  (read-bytes BUFFER-SIZE vorbis-binary)
                  (vorbis-frequency m))
     ; Queue buffer in the source
     (source-queue-buffers! source (list buffer))
     (enqueue! buffer-queue buffer))

(displayln "Playing...")
(play-source source)

;; (thread (λ()
;;           (let loop ()
;;               (set-source-pitch!
;;                source
;;                (+ 1.0
;;                   (/ (sin (/ (current-milliseconds) 100))
;;                      50)))
;;               (sleep 0.05)
;;               (loop))))

(let loop ()
  ;(collect-garbage)
  ;; Uncomment for fun! :D

  (if (zero? (source-buffers-processed source))
      ; Nothing to do
      (begin (sleep 0.1)
             (newline)
             (loop))
      ; We're finished playing a buffer so fill it up again
      (begin (displayln "Refilling buffer")
             ;; Take it out
             (let ([buffer (dequeue! buffer-queue)])
               (source-unqueue-buffers! source (list buffer))
               ;; Refill with vorbis data
               (let ([buf (read-bytes BUFFER-SIZE vorbis-binary)])
                 (if (eof-object? buf)
                     (begin
                       ;; At the end of the file -- restart
                       (vorbis-seek! m 0.0)
                       (loop))
                     (begin
                       ;; Not at the end
                       ;; Load our data back into the buffer
                       (buffer-data buffer AL_FORMAT_STEREO16
                                    buf
                                    (vorbis-frequency m))
                       ;; Put it back on both queues
                       (source-queue-buffers! source (list buffer))
                       (enqueue! buffer-queue buffer)

                       (displayln (vorbis-current-time m))
                       (loop))))))))

(printf "File ended\n")
;; Well, not really -- the last few buffers are still playing but
;; frankly I don't care.

(set-current-context #f)
(destroy-context! context)
(close-device! device)
