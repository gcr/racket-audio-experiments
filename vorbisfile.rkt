#lang racket

;; A simple vorbis library.

(require ffi/unsafe
         ffi/unsafe/define
         ffi/unsafe/alloc
         ffi/vector)

(provide (all-defined-out))

(define libvorbis
  (ffi-lib "libvorbisfile"))

(define-ffi-definer define/native
  libvorbis)

(define-cstruct _ov_callbacks
  ([read_func (_fun [ptr : _pointer]
                    [size : _int]
                    [nmemb : _int]
                    [datasource : _pointer] -> _int)]
   [seek_func (_fun [datasource : _pointer]
                    [offset : _int64]
                    [whence : _int] -> _int)]
   [close_func (_fun [datasource : _pointer] -> _int)]
   [tell_func (_fun [datasource : _pointer] -> _long)]))

(define-cstruct _ogg_sync_state
  ([data _bytes]
   [storage _int]
   [fill _int]
   [returned _int]
   [unsynced _int]
   [headerbytes _int]
   [bodybytes _int]))

(define-cstruct _vorbis_info
  ([version _int]
   [channels _int]
   [rate _long]
   [bitrate_upper _long]
   [bitrate_nominal _long]
   [bitrate_lower _long]
   [bitrate_window _long]
   [codec_setup _pointer]))

(define-cstruct _vorbis_dsp_state
  ([analysisp _int]
   [vi _vorbis_info-pointer]
   [pcm _pointer]
   [pcmret _pointer]
   [pcm_storage _int]
   [pcm_current _int]
   [pcm_returned _int]
   [preextrapolate _int]
   [eofflag _int]
   [lW _long]
   [W _long]
   [nW _long]
   [centerW _long]
   [granulepos _int64]
   [sequence _int64]
   [glue_bits _int64]
   [time_bits _int64]
   [floor_bits _int64]
   [res_bits _int64]
   [backend_state _pointer]))

(define-cstruct _oggpack_buffer
  ([endbyte _long]
   [endint _int]
   [buffer _pointer]
   [ptr _pointer]
   [storage _long]))

(define-cstruct _vorbis_block
  ([pcm _float]
   [opb _oggpack_buffer]
   [lW _long]
   [W _long]
   [nW _long]
   [pcmend _int]
   [mode _int]
   [eofflag _int]
   [granulepos _int64]
   [sequence _int64]
   [vd _vorbis_dsp_state-pointer]
   [localstore _pointer]
   [localtop _long]
   [localalloc _long]
   [totaluse _long]
   [alloc_chain _pointer]
   [glue_bits _long]
   [time_bits _long]
   [floor_bits _long]
   [res_bits _long]
   [internal _pointer]))

(define-cstruct _vorbis_comment
  ([user_comments _pointer]
   [comment_lengths _pointer]
   [comments _int]
   [vendor _string/utf-8]))

(define-cstruct _ogg_stream_state
  ([body_data _pointer]
   [body_storage _long]
   [body_fill _long]
   [body_returned _long]
   [lacing_vals _intptr]
   [granule_vals _pointer]
   [lacing_storage _long]
   [lacing_fill _long]
   [lacing_packet _long]
   [lacing_returned _long]
   [header (_array _uint8 282)]
   [header_fill _int]
   [e_o_s _int]
   [b_o_s _int]
   [serialno _long]
   [pageno _long]
   [packetno _int64]
   [granulepos _int64]))

(define-cstruct _OggVorbis_File
  ([datasource _pointer]
   [seekable _int]
   [offset _int64]
   [end _int64]
   [oy _ogg_sync_state]
   [links _int]
   [offsets _pointer]
   [dataoffsets _pointer]
   [serialnos _pointer]
   [pcmlengths _pointer]
   [vi _vorbis_info-pointer]
   [vc _vorbis_comment-pointer]
   [pcm_offset _int64]
   [ready_state _int]
   [current_serialno _long]
   [current_link _int]
   [bittrack _double]
   [samptrack _double]
   [os _ogg_stream_state]
   [vd _vorbis_dsp_state]
   [vb _vorbis_block]
   [callbacks _ov_callbacks]))

;;; FUNCTIONS ;;;

(define/native close-vorbis-file!
  (_fun [vf : _pointer] -> _int)
  #:c-id ov_clear
  #:wrap (releaser))
;;  XXX Would this actually deallocate the OggVorbis_file ?
;; Note that this only clears it; it does not deallocate it.

(define/native open-vorbis-file
  (_fun [path : _path]
        [vf : _pointer
            = (malloc _OggVorbis_File 'interior)]
        ;; XXX I'm doing this wrong . ... . ...
        -> [return : _int]
        -> (if (zero? return) vf #f))
  #:c-id ov_fopen
  #:wrap (allocator close-vorbis-file!))

(define/native vorbis-length-sec
  (_fun [vf : _pointer]
        [channel : _int = -1]
        -> [ret : _double]
        -> (if (= ret -131) #f ret))
  #:c-id ov_time_total)

(define (make-bitstream-ptr) (malloc _int))

(define max-buffer-size 40960)
(define *buffer* (make-bytes max-buffer-size 0))

(define/native vorbis-read-bytes!
  (_fun (vf len bigendianp wordsize signedp bitstream) ::
   [vf : _pointer]
   [buf : _bytes = *buffer* #;(_u8vector o len)]
   [len : _int = (min len max-buffer-size)]
   [bigendianp : _int]
   [wordsize : _int]
   [signedp : _int]
   [bitstream : _pointer]
   -> [byteswritten : _int]
   -> (cond
       [(zero? byteswritten) eof]
       [(> byteswritten 0) (subbytes buf 0 byteswritten)]
       [else (error "Corrupt file or other file error")]))
  #:c-id ov_read)

(define/native vorbis-current-time
  (_fun [vf : _pointer] -> _double)
   #:c-id ov_time_tell)

(define/native vorbis-seek!
  (_fun [vf : _pointer] [s : _double]
        -> [return : _int]
        -> (zero? return))
  #:c-id ov_time_seek_lap)

(define/native vorbis-avg-bitrate
  (_fun [vf : _pointer] [i : _int = -1]
        -> _long)
  #:c-id ov_bitrate)

(define/native _ov_info
  (_fun [vf : _pointer] [i : _int = -1]
        -> _vorbis_info-pointer)
  #:c-id ov_info)

(define (vorbis-channels vf)
  (vorbis_info-channels (_ov_info vf)))
(define (vorbis-frequency vf)
  (vorbis_info-rate (_ov_info vf)))

(define/native _ov_comment
  (_fun [vf : _pointer] [i : _int = -1]
        -> _vorbis_comment-pointer)
  #:c-id ov_comment)

(define (vorbis-comments vf)
  (let* ([vc (_ov_comment vf)]
         [num_comments (vorbis_comment-comments vc)]
         [comment_lengths
          (cblock->list (vorbis_comment-comment_lengths vc)
                        _int
                        num_comments)]
         [comments (vorbis_comment-user_comments vc)])
    (let loop ([curpos 0]
               [curlen comment_lengths]
               [curcomments '()])
      (if (null? curlen)
          (reverse curcomments)
          (loop (add1 curpos)
                (cdr curlen)
                (cons (ptr-ref (ptr-add comments curpos _pointer) _string/utf-8)
                      curcomments))))))
(define (vorbis-vendor vf)
  (define vc (_ov_comment vf))
  (printf "Vendor: ~a\n" (vorbis_comment-vendor vc)))



#|
(defvorbis ov_clear [vf : _pointer] -> _int)
(defvorbis ov_fopen [path: _path] [vf : _pointer] -> _int)
(defvorbis ov_test_open [vf : _pointer] -> _int)
(defvorbis ov_bitrate [vf: _pointer] [i : _int] -> _long)
(defvorbis ov_bitrate_instant [vf : _pointer] -> _long)
(defvorbis ov_streams [vf : _pointer] -> _long)
(defvorbis ov_seekable [vf : _pointer] -> _long)
(defvorbis ov_serialnumber [vf : _pointer] [i : _int] -> _long)

(defvorbis ov_raw_total [vf : _pointer] [i : _int] -> _int64)
(defvorbis ov_pcm_total [vf : _pointer] [i : _int] -> _int64)
(defvorbis ov_time_total [vf : _pointer] [i : _int] -> _double)

(defvorbis ov_raw_seek [vf : _pointer] [pos : _int64] -> _int)
(defvorbis ov_pcm_seek [vf : _pointer] [pos : _int64] -> _int)
(defvorbis ov_pcm_seek_page [vf : _pointer] [pos : _int64] -> _int)
(defvorbis ov_time_seek [vf : _pointer] [pos : _double] -> _int)
(defvorbis ov_time_seek_page [vf : _pointer] [pos : _double] -> _int)

(defvorbis ov_raw_seek_lap [vf : _pointer] [pos : _int64] -> _int)
(defvorbis ov_pcm_seek_lap [vf : _pointer] [pos : _int64] -> _int)
(defvorbis ov_pcm_seek_page_lap [vf : _pointer] [pos : _int64] -> _int)
(defvorbis ov_time_seek_lap [vf : _pointer] [pos : _double] -> _int)
(defvorbis ov_time_seek_page_lap [vf : _pointer] [pos : _double] -> _int)

(defvorbis ov_raw_tell [vf : _pointer] -> _int64)
(defvorbis ov_pcm_tell [vf : _pointer] -> _int64)
(defvorbis ov_time_tell [vf : _pointer] -> _double)

(defvorbis ov_info [vf : _pointer] [link : _int] -> _vorbis_info)
(defvorbis ov_comment [vf : _pointer] [link : _int] -> _vorbis_comment)

#;(defvorbis ov_read_float [vf : _pointer] ... -> _long)
#;(defvorbis ov_read_filter [vf : _pointer] ... -> _long)

(defvorbis ov_read
  [vf : _pointer]
  [buffer : (_bytes length)]
  [length : _int]
  [bigendianp : _int]
  [word : _int]
  [spend : _int]
  [bitstream : _int-pointer]
  -> [return : _long]
  -> (values buffer return)


(defvorbis ov_crosslap [vf1 : _pointer] [vf2 : _pointer] -> _int)
(defvorbis ov_halfrate [vf : _pointer] [flag : _int] -> _int)
(defvorbis ov_halfrate_p [vf : _pointer] -> _int)
|#