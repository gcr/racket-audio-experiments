#lang racket

(require ffi/unsafe
         ffi/unsafe/define
         ffi/unsafe/alloc
         ffi/vector)

(provide (all-defined-out))

(define libopenal
  (ffi-lib "libopenal"))

(define-ffi-definer define/native libopenal)

; Enums
(define AL_INVALID -1)
(define AL_NONE 0)
(define AL_FALSE 0)
(define AL_TRUE 1)
(define AL_SOURCE_RELATIVE #x202)
(define AL_CONE_INNER_ANGLE #x1001)
(define AL_CONE_OUTER_ANGLE #x1002)
(define AL_PITCH #x1003)
(define AL_POSITION #x1004)
(define AL_DIRECTION #x1005)
(define AL_VELOCITY #x1006)
(define AL_LOOPING #x1007)
(define AL_BUFFER #x1009)
(define AL_GAIN #x100A)
(define AL_MIN_GAIN #x100D)
(define AL_MAX_GAIN #x100E)
(define AL_ORIENTATION #x100F)
(define AL_SOURCE_STATE #x1010)
(define AL_INITIAL #x1011)
(define AL_PLAYING #x1012)
(define AL_PAUSED #x1013)
(define AL_STOPPED #x1014)
(define AL_BUFFERS_QUEUED #x1015)
(define AL_BUFFERS_PROCESSED #x1016)
(define AL_SEC_OFFSET #x1024)
(define AL_SAMPLE_OFFSET #x1025)
(define AL_BYTE_OFFSET #x1026)
(define AL_SOURCE_TYPE #x1027)
(define AL_STATIC #x1028)
(define AL_STREAMING #x1029)
(define AL_UNDETERMINED #x1030)
(define AL_FORMAT_MONO8 #x1100)
(define AL_FORMAT_MONO16 #x1101)
(define AL_FORMAT_STEREO8 #x1102)
(define AL_FORMAT_STEREO16 #x1103)
(define AL_REFERENCE_DISTANCE #x1020)
(define AL_ROLLOFF_FACTOR #x1021)
(define AL_CONE_OUTER_GAIN #x1022)
(define AL_MAX_DISTANCE #x1023)
(define AL_FREQUENCY #x2001)
(define AL_BITS #x2002)
(define AL_CHANNELS #x2003)
(define AL_SIZE #x2004)
(define AL_UNUSED #x2010)
(define AL_PENDING #x2011)
(define AL_PROCESSED #x2012)
(define AL_NO_ERROR AL_FALSE)
(define AL_INVALID_NAME #xA001)
(define AL_INVALID_ENUM #xA002)
(define AL_INVALID_VALUE #xA003)
(define AL_INVALID_OPERATION #xA004)
(define AL_OUT_OF_MEMORY #xA005)
(define AL_VENDOR #xB001)
(define AL_VERSION #xB002)
(define AL_RENDERER #xB003)
(define AL_EXTENSIONS #xB004)
(define AL_DOPPLER_FACTOR #xC000)
(define AL_DOPPLER_VELOCITY #xC001)
(define AL_SPEED_OF_SOUND #xC003)
(define AL_DISTANCE_MODEL #xD000)
(define AL_INVERSE_DISTANCE #xD001)
(define AL_INVERSE_DISTANCE_CLAMPED #xD002)
(define AL_LINEAR_DISTANCE #xD003)
(define AL_LINEAR_DISTANCE_CLAMPED #xD004)
(define AL_EXPONENT_DISTANCE #xD005)
(define AL_EXPONENT_DISTANCE_CLAMPED #xD006)
(define AL_ILLEGAL_ENUM AL_INVALID_ENUM)
(define AL_ILLEGAL_COMMAND AL_INVALID_OPERATION)



(define/native open-device
  (_fun [devicename : _string] -> _pointer)
  #:c-id alcOpenDevice
;  #:wrap (allocator)
  )

(define/native create-context
  (_fun [device : _pointer] [attrlist : _pointer = #f] -> _pointer)
  #:c-id alcCreateContext
  )

(define/native set-current-context
  (_fun [context : _pointer] -> _bool)
  #:c-id alcMakeContextCurrent
  )

(define/native destroy-context!
  (_fun [context : _pointer] -> _void)
  #:c-id alcDestroyContext)

(define/native get-last-error
  (_fun [device : _pointer] -> _int)
  #:c-id alcGetError
  )

(define/native gen-buffers
  (_fun [num-buffers : _int]
        [buffers : (_list o _int num-buffers)]
        -> _void
        -> buffers)
  #:c-id alGenBuffers)

(define/native delete-buffers!
  (_fun (num-buffers buffers) ::
        [num-buffers : _int = (length buffers)]
        [buffers : (_list i _int)]
        -> _void)
  #:c-id alDeleteBuffers)

(define/native buffer?
  (_fun [bufid : _int] -> _bool)
  #:c-id alIsBuffer)

(define/native buffer-data
  (_fun [bufid : _int]
        [format : _int]
        [data : _bytes]
        [size : _int = (bytes-length data)]
        [freq : _int] -> _void)
  #:c-id alBufferData)

(define/native gen-sources
  (_fun [num-sources : _int]
        [sources : (_list o _int num-sources)]
        -> _void
        -> sources)
  #:c-id alGenSources)

(define/native delete-sources!
  (_fun (num-sources sources) ::
        [num-sources : _int = (length sources)]
        [sources : (_list i _int)]
        -> _void)
  #:c-id alDeleteSources)

(define/native alSourcei
  (_fun [sid : _int]
        [param : _int]
        [value : _int] -> _void)
  #:c-id alSourcei)

(define/native play-source
  (_fun [sid : _int] -> _void)
  #:c-id alSourcePlay)

(define/native stop-source
  (_fun [sid : _int] -> _void)
  #:c-id alSourceStop)

(define/native get-current-context
  (_fun -> [context : _pointer])
  #:c-id alcGetCurrentContext)

(define/native get-device-from-context
  (_fun [context : _pointer] -> [device : _pointer])
  #:c-id alcGetContextsDevice)

(define/native close-device!
  (_fun [device : _pointer] -> _bool)
  #:c-id alcCloseDevice)