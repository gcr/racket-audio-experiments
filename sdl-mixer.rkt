#lang racket
(require ffi/unsafe)

(define libsdl-mixer
  (ffi-lib "libSDL_mixer"))

(define-syntax defsdlmixer
  (syntax-rules (:)
    [(_ name : type ...)
     (define name
       (get-ffi-obj (regexp-replaces 'name '(;(#rx"-" "_")
                                             (#rx"^" "Mix_")))
                    libsdl-mixer (_fun type ...)))]))


;; This comes directly from SDL/SDL_mixer.h
(define _mix-initflags
  (_bitmask '(mix-init-flac = #x00000001
              mix-init-mod  = #x00000002
              mix-init-mp3  = #x00000004
              mix-init-ogg  = #x00000008)))
(define *audio-u8* #x0008)
(define *audio-s8* #x8008)
(define *audio-u16lsb* #x0010)
(define *audio-s16lsb* #x8010)
(define *audio-u16msb* #x1010)
(define *audio-s16msb* #x9010)
(define *audio-u16* *audio-u16lsb*)
(define *audio-s16* *audio-s16lsb*)
(define *mix-default-frequency* 22050)
(define *mix-default-format* *audio-s16lsb*)
(define *mix-default-channels* 2)
(define *mix-channel-post* -2)


(define-cstruct _mix-chunk ([allocated _int]
                            [abuf      _bytes]
                            [alen      _uint32]
                            [volume    _uint8]))    ; 0-128

(define _mix-fading (_enum '(mix-no-fading
                             mix-fading-out
                             mix-fading-in)))

(define _mix-musictype (_enum '(mus-none
                                mus-cmd
                                mus-wav
                                mus-mod
                                mus-mid
                                mus-ogg
                                mus-mp353
                                mus-mp3-mad
                                mus-flac)))

(define-cstruct _mix-music ([type _mix-musictype]
                            [data _pointer]
                            [fading _mix-fading]
                            [fade-step _int]
                            [fade-steps _int]
                            [error _int]))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defsdlmixer Init : _mix-initflags -> _) ; pass in the mix-initflags OR'd together.
(defsdlmixer Quit : -> _void)

; Open the mixer with a certain audio format
(defsdlmixer OpenAudio :
  (freq : _int)
  (format : _uint16)
  (channels : _int)
  (chunksize : _int) -> _int)

; Dynamically change the number of channels managed by the mixer. (Returns new number of channels)
(defsdlmixer AllocateChannels : _int -> _int)

; Find the parameters of the audio device.
; Returns 1 if the audio has been opened, 0 otherwise.
(defsdlmixer QuerySpec :
  [freq : (_ptr o _int)]
  [format : (_ptr o _uint16)]
  [channels : (_ptr o _int)]
  -> (opened? : _int)
  -> (values opened? freq format channels))

; Load a wave file
(defsdlmixer LoadMUS : _path -> _mix-music-pointer)

(defsdlmixer QuickLoad_WAV : _bytes -> _mix-chunk-pointer)

(defsdlmixer FreeChunk : _mix-chunk-pointer -> _void)
(defsdlmixer FreeMusic : _mix-music-pointer -> _void)

; List of chunk decoders
(defsdlmixer GetNumChunkDecoders : -> _int)
(defsdlmixer GetChunkDecoder : _int -> _string)
(defsdlmixer GetNumMusicDecoders : -> _int)
(defsdlmixer GetMusicDecoder : _int -> _string)

(defsdlmixer GetMusicType : _mix-music-pointer -> _mix-musictype)

;/* Set a function that is called after all mixing is performed.
;   This can be used to provide real-time visual display of the audio stream
;   or add a custom mixer filter for the stream data.
;*/
;extern DECLSPEC void SDLCALL Mix_SetPostMix(void (*mix_func)
;                             (void *udata, Uint8 *stream, int len), void *arg);
(defsdlmixer SetPostMix : (_fun [udata : _pointer]
                                [stream : _bytes]
                                [len : _int] -> _void)
                           [arg : _pointer] -> _void)

;/* Add your own music player or additional mixer function.
;   If 'mix_func' is NULL, the default music player is re-enabled.
; */
;extern DECLSPEC void SDLCALL Mix_HookMusic(void (*mix_func)
;                          (void *udata, Uint8 *stream, int len), void *arg);
(defsdlmixer HookMusic : (_fun [udata : _pointer]
                               [stream : _bytes]
                               [len : _int] -> _void)
                           [arg : _pointer] -> _void)

;/* Add your own callback when the music has finished playing.
;   This callback is only called if the music finishes naturally.
; */
;extern DECLSPEC void SDLCALL Mix_HookMusicFinished(void (*music_finished)(void));
(defsdlmixer HookMusicFinished : (_fun -> _void) -> _void)

; Get a pointer to the user data for the current music hook
(defsdlmixer GetMusicHookData : -> _void)

(defsdlmixer ChannelFinished : (_fun [channel : _int] -> _void) -> _void)


(define _effect-funct (_fun #:async-apply values ; TODO
                            [chan : _int]
                            [stream : _pointer]
                            [len : _int]
                            [user-data : _pointer] -> _void))
(define _effect-done (_fun #:async-apply values
                           [chan : _int]
                           [user-data : _pointer] -> _void))

(defsdlmixer RegisterEffect : [chan : _int]
                              [f : _effect-funct]
                              [d : _effect-done]
                              [arg : _pointer] -> _int)

(defsdlmixer UnregisterEffect : [chan : _int]
                                [f : _effect-funct] -> _void)

(defsdlmixer UnregisterAllEffects : [chan : _int] -> _void)


(defsdlmixer SetPanning : [channel : _int]
                          [left : _uint8]
                          [right : _uint8] -> _int)

; Set the position of a channel. (angle) is an int from 0 to 360 that
; specifies the location of the sound in relation to the listener. It will be
; reduced as necessary. 0 is due north and rotates clockwise.
(defsdlmixer SetPosition : [channel : _int]
                           [angle : _sint16]
                           [distance : _uint8] -> _int)

(defsdlmixer SetDistance : [channel : _int] [distance : _uint8] -> _int)

(defsdlmixer SetReverseStereo : [channel : _int] [flip : _int] -> _int)

(defsdlmixer ReserveChannels : _int -> _int)

; Channel grouping
(defsdlmixer GroupChannel : [which : _int] [tag : _int] -> _int)
(defsdlmixer GroupChannels : [from : _int] [to : _int] [tag : _int] -> _int)
(defsdlmixer GroupAvailable : _int -> _int) ; Finds the first group available, or -1 if none
(defsdlmixer GroupCount : _int -> _int)
(defsdlmixer GroupOldest : _int -> _int)
(defsdlmixer GroupNewer : _int -> _int)

; Play a chunk on a specific channel.
; If channel is -1, play on the first free channel.
; If loops is greater than 0, loop that many times, or -1 to loop infinitely (~65000)
; Returns which channel was used to play the sound
(defsdlmixer PlayChannelTimed : [channel : _int]
                                    _mix-chunk-pointer
                                    [loops : _int]
                                    [ticks : _int] -> _int)
(define (PlayChannel channel chunk loops)
  (PlayChannelTimed channel chunk loops -1))
(defsdlmixer PlayMusic : _mix-music-pointer [loops : _int] -> _int)

; Fade in music or a channel over specified ms
(defsdlmixer FadeInMusic : _mix-music-pointer [loops : _int] [ms : _int] -> _int)
(defsdlmixer FadeInMusicPos : _mix-music-pointer [loops : _int] [ms : _int] [pos : _double] -> _int)
(defsdlmixer FadeInChannelTimed : [chan : _int]
                                      _mix-chunk-pointer
                                      [loops : _int]
                                      [ms : _int]
                                      [ticks : _int] -> _int)
(define (FadeInChannel chan chunk loops ms)
  (FadeInChannelTimed chan chunk loops ms -1))

; If channel is -1, set for all. Returns the original. If volume is -1, just return current.
(defsdlmixer Volume : _int _int -> _int)
(defsdlmixer VolumeChunk : _mix-chunk-pointer _int -> _int)
(defsdlmixer VolumeMusic : _int -> _int)

(defsdlmixer HaltChannel : _int -> _int)
(defsdlmixer HaltGroup : _int -> _int)
(defsdlmixer HaltMusic : -> _int)

(defsdlmixer ExpireChannel : [chan : _int]
                                 [ticks : _int] -> _int)

(defsdlmixer FadeOutChannel : [chan : _int]
                                  [ms : _int] -> _int)
(defsdlmixer FadeOutGroup : [tag : _int] [ms : _int] -> _int)
(defsdlmixer FadeOutMusic : [ms : _int] -> _int)

; Query fading status of channel or music
(defsdlmixer FadingChannel : _int -> _mix-fading)
(defsdlmixer FadingMusic : -> _mix-fading)

(defsdlmixer Pause : _int -> _void)
(defsdlmixer Resume : _int -> _void)
(defsdlmixer Paused : _int -> _int)

(defsdlmixer PauseMusic : -> _void)
(defsdlmixer ResumeMusic : -> _void)
(defsdlmixer RewindMusic : -> _void)
(defsdlmixer PausedMusic : -> _int)

; only successful for MOD music formats (set pattern order number) and for OGG (set pos in seconds)
(defsdlmixer SetMusicPosition : _double -> _int)

; check statuses
(defsdlmixer Playing : _int -> _int)
(defsdlmixer PlayingMusic : -> _int)

; external music playback command
(defsdlmixer SetMusicCMD : _bytes -> _int)

(defsdlmixer SetSynchroValue : _int -> _int)
(defsdlmixer GetSynchroValue : -> _int)

; Which chunk is associated with this mixer channel?
(defsdlmixer GetChunk : _int -> _mix-chunk-pointer)

(defsdlmixer CloseAudio : -> _void)