#lang scribble/doc
@(require scribble/manual
          planet/scribble
          (for-label racket)
          (for-label (this-package-in main)))

@title{@bold{OpenAL}: Bindings for the OpenAL sound library}
@author{gcr}

@(define openal-programmers-guide
   (hyperlink "http://connect.creativelabs.com/openal/Documentation/OpenAL_Programmers_Guide.pdf" "OpenAL Programmer's Guide"))

@defmodule/this-package[main]{
  This library provides low-level bindings for the OpenAL sound library.
  Because these bindings are low-level, you may want to look at the
  @openal-programmers-guide to get a sense of how it works.
}

@table-of-contents[]

@section[#:tag "example"]{Example: Playing sound from a raw buffer}

In this example, we play an 8-bit sine wave generated from a
bytestring.

@codeblock{
#lang racket
(require (planet gcr/openal))

;; Our sound data
(define sinewave
  (list->bytes
   (for/list ([i (in-range 0 6.28 0.05)])
     (inexact->exact
       (floor
         (+ 128
            (* (sin i) 64)))))))
}

First, we must open the sound device and create a context for it:

@codeblock{
;; Initialize OpenAL (see the OpenAL guide)
(define device (open-device #f))
(define context (create-context device))
(set-current-context context)
}

Once we have a device context, we must create a buffer to hold our
sound data.

@codeblock{
;; Make our OpenAL buffer
(define buffer (car (gen-buffers 1)))
;; Copy the bytes to this buffer
(buffer-data buffer
             AL_FORMAT_MONO8
             sinewave
             44100)
}

In this example, each unsigned byte of @racket[sinewave] is a separate
8-bit sound sample. Realistically, a real-world application would want
to use 16-bit samples, but this grainy, ugly example is fine for us.

Once our data is safe in an OpenAL buffer, we make a source and play
it. Note that OpenAL internally copies that data -- we are free to mutate it however we like after the call to @racket[buffer-data].

@codeblock{
;; Make our OpenAL source
(define source (car (gen-sources 1)))

;; Bind our buffer to the source -- 8-bit mono
(set-source-buffer! source buffer)
}

OpenAL allows us to define several simultaneously playing sources.
Though a single source can only have one buffer, note that a buffer
can be bound to several sources at the same time to save space.

Now that we have a source and a sound buffer to play, we can set our
source to play the data from the sound buffer.

@codeblock{
;; Loop forever (without this, the source will play just once and then stop)
(set-source-looping! source AL_TRUE)

;; Start playing
(play-source source)
}

OpenAL uses a separate OS-level thread to play sounds. The
@racket[play-source] call will exit instantly and our sound will
continue to play in the background until we choose to stop it.

@codeblock{
;; Wait until we're fisished playing
(sleep 5)

;; Clean up
(delete-sources! (list source))
(delete-buffers! (list buffer))
(set-current-context #f)
(destroy-context! context)
(close-device! device)
}

@section{Constants}

All constants defined in @tt{al.h} and @tt{alc.h} are re-exported
under their usual names. See the @openal-programmers-guide for
information about what each constant means.

@(define-syntax-rule (def-constants (decl ...) body ...)
  (deftogether
    (@defthing[decl exact-integer?] ...)
     body ...))
@def-constants[(
AL_BITS
AL_BUFFER
AL_BUFFERS_PROCESSED
AL_BUFFERS_QUEUED
AL_BYTE_OFFSET
AL_CHANNELS
AL_CONE_INNER_ANGLE
AL_CONE_OUTER_ANGLE
AL_CONE_OUTER_GAIN
AL_DIRECTION
AL_DISTANCE_MODEL
AL_DOPPLER_FACTOR
AL_DOPPLER_VELOCITY
AL_EXPONENT_DISTANCE
AL_EXPONENT_DISTANCE_CLAMPED
AL_EXTENSIONS
AL_FALSE
AL_FORMAT_MONO16
AL_FORMAT_MONO8
AL_FORMAT_STEREO16
AL_FORMAT_STEREO8
AL_FREQUENCY
AL_GAIN
AL_ILLEGAL_COMMAND
AL_ILLEGAL_ENUM
AL_INITIAL
AL_INVALID
AL_INVALID_ENUM
AL_INVALID_NAME
AL_INVALID_OPERATION
AL_INVALID_VALUE
AL_INVERSE_DISTANCE
AL_INVERSE_DISTANCE_CLAMPED
AL_LINEAR_DISTANCE
AL_LINEAR_DISTANCE_CLAMPED
AL_LOOPING
AL_MAX_DISTANCE
AL_MAX_GAIN
AL_MIN_GAIN
AL_NO_ERROR
AL_NONE
AL_ORIENTATION
AL_OUT_OF_MEMORY
AL_PAUSED
AL_PENDING
AL_PITCH
AL_PLAYING
AL_POSITION
AL_PROCESSED
AL_REFERENCE_DISTANCE
AL_RENDERER
AL_ROLLOFF_FACTOR
AL_SAMPLE_OFFSET
AL_SEC_OFFSET
AL_SIZE
AL_SOURCE_RELATIVE
AL_SOURCE_STATE
AL_SOURCE_TYPE
AL_SPEED_OF_SOUND
AL_STATIC
AL_STOPPED
AL_STREAMING
AL_TRUE
AL_UNDETERMINED
AL_UNUSED
AL_VELOCITY
AL_VENDOR
AL_VERSION
)]

@section{Device and context management}

With OpenAL, we must manage our own devices and our device contexts.
It's good practice to close all devices at the end of your program --
some platforms may get confused.

@defproc[(open-device [devicename (or/c string? #f)]) any/c]{
Opens an OpenAL device. Pass @racket[#f] to just use the default device.
}

@defproc[(close-device! [device any/c]) bool?]{
Closes an OpenAL device.
}

@defproc[(create-context [device any/c]) any/c]{
Creates an OpenAL device context using a device. You must use
@racket[set-current-context] to make it active.
}

@defproc[(set-current-context [context any/c]) any/c]{
Sets the @racket[context] to be the current context for the device.
You must do this before you can play any sound.
}

@defproc[(destroy-context! [context any/c]) any/c]{
Removes @racket[context] from the device. Do not play any sounds after
using this without reinstating another device context.
}

@defproc[(get-last-error) exact-integer?]{
Returns (and clears) OpenAL's last error. Will be 0 if there are no errors.
}

@section{Buffers}

OpenAL buffers hold sound data. They are kept in completely separate
memory outside Racket's garbage collection pool, so you must free
buffers yourself when you're finished with them to avoid memory leaks.


@defproc[(gen-buffers [num exact-integer?]) (listof exact-integer?)]{
Generates @racket[num] buffers, and returns a list of the buffer IDs.

Once you have a buffer, you must load data with @racket[buffer-data],
bind it to a source with @racket[set-source-buffer!], and then play
the source with @racket[play-source]. See the @secref{example} section.
}

@defproc[(delete-buffers! [buffers (listof exact-integer?)]) (any/c)]{
Deletes the given @racket[buffers]. Do not attempt to modify them
after calling this function.
}

@defproc[(buffer? [buffer exact-integer?]) (boolean?)]{
Returns whether @racket[buffer] is an OpenAL buffer.
}

@defproc[(buffer-data [bufid exact-integer?]
                      [format (one-of/c AL_FORMAT_MONO8
                                        AL_FORMAT_MONO16
                                        AL_FORMAT_STEREO8
                                        AL_FORMAT_STEREO16)]
                      [data bytes?]
                      [frequency exact-integer?]) (any/c)]{
Loads the data in @racket[data] to the given buffer with ID
@racket[bufid].

Format should be an OpenAL constant signifying the format of the
samples in @racket[data]. For 8-bit formats, a value of 128 means no
DC offset, with 0 and 255 being the extreme DC offsets. For 16-bit
formats, samples are signed little-endian 16-bit integers. Stereo
formats include interleaved samples, left first.

The @racket[frequency] of the data is how many samples per second the
source should play, usually 44100.

Note that @racket[data] is automatically copied, not referenced -- you
can reuse or mutate @racket[data] however you like after this call.
}


@subsection{Buffer properties}

@section{Sources}

Sources are actual playing sounds in the world. Each source can either
be bound to a single buffer or can contain a buffer queue that
continuously streams sound data.

Sources have a 3D position and velocity in space. This means that
OpenAL can optionally apply certain effects such as attenuation
(softening far-away sounds) and pitch shifting (the doppler effect).

@defproc[(gen-sources [num-sources exact-integer?]) (listof exact-integer?)]{
Generates a list of @racket[num-sources]. Note that these will not
be garbage-collected; you are responsible for freeing them yourself
with @racket[delete-sources!]
}

@defproc[(delete-sources! [sources (listof exact-integer!)]) (any/c)]{
Stops and removes the listed @racket[sources]. Do not attempt to
use them after calling this function.
}

@subsection{Source properties}

These bindings define two ``levels'' of functions that set and
retrieve properties. First, there are the low-level ``C-like''
functions like @racket[alSourcef] that allow you to get and set
individual properties of sources defined by an OpenAL constant.
Alternatively, there are the friendlier functions like
@racket[set-source-looping!] that offer a simpler interface for
setting the same properties.

@subsubsection{C-like getters and setters}

@defproc[(alSourcef [source exact-integer?] [param exact-integer?] [value real?]) (any/c)]{Sets the given @racket[param] of the given @racket[source] to the given @racket[value].}
@defproc[(alSource3f [source exact-integer?] [param exact-integer?] [value1 real?][value2 real?][value3 real?]) (any/c)]{Sets the given @racket[param] of the given @racket[source] to the given values.}
@defproc[(alSourcefv [source exact-integer?] [param exact-integer?]
[values (listof real?)]) (any/c)]{Sets the given @racket[param] of the given @racket[source] to the given @racket[values].}
@defproc[(alSourcei [source exact-integer?] [param exact-integer?] [value exact-integer?]) (any/c)]{Sets the given @racket[param] of the given @racket[source] to the given @racket[value].}
@defproc[(alSource3i [source exact-integer?] [param exact-integer?]
[value1 exact-integer?][value2 exact-integer?][value3 exact-integer?]) (any/c)]{Sets the given @racket[param] of the given @racket[source] to the given values.}
@defproc[(alSourceiv [source exact-integer?] [param exact-integer?]
[values (listof exact-integer?)]) (any/c)]{Sets the given @racket[param] of the given @racket[source] to the given @racket[values].}


@defproc[(alGetSourcef [source exact-integer?] [param exact-integer?]) (real?)]{Gets the given @racket[param] of the given @racket[source].}
@defproc[(alGetSource3f [source exact-integer?] [param
exact-integer?]) (list/c real? real? real?)]{Gets the given @racket[param] of the given @racket[source].}
@defproc[(alGetSourcefv [source exact-integer?] [param
exact-integer?]) (listof real?)]{Gets the given @racket[param] of the given @racket[source].}
@defproc[(alGetSourcei [source exact-integer?] [param exact-integer?]) (exact-integer?)]{Gets the given @racket[param] of the given @racket[source].}
@defproc[(alGetSource3i [source exact-integer?] [param
exact-integer?]) (list/c exact-integer? exact-integer? exact-integer?)]{Gets the given @racket[param] of the given @racket[source].}
@defproc[(alGetSourceiv [source exact-integer?] [param
exact-integer?]) (listof exact-integer?)]{Gets the given @racket[param] of the given @racket[source].}

@subsubsection{Friendly getters and setters}
@deftogether[(
  @defproc[(set-source-pitch! [source exact-integer?] [value real?]) (any/c)]
  @defproc[(source-pitch [source exact-integer?]) (real?)]
)]{
Gets or sets the @racket[source]'s pitch, a number greater
than 0. The default is 1.0. A @racket[value] of 2.0 will make the
sound play twice as fast and one octave higher; a value of 0.5 will make
the sound go twice as slow and one octave lower.
}
@deftogether[(
  @defproc[(set-source-gain! [source exact-integer?] [value real?]) (any/c)]
  @defproc[(source-gain [source exact-integer?]) (real?)]
)]{
Gets or sets the @racket[source]'s gain (volume), a number greater
than 0. The default is 1.0. Note that values higher than 1 may cause
clipping. Gain can also be set globally with @racket[set-listener-gain!]
}
}

@deftogether[(
  @defproc[(set-source-rolloff-factor! [source exact-integer?] [value real?]) (any/c)]
  @defproc[(source-rolloff-factor [source exact-integer?]) (real?)]
)]{
Gets or sets the rolloff rate of the source. See the
@secref{distance-models} section for more details.
}
@deftogether[(
  @defproc[(set-source-reference-distance! [source exact-integer?] [value real?]) (any/c)]
  @defproc[(source-reference-distance [source exact-integer?]) (real?)]
)]{
Gets or sets the source's reference distance, used for calculating the
gain. See the @secref{distance-models} section for more details.
}
@deftogether[(
  @defproc[(set-source-min-gain! [source exact-integer?] [value real?]) (any/c)]
  @defproc[(source-min-gain [source exact-integer?]) (real?)]
  @defproc[(set-source-max-gain! [source exact-integer?] [value real?]) (any/c)]
  @defproc[(source-max-gain [source exact-integer?]) (real?)]
  @defproc[(set-source-max-distance! [source exact-integer?] [value real?]) (any/c)]
  @defproc[(source-max-distance [source exact-integer?]) (real?)]
)]{
Gets or sets the source's minimum and maximum gain and distance. See the @secref{distance-models} section for more details.
}

@deftogether[(
  @defproc[(set-source-cone-outer-gain! [source exact-integer?] [value real?]) (any/c)]
  @defproc[(source-cone-outer-gain [source exact-integer?]) (real?)]
  @defproc[(set-source-cone-inner-angle! [source exact-integer?] [value real?]) (any/c)]
  @defproc[(source-cone-inner-angle [source exact-integer?]) (real?)]
  @defproc[(set-source-cone-outer-angle! [source exact-integer?] [value real?]) (any/c)]
  @defproc[(source-cone-outer-angle [source exact-integer?]) (real?)]
)]{
Gets or sets the parameters of an oriented sound cone around the
source. See the @openal-programmers-guide
}

@deftogether[(
  @defproc[(set-source-position! [source exact-integer?] [x real?][y real?][z real?]) (any/c)]
  @defproc[(source-position [source exact-integer?]) (list/c real? real? real?)]
  @defproc[(set-source-direction! [source exact-integer?] [x real?][y real?][z real?]) (any/c)]
  @defproc[(source-direction [source exact-integer?]) (list/c real? real? real?)]
  @defproc[(set-source-velocity! [source exact-integer?] [x real?][y real?][z real?]) (any/c)]
  @defproc[(source-velocity [source exact-integer?]) (list/c real? real? real?)]
)]{
Gets or sets the source's position, velocity, and direction in 3D space.
}

@deftogether[(
  @defproc[(set-source-source-relative! [source exact-integer?] [value (one-of/c 0 1)]) (any/c)]
  @defproc[(source-source-relative [source exact-integer?]) (one-of/c 0 1)]
)]{
Sets whether the source's position is relative to the listener or an
absolute position.
}

@deftogether[(
  @defproc[(source-source-type [source exact-integer?]) (one-of/c
  AL_STATIC AL_STREAMING AL_UNDETERMINED)]
)]{
Returns the source's type -- @racket[AL_STATIC] for buffered sources,
@racket[AL_STREAMING] for sources with buffer queues, and
@racket[AL_UNDETERMINED] for sources with unknown type.
}

@deftogether[(
  @defproc[(set-source-looping! [source exact-integer?] [value (one-of/c 0 1)]) (any/c)]
  @defproc[(source-looping [source exact-integer?]) (one-of/c 0 1)]
)]{
Gets or sets whether buffered sources should loop back to the
beginning of the buffer upon completion. May misbehave on streaming sources.
}
@deftogether[(
  @defproc[(set-source-buffer! [source exact-integer?] [buffer exact-integer?]) (any/c)]
  @defproc[(source-buffer [source exact-integer?]) exact-integer]
)]{
Binds a source to a given buffer. This does not begin to play the
source yet -- use @racket[play-source]
}
@deftogether[(
  @defproc[(source-source-state [source exact-integer?]) (one-of/c
  AL_STOPPED AL_PLAYING AL_PAUSED)]
)]{
Returns the state of a source. Use @racket[play-source],
@racket[stop-source], @racket[pause-source] to change.
}
@deftogether[(
  @defproc[(source-buffers-queued [source exact-integer?]) exact-integer?]
)]{
For streaming sources, returns how many buffers are in the source's
buffer queue. Includes both processed and yet-to-be-processed buffers.
}
@deftogether[(
  @defproc[(source-buffers-processed [source exact-integer?]) exact-integer?]
)]{
For streaming sources, returns how many buffers in the source's
buffer queue were processed.
}
@deftogether[(
  @defproc[(source-sec-offset [source exact-integer?]) real?]
)]{
Returns how many seconds the source is in its buffer.
}
@deftogether[(
  @defproc[(source-sample-offset [source exact-integer?]) exact-integer?]
)]{
Returns how many samples the source is in its buffer.
}
@deftogether[(
  @defproc[(source-byte-offset [source exact-integer?]) exact-integer?]
)]{
Returns how many bytes the source is in its buffer.
}

@section{Listener properties}
@subsection{C-like getters and setters}
@subsection{Friendly getters and setters}

@section{Streaming sound}

@section[#:tag "distance-models"]{Distance models}
OpenAL defines several models that select how sound is attenuated
(softened) over distance.

According to the @openal-programmers-guide, the inverse distance model
works with this formula:
@verbatim{
gain = AL_REFERENCE_DISTANCE / (AL_REFERENCE_DISTANCE +
          AL_ROLLOFF_FACTOR *
          (distance – AL_REFERENCE_DISTANCE));
}
Similarly, the Inverse Clamped Distance model works with this formula:
@verbatim{
distance = max(distance,AL_REFERENCE_DISTANCE);
distance = min(distance,AL_MAX_DISTANCE);
gain = AL_REFERENCE_DISTANCE / (AL_REFERENCE_DISTANCE +
       AL_ROLLOFF_FACTOR *
       (distance – AL_REFERENCE_DISTANCE));
}

By contrast, the Linear Distance Model works according to the
following formula:
@verbatim{
distance = min(distance, AL_MAX_DISTANCE) // avoid negative gain
gain = (1 – AL_ROLLOFF_FACTOR * (distance –
       AL_REFERENCE_DISTANCE) /
       (AL_MAX_DISTANCE – AL_REFERENCE_DISTANCE))
}

And the Clamped Linear Distance Model works according to this formula:
@verbatim{
distance = max(distance, AL_REFERENCE_DISTANCE)
distance = min(distance, AL_MAX_DISTANCE)
gain = (1 – AL_ROLLOFF_FACTOR * (distance –
        AL_REFERENCE_DISTANCE) /
        (AL_MAX_DISTANCE – AL_REFERENCE_DISTANCE))
}

The Exponential Distance Model works like this:
@verbatim{
gain = (distance / AL_REFERENCE_DISTANCE) ^
                   (- AL_ROLLOFF_FACTOR)
}
The Clamped Exponential Distance model works like this:
@verbatim{
distance = max(distance, AL_REFERENCE_DISTANCE)
distance = min(distance, AL_MAX_DISTANCE)
gain = (distance / AL_REFERENCE_DISTANCE) ^
                   (- AL_ROLLOFF_FACTOR)
}

Without any distance model, the gain remains fixed at 1.