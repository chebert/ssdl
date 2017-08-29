;;;; ssdl.lisp
;;;;
;;;; Copyright (c) 2017 Christopher Hebert <hebert.christopherj@gmail.com>

(in-package #:ssdl)

(cffi:define-foreign-library :ssdl
  (t #.(asdf:system-relative-pathname :ssdl "libssdl.so")))

(cffi:use-foreign-library :ssdl)

(cffi:defcfun ("quit" quit) :void
  "Quit SDL. Close the audio-stream.")

(cffi:defcfun ("init" init) :bool
  "Initialize SDL and create a window with the given title and dimensions."
  (title :string)
  (width :int)
  (height :int))

(defmacro with-init (title width height &body body)
  "Calls init and performs BODY in a protected form.
Calls QUIT to cleanup."
  `(unwind-protect
	(progn (init ,title ,width ,height)
	       ,@body)
     (quit)))

(cffi:defcfun ("display" display) :void
  "Display the changes drawn to the screen.
Call after calling draw functions to see changes.")
(cffi:defcfun ("clear" clear) :void
  "Clear the screen to the draw-color.")
(cffi:defcfun ("draw_rect" draw-rect) :void
  "Draw a rect at (X,Y) with width W and height H.
Draw an outline if FILLED? is false, otherwise draw solid."
  (x :int)
  (y :int)
  (w :int)
  (h :int)
  (filled? :bool))

(cffi:defcfun ("load_bmp" load-bmp) :pointer
  "Load a 24-bit RGB Bitmap with a color mask of magenta (255,0,255).
This means that all magenta pixels will not be drawn."
  (path :string))

(cffi:defcfun ("texture_color_mod" texture-color-mod) :void
  "Modulate the colors of the texture. Modulate the transparency of the
texture."
  (texture :pointer)
  (red :uint8)
  (green :uint8)
  (blue :uint8)
  (alpha :uint8))

(cffi:defcfun ("flip_none" flip-none) :uint32
  "Flag that specifies texture should not be flipped when used in
DRAW-TEXTURE call.")
(cffi:defcfun ("flip_horizontal" flip-horizontal) :uint32
  "Flag that specifies texture should be flipped horizontally when used in
DRAW-TEXTURE call.")
(cffi:defcfun ("flip_vertical" flip-vertical) :uint32
  "Flag that specifies texture should be flipped vertically when used in
DRAW-TEXTURE call.")
(cffi:defcfun ("flip_horizontal_and_vertical" flip-horizontal-and-vertical)
    :uint32
  "Flag that specifies texture should be flipped vertically when used in
DRAW-TEXTURE call.")

(cffi:defcfun ("draw_texture" draw-texture) :void
  "Draw the source rectangle (SX,SY,SW,SH) from TEXTURE to the destination
rectangle (DX,DY,DW,DH) stretching or squashing to fit.
Flip the drawing according to flip-flags"
  (texture :pointer)
  (sx :int) (sy :int)
  (sw :int) (sh :int)
  (dx :int) (dy :int)
  (dw :int) (dh :int)
  (flip-flags :uint32))

(cffi:defcfun ("free_texture" free-texture) :void
  "Free the memory held by texture."
  (texture :pointer))

(cffi:defcfun ("draw_color" draw-color) :void
  "Set the draw color used by CLEAR and DRAW-RECT.
Colors are in the range 0-255.
0 is no color, and 255 is full color."
  (r :uint8)
  (g :uint8)
  (b :uint8)
  (a :uint8))

(cffi:defcfun ("poll_event" poll-event) :bool
  "Poll for the next event. True if there were more events,
false if all events in the queue have been exhausted.
The last polled event is held in a variable checked by functions
like KEY-DOWN?, MOUSE-MOTION? and QUIT?, etc.
The event remains the same until POLL-EVENT is called again.")
(cffi:defcfun ("is_key_down" key-down?) :bool
  "True if key press event.")
(cffi:defcfun ("is_key_up" key-up?) :bool
  "True if key released event.")
(cffi:defcfun ("is_repeat" repeat?) :bool
  "True if key press is repeated.")
(cffi:defcfun ("scancode" scancode) :uint32
  "Return the scancode of the pressed or released key.")

(cffi:defcfun ("is_mouse_motion" mouse-motion?) :bool
  "True if mouse moved event.")
(cffi:defcfun ("is_mouse_button_down" mouse-button-down?) :bool
  "True if mouse button pressed event.")
(cffi:defcfun ("is_mouse_button_up" mouse-button-up?) :bool
  "True if mouse button released event.")
(cffi:defcfun ("is_lmb" lmb?) :bool
  "True if the mouse button pressed was the left mouse button.")
(cffi:defcfun ("is_mmb" mmb?) :bool
  "True if the mouse button pressed was the middle mouse button.")
(cffi:defcfun ("is_rmb" rmb?) :bool
  "True if the mouse button pressed was the right mouse button.")
(cffi:defcfun ("mouse_x" mouse-x) :int
  "Return the x position of the mouse for mouse move or mouse button events.")
(cffi:defcfun ("mouse_y" mouse-y) :int
  "Return the y position of the mouse for mouse move or mouse button events.")

(cffi:defcfun ("is_joy_added" joy-added?) :bool
  "True if event indicates a joystick was connected.")
(cffi:defcfun ("is_joy_removed" joy-removed?) :bool
  "True if event indicates a joystick was removed")
(cffi:defcfun ("is_joy_down" joy-down?) :bool
  "True if event is joy button press")
(cffi:defcfun ("is_joy_up" joy-up?) :bool
  "True if event is joy button release")
(cffi:defcfun ("is_joy_axis" joy-axis?) :bool
  "True if event is joy axis motion")
(cffi:defcfun ("joy_id" joy-id) :int
  "Return index of joystick associated with joystick event")
(cffi:defcfun ("joy_button" joy-button) :int
  "Return index of button associated with joystick event")
(cffi:defcfun ("joy_axis" joy-axis) :int
  "Return index of axis associated with joystick event")
(cffi:defcfun ("joy_axis_value" joy-axis-value) :int
  "Return value of axis for a joystick axis event")

(cffi:defcfun ("is_quit" quit?) :bool
  "True if quit event.")

(cffi:defcfun ("open_joystick" open-joystick) :pointer
  "Open a joystick. JOY-ID is the same one used in a
joy-added? event. A joystick must be opened in order to listen for
button press and axis motion events."
  (joy-id :int))
(cffi:defcfun ("close_joystick" close-joystick) :void
  "Close the joystick. Call when JOY-REMOVED? event occurs."
  (joystick :pointer))
(cffi:defcfun ("joystick_id" joystick-id) :int
  "Return the joystick's unique id. This id is used for all joystick events
EXCEPT for JOY-ADDED?"
  (joystick :pointer))

(cffi:defcfun ("SDL_LockAudio" audio-lock) :void)
(cffi:defcfun ("SDL_UnlockAudio" audio-unlock) :void)

(defmacro with-audio-lock (&body body)
  "Must surround calls that pertain to the audio buffer during audio playback,
e.g. WRITE-AUDIO, AUDIO-AVAILABLE, and CLEAR-AUDIO.
Locks the audio device from modifying the audio buffer on the audio
thread."
  `(progn
     (audio-lock)
     ,@body
     (audio-unlock)))

(cffi:defcfun ("audio_available" audio-available) :int
  "MUST BE CALLED in WITH-AUDIO-LOCK if sound is playing.
Return the amount of audio bytes that can be written to the audio buffer.")
(cffi:defcfun ("write_audio" write-audio%) :void
  (source (:pointer :uint8))
  (n :int))

(defun write-audio (bytes)
  "MUST BE CALLED in WITH-AUDIO-LOCK if sound is playing.
Write the (simple-array (unsigned-byte 8)) bytes to the audio buffer.
Audio will play silence if no audio is present in the buffer."
  (declare (type (simple-array (unsigned-byte 8)) bytes))
  (let ((len (length bytes)))
    (cffi:with-foreign-object (carr :uint8 len)
      (loop for i below len
	 do (setf (cffi:mem-aref carr :uint8 i) (aref bytes i)))
      (write-audio% carr len))))

(cffi:defcfun ("clear_audio" clear-audio) :void
  "MUST BE CALLED in WITH-AUDIO-LOCK if sound is playing.
Clear the audio buffer.")
(cffi:defcfun ("stop_audio" stop-audio) :void
  "Pause audio playback.")
(cffi:defcfun ("play_audio" play-audio) :void
  "Play/Resume audio playback.")

;; Audio Formats
(cffi:defcfun ("format_byte_size" format-byte-size) :int
  "Returns the size in bytes of a (1-channel) sample of format AUDIO-FORMAT."
  (audio-format :uint16))
(cffi:defcfun ("audio_u8" audio-u8) :uint16
  "Audio format used in OPEN-AUDIO. 8-bit unsigned int.
Range: 0 to 255, centerpoint: 128")
(cffi:defcfun ("audio_s8" audio-s8) :uint16
  "Audio format used in OPEN-AUDIO. 8-bit signed int.
Range: -128 to 127")
(cffi:defcfun ("audio_u16" audio-u16) :uint16
  "Audio format used in OPEN-AUDIO. 16-bit unsigned int.
Range: 0 to 65535, centerpoint: 32768
Little-endian.")
(cffi:defcfun ("audio_s16" audio-s16) :uint16
  "Audio format used in OPEN-AUDIO. 16-bit signed int.
Range: -32768 to 32767
Little-endian.")
(cffi:defcfun ("audio_s32" audio-s32) :uint16
  "Audio format used in OPEN-AUDIO. 32-bit unsigned int.
Range: -#x8000000 to #x7FFFFFFF
Little-endian.")
(cffi:defcfun ("audio_f32" audio-f32) :uint16
  "Audio format used in OPEN-AUDIO. 32-bit floating point.
Little-endian.")

(cffi:defcfun ("open_audio" open-audio) :bool
  "Open an audio device.
AUDIO-FORMAT is one of AUDIO-U8, AUDIO-S8, etc.

SAMPLES-PER-SECOND e.g. 44100 for high quality.

NUM-CHANNELS is 1 for mono, 2 for stereo. Samples for channels are interleaved.

AUDIO-DEVICE-BUFFER-SIZE-IN-SAMPLES specifies the size in samples of the 
audio buffer used directly by the device. 2048, 4096, etc. For games you might
calculate this based on the number of video frames of sound. E.g. write audio
for 4 frames of video at 60Hz would be the closest power of 2 to
 4*SAMPLES-PER-SECOND/FPS.

BUFFER-SIZE-IN-BYTES specifies the size in bytes of the buffer used by 
WRITE-AUDIO. Typically this is the same as the 
AUDIO-DEVICE-BUFFER-SIZE-IN-SAMPLES, but in bytes. See SAMPLE-SIZE-IN-BYTES."
  (audio-format :uint16)
  (samples-per-second :int)
  (num-channels :int)
  (audio-device-buffer-size-in-samples :int)
  (buffer-size-in-bytes :int))

(defun sample-size-in-bytes (audio-format num-channels)
  "Return the size in bytes of a sample
AUDIO-FORMAT is one of AUDIO-U8, AUDIO-S8, etc.
NUM-CHANNELS is 1 for mono, 2 for stereo."
  (* (format-byte-size audio-format)
     num-channels))

(cffi:defcfun ("ticks" ticks) :int
  "Number of milliseconds elapsed since INIT")
(cffi:defcfun ("delay" delay) :void
  "Delay (and yield to other threads) execution for MS amount of time.
Use in infinite loops to ensure other threads have an opportunity to run.
Delays for at least the specified time.
NOTE: Count on a delay granularity of *at least* 10 ms."
  (ms :int))

(cffi:defcfun ("scancode_name" scancode-name) :string
  "Return the name of the SCANCODE which can be used in SCANCODE-FROM-NAME."
  (scancode :uint32))
(cffi:defcfun ("scancode_from_name" scancode-from-name) :uint32
  "Return the scancode given the NAME. See SCANCODE-NAME.
Consider obtaining scancodes needed all at once and storing in variables."
  (name :string))

(cffi:defcfun ("make_texture_from_pixels" make-texture-from-pixels%) :pointer
  (width :int)
  (height :int)
  (pixels :pointer))
(defun make-texture-from-pixels (width height pixel-bytes)
  "Make a new texture from the given byte array of PIXELS.
A pixel is a 32-bit RGBA value (in that order)."
  (cffi:with-foreign-object (arr :uint8 (length pixel-bytes))
    (loop for i below (length pixel-bytes)
       do (setf (cffi:mem-aref arr :uint8 i) (aref pixel-bytes i)))
    (make-texture-from-pixels% width height arr)))

(cffi:defcfun ("make_texture" make-texture) :pointer
  "Make a new texture that can be used as a render target."
  (width :int)
  (height :int))

(cffi:defcfun ("render_to_texture" render-to-texture) :void
  "Set the render target to be the given texture instead of the screen."
  (texture :pointer))
(cffi:defcfun ("render_to_window" render-to-window) :void
  "Set the render target back to being the screen, after RENDER-TO-TEXTURE
has been called.")
