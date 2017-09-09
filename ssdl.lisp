;;;; ssdl.lisp
;;;;
;;;; Copyright (c) 2017 Christopher Hebert <hebert.christopherj@gmail.com>

(in-package #:ssdl)

(cffi:define-foreign-library :ssdl
  (:darwin)
  (:unix  #.(asdf:system-relative-pathname :ssdl "libssdl.so"))
  (t #.(asdf:system-relative-pathname :ssdl "ssdl.dll")))

(cffi:use-foreign-library :ssdl)

(cffi:defcfun ("quit" quit) :void
  "Quit SDL. Close the audio-stream.")

(cffi:defcfun ("init" init) :boolean
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
  (filled? :boolean))

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

(cffi:defcfun ("draw_texture" draw-texture) :void
  "Draw the source rectangle (SX,SY,SW,SH) from TEXTURE to the destination
rectangle (DX,DY,DW,DH) stretching or squashing to fit.
Flip the drawing according to the given flip-flags"
  (texture :pointer)
  (sx :int) (sy :int)
  (sw :int) (sh :int)
  (dx :int) (dy :int)
  (dw :int) (dh :int)
  (flip-horizontal :boolean)
  (flip-vertical :boolean))

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

(cffi:defcfun ("poll_event" poll-event) :boolean
  "Poll for the next event. True if there were more events,
false if all events in the queue have been exhausted.
The last polled event is held in a variable checked by functions
like KEY-DOWN?, MOUSE-MOTION? and QUIT?, etc.
The event remains the same until POLL-EVENT is called again.")

(cffi:defcfun ("is_key_down" key-down?) :boolean
  "True if key press event.")
(cffi:defcfun ("is_key_up" key-up?) :boolean
  "True if key released event.")
(cffi:defcfun ("is_repeat" repeat?) :boolean
  "True if key press is repeated.")
(cffi:defcfun ("scancode" scancode) :uint32
  "Return the scancode of the pressed or released key.")

(cffi:defcfun ("is_mouse_motion" mouse-motion?) :boolean
  "True if mouse moved event.")
(cffi:defcfun ("is_mouse_button_down" mouse-button-down?) :boolean
  "True if mouse button pressed event.")
(cffi:defcfun ("is_mouse_button_up" mouse-button-up?) :boolean
  "True if mouse button released event.")
(cffi:defcfun ("is_lmb" lmb?) :boolean
  "True if the mouse button pressed was the left mouse button.")
(cffi:defcfun ("is_mmb" mmb?) :boolean
  "True if the mouse button pressed was the middle mouse button.")
(cffi:defcfun ("is_rmb" rmb?) :boolean
  "True if the mouse button pressed was the right mouse button.")
(cffi:defcfun ("mouse_x" mouse-x) :int
  "Return the x position of the mouse for mouse move or mouse button events.")
(cffi:defcfun ("mouse_y" mouse-y) :int
  "Return the y position of the mouse for mouse move or mouse button events.")

(cffi:defcfun ("is_joy_added" joy-added?) :boolean
  "True if event indicates a joystick was connected.")
(cffi:defcfun ("is_joy_removed" joy-removed?) :boolean
  "True if event indicates a joystick was removed")
(cffi:defcfun ("is_joy_down" joy-down?) :boolean
  "True if event is joy button press")
(cffi:defcfun ("is_joy_up" joy-up?) :boolean
  "True if event is joy button release")
(cffi:defcfun ("is_joy_axis" joy-axis?) :boolean
  "True if event is joy axis motion")
(cffi:defcfun ("joy_id" joy-id) :int
  "Return index of joystick associated with joystick event")
(cffi:defcfun ("joy_button" joy-button) :int
  "Return index of button associated with joystick event")
(cffi:defcfun ("joy_axis" joy-axis) :int
  "Return index of axis associated with joystick event")
(cffi:defcfun ("joy_axis_value" joy-axis-value) :int
  "Return value of axis for a joystick axis event")

(cffi:defcfun ("is_quit" quit?) :boolean
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

(defmacro with-foreign-uint8-array ((name bytes) &body body)
  "Creates a new C byte array, the same length as byte-vector,
and copies all the data from byte-vector into it."
  (let ((length (gensym)))
    `(let ((,length (length ,bytes)))
       (cffi:with-foreign-object (,name :uint8 ,length)
	 (loop for i below ,length
	    do
	      (setf (cffi:mem-aref ,name :uint8 i) (aref ,bytes i)))
	 ,@body))))

(defun write-audio (bytes)
  "MUST BE CALLED in WITH-AUDIO-LOCK if sound is playing.
Write the (simple-array (unsigned-byte 8)) bytes to the audio buffer.
Audio will play silence if no audio is present in the buffer."
  (declare (type (simple-array (unsigned-byte 8)) bytes))
  #+sbcl
  (sb-sys:with-pinned-objects (bytes)
    (write-audio% (sb-sys:vector-sap bytes) (length bytes)))
  #-sbcl
  (with-foreign-uint8-array (carr bytes)
    (write-audio% carr (length bytes))))

(cffi:defcfun ("clear_audio" clear-audio) :void
  "MUST BE CALLED in WITH-AUDIO-LOCK if sound is playing.
Clear the audio buffer.")
(cffi:defcfun ("stop_audio" stop-audio) :void
  "Pause audio playback.")
(cffi:defcfun ("play_audio" play-audio) :void
  "Play/Resume audio playback.")

(cffi:defcfun ("open_audio" open-audio) :boolean
  "Open an audio device.
The audio format 16-bit signed little-endian integers.

SAMPLES-PER-SECOND e.g. 44100 for high quality.

NUM-CHANNELS is 1 for mono, 2 for stereo. Samples for channels are interleaved.

AUDIO-DEVICE-BUFFER-SIZE-IN-SAMPLES specifies the size in samples of the 
audio buffer used directly by the device. 2048, 4096, etc. For games you might
calculate this based on the number of video frames of sound. E.g. write audio
for 4 frames of video at 60Hz would be the closest power of 2 to
 4*SAMPLES-PER-SECOND/FPS."
  (samples-per-second :int)
  (num-channels :int)
  (audio-device-buffer-size-in-samples :int))

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
A pixel is a 32-bit RGBA value (in that order). E.g.
#(r1 g1 b1 a1 r2 g2 b2 a2 ...)"
  (declare (type (simple-array (unsigned-byte 8)) pixel-bytes))
  #+sbcl
  (sb-sys:with-pinned-objects (pixel-bytes)
    (make-texture-from-pixels% width height (sb-sys:vector-sap pixel-bytes)))
  #-sbcl
  (with-foreign-uint8-array (arr pixel-bytes)
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

(cffi:defcfun ("render_pixels_to_window" render-pixels-to-window%) :void
  (pixels :pointer))

(defun render-pixels-to-window (pixel-bytes)
  "Copy the pixel uint8 array, with dimensions window-width X window-height,
to the window. A pixel is a 32-bit RGBA value (in that order). E.g.
#(r1 g1 b1 a1 r2 g2 b2 a2 ...).
Use this instead of MAKE-TEXTURE-FROM-PIXELS for software rendering."
  (declare (type (simple-array (unsigned-byte 8)) pixel-bytes))
  #+sbcl
  (sb-sys:with-pinned-objects (pixel-bytes)
    (render-pixels-to-window% (sb-sys:vector-sap pixel-bytes)))
  #-sbcl
  (with-foreign-uint8-array (arr pixel-bytes)
    (make-texture-from-pixels% arr)))
