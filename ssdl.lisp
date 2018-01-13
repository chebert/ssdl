;;;; ssdl.lisp
;;;;
;;;; Copyright (c) 2017 Christopher Hebert <hebert.christopherj@gmail.com>

(in-package #:ssdl)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (let ((pathname (merge-pathnames #+win32 "ssdl.dll"
				   #+linux "libssdl.so"	
				   #+load-system *default-pathname-defaults*
				   #-load-system (asdf:system-source-directory :ssdl))))
    (unless (probe-file pathname)
      (error "Could not find path ~A" pathname))
    (load-shared-object pathname)))

(defun null? (pointer) (sb-alien::sap= (sb-alien::int-sap 0) pointer))

(define-alien-routine ("init" init) boolean
  "Initialize SDL and create a window with the given title and dimensions."
  (title c-string)
  (width int)
  (height int))

(define-alien-routine ("quit" quit) void
  "Quit SDL. Close the audio-stream.")

(define-alien-routine ("window_size" window-size) void
  "Resize the window."
  (width int)
  (height int))

(defmacro with-init (title width height &body body)
  "Calls init and performs BODY in a protected form.
Calls QUIT to cleanup."
  `(unwind-protect
	(progn (init ,title ,width ,height)
	       ,@body)
     (quit)))

(define-alien-routine ("error_string" error-string) c-string
  "Return the error string based on the last set error code.")

;;; Drawing
(define-alien-routine ("draw_color" draw-color) void
  "Set the draw color used by CLEAR and DRAW-RECT.
Colors are in the range 0-255.
0 is no color, and 255 is full color."
  (r (unsigned 8))
  (g (unsigned 8))
  (b (unsigned 8))
  (a (unsigned 8)))

(define-alien-routine ("display" display) void
  "Display the changes drawn to the screen.
Call after calling draw functions to see changes.")

(define-alien-routine ("draw_rect" draw-rect) void
  "Draw a rect at (X,Y) with width W and height H.
Draw an outline if FILLED? is false, otherwise draw solid."
  (x integer)
  (y integer)
  (w integer)
  (h integer)
  (filled? boolean))

;;; Textures
(define-alien-routine ("load_bmp" load-bmp) system-area-pointer
  "Load a 24-bit RGB Bitmap with a color mask of magenta (255,0,255).
This means that all magenta pixels will not be drawn."
  (path c-string))
(define-alien-routine ("load_image" load-image) system-area-pointer
  "Load a PNG, JPG, or BMP into a texture."
  (path c-string))

(define-alien-routine ("texture_color_mod" texture-color-mod) void
  "Modulate the colors of the texture. Modulate the transparency of the
texture."
  (texture system-area-pointer)
  (red (unsigned 8))
  (green (unsigned 8))
  (blue (unsigned 8))
  (alpha (unsigned 8)))

(define-alien-routine ("draw_texture" draw-texture) void
  "Draw the source rectangle (SX,SY,SW,SH) from TEXTURE to the destination
rectangle (DX,DY,DW,DH) stretching or squashing to fit.
Flip the drawing according to the given flip-flags"
  (texture system-area-pointer)
  (sx integer) (sy integer)
  (sw integer) (sh integer)
  (dx integer) (dy integer)
  (dw integer) (dh integer)
  (flip-horizontal boolean)
  (flip-vertical boolean))

(define-alien-routine ("free_texture" free-texture) void
  "Free the memory held by texture."
  (texture system-area-pointer))

(define-alien-routine ("clear" clear) void
  "Clear the screen to the draw-color.")

(define-alien-routine ("texture_width" texture-width) integer
  "Return the width (in pixels) of the texture."
  (texture system-area-pointer))
(define-alien-routine ("texture_height" texture-height) integer
  "Return the height (in pixels) of the texture."
  (texture system-area-pointer))

;;; TTF Fonts/Text
(define-alien-routine ("open_font" open-font) system-area-pointer
  "Open a TTF given the path and point size.
Return a pointer to the font object."
  (path c-string)
  (point-size integer))
(define-alien-routine ("close_font" close-font) void
  "Close the opened TTF."
  (font system-area-pointer))
(define-alien-routine ("text_texture" text-texture) system-area-pointer
  "Creates a new texture and draws the UTF8 text,
shaded using the foreground color, onto a box of the given background color."
  (font system-area-pointer)
  (text c-string)
  (fg-r unsigned-char)
  (fg-g unsigned-char)
  (fg-b unsigned-char)
  (fg-a unsigned-char)
  (bg-r unsigned-char)
  (bg-g unsigned-char)
  (bg-b unsigned-char)
  (bg-a unsigned-char))

;;; Events
(define-alien-routine ("poll_event" poll-event) boolean
  "Poll for the next event. True if there were more events,
false if all events in the queue have been exhausted.
The last polled event is held in a variable checked by functions
like KEY-DOWN?, MOUSE-MOTION? and QUIT?, etc.
The event remains the same until POLL-EVENT is called again.")

;;; Keybord Event Accessors
(define-alien-routine ("is_key_down" key-down?) boolean
  "True if key press event.")
(define-alien-routine ("is_key_up" key-up?) boolean
  "True if key released event.")
(define-alien-routine ("is_repeat" repeat?) boolean
  "True if key press is repeated.")
(define-alien-routine ("scancode" scancode) (unsigned 32)
  "Return the scancode of the pressed or released key.")

;;; Mouse Event Accessors
(define-alien-routine ("is_mouse_motion" mouse-motion?) boolean
  "True if mouse moved event.")
(define-alien-routine ("is_mouse_button_down" mouse-button-down?) boolean
  "True if mouse button pressed event.")
(define-alien-routine ("is_mouse_button_up" mouse-button-up?) boolean
  "True if mouse button released event.")
(define-alien-routine ("is_lmb" lmb?) boolean
  "True if the mouse button pressed was the left mouse button.")
(define-alien-routine ("is_mmb" mmb?) boolean
  "True if the mouse button pressed was the middle mouse button.")
(define-alien-routine ("is_rmb" rmb?) boolean
  "True if the mouse button pressed was the right mouse button.")
(define-alien-routine ("mouse_x" mouse-x) integer
  "Return the x position of the mouse for mouse move or mouse button events.")
(define-alien-routine ("mouse_y" mouse-y) integer
  "Return the y position of the mouse for mouse move or mouse button events.")

;;; Joystick Event Accessors
(define-alien-routine ("is_joy_added" joy-added?) boolean
  "True if event indicates a joystick was connected.")
(define-alien-routine ("is_joy_removed" joy-removed?) boolean
  "True if event indicates a joystick was removed")
(define-alien-routine ("is_joy_down" joy-down?) boolean
  "True if event is joy button press")
(define-alien-routine ("is_joy_up" joy-up?) boolean
  "True if event is joy button release")
(define-alien-routine ("is_joy_axis" joy-axis?) boolean
  "True if event is joy axis motion")
(define-alien-routine ("joy_id" joy-id) integer
  "Return index of joystick associated with joystick event")
(define-alien-routine ("joy_button" joy-button) integer
  "Return index of button associated with joystick event")
(define-alien-routine ("joy_axis" joy-axis) integer
  "Return index of axis associated with joystick event")
(define-alien-routine ("joy_axis_value" joy-axis-value) integer
  "Return value of axis for a joystick axis event")

(define-alien-routine ("is_quit" quit?) boolean
  "True if quit event.")

;;; Text Input Accessors
(define-alien-routine ("is_text_input" text-input?) boolean
  "True if text input event.")
(define-alien-routine ("text_input" text-input) c-string
  "Returns the text input of a text input event.")
(define-alien-routine ("enable_text_input" enable-text-input) void
  "Enables text input events.")
(define-alien-routine ("disable_text_input" disable-text-input) void
  "Disables text input events.")

;;; Joysticks
(define-alien-routine ("open_joystick" open-joystick) system-area-pointer
  "Open a joystick. JOY-ID is the same one used in a
joy-added? event. A joystick must be opened in order to listen for
button press and axis motion events."
  (joy-id integer))
(define-alien-routine ("close_joystick" close-joystick) void
  "Close the joystick. Call when JOY-REMOVED? event occurs."
  (joystick system-area-pointer))
(define-alien-routine ("joystick_id" joystick-id) integer
  "Return the joystick's unique id. This id is used for all joystick events
EXCEPT for JOY-ADDED?"
  (joystick system-area-pointer))

;;; Audio
(define-alien-routine ("open_audio" open-audio) boolean
  "Open an audio device.
The audio format 16-bit signed little-endian integers.

SAMPLES-PER-SECOND e.g. 44100 for high quality.

NUM-CHANNELS is 1 for mono, 2 for stereo. Samples for channels are interleaved.

AUDIO-DEVICE-BUFFER-SIZE-IN-SAMPLES specifies the size in samples of the 
audio buffer used directly by the device. 2048, 4096, etc. For games you might
calculate this based on the number of video frames of sound. E.g. write audio
for 4 frames of video at 60Hz would be the closest power of 2 to
 4*SAMPLES-PER-SECOND/FPS."
  (samples-per-second integer)
  (num-channels integer)
  (audio-device-buffer-size-in-samples integer))


(define-alien-routine ("SDL_LockAudio" audio-lock) void)
(define-alien-routine ("SDL_UnlockAudio" audio-unlock) void)
(defmacro with-audio-lock (&body body)
  "Must surround calls that pertain to the audio buffer during audio playback,
e.g. WRITE-AUDIO, AUDIO-AVAILABLE, and CLEAR-AUDIO.
Locks the audio device from modifying the audio buffer on the audio
thread."
  (let ((res (gensym)))
    `(let ((,res ()))
       (audio-lock)
       (setq ,res (progn ,@body))
       (audio-unlock)
       ,res)))

(define-alien-routine ("audio_available" audio-available) integer
  "MUST BE CALLED in WITH-AUDIO-LOCK if sound is playing.
Return the amount of audio bytes that can be written to the audio buffer.")
(define-alien-routine ("write_audio" write-audio%) void
  (source system-area-pointer)
  (n integer))

(defun write-audio (bytes)
  "MUST BE CALLED in WITH-AUDIO-LOCK if sound is playing.
Write the (simple-array (unsigned-byte 8)) bytes to the audio buffer.
Audio will play silence if no audio is present in the buffer."
  (declare (type (simple-array (unsigned-byte 8)) bytes))
  (sb-sys:with-pinned-objects (bytes)
    (write-audio% (sb-sys:vector-sap bytes) (length bytes))))

(define-alien-routine ("play_audio" play-audio) void
  "Play/Resume audio playback.")
(define-alien-routine ("stop_audio" stop-audio) void
  "Pause audio playback.")
(define-alien-routine ("clear_audio" clear-audio) void
  "MUST BE CALLED in WITH-AUDIO-LOCK if sound is playing.
Clear the audio buffer.")

;;; General
(define-alien-routine ("ticks" ticks) integer
  "Number of milliseconds elapsed since INIT")
(define-alien-routine ("delay" delay) void
  "Delay (and yield to other threads) execution for MS amount of time.
Use in infinite loops to ensure other threads have an opportunity to run.
Delays for at least the specified time.
NOTE: Count on a delay granularity of *at least* 10 ms."
  (ms integer))

;;; Scancodes
(define-alien-routine ("scancode_name" scancode-name) c-string
  "Return the name of the SCANCODE which can be used in SCANCODE-FROM-NAME."
  (scancode (unsigned 32)))
(define-alien-routine ("scancode_from_name" scancode-from-name) (unsigned 32)
  "Return the scancode given the NAME. See SCANCODE-NAME.
Consider obtaining scancodes needed all at once and storing in variables."
  (name c-string))

;;; Render Alternatives
(define-alien-routine ("make_texture_from_pixels" make-texture-from-pixels%) system-area-pointer
  (width integer)
  (height integer)
  (pixels system-area-pointer))
(defun make-texture-from-pixels (width height pixel-bytes)
  "Make a new texture from the given byte array of PIXELS.
A pixel is a 32-bit RGBA value (in that order). E.g.
#(r1 g1 b1 a1 r2 g2 b2 a2 ...)"
  (declare (type (simple-array (unsigned-byte 8)) pixel-bytes))
  (sb-sys:with-pinned-objects (pixel-bytes)
    (make-texture-from-pixels% width height (sb-sys:vector-sap pixel-bytes))))

(define-alien-routine ("make_texture" make-texture) system-area-pointer
  "Make a new texture that can be used as a render target."
  (width integer)
  (height integer))

(define-alien-routine ("render_to_texture" render-to-texture) void
  "Set the render target to be the given texture instead of the screen.
Texture should be created using MAKE-TEXTURE."
  (texture system-area-pointer))
(define-alien-routine ("render_to_window" render-to-window) void
  "Set the render target back to being the screen, after RENDER-TO-TEXTURE
has been called.")

(define-alien-routine ("render_pixels_to_window" render-pixels-to-window%) void
  (pixels system-area-pointer))

(defun render-pixels-to-window (pixel-bytes)
  "Copy the pixel uint8 array, with dimensions window-width X window-height,
to the window. A pixel is a 32-bit RGBA value (in that order). E.g.
#(r1 g1 b1 a1 r2 g2 b2 a2 ...).
Use this instead of MAKE-TEXTURE-FROM-PIXELS for software rendering."
  (declare (type (simple-array (unsigned-byte 8)) pixel-bytes))
  (sb-sys:with-pinned-objects (pixel-bytes)
    (render-pixels-to-window% (sb-sys:vector-sap pixel-bytes))))
