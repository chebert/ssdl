;;;; ssdl.lisp
;;;;
;;;; Copyright (c) 2017 Christopher Hebert <hebert.christopherj@gmail.com>

(in-package #:ssdl)

;;; "ssdl" goes here. Hacks and glory await!

(cffi:define-foreign-library :ssdl
  (t #.(asdf:system-relative-pathname :ssdl "libssdl.so")))

(cffi:use-foreign-library :ssdl)

(cffi:defcfun ("quit" quit) :void
  "Quit SDL.")

(cffi:defcfun ("init" init) :bool
  "Initialize SDL and create a window with the given title and dimensions."
  (title :string)
  (width :int)
  (height :int))

(cffi:defcfun ("flip" display) :void
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

(cffi:defcfun ("flip_none" flip-none) :uint32
  "Flag that specifies texture should not be flipped when used in
DRAW-TEXTURE call.")
(cffi:defcfun ("flip_horizontal" flip-horizontal) :uint32
  "Flag that specifies texture should be flipped horizontally when used in
DRAW-TEXTURE call. Can be OR'd with FLIP-VERTICAL")
(cffi:defcfun ("flip_vertical" flip-vertical) :uint32
  "Flag that specifies texture should be flipped vertically when used in
DRAW-TEXTURE call. Can be OR'd with FLIP-HORIZONTAL")

(cffi:defcfun ("draw_texture" draw-texture) :void
  "Draw the source rectangle (SX,SY,SW,SH) from TEXTURE to the destination
rectangle (DX,DY,DW,DH) stretching or squashing to fit. Rotate the texture
ANGLE radians about the pivot offset (PX,PY). 
Flip the drawing according to flip-flags"
  (texture :pointer)
  (sx :int) (sy :int)
  (sw :int) (sh :int)
  (dx :int) (dy :int)
  (dw :int) (dh :int)
  (angle :double)
  (px :int) (py :int)
  (flip-flags :uint32))

(cffi:defcfun ("free_texture" free-texture) :void
  "Free the memory held by texture."
  (texture :pointer))

(cffi:defcfun ("draw_color" draw-color) :void
  "Set the draw color used by CLEAR and DRAW-RECT."
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

(cffi:defcfun ("is_quit" quit?) :bool
  "True if quit event.")

(cffi:defcfun ("audio_available" audio-available) :int
  "Return the amount of audio bytes that can be written to the audio buffer.")
(cffi:defcfun ("write_audio" write-audio%) :void
  (source (:pointer :uint8))
  (n :int))

(defun write-audio (bytes)
  "Write the vector of unsigned bytes to the audio buffer."
  (declare (type (simple-array (unsigned-byte 8)) bytes))
  (let ((len (length bytes)))
    (cffi:with-foreign-object (carr :uint8 len)
      (loop for i below len
	 do (setf (cffi:mem-aref carr :uint8 i) (aref bytes i)))
      (write-audio% carr len))))

(cffi:defcfun ("clear_audio" clear-audio) :void
  "Clear the audio buffer.")
(cffi:defcfun ("stop_audio" stop-audio) :void
  "Pause audio playback.")
(cffi:defcfun ("play_audio" play-audio) :void
  "Play/Resume audio playback.")

;; Audio Formats
(defparameter *audio-u8* #x0008
  "Audio format used in OPEN-AUDIO")
(defparameter *audio-s8* #x8008
  "Audio format used in OPEN-AUDIO")
(defparameter *audio-u16* #x0010
  "Audio format used in OPEN-AUDIO")
(defparameter *audio-s16* #x8010
  "Audio format used in OPEN-AUDIO")
(defparameter *audio-s32* #x8020
  "Audio format used in OPEN-AUDIO")
(defparameter *audio-f32* #x8120
  "Audio format used in OPEN-AUDIO")

(cffi:defcfun ("open_audio" open-audio) :bool
  "Open an audio device.
AUDIO-FORMAT is one of *AUDIO-U8*, *AUDIO-S8*, etc.
SAMPLES-PER-SECOND e.g. 44100 for high quality.
NUM-CHANNELS is 1 for mono, 2 for stereo.
AUDIO-BUFFER-SIZE-IN-SAMPLES specifies the size in samples of the audio buffer used
directly by the device. 2048, 4096, etc.
BUFFER-SIZE-IN-BYTES specifies the size in bytes of the buffer used by WRITE-AUDIO."
  (audio-format :uint16)
  (samples-per-second :int)
  (num-channels :int)
  (audio-buffer-size-in-samples :int)
  (buffer-size-in-bytes :int))

(defun sample-size-in-bytes (audio-format num-channels)
  "Return the size in bytes of a sample."
  (* (ecase audio-format
       ((#.*audio-u8* #.*audio-s8*) 1)
       ((#.*audio-u16* #.*audio-s16*) 2)
       ((#.*audio-s32* #.*audio-f32*) 4))
     num-channels))

(cffi:defcfun ("close_audio" close-audio) :void
  "Close the audio device.")

(cffi:defcfun ("ticks" ticks) :int
  "Number of milliseconds since INIT")
(cffi:defcfun ("delay" delay) :void
  "Delay (and yield) execution for MS amount of time."
  (ms :int))

(cffi:defcfun ("scancode_name" scancode-name) :string
  "Return the name of the SCANCODE which can be used in SCANCODE-FROM-NAME."
  (scancode :uint32))
(cffi:defcfun ("scancode_from_name" scancode-from-name) :uint32
  "Return the scancode given the NAME. See SCANCODE-NAME."
  (name :string))

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

(defparameter *samples/second* 44100)
(defparameter *num-channels* 2)
(defparameter *audio-buffer-size-in-samples* 2048)
(defparameter *audio-format* *audio-s16*)
(let ((sn 0))
  (defun sine-audio (num-samples)
    (let* ((num-bytes (* num-samples
			 (sample-size-in-bytes *audio-format* *num-channels*)
			 *num-channels* 2))
	   (v (make-array num-bytes :element-type '(unsigned-byte 8)))
	   (n 0))
      (loop while (< n num-bytes)
	 do
	   (let* ((time (/ sn *samples/second*))
		  (smp1 (logand #xFFFF
				(floor (* 28000 (sin (* 2 pi 400 time))))))
		  (smp2 (logand #xFFFF
				(floor (* 28000 (sin (* 2 pi 720 time)))))))
	     (setf (aref v n) (logand #xFF smp1))
	     (setf (aref v (1+ n)) (logand #xFF (ash smp1 -8)))
	     (setf (aref v (+ 2 n)) (logand #xFF smp2))
	     (setf (aref v (+ 3 n)) (logand #xFF (ash smp2 -8)))
	     (incf sn)
	     (incf n 4)))
      v)))

(defun test ()
  (init "hello" 640 480)
  (clear)
  (flip)

  (open-audio *audio-format*
	      *samples/second*
	      *num-channels*
	      *audio-buffer-size-in-samples*
	      (* 2048 2 2))

  (play-audio)
  (let ((start-ticks (ticks)))
    (loop while (< (- (ticks) start-ticks) 1000)
       do
	 (write-audio (sine-audio (floor (audio-available)
					 (* 2 *num-channels*))))
	 (delay 1)))
  (stop-audio)

  (clear)
  (let ((tex (load-bmp "/home/chebert/Projects/cave-story-content/MyChar.bmp")))
    (draw-texture tex 32 32 32 32 10 100 32 32 0.0d0 0 0 (flip-none))
    (draw-color 255 0 0 255)
    (draw-rect 100 10 200 300 t))
  (flip)

  (let ((quit? nil))
    (loop until quit? do
	 (loop while (poll-event)
	    do
	      (when (key-down?)
		(format t "~&Key ~A pressed." (scancode-name (scancode))))
	      (when (or (quit?)
			(and (key-down?)
			     (= (scancode) (scancode-from-name "escape"))))
		(setq quit? t))

	      (when (mouse-motion?)
		(format t "~&Mouse moved to (~a,~a)" (mouse-x) (mouse-y)))
	      (when (and (mouse-button-down?)
			 (lmb?))
		(format t "~&Mouse clicked at (~a,~a)." (mouse-x) (mouse-y)))

	      (when (joy-added?)
		(format t "~&joystick ~D plugged in" (joy-id))))
	 (delay 1)))
  
  (quit))

;;(test)
