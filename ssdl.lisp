;;;; ssdl.lisp
;;;;
;;;; Copyright (c) 2017 Christopher Hebert <hebert.christopherj@gmail.com>

(in-package #:ssdl)

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
DRAW-TEXTURE call. Can be OR'd with FLIP-VERTICAL")
(cffi:defcfun ("flip_vertical" flip-vertical) :uint32
  "Flag that specifies texture should be flipped vertically when used in
DRAW-TEXTURE call. Can be OR'd with FLIP-HORIZONTAL")

(cffi:defcfun ("draw_texture" draw-texture) :void
  "Draw the source rectangle (SX,SY,SW,SH) from TEXTURE to the destination
rectangle (DX,DY,DW,DH) stretching or squashing to fit. Rotate the texture
ANGLE degrees about the pivot offset (PX,PY). 
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
  "Audio format used in OPEN-AUDIO.
Range: 0 to 255, centerpoint: 128")
(cffi:defcfun ("audio_s8" audio-s8) :uint16
  "Audio format used in OPEN-AUDIO
Range: -128 to 127")
(cffi:defcfun ("audio_u16" audio-u16) :uint16
  "Audio format used in OPEN-AUDIO.
Range: 0 to 65535, centerpoint: 32768
Little-endian.")
(cffi:defcfun ("audio_s16" audio-s16) :uint16
  "Audio format used in OPEN-AUDIO.
Range: -32768 to 32767
Little-endian.")
(cffi:defcfun ("audio_s32" audio-s32) :uint16
  "Audio format used in OPEN-AUDIO.
Range: -#x8000000 to #x7FFFFFFF
Little-endian.")
(cffi:defcfun ("audio_f32" audio-f32) :uint16
  "Audio format used in OPEN-AUDIO.
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

(defparameter *samples/second* 44100
  "The number of samples in one second of audio for the TEST-AUDIO
example.")
(defparameter *num-channels* 2
  "The number of channels to use (1 is mono, 2 is stereo) for the
TEST-AUDIO example.")
(defparameter *audio-device-buffer-size-in-samples* 2048
  "The size (in samples) of the buffer on the audio device. 
Should be a power of 2. Used in the TEST-AUDIO example.")
(defparameter *audio-format* (audio-s16)
  "The audio format to use in the TEST-AUDIO example.")

(defun signed-int16 (v)
  "Convert v to a 16-bit signed int."
  (logand #xFFFF (truncate v)))
(defun byte1 (int)
  "Return the first byte of the little-endian integer."
  (logand #xFF int))
(defun byte2 (int)
  "Return the second byte of the little-endian integer."
  (logand #xFF (ash int -8)))

(defparameter *num-samples-written* 0
  "Keeps track of how many samples have been written by SINE-AUDIO.")

(defun sine-audio (num-bytes-to-write)
  "Generate num-bytes-to-write of the next part of a 2-channel sine-wave
and return the result as an array of bytes."
  (let* (;; contains the audio data to be written
	 (result-array (make-array num-bytes-to-write
				   :element-type '(unsigned-byte 8)))
	 ;; bytes have been written to result-array
	 (num-bytes-written 0))

    ;; Loop until all bytes have been written to result-array
    (loop while (< num-bytes-written num-bytes-to-write)
       do
       ;; Process one 2-channel, 16-bit signed, little-endian, sample
	 (let* (;; Time (in seconds) elapsed since sound started
		(time (/ *num-samples-written* *samples/second*))
		;; How loud the sample is (#x7FFF is max)
		(amplitude #x6D60)
		;; Frequency for channel 1: A4
		(frequency1 440)
		;; Frequency for channel 2: F#5
		(frequency2 739.989)
		;; signed-int 16 for channel 1 sample
		(sample1 (signed-int16
			  (* amplitude (sin (* 2 pi frequency1 time)))))
		;; signed-int 16 for channel 2 sample
		(sample2 (signed-int16
			  (* amplitude (sin (* 2 pi frequency2 time))))))

	   ;; Channels are interleaved: write the audio for the first channel,
	   ;; then write the audio for the second channel.

	   ;; Channel 1
	   (setf (aref result-array num-bytes-written) (byte1 sample1))
	   (incf num-bytes-written)
	   (setf (aref result-array num-bytes-written) (byte2 sample1))
	   (incf num-bytes-written)

	   ;; Channel 2
	   (setf (aref result-array num-bytes-written) (byte1 sample2))
	   (incf num-bytes-written)
	   (setf (aref result-array num-bytes-written) (byte2 sample2))
	   (incf num-bytes-written)

	   ;; One sample has been written.
	   (incf *num-samples-written*)))
    result-array))

(defun test-audio ()
  "Play 2-channel audio. Each channel simultaneously plays a different frequency
for 3 seconds, then the program ends."
  ;; Create a new window titled Audio Test of width,height=200,200.
  (with-init "Audio Test" 200 200
    ;; Clear the screen to black and display it.
    (clear)
    (display)

    (unless (open-audio *audio-format*
			*samples/second*
			*num-channels*
			*audio-device-buffer-size-in-samples*
			;; Make the audio buffer the same size as the
			;; device audio buffer.
			(* *audio-device-buffer-size-in-samples*
			   (sample-size-in-bytes *audio-format*
						 *num-channels*)))
      (error "~&Failed to open audio. See stderr for details."))


    ;; It helps to start playing sound with the buffer full,
    ;; so write a sine-wave to it.
    ;; WITH-AUDIO-LOCK is not needed here, since sound is not playing, yet.
    (write-audio (sine-audio (audio-available)))

    (play-audio)
    (let ((start-ticks (ticks)))
      ;; Loop until at least 3 seconds have elapsed.
      (loop while (< (- (ticks) start-ticks) 3000)
	 do
	 ;; Acquire the audio lock, since the sound is playing.
	   (with-audio-lock
	     ;; Compute the next bytes of audio, and write them to the buffer.
	     ;; NOTE: write-audio and audio-available should BOTH
	     ;; be in with-audio-lock block, but sine-audio doesn't need to be.
	     (write-audio (sine-audio (audio-available))))
	   (delay 1)))
    (stop-audio)))

(defun test-mouse ()
  "Black window with a white cursor.
Print mouse press/release events."
  (with-init "Mouse Test" 200 200
    (let (;; Keep track of if a quit event has occurred.
	  (quit? nil)
	  ;; Keep track of the last time we updated.
	  (last-update-ticks (ticks))

	  ;; Keep track of the mouse pixel position for the cursor
	  (mouse-x 0)
	  (mouse-y 0))
      (loop until quit? do
	 ;; Only update if 1/60th of a second (16 ms) has occured.
	   (when (> (- (ticks) last-update-ticks) 16)
	     ;; Load the next event from the queue if there is one.
	     (loop while (poll-event)
		do
		  (cond
		    ;; Event was an in-window mouse movement
		    ((mouse-motion?)
		     (setq mouse-x (mouse-x))
		     (setq mouse-y (mouse-y)))
		    ;; Event was an in-window mouse-button press/release
		    ((or (mouse-button-down?)
			 (mouse-button-up?))
		     (format t "~&~A mouse button ~A at (~A,~A)"
			     (cond ((lmb?) "Left")
				   ((rmb?) "Right")
				   ((mmb?) "Middle"))
			     (if (mouse-button-down?) "pressed" "released")
			     (mouse-x) (mouse-y)))

		    ((quit?) (setq quit? t))))

	     ;; Clear the screen to black.
	     (draw-color 0 0 0 255)
	     (clear)
	     ;; Draw a white cursor
	     (draw-color 255 255 255 255)
	     (draw-rect mouse-x mouse-y 5 5 t)
	     ;; Display the new drawings
	     (display)

	     ;; Just finished an update, update the last-update-ticks
	     (setq last-update-ticks (ticks)))

	 ;; Yield to not be a CPU hog.
	   (delay 1)))))

(defun test-keyboard ()
  "Black window. Print key press/release events.
Prints scancode names that can be used with SCANCODE-FROM-NAME."
  (with-init "Keyboard Test" 200 200
    (let (;; Keep track of if a quit event has occurred.
	  (quit? nil)
	  ;; Keep track of the last time we updated.
	  (last-update-ticks (ticks)))
      (loop until quit? do
	 ;; Only update if 1/60th of a second (16 ms) has occured.
	   (when (> (- (ticks) last-update-ticks) 16)
	     ;; Load the next event from the queue if there is one.
	     (loop while (poll-event)
		do
		  (cond
		    ;; Event was key press/release
		    ((or (key-down?)
			 (key-up?))
		     (format t "~&Key ~A ~A."
			     (scancode-name (scancode))
			     (if (key-down?) "pressed" "released")))
		    ((quit?) (setq quit? t))))

	     ;; Clear the screen to black.
	     (draw-color 0 0 0 255)
	     (clear)
	     ;; Display the new drawings
	     (display)

	     ;; Just finished an update, update the last-update-ticks
	     (setq last-update-ticks (ticks)))

	 ;; Yield to not be a CPU hog.
	   (delay 1)))))

(defun test-joysticks ()
  "Black window. Print joysticks being added and removed.
Print joystick buttons being pressed/released and joystick axis movements."
  (with-init "Joysticks Test" 200 200
    (let (;; Keep track of if a quit event has occurred.
	  (quit? nil)
	  ;; Keep track of the last time we updated.
	  (last-update-ticks (ticks))

	  ;; List of joysticks that have been opened.
	  (joysticks nil))
      (loop until quit? do
	 ;; Only update if 1/60th of a second (16 ms) has occured.
	   (when (> (- (ticks) last-update-ticks) 16)
	     ;; Load the next event from the queue if there is one.
	     (loop while (poll-event)
		do
		  (cond
		    ;; Event was a joystick being plugged-in.
		    ((joy-added?)
		     
		     ;; If the joystick was added, open it so that we can
		     ;; listen for events.
		     ;; In this case only, JOY-ID is the order that the
		     ;; joystick was plugged in, not the same id as JOYSTICK-ID.
		     (let ((joystick (open-joystick (joy-id))))
		       (push joystick joysticks)
		       (format t "~&Joystick ~A added. ~Ath joystick plugged in."
			       (joystick-id joystick)
			       (joy-id))))

		    ;; Event was a joystick being un-plugged.
		    ((joy-removed?)
		     ;; JOY-ID is the unique id of the joystick,
		     ;; matching with JOYSTICK-ID.
		     (let ((joystick
			    (find (joy-id) joysticks :key #'joystick-id)))
		       ;; Close the joystick if it was un-plugged.
		       (close-joystick joystick)
		       ;; Remove the closed joystick from our list of
		       ;; JOYSTICKS.
		       (setq joysticks (remove joystick joysticks)))
		     (format t "~&Joystick ~A removed." (joy-id)))  

		    ;; Event was a joystick button being pressed/released.
		    ((or (joy-down?)
			 (joy-up?))
		     (format t "~&Joystick ~A ~A button ~A"
			     (joy-id)
			     (if (joy-down?) "pressed" "released")
			     (joy-button)))

		    ;; Event was a joystick axis being moved.
		    ((joy-axis?)
		     (format t "~&Joystick ~A moved axis ~A to ~A"
			     (joy-id)
			     (joy-axis)
			     (joy-axis-value)))
		    ((quit?) (setq quit? t))))

	     ;; Clear the screen to black.
	     (draw-color 0 0 0 255)
	     (clear)
	     ;; Display the new drawings
	     (display)

	     ;; Just finished an update, update the last-update-ticks
	     (setq last-update-ticks (ticks)))

	 ;; Yield to not be a CPU hog.
	   (delay 1))

      ;; Be sure to close all open joysticks
      (mapc #'close-joystick joysticks))))

(defun test-texture ()
  "Draw some textures to the window.
Shows angle, pivot, flipping, animation, and stretching."
  (with-init "Texture test" 320 240
    (let (;; Keep track of if a quit event has occurred.
	  (quit? nil)
	  ;; Keep track of the last time we updated.
	  (last-update-ticks (ticks))
	  (texture (load-bmp
		    (namestring
		     (asdf:system-relative-pathname :ssdl "megasheet.bmp")))))
      (loop until quit? do
	 ;; Only update if 1/60th of a second (16 ms) has occured.
	   (when (> (- (ticks) last-update-ticks) 16)
	     ;; Load the next event from the queue if there is one.
	     (loop while (poll-event)
		do
		  (cond ((quit?) (setq quit? t))))

	     ;; Clear the screen to black.
	     (draw-color 0 0 0 255)
	     (clear)

	     ;; Draw a megaman, spinning around his center.
	     (draw-texture
	      texture
	      ;; Source rectangle of the megasheet.bmp spritesheet.
	      2 2 48 48
	      ;; Destination rectangle on the window.
	      10 10 48 48
	      ;; Angle in degrees, rotating over time
	      (* 1d0 (mod (floor (ticks) 15) 360))
	      ;; Pivot is at the center of the destination rectangle.
	      (/ 48 2) (/ 48 2)
	      (flip-none))

	     ;; Draw a big megaman.
	     (draw-texture
	      texture
	      2 2 48 48
	      ;; Destination rectangle is twice as big.
	      ;; Stretched to be wider than tall.
	      100 10 (* 48 3) (* 48 2)
	      0.0d0
	      0 0
	      (flip-none))

	     ;;; Draw flipped megamen
	     ;; Flipped horizontally
	     (draw-texture
	      texture
	      2 2 48 48
	      10 100 48 48
	      0.0d0
	      0 0
	      (flip-horizontal))
	     ;; Flipped both ways
	     (draw-texture
	      texture
	      2 2 48 48
	      40 100 48 48
	      0.0d0
	      0 0
	      (logior (flip-horizontal) (flip-vertical)))

	     ;; Draw animated
	     (draw-texture
	      texture
	      ;; Animate the x coordinate of the sprite
	      (+ 2 (* (+ 3 (mod (floor (ticks) 100) 4)) 51)) 2 48 48
	      100 100 48 48
	      0d0
	      0 0
	      (flip-none))

	     ;; Display the new drawings
	     (display)

	     ;; Just finished an update, update the last-update-ticks
	     (setq last-update-ticks (ticks)))

	 ;; Yield to not be a CPU hog.
	   (delay 1))

      ;; Now we can free the texture.
      (free-texture texture))))

(defun test-texture-mod ()
  (with-init "Texture Modtest" 320 240
    (let ((texture (load-bmp
		    (namestring
		     (asdf:system-relative-pathname :ssdl "megasheet.bmp")))))
      ;; Clear the screen to green.
      (draw-color 0 255 0 255)
      (clear)

      ;; Give a reddish tint.
      (texture-color-mod texture 255 100 100 255)
      (draw-texture
       texture
       2 2 48 48
       10 10 (* 2 48) (* 2 48)
       0d0 0 0 (flip-none))

      ;; Make semi-transparent.
      (texture-color-mod texture 255 255 255 128)
      (draw-texture
       texture
       2 2 48 48
       150 10 (* 2 48) (* 2 48)
       0d0 0 0 (flip-none))

      (display)
      (delay 3000)

      ;; Now we can free the texture.
      (free-texture texture))))

(defun test-rect ()
  "Draw two rects on a green background for 3 seconds."
  (with-init "Test Rect" 640 480
    ;; Clear to Green
    (draw-color 0 255 0 255)
    (clear)

    ;; Red outline of a rectangle
    (draw-color 255 0 0 255)
    (draw-rect 10 10 100 100 nil)

    ;; Translucent Blue rectangle
    (draw-color 0 0 255 128)
    (draw-rect 80 80 50 50 t)

    (display)
    (delay 3000)))
