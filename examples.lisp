(in-package :ssdl-examples)

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

(defun test-texture-target ()
  "Make a texture that can be drawn to, and then draw it to the screen."
  (with-init "Test texture target" 640 480
    (let* ((width 40)
	   (height 40)
	   (texture (make-texture width height)))

      ;; Switch to rendering to the new texture.
      (render-to-texture texture)
      ;; Draw a couple of green rects on a magenta background
      ;; to the texture.

      (draw-color 255 0 255 255)
      (clear)
      (draw-color 0 255 0 255)
      (draw-rect 0 0 20 20 t)

      (draw-color 0 255 0 120)
      (draw-rect 20 20 20 20 nil)

      ;; Now switch back to rendering to the window.
      (render-to-window)
      ;; Clear to black
      (draw-color 0 0 0 255)
      (clear)
      ;; Draw the texture.
      (draw-texture
       texture
       0 0 40 40
       10 10 80 80
       0d0 0 0 (flip-none))
      (display)

      (delay 3000)
      (free-texture texture))))

(defun test-pixel-texture ()
  "Make a texture from an array of pixels and draw it to the screen.
Quit after a few seconds."
  (with-init "Test Pixel Texture" 640 480
    (let* ((width 20)
	   (height 40)
	   (num-pixels (* width height))
	   ;; Pixels are 4 bytes: R,G,B,A (in that order)
	   (num-bytes (* num-pixels 4))
	   ;; the index of the next byte to be written to the pixel array
	   (next-byte 0)
	   ;; The index of the next pixel to be written to the next pixel
	   (next-pixel 0)
	   (texture nil))
      (let (;; The array of pixels that will become the texture.
	    (pixels (make-array num-bytes :element-type '(unsigned-byte 8))))
	(loop while (< next-byte num-bytes)
	   do
	     (cond
	       ;; Create a checkerboard pattern
	       ((eq (zerop (mod (floor next-pixel 5) 2))
		    (= 1 (mod (floor next-pixel (* 5 width)) 2)))
		;; Red
		(setf (aref pixels next-byte) 255)
		(incf next-byte)
		;; Green
		(setf (aref pixels next-byte) 0)
		(incf next-byte)
		;; Blue
		(setf (aref pixels next-byte) 0)
		(incf next-byte)
		;; Alpha
		(setf (aref pixels next-byte) 128)
		(incf next-byte))
	       (t
		;; Red
		(setf (aref pixels next-byte) 0)
		(incf next-byte)
		;; Green
		(setf (aref pixels next-byte) 0)
		(incf next-byte)
		;; Blue
		(setf (aref pixels next-byte) 255)
		(incf next-byte)
		;; Alpha
		(setf (aref pixels next-byte) 200)
		(incf next-byte)))
	     (incf next-pixel))
	(setq texture (make-texture-from-pixels width height pixels)))
      
      (draw-color 0 255 0 255)
      (clear)
      (draw-texture texture
		    0 0 width height
		    0 0 width height
		    0d0 0 0 (flip-none))
      (display)
      (delay 3000)
      (free-texture texture))))
