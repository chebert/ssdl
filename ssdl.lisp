(ql:quickload :cffi)

(defpackage :ssdl
  (:use :cl))

(in-package :ssdl)

(cffi:define-foreign-library :ssdl
  (t "/home/chebert/Projects/ssdl/libssdl.so"))

(cffi:use-foreign-library :ssdl)

(cffi:defcfun ("quit" quit) :void)

(cffi:defcfun ("init" init) :bool
  (title :string)
  (width :int)
  (height :int))

(cffi:defcfun ("flip" flip) :void)
(cffi:defcfun ("clear" clear) :void)
(cffi:defcfun ("draw_rect" draw-rect) :void
  (x :int)
  (y :int)
  (w :int)
  (h :int)
  (filled? :bool))

(cffi:defcfun ("load_bmp" load-bmp) :pointer
  (path :string))

(cffi:defcfun ("flip_none" flip-none) :uint32)
(cffi:defcfun ("flip_horizontal" flip-horizontal) :uint32)
(cffi:defcfun ("flip_vertical" flip-vertical) :uint32)

(cffi:defcfun ("draw_texture" draw-texture) :void
  (texture :pointer)
  (sx :int) (sy :int)
  (sw :int) (sh :int)
  (dx :int) (dy :int)
  (dw :int) (dh :int)
  (angle :double)
  (px :int) (py :int)
  (flip-flags :uint32))

(cffi:defcfun ("free_texture" free-texture) :void
  (texture :pointer))

(cffi:defcfun ("draw_color" draw-color) :void
  (r :uint8)
  (g :uint8)
  (b :uint8)
  (a :uint8))

(cffi:defcfun ("poll_event" poll-event) :bool)
(cffi:defcfun ("is_key_down" key-down?) :bool)
(cffi:defcfun ("is_key_up" key-up?) :bool)
(cffi:defcfun ("is_repeat" repeat?) :bool)
(cffi:defcfun ("scancode" scancode) :uint32)

(cffi:defcfun ("is_mouse_motion" mouse-motion?) :bool)
(cffi:defcfun ("is_mouse_button_down" mouse-button-down?) :bool)
(cffi:defcfun ("is_mouse_button_up" mouse-button-up?) :bool)
(cffi:defcfun ("is_lmb" lmb?) :bool)
(cffi:defcfun ("is_mmb" mmb?) :bool)
(cffi:defcfun ("is_rmb" rmb?) :bool)
(cffi:defcfun ("mouse_x" mouse-x) :int)
(cffi:defcfun ("mouse_y" mouse-y) :int)

(cffi:defcfun ("is_quit" quit?) :bool)

(cffi:defcfun ("audio_available" audio-available) :int)
(cffi:defcfun ("write_audio" write-audio%) :void
  (source (:pointer :uint8))
  (n :int))

(defun write-audio (bytes)
  (declare (type (simple-array (unsigned-byte 8)) bytes))
  (let ((len (length bytes)))
    (cffi:with-foreign-object (carr :uint8 len)
      (loop for i below len
	 do (setf (cffi:mem-aref carr :uint8 i) (aref bytes i)))
      (write-audio% carr len))))

(cffi:defcfun ("clear_audio" clear-audio) :void)
(cffi:defcfun ("stop_audio" stop-audio) :void)
(cffi:defcfun ("play_audio" play-audio) :void)

;; TODO: audio-format
(defparameter *audio-u8* #x0008)
(defparameter *audio-s8* #x8008)
(defparameter *audio-u16* #x0010)
(defparameter *audio-s16* #x8010)
(defparameter *audio-s32* #x8020)
(defparameter *audio-f32* #x8120)

(cffi:defcfun ("open_audio" open-audio) :bool
  (audio-format :uint16)
  (samples-per-second :int)
  (num-channels :int)
  (audio-buffer-size-in-samples :int)
  (buffer-size-in-bytes :int))

(cffi:defcfun ("close_audio" close-audio) :void)

(cffi:defcfun ("ticks" ticks) :int)
(cffi:defcfun ("delay" delay) :void
  (ms :int))

(cffi:defcfun ("scancode_name" scancode-name) :string
  (scancode :uint32))
(cffi:defcfun ("scancode_from_name" scancode-from-name) :uint32
  (name :string))

(cffi:defcfun ("is_joy_added" joy-added?) :bool)
(cffi:defcfun ("is_joy_removed" joy-removed?) :bool)
(cffi:defcfun ("is_joy_down" joy-down?) :bool)
(cffi:defcfun ("is_joy_up" joy-up?) :bool)
(cffi:defcfun ("is_joy_axis" joy-axis?) :bool)
(cffi:defcfun ("joy_id" joy-id) :int)
(cffi:defcfun ("joy_button" joy-button) :int)
(cffi:defcfun ("joy_axis" joy-axis) :int)
(cffi:defcfun ("joy_axis_value" joy-axis-value) :int)

(defparameter *samples/second* 44100)
(defparameter *num-channels* 2)
(defparameter *audio-buffer-size-in-samples* 2048)
(let ((sn 0))
  (defun sine-audio (num-samples)
    (let* ((num-bytes (* num-samples *num-channels* 2))
	   (v (make-array num-bytes))
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

  (open-audio *audio-s16*
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
