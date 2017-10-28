;;;; package.lisp
;;;;
;;;; Copyright (c) 2017 Christopher Hebert <hebert.christopherj@gmail.com>

(defpackage #:ssdl
  (:use #:cl #:sb-alien)
  (:export
   #:init
   #:quit
   #:with-init
   #:error-string

   #:display

   #:clear
   #:draw-rect
   #:draw-color

   #:load-bmp
   #:load-image
   #:draw-texture
   #:free-texture

   #:poll-event

   #:key-down?
   #:key-up?
   #:repeat?
   #:scancode
   #:scancode-name
   #:scancode-from-name

   #:mouse-motion?
   #:mouse-button-down?
   #:mouse-button-up?
   #:lmb?
   #:mmb?
   #:rmb?
   #:mouse-x
   #:mouse-y

   #:joy-added?
   #:joy-removed?
   #:joy-down?
   #:joy-up?
   #:joy-axis?
   #:joy-id
   #:joy-button
   #:joy-axis
   #:joy-axis-value

   #:quit?

   #:open-joystick
   #:close-joystick
   #:joystick-id

   #:with-audio-lock
   #:audio-lock
   #:audio-unlock
   #:format-byte-size
   #:open-audio
   #:close-audio
   #:audio-available
   #:write-audio
   #:clear-audio
   #:play-audio
   #:stop-audio
   #:sample-size-in-bytes

   #:ticks
   #:delay
   #:make-texture-from-pixels
   #:make-texture
   #:render-to-texture
   #:texture-color-mod
   #:render-to-window
   #:flip-horizontal-and-vertical
   #:render-pixels-to-window
   #:texture-height
   #:texture-width
   #:null?))

(defpackage #:ssdl-examples
  (:use #:cl #:ssdl)
  (:export
   #:test-audio
   #:test-mouse
   #:test-keyboard
   #:test-joysticks
   #:test-texture
   #:test-texture-mod
   #:test-rect

   #:*samples/second*
   #:*num-channels*
   #:*audio-device-buffer-size-in-samples*
   #:*audio-format*
   #:signed-int16
   #:byte1
   #:byte2
   #:*num-samples-written*
   #:sine-audio
   #:test-pixel-texture
   #:test-pixels-to-window
   #:test-texture-target))
