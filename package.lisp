;;;; package.lisp
;;;;
;;;; Copyright (c) 2017 Christopher Hebert <hebert.christopherj@gmail.com>

(defpackage #:ssdl
  (:use #:cl)
  (:export
   #:quit
   #:init
   #:display
   #:clear
   #:draw-rect
   #:load-bmp
   #:flip-none
   #:flip-horizontal
   #:flip-vertical
   #:draw-texture
   #:free-texture
   #:draw-color
   #:poll-event
   #:key-down?
   #:key-up?
   #:repeat?
   #:scancode
   #:mouse-motion?
   #:mouse-button-down?
   #:mouse-button-up?
   #:lmb?
   #:mmb?
   #:rmb?
   #:mouse-x
   #:mouse-y
   #:quit?
   #:audio-available
   #:write-audio
   #:clear-audio
   #:stop-audio
   #:play-audio
   #:*audio-u8*
   #:*audio-s8*
   #:*audio-u16*
   #:*audio-s16*
   #:*audio-s32*
   #:*audio-f32*
   #:open-audio
   #:sample-size-in-bytes
   #:close-audio
   #:ticks
   #:delay
   #:scancode-name
   #:scancode-from-name
   #:joy-added?
   #:joy-removed?
   #:joy-down?
   #:joy-up?
   #:joy-axis?
   #:joy-id
   #:joy-button
   #:joy-axis
   #:joy-axis-value
   #:audio-u8
   #:audio-s8
   #:audio-u16
   #:audio-s16
   #:audio-s32
   #:audio-f32))

