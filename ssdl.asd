;;;; ssdl.asd
;;;;
;;;; Copyright (c) 2017 Christopher Hebert <hebert.christopherj@gmail.com>

(asdf:defsystem #:ssdl
  :description "Super-Simple DirectMedia Layer. Simplified wrapper around SDL
to remove some lower-level details. Hides structures behind functions to make
FFI much less complicated. Only uses SDL2.0.4.

Supports exactly one window and up to one audio device.
Can draw textures (angled and/or flipped) and rectangles and clear the screen.
Supports one keyboard, one mouse and many joysticks (buttons/axes only).
Supports loading bitmaps with magenta masked out.

See the README.txt for example programs."
  :author "Christopher Hebert <hebert.christopherj@gmail.com>"
  :license "All Rights Reserved"
  :depends-on ()
  :serial t
  :components ((:file "package")
               (:file "ssdl")
	       (:file "examples")))

