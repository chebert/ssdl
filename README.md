Super-Simple DirectMedia Layer.
===============================

Simplified wrapper around SDL meant for one-window projects
that just want a simple foreign function interface with the bare essentials.

SSDL removes many of the lower-level details in SDL.

* hides constants and structures behind function calls
* removes callback functions

making the FFI (foreign function interface) much less complicated.
SSDL is extremely tiny and only has two dynamic library files: SSDL and version
2.0.4 of SDL.

Features:
* Documentation!
* Examples! See examples.lisp and :ssdl-examples package.
* Exactly one window
* Write your own audio mixer! Write bytes directly to the audio buffer.
* Load BMP, 24-bit RGB images: 
Automagically mask out magenta (1.0, 0.0, 1.0) pixels, 
whether you want it or not!
* Draw Textures! Rotate, flip, or modulate the color & alpha.
* Render to a texture!
* Draw Rectangles! Just the outline, or draw 'em solid!
* Keyboard and mouse input!
* As many joysticks as you want, but axes and buttons only.

In fact, that's it. Everything else is up to you. Simple. Super-Simple. 

Things you FINALLY have the opportunity to implement yourself:
* Audio mixing: Just make an array of bytes and write it to the buffer!
* Audio decoding and streaming
* Image decoders
* Bitmap fonts
* Drawing shapes
