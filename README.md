Super-Simple DirectMedia Layer.
===============================

Simplified wrapper around SDL meant for one-window projects
that just want a simple foreign function interface with the bare essentials.
Basically for 2D games, where you want to do everything yourself, but just
want a window, input, and textures.

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
* Draw Textures! Flip and/or modulate the color & alpha.
* Render to a texture instead of the screen
* Make a texture from an array of pixels
* Draw Rectangles! Just the outline, or draw 'em solid!
* Detect keyboard and mouse input!
* Support as many joysticks as you want, but axes and buttons only.

In fact, that's it. Everything else is up to you. Simple. Super-Simple. 

Things you FINALLY have the opportunity to implement yourself:
* Audio mixing: Just make an array of bytes and write it to the buffer!
* Audio decoding and streaming
* Image decoders
* Bitmap fonts
* Drawing shapes

With SSDL, less is more. After all, everyone has their own way of
structuring things.
