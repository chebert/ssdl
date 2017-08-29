Super-Simple DirectMedia Layer.
Simplified wrapper around SDL meant for one-window projects
that just want the bare essentials.

Wrapper removes some lower-level details.
Hides structures behind functions and removes audio callback
to make FFI much less complicated. Only uses SDL2.0.4.

Supports exactly one window and up to one audio device.
Can draw textures (angled and/or flipped) and rectangles and clear the screen.
Supports one keyboard, one mouse and many joysticks (buttons/axes only).
Supports loading bitmaps with magenta masked out.

