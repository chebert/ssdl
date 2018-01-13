#include "SDL2/SDL.h"
#include "SDL2/SDL_image.h"
#include "SDL2/SDL_ttf.h"
#include <assert.h>
#include <math.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#define STDERR stdout

#ifdef ADD_EXPORTS
	#define API __declspec(dllexport)
#else
	#define API
#endif

static SDL_Window* window;
static SDL_Renderer* renderer;
static SDL_Texture* window_pixels;
static int window_width, window_height;

API void quit() {
   if (window) SDL_DestroyWindow(window);
   if (renderer) SDL_DestroyRenderer(renderer);
   if (window_pixels) SDL_DestroyTexture(window_pixels);
   window = NULL;
   renderer = NULL;
   window_pixels = NULL;
   IMG_Quit();
   TTF_Quit();
   // Quit SDL and close the audio.
   SDL_Quit();
}

API int init(const char *title, int width, int height) {
   // Return true if successful, otherwise false and prints error to STDERR.
   int success = 1;
   if (SDL_Init(SDL_INIT_EVERYTHING) != 0) {
      fprintf(STDERR, "Failed to init: %s\n", SDL_GetError());
      success = 0;
   }
   if (window) SDL_DestroyWindow(window);
   window = SDL_CreateWindow(title, 100, 100, width, height, 0);
   if (!window) {
      fprintf(STDERR, "Failed to create a window: %s\n", SDL_GetError());
      success = 0;
   }
   window_width = width;
   window_height = height;
   if (renderer) SDL_DestroyRenderer(renderer);
   renderer = SDL_CreateRenderer(window, -1, 0);
   if (!renderer) {
      fprintf(STDERR, "Failed to create a renderer: %s\n", SDL_GetError());
      success = 0;
   }
   if (window_pixels) SDL_DestroyTexture(window_pixels);
   window_pixels = SDL_CreateTexture(renderer,
				     SDL_PIXELFORMAT_ABGR8888,
				     SDL_TEXTUREACCESS_STREAMING,
				     width, height);
   if (!window_pixels) {
      fprintf(STDERR, "Failed to create a pixel back-buffer: %s\n", SDL_GetError());
      success = 0;
   }

   IMG_Init(IMG_INIT_JPG | IMG_INIT_PNG | IMG_INIT_TIF);
   TTF_Init();
   SDL_ShowCursor(SDL_DISABLE);
   SDL_SetRenderDrawBlendMode(renderer, SDL_BLENDMODE_BLEND);
   return success;
}

API const char* error_string() {
   return SDL_GetError();
}

API void display() {
   // Flip the back buffer.
   SDL_RenderPresent(renderer);
}

API void clear() {
   // Clear the screen to the render draw color
   SDL_RenderClear(renderer);
}

API void draw_rect(int x, int y, int w, int h, int filled) {
   // Draw a rect using the render draw color.
   // Filled if filled is true, outlined if filled is false.
   SDL_Rect rect;
   rect.x = x; rect.y = y; rect.w = w; rect.h = h;
   if (filled) {
      SDL_RenderFillRect(renderer, &rect);
   } else {
      SDL_RenderDrawRect(renderer, &rect);
   }
}

API SDL_Texture* load_bmp(const char* path) {
   // Load a BMP from path using magenta as the color key.
   SDL_Surface* surf = SDL_LoadBMP(path);
   if (!surf) return NULL;
   SDL_SetColorKey(surf, 1, SDL_MapRGB(surf->format, 255, 0, 255));
   SDL_Texture* tex = SDL_CreateTextureFromSurface(renderer, surf);
   SDL_FreeSurface(surf);
   return tex;
}

API SDL_Texture* load_image(const char* path) {
   // Load image from the path.
   SDL_Surface* surf = IMG_Load(path);
   if (!surf) return NULL;
   SDL_Texture* tex = SDL_CreateTextureFromSurface(renderer, surf);
   SDL_FreeSurface(surf);
   return tex;
}

API void draw_texture(SDL_Texture* tex,
		  int sx, int sy, int sw, int sh,
		  int dx, int dy, int dw, int dh,
		  int flip_horizontal, int flip_vertical) {
   // Draw the texture from the source rect to the dest rect.
   SDL_Rect src, dst;
   src.x = sx; src.y = sy; src.w = sw, src.h = sh;
   dst.x = dx; dst.y = dy; dst.w = dw, dst.h = dh;

   SDL_RendererFlip flip = flip_horizontal ? SDL_FLIP_HORIZONTAL : SDL_FLIP_NONE;
   flip = flip_vertical ? SDL_FLIP_VERTICAL | flip : flip;
   if (flip != SDL_FLIP_NONE) {
      SDL_RenderCopyEx(renderer, tex, &src, &dst, 0.0, NULL, flip);
   } else {
      SDL_RenderCopy(renderer, tex, &src, &dst);
   }
}

API void free_texture(SDL_Texture* tex) {
   // Free the memory held by tex
   SDL_DestroyTexture(tex);
}

API void draw_color(unsigned char r, unsigned char g, unsigned char b, unsigned char a) {
   // Set the render draw color
   SDL_SetRenderDrawColor(renderer, r, g, b, a);
}

API int texture_width(SDL_Texture* tex) {
   int width;
   SDL_QueryTexture(tex, NULL, NULL, &width, NULL);
   return width;
}
API int texture_height(SDL_Texture* tex) {
   int height;
   SDL_QueryTexture(tex, NULL, NULL, NULL, &height);
   return height;
}

API TTF_Font* open_font(const char* file, int ptsize) {
   return TTF_OpenFont(file, ptsize);
}
API void close_font(TTF_Font* font) {
   TTF_CloseFont(font);
}

API SDL_Texture* text_texture(TTF_Font* font,
			  const char* text,
			  unsigned char fg_r,
			  unsigned char fg_g,
			  unsigned char fg_b,
			  unsigned char fg_a,
			  unsigned char bg_r,
			  unsigned char bg_g,
			  unsigned char bg_b,
			  unsigned char bg_a) {
   // Shaded drawing of text using fg color onto box of bg color.
   // Text is utf8.
   const SDL_Color fg = {fg_r, fg_g, fg_b, fg_a}, bg = {bg_r, bg_g, bg_b, bg_a};
   SDL_Surface* surf = TTF_RenderUTF8_Shaded(font, text, fg, bg);
   if (!surf) return NULL;
   SDL_Texture* tex = SDL_CreateTextureFromSurface(renderer, surf);
   SDL_FreeSurface(surf);
   return tex;
}

static SDL_Event event;

API int poll_event() {
   // Poll the next event. True if there are pending events.
   return SDL_PollEvent(&event);
}

// Keyboard event accessors
API int is_key_down() { return event.type == SDL_KEYDOWN; }
API int is_key_up() { return event.type == SDL_KEYUP; }

API int is_repeat() { return event.key.repeat; }
API SDL_Scancode scancode() { return event.key.keysym.scancode; }

// Mouse event accessors
API int is_mouse_motion() { return event.type == SDL_MOUSEMOTION; }
API int is_mouse_button_down() { return event.type == SDL_MOUSEBUTTONDOWN; }
API int is_mouse_button_up() { return event.type == SDL_MOUSEBUTTONUP; }
API int is_lmb() { return event.button.button == SDL_BUTTON_LEFT; }
API int is_mmb() { return event.button.button == SDL_BUTTON_MIDDLE; }
API int is_rmb() { return event.button.button == SDL_BUTTON_RIGHT; }
API int mouse_x() {
   if (event.type == SDL_MOUSEMOTION) {
      return event.motion.x;
   } else {
      return event.button.x;
   }
}
API int mouse_y() {
   if (event.type == SDL_MOUSEMOTION) {
      return event.motion.y;
   } else {
      return event.button.y;
   }
}

API void window_size(int w, int h) {
   SDL_SetWindowSize(window, w, h);
}

// Joystick event accessors
API int is_joy_added() { return event.type == SDL_JOYDEVICEADDED; }
API int is_joy_removed() { return event.type == SDL_JOYDEVICEREMOVED; }
API int joy_id() {
   switch (event.type) {
   case SDL_JOYAXISMOTION: return event.jaxis.which;
   case SDL_JOYBUTTONDOWN:
   case SDL_JOYBUTTONUP: return event.jbutton.which;
   case SDL_JOYDEVICEADDED:
   case SDL_JOYDEVICEREMOVED: return event.jdevice.which;
   }
}
API int is_joy_down() { return event.type == SDL_JOYBUTTONDOWN; }
API int is_joy_up() { return event.type == SDL_JOYBUTTONUP; }
API int joy_button() { return event.jbutton.button; }
API int is_joy_axis() { return event.type == SDL_JOYAXISMOTION; }
API int joy_axis() { return event.jaxis.axis; }
API int joy_axis_value() { return event.jaxis.value; }

API int is_quit() { return event.type == SDL_QUIT; }

// Text Editing
API int is_text_input() { return event.type == SDL_TEXTINPUT; }
API char* text_input() { return event.text.text; }
API void enable_text_input() { SDL_StartTextInput(); }
API void disable_text_input() { SDL_StopTextInput(); }

// Structure that holds audio parameters.
static struct {
   SDL_AudioFormat format;
   int frequency, channels, samples;

   // Ring Buffer for queuing audio to be written by SDL's audio callback
   unsigned char *buffer;
   int start, end, size, capacity;
} audio = { 0 };

API int audio_available() {
   // Number of bytes that can be written to the ring buffer
   // Call inside of a SDL_LockAudio block
   return audio.capacity - audio.size;
}

API void write_audio(unsigned char* src, int n) {
   // Write n bytes of audio from src into the ring buffer.
   // Call inside of a SDL_LockAudio block

   // Assume that n <= audio.size
   assert(n <= audio_available());

   // start < end
   //  | 0 | 1 | ... | start | ... | end   | ... | capacity-1 |
   // start == end
   //  | 0 | 1 | ... | start, end  | ...         | capacity-1 |, size = 0
   //  r1=end,capacity-end
   //  r2=0,start
   if (audio.start <= audio.end) {
      int r1_size = audio.capacity - audio.end;
      if (n <= r1_size) {
	 memcpy(&audio.buffer[audio.end], &src[0], n);
	 audio.end += n;
	 if (audio.end == audio.capacity)
	    audio.end = 0;
      } else {
	 int r2_size = n - r1_size;
	 memcpy(&audio.buffer[audio.end], &src[0], r1_size);
	 memcpy(&audio.buffer[0], &src[r1_size], r2_size);
	 audio.end = r2_size;
      }
   } else {
      // start > end
      //  | 0 | 1 | ... | end | ...   | start | ... | capacity-1 |, size = (end) + (capacity - start)
      memcpy(&audio.buffer[audio.end], &src[0], n);
      audio.end += n;
   }

   audio.size += n;
}

static void read_audio(unsigned char* dest, int n) {
   // Read n bytes of audio from ring buffer into dest.
   assert(n <= audio.size);
   // start < end
   //  | 0 | 1 | ... | start | ... | end   | ... | capacity-1 |
   if (audio.start < audio.end) {
      memcpy(&dest[0], &audio.buffer[audio.start], n);
      audio.start += n;
      if (audio.start == audio.capacity)
	 audio.start = 0;
   } else {
      // start == end
      //  | 0 | 1 | ... | start, end  | ...         | capacity-1 |, size = capacity
      // start > end
      //  | 0 | 1 | ... | end | ...   | start | ... | capacity-1 |
      int r1_size = audio.capacity - audio.start;
      if (n <= r1_size) {
	 memcpy(&dest[0], &audio.buffer[audio.start], n);
	 audio.start += n;
      } else {
	 int r2_size = n - r1_size;
	 memcpy(&dest[0], &audio.buffer[audio.start], r1_size);
	 memcpy(&dest[r1_size], &audio.buffer[0], r2_size);
	 audio.start = r2_size;
      }
   }

   audio.size -= n;
}

API void clear_audio() {
   // Clear any queued audio from the ring buffer.
   // Call inside of a SDL_LockAudio block
   audio.start = 0;
   audio.end = 0;
   audio.size = 0;
}

static void init_audio(int bytes) {
   // Create a new audio buffer of length bytes
   if (audio.buffer) free(audio.buffer);
   audio.buffer = (unsigned char*)malloc(bytes);
   audio.capacity = bytes;
   clear_audio();
}

static void silent_audio(unsigned char *dest, int n) {
   // Write n bytes of silent audio into dest
   int i;
   for (i = 0; i < n; ++i) {
      dest[i] = 0;
   }
}

static void audio_callback(void *userdata, unsigned char *stream, int bytes) {
   // audio_callback reads from ring buffer into stream
   // if ring buffer is empty fill with 0s
   int read_bytes = bytes > audio.size ? audio.size : bytes;
   int silent_bytes = bytes > audio.size ? bytes - audio.size : 0;
   read_audio(stream, read_bytes);
   stream += read_bytes;
   silent_audio(stream, silent_bytes);
}

static void write_sine() {
   // Write a test sine wave to the 2-channel audio buffer.
   static int sn1 = 0;
   int i;
   SDL_LockAudio();
   int n = audio_available() / (audio.channels * 2);
   for (i = 0; i < n; ++i) {
      double time = (double)sn1 / (double) audio.frequency;
      Sint16 sample1 = (Sint16)(28000 * sin(2.0f * M_PI * 400.0f * time));
      Sint16 sample2 = (Sint16)(28000 * sin(2.0f * M_PI * 720.0f * time));
      // channel 1
      write_audio((unsigned char*)&sample1, 2);
      // channel 2
      write_audio((unsigned char*)&sample2, 2);
      sn1++;
   }
   SDL_UnlockAudio();
}

API void stop_audio() {
   // Pause/stop playback of audio.
   SDL_PauseAudio(1);
}
API void play_audio() {
   // Resume/Start playback of audio.
   SDL_PauseAudio(0);
}

static int format_byte_size(SDL_AudioFormat fmt) { return (0xFF & fmt) >> 3; }

API int open_audio(int freq, int num_channels, int num_samples) {
   // Open an audio device with the provided parameters,
   // and create the audio buffer of size buffer_bytes.
   // Return true if successful.
   int success = 1;
   SDL_AudioSpec desired, obtained;
   SDL_AudioFormat fmt = AUDIO_S16LSB;
   desired.format = audio.format = fmt;
   desired.freq = audio.frequency = freq;
   desired.channels = audio.channels = num_channels;
   desired.samples = audio.samples = num_samples;
   desired.callback = audio_callback;

   if (SDL_OpenAudio(&desired, &obtained) != 0) {
      success = 0;
      fprintf(STDERR, "Failed to open audio: %s\n", SDL_GetError());
   }
   if (obtained.format != desired.format) {
      success = 0;
      fprintf(STDERR, "Failed to obtain desired audio spec: %s\n", SDL_GetError());
   }

   int buffer_bytes = num_samples * format_byte_size(fmt) * num_channels;
   init_audio(buffer_bytes);
   return success;
}
API void close_audio () {
   // Close the audio device and free the audio buffer.
   SDL_CloseAudio();
   free(audio.buffer);
}

static void print_audio() {
   int i;
   printf("Audio:\n");
   for (i = 0; i < audio.capacity; ++i) {
      printf("| %d ", i);
   }
   printf("|\n");
   for (i = 0; i < audio.capacity; ++i) {
      printf("| %d ", audio.buffer[i]);
   }
   printf("|\n");
   for (i = 0; i < audio.capacity; ++i) {
      if (i == audio.start && i == audio.end) {
	 if (audio.size == 0) 
	    printf("| <>");
	 else
	    printf("| ><");
      } else if (i == audio.start) {
	 printf("| < ");
      } else if (i == audio.end) {
	 printf("| > ");
      } else {
	 printf("|   ");
      }
   }
   printf("|\n");
}

static int test_ring_buffer() {
   unsigned char to_write[] = { 0, 1, 2, 3, 4, 5, 6, 7 };
   unsigned char to_read[] = { 0, 0, 0, 0, 0, 0, 0, 0 };
   int i;
   init_audio(8);
   write_audio(to_write, 4);
   print_audio();
   write_audio(to_write, 2);
   print_audio();
   read_audio(to_read, 2);
   print_audio();
   printf("To Read\n");
   for (i = 0; i < 8; ++i) {
      printf("| %d ", i);
   }
   printf("|\n");
   for (i = 0; i < 8; ++i) {
      printf("| %d ", to_read[i]);
   }
   printf("|\n");

   clear_audio();
   print_audio();
   write_audio(to_write, 8);
   read_audio(to_read, 4);
   write_audio(to_write, 4);
   read_audio(to_read, 8);
   print_audio();
}

API int ticks() {
   // Milliseconds elapsed since init() was called
   return SDL_GetTicks();
}

API void delay(int milliseconds) {
   // Pause execution for at least milliseconds time.
   // Yield execution.
   // Call delay(1) to yield execution in busy loops.
   SDL_Delay(milliseconds);
}
API const char* scancode_name(SDL_Scancode sc) {
   // Return the string name of the scancode.
   // String can be used in scancode_from_name()
   return SDL_GetScancodeName(sc);
}
API SDL_Scancode scancode_from_name(const char* str) {
   // Return the scancode given the string name.
   return SDL_GetScancodeFromName(str);
}

API SDL_Joystick* open_joystick(int device_index) {
   // Open the Nth plugged-in joystick.
   // device_index is the joy_id() of an joystick add event.
   // Start reading for button/axis events.
   return SDL_JoystickOpen(device_index);
}
API int joystick_id(SDL_Joystick* js) {
   // Return the id of the joystick.
   // Used in button/axis/removed events.
   return SDL_JoystickInstanceID(js);
}
API void close_joystick(SDL_Joystick *js) {
   // Close a joystick opened with open_joystick
   SDL_JoystickClose(js);
}

API void texture_color_mod(SDL_Texture* tex, Uint8 r, Uint8 g, Uint8 b, Uint8 a) {
   // Modulate the color and alpha of the texture when drawing to the screen.
   // Clear modulation with 255,255,255,255
   SDL_SetTextureColorMod(tex, r, g, b);
   SDL_SetTextureAlphaMod(tex, a);
}

API SDL_Texture* make_texture_from_pixels(int width, int height, const void* pixels) {
   // Create a new texture from an array of pixels.
   // Pixels are in 32-bit RGBA format.
   SDL_Texture* texture = SDL_CreateTexture(renderer, SDL_PIXELFORMAT_ABGR8888,
					    SDL_TEXTUREACCESS_STATIC, width, height);
   if (!texture) {
      fprintf(STDERR, "Failed to make a texture. %s\n", SDL_GetError());
   } else {
      if (SDL_SetTextureBlendMode(texture, SDL_BLENDMODE_BLEND)) {
	 fprintf(STDERR, "Failed to set the blendmode for the texture. %s\n", SDL_GetError());
      }
      if (SDL_UpdateTexture(texture, NULL, pixels, width*4)) {
	 fprintf(STDERR, "Failed to set the pixels for the texture. %s\n", SDL_GetError());
      }
   }
   return texture;
}

API SDL_Texture* make_texture(int width, int height) {
   // Create a 32-bit RGBA texture that can be rendered to
   // using render_to_texture.
   SDL_Texture* tex = SDL_CreateTexture(renderer, SDL_PIXELFORMAT_ABGR8888,
					SDL_TEXTUREACCESS_TARGET, width, height);
   if (!tex) {
      fprintf(STDERR, "Failed to make a texture. %s\n", SDL_GetError());
   }
   return tex;
}

API void render_to_texture(SDL_Texture* texture) {
   // Render to a texture created with make_texture()
   SDL_SetRenderTarget(renderer, texture);
}
API void render_to_window() {
   // Render to the screen. Use after render_to_texture()
   SDL_SetRenderTarget(renderer, NULL);
}

API void render_pixels_to_window(Uint8 *pixels) {
   void* dest;
   int pitch;
   SDL_LockTexture(window_pixels, NULL, &dest, &pitch);
   memcpy(dest, (void*)pixels, window_width*window_height*4);
   SDL_UnlockTexture(window_pixels);
   SDL_RenderCopy(renderer, window_pixels, NULL, NULL);
}

static int test_main() {
   if (!init("Test", 640, 480)) {
      quit();
      return 1;
   }

   if (open_audio(44100, 2, 2048)) {
      play_audio();
      int start_ticks = ticks();
      while (ticks() - start_ticks < 300) {
	 write_sine();
	 delay(15);
      }
      stop_audio();
      close_audio();
   }

   SDL_Texture* tex = load_bmp("/home/chebert/Projects/cave-story-content/MyChar.bmp");
   if (!tex) return -1;
   draw_color(0, 0, 0, 255);
   clear();
   draw_texture(tex, 32, 32, 32, 32, 100, 100, 32, 32, 0, 0);
   draw_color(255, 0, 0, 255);
   draw_rect(10, 10, 20, 30, 1);
   display();
   int q = 0;
   while (!q) {
      while (poll_event()) {
	 if (is_key_down()) {
	    printf("%s pressed\n", scancode_name(scancode()));
	 }
	 if (is_joy_down()) {
	    printf("joy button pressed\n");
	 }
	 if (is_key_down() && scancode() == scancode_from_name("escape")) {
	    q = 1;
	 }
      }
   }
   free_texture(tex);
   quit();
   return 0;
}

int main(int argc, char **argv) {
   //return test_ring_buffer();
   return test_main();
}
