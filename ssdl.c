// NOTE: Compile with g++ to avoid cannot connect to Mir server error.

#include <SDL2/SDL.h>
#include <assert.h>

static SDL_Window* window;
static SDL_Renderer* renderer;

void quit() {
	SDL_Quit();
}

int init(const char *title, int width, int height) {
	// Return true if successful, otherwise false and prints error to stderr.
	int success = 1;
	if (SDL_Init(SDL_INIT_EVERYTHING) != 0) {
		fprintf(stderr, "Failed to init: %s\n", SDL_GetError());
		success = 0;
	}
	window = SDL_CreateWindow(title, 100, 100, width, height, 0);
	if (!window) {
		fprintf(stderr, "Failed to create a window: %s\n", SDL_GetError());
		success = 0;
	}
	renderer = SDL_CreateRenderer(window, -1, 0);
	if (!renderer) {
		fprintf(stderr, "Failed to create a renderer: %s\n", SDL_GetError());
		success = 0;
	}

	SDL_ShowCursor(SDL_DISABLE);
	SDL_SetRenderDrawBlendMode(renderer, SDL_BLENDMODE_BLEND);
	return success;
}

void flip() {
	// Flip the back buffer.
	SDL_RenderPresent(renderer);
}

void clear() {
	// Clear the screen to the render draw color
	SDL_RenderClear(renderer);
}

void draw_rect(int x, int y, int w, int h, int filled) {
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

SDL_Texture* load_bmp(const char* path) {
	// Load a BMP from path using magenta as the color key.
	SDL_Surface* surf = SDL_LoadBMP(path);
	if (!surf) return NULL;
	SDL_SetColorKey(surf, 1, SDL_MapRGB(surf->format, 255, 0, 255));
	SDL_Texture* tex = SDL_CreateTextureFromSurface(renderer, surf);
	return tex;
}

SDL_RendererFlip flip_none() { return SDL_FLIP_NONE; }
SDL_RendererFlip flip_horizontal() { return SDL_FLIP_HORIZONTAL; }
SDL_RendererFlip flip_vertical() { return SDL_FLIP_VERTICAL; }

void draw_texture(SDL_Texture* tex,
		int sx, int sy, int sw, int sh,
		int dx, int dy, int dw, int dh,
		double angle,
		int px, int py,
		SDL_RendererFlip flip) {
	// Draw the texture from the source rect to the dest rect.
	SDL_Rect src, dst;
	SDL_Point pivot;
	src.x = sx; src.y = sy; src.w = sw, src.h = sh;
	dst.x = dx; dst.y = dy; dst.w = dw, dst.h = dh;
	pivot.x = px; pivot.y = py;

	SDL_RenderCopyEx(renderer, tex, &src, &dst, angle, &pivot, flip);
}

void free_texture(SDL_Texture* tex) {
	SDL_DestroyTexture(tex);
}

void draw_color(unsigned char r, unsigned char g, unsigned char b, unsigned char a) {
	// Set the render draw color
	SDL_SetRenderDrawColor(renderer, r, g, b, a);
}

static SDL_Event event;

int poll_event() {
	// Poll the next event. True if there are pending events.
	return SDL_PollEvent(&event);
}

int is_key_down() { return event.type == SDL_KEYDOWN; }
int is_key_up() { return event.type == SDL_KEYUP; }

int is_repeat() { return event.key.repeat; }
SDL_Scancode scancode() { return event.key.keysym.scancode; }
int is_no_mods() { return !event.key.keysym.mod; }
int is_lshift() { return KMOD_LSHIFT & event.key.keysym.mod; }
int is_rshift() { return KMOD_RSHIFT & event.key.keysym.mod; }
int is_lctrl() { return KMOD_LCTRL & event.key.keysym.mod; }
int is_rctrl() { return KMOD_RCTRL & event.key.keysym.mod; }
int is_lalt() { return KMOD_LALT & event.key.keysym.mod; }
int is_ralt() { return KMOD_RALT & event.key.keysym.mod; }
int is_num() { return KMOD_NUM & event.key.keysym.mod; }
int is_caps() { return KMOD_CAPS & event.key.keysym.mod; }

int is_mouse_motion() { return event.type == SDL_MOUSEMOTION; }
int is_mouse_button_down() { return event.type == SDL_MOUSEBUTTONDOWN; }
int is_mouse_button_up() { return event.type == SDL_MOUSEBUTTONUP; }
int is_lmb() { return event.button.button == SDL_BUTTON_LEFT; }
int is_mmb() { return event.button.button == SDL_BUTTON_MIDDLE; }
int is_rmb() { return event.button.button == SDL_BUTTON_RIGHT; }
int mouse_x() {
	if (event.type == SDL_MOUSEMOTION) {
		return event.motion.x;
	} else {
		return event.button.x;
	}
}
int mouse_y() {
	if (event.type == SDL_MOUSEMOTION) {
		return event.motion.y;
	} else {
		return event.button.y;
	}
}

int is_joy_added() { return event.type == SDL_JOYDEVICEADDED; }
int is_joy_removed() { return event.type == SDL_JOYDEVICEREMOVED; }
int joy_id() {
	switch (event.type) {
		case SDL_JOYAXISMOTION: return event.jaxis.which;
		case SDL_JOYBUTTONDOWN:
		case SDL_JOYBUTTONUP: return event.jbutton.which;
		case SDL_JOYDEVICEADDED:
		case SDL_JOYDEVICEREMOVED: return event.jdevice.which;
	}
}
int is_joy_down() { return event.type == SDL_JOYBUTTONDOWN; }
int is_joy_up() { return event.type == SDL_JOYBUTTONUP; }
int joy_button() { return event.jbutton.button; }
int is_joy_axis() { return event.type == SDL_JOYAXISMOTION; }
int joy_axis() { return event.jaxis.axis; }
int joy_axis_value() { return event.jaxis.value; }

int is_quit() { return event.type == SDL_QUIT; }

// Structure that holds audio parameters.
static struct {
	SDL_AudioFormat format;
	int frequency, channels, samples;

	// Ring Buffer for queuing audio to be written by SDL's audio callback
	unsigned char *buffer;
	int start, end, size, capacity;
} audio;

int audio_available() {
	// Number of bytes that can be written to the ring buffer
	return audio.capacity - audio.size;
}

void write_audio(unsigned char* src, int n) {
	// Write n bytes of audio from src into the ring buffer.
	// Assume that a lock has been acquired.
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

void clear_audio() {
	// Clear any queued audio from the ring buffer.
	audio.start = 0;
	audio.end = 0;
	audio.size = 0;
}

static void init_audio(int bytes) {
	// Create a new audio buffer of length bytes
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

void stop_audio() {
	SDL_PauseAudio(1);
}
void play_audio() {
	SDL_PauseAudio(0);
}

int open_audio(SDL_AudioFormat fmt, int freq, int num_channels, int num_samples, int buffer_bytes) {
	// Open an audio device with the provided parameters,
	// and create the audio buffer of size buffer_bytes.
	// Return true if successful.
	int success = 1;
	SDL_AudioSpec desired, obtained;
	desired.format = audio.format = fmt;
	desired.freq = audio.frequency = freq;
	desired.channels = audio.channels = num_channels;
	desired.samples = audio.samples = num_samples;
	desired.callback = audio_callback;

	if (SDL_OpenAudio(&desired, &obtained) != 0) {
		success = 0;
		fprintf(stderr, "Failed to open audio: %s\n", SDL_GetError());
	}
	if (obtained.format != desired.format) {
		success = 0;
		fprintf(stderr, "Failed to obtain desired audio spec: %s\n", SDL_GetError());
	}

	init_audio(buffer_bytes);
	return success;
}
void close_audio () {
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

int ticks() {
	return SDL_GetTicks();
}

void delay(int ms) { SDL_Delay(ms); }
const char* scancode_name(SDL_Scancode sc) { return SDL_GetScancodeName(sc); }
SDL_Scancode scancode_from_name(const char* str) { return SDL_GetScancodeFromName(str); }

SDL_Joystick* open_joystick(int device_index) {
	return SDL_JoystickOpen(device_index);
}
void close_joystick(SDL_Joystick *js) {
	SDL_JoystickClose(js);
}

static int test_main() {
	if (!init("Test", 640, 480)) {
		quit();
		return 1;
	}

	if (open_audio(AUDIO_S16, 44100, 2, 2048, 2048*2*2)) {
		play_audio();
		int start_ticks = ticks();
		while (ticks() - start_ticks < 3000) {
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
	draw_texture(tex, 32, 32, 32, 32, 100, 100, 32, 32, 0.0, 0, 0, flip_none());
	draw_color(255, 0, 0, 255);
	draw_rect(10, 10, 20, 30, 1);
	flip();
	int q = 0;
	while (!q) {
		while (poll_event()) {
			if (is_key_down()) {
				printf("%s pressed\n", scancode_name(scancode()));
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
