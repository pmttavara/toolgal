
// TOP

#define UNICODE
#define STRICT
// #define WIN32_LEAN_AND_MEAN
// #define VC_EXTRALEAN
#define NOGDICAPMASKS
#define NOVIRTUALKEYCODES
#define NOWINMESSAGES
#define NOWINSTYLES
#define NOSYSMETRICS
#define NOMENUS
#define NOICONS
#define NOKEYSTATES
#define NOSYSCOMMANDS
#define NORASTEROPS
#define NOSHOWWINDOW
#define OEMRESOURCE
#define NOATOM
#define NOCLIPBOARD
#define NOCOLOR
// #define NOCTLMGR
#define NODRAWTEXT
// #define NOGDI
#define NOKERNEL
// #define NOUSER
#define NONLS
// #define NOMB
#define NOMEMMGR
#define NOMETAFILE
#define NOMINMAX
// #define NOMSG
#define NOOPENFILE
#define NOSCROLL
#define NOSERVICE
#define NOSOUND
#define NOTEXTMETRIC
#define NOWH
#define NOWINOFFSETS
#define NOCOMM
#define NOKANJI
#define NOHELP
#define NOPROFILER
#define NODEFERWINDOWPOS
#define NOMCX

struct IUnknown; // dumb "Windows Kits 8.1" garbage: errors on /permissive-

#define Rectangle WindowsRectangle
#include <windows.h>
#undef Rectangle

#include "good_assert.h"
#ifdef _WIN32
#undef assert
#define assert(expr) good_assert(expr)
#endif

#define STRINGIZE_(x) #x
#define STRINGIZE(x) STRINGIZE_(x)

#define STR_VERSION_ "0.92"

#ifdef NDEBUG
#define STR_VERSION STR_VERSION_
#else
#define STR_VERSION STR_VERSION_ " DEBUG"
#endif

// #pragma warning(disable : 4081)

#define NG_DEFINE
#define NG_NO_CRT
#include "ng.hpp"

#define countof(a) (sizeof(a) / sizeof 0[(a)])
// #define bitcast ng_bitcast
#define cast(T) (T)
#define For(array) for (auto &&it : array)

#ifndef defer
struct defer_dummy {};
template <class F> struct deferrer { F f; ~deferrer() { f(); } };
template <class F> deferrer<F> operator*(defer_dummy, F f) { return {f}; }
#define DEFER_(LINE) zz_defer##LINE
#define DEFER(LINE) DEFER_(LINE)
#define defer auto DEFER(__LINE__) = defer_dummy{} *[&]()
#endif // defer

//#ifndef ctx
//typedef const struct Context &context, &ctx;
//inline ctx operator&(ctx c, ctx) { return c; }
//template <class F> void operator->*(ctx c, F &&f) { f(c); }
//#define push_context(c) ctx{(c)}->*[&](ctx context)
//#define ctx(...) (context & context, ##__VA_ARGS__)
//#endif // ctx

using namespace ng::int_types;
using ng::operator""_s;

#define SDL_MAIN_HANDLED
#include "SDL2.h"

#define STBTT_ifloor(x) (cast(int) ng::floor(x))
#define STBTT_iceil(x) (cast(int) ng::ceil(x))
#define STBTT_sqrt ng::sqrt
#define STBTT_pow ng::pow
#define STBTT_cos ng::cos
#define STBTT_acos ng::acos
#define STBTT_fabs ng::abs
#define STBTT_fmod ng::mod
#define STBTT_strlen ng::strlen
#define STBTT_memcpy ng::memcpy
#define STBTT_memset ng::memset
#define STBTT_assert ng_assert

#define STBTT_malloc STBTT_malloc
#define STBTT_free STBTT_free
void *STBTT_malloc(u64 size, void *userdata);
void STBTT_free(void *block, void *userdata);

#define stbtt_uint8 u8
#define stbtt_int8 s8
#define stbtt_uint16 u16
#define stbtt_int16 s16
#define stbtt_uint32 u32
#define stbtt_int32 s32
#define STB_TRUETYPE_IMPLEMENTATION
#include "stb_truetype.h"

#define STBI_ASSERT ng_assert

#define STBI_MALLOC ng::malloc
#define STBI_FREE ng::free
#define STBI_REALLOC ng::realloc

#define STBI_ONLY_JPEG
#define STBI_ONLY_PNG
#define STBI_ONLY_BMP
#define STBI_ONLY_GIF

#define STBI_NO_STDIO
#define STB_IMAGE_STATIC
#define STB_IMAGE_IMPLEMENTATION

#define STBI_memcpy ng::memcpy
#define STBI_memset ng::memset
#define STBI_abs ng::abs
#define STBI_HAS_LROTL 0

#include "stb_image.h"
#include "utf8.h"

// :CodeInAssert
// I don't think I can uncomment the following lines of code because good_assert works like regular assert, which is different in important ways from the way that my custom "ng_assert" works. (Namely, the fact that release mode compiles out the entire good_assert expression, whereas release mode merely executes my assert's expression without checking its value.)
#undef ng_assert
// #define ng_assert(expr) good_assert(expr)
// #define ng_assert(expr, ...) good_assert(expr) // good_assert((expr) || ("" __VA_ARGS__, 0))

bool isdigit(u8 c) { return c >= '0' && c <= '9'; }
bool isalpha(u8 c) {
    return (c >= 'A' && c <= 'Z') || (c >= 'a' && c <= 'z');
}
bool isalnum(u8 c) { return isdigit(c) || isalpha(c); }

#define FatalDialog(msg) do { auto msgbox_result = SDL_ShowSimpleMessageBox(SDL_MESSAGEBOX_ERROR, "Fatal Error", msg, nullptr); ng::print("Fatal error: %"_s, msg); ng::exit(1); } while (0)
#define OutOfMem() FatalDialog("The program ran out of memory.")

// #define default_bad break; default: ng_assert(false, "Invalid case value"); break
#define default_bad break; default: assert(!"Invalid case value"); break

void string_buffer_append(ng::array<u8> *buf, ng::string s) {
    auto success = buf->amortize(buf->count + s.len);
    if (!success) OutOfMem(); // @Temporary: i guess should be return false instead
    ng::memcpy(buf->data + buf->count, s.ptr, s.len);
    buf->count += s.len;
}

void string_buffer_prepend(ng::array<u8> *buf, ng::string s) {
    auto success = buf->amortize(buf->count + s.len);
    if (!success) OutOfMem(); // @Temporary: i guess should be return false instead
    ng::memmove(buf->data + s.len, buf->data, buf->count);
    ng::memcpy(buf->data, s.ptr, s.len);
    buf->count += s.len;
}

ng::string string_buffer_to_string(const ng::array<u8> *buf) { return {buf->count, buf->data}; }

int get_system_timezone_offset() {
    TIME_ZONE_INFORMATION tzi = {};
    int off = 0;
    if (GetTimeZoneInformation(&tzi) == TIME_ZONE_ID_DAYLIGHT) off += 60;
    off += -tzi.Bias;
    return off;
}

struct Time {
    int timezone; // in minutes, e.g. GMT-5 (DST) means timezone = (-5 + 1) * 60 = -240
    
    int hour, minute, second;
    int year;
    int month; // ZERO-indexed. January == 0.
    int day, day_of_week, day_of_year; // ZERO-indexed. 1st/Monday == 0.
};

struct Time decompose_unix_timestamp_gregorian(long long unix, int timezone_offset_minutes) {
    struct Time result = {0};
    
    static const char days_in_month[] = {31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31};
    
    int second_of_day = (unix % 86400 + 86400) % 86400; // positive modulo
    result.hour = second_of_day / 3600;
    result.minute = second_of_day / 60 % 60;
    result.second = second_of_day % 60;
    
    long long days = unix / 86400 + 134774; // move epoch to Mon, Jan 1, 1601
    
    long long unleaped_days = days - days / 1461 + days / 36524 - days / 146097;
    result.year = unleaped_days / 365 + 1601;
    result.day_of_year = unleaped_days % 365;
    
    int leap_year = !(result.year % 4) && result.year % 100 || !(result.year % 400);
    result.day = result.day_of_year;
    if (result.day >= 31) {
        result.day -= 31;
        int feb_len = 28 + (leap_year ? 1 : 0);
        if (result.day < feb_len) {
            result.month = 1;
        } else {
            result.day -= feb_len;
            result.month = 2;
            while (result.day >= days_in_month[result.month]) {
                result.day -= days_in_month[result.month];
                result.month += 1;
            }
        }
    }
    
    result.timezone += timezone_offset_minutes;
    
    if (result.timezone) {
        result.minute += result.timezone;
        result.hour += result.minute / 60 - (result.minute < 0);
        result.minute = (result.minute % 60 + 60) % 60;
        
        if (result.hour >= 24) {
            result.day += 1;
            result.day_of_week += 1;
        }
        if (result.hour < 0) {
            result.day -= 1;
            result.day_of_week -= 1;
        }
        result.hour = (result.hour % 24 + 24) % 24;
        
        int leap = !(result.year % 4) && result.year % 100 || !(result.year % 400);
        
        if (result.day > days_in_month[result.month] + (leap && result.month == 1)) {
            result.month += 1;
            int diff = result.day - days_in_month[result.month];
            result.day -= diff;
            result.day_of_week -= diff;
        }
        if (result.day < 0) {
            result.month -= 1;
            int diff = -result.day + days_in_month[result.month] + (leap && result.month == 1) - 1;
            result.day += diff;
            result.day_of_week += diff;
        }
    }
    
    result.day_of_week += days;
    result.day_of_week = (result.day_of_week % 7 + 7) % 7; // 0-6 -> Mon-Sun
    
    return result;
}

void *temporary_allocator_proc(ng::allocate_mode mode, ng::allocator *self, u64 size, u64 old_size, void *old_block, s64 options);
struct Temporary_Allocator : ng::allocator {
    u8 data[4096 * 16];
    
    u64 mark = 0;
    u64 water_mark = 0;
    
    int relocations = 0;
    int expansions = 0;
    
    Temporary_Allocator() : ng::allocator{temporary_allocator_proc} {}
    u64 get_mark() {
        return mark;
    }
    void set_mark(u64 new_mark) {
        assert(new_mark < sizeof(data));
        if (new_mark < sizeof(data)) {
            mark = new_mark;
        }
    }
    
    void *call(ng::allocate_mode mode, u64 size, u64 old_size, void *old_block) {
        defer {
            assert(mark % 16 == 0);
            water_mark = ng_max(water_mark, mark);
        };
        
        auto good_size = (size + 15ull) & ~15ull;
        auto good_old_size = (old_size + 15ull) & ~15ull;
        
        if (mode == ng::allocate_mode::resize) {
            bool relocate = true;
            
            if (cast(u8 *) old_block + good_old_size == data + mark) {
                relocate = false; // this was the most recent allocation
            }
            
            if (!old_block) {
                relocate = true;
            }
            
            if (relocate) { // allocate new block and copy over
                
                if (mark + good_size > sizeof(data)) {
                    return nullptr; // request is too big
                }
                
                auto result = &data[mark];
                
                if (old_block && old_size) {
                    ng::memcpy(result, old_block, old_size);
                }
                
                mark += good_size;
                
                relocations += 1;
                
                return result;
                
            }
            
            // expand the old block and return it
            
            auto delta = good_size - good_old_size;
            
            if (mark + delta > sizeof(data)) {
                return nullptr; // request is too big
            }
            
            expansions += 1;
            
            mark += delta;
            return old_block;
        }
        
        if (mode == ng::allocate_mode::allocate) {
            
            if (mark + good_size > sizeof(data)) {
                return nullptr; // request is too big
            }
            
            auto result = &data[mark];
            
            mark += good_size;
            
            return result;
        }
        
        if (mode == ng::allocate_mode::free_all) {
            mark = 0;
            ng::memset(data, 0xcc, sizeof(data));
        }
        return nullptr;
    }
};
void *temporary_allocator_proc(ng::allocate_mode mode, ng::allocator *self, u64 size, u64 old_size, void *old_block, s64 options) {
    assert(self);
    if (!self) return nullptr;
    auto me = cast(Temporary_Allocator *) self;
    return me->call(mode, size, old_size, old_block);
}


enum struct Read_Entire_File_Result {
    SUCCESSFUL,
    NONEXISTENT,
    READ_ERROR,
    ALLOCATION_FAIL,
};

Read_Entire_File_Result read_entire_file(ng::string file_path, ng::string *result, ng::allocator *alloc, Temporary_Allocator *temp_alloc) {
    assert(result);
    assert(alloc);
    assert(temp_alloc);
    if (!result || !alloc || !temp_alloc) return Read_Entire_File_Result::NONEXISTENT;
    
    *result = {};
    
    SDL_RWops *rw = nullptr;
    {
        auto mark = temp_alloc->get_mark();
        defer { temp_alloc->set_mark(mark); };
        
        auto c_str = ng::c_str(file_path, temp_alloc);
        if (!c_str) return Read_Entire_File_Result::ALLOCATION_FAIL;
        
        rw = SDL_RWFromFile(cast(char *) c_str.ptr, "rb");
    }
    if (!rw) return Read_Entire_File_Result::NONEXISTENT;
    defer { SDL_RWclose(rw); };
    
    s64 filesize = SDL_RWsize(rw);
    if (filesize < 0) return Read_Entire_File_Result::READ_ERROR;
    
    // Avoid allocations via early-out if the file is empty.
    if (filesize == 0) return Read_Entire_File_Result::SUCCESSFUL;
    
    result->len = cast(u64) filesize;
    result->ptr = cast(u8 *) alloc->allocate(filesize);
    if (!result->ptr) {
        *result = {};
        return Read_Entire_File_Result::ALLOCATION_FAIL;
    }
    
    auto read_result = SDL_RWread(rw, result->ptr, 1, filesize);
    if (read_result != cast(u64) filesize) {
        ng::free_string(result, alloc);
        return Read_Entire_File_Result::READ_ERROR;
    }
    
    return Read_Entire_File_Result::SUCCESSFUL;
}

enum struct Write_Entire_File_Result {
    SUCCESSFUL,
    OPEN_ERROR,
    WRITE_ERROR,
    
};

Write_Entire_File_Result write_entire_file(ng::string file_path, ng::string file, Temporary_Allocator *temp_alloc) {
    assert(temp_alloc);
    if (!temp_alloc) return Write_Entire_File_Result::OPEN_ERROR;
    
    SDL_RWops *rw = nullptr;
    {
        auto mark = temp_alloc->get_mark();
        defer { temp_alloc->set_mark(mark); };
        
        auto c_str = ng::c_str(file_path, temp_alloc);
        assert(c_str);
        if (!c_str) OutOfMem();
        
        rw = SDL_RWFromFile(cast(char *) c_str.ptr, "wb");
    }
    if (!rw) return Write_Entire_File_Result::OPEN_ERROR;
    defer { SDL_RWclose(rw); };
    
    auto read_result = SDL_RWwrite(rw, file.ptr, 1, file.len);
    if (file.len < 0 || read_result != cast(u64) file.len) return Write_Entire_File_Result::WRITE_ERROR;
    
    return Write_Entire_File_Result::SUCCESSFUL;
}

inline f32 safe_ratio(f32 a, f32 b) {
    if (a == 0) return 0;
    if (b == 0) return 1;
    return a / b;
}

#define initializer_list(...) (initializer_list_make(__VA_ARGS__))
template <class Elem, int N> struct initializer_list {
    using T = Elem;
    T e[N];
    using array_ref_type = T (&)[N];
    operator array_ref_type() { return e; }
    T       *begin()       { return e    ; }
    T const *begin() const { return e    ; }
    T       *end()         { return e + N; }
    T const *end()   const { return e + N; }
    T &operator[](sptr n) {
        assert(n < N);
        return e[n];
    }
};
template <class T, class... Ts>
initializer_list<T, sizeof...(Ts) + 1> initializer_list_make(const T &t, const Ts &... ts) { return {t, ts...}; }

#define Use (void)

void *STBTT_malloc(u64 size, void *userdata) {
    assert(userdata);
    if (!userdata) return nullptr;
    
    auto raw_size = size + 16; // store size alongsize data
    auto alloc = cast(ng::allocator *) userdata;
    auto raw_block = cast(u64 *) alloc->allocate(raw_size);
    if (!raw_block) {
        // @Todo: raw_block = cast(u64 *) ng::default_allocator->allocate(raw_size);
        OutOfMem();
    }
    *raw_block = raw_size;
    
    return raw_block + 1;
}
void STBTT_free(void *block, void *userdata) {
    assert(userdata);
    if (!userdata) return;
    auto raw_block = (cast(u64 *) block) - 1;
    auto alloc = cast(ng::allocator *) userdata;
    alloc->free(raw_block, *raw_block);
}

struct Vector2 {
    float x;
    float y;
    
    Vector2() = default; Vector2(float x, float y) : x{x}, y{y} {}
    
    Vector2 &operator+=(Vector2 v) { return x += v.x, y += v.y, *this; }
    Vector2 &operator-=(Vector2 v) { return x -= v.x, y -= v.y, *this; }
    Vector2 &operator*=(float v) { return x *= v, y *= v, *this; }
    Vector2 &operator/=(float v) { return x /= v, y /= v, *this; }
    float magsq() const { return x * x + y * y; }
    float mag() const { return ng::sqrt(magsq()); }
    Vector2 hat() const {
        Vector2 result;
        auto m = magsq();
        if (m != 0.0f) {
            m = ng::rsqrt(m);
        }
        result.x = x * m;
        result.y = y * m;
        return result;
    }
    Vector2 perp() const { return {-y, x}; }
    Vector2 operator-() const { return {-x, -y}; }
};
typedef Vector2 v2;

Vector2 operator+(Vector2 a, Vector2 b) { return {a.x + b.x, a.y + b.y}; }
Vector2 operator-(Vector2 a, Vector2 b) { return {a.x - b.x, a.y - b.y}; }
Vector2 operator*(Vector2 v, float f) { return {v.x * f, v.y * f}; }
Vector2 operator/(Vector2 v, float f) { return {v.x / f, v.y / f}; }
Vector2 operator*(float f, Vector2 v) { return {v.x * f, v.y * f}; }
Vector2 operator/(float f, Vector2 v) { return {v.x / f, v.y / f}; }
bool operator==(Vector2 a, Vector2 b) { return a.x == b.x && a.y == b.y; }
bool operator!=(Vector2 a, Vector2 b) { return a.x != b.x || a.y != b.y; }
float dot(Vector2 l, Vector2 r) { return l.x * r.x + l.y * r.y; }
// proj_u(v) = (v . u / u . u) * u
Vector2 proj(Vector2 v, Vector2 u) { return dot(u, v) / u.magsq() * u; }

union Rectangle {
    struct {
        float x;
        float y;
        float w;
        float h;
    };
    struct {
        Vector2 pos;
        Vector2 size;
    };
    constexpr Rectangle() = default;
    constexpr Rectangle(float x, float y, float w, float h) : x{x}, y{y}, w{w}, h{h} {}
    constexpr Rectangle(Vector2 pos, Vector2 size) : pos{pos}, size{size} {}
};
typedef Rectangle rect;

Rectangle rect_lbrt(float left, float bottom, float right, float top) {
    Rectangle result;
    result.x = left;
    result.y = bottom;
    result.w = right - left;
    result.h = top - bottom;
    return result;
}

bool rect_empty(Rectangle r) { return (r.w <= 0 || r.h <= 0); }

Vector2 rect_center(Rectangle r) { return r.pos + r.size / 2; }

bool rect_intersect(Rectangle a, Rectangle b) {
    bool horizontal = (a.x < b.x + b.w && a.x + a.w > b.x);
    bool vertical = (a.y < b.y + b.h && a.y + a.h > b.y);
    return horizontal && vertical;
}

Rectangle rect_centered(float w, float h) {
    Rectangle result;
    result.w = w;
    result.h = h;
    result.x = w * -0.5f;
    result.y = h * -0.5f;
    return result;
}

Rectangle offset_rect(Rectangle r, Vector2 v) {
    r.x += v.x;
    r.y += v.y;
    return r;
}

Rectangle rect_of_rect(Rectangle source, Rectangle selector) {
    Rectangle result;
    result.x = source.x + selector.x * source.w;
    result.y = source.y + selector.y * source.h;
    result.w = selector.w * source.w;
    result.h = selector.h * source.h;
    return result;
}

Vector2 v2_of_rect(Rectangle source, Vector2 selector) {
    Vector2 result;
    result.x = source.x + selector.x * source.w;
    result.y = source.y + selector.y * source.h;
    return result;
}

SDL_Rect rect_to_sdl(Rectangle r) {
    SDL_Rect result;
    result.x = ng::round(r.x);
    result.y = ng::round(r.y);
    result.w = ng::round(r.w);
    result.h = ng::round(r.h);
    return result;
}

struct Render_Rect {
    Rectangle rect = {};
    SDL_Color color = {};
};

Rectangle expand_about_center(Rectangle rect, float amount) {
    rect.x -= amount;
    rect.y -= amount;
    rect.w += amount * 2;
    rect.h += amount * 2;
    return rect;
}


// float rand_range(u32 *rng, float min, float max) {
//     assert(rng);
//     auto range = max - min;
//     rng += 1;
//     return ng::rand_f32(*rng) * range + min;
// }

Vector2 v2_world_to_screen(Vector2 v, Vector2 screen_size) {
    // scaling
    v.x *= screen_size.x;
    v.y *= screen_size.y;
    
    // cartesian to screen
    v.y = -v.y;
    v.y += screen_size.y;
    return v;
}
Vector2 v2_screen_to_world(Vector2 v, Vector2 screen_size) {
    // screen to cartesian
    v.y -= screen_size.y;
    v.y = -v.y;
    
    // scaling
    v.x /= screen_size.x;
    v.y /= screen_size.y;
    return v;
}

Rectangle rect_world_to_screen(Rectangle r, Vector2 screen_size) {
    r.pos = v2_world_to_screen(r.pos, screen_size);
    r.w *= screen_size.x;
    r.h *= screen_size.y;
    
    r.y -= r.h;
    return r;
}

Rectangle rect_screen_to_world(Rectangle r, Vector2 screen_size) {
    r.y += r.h;
    
    r.w /= screen_size.x;
    r.h /= screen_size.y;
    r.pos = v2_screen_to_world(r.pos, screen_size);
    
    return r;
}

bool v2_in_rect(Vector2 v, Rectangle r) {
    if (v.x < r.x || v.x > r.x + r.w) return false;
    if (v.y < r.y || v.y > r.y + r.h) return false;
    return true;
}

struct Glyph_Id {
    u32 codepoint = 0;
    float scale = 0;
};

struct Glyph {
    SDL_Texture *texture = nullptr;
    int bitmap_w = 0;
    int bitmap_h = 0;
    int off_x = 0;
    int off_y = 0;
    int x_pre_advance = 0;
    int x_total_advance = 0;
};

struct Font_Data {
    ng::map<Glyph_Id, Glyph> glyphs = {};
    
    stbtt_fontinfo info = {};
    SDL_Color palette[256] = {};
    int ascent = 0;
    int descent = 0;
    int line_gap = 0;
    
    // useful info
    int line_advance = 0;
    int space_width = 0;
};

struct Input_State {
    Vector2 mouse_pos = {};
    bool lmb = false;
    bool rmb = false;
    bool lmb_down = false;
    bool rmb_down = false;
    bool lmb_up = false;
    bool rmb_up = false;
    
    bool keys[SDL_NUM_SCANCODES] = {};
    u8 keys_down[SDL_NUM_SCANCODES] = {};
    bool keys_up[SDL_NUM_SCANCODES] = {};
    SDL_Keymod keymod = KMOD_NONE;
};

#define DEFAULT_SCREEN_SIZE (v2(640, 480))

struct Text_Input_State {
    // state
    int cursor;
    int selection_cursor;
    
    // cursor visuals
    float blink_timer;
    float hot_timer;
    float cursor_eased_pos;
    float cursor_eased_width;
    float backspace_timer; // backspace moves faster
    
    // reset per frame
    bool changed;
    bool backspaced;
    bool cursor_moved;
};

struct Ui_State {
    ng::allocator *main_allocator = nullptr;
    Temporary_Allocator *temp_allocator = nullptr;
    
    SDL_Renderer *renderer = nullptr;
    Font_Data font_data = {};
    Input_State input_state = {};
    Vector2 screen_size = {};
    Rectangle clip_rect = {};
    
    Text_Input_State tis = {};
    
    bool obscured = false; // Set this when you want to render elements that you know you'll render overtop later.
    
    float *dragging_scrollbar = nullptr;
    f32 drag_offset = {};
    
    f64 t = 0; // kind of a @Hack.
    
    ng::array<u8> tooltip = {};
    bool tooltip_assigned = false;
    
    SDL_Texture *tooltip_image = {};
    
    float tooltip_timer = 0;
#define TOOLTIP_BEGIN (0.5f)
#define TOOLTIP_END (1.0f)
};

float tooltip_alpha(float x) {
    auto ease = [](float x) { return (3 - 2 * x) * x * x; };
    return ease((x - TOOLTIP_BEGIN) / (TOOLTIP_END - TOOLTIP_BEGIN));
}

int ui_get_current_point_height(Ui_State *ui) {
    assert(ui);
    if (!ui) return 0;
    
    auto x_ratio = ui->screen_size.x / DEFAULT_SCREEN_SIZE.x;
    auto y_ratio = ui->screen_size.y / DEFAULT_SCREEN_SIZE.y;
    x_ratio *= 32;
    y_ratio *= 24;
    return cast(int) ng::max(ng::min(x_ratio, y_ratio), 1);
}

float ui_get_current_font_scale(Ui_State *ui) {
    assert(ui);
    if (!ui) return 0;
    
    return stbtt_ScaleForPixelHeight(&ui->font_data.info, ui_get_current_point_height(ui));
}

void ui_update_input(Ui_State *ui, Input_State input_state) {
    assert(ui);
    if (!ui) return;
    
    ui->input_state = input_state;
}

SDL_Rect ui_push_clip(Ui_State *ui) {
    assert(ui);
    if (!ui) return {};
    
    bool old_clip_rect_enabled = SDL_RenderIsClipEnabled(ui->renderer);
    SDL_Rect old_clip_rect = {};
    if (old_clip_rect_enabled) SDL_RenderGetClipRect(ui->renderer, &old_clip_rect);
    
    bool is_using_clip_rect = ui->clip_rect.w > 0 && ui->clip_rect.h > 0;
    if (is_using_clip_rect) {
        auto clip_rect = rect_to_sdl(rect_world_to_screen(ui->clip_rect, ui->screen_size));
        SDL_RenderSetClipRect(ui->renderer, &clip_rect);
    }
    
    return old_clip_rect;
}
void ui_pop_clip(Ui_State *ui, SDL_Rect old_clip_rect) {
    assert(ui);
    if (!ui) return;
    
    if (!SDL_RectEmpty(&old_clip_rect)) {
        SDL_RenderSetClipRect(ui->renderer, &old_clip_rect);
    } else {
        SDL_RenderSetClipRect(ui->renderer, nullptr);
    }
}

bool ui_is_clipping(Ui_State *ui) {
    assert(ui);
    if (!ui) return false;
    
    return !rect_empty(ui->clip_rect);
}

void ui_set_tooltip(Ui_State *ui, ng::string tooltip, SDL_Texture *tooltip_image = nullptr) {
    assert(ui);
    if (!ui) return;
    
    if (ui->obscured) return;
    
    if (!tooltip) return;
    
    ui->tooltip.clear();
    string_buffer_append(&ui->tooltip, tooltip);
    
    ui->tooltip_image = tooltip_image;
    
    ui->tooltip_assigned = true;
}

void ui_rect(Ui_State *ui, Rectangle rect, SDL_Color color) {
    assert(ui);
    if (!ui) return;
    
    auto old_clip_rect = ui_push_clip(ui);
    defer { ui_pop_clip(ui, old_clip_rect); };
    
    {
        SDL_Rect dest = rect_to_sdl(rect_world_to_screen(rect, ui->screen_size));
        SDL_SetRenderDrawColor(ui->renderer, color.r, color.g, color.b, color.a);
        
        SDL_RenderFillRect(ui->renderer, &dest);
    }
}

void ui_rect(Ui_State *ui, Rectangle rect, float alpha) {
    alpha = ng_clamp(alpha, 0, 1);
    ui_rect(ui, rect, SDL_Color{0, 0, 0, cast(u8)(alpha * 255)});
}

Glyph_Id glyph_id_from_scale(Ui_State *ui, u32 codepoint, float scale) {
    assert(ui);
    if (!ui) return {};
    
    Glyph_Id result = {};
    result.codepoint = codepoint;
    result.scale = scale;
    Use ui;
    return result;
}
Glyph_Id glyph_id_from_current_scale(Ui_State *ui, u32 codepoint) {
    assert(ui);
    if (!ui) return {};
    
    return glyph_id_from_scale(ui, codepoint, ui_get_current_font_scale(ui));
}

namespace ng {
    u64 get_hash(Glyph_Id &id) {
        return get_hash(id.codepoint) ^ get_hash(id.scale);
    }
}
bool operator==(const Glyph_Id &a, const Glyph_Id &b) {
    return a.codepoint == b.codepoint && a.scale == b.scale;
}

Glyph create_glyph(SDL_Renderer *renderer, Font_Data *font_data, Glyph_Id id) {
    assert(renderer);
    assert(font_data);
    if (!renderer || !font_data) return {};
    
    auto temp_alloc = (cast(Temporary_Allocator *) font_data->info.userdata);
    auto mark = temp_alloc->get_mark(); // "Brute-force" clear memory.
    defer { temp_alloc->set_mark(mark); };
    
    Glyph result = {};
    auto scale = id.scale;
    auto c = id.codepoint;
    auto bitmap = stbtt_GetCodepointBitmap(&font_data->info, scale, scale, c, &result.bitmap_w,
                                           &result.bitmap_h, &result.off_x, &result.off_y);
    
    stbtt_GetCodepointHMetrics(&font_data->info, c, &result.x_total_advance, &result.x_pre_advance);
    
    if (!bitmap) return result;
    defer { stbtt_FreeBitmap(bitmap, font_data->info.userdata); };
    
    auto surf = SDL_CreateRGBSurfaceFrom(bitmap, result.bitmap_w, result.bitmap_h, 8,
                                         result.bitmap_w, 0, 0, 0, 0);
    assert(surf);
    if (!surf) return result;
    defer { SDL_FreeSurface(surf); };
    
    SDL_SetPaletteColors(surf->format->palette, font_data->palette, 0, 256);
    
    auto tex = SDL_CreateTextureFromSurface(renderer, surf);
    assert(tex);
    if (!tex) return result;
    
    SDL_SetTextureAlphaMod(tex, 255);
    SDL_SetTextureColorMod(tex, 255, 255, 255);
    SDL_SetTextureBlendMode(tex, SDL_BLENDMODE_BLEND);
    result.texture = tex;
    return result;
}

Glyph get_or_add_glyph(SDL_Renderer *renderer, Font_Data *font_data, Glyph_Id id) {
    assert(renderer);
    assert(font_data);
    if (!renderer || !font_data) return {};
    
    Glyph result = {};
    auto find_result = font_data->glyphs[id];
    if (find_result.found) {
        result = *find_result.value;
    } else {
        auto new_glyph = font_data->glyphs.insert(id, create_glyph(renderer, font_data, id));
        result = *new_glyph;
    }
    return result;
}
using Utf32 = u32;

struct Text_Size {
    Vector2 total_size = {};
    float x_cursor_on_last_line = 0;
};

Text_Size get_text_size(Font_Data *font_data, ng::string text, float scale = 1.0f) {
    assert(font_data);
    if (!font_data) return {};
    
    Text_Size result = {};
    
    float x_cursor = 0;
    float y_cursor = 0;
    
    auto p = text.begin();
    
    unsigned int decoder_state = 0;
    unsigned int codepoint = 0;
    For (text) {
        utf8_decode(&decoder_state, &codepoint, it);
        assert(decoder_state != utf8_decode_reject); // , "invalid utf8 string given");
        if (decoder_state == utf8_decode_reject) break;
        if (decoder_state != utf8_decode_accept) continue;
        
        if (codepoint == ' ') {
            x_cursor += font_data->space_width;
        } else if (codepoint == '\n') {
            y_cursor += font_data->line_advance;
            x_cursor = 0;
        } else {
            int x_total_advance = 0;
            stbtt_GetCodepointHMetrics(&font_data->info, codepoint, &x_total_advance, nullptr);
            
            x_cursor += x_total_advance;
        }
        if (result.total_size.x < x_cursor) {
            result.total_size.x = x_cursor;
        }
    }
    
    result.total_size.y = y_cursor;
    result.x_cursor_on_last_line = x_cursor;
    
    result.total_size.y += font_data->ascent;
    result.total_size.y -= font_data->descent;
    
    result.total_size *= scale;
    result.x_cursor_on_last_line *= scale;
    return result;
};

enum struct Text_Halign {
    Left,
    Center,
    Right
};
enum struct Text_Valign {
    Top,
    Center,
    Bottom
};

#define TEXT_ALIGN_FACTORS initializer_list((float) 0, 0.5f, 1.0f)

#define TEXT_COLOR SDL_Color{0, 0, 0, 255}
#define CURSOR_COLOR SDL_Color{0, 0, 0, 255}
#define CURSOR_HOT_COLOR SDL_Color{0xee, 0x77, 0x33, 255}
#define HIGHLIGHTED_BUTTON_COLOR SDL_Color{0xee, 0x77, 0x33, 255}

bool draw_grid;
bool draw_bounds;

Text_Size ui_text(Ui_State *ui, Vector2 pos, ng::string text, float alpha = 1.0f, float scale = 1.0f,
                  Text_Halign halign = Text_Halign::Center, Text_Valign valign = Text_Valign::Center, SDL_Color color = TEXT_COLOR, ng::string tooltip = ""_s) {
    assert(ui);
    if (!ui) return {};
    
    auto old_clip_rect = ui_push_clip(ui);
    defer { ui_pop_clip(ui, old_clip_rect); };
    
    auto renderer = ui->renderer;
    auto screen_size = ui->screen_size;
    auto font_data = &ui->font_data;
    auto glyphs = &font_data->glyphs; // @Cleanup
    
    pos = v2_world_to_screen(pos, screen_size);
    
    auto text_size = get_text_size(font_data, text);
    
    auto text_scale = ui_get_current_font_scale(ui) * scale;
    auto text_point_height = ui_get_current_point_height(ui) * scale;
    
    pos.x -= text_size.total_size.x * TEXT_ALIGN_FACTORS[cast(int) halign] * text_scale;
    pos.y -= text_size.total_size.y * TEXT_ALIGN_FACTORS[cast(int) valign] * text_scale;
    pos.y += font_data->ascent * text_scale;
    
    float x_draw_cursor = 0;
    float y_draw_cursor = 0;
    
    auto shadow_alpha = ng::lerp(0.0f, 0.125f, alpha); // bit of a @Hack
    auto shadow_color = color;
    shadow_color.a = shadow_alpha * 255;
    
    unsigned int decoder_state = 0;
    unsigned int codepoint = 0;
    For (text) { // @Cutnpaste from get_text_size
        utf8_decode(&decoder_state, &codepoint, it);
        assert(decoder_state != utf8_decode_reject); // , "invalid utf8 string given");
        if (decoder_state == utf8_decode_reject) break;
        if (decoder_state != utf8_decode_accept) continue;
        
        
        if (codepoint == ' ') {
            x_draw_cursor += font_data->space_width;
            continue;
        }
        if (codepoint == '\n') {
            y_draw_cursor += font_data->line_advance;
            x_draw_cursor = 0;
            continue;
        }
        
        auto glyph = get_or_add_glyph(renderer, font_data, glyph_id_from_scale(ui, codepoint, text_scale));
        
        Rectangle dest = {};
        
        dest.x = pos.x;
        dest.y = pos.y;
        
        dest.w = glyph.bitmap_w;
        dest.h = glyph.bitmap_h;
        
        dest.x += x_draw_cursor * text_scale;
        dest.x += glyph.off_x;
        // dest.x += glyph.x_pre_advance * font_data->scale * text_scale;
        dest.y += y_draw_cursor * text_scale;
        dest.y += glyph.off_y;
        
        auto draw_shadow = true;
        auto shadow_distance = cast(int)(text_point_height / 16.5);
        if (draw_shadow) {
            auto shadow_dest = dest;
            shadow_dest.x += shadow_distance;
            shadow_dest.y += shadow_distance;
            SDL_SetTextureColorMod(glyph.texture, shadow_color.r, shadow_color.g, shadow_color.b);
            SDL_SetTextureAlphaMod(glyph.texture, shadow_color.a);
            auto c_shadow_dest = rect_to_sdl(shadow_dest);
            SDL_RenderCopy(renderer, glyph.texture, nullptr, &c_shadow_dest);
        }
        
        SDL_SetTextureColorMod(glyph.texture, color.r, color.g, color.b);
        SDL_SetTextureAlphaMod(glyph.texture, color.a * alpha);
        auto c_dest = rect_to_sdl(dest);
        SDL_RenderCopy(renderer, glyph.texture, nullptr, &c_dest);
        
        x_draw_cursor += glyph.x_total_advance;
    }
    if (!ui->obscured && tooltip && !ui->dragging_scrollbar) {
        auto bounds = rect(pos, text_size.total_size * text_scale);
        bounds.y -= font_data->ascent * text_scale;
        bounds = rect_screen_to_world(bounds, ui->screen_size);
        
        bool mouse_is_in_rect = v2_in_rect(ui->input_state.mouse_pos, bounds) &&
            (!ui_is_clipping(ui) || v2_in_rect(ui->input_state.mouse_pos, ui->clip_rect));
        
        if (mouse_is_in_rect) {
            ui_set_tooltip(ui, tooltip);
        }
    }
    
    if (draw_bounds) { // @Debug
        SDL_SetRenderDrawColor(renderer, 255, 0, 0, 255);
        
        auto pos_in_screen = v2_world_to_screen(pos, ui->screen_size);
        SDL_RenderDrawPoint(renderer, pos_in_screen.x, pos_in_screen.y);
        
        SDL_Rect text_bounds = {};
        text_bounds.w = text_size.total_size.x * text_scale;
        text_bounds.h = text_size.total_size.y * text_scale;
        text_bounds.x = pos.x;
        text_bounds.y = pos.y - font_data->ascent * text_scale;
        SDL_RenderDrawRect(renderer, &text_bounds);
    }
    
    return text_size;
};

template <class T> T damp(T current, T target, float rate, float dt) {
    return ng::lerp(current, target, 1 - ng::pow(rate, dt));
}

inline float ease(float x) { // smoothesttep: -20x^7 + 70x^6 + -84x^5 + 35x^4
    if (x < 0) return 0;
    if (x > 1) return 1;
    return x * x * x * x * (x * (x * (-20 * x + 70) - 84.0) + 35);
}

SDL_Color lerp(SDL_Color a, SDL_Color b, float t) {
    SDL_Color result; // @Temporary: RGB lerp. @Todo: HSV lerp
    result.r = ng::lerp(a.r, b.r, t);
    result.g = ng::lerp(a.g, b.g, t);
    result.b = ng::lerp(a.b, b.b, t);
    result.a = ng::lerp(a.a, b.a, t);
    return result;
}

bool ui_key(Ui_State *ui, SDL_Scancode key, SDL_Keymod mod = KMOD_NONE) {
    if (ui->obscured) return false;
    
    if (key == SDL_SCANCODE_UNKNOWN) return false;
    if (mod == KMOD_NONE) return ui->input_state.keys[key];
    return ui->input_state.keys[key] && (ui->input_state.keymod & mod);
}
bool ui_keydown(Ui_State *ui, SDL_Scancode key, SDL_Keymod mod = KMOD_NONE, bool repeat = false) {
    if (ui->obscured) return false;
    
    if (key == SDL_SCANCODE_UNKNOWN) return false;
    
    if (mod != KMOD_NONE) {
        if ((ui->input_state.keymod & mod) == 0) return false;
    }
    
    if (repeat) {
        if (ui->input_state.keys_down[key]) return true;
    }
    
    if (ui->input_state.keys_down[key] == 1) return true;
    
    return false;
}

// Some flags override others.
const int BUT_OFF   = 0x01; // Draw the button disabled, return false.
const int BUT_DOWN  = 0x02; // Draw the button pressed, return false.
const int BUT_FLASH = 0x04; // Make the button flash.
const int BUT_RED   = 0x08;
const int BUT_WHITE = 0x10;

bool ui_button(Ui_State *ui, int flags, Rectangle button_rect, ng::string label, SDL_Color color = TEXT_COLOR, ng::string tooltip = ""_s) { // @Todo @Temporary: Don't make tooltip optional, and delete text color.
    assert(ui);
    if (!ui) return false;
    
    auto old_clip_rect = ui_push_clip(ui);
    defer { ui_pop_clip(ui, old_clip_rect); };
    
    bool hovering = v2_in_rect(ui->input_state.mouse_pos, button_rect) &&
        (!ui_is_clipping(ui) || v2_in_rect(ui->input_state.mouse_pos, ui->clip_rect));
    bool clicking = hovering && ui->input_state.lmb;
    bool clicked = hovering && ui->input_state.lmb_up;
    
    if (hovering && !ui->dragging_scrollbar && tooltip) {
        ui_set_tooltip(ui, tooltip);
    }
    
    if (ui->obscured) {
        flags |= BUT_OFF;
        
        hovering = false;
        clicking = false;
        clicked = false;
    }
    
    if (ui->dragging_scrollbar) {
        hovering = false;
        clicking = false;
        clicked = false;
    }
    
    if (flags & BUT_OFF) {
        hovering = false;
        clicking = false;
        clicked = false;
    }
    if (flags & BUT_DOWN) {
        clicking = true;
    }
    
    auto button_clip_rect = rect(0, 0, 1, 1);
    if (ui_is_clipping(ui)) button_clip_rect = ui->clip_rect;
    if (rect_intersect(button_rect, button_clip_rect)) { // render
        float button_grey = 0.8;
        float button_tl = 0.98;
        float button_br = 0.25;
        
        if (flags & BUT_OFF && !clicking) {
            button_grey = 0.7;
            button_tl = 0.9;
            button_br = 0.4;
        } else {
            if (clicking) {
                button_grey = 0.7;
                ng::swap(button_tl, button_br);
                
                color = lerp(color, SDL_Color{0, 0, 0, color.a}, 0.125f);
            } else if (hovering) {
                button_grey = 0.9;
                button_br = 0.4;
                
                color = lerp(color, SDL_Color{255, 255, 255, color.a}, 0.25f);
            }
        }
        
        auto getcolor = [](float g) {
            auto cg = cast(u8) ng::clamp(ng::round(g * 255.0f), 0.0f, 255.0f);
            return SDL_Color{cg, cg, cg, 255};
        };
        
        auto c = getcolor(button_grey);
        auto tl_c = getcolor(button_tl);
        auto br_c = getcolor(button_br);
        
        
        if (flags & BUT_FLASH) { // @Hack
            
            auto lerp_t = 0.875f;
            if (hovering) lerp_t = 0.5f;
            if (clicking) lerp_t = 0.25f;
            if (flags & BUT_OFF) lerp_t = 0.5f;
            
            tl_c = lerp(tl_c, HIGHLIGHTED_BUTTON_COLOR, lerp_t);
            br_c = lerp(br_c, HIGHLIGHTED_BUTTON_COLOR, lerp_t);
            
            c = lerp(c, HIGHLIGHTED_BUTTON_COLOR, lerp_t);
        }
        
        auto c_button_rect = rect_to_sdl(rect_world_to_screen(button_rect, ui->screen_size));
        auto border = ng::max(ng::min(c_button_rect.w, c_button_rect.h) / 10, 1);
        for (int i = 0; i < border; i += 1) {
            {
                auto r = c_button_rect;
                r.x += i;
                r.y += i;
                r.w -= i;
                r.h -= i;
                SDL_SetRenderDrawColor(ui->renderer, br_c.r, br_c.g, br_c.b, br_c.a);
                SDL_RenderFillRect(ui->renderer, &r);
            }
            {
                auto r = c_button_rect;
                r.x += i;
                r.y += i;
                r.w -= i * 2 + 1;
                r.h -= i * 2 + 1;
                SDL_SetRenderDrawColor(ui->renderer, tl_c.r, tl_c.g, tl_c.b, tl_c.a);
                SDL_RenderFillRect(ui->renderer, &r);
            }
        }
        {
            auto r = c_button_rect;
            r.x += 1 * border;
            r.y += 1 * border;
            r.w -= 2 * border;
            r.h -= 2 * border;
            SDL_SetRenderDrawColor(ui->renderer, c.r, c.g, c.b, c.a);
            SDL_RenderFillRect(ui->renderer, &r);
        }
        {
            auto text_pos = rect_center(button_rect);
            
            //text_pos.x += (button_rect.w * (TEXT_ALIGN_FACTORS[cast(int) halign] - 0.5f)) * 0.95f;
            
            {
                auto screen_pos = v2_world_to_screen(text_pos, ui->screen_size);
                
                auto offset = v2(1, 1) * ui_get_current_point_height(ui) / 16;
                
                if (clicking) {
                    // screen_pos += offset;
                } else {
                    screen_pos -= offset;
                }
                
                text_pos = v2_screen_to_world(screen_pos, ui->screen_size);
            }
            
            auto font_scale = ui_get_current_font_scale(ui);
            auto text_size = get_text_size(&ui->font_data, label);
            // Bit of a @Hack: reassigning these vars.
            text_size.total_size = rect_screen_to_world(rect(v2(0, 0), text_size.total_size), ui->screen_size).size;
            text_size.total_size *= font_scale;
            text_size.x_cursor_on_last_line = rect_screen_to_world(rect(0, 0, text_size.x_cursor_on_last_line, 0), ui->screen_size).x; // @Hack
            text_size.x_cursor_on_last_line *= font_scale;
            
            auto fitting_rect = (button_rect);
            fitting_rect.size *= 0.95f;
            
            auto fit_scale = ng::min(fitting_rect.h / text_size.total_size.y,
                                     fitting_rect.w / text_size.total_size.x);
            
            if (flags & BUT_FLASH && !(flags & BUT_OFF)) {
                
                auto get_highlighted_text_color = [](f64 t) -> SDL_Color {
                    
                    auto lerp_t = (ng::sin(ng::TAU * 2 * t) + 1) / 2;
                    lerp_t = 1 - lerp_t;
                    lerp_t = 1 - lerp_t * lerp_t;
                    auto result = lerp(TEXT_COLOR/*HIGHLIGHTED_BUTTON_COLOR*/, SDL_Color{255, 255, 255, 255}, lerp_t);
                    return result;
                };
                
                color = get_highlighted_text_color(ui->t);
            }
            
            if (flags & BUT_OFF) {
                color.a *= 0.5f;
            }
            
            if (flags & BUT_WHITE) {
                color = SDL_Color{255, 255, 255, 255};
            }
            
            ui_text(ui, text_pos, label, 1.0f, ng::min(fit_scale, 1.0f), Text_Halign::Center, Text_Valign::Center, color);
        }
    }
    
    if (flags & BUT_OFF) {
        return false;
    }
    return clicked;
}

bool ui_is_selecting(Ui_State *ui) {
    return ui->tis.cursor != ui->tis.selection_cursor;
}

// :Quicksort
using quick_sort_comparator = s64(const void *a, const void *b, void *userdata);

s64 quick_sort_partition(u8 *arr, s64 element_size, s64 begin, s64 end, quick_sort_comparator *comparator, void *userdata) {
    auto x = arr + begin * element_size;
    auto i = begin;
    
    for (auto j = begin + 1; j < end; j += 1) {
        auto cmp = comparator(arr + j * element_size, x, userdata);
        if (cmp <= 0) {
            i = i + 1;
            ng::memswap(arr + i * element_size, arr + j * element_size, element_size);
        }
    }
    
    ng::memswap(arr + i * element_size, arr + begin * element_size, element_size);
    return i;
}

// :Quicksort
void quick_sort(u8 *arr, s64 element_size, s64 begin, s64 end, quick_sort_comparator *comparator, void *userdata) {
    if (begin < end) {
        auto r = quick_sort_partition(arr, element_size, begin, end, comparator, userdata);
        quick_sort(arr, element_size, begin, r, comparator, userdata);
        quick_sort(arr, element_size, r + 1, end, comparator, userdata);
    }
}

template <class T, class Func> void quick_sort(ng::array<T> *arr, Func comparator) {
    assert(arr);
    if (!arr) return;
    
    auto compare_closure = [](const void *aa, const void *bb, void *comparator) -> s64 {
        auto a = cast(const T *) aa;
        auto b = cast(const T *) bb;
        return (*cast(Func *) comparator)(a, b);
    };
    quick_sort(cast(u8 *) arr->data, sizeof(T), cast(u64) 0, arr->count, compare_closure, &comparator);
}

#define CHAR_COMPARATOR(name) bool name(u8 a, u8 b)

bool codepoint_equal(u32 a, u32 b) { return a == b; }
bool codepoint_equal_case_insensitive(u32 a, u32 b) {
    if (a > 'z' || b > 'z') return a == b;
    if (a >= 'a') a &= ~0x20; // Mask out lowercase bit.
    if (b >= 'a') b &= ~0x20;
    return a == b;
}
CHAR_COMPARATOR(char_equal) {
    return a == b;
}
CHAR_COMPARATOR(char_equal_case_insensitive) {
    return codepoint_equal_case_insensitive(a, b);
}

typedef CHAR_COMPARATOR(Char_Comparator);

// If trimmed is nonnull, trimmed is assigned the remainder of haystack after needle.
// if haystack or needle have lengths, they must be nonnull.
bool string_find(ng::string haystack, ng::string needle, ng::string *trimmed, Char_Comparator *equal = &char_equal) {
    assert(equal);
    if (!equal) return false;
    
    // @Todo: switch from naive to Boyer-Moore
    auto finder = haystack;
    while (finder.len >= needle.len) {
        for (s64 i = 0; i < needle.len; i += 1) {
            if (!equal(finder.ptr[i], needle.ptr[i])) goto double_continue;
        }
        
        if (trimmed) {
            finder.ptr += needle.len;
            finder.len -= needle.len;
            *trimmed = finder;
        }
        return true;
        
        double_continue:
        ++finder;
    }
    
    return false;
}

bool string_heads_match(ng::string a, ng::string b, Char_Comparator *equal = &char_equal) {
    assert(equal);
    if (!equal) return false;
    
    if (!a || !b) return (!a) == (!b);
    s64 length_to_check = ng::min(a.len, b.len);
    for (s64 i = 0; i < length_to_check; i += 1) if (!equal(a.ptr[i], b.ptr[i])) return false;
    return true;
}

ng::string split(ng::string &s, u8 ch) {
    while (s && s[0] == ch) ++s;
    auto result = s;
    
    while (s && s[0] != ch) ++s;
    result.len = s.ptr - result.ptr;
    
    return result;
}

ng::string split_by_line(ng::string &s) {
    if (s && (s[0] == '\r' || s[0] == '\n')) {
        if (s.len > 1 && s[0] + s[1] == '\r' + '\n') ++s;// @Attribution for addition trick goes to Sean Barrett (@nothings)
        ++s;
    }
    
    auto result = s;
    while (s && (s[0] != '\r' && s[0] != '\n')) ++s;
    result.len = s.ptr - result.ptr;
    
    return result;
}

ng::string trim_whitespace(ng::string s, bool trim_leading, bool trim_trailing) {
    if (!s) return s;
    if (trim_leading) {
        while (s && s[0] == ' ') ++s;
    }
    if (trim_trailing) {
        while (s && s[s.len - 1] == ' ') s.len -= 1;
    }
    return s;
}

ng::string get_first_name(ng::string name) {
    auto result = name;
    
    while (name && name[0] != ' ') ++name;
    result.len = name.ptr - result.ptr;
    
    return result;
}
ng::string get_last_name(ng::string name) {
    if (!name) return {};
    auto end = name.len;
    while (end > 0 && name.ptr[end - 1] == ' ') end -= 1;
    auto beginning = end;
    while (beginning > 0 && name.ptr[beginning - 1] != ' ') beginning -= 1;
    auto result = name;
    result.ptr += beginning;
    result.len = end - beginning;
    return result;
}

ng::string utf32_to_utf8(const ng::array<u32> *utf32, ng::allocator *alloc) {
    assert(utf32);
    assert(alloc);
    if (!utf32 || !alloc) return {};
    
    ng::string result = {};//auto result = ""_s;
    if (!utf32->count) return result;
    
    unsigned char dummy[4];
    unsigned int errors = 0;
    For (*utf32) result.len += utf8_encode(it, dummy, &errors);
    // if (errors) return ng::string{};
    assert(!errors);
    if (errors) return result;
    
    result.ptr = cast(u8 *) alloc->allocate(result.len);
    assert(result.ptr);
    if (!result.ptr) OutOfMem();
    
    auto cur = result.ptr, end = result.end();
    auto i = -1;
    while ((cur += utf8_encode((*utf32)[i += 1], cur, &errors)) < end);
    assert(!errors);
    
    return result;
}
void utf8_to_utf32(ng::string utf8, ng::array<u32> *utf32) {
    assert(utf32);
    if (!utf32) return;
    
    if (!utf8.len) return;
    
    if (utf32->count < 0) utf32->count = 0;
    utf32->reserve(utf32->count + ng::utf8strlen(utf8));
    
    unsigned int decoder_state = utf8_decode_accept;
    unsigned int codepoint = 0;
    For (utf8) {
        utf8_decode(&decoder_state, &codepoint, it);
        assert(decoder_state != utf8_decode_reject); // , "invalid utf8 string given");
        if (decoder_state == utf8_decode_reject) break;
        if (decoder_state != utf8_decode_accept) continue;
        
        utf32->push(codepoint);
    }
}

void ui_dialog(ng::string button, ng::string title, ng::string message) {
    
    auto message_c_str = ng::concatenate(ng::default_allocator, message, "\0"_s);
    defer { ng::free_string(&message_c_str, ng::default_allocator); };
    if (!message_c_str.len) OutOfMem();
    
    SDL_MessageBoxButtonData button_ = { 0, 0, cast(const char *) button.ptr };
    
    const SDL_MessageBoxData messageboxdata = {
        SDL_MESSAGEBOX_INFORMATION,
        nullptr,
        cast(char *) title.ptr,
        cast(char *) message_c_str.ptr,
        1, &button_,
        nullptr
    };
    
    int buttonid;
    if (SDL_ShowMessageBox(&messageboxdata, &buttonid) < 0) {
        // error showing error. hmmm
    }
};

void ui_clip_rect(Ui_State *ui, Rectangle rect) {
    assert(ui);
    if (!ui) return;
    
    ui->clip_rect = rect;
}
void ui_unclip_rect(Ui_State *ui) {
    assert(ui);
    if (!ui) return;
    
    ui->clip_rect = {};
}

int quit(
// App *app;
) {
    // assert(app);
    //
    // SDL_Quit();
    // ng::exit(0);
    return 0;
}

ng::string plural(s64 n, ng::string plural) {
    if (n == 1) plural.len -= 1; // remove 's'
    return plural;
}
ng::string plural(s64 n, ng::string plural, ng::string singular) {
    if (n == 1) return singular;
    return plural;
}

using Student_Id = s64;
using Tool_Id = s64;
// using Instructor_Id = s64;

enum struct Tool_Size_Type {
    Unsized = 0,
    Imperial,
    Metric,
    Thread_Imperial,
    Thread_Metric,
    
    Count
};

struct Tool_Size {
    union {
        u64 dummy = 0;
        struct { // metric/imperial
            f32 size;
        };
        struct { // thread (imperial)
            f32 diameter_in;
            u32 threads_per_in;
        };
        struct { // thread (metric)
            f32 diameter_mm;
            f32 pitch_mm;
        };
    };
};
static_assert(sizeof(Tool_Size) == 8, "");

bool tool_size_equal(Tool_Size a, Tool_Size b) {
    // return a.diameter_in == b.diameter_in && a.threads_per_in == b.threads_per_in; // @Volatile!!!
    // return !ng::memcmp(&a, &b, sizeof(Tool_Size));
    return a.dummy == b.dummy;
}

struct Owed_Item {
    Tool_Id tool_id = 0;
    Tool_Size tool_size = {};
    s64 quantity = 0;
    s64 earliest = 0;
    
    bool overdue = false;
    
    s64 new_quantity = 0; // Transient data used only for caching.
};
struct Borrower {
    Student_Id student_id = 0;
    s64 quantity = 0;
    
    bool overdue = false;
};

enum struct Class_Day {
    None = 0,
    Monday,
    Tuesday,
    Wednesday,
    Thursday,
    Friday,
    Saturday,
    Sunday
};

ng::string class_day_to_string(Class_Day cd) {
    if (cd == Class_Day::Monday) return "Monday"_s;
    if (cd == Class_Day::Tuesday) return "Tuesday"_s;
    if (cd == Class_Day::Wednesday) return "Wednesday"_s;
    if (cd == Class_Day::Thursday) return "Thursday"_s;
    if (cd == Class_Day::Friday) return "Friday"_s;
    if (cd == Class_Day::Saturday) return "Saturday"_s;
    if (cd == Class_Day::Sunday) return "Sunday"_s;
    return ""_s;
}

struct Student {
    // storage
    
    Student_Id student_id = 0;
    ng::string name = {};
    Class_Day class_day = Class_Day::None;
    s64 instructor = 0;
    
    // end storage
    
    ng::string first_name = {}; // non-owning
    ng::string last_name = {}; // non-owning
    
    ng::array<Owed_Item> owed_items = {};
    
    bool checkmarked = false;
};
struct Student_Search_Result {
    Student_Id student_id = 0;
    // @Refactor: do search results need first AND last name, rather than just one name string?
    // ng::string first_name = {}; // non-owning
    // ng::string last_name = {}; // non-owning
    bool first_name_query_match = 0;
    bool last_name_query_match = 0;
    bool id_query_match = 0;
};

bool student_registered(Student *student) {
    assert(student);
    
    if (student->name) return true;
    return false;
}

struct Tool {
    
    // storage
    
    Tool_Id tool_id = 0;
    Tool_Size_Type tool_size_type = Tool_Size_Type::Unsized;
    ng::string name = {};
    s64 tool_box_number = 0;
    s64 drawer_number = 0;
    
    s64 inventory_total = 0;
    
    ng::string image_filename = {};
    
    // end storage
    
    bool infinite = false;
    
    s64 inventory_available = 0;
    s64 times_loaned = 0; // times used EVER, to sort by most common/"popular" tools.
    
    ng::array<Borrower> borrowers = {};
    
    SDL_Texture *image = nullptr;
};
struct Tool_Search_Result {
    Tool_Id tool_id = 0;
    ng::string name = {}; // non-owning
    s64 name_query_matches = 0;
};

template <class T, class Func> s64 binary_search(const ng::array<T> *arr, Func comparator) {
    assert(arr);
    
    // @Todo: generalize to void-ptr C ABI, and THEN templatize on array element type and comparator type
    s64 begin = 0;
    s64 end = arr->count - 1;
    while (begin <= end) {
        auto middle = (begin + end) / 2;
        const T *element = &(*arr)[middle];
        
        s64 comparison = comparator(element);
        if (comparison == 0) {
            return middle;
        } else if (comparison < 0) {
            begin = middle + 1;
        } else {
            end = middle - 1;
        }
    }
    
    return -1;
}

Student *get_student(ng::array<Student> *students, Student_Id id) {
    assert(students);
    
    Student *result = nullptr;
    auto index = binary_search(students, [&](const Student *student) -> s64 { return student->student_id - id; });
    if (index >= 0) {
        result = &(*students)[index];
        assert(result->student_id == id);
    }
    return result;
}

Tool *get_tool(ng::array<Tool> *tools, Tool_Id id) {
    assert(tools);
    
    Tool *result = nullptr;
    auto index = binary_search(tools, [&](const Tool *tool) -> s64 { return tool->tool_id - id; });
    if (index >= 0) {
        result = &(*tools)[index];
        assert(result->tool_id == id);
    }
    return result;
}


void sort_tools(ng::array<Tool> *tools) { // :Quicksort
    assert(tools);
    
    quick_sort(tools, [](const Tool *a, const Tool *b) { return a->tool_id - b->tool_id; });
}

void sort_students(ng::array<Student> *students) { // :Quicksort
    
    assert(students);
    quick_sort(students, [](const Student *a, const Student *b) { return a->student_id - b->student_id; });
}


enum Transaction_Flags {
    TRANSACTION_IS_RETURN            = 0x1, // Default is loan, set this to make it a return.
    
    // @Todo @Incomplete: these
    // TRANSACTION_TOOL_IS_BROKEN       = 0x2,
    // TRANSACTION_IS_MANUAL_ADJUSTMENT = 0x4,
};

ng::string skip_whitespace(ng::string s) {
    while (s && s[0] == ' ') {
        ++s;
    }
    return s;
}

struct Transaction {
    
    // storage
    
    u64 flags = {};
    Tool_Size tool_size;
    Student_Id student_id;
    Tool_Id tool_id;
    s64 unix_timestamp;
    
    // end storage
    
};

void delete_tool(ng::array<Tool> *tool_storage, Tool *tool) {
    
    // free memory stuff
    ng::free_string(&tool->name, ng::default_allocator);
    
    // free memory stuff
    ng::free_string(&tool->image_filename, ng::default_allocator);
    
    if (tool->image) SDL_DestroyTexture(tool->image);
    
    tool->borrowers.release();
    
    // actual deletion from array
    s64 index = tool - tool_storage->data;
    assert(index >= 0);
    tool_storage->remove_ordered(index);
}

void delete_student(ng::array<Student> *student_storage, Student *student) {
    
    // free memory stuff
    ng::free_string(&student->name, ng::default_allocator);
    
    student->owed_items.release();
    
    // actual deletion from array
    s64 index = student - student_storage->data;
    assert(index >= 0);
    student_storage->remove_ordered(index);
}

void compile_data_from_transactions(ng::array<Tool> *tools, ng::array<Student> *students, const ng::array<Transaction> *transactions, int *num_tools_loaned, int *total_items_owed, int *total_overdue) {
    assert(tools);
    assert(students);
    assert(transactions);
    assert(num_tools_loaned);
    assert(total_items_owed);
    assert(total_overdue);
    
    *num_tools_loaned = 0;
    *total_items_owed = 0;
    *total_overdue = 0;
    
    // @Todo @Incomplete: account for broken tools and manual adjustments.
    
    auto prime_tool_for_processing = [](Tool &it) {
        it.inventory_available = it.inventory_total;
        //it.inventory_available_but_broken = 0;
        it.times_loaned = 0;
        // A crude optimization.
        it.borrowers.clear();
        it.borrowers.reserve(ng::clamp(it.inventory_available, 0, 1024));
    };
    auto prime_student_for_processing = [](Student &it) {
        it.owed_items.clear();
        it.owed_items.reserve(100); // A crude optimization.
    };
    
    For (*tools) prime_tool_for_processing(it);
    
    For (*students) prime_student_for_processing(it);
    
    for (auto &&transaction : *transactions) {
        auto tool_id = transaction.tool_id;
        auto student_id = transaction.student_id;
        
        auto tool_size = transaction.tool_size;
        
        auto tool = get_tool(tools, tool_id);
        
        if (!tool) { // Tool with that ID does not exist, so probably is a dead forgotten tool.
            continue;
        }
        
        auto student = get_student(students, student_id);
        if (!student) { // insert unregistered student
            {
                auto new_student = students->push();
                new_student->student_id = student_id;
                prime_student_for_processing(*new_student);
            }
            
            sort_students(students); // :Quicksort
            
            student = get_student(students, student_id);
        }
        
        assert(student);
        
        if (transaction.flags & TRANSACTION_IS_RETURN) {
            tool->inventory_available += 1;
            
            For (tool->borrowers) {
                if (it.student_id == student_id) {
                    it.quantity -= 1;
                }
            }
            
            for (int i = 0; i < student->owed_items.count; i += 1) {
                // So, for the owed items, our algorithm is this:
                // 1. Iterate through the transactions, and push/pop owed items as if 
                //    owed_items were a queue. This way, whenever we encounter a return, the
                //    earliest loans go out first.
                // 2. Collate the remaining owed items into one entry,
                //    containing a quantity and an 'earliest' timestamp.
                // 
                // The good news is, if we are in the transact page and we dismiss (a.k.a.
                // return) a pending loaned item, postprocess() will call this and
                // appropriately re-calculate all owed items and borrowers.
                // 
                // 2018-01-28
                // 
                if (student->owed_items.data[i].tool_id == tool_id && tool_size_equal(student->owed_items.data[i].tool_size, tool_size)) {
                    student->owed_items.remove_ordered(i);
                    break;
                }
            }
        } else {
            tool->inventory_available -= 1;
            
            bool borrower_found = false;
            For (tool->borrowers) {
                if (it.student_id == student_id) {
                    it.quantity += 1;
                    borrower_found = true;
                }
            }
            if (!borrower_found) {
                auto new_borrower = tool->borrowers.push();
                new_borrower->student_id = student_id;
                new_borrower->quantity = 1;
            }
            
            if (student) {
                auto new_owed_item = student->owed_items.push();
                new_owed_item->tool_id = tool_id;
                new_owed_item->tool_size = tool_size;
                new_owed_item->quantity = 1;
                new_owed_item->earliest = transaction.unix_timestamp;
            }
            
            tool->times_loaned += 1;
        }
    }
    
    For (*tools) {
        for (int i = 0; i < it.borrowers.count;) {
            auto &&borrower = it.borrowers[i];
            if (borrower.quantity <= 0) {
                it.borrowers.remove(i);
            } else {
                i += 1;
            }
        }
    }
    
    for (auto &&student : *students) {
        
        for (int outer = 0; outer < student.owed_items.count; outer += 1) {
            auto &&owed = student.owed_items[outer];
            
            int begin_at = outer;
            
            for (int i = begin_at; i < student.owed_items.count; i += 1) {
                auto &&other_owed = student.owed_items[i];
                
                if (&owed == &other_owed) continue;
                
                // If the other guy is coming before us, we want to be absolutely sure that he's not some duplicate. 
                if (&other_owed < &owed) assert(other_owed.tool_id != owed.tool_id || !tool_size_equal(other_owed.tool_size, owed.tool_size));
                
                if (other_owed.tool_id == owed.tool_id && tool_size_equal(other_owed.tool_size, owed.tool_size)) {
                    owed.quantity += 1;
                    owed.earliest = ng_min(owed.earliest, other_owed.earliest);
                    
                    student.owed_items.remove_ordered(i);
                    
                    i -= 1;
                    // outer -= 1;
                }
            }
        }
        
        quick_sort(&student.owed_items, [](const Owed_Item *a, const Owed_Item *b) { return a->earliest - b->earliest; });
        
#if 0//ndef NDEBUG
        // Make sure there are no duplicates.
        for (int i = 0; i < student.owed_items.count; i += 1) {
            for (int j = 1; j < student.owed_items.count; j += 1) {
                if (i == j) continue;
                assert(student.owed_items[i].tool_id != student.owed_items[j].tool_id || !tool_size_equal(student.owed_items[i].tool_size, student.owed_items[j].tool_size));
            }
        }
#endif
    }
    
    // delete unregistered students that ended up not owing anything
    for (int i = 0; i < students->count; i += 1) {
        auto &&student = students->data[i];
        
        if (student_registered(&student)) {
            continue;
        }
        
        if (student.owed_items.count <= 0) {
            delete_student(students, &student);
            i -= 1;
        }
    }
    
    For (*tools) {
        if (it.borrowers.count) *num_tools_loaned += 1;
        For (it.borrowers) *total_items_owed += it.quantity;
    }
    
    auto overdue = [](s64 then) -> bool { // Anything not returned same day is overdue.
        auto now = ng::get_unix_timestamp();
        
        // If it's somehow in the future, we're boned.
        if (then > now) return false;
        
        // If it's over 24h ago, it's definitely overdue.
        if (then < now - 86400) return true;
        
        // Otherwise, we need to figure out whether it's the same day.
        s64 timezone = 0;
        TIME_ZONE_INFORMATION tzi = {};
        if (GetTimeZoneInformation(&tzi) == TIME_ZONE_ID_DAYLIGHT) timezone += 60;
        timezone += -tzi.Bias;
        
        // Shift by timezone, in case it's the same day in local time but different days in UTC.
        timezone *= 60;
        then += timezone;
        now += timezone;
        
        then /= 86400;
        now /= 86400;
        
        assert(then <= now);
        
        return (then != now);
    };
    
    for (auto &&s : *students) {
        for (auto &&owed : s.owed_items) {
            if (overdue(owed.earliest)) {
                owed.overdue = true;
                *total_overdue += owed.quantity;
            }
        }
    }
    for (auto &&t : *tools) {
        for (auto &&borrower : t.borrowers) {
            
            auto s = get_student(students, borrower.student_id);
            if (s) {
                
                for (auto &&owed : s->owed_items) {
                    
                    if (owed.tool_id != t.tool_id) continue;
                    
                    borrower.overdue = borrower.overdue || owed.overdue;
                    
                }
            }
            
        }
    }
    
    
}


ng::string pluralize(s64 n, ng::string plural) {
    if (n == 1) {
        plural.len -= 1;
        if (plural.len < 0) plural.len = 0;
    }
    return plural;
}
ng::string pluralize(s64 n, ng::string plural, ng::string singular) {
    if (n == 1) {
        return singular;
    }
    return plural;
}

// @@@@@@@@@@@@@@@@@@@@
// @Todo @Temporary @Incomplete: Actually use this function!!!!
void notify_if_extraneous_transactions(Ui_State *ui, Tool *tool) {
    assert(ui);
    assert(tool);
    
    if (tool->inventory_available > tool->inventory_total) {
        // @@@@@@@@@@@@@@@@@@@@
        // @Todo @Temporary @Incomplete: Actually display this message!!!!
        s64 extra = tool->inventory_available - tool->inventory_total;
        auto msg = ng::aprint(ui->temp_allocator, "Tool '%' has untracked inventory (% items tracked). The transaction history lists % extraneous %."_s, tool->name, tool->inventory_total, extra, pluralize(extra, "returns"_s));
    }
    if (tool->inventory_available < 0 && !tool->infinite) {
        s64 extra = -tool->inventory_available;
        auto msg = ng::aprint(ui->temp_allocator, "Tool '%' has untracked inventory (% items tracked). The transaction history lists % extraneous %."_s, tool->name, tool->inventory_total, extra, pluralize(extra, "loans"_s));
    }
}

enum struct Ui_Page {
    Tools,
    Students,
    Transact,
    History,
    Locked,
    
    Count
};

enum struct Ui_List {
    None,
    Tools,
    Students,
    History,
    
    Count
};



enum struct Tool_Sort_Mode {
    Name,
    Inventory,
    Most_Loaned,
    Search_Matches,
};

enum struct Tool_Entry_State {
    None,
    New_Tool,
    Edit_Tool
};

struct Tools_Page_State { 
    Tool_Entry_State current_state = Tool_Entry_State::None;
    
    //ng::array<u32> search_input = {}; // :ReplaceInputArray @Todo: replace the text inputs array with pointers to these inputs.
    
    Tool_Id editing_tool = 0; // 0 => entry is a new tool.
    
    Tool_Size_Type entry_size_type = Tool_Size_Type::Unsized;
    
    ng::array<Tool_Search_Result> results = {};
    Tool_Sort_Mode tool_sort_mode = Tool_Sort_Mode::Name;
};
void tools_page_state_reset(Tools_Page_State *tps) {
    assert(tps);
    
    // Slower than clearing, but more robust to changes in the data structure.
    //tps->search_input.release(); // :ReplaceInputArray
    tps->results.release();
    
    *tps = {};
}



enum struct Student_Sort_Mode {
    Id,
    First_Name,
    Last_Name,
    Most_Owed,
    Instructor,
    Class_Day,
};

enum struct Student_Entry_State {
    None,
    New_Student,
    Edit_Student,
    Class_Day_And_Instructor,
    New_Or_Editing_Instructor,
    
};

struct Students_Page_State {
    Student_Entry_State current_state = Student_Entry_State::None;
    
    //ng::array<u32> search_input = {}; // :ReplaceInputArray
    
    Student_Id editing_student = 0; // 0 => entry is a new student.
    
    bool return_to_transact_page = false;
    
    Tool_Id listing_borrowers = 0;
    
    Class_Day selected_class_day = Class_Day::None;
    s64 selected_instructor = 0;
    
    s64 new_or_editing_instructor = 0;
    
    // @Robustness @Hack: should really just rearchitect all of this
    
    ng::array<Student_Search_Result> results = {};
    Student_Sort_Mode student_sort_mode = Student_Sort_Mode::Last_Name;
};
void students_page_state_reset(Students_Page_State *sps) {
    assert(sps);
    
    // Slower than clearing, but more robust to changes in the data structure.
    //sps->search_input.release(); // :ReplaceInputArray
    sps->results.release();
    
    *sps = {};
}

struct Pending_Tx { // Does not specify loan or return!!
    Tool_Id tool_id = {};
    Tool_Size tool_size = {};
    s64 quantity = 0;
};

enum struct Transact_Entry_State {
    None,
    Size,
};

struct Transact_Page_State {
    Transact_Entry_State current_state = Transact_Entry_State::None;
    
    //ng::array<u32> student_search_input = {}; // :ReplaceInputArray
    //ng::array<u32> tool_search_input = {};
    
    Student_Id who = 0;
    Tool_Id which = 0;
    
    ng::array<Pending_Tx> loans = {};
    ng::array<Pending_Tx> returns = {};
};
void transact_page_state_reset(Transact_Page_State *tps) {
    assert(tps);
    
    // Slower than clearing, but more robust to changes in the data structure.
    //tps->student_search_input.release(); // :ReplaceInputArray
    //tps->tool_search_input.release(); // :ReplaceInputArray
    
    tps->loans.release();
    tps->returns.release();
    
    *tps = {};
}

struct History_Page_State {
    //ng::array<u32> search_input = {}; // :ReplaceInputArray
    ng::array<const Transaction *> results = {};
};
void history_page_state_reset(History_Page_State *hps) {
    assert(hps);
    
    //hps->search_input.release(); // :ReplaceInputArray
    hps->results.release();
    
    *hps = {};
}

//#pragma warning(push)
#pragma warning(disable : 4307)
constexpr inline uint64_t hash_64_fnv1a_const(const char*k,int n){return(n--?hash_64_fnv1a_const(k,n)^k[n]:0x1d10331a6b696bc7)*0x100000001b3;}
//#pragma warning(pop)

ng::string ordinal(int x) { // Anglocentric function returns the english ordinal string.
    auto result = "th"_s;
    
    if (x < 0) x = -x; // don't pass in negatives, but okay.
    
    if (x / 10 % 10 != 1) { // 1001st, 1002nd, 1003rd but 1011th, 1012th, 1013th
        auto x10 = x % 10;
        if (x10 == 1) result = "st"_s;
        if (x10 == 2) result = "nd"_s;
        if (x10 == 3) result = "rd"_s;
    }
    
    return result;
}

ng::string get_month_string(int i) {
    return initializer_list((ng::string)
                            "January"_s, "February"_s, "March"_s, "April"_s, "May"_s, "June"_s,
                            "July"_s, "August"_s, "September"_s, "October"_s, "November"_s, "December"_s)[i];
}

struct Time_Strings {
    ng::string short_time, long_time;
};
Time_Strings get_time_strings(s64 unix, Temporary_Allocator *temp_allocator) {
    auto time = decompose_unix_timestamp_gregorian(unix, get_system_timezone_offset());
    
    auto hour12 = time.hour;
    auto minute = time.minute;
    auto second = time.second;
    auto meridian = "AM"_s;
    { // hour12 = ((hour - 1) % 12 + 12) % 12 + 1;
        if (hour12 >= 12) {
            hour12 -= 12;
            meridian = "PM"_s;
        }
        if (hour12 == 0) hour12 = 12;
    }
    
    auto day_of_week = time.day_of_week;
    //auto day_of_week = (time.day_of_week + 7) % 7; // @Inexplicable
    auto day_of_week_string = class_day_to_string(cast(Class_Day) (day_of_week + 1));
    
    auto month_string = get_month_string(time.month);
    
    auto day = time.day + 1;
    auto day_ordinal = ordinal(day);
    
    auto short_time = ng::aprint(temp_allocator, //"%, "
                                 "% %:% %"_s,
                                 //page_string,
                                 day_of_week_string.substring(0, 3), hour12, ng::fmt_int(minute, false, 10, 2), meridian);
    auto long_time = ng::aprint(temp_allocator, "%, % %%, %, %:%:%"_s,
                                day_of_week_string,
                                month_string,
                                day,
                                day_ordinal,
                                time.year,
                                ng::fmt_int(time.hour, false, 10, 2),
                                ng::fmt_int(minute, false, 10, 2),
                                ng::fmt_int(second, false, 10, 2)
                                );
    return {short_time, long_time};
}

int main() {
    
    ng::allocator_init();
    ng::assert_init();
    
    // We put this here because it's got a big stack we don't want to be stepping over to get to regular variables.
    Temporary_Allocator temp_allocator_ = {};
    auto temp_allocator = &temp_allocator_;
#ifndef NDEBUG
    defer { ng::print("Temporary allocator water_mark was %. % expansions, % relocations."_s, temp_allocator->water_mark, temp_allocator->expansions, temp_allocator->relocations); };
#endif
    
    auto tprint = [&temp_allocator](auto... args) {
        return ng::aprint(temp_allocator, args...);
    };
    
    // @Todo @Allocator
    // auto initial_mallocator = ng::default_allocator;
    // ng::default_allocator = nullptr;
    // auto main_pool = ng::create_pool(1024 * 1024 * 1024, initial_mallocator);
    // defer { ng::release_pool(&main_pool); };
    
    
    // @Todo @Allocator @Incomplete: auto transaction_pool = ;
    
    auto old_default_allocator = ng::default_allocator;
    auto main_proxy = ng::create_proxy_allocator("main", old_default_allocator); // @Todo: @Audit
    defer {
        ng::default_allocator = old_default_allocator;
        ng::destroy_proxy_allocator(&main_proxy);
    };
    ng::default_allocator = &main_proxy;
    
    // We always increment this with a new tool, lest old transactions false-match against a recycled tool ID.
    // @Todo: Handle situations where this number overflows, exceeding 9 quintillion.
    s64 global_tool_id_counter = 1;
    
    auto get_next_tool_id = [](s64 *global_tool_id_counter) -> s64 {
        auto result = *global_tool_id_counter;
        *global_tool_id_counter += 1;
        return result;
    };
    
    ng::array<ng::string> instructors = {};
    defer {
        For (instructors) {
            ng::free_string(&it, ng::default_allocator);
        }
        instructors.release();
    };
    
    instructors.push(ng::copy_string(""_s, ng::default_allocator));
    
    bool tools_changed = false;
    ng::array<Tool> tool_storage = {};
    defer {
        For (tool_storage) {
            ng::free_string(&it.name, ng::default_allocator); // @Allocator
            ng::free_string(&it.image_filename, ng::default_allocator); // @Allocator
            it.borrowers.release();
        }
        tool_storage.release();
    };
    
    bool students_changed = false;
    ng::array<Student> student_storage = {};   
    defer {
        For (student_storage) {
            ng::free_string(&it.name, ng::default_allocator); // @Allocator
            it.owed_items.release();
        }
        student_storage.release();
    };
    
    bool transactions_changed = false;
    ng::array<Transaction> transactions = {};  
    defer {
        transactions.release();
    };
    
    int num_tools_loaned = 0;
    int total_items_owed = 0;
    int total_overdue = 0;
    
    const auto about_text = ""
        "ToolGal Tool Manager © 2017-2019 Phillip Trudeau-Tavara.\n"
        "\n"
        "Redistribution and use in source and binary forms, with or\n"
        "without modification, are permitted.\n"
        "\n"
        "This software is provided 'as-is', without any express or implied\n"
        "warranty. In no event will the authors be held liable for any\n"
        "damages arising from the use of this software."_s;
    
    auto prompt_image_file_string = [&](ng::string default_path) -> ng::string {
        
        OPENFILENAMEW open_file_name = {};
        open_file_name.lStructSize = sizeof(OPENFILENAMEW);
        open_file_name.hwndOwner = nullptr;
        open_file_name.hInstance = nullptr;
        open_file_name.lpstrFilter = L"Image Files\0"
            "*.jpg;"
            "*.jpeg;"
            "*.png;"
            "*.bmp;"
            "*.gif;\0"
            "All Files\0"
            "*.*\0";
        open_file_name.lpstrCustomFilter = nullptr;
        open_file_name.nMaxCustFilter = 0;
        open_file_name.nFilterIndex = 1;
        
        const auto FILENAME_STRING_LENGTH = 65536;
        ng::string filename_string = {FILENAME_STRING_LENGTH, cast(u8 *) ng::default_allocator->allocate(FILENAME_STRING_LENGTH)};
        if (!filename_string.ptr) OutOfMem();
        
        defer { ng::free_string(&filename_string, ng::default_allocator); };
        
        open_file_name.lpstrFile = cast(__wchar_t *) temp_allocator->data + temp_allocator->mark;
        open_file_name.nMaxFile = cast(DWORD)(sizeof(temp_allocator->data) - temp_allocator->mark);
        
        if (open_file_name.nMaxFile > 0) open_file_name.lpstrFile[0] = 0;
        
        open_file_name.lpstrFileTitle = nullptr;
        open_file_name.nMaxFileTitle = 0;
        
        //while (default_path && ) 
        
        auto initial_dir = ng::utf8_to_utf16(default_path);
        defer { initial_dir.release(); };
        initial_dir.push('\0');
        open_file_name.lpstrInitialDir = cast(__wchar_t *) initial_dir.data; // L"./";
        
        open_file_name.lpstrTitle = L"Open Image";
        open_file_name.Flags = 0;
        open_file_name.nFileOffset = 0;
        open_file_name.nFileExtension = 0;
        open_file_name.lpstrDefExt = L"";
        open_file_name.lCustData = 0;
        open_file_name.lpfnHook = nullptr;
        open_file_name.lpTemplateName = nullptr;
        
        auto result = GetOpenFileNameW(&open_file_name);
        
        if (!result) {
            // handle user cancel or error
            
            auto err = CommDlgExtendedError();
            ng::print("CommDlg error: %\n"_s, cast(int) err);
            return {};
        }
        
        u16 *filename16 = cast(u16 *) open_file_name.lpstrFile;
        u64 filename16_len = 0;
        
        for (auto p = filename16; *p; p += 1) filename16_len += 1;
        
        auto filename = ng::utf16_to_utf8(filename16, filename16_len);
        
        return filename;
    };
    
    auto add_transaction = [&transactions](u64 flags, s64 tool_id, s64 student_id, Tool_Size tool_size, s64 unix_timestamp = ng::get_unix_timestamp()) {
        Transaction new_transaction;
        new_transaction.flags = flags;
        new_transaction.tool_id = tool_id;
        new_transaction.student_id = student_id;
        new_transaction.tool_size = tool_size;
        new_transaction.unix_timestamp = unix_timestamp;
        transactions.push(new_transaction);
    };
    
    auto sdl_init_result = SDL_Init(SDL_INIT_EVERYTHING);
    if (sdl_init_result) return 1;
    defer { SDL_Quit(); };
    
    auto window = SDL_CreateWindow("ToolGal v" STR_VERSION "", SDL_WINDOWPOS_UNDEFINED, SDL_WINDOWPOS_UNDEFINED,
                                   DEFAULT_SCREEN_SIZE.x, DEFAULT_SCREEN_SIZE.y,
                                   SDL_WINDOW_HIDDEN | SDL_WINDOW_RESIZABLE | SDL_WINDOW_MAXIMIZED);
    if (!window) return 1;
    
    bool window_has_focus = false;
    
    auto renderer = SDL_CreateRenderer(window, -1, SDL_RENDERER_ACCELERATED | SDL_RENDERER_TARGETTEXTURE);
    if (!renderer) return 1;
    
    SDL_SetRenderDrawBlendMode(renderer, SDL_BLENDMODE_BLEND);
    
    auto load_texture_from_path = [&](ng::string filename) -> SDL_Texture * {
        
        SDL_Texture *result = nullptr;
        
        auto image_file_alloc = ng::default_allocator;
        
        ng::string image_file = {};
        
        defer { ng::free_string(&image_file, ng::default_allocator); };
        
        auto read_result = read_entire_file(filename, &image_file, ng::default_allocator, temp_allocator);
        
        if (read_result == Read_Entire_File_Result::SUCCESSFUL) {
            int w = 0;
            int h = 0;
            int n = 0;
            auto image = stbi_load_from_memory(image_file.ptr, image_file.len, &w, &h, &n, 4);
            
            // operate on image
            auto surf = SDL_CreateRGBSurfaceFrom(image, w, h, 32, w * 4, 0xff, 0xff00, 0xff0000, 0xff000000);
            assert(surf);
            defer { SDL_FreeSurface(surf); };
            result = SDL_CreateTextureFromSurface(renderer, surf);
            
            defer { stbi_image_free(image); };
        } else {
            // handle error
            
            return nullptr;
        }
        
        return result;
    };
    
    auto postprocess = [&load_texture_from_path](ng::array<Student> *student_storage, ng::array<Tool> *tool_storage, ng::array<Transaction> *transactions, int *num_tools_loaned, int *total_items_owed, int *total_overdue) { // @Incomplete
        For (*student_storage) {
            it.first_name = get_first_name(it.name);
            it.last_name = get_last_name(it.name);
        }
        
        For (*tool_storage) {
            if (!it.image) continue;
            SDL_DestroyTexture(it.image);
        }
        
        For (*tool_storage) {
            if (!it.image_filename) continue;
            it.image = load_texture_from_path(it.image_filename);
        }
        
        sort_tools(tool_storage);
        sort_students(student_storage);
        
        compile_data_from_transactions(tool_storage, student_storage, transactions, num_tools_loaned, total_items_owed, total_overdue);
        
    };
    postprocess(&student_storage, &tool_storage, &transactions, &num_tools_loaned, &total_items_owed, &total_overdue);
    
    Ui_State ui_ = {};
    auto ui = &ui_;
    
    ui->main_allocator = ng::default_allocator;
    ui->temp_allocator = temp_allocator;
    
    ui->tooltip.alloc = ui->main_allocator;
    defer {
        ui->tooltip.release();
    };
    
    ui->renderer = renderer;
    ui->screen_size = DEFAULT_SCREEN_SIZE;
    ui->font_data.info.userdata = ui->temp_allocator;
    
    {
        int w = 0;
        int h = 0;
        SDL_GetWindowSize(window, &w, &h);
        if (w != 0) ui->screen_size.x = w;
        if (h != 0) ui->screen_size.y = h;
    }
    
    // need greyscale palette
    ng::memset(ui->font_data.palette, 255, sizeof(ui->font_data.palette));
    for (int i = 0; i < countof(ui->font_data.palette); i += 1) {
        ui->font_data.palette[i].a = i;
    }
    
    auto font_file = ""_s;
    
    ui->font_data.glyphs.alloc = ng::default_allocator;
    
    auto clear_glyphs = [](ng::map<Glyph_Id, Glyph> *glyphs) {
        For (*glyphs) {
            // assert(it.value.texture);
            if (it.value.texture) SDL_DestroyTexture(it.value.texture);
            it.occupied = false;
        }
    };
    
    defer {
        clear_glyphs(&ui->font_data.glyphs);
        ui->font_data.glyphs.release();
    };
    
    defer { // Make sure this runs only on program exit!
        if (font_file.ptr) ng::free_string(&font_file, ng::default_allocator);
    };
    {
        auto result = Read_Entire_File_Result::NONEXISTENT;
        if (result != Read_Entire_File_Result::SUCCESSFUL) {
            result = read_entire_file("C:/Windows/Fonts/SegUISB.ttf"_s, &font_file, ng::default_allocator, temp_allocator);
        }
        if (result != Read_Entire_File_Result::SUCCESSFUL) {
            result = read_entire_file("C:/Windows/Fonts/SegoeUI.ttf"_s, &font_file, ng::default_allocator, temp_allocator);
        }
        if (result != Read_Entire_File_Result::SUCCESSFUL) {
            result = read_entire_file("C:/Windows/Fonts/Arial.ttf"_s, &font_file, ng::default_allocator, temp_allocator);
        }
        if (result != Read_Entire_File_Result::SUCCESSFUL) {
            FatalDialog("No font could be loaded! Make sure C:/Windows/Fonts/ can be accessed.");
        }
        
        auto font_init_result = stbtt_InitFont(&ui->font_data.info, font_file.ptr, 0);
        assert(font_init_result);
    }
    //auto font_init_result = stbtt_InitFont(&ui->font_data.info, file_OpenSans_SemiBold_ttf, 0);
    //assert(font_init_result);
    
    auto get_metrics = [](Ui_State *ui) {
        assert(ui);
        
        stbtt_GetFontVMetrics(&ui->font_data.info, &ui->font_data.ascent, &ui->font_data.descent,
                              &ui->font_data.line_gap);
        
        ui->font_data.line_advance = ui->font_data.ascent - ui->font_data.descent + ui->font_data.line_gap;
        
        stbtt_GetCodepointHMetrics(&ui->font_data.info, ' ', &ui->font_data.space_width, nullptr);
    };
    
    get_metrics(ui);
    
    ng::array<u32> text_inputs[6] = {};
    ng::array<u8> text_input_errors[countof(text_inputs)] = {};
    int text_input = 0;
    defer {
        For (text_inputs) it.release();
        For (text_input_errors) it.release();
    };
    
    SDL_StartTextInput();
    defer { SDL_StopTextInput(); };
    
    auto mouse_cursor_ibeam = SDL_CreateSystemCursor(SDL_SYSTEM_CURSOR_IBEAM);
    defer { SDL_FreeCursor(mouse_cursor_ibeam); };
    
    int scroll_index = 0;
    float scroll_eased_pos = 0;
    
#define scroll_index_to_pos(ind, item_h) ((ind) * (item_h))
#define scroll_pos_to_index(pos, item_h) ((pos) / (item_h))
    
    float scroll_pos_owed = 0;
    float scroll_pos_pending = 0;
    
#define SAVED_MESSAGE_TIME 6.0f
    float saved_message_timer = SAVED_MESSAGE_TIME;
    auto saved_message_alpha = [](float x) -> float {
        const float k = SAVED_MESSAGE_TIME;
        
        float result = (k - x) / k;
        result *= result * result;
        
        return ng::clamp(result, 0, 1);
    };
    
    const float CURSOR_MAX_HOTNESS = 0.75;
    const float CURSOR_HOT_TIME = CURSOR_MAX_HOTNESS / 5;
    const float CURSOR_EASE_BACKSPACE_TIME = 0.1f;
    
#ifndef NDEBUG
#define USE_PASSWORD 0
#else
#define USE_PASSWORD 1
#endif
    
    auto active_page = Ui_Page::Locked;
    // auto active_list = Ui_List::None;
    
    Tools_Page_State tools_page_state = {};
    defer { tools_page_state_reset(&tools_page_state); };
    Students_Page_State students_page_state = {};
    defer { students_page_state_reset(&students_page_state); };
    Transact_Page_State transact_page_state = {};
    defer { transact_page_state_reset(&transact_page_state); };
    History_Page_State history_page_state = {};
    defer { history_page_state_reset(&history_page_state); };
    
    bool really_delete = false;
    
    auto active_list = [&active_page, &text_input, &tools_page_state, &students_page_state, &transact_page_state]() -> Ui_List {
        if (active_page == Ui_Page::Tools) {
            if (tools_page_state.current_state == Tool_Entry_State::None) {
                return Ui_List::Tools;
            }
        }
        if (active_page == Ui_Page::Students) {
            if (students_page_state.current_state == Student_Entry_State::None) {
                return Ui_List::Students;
            }
        }
        if (active_page == Ui_Page::Transact) {
            if (transact_page_state.current_state == Transact_Entry_State::None) {
                if (text_input == 0) {
                    return Ui_List::Students;
                }
                if (text_input == 1) {
                    return Ui_List::Tools;
                }
            }
        }
        if (active_page == Ui_Page::History) {
            return Ui_List::History;
        }
        return Ui_List::None;
    };
    
    auto base_path = ng::string::c_str(SDL_GetBasePath());
    if (!base_path.ptr) base_path = ng::string::c_str(SDL_strdup("./"));
    defer { SDL_free(base_path.ptr); };
    
    SDL_ShowWindow(window);
    
    SDL_SetRenderDrawColor(renderer, 255, 255, 255, 255);
    SDL_RenderClear(renderer);
    float y_cursor = 0.75f;
    auto render_load_message = [&ui, &y_cursor, &renderer](ng::string message) {
        ui_text(ui, v2(0.5f, y_cursor), message);
        y_cursor -= 0.05f;
        SDL_RenderPresent(renderer);
#if 0
        // This is actually good, because it loads slower, but it is usually imperceptibly fast,
        // so now it is noticeable that the program is so fast.
        SDL_Delay(500);
#endif
    };
    
    auto create_directory = [&temp_allocator, &tprint, &base_path, &ui](ng::string name) -> bool {
        auto mark = temp_allocator->get_mark();
        defer { temp_allocator->set_mark(mark); };
        
        auto new_path = ng::concatenate(temp_allocator, base_path, name);
        
        auto result = ng::create_directory(new_path);
        
        if (result != ng::create_directory_result::success && result != ng::create_directory_result::already_exists) {
            ui_dialog("Exit"_s, "Fatal Error"_s, tprint("Couldn't create directory % for storage. Ensure the program is permitted to write to that folder."_s, new_path));
            
            return false;
        }
        return true;
    };
    
    render_load_message("Opening Toolgal Directory"_s);
    if (!create_directory("toolgal"_s)) {
        return quit(
            // app
            );
    }
    render_load_message("Opening Toolgal Backups Directory"_s);
    if (!create_directory("toolgal.backups"_s)) {
        return quit(
            // app
            );
    }
    // accessing ./toolgal/ and ./toolgal.backups/ is now valid
    
    // Attempt to write this, but don't care if it fails.
    write_entire_file(tprint("%toolgal/info.toolgal.txt"_s, base_path),
                      "You can freely edit the tools.toolgal and students.toolgal files.\n"
                      "The tool entry format is <ID> `<Name>` <Total Inventory> <Size Type> <Optional Tool Box #> <Drawer #> \"<Path to image file>\"\n"
                      "    (If there is a tool box number, there must also be a drawer number.)\n"
                      "    (The size types are 0=Unsized, 1=Imperial, 2=Metric, 3=Thread (Imperial), 4=Thread (Metric).)\n"
                      "\n"
                      "The instructors go into the students file. The format is:\n" "[Firstname Lastname]\n"
                      "There is one instructor per line. They can go anywhere in the list.\n"
                      "The student entry format is <ID> <Class Day> <Instructor> <Name>\n"
                      "The transactions file must be stored in binary format to be efficient and fast to load,\n"
                      "    so it cannot be directly edited. You will have to make changes from within the program\n"
                      "    (or use a hex editor). Sorry!"_s, temp_allocator);
    
    auto load = [&temp_allocator](ng::string file_path) -> ng::string {
        ng::string file = {};
        assert(file_path);
        auto result = read_entire_file(file_path, &file, ng::default_allocator, temp_allocator);
        
        switch (result) {
            case Read_Entire_File_Result::SUCCESSFUL: {
                //*existed = true;
            } break;
            case Read_Entire_File_Result::NONEXISTENT: {
                //*existed = false;
            } break;
            default_bad; // @Todo @Incomplete
        }
        
        return file;
    };
    
    bool tools_backup_succeeded = false;
    bool students_backup_succeeded = false;
    bool transactions_backup_succeeded = false;
    
    auto tools_file_path = tprint("%toolgal/tools.toolgal\0"_s, base_path);
    // @Todo: OutOfMem(); defer { free_string; };
    auto students_file_path = tprint("%toolgal/students.toolgal\0"_s, base_path);
    auto transactions_file_path = tprint("%toolgal/transactions.toolgal\0"_s, base_path);
    
    auto tools_file = load(tools_file_path);
    auto students_file = load(students_file_path);
    auto transactions_file = load(transactions_file_path);
    
    // Since transactions is a binary file, we don't need to parse it, we can simply reinterpret the string pointer
    // as the data pointer. Therefore we pass along ownership of the allocation for the string to our transactions array,
    // and it gets treated as if it were originally allocated as an array of transactions, including the ability
    // to dynamically resize it later. HOWEVER, if there is some failure, we have to free everything here and now
    // lest we leak memory (though, we're likely just quitting anyway...) so we assume that we are actually freeing the
    // file and only forgetting that obligation upon success. 2018-01-22
    bool transaction_free_dismissed = false;
    defer {
        ng::free_string(&tools_file, ng::default_allocator); // @Allocator
        ng::free_string(&students_file, ng::default_allocator); // @Allocator
        if (!transaction_free_dismissed) {
            ng::free_string(&transactions_file, ng::default_allocator); // @Allocator
        }
    };
    
    auto make_backups = [&temp_allocator, &tprint, &ui, &base_path, &tools_file, &students_file, &transactions_file, &render_load_message, &tools_file_path, &students_file_path, &transactions_file_path] {
        auto mark = temp_allocator->get_mark();
        defer { temp_allocator->set_mark(mark); };
        
        auto now = ng::get_unix_timestamp();
        auto back_up = [&tprint, &temp_allocator, &ui, &base_path, &now](ng::string file_path, ng::string name) {
            auto new_path = tprint("%toolgal.backups/%_%.toolgal\0"_s, base_path, name, now); // @Portability: Does Linux support '-' in filenames? Because this timestamp might be negative.
            if (!new_path) OutOfMem(); // @Incomplete: put OutOfMeme() everywhere it should be
            
            if (CopyFileA(cast(char *) file_path.ptr, cast(char *) new_path.ptr, true)) { // @Todo: CopyFileW (so we can open paths with length > MAX_PATH)
                // Backup was successful.
            } else {
                ui_dialog("Exit"_s, "Fatal Data File Write Error"_s, tprint("Fatal error: A backup could not be made at the path \"%\". "_s, new_path));
                return false;
            }
            
            return true;
        };
        
        // We also check whether the strings are empty, because there's no point in backing up empty files.
        render_load_message("Backing Up Tools"_s);
        if (tools_file) if (!back_up(tools_file_path, "tools"_s)) return quit(
            // app
            );
        render_load_message("Backing Up Students"_s);
        if (students_file) if (!back_up(students_file_path, "students"_s)) return quit(
            // app
            );
        render_load_message("Backing Up Transactions"_s);
        if (transactions_file) if (!back_up(transactions_file_path, "transactions"_s)) return quit(
            // app
            );
        return 0;
    };
    
    render_load_message("Loading Tools"_s);
    
    auto tools_file_success = [&ui, &tools_file, &temp_allocator, &tool_storage, &tprint, &global_tool_id_counter] {
        auto file = tools_file;
        
        if (!file) {
            return true; // empty
        }
        
        s64 line_number = 0;
        
        auto entry_error = [&ui, &tprint, &line_number](ng::string message) {
            ui_dialog("Exit"_s,
                      "Fatal Data File Load Error"_s, tprint("Fatal error - the tools file has an invalid entry on line %: %."_s, line_number, message));
            return false;
        };
        
        auto file_splitter = file;
        line_number += 1;
        auto line_1 = split_by_line(file_splitter);
        line_number += 1;
        auto line_2 = split_by_line(file_splitter);
        
        const auto FORMAT_VERSION_STRING = "[1]"_s;
        if (!line_1 || !line_2) {
            ui_dialog("Exit"_s, "Data File Load Error"_s, "The tools file is invalid. Please delete it."_s);
            return false;
        }
        
        if (line_2[0] != '[') {
            ui_dialog("Exit"_s, "Data File Load Error"_s, "Line 2 of the tools file does not start with a '['."_s);
            return false;
        }
        ++line_2;
        if (!ng::str_to_s64(&line_2, &global_tool_id_counter)) {
            ui_dialog("Exit"_s, "Data File Load Error"_s, "The tools file is invalid. Please delete it."_s);
            return false;
        }
        if (!line_2 || line_2[0] != ']') {
            ui_dialog("Exit"_s, "Data File Load Error"_s, "The tools file is invalid. Please delete it."_s);
            return false;
        }
        
        if (!string_heads_match(line_1, FORMAT_VERSION_STRING)) { // @Hack @Temporary @Incomplete: Don't be lazy, explicitly check for a #comment, don't ignore rando junk after the version number.
            ui_dialog("Exit"_s, "Data File Load Error"_s, tprint("The tools file is of a newer or unknown version \"%\" (should be \"%\")."_s, line_1, FORMAT_VERSION_STRING));
            return false;
        }
        
        // No failures
        bool errored = true;
        while (true) {
            line_number += 1;
            auto line = split_by_line(file_splitter);
            if (!line) {
                if (!file_splitter) break;
                
                // just blank line
                else continue;
            }
            
            if (line[0] == '#') continue; // comment
            while (line && line[0] == ' ') {
                ++line;
            }
            if (!line || line[0] == '#') return entry_error("The tool ID number is missing"_s);
            s64 tool_id = 0;
            if (!ng::str_to_s64(&line, &tool_id)) return entry_error("The tool ID is not a valid number"_s);
            if (tool_id == 0) return entry_error("The tool ID number is zero"_s);
            if (tool_id < 0) return entry_error("The tool ID number is negative"_s);
            if (tool_id >= global_tool_id_counter) return entry_error("The tool ID number was larger than the global tool ID counter, which makes no sense"_s);
            For (tool_storage) {
                if (it.tool_id == tool_id) {
                    return entry_error(tprint("This tool's ID number matches another (named \"%\")"_s, it.name));
                }
            }
            while (line && line[0] == ' ') {
                ++line;
            }
            if (!line || line[0] == '#') return entry_error("The tool has no name"_s);
            if (line[0] != '`') return entry_error("The tool's name was not found after its ID, surrounded by grave ticks"_s);
            auto name = ""_s;
            name.ptr = line.ptr + 1;
            while (true) {
                ++line;
                if (!line) {
                    return entry_error("No closing quotation mark was found while reading the name"_s);
                }
                if (line[0] == '`') { // We're done with the name
                    break;
                }
            }
            name.len = line.ptr - name.ptr;
            
            if (!name) return entry_error("The tool's name is empty"_s);
            ++line;
            while (line && line[0] == ' ') {
                ++line;
            }
            if (!line || line[0] == '#') return entry_error("The tool has no inventory value"_s);
            s64 inventory_total = 0;
            if (!ng::str_to_s64(&line, &inventory_total)) return entry_error("The tool inventory value is not a valid number"_s);
            if (inventory_total < 0) return entry_error("The tool inventory value is negative"_s);
            // We will use zero to represent that the value is infinite.
            // if (inventory_total == 0) return entry_error("The tool inventory value is zero"_s);
            
            auto new_tool = tool_storage.push();
            new_tool->tool_id = tool_id;
            new_tool->name = ng::copy_string(name, ng::default_allocator); // @Allocator
            new_tool->inventory_total = inventory_total;
            if (new_tool->inventory_total == 0) new_tool->infinite = true;
            assert(new_tool->inventory_total >= 0);
            
            while (line && line[0] == ' ') {
                ++line;
            }
            
            s64 tool_size_type_number = 0;
            if (!ng::str_to_s64(&line, &tool_size_type_number)) return entry_error("The tool size type number is not a valid number"_s);
            if (tool_size_type_number < 0 || tool_size_type_number >= cast(int) Tool_Size_Type::Count) {
                return entry_error("The tool size type number does not represent a valid type"_s);
            }
            Tool_Size_Type tool_size_type = cast(Tool_Size_Type) tool_size_type_number;
            
            new_tool->tool_size_type = tool_size_type;
            
            while (line && line[0] == ' ') {
                ++line;
            }
            if (!line || line[0] == '#') {
                continue; // The rest is optional, so just skip it.
            }
            
            s64 tool_box_number = 0;
            if (!ng::str_to_s64(&line, &tool_box_number)) return entry_error("The tool box number is not a valid number"_s);
            if (tool_box_number < 0) return entry_error("The tool box number is negative"_s);
            // We will use zero to represent that the value is unspecified.
            // if (tool_box_number == 0) return entry_error("The tool box number is zero"_s);
            while (line && line[0] == ' ') {
                ++line;
            }
            if (!line || line[0] == '#') return entry_error("There is a tool box number but no drawer number"_s);
            s64 drawer_number = 0;
            if (!ng::str_to_s64(&line, &drawer_number)) return entry_error("The drawer number is not a valid number"_s);
            if (drawer_number < 0) return entry_error("The drawer number is negative"_s);
            // We will use zero to represent that the value is unspecified.
            // if (drawer == 0) return entry_error("The drawer number is zero"_s);
            
            new_tool->tool_box_number = tool_box_number;
            new_tool->drawer_number = drawer_number;
            
            while (line && line[0] == ' ') ++line;
            if (!line || line[0] == '#') {
                continue; // The rest is optional, so just skip it.
            }
            if (line[0] != '"') return entry_error("The image filename path must start with a \""_s);
            
            auto image_filename = ++line;
            
            while (line && line[0] != '"') ++line;
            if (!line) return entry_error("The image filename path did not end with a \""_s);
            
            image_filename.len = line.ptr - image_filename.ptr;
            
            new_tool->image_filename = ng::copy_string(image_filename, ng::default_allocator); // @Allocator
            
            // We don't care about remaining junk.
        }
        
        return true;
    }();
    if (!tools_file_success) {
        return quit(
            // app
            );
    }
    
    render_load_message("Loading Students"_s);
    
    auto students_file_success = [&ui, &students_file, &temp_allocator, &student_storage, &instructors, &tprint] {
        auto file = students_file;
        
        if (!file) {
            return true; // empty
        }
        
        s64 line_number = 0;
        
        auto entry_error = [&ui, &tprint, &line_number](ng::string message) {
            ui_dialog("Exit"_s,
                      "Fatal Data File Load Error"_s, tprint("Fatal error - the students file has an invalid entry on line %: %."_s, line_number, message));
            return false;
        };
        
        auto file_splitter = file;
        line_number += 1;
        auto line_1 = split_by_line(file_splitter);
        
        const auto FORMAT_VERSION_STRING = "[1]"_s;
        
        if (!string_heads_match(line_1, FORMAT_VERSION_STRING)) { // @Hack @Temporary @Incomplete: Don't be lazy, explicitly check for a #comment, don't ignore rando junk after the version number.
            ui_dialog("Exit"_s, "Data File Load Error"_s, tprint("The students file is of a newer or unknown version \"%\" (should be \"%\")."_s, line_1, FORMAT_VERSION_STRING));
            return false;
        }
        
        // No failures
        bool errored = true;
        while (true) {
            line_number += 1;
            auto line = split_by_line(file_splitter);
            if (!line) {
                if (!file_splitter) break;
                
                // just blank line
                else continue;
            }
            
            if (line[0] == '#') continue; // comment
            
            {
                auto s = trim_whitespace(line, true, true);
                if (s.len > 1 && s[0] == '[' && s[s.len - 1] == ']') {
                    
                    ++s;
                    s.len -= 1;
                    
                    instructors.push(ng::copy_string(s, ng::default_allocator));
                    
                    continue;
                }
            }
            
            while (line && line[0] == ' ') {
                ++line;
            }
            if (!line || line[0] == '#') return entry_error("The student ID number was missing"_s);
            s64 student_id = 0;
            if (!ng::str_to_s64(&line, &student_id)) return entry_error("The student ID is not a valid number"_s);
            if (student_id == 0) return entry_error("The student ID number is zero"_s);
            if (student_id < 0) return entry_error("The student ID number is negative"_s);
            
            For (student_storage) {
                if (it.student_id == student_id) {
                    return entry_error(tprint("This student's ID number matches another (named \"%\")"_s, it.name));
                }
            }
            while (line && line[0] == ' ') {
                ++line;
            }
            if (!line || line[0] == '#') return entry_error("The class day is missing"_s);
            s64 class_day = 0;
            if (!ng::str_to_s64(&line, &class_day)) return entry_error("The class day is not a valid number"_s);
            
            if (class_day < 0 || class_day > 7) {
                return entry_error("The class day number does not represent a valid day"_s);
            }
            
            while (line && line[0] == ' ') {
                ++line;
            }
            if (!line || line[0] == '#') return entry_error("The instructor number is missing"_s);
            s64 instructor = 0;
            if (!ng::str_to_s64(&line, &instructor)) return entry_error("The instructor number is not a valid number"_s);
            if (instructor < 0) return entry_error("The instructor number is negative"_s);
            
            while (line && line[0] == ' ') {
                ++line;
            }
            // Truncate the line to the start of the comment if it exists.
            for (s64 i = 0; i < line.len; i += 1) {
                if (line.ptr[i] == '#') {
                    line.len = i;
                    break;
                }
            }
            if (!line) return entry_error("The student has no name"_s);
            auto name = trim_whitespace(line, true, true); // Just use the rest of the line, whatever.
            
            auto new_student = student_storage.push();
            new_student->student_id = student_id;
            new_student->class_day = cast(Class_Day) class_day;
            new_student->instructor = instructor;
            new_student->name = ng::copy_string(name, ng::default_allocator); // @Allocator
        }
        
        return true;
    }();
    if (!students_file_success) {
        return quit(
            // app
            );
    }
    
    render_load_message("Loading Transactions"_s);
    
    bool transactions_file_success = false;
    if (!transactions_file) {
        transactions_file_success = true;
    } else {
        if (transactions_file.len % sizeof(Transaction)) {
            ui_dialog("Exit"_s, "Fatal Data File Read Error"_s, "Fatal error: The transaction history is corrupted."_s);
        } else {
            transactions.alloc = ng::default_allocator; // @Allocator
            transactions.data = cast(Transaction *) transactions_file.ptr; // @Todo: @Alignment
            transactions.capacity = transactions_file.len / sizeof(Transaction);
            transactions.count = transactions.capacity;
            
            transaction_free_dismissed = true;
            
            transactions_file_success = true;
        }
    }
    if (!transactions_file_success) {
        
        return quit(
            // app
            );
    }
    
    {
        auto backups_result = make_backups();
        if (backups_result != 0) {
            return backups_result;
        }
    }
    postprocess(&student_storage, &tool_storage, &transactions, &num_tools_loaned, &total_items_owed, &total_overdue);
    
    auto save_transactions = [&temp_allocator, &transactions, &tprint, &base_path, &saved_message_timer] { // save transactions
        auto mark = temp_allocator->get_mark();
        defer { temp_allocator->set_mark(mark); };
        
        auto u8ified = cast(u8 *) transactions.data;
        auto count = transactions.count * sizeof(Transaction);
        assert(cast(s64) count >= 0);
        ng::string stringified = {cast(s64) count, u8ified};
        write_entire_file(tprint("%toolgal/transactions.toolgal\0"_s, base_path), stringified, temp_allocator);
        
        saved_message_timer = 0;
    };
    
    auto save_tools = [&temp_allocator, &tprint, &global_tool_id_counter, &tool_storage, &base_path, &saved_message_timer] { // save tools
        auto mark = temp_allocator->get_mark();
        defer { temp_allocator->set_mark(mark); };
        
        ng::array<u8> file = {};
        defer { file.release(); };
        
        string_buffer_append(&file, "[1]\r\n"_s);
        string_buffer_append(&file, tprint("[%]\r\n"_s, global_tool_id_counter));
        
        auto loop_temp_marker = temp_allocator->get_mark();
        For (tool_storage) {
            defer { temp_allocator->set_mark(loop_temp_marker); };
            
            auto str = tprint("% `%` % %"_s, it.tool_id, it.name, it.inventory_total, cast(int) it.tool_size_type);
            
            string_buffer_append(&file, str);
            
            if (it.tool_box_number > 0 || it.drawer_number > 0 || it.image_filename) {
                auto str = tprint(" % %"_s, it.tool_box_number, it.drawer_number);
                
                string_buffer_append(&file, str);
            }
            
            if (it.image_filename) {
                file.push(' ');
                file.push('"');
                string_buffer_append(&file, it.image_filename);
                file.push('"');
            }
            
            file.push('\r');
            file.push('\n');
        }
        
        auto u8ified = file.data;
        auto count = file.count;
        ng::string stringified = {count, u8ified};
        write_entire_file(tprint("%toolgal/tools.toolgal\0"_s, base_path), stringified, temp_allocator);
        
        saved_message_timer = 0;
    };
    
    auto save_students = [&temp_allocator, &tprint, &instructors, &student_storage, &base_path, &saved_message_timer] { // save students
        auto mark = temp_allocator->get_mark();
        defer { temp_allocator->set_mark(mark); };
        
        ng::array<u8> file = {};
        defer { file.release(); };
        
        string_buffer_append(&file, "[1]\r\n"_s);
        for (int i = 1; i < instructors.count; i += 1) {
            string_buffer_append(&file, "["_s);
            string_buffer_append(&file, instructors[i]);
            string_buffer_append(&file, "]\r\n"_s);
        }
        
        auto loop_temp_marker = temp_allocator->get_mark();
        For (student_storage) {
            defer { temp_allocator->set_mark(loop_temp_marker); };
            if (!student_registered(&it)) continue;
            
            auto str = tprint("% % % %\r\n"_s, it.student_id, cast(s64) it.class_day, it.instructor, it.name);
            
            string_buffer_append(&file, str);
        }
        
        auto u8ified = file.data;
        auto count = file.count;
        ng::string stringified = {count, u8ified};
        write_entire_file(tprint("%toolgal/students.toolgal\0"_s, base_path), stringified, temp_allocator);
        
        saved_message_timer = 0;
    };
    auto save_all = [&save_transactions, &save_tools, &save_students] {
        save_transactions();
        save_tools();
        save_students();
    };
    
    auto update_tools_search = [&active_list, &active_page, &tools_page_state, &temp_allocator, &text_inputs, &text_input, &tool_storage, &scroll_index, &scroll_eased_pos] { // @Cutnpaste
        assert(active_list() == Ui_List::Tools); // @Temporary @Debug
        
        // assert(tools_page_state.current_state == Tool_Entry_State::None); // @Todo
        if (tools_page_state.current_state != Tool_Entry_State::None) return;
        assert(active_page != Ui_Page::Locked);
        
        auto mark = temp_allocator->get_mark();
        defer { temp_allocator->set_mark(mark); };
        
        tools_page_state.results.clear();
        
        auto push_new_tool_result_from_id = [](ng::array<Tool_Search_Result> *results, const Tool *tool) -> Tool_Search_Result * {
            auto new_result = results->push();
            if (!new_result) OutOfMem();
            new_result->tool_id = tool->tool_id;
            new_result->name = tool->name;
            return new_result;
        };
        
        auto query = utf32_to_utf8(&text_inputs[text_input], temp_allocator);
        
        tools_page_state.results.reserve(tool_storage.count);
        if (text_input >= 0 && text_inputs[text_input].count) {
            
            For (tool_storage) {
                auto name_query_matches = 0;
                
                auto splitting_query = query;
                while (true) {
                    auto criterion = split(splitting_query, ' ');
                    if (!criterion) break;
                    
                    auto name_searcher = it.name;
                    while (true) {
                        auto found = string_find(name_searcher, criterion, &name_searcher, &char_equal_case_insensitive);
                        if (!found) break;
                        name_query_matches += 1;
                    }
                }
                
                if (name_query_matches) {
                    auto new_result = push_new_tool_result_from_id(&tools_page_state.results, &it);
                    new_result->name_query_matches = name_query_matches;
                }
            }
        } else {
            // @Todo: if (text_input.count) { refine search; } else draw everyone;
            For (tool_storage) push_new_tool_result_from_id(&tools_page_state.results, &it);
        }
        
        const auto tool_sort_mode = tools_page_state.tool_sort_mode;
        const bool is_searching = text_input >= 0 && text_inputs[text_input].count;
        
        auto comparator = [&tool_storage, &tool_sort_mode, &is_searching, &query](const Tool_Search_Result *a, const Tool_Search_Result *b) -> s64 {
            
            // WE WANT THIS IN DESCENDING ORDER!!! Sigh, this was a bug that took days for me to understand. Unproductive programmer, I am!
            auto name_matches_compare = -(a->name_query_matches - b->name_query_matches);
            
            auto name_compare = ng::string_compare_case_insensitive(a->name, b->name);
            
            auto tool_a = get_tool(&tool_storage, a->tool_id);
            auto tool_b = get_tool(&tool_storage, b->tool_id);
            assert(tool_a);
            assert(tool_b);
            
            switch (tool_sort_mode) {
                
                default_bad;
                
                case Tool_Sort_Mode::Search_Matches: {
                    
                    if (is_searching) {
                        bool a_head_match = string_heads_match(a->name, query, &char_equal_case_insensitive);
                        bool b_head_match = string_heads_match(b->name, query, &char_equal_case_insensitive);
                        
                        auto head_match_compare = cast(int) a_head_match - cast(int) b_head_match;
                        if (head_match_compare) return -head_match_compare;
                        
                        if (name_matches_compare) return name_matches_compare;
                    } else {
                        // There is no search; fallback to name comparison.
                        if (name_compare) return name_compare;
                    }
                    
                } break;
                case Tool_Sort_Mode::Name: {
                    
                    if (name_compare) return name_compare;
                    
                } break;
                case Tool_Sort_Mode::Inventory: {
                    
                    bool overdue_a = false;
                    bool overdue_b = false;
                    For (tool_a->borrowers) if (it.overdue) overdue_a = true;
                    For (tool_b->borrowers) if (it.overdue) overdue_b = true;
                    
                    if (overdue_a && !overdue_b) return -1;
                    if (overdue_b && !overdue_a) return +1;
                    
                    //auto infinite_compare = ;
                    //if (infinite_compare) return -infinite_compare;
                    
                    if (tool_a->infinite && !tool_b->infinite) return +1;
                    if (tool_b->infinite && !tool_a->infinite) return -1;
                    
                    //auto ratio_a = safe_ratio(tool_a->inventory_available, tool_a->inventory_total);
                    //auto ratio_b = safe_ratio(tool_b->inventory_available, tool_b->inventory_total);
                    
                    //auto inventory_compare = 100 * ((ratio_a - ratio_b) / (ratio_a + ratio_b)); // @Hack @Temporary @Todo
                    //if (inventory_compare) return inventory_compare;
                    
                    auto owed_a = tool_a->inventory_total - tool_a->inventory_available;
                    auto owed_b = tool_b->inventory_total - tool_b->inventory_available;
                    
                    auto owed_compare = -(owed_a - owed_b);
                    if (owed_compare) return owed_compare;
                    
                } break;
                case Tool_Sort_Mode::Most_Loaned: {
                    
                    auto usage_compare = -(tool_a->times_loaned - tool_b->times_loaned);
                    if (usage_compare) return usage_compare;
                    
                } break;
            }
            if (name_matches_compare) return name_matches_compare;
            if (name_compare) return name_compare;
            auto name_compare_case_sensitive = ng::string_compare(a->name, b->name);
            if (name_compare_case_sensitive) return name_compare_case_sensitive;
            
            // At this point, basically everything about these two tools are identical, save for their IDs, which are necessarily different.
            return a->tool_id - b->tool_id;
        };
        quick_sort(&tools_page_state.results, comparator);
        
        scroll_index = 0;
        //scroll_eased_pos = get_scroll_target(1) * -0.25f; // good effect
    };
    
#define BORROWING_STRING ("borrowing:"_s)
    
    auto update_students_search = [&active_page, &active_list, &students_page_state, &temp_allocator, &text_inputs, &text_input, &tool_storage, &student_storage, &tprint, &scroll_index, &scroll_eased_pos] { // @Cutnpaste
        assert(active_list() == Ui_List::Students); // @Temporary @Debug
        
        students_page_state.listing_borrowers = 0;
        
        auto mark = temp_allocator->get_mark();
        defer { temp_allocator->set_mark(mark); };
        
        students_page_state.results.clear();
        
        auto push_new_student_result_from_id = [](ng::array<Student_Search_Result> *results, const Student *student) -> Student_Search_Result * {
            auto new_result = results->push();
            if (!new_result) OutOfMem();
            new_result->student_id = student->student_id;
            // new_result->first_name = student->first_name;
            // new_result->last_name = student->last_name;
            return new_result;
        };
        
        auto query = utf32_to_utf8(&text_inputs[text_input], temp_allocator);
        
        students_page_state.results.reserve(student_storage.count);
        if (text_input >= 0 && query) {
            
            auto mark = temp_allocator->get_mark();
            if (query.len >= BORROWING_STRING.len && string_heads_match(BORROWING_STRING, query, &char_equal_case_insensitive)) {
                
                auto id_string = query;
                id_string.ptr += BORROWING_STRING.len;
                id_string.len -= BORROWING_STRING.len;
                
                s64 query_as_id = 0;
                if (ng::str_to_s64(&id_string, &query_as_id) && query_as_id > 0) {
                    students_page_state.listing_borrowers = query_as_id; // @Robustness: This gets assigned even if the tool doesn't actually exist. Is this a bad idea?
                    
                    auto tool = get_tool(&tool_storage, query_as_id);
                    
                    if (tool) {
                        students_page_state.student_sort_mode = Student_Sort_Mode::Most_Owed;
                        
                        For (tool->borrowers) {
                            auto student = get_student(&student_storage, it.student_id);
                            assert(student);
                            auto new_result = push_new_student_result_from_id(&students_page_state.results, student);
                        }
                    } else {
                        // Tool is not in the database.
                    }
                } else {
                    // Did not parse a tool ID.
                }
                
            } else For (student_storage) {
                
                defer { temp_allocator->set_mark(mark); };
                
                bool first_name_query_match = false;
                bool last_name_query_match = false;
                bool id_query_match = false;
                
                auto first_name = it.first_name;
                auto last_name = it.last_name;
                
                auto id_string = tprint("%"_s, it.student_id);
                
                auto splitting_query = query;
                while (true) {
                    auto criterion = split(splitting_query, ' ');
                    if (!criterion) break;
                    
                    if (criterion.len <= first_name.len && string_heads_match(first_name, criterion, &char_equal_case_insensitive)) {
                        first_name_query_match = true;
                    }
                    if (criterion.len <= last_name.len && string_heads_match(last_name, criterion, &char_equal_case_insensitive)) {
                        last_name_query_match = true;
                    }
                    if (criterion.len <= id_string.len && string_heads_match(id_string, criterion, &char_equal_case_insensitive)) {
                        id_query_match = true;
                    }
                }
                
                if (first_name_query_match || last_name_query_match || id_query_match) {
                    auto new_result = push_new_student_result_from_id(&students_page_state.results, &it);
                    new_result->first_name_query_match = first_name_query_match;
                    new_result->last_name_query_match = last_name_query_match;
                    new_result->id_query_match = id_query_match;
                }
            }
        } else {
            // @Todo: if (text_input.count) { refine search; } else draw everyone;
            For (student_storage) push_new_student_result_from_id(&students_page_state.results, &it);
        }
        
        auto comparator = [&student_storage, &students_page_state](const Student_Search_Result *a, const Student_Search_Result *b) -> s64 {
            
            auto student_a = get_student(&student_storage, a->student_id);
            auto student_b = get_student(&student_storage, b->student_id);
            
            assert(student_a);
            assert(student_b);
            
            // auto first_name_compare = ng::string_compare_case_insensitive(a->first_name, b->first_name);
            // auto last_name_compare = ng::string_compare_case_insensitive(a->last_name, b->last_name);
            auto first_name_compare = ng::string_compare_case_insensitive(student_a->first_name, student_b->first_name);
            auto last_name_compare = ng::string_compare_case_insensitive(student_a->last_name, student_b->last_name);
            
            auto is_registered_compare = cast(int) student_registered(student_a) - cast(int) student_registered(student_b);
            
            auto query_compare = -(cast(int) a->first_name_query_match - cast(int) b->first_name_query_match);
            query_compare += -(cast(int) a->last_name_query_match - cast(int) b->last_name_query_match);
            query_compare += -(cast(int) a->id_query_match - cast(int) b->id_query_match);
            
            switch (students_page_state.student_sort_mode) {
                
                default_bad;
                
                case Student_Sort_Mode::First_Name: {
                    
                    //auto first_name_query_match_compare = cast(int) a->first_name_query_match - cast(int) b->first_name_query_match;
                    //if (first_name_query_match_compare) return -first_name_query_match_compare;
                    
                    if (first_name_compare) return first_name_compare;
                    
                } break;
                case Student_Sort_Mode::Last_Name: {
                    
                    //auto last_name_query_match_compare = cast(int) a->last_name_query_match - cast(int) b->last_name_query_match;
                    //if (last_name_query_match_compare) {
                    //ng::print("yeah.\n"_s);
                    //return -last_name_query_match_compare;
                    //}
                    
                    if (last_name_compare) return last_name_compare;
                    
                } break;
                case Student_Sort_Mode::Most_Owed: {
                    
                    bool overdue_a = false;
                    bool overdue_b = false;
                    For (student_a->owed_items) if (it.overdue) overdue_a = true;
                    For (student_b->owed_items) if (it.overdue) overdue_b = true;
                    
                    if (overdue_a && !overdue_b) return -1;
                    if (overdue_b && !overdue_a) return +1;
                    
                    int owed_a = 0;//student_a->owed_items.count;
                    int owed_b = 0;//student_b->owed_items.count;
                    For (student_a->owed_items) owed_a += it.quantity;
                    For (student_b->owed_items) owed_b += it.quantity;
                    
                    auto owed_compare = -(owed_a - owed_b);
                    if (owed_compare) return owed_compare;
                    
                } break;
                case Student_Sort_Mode::Instructor: {
                    
                    auto has_instructor_compare = -(cast(int)(student_a->instructor > 0) - cast(int)(student_b->instructor > 0));
                    if (has_instructor_compare) return has_instructor_compare;
                    
                    auto instructor_compare = -(student_a->instructor - student_b->instructor);
                    if (instructor_compare) return instructor_compare;
                    
                } break;
                case Student_Sort_Mode::Class_Day: {
                    
                    auto has_class_day_compare = -(cast(int)(student_a->class_day != Class_Day::None) - cast(int)(student_b->class_day != Class_Day::None));
                    if (has_class_day_compare) return has_class_day_compare;
                    
                    auto class_day_compare = (cast(int) student_a->class_day - cast(int) student_b->class_day);
                    if (class_day_compare) return class_day_compare;
                    
                } break;
            }
            
            if (is_registered_compare) return -is_registered_compare;
            if (query_compare) return query_compare;
            
            auto id_query_match_compare = cast(int) a->id_query_match - cast(int) b->id_query_match;
            if (id_query_match_compare) return -id_query_match_compare;
            
            if (last_name_compare) return last_name_compare;
            if (first_name_compare) return first_name_compare;
            
            auto name_compare = ng::string_compare_case_insensitive(student_a->name, student_b->name);
            if (name_compare) return name_compare;
            // See the comment at the end of the tools comparator.
            return ng::string_compare(student_a->name, student_b->name);
            
        };
        quick_sort(&students_page_state.results, comparator);
        
        if (active_page == Ui_Page::Transact) {
            s64 query_as_id = 0;
            // @Refactor: I hate having to make a new one every dang time i use str_to_s64.
            auto remainder = query; 
            
            bool is_only_a_number = ng::str_to_s64(&remainder, &query_as_id);
            For (remainder) if (it != ' ') is_only_a_number = false;
            
            if (is_only_a_number && query_as_id > 0) {
                bool already_exists = false;
                For (students_page_state.results) {
                    if (it.student_id == query_as_id) {
                        already_exists = true;
                    }
                }
                
                if (!already_exists) {
                    auto new_result = students_page_state.results.push();
                    new_result->student_id = query_as_id;
                    // new_result->last_name = "[Add This Student]"_s;
                }
            }
        }
        
        scroll_index = 0;
        //scroll_eased_pos = get_scroll_target(1) * -0.25f; // good effect
    };
    
    auto update_history_search = [&active_page, &active_list, &history_page_state, &temp_allocator, &text_inputs, &text_input, &tool_storage, &student_storage, &transactions, &scroll_index]() {
        
        assert(active_list() == Ui_List::History);
        assert(text_input == 0);
        
        history_page_state.results.clear();
        
        if (text_inputs[text_input].count <= 0) {
            
            For (transactions) {
                history_page_state.results.push(&it);
            }
            
        } else {
            auto mark = temp_allocator->get_mark();
            defer { temp_allocator->set_mark(mark); };
            
            auto query = utf32_to_utf8(&text_inputs[text_input], temp_allocator);
            
            ng::array<Student_Id> query_students = {};
            query_students.alloc = temp_allocator;
            
            ng::array<Tool_Id> query_tools = {}; // Reminder: Tool IDs are opaque to the user, so search queries don't consider them.
            query_tools.alloc = temp_allocator;
            
            ng::array<int> query_years = {};
            query_years.alloc = temp_allocator;
            
            bool query_loans = false;
            bool query_returns = false;
            
            bool query_any_months = false;
            bool query_months[12] = {};
            
            bool query_any_days = false;
            bool query_days[31] = {};
            
            bool query_any_days_of_week = false;
            bool query_days_of_week[7] = {};
            
            {
                ng::string month_strings[12] = {};
                for (int i = 0; i < 12; i += 1) {
                    month_strings[i] = get_month_string(i);
                }
                
                auto number_already_exists = [&](s64 n) -> bool {
                    assert(n > 0);
                    
                    if (n <= 31) return query_days[n - 1]; // Day of the month
                    For (query_students) if (it == n) return true; // Student ID number
                    For (query_years) if (it == n) return true; // Year
                    
                    return false;
                };
                
                auto splitting_query = query;
                while (true) {
                    
                    // We search tool names BEFORE splitting up the query.
                    // We do this each iteration to compare the remainder, starting with the FULL query,
                    // against the name of every tool.
                    // This way "Lathe dog 2019" will search for lathe dogs but not lathe kits.
                    // And "2019 lathe dog" will do the same.
                    // 2019-02-01
                    
                    For (tool_storage) {
                        if (splitting_query && string_heads_match(it.name, trim_whitespace(splitting_query, true, true), &char_equal_case_insensitive)) {
                            query_tools.push(it.tool_id);
                            
                            goto double_break;
                        }
                    }
                    
                    auto criterion = split(splitting_query, ' ');
                    if (!criterion) break;
                    
                    For (student_storage) {
                        if (criterion.len <= it.first_name.len && string_heads_match(it.first_name, criterion, &char_equal_case_insensitive)) {
                            query_students.push(it.student_id);
                        }
                        if (criterion.len <= it.last_name.len && string_heads_match(it.last_name, criterion, &char_equal_case_insensitive)) {
                            query_students.push(it.student_id);
                        }
                    }
                    
                    auto cmp = [](ng::string a, ng::string b) { return ng::string_compare_case_insensitive(a, b) == 0; };
                    
                    if (cmp(criterion, "loan"_s) || cmp(criterion, "loaned"_s) || cmp(criterion, "loans"_s)) {
                        query_loans = true;
                    }
                    if (cmp(criterion, "return"_s) || cmp(criterion, "returned"_s) || cmp(criterion, "returns"_s)) {
                        query_returns = true;
                    }
                    
                    for (int i = 0; i < 12; i += 1) {
                        
                        if (string_heads_match(month_strings[i], criterion, &char_equal_case_insensitive)) {
                            
                            query_any_months = true;
                            query_months[i] = true;
                            
                        }
                    }
                    
                    s64 as_number = 0;
                    auto num_parser = criterion;
                    if (ng::str_to_s64(&num_parser, &as_number) && as_number > 0) {
                        
                        if (!number_already_exists(as_number)) {
                            
                            // @Hack @Correctness: We assume that entering a number <= 31 represents a day of the month rather than a student ID. Sorry!
                            
                            if (as_number <= 31) { // Day of the month
                                
                                query_any_days = true;
                                query_days[as_number - 1] = true;
                                
                            } else if (get_student(&student_storage, as_number)) { // Student ID number
                                
                                query_students.push(as_number);
                                
                            } else { // Year
                                
                                query_years.push(as_number);
                            }
                            
                        }
                        
                    }
                }
                double_break:;
                
            }
            
            For (transactions) {
                
                auto time = decompose_unix_timestamp_gregorian(it.unix_timestamp, get_system_timezone_offset());
                
                bool match = true;
                
                if (query_any_months) {
                    if (!query_months[time.month]) match = false;
                }
                
                if (query_loans || query_returns) {
                    
                    if (query_returns && (it.flags & TRANSACTION_IS_RETURN)) {
                        // Still match.
                    } else if (query_loans && !(it.flags & TRANSACTION_IS_RETURN)) {
                        // Still match.
                    } else {
                        match = false;
                    }
                    
                }
                
                if (query_any_days) {
                    if (!query_days[time.day]) match = false;
                }
                
                if (query_students.count) {
                    
                    bool any_matches = false;
                    
                    for (auto &&it2 : query_students) {
                        if (it.student_id == it2) {
                            any_matches = true;
                        }
                    }
                    
                    if (!any_matches) match = false;
                    
                }
                
                if (query_tools.count) {
                    
                    bool any_matches = false;
                    
                    for (auto &&it2 : query_tools) {
                        if (it.tool_id == it2) {
                            any_matches = true;
                        }
                    }
                    
                    if (!any_matches) match = false;
                    
                }
                
                if (query_years.count) {
                    
                    bool any_matches = false;
                    
                    for (auto &&it2 : query_years) {
                        if (time.year == it2) {
                            any_matches = true;
                        }
                    }
                    
                    if (!any_matches) match = false;
                    
                }
                
                
                if (match) history_page_state.results.push(&it);
            }
        }
        
        scroll_index = 0;
    };
    
    auto get_num_list_items = [&active_list, &students_page_state, &tools_page_state, &history_page_state, &transactions] () -> s64 {
        // @Duplicate @Deduplicate @Cleanup @Refactor @Cutnpaste from below where you actually render the list
        
        if (active_list() == Ui_List::Tools) return tools_page_state.results.count;
        if (active_list() == Ui_List::Students) return students_page_state.results.count;
        if (active_list() == Ui_List::History) return history_page_state.results.count;
        return 0;
    };
    
    auto update_search = [&text_input, &active_list, &update_tools_search, &update_students_search, &update_history_search, &get_num_list_items] {
        if (text_input < 0) return;
        if (active_list() == Ui_List::None) return;
        
        int num_list_items = 0;
        
        if (active_list() == Ui_List::Tools) update_tools_search();
        if (active_list() == Ui_List::Students) update_students_search();
        if (active_list() == Ui_List::History) update_history_search();
    };
    
    auto resize_framebuffer = [&](SDL_Texture **tex) {
        SDL_DestroyTexture(*tex);
        *tex = SDL_CreateTexture(renderer, SDL_PIXELFORMAT_RGBA8888, SDL_TEXTUREACCESS_TARGET, ui->screen_size.x, ui->screen_size.y);
        SDL_SetTextureBlendMode(*tex, SDL_BLENDMODE_BLEND);
    };
    
    SDL_Texture *framebuffer = nullptr;
    SDL_Texture *fade_buffer = nullptr;
    
#define FRAMEBUFFER_FADE_TIME (0.100f)
    float framebuffer_fader = FRAMEBUFFER_FADE_TIME;
    
    resize_framebuffer(&framebuffer);
    resize_framebuffer(&fade_buffer);
    
    f64 invfreq = 1.0f / SDL_GetPerformanceFrequency();
    u64 prev = SDL_GetPerformanceCounter();
    
    begin_frame:;
    
    assert(!ui->obscured); // Somebody forgot to de-obscure.
    ui->obscured = false;
    
    defer { ui->obscured = false; };
    if (really_delete) {
        ui->obscured = true;
    }
    
#define RESTART_FRAME_NO_FADE() do { goto begin_frame; } while (0)
#define RESTART_FRAME() do { \
        if (framebuffer_fader >= FRAMEBUFFER_FADE_TIME) framebuffer_fader = 0; \
        RESTART_FRAME_NO_FADE(); \
    } while (0)
    
    ui->tis.changed = false;
    ui->tis.backspaced = false;
    ui->tis.cursor_moved = false;
    
    auto switch_text_input = [&active_page, &transact_page_state, &students_page_state, &tools_page_state, &text_input,
        //&ui_is_selecting(ui),
        &text_inputs, &ui, &update_search](int i, int direction = 0) {
        
        if (active_page == Ui_Page::Transact) {
            if (transact_page_state.current_state == Transact_Entry_State::None) {
                if (transact_page_state.who && i == 0) {
                    // Can't switch to here, you must click the little [x] button.
                    i = 1;
                }
            }
        } else if (active_page == Ui_Page::Students) {
            if (students_page_state.editing_student && i == 0) {
                // Can't switch to here, you must cancel out of editing.
                i = 1;
            }
        } else if (active_page == Ui_Page::Tools) {
            if (tools_page_state.current_state != Tool_Entry_State::None) {
                if (tools_page_state.entry_size_type != Tool_Size_Type::Unsized) {
                    if (i == 2) {
                        assert(direction);
                        i += direction < 0 ? -1 : +1;
                    }
                }
            }
        }
        
        text_input = i;
        
        ui->tis.cursor = 0;
        ui->tis.selection_cursor = 0;
        
        if (text_input >= 0) {
            
            // assert(!text_input_disabled[text_input]);
            
            ui->tis.cursor = text_inputs[text_input].count;
            //ui->tis.selection_cursor = 0;
            ui->tis.selection_cursor = ui->tis.cursor;
            
            if (active_page != Ui_Page::Locked) {
                update_search();
            }
        }
        
        ui->tis.cursor_moved = true;
        
        ui->tis.cursor_eased_pos = 0;
        ui->tis.cursor_eased_width = 0;
        
        ui->tis.backspace_timer = 0;
        
        ui->tis.blink_timer = 0;
    };
    
    auto clear_text_input = [&text_inputs, &text_input_errors, &text_input, &ui, &scroll_index, &scroll_eased_pos](ng::array<u32> *input) {
        assert(input);
        // cursor_hot_timer = 0;
        if (input == &text_inputs[text_input]) {
            ui->tis.changed = true;
            ui->tis.cursor_moved = true;
            ui->tis.cursor = 0;
            
            ui->tis.cursor_eased_pos = 0;
            ui->tis.cursor_eased_width = 0;
            
            ui->tis.backspace_timer = 0;
            
            ui->tis.selection_cursor = ui->tis.cursor;
            
            ui->tis.blink_timer = 0;
            
            scroll_index = 0;
            scroll_eased_pos = 0;
        }
        input->clear();
        auto i = input - text_inputs;
        text_input_errors[i].clear();
    };
    
    auto transact_page_push = [](ng::array<Pending_Tx> *txs, Tool *tool, Tool_Size tool_size) -> Pending_Tx * {
        //For (*txs) {
        //    assert(it.tool_id != tool->tool_id || !tool_size_equal(it.tool_size, tool_size));
        //}
        For (*txs) {
            if (it.tool_id == tool->tool_id && tool_size_equal(it.tool_size, tool_size)) {
                
                it.quantity += 1;
                
                return &it;
            }
        }
        
        auto new_loan = txs->push();
        new_loan->quantity = 1;
        new_loan->tool_id = tool->tool_id;
        new_loan->tool_size = tool_size;
        
        return new_loan;
    };
    
    auto transact_page_push_loan_or_go_to_size_menu = [&transact_page_state, &text_inputs, &transact_page_push, &clear_text_input, &switch_text_input](Tool *tool) {
        if (tool->tool_size_type != Tool_Size_Type::Unsized) {
            
            transact_page_state.which = tool->tool_id;
            
            transact_page_state.current_state = Transact_Entry_State::Size;
            
            For (text_inputs) clear_text_input(&it);
            
            switch_text_input(0);
            
            // return false;
        } else {
            Tool_Size tool_size = {};
            tool_size.size = 0;
            
            auto new_loan = transact_page_push(&transact_page_state.loans, tool, tool_size);
            assert(new_loan);
            
            clear_text_input(&text_inputs[1]);
            
            switch_text_input(-1);
            
            // return true;
        }
    };
    
    auto transact_page_push_loan_from_size_menu = [&transact_page_push, &switch_text_input, &text_inputs, &clear_text_input, &transact_page_state](Tool *tool, Tool_Size tool_size) {
        auto new_loan = transact_page_push(&transact_page_state.loans, tool, tool_size);
        assert(new_loan);
        
        switch_text_input(-1);
        
        For (text_inputs) clear_text_input(&it);
        transact_page_state.current_state = Transact_Entry_State::None;
    };
    
    auto transact_page_switch_to_add_student = [&text_inputs, &text_input, &temp_allocator, &students_page_state, &active_page, &switch_text_input, &clear_text_input, &tprint] {
        
        auto query = utf32_to_utf8(&text_inputs[text_input], temp_allocator);
        
        auto remainder = query;
        s64 student_id = 0;
        bool valid_id = ng::str_to_s64(&remainder, &student_id);
        assert(valid_id);
        
        // @Todo @Incomplete
        {
            // @Todo @Incomplete
            
            students_page_state.return_to_transact_page = true;
            
            // reset_pages();
            
            active_page = Ui_Page::Students;
            
            // @Hack: why do I need this to switch text inputs?.............
            switch_text_input(1);
            
            // @Todo: @Incomplete?
        }
        
        // students_page_state.editing_student = student_id;
        // // see "Edit Student"
        
        students_page_state.current_state = Student_Entry_State::New_Student;
        
        For (text_inputs) clear_text_input(&it);
        
        For (tprint("%"_s, student_id)) text_inputs[0].push(it);
        
        // @Hack @Todo @Incomplete
    };
    
    // Switch to a new page and clear everything else.
    auto switch_to_page = [&ui, &active_page, &student_storage, &tools_page_state, &students_page_state, &transact_page_state, &history_page_state, &scroll_eased_pos, &scroll_index, &text_inputs, &clear_text_input, &text_input](Ui_Page new_page) {
        if (new_page != Ui_Page::Students) { // Leaving students page? Then delete all the checkmarks.
            For (student_storage) it.checkmarked = false;
        }
        
        active_page = new_page;
        
        tools_page_state_reset(&tools_page_state);
        students_page_state_reset(&students_page_state);
        transact_page_state_reset(&transact_page_state);
        history_page_state_reset(&history_page_state);
        
        scroll_eased_pos = 0;
        scroll_index = 0;
        
        For (text_inputs) clear_text_input(&it);
        text_input = -1;
        
        ui->dragging_scrollbar = nullptr; // @Volatile @Refactor
    };
    
    auto can_loan = [&](Tool *tool) -> bool {
        auto can_loan = true;
        
        if (tool->inventory_available <= 0) can_loan = false;
        if (tool->infinite) can_loan = true;
        
        // Obsoleted by adjustable tool_sizes IF it has a size.
        // I used to pretend it's always obsoleted and it made a bug, lol.
        if (tool->tool_size_type == Tool_Size_Type::Unsized) {
            For (transact_page_state.loans) {
                if (it.tool_id == tool->tool_id) {
                    can_loan = false;
                    break;
                }
            }
            For (transact_page_state.returns) {
                if (it.tool_id == tool->tool_id) {
                    can_loan = false;
                    break;
                }
            }
        } else {
            assert(tool->infinite);
        }
        
        return can_loan;
    };
    
    // :KeyboardToButtons
    // Returns whether to restart the frame.
    auto progress_data_entry = [&]() -> bool {
        auto parse_integer = [&] (ng::string str, s64 *result, ng::array<u8> *errbuf) -> bool {
            auto reader = str;
            s64 x = 0;
            if (ng::str_to_s64(&reader, &x)) {
                bool extra_junk = false;
                
                auto tail = reader;
                For (tail) {
                    if (it != ' ') {
                        extra_junk = true;
                        break;
                    }
                }
                
                str.len = (tail.ptr - str.ptr);
                
                if (extra_junk) { // @Todo: good error handling.
                    if (errbuf) {
                        errbuf->clear();
                        string_buffer_append(errbuf, tprint("You can't type extra junk after \"%\", sorry."_s, str));
                    }
                    return false;
                }
            } else { // @Todo: good error handling.
                // @Todo: Say when the number's too big, as distinct from when there were nonsense characters.
                
                if (errbuf) {
                    errbuf->clear();
                    string_buffer_append(errbuf, "This isn't a valid number, sorry."_s);
                }
                return false;
            }
            
            *result = x;
            return true;
        };
        auto parse_float = [&] (ng::string str, f64 *result, ng::array<u8> *errbuf) -> bool {
            auto reader = str;
            f64 x = 0;
            if (ng::str_to_f64(&reader, &x)) {
                bool extra_junk = false;
                
                auto tail = reader;
                For (tail) {
                    if (it != ' ') {
                        extra_junk = true;
                        break;
                    }
                }
                
                if (extra_junk) { // @Todo: good error handling.
                    
                    if (errbuf) {
                        errbuf->clear();
                        string_buffer_append(errbuf, tprint("You can't type extra junk after \"%\", sorry."_s, str));
                    }
                    return false;
                }
            } else { // @Todo: good error handling.
                // @Todo: Say when the number's too big, as distinct from when there were nonsense characters.
                
                if (errbuf) {
                    errbuf->clear();
                    string_buffer_append(errbuf, "This isn't a valid number, sorry."_s);
                }
                return false;
            }
            
            *result = x;
            return true;
        };
        
        auto transact_page_begin_loaning_tool = [&] {
            
            // @Cutnpaste @Refactor
            // @Cutnpaste @Refactor
            // @Cutnpaste @Refactor
            // @Cutnpaste @Refactor
            // @Cutnpaste @Refactor
            // @Cutnpaste @Refactor
            // @Cutnpaste @Refactor
            auto search_results = &tools_page_state.results;
            
            if (search_results->count) {
                assert(search_results->count > 0);
                
                auto tool_id = (*search_results)[scroll_index].tool_id;
                
                auto tool = get_tool(&tool_storage, tool_id);
                assert(tool);
                
                if (can_loan(tool)) {
                    
                    if (active_page != Ui_Page::Transact) {
                        switch_to_page(Ui_Page::Transact);
                    }
                    
                    
                    transact_page_push_loan_or_go_to_size_menu(tool);
                }
            }
            
        };
        
        auto transact_page_begin_transacting_student = [&] {
            
            auto search_results = &students_page_state.results;
            
            if (search_results->count) {
                assert(search_results->count > 0);
                
                auto result = &(*search_results)[scroll_index];
                
                auto student = get_student(&student_storage, result->student_id);
                // assert(student); // @Temporary
                if (!student) { // @Todo @Incomplete
                    
                    assert(active_page == Ui_Page::Transact);
                    
                    transact_page_switch_to_add_student();
                } else {
                    
                    if (active_page != Ui_Page::Transact) {
                        switch_to_page(Ui_Page::Transact);
                    }
                    
                    transact_page_state.who = result->student_id;
                    
                    clear_text_input(&text_inputs[0]);
                    
                    switch_text_input(-1);
                }
            }
            
        };
        
        For (text_input_errors) it.clear();
        
        if (active_page == Ui_Page::Tools) {
            Tool *editing_tool = nullptr;
            if (tools_page_state.editing_tool) {
                editing_tool = get_tool(&tool_storage, tools_page_state.editing_tool);
                assert(editing_tool);
            }
            
            auto finish_tool_entry = [&](ng::string name, s64 inventory, Tool_Size_Type tool_size_type, s64 tool_box_number, s64 drawer_number, ng::string image_filename) {
                
                auto tool = editing_tool;
                if (!tool) {
                    auto new_tool = tool_storage.push();
                    
                    if (!new_tool) OutOfMem();
                    
                    new_tool->tool_id = get_next_tool_id(&global_tool_id_counter);
                    
                    tool = new_tool;
                }
                tool->inventory_total = inventory;
                if (tool->inventory_total == 0) {
                    tool->infinite = true;
                } else {
                    tool->infinite = false;
                }
                
                tool->tool_size_type = tool_size_type;
                
                tool->tool_box_number = tool_box_number;
                tool->drawer_number = drawer_number;
                
                ng::free_string(&tool->name, ng::default_allocator); // @Allocator // Free old name.
                tool->name = name;
                
                ng::free_string(&tool->image_filename, ng::default_allocator); // @Allocator // Free old name.
                tool->image_filename = image_filename;
                
                postprocess(&student_storage, &tool_storage, &transactions, &num_tools_loaned, &total_items_owed, &total_overdue);
                
                save_tools(); // :Save
                // save_all(); // :Save
                
                tools_page_state_reset(&tools_page_state);
                
                For (text_inputs) clear_text_input(&it);
                
                switch_text_input(0);
            };
            
            // @Todo: Now that I have turned tool creation/editing into a single-page
            // form, I ought to simplify a lot of the data surrounding it.
            // For example, I don't need to store any state at all the way I
            // currently am with tool_entry_data and whatnot.
            // The string inputs uniquely identify the set of data to be entered
            // and so the entirety of tool creation/editing can be performed
            // over the course of a single block scope in the middle of a
            // frame. 2018-06-21
            
            switch (tools_page_state.current_state) { 
                case Tool_Entry_State::None: {
                    
                    transact_page_begin_loaning_tool();
                    
                } break;
                case Tool_Entry_State::New_Tool:
                case Tool_Entry_State::Edit_Tool: {
                    
                    auto &&name_input = text_inputs[0];
                    // text_inputs[1] is unused here for @Hack reasons.
                    auto &&inventory_input = text_inputs[2];
                    auto &&tool_box_number_input = text_inputs[3];
                    auto &&drawer_number_input = text_inputs[4];
                    auto &&image_filename_input = text_inputs[5];
                    
                    bool name_is_good = false;
                    
                    s64 inventory = 0;
                    bool inventory_is_good = false;
                    
                    
                    { // name input
                        bool name_is_empty = true;
                        For (name_input) {
                            if (it != ' ') {
                                name_is_empty = false;
                                break;
                            }
                        }
                        
                        if (name_is_empty) {
                            
                            auto errbuf = &text_input_errors[0];
                            errbuf->clear();
                            string_buffer_append(errbuf, "Enter a name."_s);
                        } else {
                            // @Todo: verify that the name is unique etc... Or not? @V0.02 maybe?
                            
                            bool has_quotations = false;
                            For (name_input) {
                                if (it == '`') {
                                    has_quotations = true;
                                    break;
                                }
                            }
                            
                            if (has_quotations) {
                                
                                auto errbuf = &text_input_errors[0];
                                errbuf->clear();
                                string_buffer_append(errbuf, "Tool names can't have `grave ticks`, sorry."_s);
                            } else {
                                
                                name_is_good = true;
                                
                            }
                        }
                    }
                    if (tools_page_state.entry_size_type == Tool_Size_Type::Unsized) { // inventory input
                        auto new_inventory_string = utf32_to_utf8(&inventory_input, temp_allocator);
                        
                        if (!new_inventory_string) {
                            
                            auto errbuf = &text_input_errors[2];
                            errbuf->clear();
                            string_buffer_append(errbuf, "Please enter an initial inventory."_s);
                            
                        } else {
                            if (parse_integer(new_inventory_string, &inventory, &text_input_errors[2])) { // @Volatile
                                if (inventory >= 0) {
                                    
                                    inventory_is_good = true;
                                    
                                } else {
                                    
                                    auto errbuf = &text_input_errors[2];
                                    errbuf->clear();
                                    string_buffer_append(errbuf, "Cannot have a negative inventory."_s);
                                }
                            }
                        }
                    } else {
                        inventory = 0;
                        inventory_is_good = true;
                    }
                    
                    bool tool_box_number_is_good = false;
                    s64 tool_box_number = 0;
                    if (tool_box_number_input.count > 0) {
                        
                        auto tool_box_number_string = utf32_to_utf8(&tool_box_number_input, temp_allocator);
                        if (parse_integer(tool_box_number_string, &tool_box_number, &text_input_errors[3])) { // @Volatile
                            if (tool_box_number > 0) {
                                
                                tool_box_number_is_good = true;
                                
                            } else {
                                
                                auto errbuf = &text_input_errors[3];
                                errbuf->clear();
                                string_buffer_append(errbuf, "This should either be at least 1, or left empty."_s);
                            }
                        }
                        
                    } else {
                        
                        tool_box_number_is_good = true;
                        
                    }
                    
                    bool drawer_number_is_good = false;
                    s64 drawer_number = 0;
                    if (drawer_number_input.count > 0) {
                        
                        auto drawer_number_string = utf32_to_utf8(&drawer_number_input, temp_allocator);
                        if (parse_integer(drawer_number_string, &drawer_number, &text_input_errors[4])) { // @Volatile
                            if (drawer_number > 0) {
                                
                                drawer_number_is_good = true;
                                
                            } else {
                                
                                auto errbuf = &text_input_errors[4];
                                errbuf->clear();
                                string_buffer_append(errbuf, "This should either be at least 1, or left empty."_s);
                            }
                        }
                        
                    } else {
                        
                        drawer_number_is_good = true;
                        
                    }
                    
                    bool image_filename_is_good = true;
                    For (image_filename_input) {
                        if (it == '"') {
                            auto errbuf = &text_input_errors[5]; // @Volatile
                            errbuf->clear();
                            string_buffer_append(errbuf, "The filename can't have \"quotation marks\" in it, sorry."_s);
                            
                            image_filename_is_good = false;
                            break;
                        }
                    }
                    
                    if (name_is_good && inventory_is_good && tool_box_number_is_good && drawer_number_is_good && image_filename_is_good) {
                        
                        // @Todo @Cleanup: move this into finish_new_tool_entry
                        auto new_name = utf32_to_utf8(&name_input, ng::default_allocator); // @Allocator: this needs to be heap allocated.
                        
                        ng::string image_filename = {};
                        if (image_filename_is_good) {
                            image_filename = utf32_to_utf8(&image_filename_input, ng::default_allocator); // @Allocator: this needs to be heap allocated.
                        }
                        
                        finish_tool_entry(new_name, inventory, tools_page_state.entry_size_type, tool_box_number, drawer_number, image_filename);
                    }
                } break;
                default_bad;
            }
        } else if (active_page == Ui_Page::Students) {
            Student *editing_student = nullptr;
            if (students_page_state.editing_student) {
                editing_student = get_student(&student_storage, students_page_state.editing_student);
                assert(editing_student);
            }
            
            auto finish_student_entry = [&](ng::string name, Student_Id student_id) {
                
                auto student = editing_student;
                if (!student) {
                    auto new_student = student_storage.push();
                    if (!new_student) OutOfMem();
                    
                    new_student->student_id = student_id;
                    
                    student = new_student;
                }
                
                ng::free_string(&student->name, ng::default_allocator); // @Allocator // Free old name.
                student->name = name;
                
                postprocess(&student_storage, &tool_storage, &transactions, &num_tools_loaned, &total_items_owed, &total_overdue);
                
                save_students(); // :Save
                // save_all(); // :Save
                
                bool should_return = students_page_state.return_to_transact_page;
                
                students_page_state_reset(&students_page_state);
                
                For (text_inputs) clear_text_input(&it);
                
                if (should_return) {
                    active_page = Ui_Page::Transact;
                    
                    transact_page_state.who = student_id;
                    
                    switch_text_input(1);
                } else {
                    switch_text_input(0);
                }
            };
            
            switch (students_page_state.current_state) { 
                case Student_Entry_State::None: {
                    
                    transact_page_begin_transacting_student();
                    
                } break;
                case Student_Entry_State::New_Student:
                case Student_Entry_State::Edit_Student: {
                    
                    s64 student_id = 0;
                    bool id_is_good = false;
                    bool name_is_good = false;
                    
                    auto id_input = &text_inputs[0];
                    auto name_input = &text_inputs[1];
                    
                    if (students_page_state.current_state == Student_Entry_State::Edit_Student) { // Can't modify ID in edit mode.
                        
                        student_id = students_page_state.editing_student;
                        id_is_good = true;
                        
                    } else { // id input
                        auto student_id_string = utf32_to_utf8(&text_inputs[0], ng::default_allocator);
                        defer { ng::free_string(&student_id_string, ng::default_allocator); };
                        
                        if (!student_id_string) {
                            
                            auto errbuf = &text_input_errors[0];
                            errbuf->clear();
                            string_buffer_append(errbuf, "Please enter a student ID."_s);
                        } else {
                            if (parse_integer(student_id_string, &student_id, &text_input_errors[0])) {
                                Student *matched = false;
                                
                                For (student_storage) {
                                    if (it.student_id == student_id) {
                                        matched = &it;
                                        break;
                                    }
                                }
                                
                                if (matched) {
                                    
                                    auto errbuf = &text_input_errors[0];
                                    errbuf->clear();
                                    string_buffer_append(errbuf, tprint("% already has that ID."_s, matched->name));
                                } else if (student_id > 0) {
                                    
                                    id_is_good = true;
                                    
                                } else if (student_id == 0) {
                                    
                                    auto errbuf = &text_input_errors[0];
                                    errbuf->clear();
                                    string_buffer_append(errbuf, "Student IDs cannot be zero, sorry."_s);
                                } else {
                                    
                                    auto errbuf = &text_input_errors[0];
                                    errbuf->clear();
                                    string_buffer_append(errbuf, "Student IDs cannot be negative, sorry."_s);
                                }
                            }
                        }
                    }
                    
                    { // name input
                        bool name_is_empty = true;
                        For (text_inputs[1]) if (it != ' ') name_is_empty = false;
                        
                        if (name_is_empty) {
                            
                            auto errbuf = &text_input_errors[1];
                            errbuf->clear();
                            string_buffer_append(errbuf, "Please enter a student name."_s);
                        } else {
                            // @Todo: verify that the name is unique etc... Or not?
                            
                            name_is_good = true;
                        }
                    }
                    
                    
                    if (id_is_good && name_is_good) {
                        
                        // @Todo @Cleanup: move this into finish_student_entry
                        auto new_name = utf32_to_utf8(&text_inputs[1], ng::default_allocator); // @Allocator
                        finish_student_entry(new_name, student_id);
                    }
                } break;
                case Student_Entry_State::Class_Day_And_Instructor: {
                    For (student_storage) {
                        if (!it.checkmarked) continue;
                        
                        it.class_day = students_page_state.selected_class_day;
                        it.instructor = students_page_state.selected_instructor;
                    }
                    
                    save_students();
                    
                    // reset pages
                    For (student_storage) it.checkmarked = false;
                    
                    switch_to_page(Ui_Page::Students);
                    
                    For (text_inputs) clear_text_input(&it);
                    
                    switch_text_input(0);
                } break;
                
                case Student_Entry_State::New_Or_Editing_Instructor: {
                    bool name_good = true;
                    
                    For (text_inputs[0]) {
                        if (it == '[' || it == ']') {
                            auto errbuf = &text_input_errors[0];
                            errbuf->clear();
                            string_buffer_append(errbuf, "Names cannot have [square brackets], sorry."_s);
                            name_good = false;
                        }
                    }
                    if (name_good) {
                        
                        ng::string *instructor = nullptr;
                        
                        if (students_page_state.new_or_editing_instructor > 0) {
                            
                            instructor = &instructors[students_page_state.new_or_editing_instructor];
                            ng::free_string(instructor, ng::default_allocator);
                            
                        } else {
                            instructor = instructors.push();
                        }
                        
                        *instructor = utf32_to_utf8(&text_inputs[0], ng::default_allocator);
                        
                        // reset pages
                        For (text_inputs) clear_text_input(&it);
                        switch_text_input(-1);
                        
                        postprocess(&student_storage, &tool_storage, &transactions, &num_tools_loaned, &total_items_owed, &total_overdue);
                        
                        save_students();
                        
                        students_page_state.current_state = Student_Entry_State::Class_Day_And_Instructor;
                    }
                } break;
                
                default_bad;
            }
        } else if (active_page == Ui_Page::Transact) {
            
            if (transact_page_state.current_state == Transact_Entry_State::None) {
                // @Cleanup @Refactor: Basically all of the Ui_Page::Transact code in progress_data_entry should be deduplicated with the actual IMGUI button code.
                // :KeyboardToButtons
                // @Cutnpaste @Refactor
                // @Cutnpaste @Refactor
                if (text_input == 0) {
                    if (students_page_state.results.count > 0) {
                        
                        transact_page_begin_transacting_student();
                        
                        return true;
                    } else {
                        return false;
                    }
                } else if (text_input == 1) {
                    
                    transact_page_begin_loaning_tool();
                    
                } else assert(text_input == -1);
            } else if (transact_page_state.current_state == Transact_Entry_State::Size) {
                
                auto next_token = [](ng::string &s) -> ng::string {
                    s = skip_whitespace(s);
                    if (!s) {
                        return s;
                    }
                    
                    auto result = s;
                    if (isalnum(result[0])) {
                        // Scan until either 1. we stop seeing alphanumerics, or 2. we were alphabetical before and start seeing numerics or vice versa.
                        bool alpha = isalpha(s[0]);
                        while (s && isalnum(s[0]) && (alpha == isalpha(s[0]))) {
                            ++s;
                        }
                    } else {
                        ++s;
                    }
                    result.len = s.ptr - result.ptr;
                    return result;
                };
                
                auto entry_error = [&](ng::string msg, ng::array<u8> *errbuf) -> bool {
                    auto alloc = temp_allocator;
                    assert(errbuf);
                    errbuf->clear();
                    string_buffer_append(errbuf, msg);
                    return false;
                };
                
                auto parse_in = [&](ng::string s, f64 *inch, ng::array<u8> *errbuf) -> bool {
                    assert(inch);
                    
                    auto check_mm_or_in = [&](ng::string tok) {
                        if (!tok || tok == "in"_s || tok == "\""_s || tok == "inch"_s || tok == "inches"_s) {
                            return true; // We're straight.
                        } if (tok == "mm"_s || tok == "millimeter"_s || tok == "millimetre"_s || tok == "millimeters"_s || tok == "millimetres"_s) {
                            return entry_error("This tool is not sized in millimetres!"_s, errbuf);
                        }
                        return entry_error(tprint("You can't type extra junk after the number, sorry: \"%\""_s, tok), errbuf);
                    };
                    
                    auto is_pow_of_2 = [](s64 x) -> bool {
                        if (x <= 0) return false;
                        return !(x & (x - 1));
                    };
                    
                    s64 whole_part = 0;
                    s64 numerator = 0;
                    s64 denominator = 1;
                    
                    auto tok = next_token(s);
                    
                    if (!tok) {
                        return entry_error("Please enter a valid number, such as 1 3/8."_s, errbuf);
                    }
                    if (!parse_integer(tok, &whole_part, nullptr)) {
                        return entry_error(tprint("Please enter a valid number, such as 1 3/8. \"%\" is not valid."_s, tok), errbuf);
                    }
                    if (whole_part < 0) {
                        return entry_error(tprint("Please enter a valid number, not a negative one such as \"%\"."_s, tok), errbuf);
                    }
                    
                    tok = next_token(s);
                    
                    if (tok == "/"_s) {
                        // We actually just have the fraction, no whole part.
                        
                        tok = next_token(s);
                        
                        if (!tok) {
                            return entry_error(tprint("Please enter a valid fraction."_s, tok), errbuf);
                        } else if (!parse_integer(tok, &denominator, nullptr) || !denominator) {
                            return entry_error(tprint("Please enter a valid fraction. \"%\" is not valid."_s, tok), errbuf);
                        }
                        if (denominator < 0) {
                            return entry_error(tprint("Please enter a valid number, not a negative one such as \"%\"."_s, tok), errbuf);
                        }
                        if (!is_pow_of_2(denominator)) {
                            return entry_error(tprint("Please enter a fraction with a denominator that is a power of 2 (4, 8, 16, 32, etc.). % is not a power of 2."_s, denominator), errbuf);
                        }
                        
                        numerator = whole_part;
                        whole_part = 0;
                        
                        tok = next_token(s);
                        if (!check_mm_or_in(tok)) {
                            return false;
                        }
                        if (next_token(s)) {
                            return entry_error(tprint("You can't type extra junk after the number, sorry: \"%\""_s, tok), errbuf);
                        }
                        
                    } else if (tok && parse_integer(tok, &numerator, nullptr)) {
                        
                        if (numerator < 0) {
                            return entry_error(tprint("Please enter a valid numerator, not a negative one such as \"%\"."_s, tok), errbuf);
                        }
                        
                        // @Cutnpaste @Copypaste @Redunant with above
                        tok = next_token(s);
                        
                        if (tok == "/"_s) {
                            
                            tok = next_token(s);
                            
                            if (!tok) {
                                return entry_error("Please enter a valid fraction."_s, errbuf);
                            } else if (!parse_integer(tok, &denominator, nullptr) || !denominator) {
                                return entry_error(tprint("Please enter a valid fraction. \"%\" is not valid."_s, tok), errbuf);
                            }
                            if (!is_pow_of_2(denominator)) {
                                return entry_error(tprint("Please enter a fraction with a denominator that is a power of 2 (4, 8, 16, 32, etc.). % is not a power of 2."_s, denominator), errbuf);
                            }
                            
                            tok = next_token(s);
                            if (!check_mm_or_in(tok)) {
                                return false;
                            }
                            tok = next_token(s);
                            if (tok) {
                                return entry_error(tprint("You can't type extra junk after the number, sorry: \"%\""_s, tok), errbuf);
                            }
                            
                        } else if (tok) {
                            return entry_error(tprint("A fraction was expected, not \"%\"."_s, tok), errbuf);
                        } else {
                            return entry_error("A fraction was expected."_s, errbuf);
                        }
                    } else if (!check_mm_or_in(tok)) {
                        return false;
                    }
                    tok = next_token(s);
                    if (tok) {
                        return entry_error(tprint("You can't type extra junk after the number, sorry: \"%\""_s, tok), errbuf);
                    }
                    
                    *inch = numerator;
                    *inch /= denominator;
                    *inch += whole_part;
                    return true;
                };
                auto parse_mm = [&](ng::string s, f64 *mm, ng::array<u8> *errbuf) -> bool {
                    assert(mm);
                    
                    auto full_line = s;
                    if (!ng::str_to_f64(&s, mm)) {
                        return entry_error(tprint("Please enter a valid measure in millimetres. \"%\" is not valid."_s, full_line), errbuf);
                    }
                    
                    auto tok = next_token(s);
                    if (!tok || tok == "mm"_s || tok == "millimeters"_s || tok == "millimetres"_s) {
                        // We're straight.
                    } else {
                        return entry_error(tprint("You can't type extra junk after the number, sorry: \"%\""_s, tok), errbuf);
                    }
                    
                    while (s && s[0] == ' ') ++s;
                    if (s) {
                        return entry_error(tprint("You can't type extra junk after the number, sorry: \"%\""_s, s), errbuf);
                    }
                    
                    return true;
                };
                
                bool success = true;
                
                auto tool = get_tool(&tool_storage, transact_page_state.which);
                assert(tool);
                Tool_Size tool_size = {};
                
                switch (tool->tool_size_type) {
                    case Tool_Size_Type::Unsized: {
                        // We shouldn't be on this menu.
                        assert(0);
                    } break;
                    case Tool_Size_Type::Imperial: {
                        auto input = utf32_to_utf8(&text_inputs[0], temp_allocator);
                        
                        f64 size_in = 0;
                        
                        if (parse_in(input, &size_in, &text_input_errors[0])) {
                            
                            tool_size.size = cast(f32) size_in;
                            
                        } else {
                            success = false;
                        }
                        
                    } break;
                    case Tool_Size_Type::Metric: {
                        auto input = utf32_to_utf8(&text_inputs[0], temp_allocator);
                        
                        f64 size_mm = 0;
                        
                        if (parse_mm(input, &size_mm, &text_input_errors[0])) {
                            
                            tool_size.size = cast(f32) size_mm;
                            
                        } else {
                            success = false;
                        }
                        
                    } break;
                    case Tool_Size_Type::Thread_Imperial: {
                        auto input = utf32_to_utf8(&text_inputs[0], temp_allocator);
                        auto input2 = utf32_to_utf8(&text_inputs[1], temp_allocator);
                        
                        f64 diameter_in = 0;
                        s64 threads_per_in = 0;
                        
                        if (parse_in(input, &diameter_in, &text_input_errors[0])) {
                            
                            tool_size.diameter_in = cast(f32) diameter_in;
                            
                        } else {
                            success = false;
                        }
                        if (parse_integer(input2, &threads_per_in, &text_input_errors[1])) {
                            
                            tool_size.threads_per_in = threads_per_in;
                            
                        } else {
                            success = false;
                        }
                        
                    } break;
                    case Tool_Size_Type::Thread_Metric: {
                        auto input = utf32_to_utf8(&text_inputs[0], temp_allocator);
                        auto input2 = utf32_to_utf8(&text_inputs[1], temp_allocator);
                        
                        f64 diameter_mm = 0;
                        f64 pitch_mm = 0;
                        
                        if (parse_mm(input, &diameter_mm, &text_input_errors[0])) {
                            
                            tool_size.diameter_mm = cast(f32) diameter_mm;
                            
                        } else {
                            success = false;
                        }
                        if (parse_mm(input2, &pitch_mm, &text_input_errors[1])) {
                            
                            tool_size.pitch_mm = cast(f32) pitch_mm;
                            
                        } else {
                            success = false;
                        }
                        
                    } break;
                    default_bad;
                }
                
                if (success) {
                    transact_page_push_loan_from_size_menu(tool, tool_size);
                    
                    For (text_inputs) clear_text_input(&it);
                    switch_text_input(-1);
                }
            } else assert(0);
        }
        
        return false; // @Temporary @Todo: go in and return true for all of the appropriate times
    };
    
    ui->temp_allocator->free_all();
    
    {
        auto page_string = ""_s;
        if (active_page == Ui_Page::Tools) page_string = "Tools Page"_s;
        else if (active_page == Ui_Page::Students) page_string = "Students Page"_s;
        else if (active_page == Ui_Page::Transact) page_string = "Transact Page"_s;
        else if (active_page == Ui_Page::History) page_string = "History Page"_s;
        else if (active_page == Ui_Page::Locked) page_string = "Lock Screen"_s;
        else assert(0);
        SDL_SetWindowTitle(window, cast(char *) tprint("% - ToolGal v" STR_VERSION "\0"_s, page_string).ptr);
    }
    
    //SDL_SetRenderTarget(renderer, framebuffer);
    
    // @Todo: Move all code from inside SDL input-handling over to the update area of the tick loop.
    // :KeyboardToButtons
    
    // auto comparator = [](const Tool *a, const Tool *b) -> s64 {
    //     return ng::string_compare(a->name, b->name);
    // };
    // quick_sort(&tools, comparator);
    
    Input_State input_state = {};
    {
        int mx = 0;
        int my = 0;
        u32 mbuttons = SDL_GetMouseState(&mx, &my);
        input_state.mouse_pos = v2_screen_to_world(v2(mx + 0.5f, my + 0.5f), ui->screen_size);
        input_state.lmb = (mbuttons & SDL_BUTTON(SDL_BUTTON_LEFT)) != 0;
        input_state.rmb = (mbuttons & SDL_BUTTON(SDL_BUTTON_RIGHT)) != 0;
    }
    
    if (text_input >= 0) {
        //assert(ui->tis.cursor >= 0);
        //assert(ui->tis.cursor <= text_inputs[text_input].count);
        //assert(ui->tis.selection_cursor >= 0);
        //assert(ui->tis.selection_cursor <= text_inputs[text_input].count);
        ui->tis.cursor = ng::clamp(ui->tis.cursor, 0, text_inputs[text_input].count);
        ui->tis.selection_cursor = ng::clamp(ui->tis.selection_cursor, 0, text_inputs[text_input].count);
    }
    
    ng::string text_input_queries[countof(text_inputs)] = {};
    ng::string text_input_query_tooltips[countof(text_inputs)] = {};
    int num_text_inputs = 1;
    
    switch (active_page) {
        
        case Ui_Page::Locked: {
            num_text_inputs = 1;
            text_input_queries[0] = "Enter password: "_s;
            text_input_query_tooltips[0] =
#if USE_PASSWORD
            "The program is locked."
#else
            "This software can be custom-distributed for certain passwords."
#endif
            ""_s;
        } break;
        
        
        case Ui_Page::Tools: {
            num_text_inputs = 1;
            switch (tools_page_state.current_state) {
                case Tool_Entry_State::None: {
                    text_input_queries[0] = "Search tools: "_s;
                    text_input_query_tooltips[0] =
                        "Search for a tool using its\n"
                        "name or parts of its name."_s;
                } break;
                case Tool_Entry_State::New_Tool:
                case Tool_Entry_State::Edit_Tool: {
                    num_text_inputs = 6;
                    
                    text_input_queries[0] = "Tool's name?: "_s;
                    text_input_query_tooltips[0] =
                        "Enter the name of the tool."_s;
                    
                    // Input [1] is special-case magic, for buttons.
                    text_input_queries[1] =
                        "Tool size type?: "_s;
                    text_input_query_tooltips[1] =
                        "Specify what size measurements this\n"
                        "tool has, or none at all. The program\n"
                        "does not track sized tool inventory."_s;
                    
                    text_input_queries[2] = "Total inventory? (0 for infinite): "_s;
                    text_input_query_tooltips[2] =
                        "Enter how many items the program will\n"
                        "track. Students can't loan the tool if\n"
                        "this number runs out. To force the program\n"
                        "to ignore inventory, enter 0 for effectively\n"
                        "\"infinite\" inventory."_s;
                    
                    text_input_queries[3] = "Tool box number (optional): "_s;
                    text_input_query_tooltips[3] =
                        "Enter a tool box to be associated with the tool.\n"
                        "This field can be empty, but not negative or zero."_s;
                    
                    text_input_queries[4] = "Drawer number (optional): "_s;
                    text_input_query_tooltips[4] =
                        "Enter a drawer to be associated with the tool.\n"
                        "This field can be empty, but not negative or zero."_s;
                    
                    text_input_queries[5] = "Image (optional): "_s;
                    text_input_query_tooltips[5] =
                        "Enter a path to an image file to be loaded\n"
                        "and displayed in assocation with the tool."_s;
                    
                } break;
                default_bad;
            }
        } break;
        
        case Ui_Page::Students: {
            switch (students_page_state.current_state) {
                case Student_Entry_State::None: {
                    num_text_inputs = 1;
                    text_input_queries[0] = "Search students: "_s;
                    text_input_query_tooltips[0] =
                        "Search for a student using their\n"
                        "first name, last name, student ID,\n"
                        "or any combination of criteria."_s;
                } break;
                case Student_Entry_State::Edit_Student:
                case Student_Entry_State::New_Student: {
                    num_text_inputs = 2;
                    text_input_queries[0] = "Student's student ID?: #"_s;
                    text_input_queries[1] = "Student's full name?: "_s;
                    
                } break;
                case Student_Entry_State::Class_Day_And_Instructor: {
                    num_text_inputs = 0;
                } break;
                case Student_Entry_State::New_Or_Editing_Instructor: {
                    num_text_inputs = 1;
                    text_input_queries[0] = "Instructor's name?: "_s;
                } break;
                default_bad;
            }
        } break;
        
        case Ui_Page::Transact: {
            switch (transact_page_state.current_state) {
                case Transact_Entry_State::None: {
                    num_text_inputs = 2;
                    text_input_queries[0] = "Which student?: "_s;
                    text_input_queries[1] = "Loan a tool: "_s;
                } break;
                case Transact_Entry_State::Size: {
                    auto tool_id = transact_page_state.which;
                    auto tool = get_tool(&tool_storage, tool_id);
                    assert(tool);
                    
                    switch (tool->tool_size_type) {
                        case Tool_Size_Type::Imperial: {
                            num_text_inputs = 1;
                            text_input_queries[0] = "Size in inches?: "_s;
                        } break;
                        case Tool_Size_Type::Metric: {
                            num_text_inputs = 1;
                            text_input_queries[0] = "Size in millimetres?: "_s;
                        } break;
                        case Tool_Size_Type::Thread_Imperial: {
                            num_text_inputs = 2;
                            text_input_queries[0] = "Diameter in inches?: "_s;
                            text_input_queries[1] = "Threads per inch?: "_s;
                        } break;
                        case Tool_Size_Type::Thread_Metric: {
                            num_text_inputs = 2;
                            text_input_queries[0] = "Diameter in millimetres?: "_s;
                            text_input_queries[1] = "Pitch in millimetres?: "_s;
                        } break;
                        default_bad;
                    }
                } break;
                default_bad;
            }
        } break;
        
        case Ui_Page::History: {
            num_text_inputs = 1;
            text_input_queries[0] = "Search transaction history: "_s;
        } break;
        
        default_bad;
    }
    
    // kind of a @Hack.
    auto correct_text_input = ng_clamp(text_input, -1, num_text_inputs - 1);
    if (correct_text_input != text_input) switch_text_input(correct_text_input);
    
    auto get_cursor_x = [&](int x) -> f32 {
        if (text_input < 0) {
            assert(x == 0);
            return 0;
        }
        if (active_page == Ui_Page::Locked) {
            int advance_width = 0;
            stbtt_GetCodepointHMetrics(&ui->font_data.info, U'•', &advance_width, nullptr);
            float star_width = advance_width * ui_get_current_font_scale(ui);
            return star_width * x;
        }
        
        auto clipped_input = text_inputs[text_input];
        assert(x <= clipped_input.count);
        clipped_input.count = x;
        return get_text_size(&ui->font_data, utf32_to_utf8(&clipped_input, temp_allocator)).x_cursor_on_last_line * ui_get_current_font_scale(ui); // @Speed @Hack // Mega-@Speed Mega-@Hack !!!!!! doing utf32_to_utf8 is even worse than before. 2018-08-28
    };
    auto get_cursor_width = [&](int n) -> f32 {
        auto count = text_inputs[text_input].count;
        assert(n <= count);
        if (n >= count) return 1.0;
        return 0.0;
    };
    
    auto delete_selection = [&] {
        auto begin = ng::min(ui->tis.cursor, ui->tis.selection_cursor);
        auto end = ng::max(ui->tis.cursor, ui->tis.selection_cursor);
        
        begin = ng::clamp(begin, 0, text_inputs[text_input].count);
        end = ng::clamp(end, 0, text_inputs[text_input].count);
        
        while (begin != end) { // @Todo @Speed: Replace with a memmove
            text_inputs[text_input].remove_ordered(begin);
            ui->tis.changed = true;
            ui->tis.hot_timer = ng::clamp(ui->tis.hot_timer + CURSOR_HOT_TIME, 0, CURSOR_MAX_HOTNESS);
            ui->tis.backspaced = true;
            ui->tis.cursor_moved = true;
            end -= 1;
        }
        
        ui->tis.cursor = begin;
        ui->tis.selection_cursor = ui->tis.cursor;
    };
    
    s64 old_text_input_cursor = 0;
    s64 old_text_input_count = 0;
    if (text_input >= 0) {
        old_text_input_cursor = ui->tis.cursor;
        old_text_input_count = text_inputs[text_input].count;
    }
    
    int wheel_offset_this_frame = 0;
    
    for (SDL_Event event; SDL_PollEvent(&event);) {
        defer {
            if (text_input >= 0) {
                ui->tis.cursor = ng::clamp(ui->tis.cursor, 0, text_inputs[text_input].count);
                ui->tis.selection_cursor = ng::clamp(ui->tis.selection_cursor, 0, text_inputs[text_input].count);
                
                // if (ui->tis.cursor != old_ui->tis.cursor) ui->tis.cursor_moved = true;
                // if (text_input->count != old_text_input_count) ui->tis.changed = true;
                
                if (ui->tis.cursor_moved) ui->tis.blink_timer = 0;
                if (ui->tis.changed) ui->tis.hot_timer += CURSOR_HOT_TIME; // This only applies 1 hot-time for all text input changes that may have happened while handling a single input event, but whatever, nobody will notice or care.
            }
        };
        
        switch (event.type) {
            
            case SDL_QUIT: {
                // return quit(app);
                return 0;
            } break;
            
            case SDL_WINDOWEVENT: {
                const auto t = event.window.event;
                if (t == SDL_WINDOWEVENT_SIZE_CHANGED) {
                    ui->screen_size.x = event.window.data1;
                    ui->screen_size.y = event.window.data2;
                    ui->tis.cursor_eased_pos = get_cursor_x(ui->tis.cursor);
                    ui->tis.blink_timer = 0;
                    
                    clear_glyphs(&ui->font_data.glyphs);
                    
                    resize_framebuffer(&framebuffer);
                    resize_framebuffer(&fade_buffer);
                    
                    RESTART_FRAME_NO_FADE(); //goto begin_frame;
                } else if (t == SDL_WINDOWEVENT_FOCUS_LOST) {
                    window_has_focus = false;
                } else if (t == SDL_WINDOWEVENT_FOCUS_GAINED) {
                    window_has_focus = true;
                    ui->tis.blink_timer = 0;
                }
            } break;
            
            case SDL_MOUSEBUTTONDOWN: {
                auto but = event.button.button;
                if (but == SDL_BUTTON_LEFT) {
                    input_state.lmb_down = true;
                } else if (but == SDL_BUTTON_RIGHT) {
                    input_state.rmb_down = true;
                }
            } break;
            
            case SDL_MOUSEBUTTONUP: {
                auto but = event.button.button;
                if (but == SDL_BUTTON_LEFT) {
                    input_state.lmb_up = true;
                } else if (but == SDL_BUTTON_RIGHT) {
                    input_state.rmb_up = true;
                }
            } break;
            
            case SDL_MOUSEWHEEL: {
                // auto sign = -ng::sign(event.wheel.y);
                // scroll_index += sign;
                wheel_offset_this_frame = -event.wheel.y;
            } break;
            
            case SDL_KEYUP: {
                //if (ui->obscured) break;
                
                input_state.keys_up[event.key.keysym.scancode] = true;
            } break;
            
            case SDL_TEXTINPUT: {
                if (ui->obscured) break;
                
                if (text_input < 0) break;
                
                if (ui_is_selecting(ui)) {
                    delete_selection();
                }
                
                unsigned int decoder_state = 0;
                unsigned int codepoint = 0;
                For (event.text.text) {
                    utf8_decode(&decoder_state, &codepoint, cast(u8) it);
                    assert(decoder_state != utf8_decode_reject); // , "invalid utf8 string given");
                    if (decoder_state == utf8_decode_reject) break;
                    if (decoder_state != utf8_decode_accept) continue;
                    
                    if (!codepoint) break;
                    
                    text_inputs[text_input].insert(ui->tis.cursor, codepoint);
                    ui->tis.cursor += 1;
                    
                    ui->tis.changed = true;
                    ui->tis.cursor_moved = true;
                }
                ui->tis.selection_cursor = ui->tis.cursor;
                
                //RESTART_FRAME_NO_FADE();
            } break;
            
            case SDL_KEYDOWN: {
                
                input_state.keys_down[event.key.keysym.scancode] = event.key.repeat + 1;
                
                if (really_delete && event.key.keysym.sym != SDLK_RETURN) {
                    // Immediately cancel out of really-deleting.
                    really_delete = false;
                }
                
                if (ui->obscured) break;
                
                bool shift = (event.key.keysym.mod & KMOD_SHIFT);
                
                switch (event.key.keysym.sym) {
                    case SDLK_TAB: {
                        
                        int direction = shift ? -1 : +1;
                        
                        if (num_text_inputs > 1) {
                            // @Hack @Robustness
                            if (text_input < 0) {
                                if (shift) {
                                    text_input = num_text_inputs - 1;
                                } else {
                                    text_input = 0;
                                }
                            } else {
                                if (shift) {
                                    text_input -= 1;
                                } else {
                                    text_input += 1;
                                }
                                
                                text_input %= num_text_inputs;
                                if (text_input < 0) text_input += num_text_inputs;
                            }
                            
                            switch_text_input(text_input, direction);
                            
                            RESTART_FRAME_NO_FADE(); //goto begin_frame; // @Goto
                        }
                    } break;
                    
                    case SDLK_a: {
                        if (event.key.keysym.mod & KMOD_CTRL) {
                            if (text_input < 0) break;
                            
                            ui->tis.cursor_moved = true;
                            
                            ui->tis.selection_cursor = 0;
                            ui->tis.cursor = text_inputs[text_input].count;
                        }
                    } break;
                    
                    case SDLK_LEFT: {
                        if (text_input < 0) break;
                        
                        ui->tis.cursor_moved = true;
                        
                        defer {
                            if (!shift) {
                                ui->tis.selection_cursor = ui->tis.cursor;
                            }
                        };
                        
                        auto is_alpha = [](unsigned int codepoint) {
                            // return ;
                        };
                        
                        if (event.key.keysym.mod & KMOD_CTRL) {
                            while (ui->tis.cursor > 0 && text_inputs[text_input][ui->tis.cursor - 1] == ' ') {
                                ui->tis.cursor -= 1;
                            }
                            while (ui->tis.cursor > 0 && text_inputs[text_input][ui->tis.cursor - 1] != ' ') {
                                ui->tis.cursor -= 1;
                            }
                        } else {
                            ui->tis.cursor -= 1;
                        }
                    } break;
                    
                    case SDLK_RIGHT: {
                        if (text_input < 0) break;
                        
                        ui->tis.cursor_moved = true;
                        
                        defer {
                            if (!shift) {
                                ui->tis.selection_cursor = ui->tis.cursor;
                            }
                        };
                        
                        if (event.key.keysym.mod & KMOD_CTRL) {
                            while (ui->tis.cursor < text_inputs[text_input].count && text_inputs[text_input][ui->tis.cursor] != ' ') {
                                ui->tis.cursor += 1;
                            }
                            while (ui->tis.cursor < text_inputs[text_input].count && text_inputs[text_input][ui->tis.cursor] == ' ') {
                                ui->tis.cursor += 1;
                            }
                        } else {
                            ui->tis.cursor += 1;
                        }
                    } break;
                    
                    case SDLK_BACKSPACE: {
                        if (text_input < 0) break;
                        
                        ui->tis.cursor_moved = true;
                        
                        if (ui_is_selecting(ui)) {
                            delete_selection();
                        } else {
                            auto backspace_one = [&] {
                                if (ui->tis.cursor > 0) {
                                    text_inputs[text_input].remove_ordered(ui->tis.cursor - 1);
                                    ui->tis.cursor -= 1;
                                    
                                    ui->tis.changed = true;
                                    ui->tis.backspaced = true;
                                    ui->tis.cursor_moved = true;
                                }
                            };
                            
                            if (event.key.keysym.mod & KMOD_CTRL) {
                                while (ui->tis.cursor > 0 && text_inputs[text_input][ui->tis.cursor - 1] == ' ') {
                                    backspace_one();
                                }
                                while (ui->tis.cursor > 0 && text_inputs[text_input][ui->tis.cursor - 1] != ' ') {
                                    backspace_one();
                                }
                            } else {
                                backspace_one();
                            }
                            
                            ui->tis.selection_cursor = ui->tis.cursor;
                        }
                    } break;
                    
                    case SDLK_DELETE: {
                        if (text_input < 0) break;
                        
                        ui->tis.cursor_moved = true;
                        
                        if (ui_is_selecting(ui)) {
                            delete_selection();
                        } else {
                            auto delete_one = [&] {
                                if (ui->tis.cursor < text_inputs[text_input].count) {
                                    text_inputs[text_input].remove_ordered(ui->tis.cursor);
                                    
                                    ui->tis.changed = true;
                                    ui->tis.cursor_moved = true;
                                }
                            };
                            
                            if (event.key.keysym.mod & KMOD_CTRL) {
                                while (ui->tis.cursor < text_inputs[text_input].count && text_inputs[text_input][ui->tis.cursor] != ' ') {
                                    delete_one();
                                }
                                while (ui->tis.cursor < text_inputs[text_input].count && text_inputs[text_input][ui->tis.cursor] == ' ') {
                                    delete_one();
                                }
                            } else {
                                delete_one();
                            }
                            
                            ui->tis.selection_cursor = ui->tis.cursor;
                        }
                    } break;
                    
                    case SDLK_ESCAPE: {
                        switch (active_page) {
                            
                            case Ui_Page::Locked: {
                                For (text_inputs) clear_text_input(&it);
                                switch_text_input(0);
                            } break;
                            
                            case Ui_Page::Tools: {
                                //tools_page_state_reset(&tools_page_state);
                                //For (text_inputs) clear_text_input(&it);
                                //switch_text_input(0);
                                
                                // all handled in ui_button code now :KeyboardToButtons
                            } break;
                            
                            case Ui_Page::Students: {
                                
                                if (students_page_state.return_to_transact_page) {
                                    active_page = Ui_Page::Transact;
                                }
                                
                                students_page_state_reset(&students_page_state);
                                For (text_inputs) clear_text_input(&it);
                                switch_text_input(0);
                                
                                RESTART_FRAME();
                            } break;
                            
                            case Ui_Page::Transact: {
                                if (transact_page_state.current_state == Transact_Entry_State::None) {
                                    if (text_input >= 0) {
                                        clear_text_input(&text_inputs[text_input]);
                                        switch_text_input(-1);
                                        
                                        RESTART_FRAME();
                                    } else {
                                        
                                        if (transact_page_state.who) {
                                            transact_page_state.who = 0;
                                            transact_page_state.returns.clear();
                                            // transact_page_state_reset(&transact_page_state);
                                            //switch_text_input(0);
                                            
                                            RESTART_FRAME();
                                        } else {
                                            
                                            transact_page_state_reset(&transact_page_state);
                                            
                                            switch_text_input(-1);
                                            
                                            RESTART_FRAME();
                                        }
                                    }
                                }
                            } break;
                            
                            case Ui_Page::History: {
                                
                                history_page_state_reset(&history_page_state);
                                
                                switch_text_input(0);
                                
                                RESTART_FRAME();
                                
                            } break;
                            
                            default_bad;
                        }
                        
                        //RESTART_FRAME(); //goto begin_frame; // @Goto
                    } break;
                    
                    case SDLK_HOME: {
                        if (text_input < 0) break;
                        
                        ui->tis.cursor_moved = true;
                        
                        defer {
                            if (!shift) {
                                ui->tis.selection_cursor = ui->tis.cursor;
                            }
                        };
                        
                        ui->tis.cursor = 0;
                    } break;
                    
                    case SDLK_END: {
                        if (text_input < 0) break;
                        
                        ui->tis.cursor_moved = true;
                        
                        defer {
                            if (!shift) {
                                ui->tis.selection_cursor = ui->tis.cursor;
                            }
                        };
                        
                        ui->tis.cursor = text_inputs[text_input].count;
                    } break;
                    
                    case SDLK_RETURN: {
                        if (text_input < 0) break;
                        
                        if (active_page == Ui_Page::Locked) {
                            auto password = utf32_to_utf8(&text_inputs[text_input], temp_allocator);
                            
                            For (text_inputs) clear_text_input(&it);
                            
#if USE_PASSWORD
#define PASSWORD "[Redacted]"
                            constexpr u64 hash = hash_64_fnv1a_const(PASSWORD, sizeof(PASSWORD) - 1);
                            if (hash_64_fnv1a_const((char *) password.ptr, password.len) == hash)
#endif
                            {
                                
                                active_page = Ui_Page::Tools;
                                
                                switch_text_input(0);
                            }
                        } else {
                            if (progress_data_entry()) {
                                RESTART_FRAME();
                            } else {
                                RESTART_FRAME_NO_FADE();
                            }
                        }
                        
                        RESTART_FRAME(); //goto begin_frame; // @Goto
                    } break;
                }
            } break;
        }
    }
    
    {
        int num_scancodes = 0;
        auto keystate = SDL_GetKeyboardState(&num_scancodes);
        for (int i = 0; i < num_scancodes; i += 1) {
            input_state.keys[i] = keystate[i];
        }
        input_state.keymod = SDL_GetModState();
    }
    
    ui_update_input(ui, input_state);
    
    //
    // update
    //
    
    if (ui->tis.backspaced) {
        ui->tis.backspace_timer = CURSOR_EASE_BACKSPACE_TIME;
    }
    
    {
        //SDL_SetRenderDrawColor(renderer, 240, 236, 232, 255);
        SDL_Color render_clear_color = {240, 236, 232, 255};
#define TOOLS_PAGE_COLOR SDL_Color{0, 0, 255, 255}
#define STUDENTS_PAGE_COLOR SDL_Color{255, 0, 0, 255}
#define TRANSACT_PAGE_COLOR SDL_Color{0, 255, 0, 255}
#define HISTORY_PAGE_COLOR SDL_Color{255, 255, 0, 255}
        
        SDL_Color lerp_to_color = {};
        if (active_page == Ui_Page::Tools) lerp_to_color = TOOLS_PAGE_COLOR;
        if (active_page == Ui_Page::Students) lerp_to_color = STUDENTS_PAGE_COLOR;
        if (active_page == Ui_Page::Transact) lerp_to_color = TRANSACT_PAGE_COLOR;
        if (active_page == Ui_Page::History) lerp_to_color = HISTORY_PAGE_COLOR;
        
        render_clear_color = lerp(render_clear_color, lerp_to_color, 0.25f);
        
        SDL_SetRenderDrawColor(renderer, render_clear_color.r, render_clear_color.g, render_clear_color.b, render_clear_color.a);
        SDL_RenderClear(renderer);
    }
    
    //f64 dt = 1.0 / 60;
    
    u64 next = SDL_GetPerformanceCounter();
    
    f64 dt = (next - prev) * invfreq;
    
    prev = next;
    
    ui->t += dt;
    
    if (text_input >= 0) {
        ui->tis.blink_timer += dt;
        
        ui->tis.hot_timer = ng_clamp(ui->tis.hot_timer, 0, CURSOR_MAX_HOTNESS);
        if (ui->tis.hot_timer > 0) {
            ui->tis.hot_timer -= dt;
            ui->tis.blink_timer = 0;
        }
        
        ui->tis.backspace_timer -= dt;
        if (ui->tis.backspace_timer < 0)  {
            ui->tis.backspace_timer = 0;
        }
        {
            auto t = ng::clamp(ui->tis.backspace_timer / CURSOR_EASE_BACKSPACE_TIME, 0, 1);
            auto rate = ng::pow(10.0, ng::lerp(-10.0, -20.0, t));
            auto target_pos = get_cursor_x(ui->tis.cursor);
            if (active_page == Ui_Page::Locked) {
                target_pos = get_text_size(&ui->font_data, "•"_s, 1).x_cursor_on_last_line * ui_get_current_font_scale(ui);
                target_pos *= ui->tis.cursor;
            }
            ui->tis.cursor_eased_pos = damp(ui->tis.cursor_eased_pos, target_pos, rate, dt);
            if (ng::abs(ui->tis.cursor_eased_pos - target_pos) > 0.1 * (ui->font_data.space_width * ui_get_current_font_scale(ui))) {
                ui->tis.blink_timer = 0;
            }
        }
        {
            auto rate = 1.0e-6;
            ui->tis.cursor_eased_width = damp(ui->tis.cursor_eased_width, get_cursor_width(ui->tis.cursor), rate, dt);
        }
    }
    
#ifndef NDEBUG
    if (input_state.keys_down[SDL_SCANCODE_F12]) {
        draw_grid = !draw_grid;
        if (!draw_grid) draw_bounds = !draw_bounds;
    }
#endif
    
    if (draw_grid) {
        for (int x = 0; x < 100; x += 1) {
            auto x1 = cast(int)(ui->screen_size.x * x / 100);
            auto y1 = cast(int)(ui->screen_size.y);
            auto x2 = x1;
            auto y2 = 0;
            
            SDL_SetRenderDrawColor(renderer, 0, 0, 0, 255);
            SDL_RenderDrawLine(renderer, x1, y1, x2, y2);
        }
        for (int y = 0; y < 100; y += 1) {
            auto x1 = cast(int)(ui->screen_size.x);
            auto y1 = cast(int)(ui->screen_size.y * y / 100);
            auto x2 = 0;
            auto y2 = y1;
            
            SDL_SetRenderDrawColor(renderer, 0, 0, 0, 255);
            SDL_RenderDrawLine(renderer, x1, y1, x2, y2);
        }
    }
    
    auto total_height = 1.0f;
    
    auto adjust_for_interspace = [](int i, int n, float advance, float width){
        auto completed = cast(float) i / (n - 1);
        return completed * (advance - width);
    };
    
    const auto tab_border = 0.01f;
    const auto tab_height = 0.08f;
    
    {
        auto size = ui_text(ui, v2(0.5, 0.99), "ToolGal Tool Manager v" STR_VERSION ""_s, 0.75f, 0.75f, Text_Halign::Center, Text_Valign::Top, TEXT_COLOR, about_text);
        total_height -= size.total_size.y * ui_get_current_font_scale(ui) / ui->screen_size.y;
    }
    {
        ui_text(ui, v2(0.01, 0.99), tprint("% % loaned (% %), % overdue"_s, total_items_owed, pluralize(total_items_owed, "items"_s), num_tools_loaned, pluralize(num_tools_loaned, "tools"_s), total_overdue), 0.75f, 0.75f, Text_Halign::Left, Text_Valign::Top, TEXT_COLOR, tprint(
            "The database currently contains % loaned\n"
            "% across % different %."_s, total_items_owed, pluralize(total_items_owed, "items"_s), num_tools_loaned, pluralize(num_tools_loaned, "tools"_s)));
    }
    
    // if (!locked)
    { // draw tabs // render tabs
        const auto n = cast(int) Ui_Page::Count;
        
        auto total_width = (1.0f - (tab_border * 2));
        auto tab_advance = total_width / n;
        auto tab_width = tab_advance * 0.95f;
        
        int i = 0; // @Hack
        
        auto button = [&](Ui_Page page, int flags, auto... args) {
            Rectangle button_rect = {};
            button_rect.size = v2(tab_width, tab_height);
            button_rect.x = tab_border + tab_advance * i + adjust_for_interspace(i, n, tab_advance, tab_width);
            button_rect.y = total_height - tab_border - tab_height;
            
            i += 1;
            
            auto result = ui_button(ui, flags, button_rect, args...);
            
            // if (page != active_page) {
            // ui_rect(ui, button_rect, 0.1);
            // }
            
            return result;
        };
        
        auto key1 = SDL_SCANCODE_F1;
        auto key2 = SDL_SCANCODE_F2;
        auto key3 = SDL_SCANCODE_F3;
        auto key4 = SDL_SCANCODE_F4;
        auto key5 = SDL_SCANCODE_F5;
        
        auto flags1 = (active_page == Ui_Page::Tools   ) * (BUT_OFF | BUT_WHITE) | ui_key(ui, key1) * BUT_DOWN;
        auto flags2 = (active_page == Ui_Page::Students) * (BUT_OFF | BUT_WHITE) | ui_key(ui, key2) * BUT_DOWN;
        auto flags3 = (active_page == Ui_Page::Transact) * (BUT_OFF | BUT_WHITE) | ui_key(ui, key3) * BUT_DOWN;
        auto flags4 = (active_page == Ui_Page::History ) * (BUT_OFF | BUT_WHITE) | ui_key(ui, key4) * BUT_DOWN;
        auto flags5 = (active_page == Ui_Page::Locked  ) * (BUT_OFF | BUT_WHITE) | ui_key(ui, key5) * BUT_DOWN;
        
        if (active_page == Ui_Page::Locked) {
            key1 = key2 = key3 = key4 = key5 = SDL_SCANCODE_UNKNOWN;
            flags1 = flags2 = flags3 = flags4 = flags5 = BUT_OFF;
        }
        
        if (active_page != Ui_Page::Locked && ui_keydown(ui, key1) || button(Ui_Page::Tools, flags1, "TOOLS"_s)) {
            
            switch_to_page(Ui_Page::Tools);
            
            switch_text_input(0);
            
            RESTART_FRAME(); //goto begin_frame; // @Goto
        }
        if (active_page != Ui_Page::Locked && ui_keydown(ui, key2) || button(Ui_Page::Students, flags2, "STUDENTS"_s)) {
            
            switch_to_page(Ui_Page::Students);
            
            switch_text_input(0);
            
            RESTART_FRAME(); //goto begin_frame; // @Goto
        }
        if (active_page != Ui_Page::Locked && ui_keydown(ui, key3) || button(Ui_Page::Transact, flags3, "TRANSACT"_s)) {
            
            switch_to_page(Ui_Page::Transact);
            
            students_page_state.student_sort_mode = Student_Sort_Mode::Last_Name;
            tools_page_state.tool_sort_mode = Tool_Sort_Mode::Search_Matches;
            
            switch_text_input(-1);
            
            // @Todo: @Incomplete?
            
            RESTART_FRAME(); //goto begin_frame; // @Goto
        }
        if (ui_keydown(ui, key4) || button(Ui_Page::History, flags4, "HISTORY"_s)) {
            
            switch_to_page(Ui_Page::History);
            
            switch_text_input(0);
            // @Todo: @Incomplete?
            
            RESTART_FRAME(); //goto begin_frame; // @Goto
        }
        
        if (ui_keydown(ui, key5) || button(Ui_Page::Tools, flags5, "LOCK"_s)) {
            
            switch_to_page(Ui_Page::Locked);
            
            switch_text_input(0);
            
            RESTART_FRAME(); //goto begin_frame; // @Goto
        }
        
        total_height -= (tab_height + tab_border * 2);
    }
    
    // bool text_input_is_surrounded_by_quotation_marks = (tools_page_state.current_state == Tool_Entry_State::Name);
    
    if (active_page == Ui_Page::Locked) {
        total_height -= 0.01;
    } else if (active_page == Ui_Page::Transact) {
        if (transact_page_state.current_state == Transact_Entry_State::Size) {
            auto tool = get_tool(&tool_storage, transact_page_state.which);
            assert(tool);
            assert(tool->tool_size_type != Tool_Size_Type::Unsized);
            auto tool_size_type_string = initializer_list((ng::string) "Unsized"_s, "Imperial"_s, "Metric"_s, "Thread (Imperial)"_s, "Thread (Metric)"_s, "<invalid>"_s)[cast(int) tool->tool_size_type];
            // assert(cast(int) tool->tool_size_type <= cast(int) Tool_Size_Type::Count));
            auto label_string = tprint("Please enter the size of %."_s, tool->name);
            auto text_size = ui_text(ui, v2(0.01, total_height), label_string, 1.0f, 1.0f, Text_Halign::Left, Text_Valign::Top, TEXT_COLOR, tprint("This tool uses the % size type."_s, tool_size_type_string));
            total_height -= text_size.total_size.y * ui_get_current_font_scale(ui) / ui->screen_size.y;
            total_height -= 0.02;
        }
        
    } else if (active_page == Ui_Page::Students) {
        
        if (students_page_state.current_state == Student_Entry_State::Class_Day_And_Instructor) {
            
            ng::array<Student *> checkmarked_students = {};
            checkmarked_students.alloc = temp_allocator;
            
            const int MAX_NAMES_LIST_PREVIEW_NUM = 50;
            
            int total_num_checkmarked = 0;
            
            For (student_storage) {
                if (!it.checkmarked) continue;
                
                if (checkmarked_students.count < MAX_NAMES_LIST_PREVIEW_NUM) {
                    checkmarked_students.push(&it);
                }
                
                total_num_checkmarked += 1;
            }
            
            ng::array<u8> names = {};
            names.alloc = temp_allocator;
            
            const int MAX_NAMES_LIST_PREVIEW_LENGTH = 50;
            
            for (int i = 0; i < checkmarked_students.count; i += 1) {
                auto student = checkmarked_students[i];
                
                if (names.count > MAX_NAMES_LIST_PREVIEW_LENGTH) {
                    string_buffer_append(&names, "..."_s);
                    break;
                }
                if (i > 0) {
                    string_buffer_append(&names, ", "_s);
                    if (i == checkmarked_students.count - 1) {
                        string_buffer_append(&names, "and "_s);
                    }
                }
                string_buffer_append(&names, student->first_name);
            }
            
            string_buffer_prepend(&names, "Setting info for "_s);
            auto text_size = ui_text(ui, v2(0.01, total_height), string_buffer_to_string(&names), 1.0f, 1.0f, Text_Halign::Left, Text_Valign::Top, TEXT_COLOR, tprint("Setting the class day and instructor for % students total."_s, total_num_checkmarked));
            
            total_height -= text_size.total_size.y * ui_get_current_font_scale(ui) / ui->screen_size.y;
            total_height -= 0.02;
        }
        
    }
    
    auto get_cursor_height = [&] {
        return (ui->font_data.ascent - ui->font_data.descent) * stbtt_ScaleForPixelHeight(&ui->font_data.info, ui_get_current_point_height(ui)); // @Speed @Robustness @Duplicate
    };
    
    auto interpolate_cursor_width = [&](Rectangle *rect, float w) {
        f32 thin_w = 0;
        f32 thin_x = 0;
        f32 thick_w = 0;
        f32 thick_x = 0;
        {
            auto width = ng::max(1.0f, ui_get_current_point_height(ui) / 16);
            thin_w = width;
            thin_x = -ng::round(width / 2);
        }
        {
            thick_w = rect->h / 2;
            thick_x = 0;
        }
        rect->w += ng::lerp(thin_w, thick_w, w);
        rect->x += ng::lerp(thin_x, thick_x, w);
    };
    
    auto cancel_edit_student = [&] {
        // @Cutnpaste @Temporary @Refactor @Duplicate @Deduplicate
        // reset_pages();
        
        students_page_state_reset(&students_page_state);
        
        // Adapted from progress_data_entry(). @Cutnpaste @Temporary @Refactor @Duplicate @Deduplicate
        if (students_page_state.return_to_transact_page) {
            
            clear_text_input(&text_inputs[1]);
            
            active_page = Ui_Page::Transact;
            
            switch_text_input(0);
            ui->tis.selection_cursor = ui->tis.cursor;
        } else {
            
            For (text_inputs) clear_text_input(&it);
            
            active_page = Ui_Page::Students;
            switch_text_input(0);
        }
    };
    
    if (active_page == Ui_Page::Tools && tools_page_state.current_state != Tool_Entry_State::None) {
        
        text_inputs[1].clear(); // @Hack @Robustness
        
        if (text_input == 1) {
            
            ui->tis = {};
            
            auto as_int = cast(int) tools_page_state.entry_size_type;
            
            if (ui_keydown(ui, SDL_SCANCODE_RIGHT, KMOD_NONE, true)) {
                as_int += 1;
            } else if (ui_keydown(ui, SDL_SCANCODE_LEFT, KMOD_NONE, true)) {
                as_int -= 1;
            }
            
            if (as_int < 0) as_int = 0;
            if (as_int >= cast(int) Tool_Size_Type::Count) as_int = cast(int) Tool_Size_Type::Count - 1;
            
            tools_page_state.entry_size_type = cast(Tool_Size_Type) as_int;
        }
    }
    
    SDL_SetCursor(SDL_GetDefaultCursor());
    
    Rectangle active_text_input_clip_rect = {};
    
    constexpr float TEXT_INPUT_LEFT_MARGIN = 0.0025;
    
    auto cursor_pos = v2(0, 0);
    for (int i = 0; i < num_text_inputs; i += 1) {
        
        // @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
        // @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
        
        // @Todo: @Refactor @@@ XXX
        
        auto text_input_pos = v2(0.01, total_height);
        auto text_input_draw_pos = v2_world_to_screen(text_input_pos, ui->screen_size);
        defer {
            // @Hack @Refactor
            //if (text_input == i) cursor_pos = text_input_draw_pos;
        };
        
        auto text_input_height = get_cursor_height() / ui->screen_size.y;
        
        if (active_page == Ui_Page::Tools && tools_page_state.current_state != Tool_Entry_State::None) {
            if (i == 2) { // @Refactor.
                if (tools_page_state.entry_size_type != Tool_Size_Type::Unsized) {
                    continue;
                }
            }
            
        }
        
        auto put_string_to_input_line = [&](ng::string str, float offset = 0, SDL_Color color = TEXT_COLOR, ng::string tooltip = ""_s) {
            auto pos = text_input_draw_pos;
            pos.x += offset;
            auto size = ui_text(ui, v2_screen_to_world(pos, ui->screen_size), str, 1.0f, 1.0f, Text_Halign::Left, Text_Valign::Top, color, tooltip);
            return size;
        };
        
        {
            auto size = put_string_to_input_line(text_input_queries[i], 0, TEXT_COLOR, text_input_query_tooltips[i]);
            text_input_draw_pos.x += size.x_cursor_on_last_line * ui_get_current_font_scale(ui); // @Speed @Hack
            // text_input_draw_pos.y += size.total_size.y * ui_get_current_font_scale(ui); // @Todo
        }
        
        defer {
            
            // @Refactor: this is where we render the tool size type "radio buttons", but it REALLY shouldn't be here.
            if (active_page == Ui_Page::Tools && tools_page_state.current_state != Tool_Entry_State::None) {
                
                const float h = text_input_height;
                if (i == 1) { // @Refactor.
                    
                    float pos = text_input_draw_pos.x / ui->screen_size.x;
                    
                    // @Todo: "Unsized (Ctrl-1): ..." etc
                    // @Hack to the max.
                    if (ui_button(ui, (tools_page_state.entry_size_type == Tool_Size_Type::Unsized) * (BUT_OFF | BUT_WHITE | (text_input == 1) * BUT_FLASH), rect(0 * 0.14 + pos, total_height - h, 0.13, h), "No size"_s)) {
                        tools_page_state.entry_size_type = Tool_Size_Type::Unsized;
                    }
                    if (ui_button(ui, (tools_page_state.entry_size_type == Tool_Size_Type::Imperial) * (BUT_OFF | BUT_WHITE | (text_input == 1) * BUT_FLASH), rect(1 * 0.14 + pos, total_height - h, 0.13, h), "Imperial"_s)) {
                        tools_page_state.entry_size_type = Tool_Size_Type::Imperial;
                        
                        if (text_input == 2) switch_text_input(0);
                    }
                    if (ui_button(ui, (tools_page_state.entry_size_type == Tool_Size_Type::Metric) * (BUT_OFF | BUT_WHITE | (text_input == 1) * BUT_FLASH), rect(2 * 0.14 + pos, total_height - h, 0.13, h), "Metric"_s)) {
                        tools_page_state.entry_size_type = Tool_Size_Type::Metric;
                        
                        if (text_input == 2) switch_text_input(0);
                    }
                    if (ui_button(ui, (tools_page_state.entry_size_type == Tool_Size_Type::Thread_Imperial) * (BUT_OFF | BUT_WHITE | (text_input == 1) * BUT_FLASH), rect(3 * 0.14 + pos, total_height - h, 0.13, h), "Thread (imperial)"_s)) {
                        tools_page_state.entry_size_type = Tool_Size_Type::Thread_Imperial;
                        
                        if (text_input == 2) switch_text_input(0);
                    }
                    if (ui_button(ui, (tools_page_state.entry_size_type == Tool_Size_Type::Thread_Metric) * (BUT_OFF | BUT_WHITE | (text_input == 1) * BUT_FLASH), rect(4 * 0.14 + pos, total_height - h, 0.13, h), "Thread (metric)"_s)) {
                        tools_page_state.entry_size_type = Tool_Size_Type::Thread_Metric;
                        
                        if (text_input == 2) switch_text_input(0);
                    }
                    total_height -= 0.01;
                }
                
            }
            total_height -= text_input_height;
            
            if (i < num_text_inputs - 1) total_height -= 0.01f;
        };
        
        if (active_page == Ui_Page::Tools && tools_page_state.current_state != Tool_Entry_State::None) {
            if (i == 1) { // @Refactor.
                continue;
            }
        }
        
        if (active_page == Ui_Page::Tools && tools_page_state.current_state != Tool_Entry_State::None) {
            if (i == 5) { // @Refactor.
                
                const float h = text_input_height;
                
                float pos = text_input_draw_pos.x / ui->screen_size.x;
                
                // @Todo: "Open (Ctrl-O): ..." etc
                // @Hack to the max.
                auto keydown = ui_keydown(ui, SDL_SCANCODE_O, cast(SDL_Keymod) KMOD_CTRL);
                auto key     = ui_key    (ui, SDL_SCANCODE_O, cast(SDL_Keymod) KMOD_CTRL);
                if (ui_button(ui, key * BUT_DOWN, rect(pos, total_height - h, 0.10, h), "Open..."_s) || keydown) {
                    
                    auto filename = prompt_image_file_string(utf32_to_utf8(&text_inputs[5], temp_allocator));
                    defer { ng::free_string(&filename, ng::default_allocator); };
                    
                    if (filename) {
                        
                        auto trimmed = filename;
                        
                        // @Hack of a fix to the nul terminator problem right there.
                        while (trimmed && trimmed[trimmed.len - 1] == '\0') trimmed.len -= 1;
                        
                        text_inputs[5].clear();
                        utf8_to_utf32(trimmed, &text_inputs[5]);
                    }
                    
                }
                
                text_input_draw_pos.x += (0.10 + 0.01) * ui->screen_size.x;
                
                total_height -= 0.01;
                
            }
        }
        
        Rectangle input_rect = {};
        {
            input_rect = rect(text_input_draw_pos.x / ui->screen_size.x, text_input_pos.y - text_input_height, 0, text_input_height);
            
            input_rect.w = 0.99 - input_rect.x;
            
            ui_rect(ui, input_rect, 0.12f);
            
            
            if (active_page == Ui_Page::Transact && transact_page_state.current_state == Transact_Entry_State::None && transact_page_state.who && i == 0) {
            } else if (active_page == Ui_Page::Students && students_page_state.editing_student && i == 0) {
            } else {
                if (!ui->obscured && v2_in_rect(input_state.mouse_pos, input_rect)) {
                    SDL_SetCursor(mouse_cursor_ibeam);
                    if (input_state.lmb_down) {
                        switch_text_input(i);
                        
                        ui->tis.selection_cursor = ui->tis.cursor;
                        // @Todo: auto where = input_state.mouse_pos.x;
                        // where.
                    }
                    
                    ui_set_tooltip(ui, text_input_query_tooltips[i]);
                }
            }
        }
        ui_clip_rect(ui, input_rect);
        defer { ui_unclip_rect(ui); };
        
        if (i == text_input) active_text_input_clip_rect = input_rect;
        
        auto where_to_draw_text_input = v2_screen_to_world(text_input_draw_pos, ui->screen_size) + v2(0.0025, 0);
        
        if (i == text_input) {
            cursor_pos = text_input_draw_pos;
            
            auto cursor_world_pos = (ui->tis.cursor_eased_pos + text_input_draw_pos.x) / ui->screen_size.x;
            if (cursor_world_pos > 0.90f) {
                auto overshoot = cursor_world_pos - 0.90f;
                
                where_to_draw_text_input.x -= overshoot;
                
                cursor_pos.x = (0.90f * ui->screen_size.x) - ui->tis.cursor_eased_pos;
            }
        }
        
        if (active_page == Ui_Page::Transact) {
            if (transact_page_state.current_state == Transact_Entry_State::None) {
                auto who = transact_page_state.who;
                if (i == 0 && who) {
                    auto name = "[Unregistered Student]"_s;
                    
                    auto student = get_student(&student_storage, who);
                    assert(student);
                    if (student_registered(student)) name = student->name;
                    
                    auto size = ui_text(ui, where_to_draw_text_input, name, 1, 1, Text_Halign::Left, Text_Valign::Top);
                    
                    auto button_rect = rect(where_to_draw_text_input, v2(0.03, 0.05));
                    button_rect.y -= button_rect.h;
                    button_rect.x += 0.01;
                    button_rect.x += size.x_cursor_on_last_line * ui_get_current_font_scale(ui) / ui->screen_size.x;
                    
                    if (ui_button(ui, 0, button_rect, "X"_s, TEXT_COLOR, ""_s)) {
                        //transact_page_state_reset(&transact_page_state);
                        transact_page_state.who = 0;
                        transact_page_state.returns.clear();
                        
                        //switch_text_input(0);
                    }
                    continue;
                }
            }
        } else if (active_page == Ui_Page::Students) {
            
            auto student_id = students_page_state.editing_student;
            if (i == 0 && student_id) {
                // @Duplicate @Deduplicate @Redundant with transact state `who` above
                auto id_string = tprint("%"_s, student_id);
                
                auto size = ui_text(ui, where_to_draw_text_input, id_string, 1, 1, Text_Halign::Left, Text_Valign::Top);
                
                auto button_rect = rect(where_to_draw_text_input, v2(0.03, 0.05));
                button_rect.y -= button_rect.h;
                button_rect.x += 0.01;
                button_rect.x += size.x_cursor_on_last_line * ui_get_current_font_scale(ui) / ui->screen_size.x;
                
                if (ui_button(ui, 0, button_rect, "X"_s)) {
                    cancel_edit_student();
                    
                    RESTART_FRAME(); //goto begin_frame; // @Goto
                }
                
                continue;
            }
        }
        
        if (active_page == Ui_Page::Locked) { // @Todo @Hack @Speed @Incomplete
            auto pos = TEXT_INPUT_LEFT_MARGIN * ui->screen_size.x;
            for (int i = 0; i < text_inputs[0].count; i += 1) {
                auto text_size = put_string_to_input_line("•"_s, pos);
                pos += text_size.total_size.x * ui_get_current_font_scale(ui);
            }
        } else {
            auto input_text_size = ui_text(ui, where_to_draw_text_input, utf32_to_utf8(&text_inputs[i], temp_allocator), 1.0f, 1.0f, Text_Halign::Left, Text_Valign::Top); // @Speed @Hack 2018-08-28
            
            auto where_to_place_error_string = input_text_size.x_cursor_on_last_line * ui_get_current_font_scale(ui) + 0.03f * ui->screen_size.x;
            if (text_input == i && ui->tis.cursor == text_inputs[text_input].count) where_to_place_error_string = ui->tis.cursor_eased_pos + 0.03f * ui->screen_size.x;
            
            put_string_to_input_line(string_buffer_to_string(&text_input_errors[i]), where_to_place_error_string, SDL_Color{255, 0, 0, 255});
        }
        
    }
    
    {
        cursor_pos.x += TEXT_INPUT_LEFT_MARGIN * ui->screen_size.x;
    }
    
    if (text_input >= 0 && !(active_page == Ui_Page::Tools && tools_page_state.current_state != Tool_Entry_State::None && text_input == 1)) {
        
        ui_clip_rect(ui, active_text_input_clip_rect);
        defer { ui_unclip_rect(ui); };
        
        // draw cursor
        const float blink_exp = 3;
        const auto blink_time = 1.0;
        
        auto cos_t = ng::cos(ng::TAU * ui->tis.blink_timer / blink_time);
        auto cursor_alpha = ng::pow(ng::abs(cos_t), 1.0 / blink_exp);
        if (cos_t < 0) cursor_alpha = -cursor_alpha;
        
        cursor_alpha = (cursor_alpha + 1) / 2;
        
        // apply simple gamma
        cursor_alpha = ng::pow(cursor_alpha, 1.8);
        
        auto draw_cursor = [&](float x, float w, float alpha, float hotness) {
            auto old_clip_rect = ui_push_clip(ui);
            defer { ui_pop_clip(ui, old_clip_rect); };
            
            Rectangle cursor_rect = {};
            
            cursor_rect.pos = cursor_pos;
            
            cursor_rect.x += x;
            cursor_rect.h = get_cursor_height();
            
            interpolate_cursor_width(&cursor_rect, w);
            
            auto c_cursor_rect = rect_to_sdl(cursor_rect);
            {
                auto c = CURSOR_COLOR;
                c.a = ng::lerp(cast(u8) 0, cast(u8) c.a, alpha);
                
                if (hotness > 0) {
                    auto hotness = ng::clamp(ui->tis.hot_timer / CURSOR_MAX_HOTNESS, 0.0f, 1.0f);
                    c = lerp(CURSOR_COLOR, CURSOR_HOT_COLOR, ng::pow(hotness, 1.0 / 3));
                }
                
                SDL_SetRenderDrawColor(renderer, c.r, c.g, c.b, c.a);
            }
            SDL_RenderFillRect(renderer, &c_cursor_rect);
        };
        if (ui_is_selecting(ui)) {
            auto old_clip_rect = ui_push_clip(ui);
            defer { ui_pop_clip(ui, old_clip_rect); };
            
            auto cursor_selection_pos = get_cursor_x(ui->tis.selection_cursor);
            auto disp = ui->tis.cursor_eased_pos - cursor_selection_pos;
            
            { // draw selection box
                Rectangle selection_rect = {};
                
                selection_rect.pos = cursor_pos;
                
                auto min = ng_min(cursor_selection_pos, ui->tis.cursor_eased_pos);
                auto max = ng_max(cursor_selection_pos, ui->tis.cursor_eased_pos);
                selection_rect.x += ng::floor(min);
                selection_rect.w = ng::floor(max) - ng::floor(min);
                selection_rect.h = get_cursor_height();
                
                auto alpha = 0.25f;
                auto c_selection_rect = rect_to_sdl(selection_rect);
                {
                    auto c = CURSOR_COLOR;
                    c.a = ng::lerp(cast(u8) 0, cast(u8) c.a, alpha);
                    SDL_SetRenderDrawColor(renderer, c.r, c.g, c.b, c.a);
                }
                SDL_RenderFillRect(renderer, &c_selection_rect);
            }
        }
        if (window_has_focus && !ui->obscured) draw_cursor(ui->tis.cursor_eased_pos, ui->tis.cursor_eased_width, cursor_alpha, ng::clamp(ui->tis.hot_timer / CURSOR_MAX_HOTNESS, 0.0f, 1.0f));
    }
    
    if (active_page == Ui_Page::Transact || active_page == Ui_Page::History) {
        total_height -= 0.01;
    }
    
    auto inches_to_string = [&tprint](f32 inches) {
        
        if (!(ng::isfinite)(inches)) {
            return "0"_s;
        }
        
        auto sign = ""_s;
        if (inches < 0) {
            sign = "-"_s;
            inches = -inches;
        }
        
        u64 whole_part = inches;
        f32 fraction = inches - whole_part;
        
        u64 numerator = 0;
        u64 denominator = 1;
        
        if (fraction) {
            // Dumb hack to convert arbitrary float to fractions.
            
            do {
                fraction *= 2;
                denominator *= 2;
            } while (fraction - (u64)fraction);
            
            numerator = fraction;
            
            if (whole_part) {
                return tprint("%% %/%"_s, sign, whole_part, numerator, denominator);
            } else {
                return tprint("%%/%"_s, sign, numerator, denominator);
            }
        } else {
            return tprint("%%"_s, sign, whole_part);
        }
    };
    
    auto transact_page_get_tool_name = [&](Tool *tool, Tool_Size tool_size) {
        if (tool->tool_size_type == Tool_Size_Type::Unsized) {
            return tool->name;
        } else if (tool->tool_size_type == Tool_Size_Type::Imperial) {
            return tprint("%1 (%0\")"_s, inches_to_string(tool_size.size), tool->name);
        } else if (tool->tool_size_type == Tool_Size_Type::Metric) {
            return tprint("%1 (%0mm)"_s, tool_size.size, tool->name);
        } else if (tool->tool_size_type == Tool_Size_Type::Thread_Imperial) {
            return tprint("%2 (%0\"-%1)"_s, inches_to_string(tool_size.diameter_in), tool_size.threads_per_in, tool->name);
        } else if (tool->tool_size_type == Tool_Size_Type::Thread_Metric) {
            return tprint("%2 (M%x%mm)"_s, tool_size.diameter_mm, tool_size.pitch_mm, tool->name);
        } else assert(0);
        return tool->name;
    };
    
    {
        
        const auto command_border_w = 0.01f;
        const auto command_border_h = command_border_w;
        const auto command_height = 0.06f;
        
        const auto n = 7;
        int i = 0;
        
        auto command_advance = (1.0f - (command_border_w * 2)) / n;
        auto command_width = command_advance * 0.95f;
        
        auto command_cursor = command_border_w;
        auto button = [&](int flags, auto &&... args) {
            defer {
                i += 1;
                command_cursor += command_advance;
            };
            
            Rectangle button_rect = {};
            button_rect.size = v2(command_width, command_height);
            button_rect.pos = v2(command_cursor + adjust_for_interspace(i, n, command_advance, command_width), total_height - command_border_h - command_height);
            
            return ui_button(ui, flags, button_rect, args...);
        };
        
#define DELETE_HOTKEY_STRING "Delete (Ctrl-D):\n"
#define DELETE_CONTROL_STRING "\n" \
        "Press ENTER or click the button to\n" \
        "confirm deletion. Press any other key\n" \
        "or click anywhere else to cancel."
        
        if (active_page == Ui_Page::Tools) {
            if (tools_page_state.current_state == Tool_Entry_State::None) {
                if (ui_keydown(ui, SDL_SCANCODE_N, cast(SDL_Keymod) KMOD_CTRL) || button(0, "New Tool"_s)) { // @Cast
                    tools_page_state.current_state = Tool_Entry_State::New_Tool;
                    
                    For (text_inputs) clear_text_input(&it);
                    
                    RESTART_FRAME(); //goto begin_frame; // @Goto
                }
                bool sorting_by_search_matches = tools_page_state.tool_sort_mode == Tool_Sort_Mode::Search_Matches;
                if (text_input >= 0 && text_inputs[text_input].count && button(sorting_by_search_matches * (BUT_OFF | BUT_WHITE), "Sort by Search Matches"_s)) {
                    tools_page_state.tool_sort_mode = Tool_Sort_Mode::Search_Matches;
                    
                    update_search();
                }
                bool sorting_by_loaned = tools_page_state.tool_sort_mode == Tool_Sort_Mode::Inventory;
                if (button(sorting_by_loaned * (BUT_OFF | BUT_WHITE), "Loaned & Overdue"_s)) {
                    tools_page_state.tool_sort_mode = Tool_Sort_Mode::Inventory;
                    
                    update_search();
                }
                bool sorting_by_name = tools_page_state.tool_sort_mode == Tool_Sort_Mode::Name;
                if (button(sorting_by_name * (BUT_OFF | BUT_WHITE), "Sort by Name"_s)) {
                    tools_page_state.tool_sort_mode = Tool_Sort_Mode::Name;
                    
                    update_search();
                }
                bool sorting_by_usage = tools_page_state.tool_sort_mode == Tool_Sort_Mode::Most_Loaned;
                if (button(sorting_by_usage * (BUT_OFF | BUT_WHITE), "Sort by Usage"_s)) {
                    tools_page_state.tool_sort_mode = Tool_Sort_Mode::Most_Loaned;
                    
                    update_search();
                }
            } else {
                if (button(BUT_FLASH, "Done"_s)) {
                    // :KeyboardToButtons
                    // Currently some logic is done in SDL event handling. But it should happen at the same place as all of the rest of the UI logic, i.e., where the buttons are.
                    if (progress_data_entry()) RESTART_FRAME(); // @Todo
                }
                if (ui_keydown(ui, SDL_SCANCODE_ESCAPE) || button(0, "Cancel"_s)) {
                    // @Cutnpaste @Temporary @Refactor @Duplicate @Deduplicate
                    
                    switch_to_page(Ui_Page::Tools);
                    
                    switch_text_input(0);
                    
                    RESTART_FRAME(); //goto begin_frame; // @Goto
                }
                
                if (tools_page_state.editing_tool) {
                    
                    auto tool = get_tool(&tool_storage, tools_page_state.editing_tool);
                    assert(tool);
                    
                    auto num_items_string = tprint("all %"_s, tool->infinite ? (-tool->inventory_available) : (tool->inventory_total - tool->inventory_available));
                    //if (tool->infinite) {
                    //num_items_string = tprint("%"_s, -tool->inventory_available);
                    //}
                    
                    auto delete_tooltip = tprint(DELETE_HOTKEY_STRING "This will DELETE the tool %\nand RETURN % of them\nthat are currently loaned."_s, tool->name, num_items_string);
                    
                    
                    if (button(0, "Delete"_s, SDL_Color{255, 64, 64, 255}, delete_tooltip) || ui_keydown(ui, SDL_SCANCODE_D, cast(SDL_Keymod) KMOD_CTRL)) {
                        
                        // @Cutnpaste
                        ui_set_tooltip(ui, delete_tooltip.substring(sizeof(DELETE_HOTKEY_STRING) - 1));
                        string_buffer_append(&ui->tooltip, DELETE_CONTROL_STRING ""_s);
                        really_delete = true;
                        
                        RESTART_FRAME();
                    }
                }
            }
            
            total_height -= command_height + command_border_h * 2;
        } else if (active_page == Ui_Page::Students) {
            if (students_page_state.current_state == Student_Entry_State::None) {
                if (students_page_state.listing_borrowers) {
                    students_page_state.student_sort_mode = Student_Sort_Mode::Most_Owed;
                    
                    total_height -= 0.01;
                } else {
                    
                    bool no_students_checkmarked = true;
                    For (student_storage) {
                        if (!it.checkmarked) continue;
                        
                        no_students_checkmarked = false;
                        break;
                    }
                    if (button(no_students_checkmarked * BUT_OFF, "Bulk Edit"_s, TEXT_COLOR, "Set the class day and/or instructor\nfor checkmarked students."_s)) {
                        students_page_state.current_state = Student_Entry_State::Class_Day_And_Instructor;
                        
                        Class_Day common_class_day = Class_Day::None;
                        
                        For (student_storage) {
                            if (!it.checkmarked) continue;
                            
                            if (common_class_day == Class_Day::None) {
                                
                                // This is the first student we've found.
                                common_class_day = it.class_day;
                                
                                continue;
                            }
                            
                            if (it.class_day != common_class_day) {
                                
                                // This student has a different class day, so there's no common one.
                                common_class_day = Class_Day::None;
                                break;
                            }
                            
                        }
                        
                        s64 common_instructor = 0;
                        For (student_storage) {
                            if (!it.checkmarked) continue;
                            
                            if (common_instructor == 0) {
                                
                                // This is the first student we've found.
                                common_instructor = it.instructor;
                                
                                continue;
                            }
                            
                            if (it.instructor != common_instructor) {
                                
                                // This student has a different instructor, so there's no common one.
                                common_instructor = 0;
                                break;
                            }
                            
                        }
                        
                        if (common_class_day != Class_Day::None) {
                            students_page_state.selected_class_day = common_class_day;
                        }
                        if (common_instructor != 0) {
                            students_page_state.selected_instructor = common_instructor;
                        }
                        
                        For (text_inputs) clear_text_input(&it);
                        
                        switch_text_input(-1);
                        
                        RESTART_FRAME(); //goto begin_frame; // @Goto
                    }
                    
                    
                    if (ui_keydown(ui, SDL_SCANCODE_N, cast(SDL_Keymod) KMOD_CTRL) || button(0, "New Student"_s)) { // @Cast
                        students_page_state.current_state = Student_Entry_State::New_Student;
                        
                        For (text_inputs) clear_text_input(&it);
                        
                        RESTART_FRAME(); //goto begin_frame; // @Goto
                    } 
                    
                    
                    // @@@@ vvvvvvvv
                    // Not supporting sort by search matches, since when you're looking through students you don't want that.
                    // @@@ YOU ACTUALLY DO, THIS IS WRONG!!! ^^^
                    bool sorting_by_owed = 
                        students_page_state.student_sort_mode == Student_Sort_Mode::Most_Owed;
                    if (button(sorting_by_owed * (BUT_OFF | BUT_WHITE), "Owed & Overdue"_s)) {
                        students_page_state.student_sort_mode = Student_Sort_Mode::Most_Owed;
                        
                        update_search();
                    }
                    bool sorting_by_first = 
                        students_page_state.student_sort_mode == Student_Sort_Mode::First_Name;
                    if (button(sorting_by_first * (BUT_OFF | BUT_WHITE), "Sort by First Name"_s)) {
                        students_page_state.student_sort_mode = Student_Sort_Mode::First_Name;
                        
                        update_search();
                    }
                    bool sorting_by_last = 
                        students_page_state.student_sort_mode == Student_Sort_Mode::Last_Name;
                    if (button(sorting_by_last * (BUT_OFF | BUT_WHITE), "Sort by Last Name"_s)) {
                        students_page_state.student_sort_mode = Student_Sort_Mode::Last_Name;
                        
                        update_search();
                    }
                    bool sorting_by_instructor = 
                        students_page_state.student_sort_mode == Student_Sort_Mode::Instructor;
                    if (button(sorting_by_instructor * (BUT_OFF | BUT_WHITE), "Sort by Instructor"_s)) {
                        students_page_state.student_sort_mode = Student_Sort_Mode::Instructor;
                        
                        update_search();
                    }
                    bool sorting_by_class_day = 
                        students_page_state.student_sort_mode == Student_Sort_Mode::Class_Day;
                    if (button(sorting_by_class_day * (BUT_OFF | BUT_WHITE), "Sort by Class Day"_s)) {
                        students_page_state.student_sort_mode = Student_Sort_Mode::Class_Day;
                        
                        update_search();
                    }
                    
                    total_height -= command_height + command_border_h * 2;
                }
            } else if (students_page_state.current_state == Student_Entry_State::Class_Day_And_Instructor) {
                if (button(BUT_FLASH, "Done"_s)) {
                    // Discard return value because we will always restart the frame.
                    progress_data_entry();
                    
                    RESTART_FRAME(); //goto begin_frame; // @Goto
                }
                if (ui_keydown(ui, SDL_SCANCODE_ESCAPE) || button(0, "Cancel"_s)) {
                    // @Cutnpaste @Temporary @Refactor @Duplicate @Deduplicate
                    switch_to_page(Ui_Page::Students);
                    
                    switch_text_input(0);
                    
                    RESTART_FRAME(); //goto begin_frame; // @Goto
                }
                total_height -= command_height * 2;
                {
                    auto cursor = total_height;
                    
                    ui_text(ui, v2(0.03, cursor + 0.01), "Class Day"_s, 1, 1, Text_Halign::Left, Text_Valign::Bottom);
                    
                    for (s64 i = 0; i <= 7; i += 1, cursor -= 0.06) {
                        
                        auto name = "None"_s;
                        if (i) name = class_day_to_string(cast(Class_Day) i);
                        
                        bool enabled = cast(s64) students_page_state.selected_class_day != i;
                        
                        if (ui_button(ui, !enabled * (BUT_OFF | BUT_WHITE), rect(0.02, cursor - 0.06, 0.4, 0.06), name)) {
                            students_page_state.selected_class_day = cast(Class_Day) i;
                        }
                    }
                }
                {
                    auto cursor = total_height + command_height * 2 - 0.06;
                    
                    ui_text(ui, v2(0.52, cursor + 0.01), "Instructor"_s, 1, 1, Text_Halign::Left, Text_Valign::Bottom);
                    
                    for (s64 i = 0; i < instructors.count; i += 1, cursor -= 0.06) {
                        
                        auto name = "[None]"_s;
                        if (i > 0) name = instructors[i];
                        
                        bool enabled = students_page_state.selected_instructor != i;
                        
                        if (ui_button(ui, !enabled * (BUT_OFF | BUT_WHITE), rect(0.50, cursor - 0.06, 0.3, 0.06), name)) {
                            
                            students_page_state.selected_instructor = i;
                            
                        }
                        if (i > 0 && ui_button(ui, 0, rect(0.80, cursor - 0.06, 0.09, 0.06), "Edit"_s)) {
                            students_page_state.current_state = Student_Entry_State::New_Or_Editing_Instructor;
                            students_page_state.new_or_editing_instructor = i;
                            
                            utf8_to_utf32(instructors[i], &text_inputs[0]);
                            
                            switch_text_input(0);
                            
                            RESTART_FRAME();
                        }
                        if (i > 0 && ui_button(ui, BUT_RED, rect(0.89, cursor - 0.06, 0.09, 0.06), "Delete"_s, SDL_Color{255, 0, 0, 255})) {
                            
                            if (students_page_state.selected_instructor == i) 
                                students_page_state.selected_instructor = 0;
                            
                            ng::free_string(&instructors[i], ng::default_allocator);
                            
                            instructors.remove_ordered(i);
                            
                            For (student_storage) {
                                if (it.instructor == i) it.instructor = 0;
                                if (it.instructor > i) it.instructor -= 1;
                            }
                            
                            postprocess(&student_storage, &tool_storage, &transactions, &num_tools_loaned, &total_items_owed, &total_overdue);
                            
                            save_students();
                            
                            RESTART_FRAME();
                        }
                    }
                    if (ui_button(ui, 0, rect(0.50, cursor - 0.06, 0.3, 0.06), "New Instructor"_s)) {
                        students_page_state.current_state = Student_Entry_State::New_Or_Editing_Instructor;
                        students_page_state.new_or_editing_instructor = 0;
                        
                        switch_text_input(0);
                        RESTART_FRAME();
                        
                    }
                    
                    total_height = cursor;
                }
                
                total_height -= command_height + command_border_h * 2;
            } else {
                if (button(BUT_FLASH, "Done"_s)) {
                    // :KeyboardToButtons
                    if (progress_data_entry()) RESTART_FRAME();
                }
                if (ui_keydown(ui, SDL_SCANCODE_ESCAPE) || button(0, "Cancel"_s)) {
                    
                    if (students_page_state.current_state == Student_Entry_State::New_Or_Editing_Instructor) {
                        // @Cutnpaste from progress_data_entry
                        // reset pages
                        For (text_inputs) clear_text_input(&it);
                        switch_text_input(-1);
                        
                        students_page_state.current_state = Student_Entry_State::Class_Day_And_Instructor;
                    } else {
                        cancel_edit_student();
                    }
                    
                    RESTART_FRAME(); //goto begin_frame; // @Goto
                }
                if (students_page_state.editing_student) {
                    
                    auto student = get_student(&student_storage, students_page_state.editing_student);
                    assert(student);
                    
                    auto student_name_string =  tprint("the student named \"%\""_s, student->name);
                    if (!student_registered(student)) {
                        student_name_string = tprint("this unregistered student with ID %"_s, student->student_id);
                    }
                    
                    auto delete_tooltip = tprint(DELETE_HOTKEY_STRING "This will DELETE %,\nand RETURN all % of their loaned items."_s, student_name_string, student->owed_items.count);
                    
                    if (button(0, "Delete"_s, SDL_Color{255, 64, 64, 255}, delete_tooltip) || ui_keydown(ui, SDL_SCANCODE_D, cast(SDL_Keymod) KMOD_CTRL)) { // @Cast
                        
                        // @Cutnpaste
                        ui_set_tooltip(ui, delete_tooltip.substring(sizeof(DELETE_HOTKEY_STRING) - 1));
                        string_buffer_append(&ui->tooltip, DELETE_CONTROL_STRING ""_s);
                        really_delete = true;
                        
                        RESTART_FRAME();
                    }
                }
                
                total_height -= command_height + command_border_h * 2;
            }
            
        }
        
        struct Tool_Info {
            ng::string tooltip;
            SDL_Texture *image;
        };
        auto get_tool_info = [&](Tool *tool, s64 overdue = 0) { 
            Tool_Info result;
            
            auto tool_size_type_string = initializer_list((ng::string) "Unsized"_s, "Imperial"_s, "Metric"_s, "Thread (Imperial)"_s, "Thread (Metric)"_s, "<invalid>"_s)[cast(int) tool->tool_size_type];
#if 1
            result.tooltip = tprint("%"
                                    "\n   Size type: %"
                                    "\n   Max inventory: %"
                                    "%"
                                    "%"
                                    "\n   Current borrower count: %%"
                                    "\n   Times loaned total: %"
                                    ""_s,
                                    tool->name,
                                    tool_size_type_string,
                                    tool->infinite ? "Infinite"_s : tprint("%"_s, tool->inventory_total),
                                    tool->tool_box_number > 0 ? tprint("\n   Tool box no.: %"_s, tool->tool_box_number) : ""_s,
                                    tool->drawer_number > 0 ? tprint("\n   Drawer no.: %"_s, tool->drawer_number) : ""_s,
                                    tool->borrowers.count, overdue ? tprint(" (% overdue)"_s, overdue) : ""_s,
                                    tool->times_loaned);
#else
            ng::string liasion = "a"_s;
            switch (tool_size_type_string[0]) { case 'U': case 'I': liasion = "an"_s; }
            
            ng::string location_preamble = ""_s;
            
            ng::string location_preposition = ""_s;
            
            auto toolbox = ""_s;
            if (tool->tool_box_number > 0) toolbox = tprint(" tool box #%"_s, tool->tool_box_number);
            auto drawer = ""_s;
            if (tool->drawer_number > 0) drawer = tprint(" drawer %"_s, tool->drawer_number);
            
            if (toolbox || drawer) location_preamble = "\nfound at"_s;
            if (toolbox && drawer) location_preposition = " and"_s;
            
            result.tooltip = tprint("% is % % tool%%%% with\n"
                                    "%% current %,%"
                                    "and has been loaned % times total."_s,
                                    tool->name, liasion, tool_size_type_string, location_preamble, toolbox, location_preposition, drawer,
                                    tool->infinite ? "infinite inventory and "_s : ""_s, tool->borrowers.count, pluralize(tool->borrowers.count, "borrowers"_s),
                                    tool->infinite ? "\n"_s : " "_s, tool->times_loaned);
#endif
            result.image = tool->image;
            return result;
        };
        
        const auto list_border_h = 0.01f;
        
        auto ui_scrollbar = [] (Ui_State *ui, const Rectangle area, const int num_items, const float item_advance, float *scroll_eased_pos, int *scroll_index) {
            if (num_items <= 1) {
                *scroll_index = 0;
                *scroll_eased_pos = 0;
                return;
            }
            
            auto inner = expand_about_center(area, -ng::min(ng::min(area.w, area.h) / 4, 0.01));
            ui_rect(ui, area, 0.075);
            ui_clip_rect(ui, area);
            defer { ui_unclip_rect(ui); };
            auto top = inner.y + inner.h; 
            auto bar_h = inner.h / num_items;
            // Make sure you can still grab it with the mouse
            if (bar_h < 0.01f) bar_h = 0.01f;
            auto bar_alpha = 0.25f;
            auto bar_pos_to_y = [](f32 pos, f32 amount, const f32 item_advance, const f32 top, const f32 inner_h, const int num_items) -> f32 {
                f32 y = pos;
                y = scroll_pos_to_index(y, item_advance);
                y += amount;
                y *= -inner_h / num_items;
                y += top;
                return y;
            };
            auto bar_y_to_pos = [](f32 y, f32 amount, const f32 item_advance, const f32 top, const f32 inner_h, const int num_items) -> f32 {
                // Exact inverse function.
                f32 pos = y;
                pos -= top;
                pos /= -inner_h / num_items;
                pos -= amount;
                pos = scroll_index_to_pos(pos, item_advance);
                return pos;
            };
            auto bar_rect = rect(inner.x, bar_pos_to_y(*scroll_eased_pos, 1.0f, item_advance, top, inner.h, num_items), inner.w, bar_h);
            if (!ui->obscured && v2_in_rect(ui->input_state.mouse_pos, bar_rect)) {
                ui_set_tooltip(ui,
                               "Click and drag this scrollbar\n"
                               "or use the mousewheel to scroll\n"
                               "through the list."_s);
                bar_alpha = 0.5f;
                if (ui->input_state.lmb_down) {
                    ui->dragging_scrollbar = scroll_eased_pos;
                    ui->drag_offset = bar_pos_to_y(*scroll_eased_pos, 0.5f, item_advance, top, inner.h, num_items) - ui->input_state.mouse_pos.y;
                }
            }
            if (!ui->obscured && ui->dragging_scrollbar == scroll_eased_pos) {
                if (ui->input_state.lmb_down || ui->input_state.lmb) {
                    bar_alpha = 0.75f;
                    *scroll_eased_pos = bar_y_to_pos(ui->input_state.mouse_pos.y + ui->drag_offset, 0.5f, item_advance, top, inner.h, num_items);
                    *scroll_eased_pos = ng::clamp(*scroll_eased_pos, 0, scroll_index_to_pos(num_items - 1, item_advance));
                    *scroll_index = scroll_pos_to_index(*scroll_eased_pos, item_advance) + 0.5f;
                    assert(*scroll_index >= 0);
                    assert(*scroll_index < num_items);
                } else {
                    ui->dragging_scrollbar = nullptr;
                    ui->drag_offset = 0;
                }
            }
            // Recompute bar rect, in case it was reassigned by dragging.
            bar_rect = rect(inner.x, bar_pos_to_y(*scroll_eased_pos, 1.0f, item_advance, top, inner.h, num_items), inner.w, bar_h);
            ui_rect(ui, bar_rect, bar_alpha);
        };
        
        if (!ui->input_state.lmb_down && !ui->input_state.lmb) {
            ui->dragging_scrollbar = nullptr;
            ui->drag_offset = 0;
        }
        
        const auto list_top = total_height;
        
        if (active_page == Ui_Page::Transact) {
            
            //if (transact_page_state.who == 0) {
            //transact_page_state.returns.clear(); // Yup. This is seriously how i'm gonna do it. I mean... what does it even mean to be robust? Because this sure as heck seems to meet most of the criteria... 2018-11-22 when i was really really tired
            //}
            
            //if (transact_page_state.current_state == Transact_Entry_State::None && text_input < 0)
            
            if (transact_page_state.current_state == Transact_Entry_State::None) {
                
                auto old_obscured = ui->obscured;
                defer { ui->obscured = old_obscured; };
                if (text_input >= 0) ui->obscured = true; // We render anyway.
                
                // back-most panel
                ui_rect(ui, rect_lbrt(0.01, 0, 0.99, list_top - 0.01), 0.125);
                
                // left-hand panel
                auto loaned_list_rect = rect_lbrt(0.02, 0.01, 0.49, list_top - 0.01 - 0.06);
                ui_rect(ui, loaned_list_rect, 0.125);
                ui_text(ui, v2(0.02, list_top - 0.01 - 0.01), "Student's Loaned Tools"_s, 1, 0.875, Text_Halign::Left, Text_Valign::Top, TEXT_COLOR,
                        "List of all tools currently loaned\n"
                        "out by this student, with their quantities."_s);
                
                // right-hand panel
                auto pending_list_rect = rect_lbrt(0.50, 0.01, 0.98, list_top - 0.01 - 0.06);
                ui_rect(ui, pending_list_rect, 0.125);
                ui_text(ui, v2(0.50, list_top - 0.01 - 0.01), "Pending Transactions"_s, 1, 0.875, Text_Halign::Left, Text_Valign::Top, TEXT_COLOR,
                        "List of all pending loans and returns.\n"
                        "None of these have been saved, because\n"
                        "they must be Performed & Saved first."_s);
                
                // return-button column
                //ui_rect(ui, rect(0.37, 0.01, 0.12, list_top - 0.04 - 0.01), 0.125);
                
                
                
                auto student = get_student(&student_storage, transact_page_state.who);
                if (transact_page_state.who) {
                    assert(student != nullptr);
                }
                
                
                
                bool can_return_all = student && student->owed_items.count > 0;
                
                if (ui_button(ui, !can_return_all * BUT_OFF | BUT_RED, rect(0.35, list_top - 0.02 - 0.04, 0.14, 0.04), "Return Everything"_s, SDL_Color{255, 0, 0, 255},
                              "Add everything currently loaned out by\n"
                              "this student to the pending returns list."_s)) {
                    
                    transact_page_state.returns.clear();
                    
                    For (student->owed_items) {
                        
                        auto ret = transact_page_state.returns.push();
                        ret->tool_id = it.tool_id;
                        ret->tool_size = it.tool_size;
                        ret->quantity = it.quantity;
                    }
                    
                }
                
                bool any_tx = (transact_page_state.loans.count + transact_page_state.returns.count) > 0;
                if (ui_button(ui, !any_tx * BUT_OFF | BUT_RED, rect(0.75, list_top - 0.02 - 0.04, 0.10, 0.04), "Cancel All"_s, SDL_Color{255, 0, 0, 255}, "Remove and cancel all pending transactions."_s)) {
                    transact_page_state.loans.clear();
                    transact_page_state.returns.clear();
                }
                
                // loaned list
                
                if (student) {
                    auto scrollbar_rect = loaned_list_rect;
                    scrollbar_rect.x = 0.47;
                    scrollbar_rect.w = 0.02;
                    int dummy = scroll_index_to_pos(scroll_pos_owed, 0.05);
                    
                    int owed_items_to_list = 0;
                    For (student->owed_items) {
                        
                        it.new_quantity = it.quantity;
                        
                        for (auto &&ret : transact_page_state.returns) {
                            if (ret.tool_id == it.tool_id && tool_size_equal(ret.tool_size, it.tool_size)) {
                                
                                it.new_quantity -= ret.quantity;
                                
                                break;
                            }
                        }
                        
                        if (it.new_quantity > 0) owed_items_to_list += 1;
                    }
                    
                    if (owed_items_to_list > 0) {
                        
                        if (!ui->obscured && v2_in_rect(ui->input_state.mouse_pos, loaned_list_rect)) {
                            scroll_pos_owed += scroll_index_to_pos(wheel_offset_this_frame, 0.05);scroll_pos_owed = ng::clamp(scroll_pos_owed, 0, scroll_index_to_pos(owed_items_to_list - 1, 0.05));
                            wheel_offset_this_frame = 0;
                        }
                        
                        ui_scrollbar(ui, scrollbar_rect, owed_items_to_list, 0.05, &scroll_pos_owed, &dummy);
                    }
                    
                    auto cursor = list_top - 0.01 - 0.06 - 0.01 + scroll_pos_owed;
                    
                    ui_clip_rect(ui, loaned_list_rect);
                    defer { ui_unclip_rect(ui); };
                    
                    for (int i = 0; i < student->owed_items.count; i += 1, cursor -= 0.05) {
                        auto &&it = student->owed_items[i];
                        
                        if (it.new_quantity <= 0) {
                            cursor += 0.05;
                            continue;
                        }
                        
                        auto item_rect = rect(0.03, cursor - 0.04, 0.46 - 0.03, 0.04);
                        auto bigger_item_rect = item_rect;
                        {
                            auto &r = bigger_item_rect;
                            r.y -= 0.005;
                            r.h += 0.01;
                            if (it.overdue) ui_rect(ui, r, SDL_Color{255, 0, 0, 128});
                            ui_rect(ui, r, i & 1 ? 0 : 0.25);
                        }
                        
                        auto tool = get_tool(&tool_storage, it.tool_id);
                        auto name = tool ? tool->name : "[Deleted Tool]"_s;
                        
                        auto full_name = name;
                        if (tool) full_name = transact_page_get_tool_name(tool, it.tool_size);
                        
                        ui_text(ui, v2_of_rect(item_rect, v2(0.08, 0.5)), tprint("%"_s, it.new_quantity), 1, 0.75, Text_Halign::Right, Text_Valign::Center);
                        
                        ui_text(ui, v2_of_rect(item_rect, v2(0.10, 0.5)), full_name, 1, 0.75, Text_Halign::Left, Text_Valign::Center);
                        
                        // To answer the question asked here (check source control): We'd rather have an overflowing tool inventory than forcibly prevent students from returning their owed items. Only the former can be remedied later.
                        
                        bool return_already_exists = false;
                        for (auto &&ret : transact_page_state.returns) {
                            if (ret.tool_id == it.tool_id && tool_size_equal(ret.tool_size, it.tool_size)) {
                                
                                return_already_exists = true;
                                
                                break;
                            }
                        }
                        bool can_return = !return_already_exists;
                        
                        if (ui_button(ui, !can_return * BUT_OFF, rect(0.46 - 0.03 - 0.03, cursor - 0.04, 0.03, 0.04), "One"_s, TEXT_COLOR, tprint(
                            "Add one %\n"
                            "to the pending returns list."_s, full_name))) {
                            
                            auto ret = transact_page_state.returns.push();
                            
                            ret->tool_id = tool->tool_id;
                            ret->tool_size = it.tool_size;
                            ret->quantity = 1;
                            
                        }
                        if (ui_button(ui, !can_return * BUT_OFF, rect(0.46 - 0.03, cursor - 0.04, 0.03, 0.04), "All"_s, TEXT_COLOR,
                                      tprint(
                            "Add all % of %\n"
                            "to the pending returns list."_s, it.quantity, full_name))) {
                            
                            auto ret = transact_page_state.returns.push();
                            
                            ret->tool_id = tool->tool_id;
                            ret->tool_size = it.tool_size;
                            ret->quantity = it.quantity;
                        }
                        
                        if (!ui->tooltip_assigned && v2_in_rect(ui->input_state.mouse_pos, bigger_item_rect)) {
                            auto info = get_tool_info(tool);
                            ui_set_tooltip(ui, tprint("%oan made %:\n%"_s, it.quantity > 1 ? "Earliest l"_s : "L"_s, get_time_strings(it.earliest, temp_allocator).short_time, info.tooltip), info.image);
                            // ui_set_tooltip(ui, tprint("%oan made %."_s, it.quantity > 1 ? "Earliest l"_s : "L"_s, get_time_strings(it.earliest, temp_allocator).long_time));
                        }
                    }
                } else {
                    assert(transact_page_state.returns.count == 0); // Can't return from no student.
                }
                
                // pending list
                auto num_pending = transact_page_state.loans.count + transact_page_state.returns.count;
                if (num_pending) {
                    auto scrollbar_rect = pending_list_rect;
                    scrollbar_rect.x = 0.96;
                    scrollbar_rect.w = 0.02;
                    int dummy = scroll_index_to_pos(scroll_pos_pending, 0.05);
                    
                    if (!ui->obscured && v2_in_rect(ui->input_state.mouse_pos, pending_list_rect)) {
                        scroll_pos_pending += scroll_index_to_pos(wheel_offset_this_frame, 0.05);
                        scroll_pos_pending = ng::clamp(scroll_pos_pending, 0, scroll_index_to_pos(num_pending - 1, 0.05));
                        wheel_offset_this_frame = 0;
                    }
                    
                    ui_scrollbar(ui, scrollbar_rect, num_pending, 0.05, &scroll_pos_pending, &dummy);
                }
                
                auto cursor = list_top - 0.01 - 0.06 - 0.01 + scroll_pos_pending; // @Cutnpaste
                
                ui_clip_rect(ui, pending_list_rect);
                
#if 0
                auto get_resultant_inventory = [&](Tool *tool, const ng::array<Pending_Tx> *pending_loans, const ng::array<Pending_Tx> *pending_returns, const Pending_Tx *loan_to_exclude, const Pending_Tx *return_to_exclude) -> s64 {
                    assert(tool);
                    
                    auto running_total = tool->inventory_available;
                    
                    For (*pending_loans) {
                        if (it.tool_id != tool->tool_id) continue;
                        
                        // This is kind of a @Hack. For the record, it's here so that we can peer into the future and consider how a specific transaction would change the running total if we were to flip it from a return to a loan or vice versa. 2018-01-28
                        if (&it == loan_to_exclude) continue;
                        
                        running_total -= it.quantity;
                    }
                    For (*pending_returns) { // @Cutnpaste almost @Redundant with above
                        if (it.tool_id != tool->tool_id) continue;
                        
                        // This is kind of a @Hack. For the record, it's here so that we can peer into the future and consider how a specific transaction would change the running total if we were to flip it from a return to a loan or vice versa. 2018-01-28
                        if (&it == return_to_exclude) continue;
                        
                        running_total += it.quantity;
                    }
                    
                    return running_total;
                };
#endif
                
                auto draw_pending_txs = [&](ng::array<Pending_Tx> *txs, bool is_loan) {
                    for (int i = 0; i < txs->count; cursor -= 0.05) {
                        
                        auto &&pending_tx = (*txs)[i];
                        
                        if (pending_tx.quantity <= 0) {
                            txs->remove_ordered(i);
                            continue;
                        }
                        
                        auto item_rect = rect(0.51, cursor - 0.04, 0.44, 0.04);
                        auto bigger_item_rect = item_rect;
                        {
                            auto &r = bigger_item_rect;
                            r.y -= 0.005;
                            r.h += 0.01;
                            ui_rect(ui, r, i & 1 ? 0.25 : 0);
                        }
                        
                        auto tool = get_tool(&tool_storage, pending_tx.tool_id);
                        assert(tool);
                        
                        //auto resultant_inventory = get_resultant_inventory(tool, &transact_page_state.loans, &transact_page_state.returns, nullptr, nullptr);
                        
                        ui_text(ui, v2_of_rect(item_rect, v2(0.005, 0.5)), is_loan ? "Loan"_s : "Return"_s, 1, 0.75, Text_Halign::Left, Text_Valign::Center, SDL_Color{255, 255, 255, 255});
                        ui_text(ui, v2_of_rect(item_rect, v2(0.17, 0.5)), tprint("%"_s, pending_tx.quantity), 1, 0.75, Text_Halign::Right, Text_Valign::Center);
                        
                        bool can_increase_quantity = true; 
                        bool can_decrease_quantity = true;
                        if (pending_tx.quantity <= 1) {
                            can_decrease_quantity = false;
                            pending_tx.quantity = 1;
                        }
                        
                        if (is_loan) {
                            if (tool->infinite) {
                                can_increase_quantity = true;
                            } else {
                                
                                if (tool->inventory_available - pending_tx.quantity <= 0) can_increase_quantity = false;
                                
                                //auto resultant_inventory_if_we_tx_another = resultant_inventory - 1;
                                //if (resultant_inventory_if_we_tx_another < 0) can_increase_quantity = false;
                            }
                        } else {
                            assert(student);
                            
                            For (student->owed_items) {
                                if (it.tool_id == pending_tx.tool_id && tool_size_equal(it.tool_size, pending_tx.tool_size)) {
                                    
                                    if (pending_tx.quantity >= it.quantity) {
                                        can_increase_quantity = false;
                                        
                                        pending_tx.quantity = it.quantity;
                                    }
                                    
                                    break;
                                }
                            }
                            
                        }
                        
                        auto           inc_tooltip = "Increase the quantity of\nthis pending return."_s;
                        auto           dec_tooltip = "Decrease the quantity of\nthis pending return."_s;
                        if (is_loan) { inc_tooltip = "Increase the quantity of\nthis pending loan."_s;
                            ;          dec_tooltip = "Decrease the quantity of\nthis pending loan."_s;
                        }
                        
                        if (ui_button(ui, !can_increase_quantity * BUT_OFF,
                                      rect_of_rect(item_rect, rect(0.18, 0.5, 0.04, 0.5)), "+"_s, TEXT_COLOR, inc_tooltip)) {
                            pending_tx.quantity += 1;
                        }
                        if (ui_button(ui, !can_decrease_quantity * BUT_OFF,
                                      rect_of_rect(item_rect, rect(0.18, 0, 0.04, 0.5)), "-"_s, TEXT_COLOR, dec_tooltip)) {
                            pending_tx.quantity -= 1;
                        }
                        
                        ui_text(ui, v2_of_rect(item_rect, v2(0.29, 0.5)), transact_page_get_tool_name(tool, pending_tx.tool_size), 1, 0.75, Text_Halign::Left, Text_Valign::Center);
                        
                        auto         remove_tooltip = "Remove this pending loan."_s;
                        if (is_loan) remove_tooltip = "Remove this pending return."_s;
                        if (ui_button(ui, 0, rect_of_rect(item_rect, rect(0.23, 0, 0.05, 1)), "X"_s, TEXT_COLOR, remove_tooltip)) {
                            
                            txs->remove_ordered(i);
                            
                        } else {
                            
                            i += 1;
                            
                        }
                        
                        //if (!ui->tooltip_assigned && v2_in_rect(ui->input_state.mouse_pos, bigger_item_rect)) {
                        //auto info = get_tool_info(tool);
                        //ui_set_tooltip(ui, info.tooltip, info.image);
                        //}
                        
                    }
                };
                
                draw_pending_txs(&transact_page_state.loans, true);
                if (transact_page_state.loans.count) cursor -= 0.02;
                draw_pending_txs(&transact_page_state.returns, false);
                
                ui_unclip_rect(ui);
                
                auto perform_pending_txs = [&] {
                    
                    assert(student);
                    
                    For (transact_page_state.loans) {
                        auto n = it.quantity;
                        for (s64 i = 0; i < n; i += 1) {
                            add_transaction(0, it.tool_id, student->student_id, it.tool_size); // @Todo :Size
                        }
                        
                    }
                    For (transact_page_state.returns) {
                        auto n = it.quantity;
                        for (s64 i = 0; i < n; i += 1) {
                            add_transaction(TRANSACTION_IS_RETURN, it.tool_id, student->student_id, it.tool_size); // @Todo :Size
                        }
                    }
                    
                    transact_page_state.loans.clear();
                    transact_page_state.returns.clear();
                    
                    save_transactions(); // :Save
                    // save_all(); // :Save
                    
                    postprocess(&student_storage, &tool_storage, &transactions, &num_tools_loaned, &total_items_owed, &total_overdue);
                    
                };
                
                bool can_perform_and_save = any_tx && student;
                if (can_perform_and_save && ui_keydown(ui, SDL_SCANCODE_RETURN) || ui_button(ui, !can_perform_and_save * BUT_OFF | BUT_FLASH, rect(0.86, list_top - 0.02 - 0.04, 0.12, 0.04), "Perform & Save"_s, TEXT_COLOR,
                                                                                             "Perform all the pending transactions,\n"
                                                                                             "save the database, and clear the pending\n"
                                                                                             "transactions window."_s)) {
                    
                    perform_pending_txs();
                    
                }
                
                
            } else if (transact_page_state.current_state == Transact_Entry_State::Size) {
                if (button(BUT_FLASH, "Done"_s)) {
                    // :KeyboardToButtons
                    if (progress_data_entry()) RESTART_FRAME();
                }
                if (ui_keydown(ui, SDL_SCANCODE_ESCAPE) || button(0, "Cancel"_s)) {
                    transact_page_state.which = 0;
                    transact_page_state.current_state = Transact_Entry_State::None;
                    
                    For (text_inputs) clear_text_input(&it);
                    switch_text_input(1);
                    
                    RESTART_FRAME(); //goto begin_frame; // @Goto
                }
            }
        }
        
        if (active_list() != Ui_List::None) { // draw list // render list
            
            if (ui->tis.changed) {
                
#if 1
                if (active_list() == Ui_List::Tools) {
                    if (!text_inputs[text_input].count) {
                        if (tools_page_state.tool_sort_mode == Tool_Sort_Mode::Search_Matches) {
                            tools_page_state.tool_sort_mode = Tool_Sort_Mode::Name;
                        }
                    } else if (!old_text_input_count) {
                        tools_page_state.tool_sort_mode = Tool_Sort_Mode::Search_Matches;
                    }
                }
#else
                bool empty_before = false;//!old_text_input_count;
                bool empty_after = false;//text_input >= 0 && !text_inputs[text_input].count;
                
                if (empty_after) { // This overrides empty_before.
                    // Use defaults
                    if (active_list() == Ui_List::Tools) tools_page_state.tool_sort_mode = Tool_Sort_Mode::Name;
                    if (active_list() == Ui_List::Students) students_page_state.student_sort_mode = Student_Sort_Mode::Last_Name;
                } else if (empty_before) {
                    // Switch to search
                    if (active_list() == Ui_List::Tools) tools_page_state.tool_sort_mode = Tool_Sort_Mode::Search_Matches;
                    if (active_list() == Ui_List::Students) students_page_state.student_sort_mode = Student_Sort_Mode::Last_Name;
                }
#endif
                
                update_search();
            }
            
            bool tools = (active_list() == Ui_List::Tools);
            bool students = (active_list() == Ui_List::Students);
            bool transact = (active_page == Ui_Page::Transact);
            bool history = (active_list() == Ui_List::History);
            
            // @Todo: if (text_input->count) { render search; } else render all;
            auto num_items = get_num_list_items();
            
            //auto item_advance = LIST_ITEM_HEIGHT;
            //auto item_spacing = item_advance / 10;
            //auto item_height = item_advance - item_spacing;
            auto item_height = get_text_size(&ui->font_data, ""_s).total_size.y * ui_get_current_font_scale(ui) / ui->screen_size.y;
            
            if (history) item_height *= 0.75f;
            
            auto item_spacing = item_height * 0.125f;
            auto item_advance = item_height + item_spacing;
            auto item_width = 0.73f;
            
            Rectangle list_rect = {};
            {
                list_rect.x = 0.01;
                list_rect.y = 0.02;
                if (transact) list_rect.y = 0.08; // @Refactor
                list_rect.w = 0.95 - 2 * list_rect.x;
                list_rect.h = list_top - list_rect.y;
            }
            auto background = list_rect;
            background.w = 0.99 - background.x;
            if (transact) {
                ui_rect(ui, background, SDL_Color{255, 255, 255, 224});
            } else {
                ui_rect(ui, background, 0.125);
            }
            
            if (ui_keydown(ui, SDL_SCANCODE_DOWN, KMOD_NONE, true)) scroll_index += 1;
            if (ui_keydown(ui, SDL_SCANCODE_UP, KMOD_NONE, true)) scroll_index -= 1;
            
            if (ui_keydown(ui, SDL_SCANCODE_PAGEDOWN, KMOD_NONE, true)) scroll_index += list_rect.h / item_advance - 2;
            if (ui_keydown(ui, SDL_SCANCODE_PAGEUP, KMOD_NONE, true)) scroll_index -= list_rect.h / item_advance - 2;
            
            if (num_items > 0) { // draw scrollbar // render scrollbar
                
                if (!ui->obscured) {
                    // Don't check any rectangular area, just do it.
                    scroll_index += wheel_offset_this_frame;
                    wheel_offset_this_frame = 0;
                }
                
                auto area = rect(0.94, list_rect.y, 0.05, 0);
                area.h = list_top - area.y;
                ui_scrollbar(ui, area, num_items, item_advance, &scroll_eased_pos, &scroll_index);
                //} else {
                //ui->dragging_scrollbar = nullptr;
            }
            
            scroll_index = ng::clamp(scroll_index, 0, get_num_list_items() - 1);
            
            ui_clip_rect(ui, list_rect);
            
            Rectangle item_rect = {};
            item_rect.size = v2(list_rect.w * 0.98, item_height);
            item_rect.x = (list_rect.x + list_rect.w * 0.5) - item_rect.w / 2;
            item_rect.y = list_top - list_border_h - item_height;
            
            item_rect.y += scroll_eased_pos;
            
            Tool *listing_borrowers_tool = nullptr;
            if (students && students_page_state.listing_borrowers) {
                listing_borrowers_tool = get_tool(&tool_storage, students_page_state.listing_borrowers);
                //assert(listing_borrowers_tool);
            }
            
            //int transaction_index = 0;
            
            for (int i = 0; i < num_items; i += 1) {
                //bool transaction_skipped = false;
                defer {
                    //if (!transaction_skipped)
                    item_rect.y -= item_advance;
                };
                
                //defer { transaction_index += 1; };
                
                if (item_rect.y > list_rect.y + list_rect.h) continue;
                if (item_rect.y < list_rect.y - item_height) break;
                
                auto item_rect_center = rect_center(item_rect);
                auto bigger_item_rect = item_rect;
                {
                    auto &r = bigger_item_rect;
                    r.y -= 0.0025;
                    r.h += 0.005;
                }
                auto draw_list_bg_rect = [&](int i, bool red) {
                    if (red) ui_rect(ui, bigger_item_rect, SDL_Color{255, 0, 0, 128});
                    ui_rect(ui, bigger_item_rect, i & 1 ? 0.25 : 0);
                };
                
                f32 running_indent = 0;
                
                const bool is_highlighted = i == scroll_index;
                
                if (tools) {
                    auto &&it = tools_page_state.results[i];
                    auto tool = get_tool(&tool_storage, it.tool_id);
                    
                    if (ui->tis.changed && text_input >= 0 && text_inputs[text_input].count) {
                        assert(it.name_query_matches);
                    }
                    
                    assert(it.name == tool->name);
                    
                    s64 overdue = 0;
                    For (tool->borrowers) if (it.overdue) overdue += 1;
                    draw_list_bg_rect(i, overdue > 0);
                    
                    const float AVAILABLE_SCALE = 1.0f;
                    
                    auto available_pos = v2_of_rect(item_rect, v2(0.5, 0.5));
                    auto size = ui_text(ui, available_pos, tprint("% "_s, tool->infinite ? -tool->inventory_available : tool->inventory_available), 1, AVAILABLE_SCALE, Text_Halign::Left, Text_Valign::Center);
                    
                    auto total_string = "loaned"_s;
                    
                    if (!tool->infinite) {
                        total_string = tprint("/ %"_s, tool->inventory_total);
                    }
                    
                    auto inventory_total_text_pos = available_pos;
                    inventory_total_text_pos.x += size.x_cursor_on_last_line * ui_get_current_font_scale(ui) * AVAILABLE_SCALE / ui->screen_size.x;
                    
                    inventory_total_text_pos.y -= size.total_size.y / 2 * ui_get_current_font_scale(ui) * AVAILABLE_SCALE / ui->screen_size.y;
                    
                    ui_text(ui, inventory_total_text_pos, total_string, 0.75, 0.75, Text_Halign::Left, Text_Valign::Bottom);
                    
                    {
                        auto mark = temp_allocator->get_mark(); // @Hack
                        defer { temp_allocator->set_mark(mark); };
                        // render tool name // render name of tool
                        ui_text(ui, v2_of_rect(item_rect, v2(0, 0.5)), tprint(" %"_s, tool->name), 1.0f, 1.0f, Text_Halign::Left, Text_Valign::Center);
                    }
                    
                    if (ui_button(ui, !can_loan(tool) * BUT_OFF | is_highlighted * BUT_FLASH, rect_of_rect(item_rect, rect(transact ? 0.935 : 0.785, 0, 0.06, 1)), "Loan"_s, TEXT_COLOR, "Loan this tool to a student."_s)) {
                        
                        if (!transact) {
                            // @Todo @Incomplete
                            switch_to_page(Ui_Page::Transact);
                            
                            switch_text_input(0);
                            
                            // @Todo: @Incomplete?
                        }
                        
                        transact_page_push_loan_or_go_to_size_menu(tool);
                        
                        RESTART_FRAME(); // @Goto
                        break; // @Goto @Cleanup @Todo @Hack
                    }
                    
                    if (!transact) {
                        
                        // @Cutnpaste
                        bool keydown = ui_keydown(ui, SDL_SCANCODE_B, cast(SDL_Keymod) KMOD_CTRL);
                        if (!is_highlighted) keydown = false;
                        
                        if (ui_button(ui, 0, rect_of_rect(item_rect, rect(0.85, 0, 0.08, 1)), "Borrowers"_s, TEXT_COLOR, "List the students currently borrowing this tool."_s)
                            || keydown) {
                            // @Cutnpaste from the tab switching buttons
                            switch_to_page(Ui_Page::Students);
                            
                            switch_text_input(0);
                            
                            utf8_to_utf32(BORROWING_STRING, &text_inputs[text_input]);
                            
                            auto id_string = tprint("%"_s, tool->tool_id);
                            
                            utf8_to_utf32(id_string, &text_inputs[text_input]);
                            
                            ui->tis.cursor = text_inputs[text_input].count;
                            ui->tis.selection_cursor = ui->tis.cursor;
                            ui->tis.cursor_eased_pos = get_cursor_x(ui->tis.cursor);
                            
                            update_search();
                            
                            RESTART_FRAME(); //goto begin_frame; // @Goto
                        }
                    }
                    
                    if (!transact) {
                        
                        // @Cutnpaste
                        bool keydown = ui_keydown(ui, SDL_SCANCODE_E, cast(SDL_Keymod) KMOD_CTRL);
                        if (!is_highlighted) keydown = false;
                        
                        if (ui_button(ui, 0, rect_of_rect(item_rect, rect(0.935, 0, 0.06, 1)), "Edit"_s, TEXT_COLOR,
                                      "Edit the name, inventory, size type,\n"
                                      "tool box number, drawer number, and\n"
                                      "image file associated with this tool;\n"
                                      "or delete this tool."_s)
                            || keydown) {
                            // @Todo @Incomplete
                            tools_page_state.current_state = Tool_Entry_State::Edit_Tool;
                            
                            tools_page_state.editing_tool = tool->tool_id;
                            tools_page_state.entry_size_type = tool->tool_size_type;
                            
                            For (text_inputs) clear_text_input(&it);
                            
                            // Fill the text inputs in with the existing data
                            utf8_to_utf32(tool->name, &text_inputs[0]);
                            For (tprint("%"_s, tool->inventory_total)) text_inputs[2].push(it);
                            if (tool->tool_box_number > 0) For (tprint("%"_s, tool->tool_box_number)) text_inputs[3].push(it);
                            if (tool->drawer_number > 0) For (tprint("%"_s, tool->drawer_number)) text_inputs[4].push(it);
                            utf8_to_utf32(tool->image_filename, &text_inputs[5]);
                            
                            switch_text_input(0);
                            
                            // @Hack @Todo @Incomplete
                            break;
                        }
                    }
                    
                    if (!ui->tooltip_assigned && v2_in_rect(ui->input_state.mouse_pos, bigger_item_rect)) {
                        auto info = get_tool_info(tool, overdue);
                        ui_set_tooltip(ui, info.tooltip, info.image);
                    }
                    
                } else if (students) {
                    auto &&it = students_page_state.results[i];
                    auto student = get_student(&student_storage, it.student_id);
                    
                    //assert(student);
                    
                    //auto owed_text = ""_s;
                    s64 owed_number = 0;
                    s64 overdue = 0;
                    auto tooltip = ""_s;
                    
                    if (student) {
                        
                        if (listing_borrowers_tool) {
                            For (student->owed_items) {
                                if (it.tool_id == listing_borrowers_tool->tool_id) {
                                    //owed_text = tprint("% owed"_s, it.quantity);
                                    owed_number = it.quantity;
                                    
                                    if (it.overdue) overdue = 1;
                                    break;
                                }
                            }
                            
                            assert(owed_number > 0);
                            
                            tooltip = tprint("This student owes % %%."_s, owed_number, listing_borrowers_tool->name, overdue ? ",\nand the loan is overdue"_s : ""_s);
                        } else {
                            
                            s64 total_owed_items = 0;
                            For (student->owed_items) {
                                total_owed_items += it.quantity;
                                if (it.overdue) overdue += 1;
                            }
                            
                            //owed_text = tprint("% owed"_s, total_owed_items);
                            owed_number = total_owed_items;
                            
                            if (owed_number) {
                                tooltip = tprint("This student owes % % across % %%."_s, owed_number, pluralize(owed_number, "items"_s), student->owed_items.count, pluralize(student->owed_items.count, "tools"_s), overdue ? tprint(",\n% of which % overdue"_s, overdue, pluralize(overdue, "are"_s, "is"_s)) : ""_s);
                            } else {
                                tooltip = "This student doesn't owe any items."_s;
                            }
                        }
                    }
                    
                    draw_list_bg_rect(i, overdue > 0);
                    
                    //ui_text(ui, v2_of_rect(item_rect, v2(0.7, 0.5)), owed_text, 1, 1, Text_Halign::Left, Text_Valign::Center, TEXT_COLOR, tooltip);
                    ui_text(ui, v2_of_rect(item_rect, v2(0.7, 0.5)), tprint("% owed"_s, owed_number, overdue), owed_number ? 1 : 0.25, 1, Text_Halign::Left, Text_Valign::Center, TEXT_COLOR);
                    
                    
                    auto name = "[Unregistered Student]"_s;
                    if (!student) name = "[Add This Student]"_s;
                    else {
                        if (student_registered(student)) name = student->name;
                    }
                    
                    if (!transact && student_registered(student) && !students_page_state.listing_borrowers) {
                        
                        // @Cutnpaste
                        bool keydown = ui_keydown(ui, SDL_SCANCODE_R, cast(SDL_Keymod) KMOD_CTRL);
                        bool key = ui_key(ui, SDL_SCANCODE_R, cast(SDL_Keymod) KMOD_CTRL);
                        if (!is_highlighted) {
                            keydown = false;
                            key = false;
                        }
                        
                        if (ui_button(ui, key * BUT_DOWN | BUT_RED, rect_of_rect(item_rect, rect(0, 0, 0.02, 1)), student->checkmarked ? "X"_s : ""_s, SDL_Color{255, 0, 0, 255},
                                      "Checkmark this student in order to\n"
                                      "select this student for bulk editing of\n"
                                      "their class day and instructor."_s)
                            || keydown) {
                            student->checkmarked = !student->checkmarked;
                        }
                    }
                    auto name_pos = v2_of_rect(item_rect, v2(transact ? 0 : 0.02, 0.5));
                    auto name_size = ui_text(ui, name_pos, tprint(" % "_s, name), 1, 1, Text_Halign::Left, Text_Valign::Center);
                    
                    name_pos.x += ng::max(0.2, name_size.total_size.x * ui_get_current_font_scale(ui) / ui->screen_size.x);
                    name_pos.y = item_rect.y;
                    
                    auto class_day_string = ""_s;
                    auto instructor = ""_s;
                    
                    if (student && !transact) {
                        instructor = "No instructor"_s;
                        
                        if (student->instructor > 0 && student->instructor < instructors.count) {
                            instructor = instructors[student->instructor];
                        }
                        
                        instructor = tprint(" - %"_s, instructor);
                        
                        class_day_string = class_day_to_string(student->class_day).substring(0, 3);
                        if (class_day_string) class_day_string = tprint(" (%)"_s, class_day_string);
                    }
                    
                    auto subtitle = tprint("#%%%"_s, it.student_id, instructor, class_day_string);
                    
                    ui_text(ui, name_pos,   subtitle, 0.75f, 0.75f, Text_Halign::Left, Text_Valign::Bottom);
                    
                    bool is_highlighted = i == scroll_index;
                    
                    if (student) {
                        
                        auto tooltip =
                            "Automatically switch to the\n"
                            "Transact page to loan and return\n"
                            "tools to this student."_s;
                        if (transact) tooltip =
                            "Choose this student\n"
                            "for loaning and returning tools."_s;
                        
                        if (ui_button(ui, is_highlighted * BUT_FLASH, rect_of_rect(item_rect, rect(transact ? 0.915 : 0.85, 0, 0.08, 1)), "Transact"_s, TEXT_COLOR, tooltip)) {
                            
                            if (!transact) {
                                switch_to_page(Ui_Page::Transact);
                                
                                switch_text_input(0);
                                
                                For (text_inputs) clear_text_input(&it);
                            }
                            
                            transact_page_state.who = student->student_id;
                            
                            clear_text_input(&text_inputs[0]);
                            
                            switch_text_input(-1);
                            
                            RESTART_FRAME(); // @Goto
                        }
                        
                    } else {
                        
                        assert(transact);
                        
                        if (ui_button(ui, is_highlighted * BUT_FLASH, rect_of_rect(item_rect, rect(transact ? 0.915 : 0.85, 0, 0.08, 1)), "Add"_s, TEXT_COLOR,
                                      "Register a new student with\n"
                                      "the entered ID, then return\n"
                                      "to the Transact page."_s)) {
                            
                            transact_page_switch_to_add_student();
                            RESTART_FRAME(); // @Goto
                        }
                        
                    }
                    
                    if (!transact) {
                        
                        // @Cutnpaste
                        bool keydown = ui_keydown(ui, SDL_SCANCODE_E, cast(SDL_Keymod) KMOD_CTRL);
                        if (!is_highlighted) keydown = false;
                        
                        if (ui_button(ui, 0, rect_of_rect(item_rect, rect(0.935, 0, 0.06, 1)), "Edit"_s, TEXT_COLOR,
                                      "Edit the name of this\n"
                                      "student; or delete this student."_s)
                            || keydown) {
                            // @Todo @Incomplete
                            
                            students_page_state.editing_student = student->student_id;
                            
                            students_page_state.current_state = Student_Entry_State::Edit_Student;
                            
                            For (text_inputs) clear_text_input(&it);
                            
                            auto id_string = tprint("%"_s, students_page_state.editing_student);
                            
                            // Fill the text input in with the current name
                            utf8_to_utf32(student->name, &text_inputs[1]);
                            
                            switch_text_input(1);
                            // @Hack @Todo @Incomplete
                            break;
                        }
                    }
                    
                    if (!ui->tooltip_assigned && v2_in_rect(ui->input_state.mouse_pos, bigger_item_rect)) {
                        ui_set_tooltip(ui, tooltip);
                    }
                    
                } else if (history) {
                    
                    //auto transaction = transactions[transactions.count - i - 1];
                    auto transaction = history_page_state.results[history_page_state.results.count - i - 1];
                    
                    //int quantity = 0;
                    
                    auto student = get_student(&student_storage, transaction->student_id);
                    auto tool = get_tool(&tool_storage, transaction->tool_id);
                    
                    //if (!student || !tool) {
                    //transaction_skipped = true;
                    //continue;
                    //}
                    
                    //defer {
                    //i -= 1;
                    //};
                    
                    //int j = i;
                    //for (; j < transactions.count; j += 1) {
                    
                    //auto &&it2 = transactions[transactions.count - j - 1];
                    
                    //if (it2.flags != transaction->flags) break;
                    //if (it2.unix_timestamp != transaction->unix_timestamp) break;
                    //if (it2.student_id != transaction->student_id) break;
                    //if (it2.tool_id != transaction->tool_id) break;
                    
                    //if (tool->tool_size_type != Tool_Size_Type::Unsized) {
                    //if (!tool_size_equal(it2.tool_size, transaction->tool_size)) break;
                    //}
                    
                    //}
                    
                    //quantity += j - i + 1;
                    
                    //i = j;
                    
                    //draw_list_bg_rect(transaction_index, false);
                    draw_list_bg_rect(i, false);
                    
                    //auto time_strings = get_time_strings(transaction->unix_timestamp, temp_allocator);
                    //auto s = time_strings.long_time;
                    //while (s && s[0] != ' ') ++s;
                    //++s;
                    auto time = decompose_unix_timestamp_gregorian(transaction->unix_timestamp, get_system_timezone_offset());
                    
                    //auto label = tprint("% % % % on %"_s,
                    //student->name,
                    //transaction->flags & TRANSACTION_IS_RETURN ? "returned"_s : "loaned"_s,
                    //quantity,
                    //tool->name,
                    //s
                    //);
                    
                    ng::string student_name = {};
                    if (student && student_registered(student)) {
                        student_name = student->name;
                    } else {
                        student_name = tprint("[Student #%]"_s, transaction->student_id);
                    }
                    
                    auto tool_name = tool ? transact_page_get_tool_name(tool, transaction->tool_size) : "[Deleted Tool]"_s;
                    
                    auto label = tprint("% % % on % %% % %:%:%"_s,
                                        student_name,
                                        transaction->flags & TRANSACTION_IS_RETURN ? "returned"_s : "loaned"_s,
                                        tool_name,
                                        get_month_string(time.month).substring(0, 3), time.day + 1, ordinal(time.day + 1), time.year,
                                        ng::fmt_int(time.hour, false, 10, 2),
                                        ng::fmt_int(time.minute, false, 10, 2),
                                        ng::fmt_int(time.second, false, 10, 2));
                    
                    ui_text(ui, v2_of_rect(item_rect, v2(0.01, 0.5)), label, 1, 0.75f, Text_Halign::Left, Text_Valign::Center);
                    
                } else assert(0);
            }
            
            {
                auto rate = 1.0e-6;
                scroll_eased_pos = damp(scroll_eased_pos, scroll_index_to_pos(scroll_index, item_advance), rate, dt);
            }
            
            ui_unclip_rect(ui);
            if (active_page == Ui_Page::Transact && active_list() != Ui_List::None) {
                if (ui_button(ui, 0, rect_lbrt(0.01, 0.01, 0.99, 0.08), "Back to Transactions"_s)) {
                    switch_text_input(-1);
                    RESTART_FRAME(); //goto begin_frame;
                }
            }
        }
        {
            auto i = cast(int) active_page;
            assert(i >= 0 && i < 5);
        }
    }
    
    if (framebuffer_fader >= FRAMEBUFFER_FADE_TIME) {
        // clear and update the fade buffer
        
        //SDL_SetRenderTarget(renderer, fade_buffer);
        //SDL_RenderCopy(renderer, framebuffer, nullptr, nullptr);
    } else {
        // keep the fade buffer as it is so we can fade from it to the framebuffer
        
        //ng::print("fading\n"_s);
        
        //SDL_SetRenderTarget(renderer, framebuffer);
        
        auto alpha = 1 - ng::clamp(framebuffer_fader, 0, FRAMEBUFFER_FADE_TIME) / FRAMEBUFFER_FADE_TIME;
        SDL_SetTextureAlphaMod(fade_buffer, alpha * 255.f);
        
        //SDL_RenderCopy(renderer, fade_buffer, nullptr, nullptr);
        
        framebuffer_fader += dt;
    }
    
    //SDL_SetRenderTarget(renderer, nullptr);
    //SDL_RenderCopy(renderer, framebuffer, nullptr, nullptr);
    
    {
        auto s = get_time_strings(ng::get_unix_timestamp(), temp_allocator);
        
        ui_text(ui, v2(0.99, 0.99), s.short_time, 0.75f, 0.75f, Text_Halign::Right, Text_Valign::Top, TEXT_COLOR, s.long_time);
    }
    
    if (active_page == Ui_Page::Locked && !ui->tooltip_assigned) {
        ui_set_tooltip(ui,
                       "Hover over items to show a tooltip\n"
                       "giving more information on the item."_s);
    }
    
    if (really_delete) { // @Hack mutates a lot of state.
        ui->tooltip_timer = TOOLTIP_END;
        
        ui_rect(ui, rect(0, 0, 1, 1), 0.5);
        
        auto old_obscured = ui->obscured;
        defer { ui->obscured = old_obscured; };
        
        ui->obscured = false;
        
        //For (ui->input_state.keys_down) 
        
        auto keydown = ui_keydown(ui, SDL_SCANCODE_RETURN);
        
        if (ui_button(ui, BUT_RED, rect(0.42, 0.46 + 0.3, 0.16, 0.08), "REALLY Delete!"_s, SDL_Color{255, 0, 0, 255}) || keydown) {
            
            if (active_page == Ui_Page::Tools) {
                assert(tools_page_state.current_state == Tool_Entry_State::Edit_Tool);
                
                auto tool = get_tool(&tool_storage, tools_page_state.editing_tool);
                assert(tool);
                
                auto unix_timestamp = ng::get_unix_timestamp();
                
                for (auto &&borrower : tool->borrowers) {
                    s64 total_loans_from_this_student = 0;
                    
                    auto student = get_student(&student_storage, borrower.student_id);
                    assert(student);
                    
                    for (auto &&owed : student->owed_items) { // @Speed @Hack
                        if (owed.tool_id != tool->tool_id) continue;
                        
                        auto n = owed.quantity;
                        for (s64 i = 0; i < n; i += 1) {
                            add_transaction(TRANSACTION_IS_RETURN, tool->tool_id, student->student_id, owed.tool_size, unix_timestamp);
                        }
                        
                        total_loans_from_this_student += n;
                    }
                    
                    assert(total_loans_from_this_student == borrower.quantity);
                }
                
                delete_tool(&tool_storage, tool);
                
                postprocess(&student_storage, &tool_storage, &transactions, &num_tools_loaned, &total_items_owed, &total_overdue);
                save_all();
                
                // @Cutnpaste @Temporary @Refactor @Duplicate @Deduplicate
                
                switch_to_page(Ui_Page::Tools);
                
                switch_text_input(0);
                
            } else if (active_page == Ui_Page::Students) {
                assert(students_page_state.current_state == Student_Entry_State::Edit_Student);
                
                auto student = get_student(&student_storage, students_page_state.editing_student);
                assert(student);
                
                auto unix_timestamp = ng::get_unix_timestamp();
                
                for (auto &&owed : student->owed_items) {
                    auto n = owed.quantity;
                    for (s64 i = 0; i < n; i += 1) {
                        add_transaction(TRANSACTION_IS_RETURN, owed.tool_id, student->student_id, owed.tool_size, unix_timestamp);
                    }
                }
                
                delete_student(&student_storage, student);
                
                postprocess(&student_storage, &tool_storage, &transactions, &num_tools_loaned, &total_items_owed, &total_overdue);
                save_all();
                
                // @Cutnpaste @Temporary @Refactor @Duplicate @Deduplicate
                switch_to_page(Ui_Page::Students);
                
                switch_text_input(0);
                
            } else assert(0);
            
            really_delete = false;
            
            RESTART_FRAME(); //goto begin_frame; // @Goto
        } else {
            // We didn't left-click INSIDE the button, but maybe we clicked OUTSIDE? Or right-clicked?
            // We check for this and leave the really_delete interface if we did.
            if (ui->input_state.lmb_up ||
                ui->input_state.rmb) {
                really_delete = false;
            }
        }
    }
    
    if (ui->tooltip_assigned) {
        ui->tooltip_timer += dt;
        if (ui->tooltip_timer > TOOLTIP_END) {
            ui->tooltip_timer = TOOLTIP_END;
        }
    } else {
        ui->tooltip_timer -= dt * 2; // @Hack.
        if (ui->tooltip_timer < TOOLTIP_BEGIN) {
            ui->tooltip_timer = 0;
        }
    }
    ui->tooltip_assigned = false;
    
    if (ui->tooltip_timer > TOOLTIP_BEGIN) {
        
        const auto s = ui_get_current_font_scale(ui);
        
        const auto TOOLTIP_SCALE = 1.0f;
        
        auto alpha = tooltip_alpha(ui->tooltip_timer);
        
        auto tooltip_pos = ui->input_state.mouse_pos + v2(8 / ui->screen_size.x, 16 / -ui->screen_size.y);
        
        auto image_size = v2();
        
        if (ui->tooltip_image) {
            int w = 0, h = 0;
            SDL_QueryTexture(ui->tooltip_image, nullptr, nullptr, &w, &h);
            image_size = v2(w / ui->screen_size.x, h / ui->screen_size.y);
        }
        
        //auto image_longest = ng::max(image_size.x, image_size.y);
        //if (image_longest > 0.50f) {
        
        //}
        
        float *wider = &image_size.x;
        float *thinner = &image_size.y;
        
        if (*thinner > *wider) ng::swap(wider, thinner);
        
        if (*wider > 0.50f) {
            auto overscale = 0.50f / *wider;
            
            *wider = 0.50f;
            *thinner *= overscale;
        }
        
        auto tooltip_string = string_buffer_to_string(&ui->tooltip);
        
        auto tooltip_text_size_measure = get_text_size(&ui->font_data, tooltip_string, TOOLTIP_SCALE);
        auto tooltip_text_size = tooltip_text_size_measure.total_size;
        tooltip_text_size.x *= s / ui->screen_size.x;
        tooltip_text_size.y *= s / ui->screen_size.y;
        
        auto tooltip_text_expanded_size = tooltip_text_size;
        tooltip_text_expanded_size.x = tooltip_text_size.x + 2 * ui->font_data.space_width * s / ui->screen_size.x;
        tooltip_text_expanded_size.y = tooltip_text_size.y + 0.25f * ui->font_data.line_advance * s / ui->screen_size.y;
        
        auto tooltip_size = tooltip_text_expanded_size;
        tooltip_size.y += image_size.y;
        tooltip_size.x = ng::max(tooltip_size.x, image_size.x);
        
        auto tooltip_rect = rect(tooltip_pos, tooltip_size);
        tooltip_rect.y -= tooltip_rect.h;
        
        if (tooltip_rect.x < 0.02) {
            tooltip_rect.x = 0.02;
        }
        if (tooltip_rect.x > 0.98 - tooltip_rect.w) {
            tooltip_rect.x = 0.98 - tooltip_rect.w;
        }
        if (tooltip_rect.y < 0.02) {
            tooltip_rect.y = 0.02;
        }
        if (tooltip_rect.y > 0.98 - tooltip_rect.h) {
            tooltip_rect.y = 0.98 - tooltip_rect.h;
        }
        
        auto tooltip_text_pos = v2(tooltip_rect.x + tooltip_text_expanded_size.x * 0.5f, tooltip_rect.y + (tooltip_text_expanded_size.y - tooltip_text_size.y) / 2);
        
        ui_rect(ui, tooltip_rect, alpha * 0.75f);
        ui_text(ui, tooltip_text_pos, tooltip_string, alpha, TOOLTIP_SCALE, Text_Halign::Center, Text_Valign::Bottom, SDL_Color{255, 255, 255, 255});
        
        {
            auto dest = rect_to_sdl(rect_world_to_screen(rect(tooltip_rect.pos + v2((tooltip_rect.w - image_size.x) / 2, tooltip_text_size.y), image_size), ui->screen_size));
            SDL_SetTextureAlphaMod(ui->tooltip_image, alpha * 255);
            SDL_RenderCopy(renderer, ui->tooltip_image, nullptr, &dest);
            
            if (draw_bounds) {
                SDL_SetRenderDrawColor(renderer, 255, 0, 0, 255);
                SDL_RenderDrawRect(renderer, &dest);
            }
        }
    }
    
    if (saved_message_timer <= SAVED_MESSAGE_TIME) {
        
        ui_rect(ui, rect(0.50 - 0.06, 0.50 - 0.035, 0.12, 0.07), saved_message_alpha(saved_message_timer));
        ui_text(ui, v2(0.50, 0.50), "Saved."_s, saved_message_alpha(saved_message_timer), 1.25, Text_Halign::Center, Text_Valign::Center, SDL_Color{255, 255, 255, 255});
        
        saved_message_timer += dt;
    }
    
    SDL_RenderPresent(renderer);
    
#define MIN_DT (1.0f / 60)
#define MAX_DT (1.0f / 10)
    auto delay = dt;
    delay = ng_clamp(delay, MIN_DT, MAX_DT);
    // delay = MIN_DT;
    //SDL_Delay(cast(int)(delay * 1000) - 0.5f);
    SDL_Delay(8);
    
    goto begin_frame;
}

#ifdef __cplusplus
extern "C" {
#endif // __cplusplus
    
    void mainCRTStartup() { ExitProcess(main()); }
    void WinMainCRTStartup() { mainCRTStartup(); }
    void __stdcall _DLLMainCRTStartup() { mainCRTStartup(); }
    /* int _fltused = 0x9875; */
    void *memset(void *dest, int x, size_t size);
    void *memcpy(void *dest, const void *src, size_t size);
    __pragma(intrinsic(memset));
    __pragma(intrinsic(memcpy));
    __pragma(function(memset));
    __pragma(function(memcpy));
    void *memset(void *dest, int x, size_t size) { return ng::memset(dest, cast(u8) x, size); }
    void *memcpy(void *dest, const void *src, size_t size) { return ng::memcpy(dest, src, size); }
    
#ifdef __cplusplus
}
#endif // __cplusplus

// BOTTOM
