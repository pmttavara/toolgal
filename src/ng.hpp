#pragma once // ng.hpp replaces C/C++ standard libs with literally no includes.
// To implement this header, define NG_DEFINE in a cpp file before including it.
/* To the extent possible under law, the author(s) have dedicated all copyright
 * and related and neighboring rights to this software to the public domain
 * worldwide. This software is distributed without any warranty.
 * You should have received a copy of the CC0 Public Domain Dedication along
 * with this software. If not, see
 * <http://creativecommons.org/publicdomain/zero/1.0/>. */
#define ng_cast(...) (__VA_ARGS__) // variadic so templates work
// assert macro
#if defined(__GNUC__) || defined(__clang__)
#define ng_break() __asm__ __volatile__("int $3")
#elif defined(_MSC_VER)
#define ng_break() do { __debugbreak(); } while (0)
#else // we tried
#define ng_break() do { (*(int *)0 = 0xdead); } while (0)
#endif // platform
#ifndef ng_inline
#if defined(_MSC_VER)
#define ng_inline inline __forceinline
#define ng_noinline __declspec(noinline)
#elif defined(__GNUC__) || defined(__clang__)
#define ng_inline inline __attribute__((always_inline))
#define ng_noinline __attribute__((noinline))
#else
#define ng_inline inline
#define ng_noinline
#endif
#endif
#if (defined(__STDC_VERSION__) && (__STDC_VERSION__ >= 199901L)) || defined(__clang__)
#define NG_FUNCTION __func__
#elif __GNUC__ >= 2 || defined(_MSC_VER)
#define NG_FUNCTION __FUNCTION__
#else
#define NG_FUNCTION ""
#endif // platform
#ifndef NG_ASSERT_ENABLED
#ifdef NDEBUG
#define NG_ASSERT_ENABLED 0
#else
#define NG_ASSERT_ENABLED 1
#endif
#endif
#if NG_ASSERT_ENABLED
#define ng_assert(e, ...)                                                                          \
    do {                                                                                           \
        static bool always_ignore = false;                                                         \
        if (!(e) && !ng::do_assert_failed(__FILE__, __LINE__, NG_FUNCTION, #e, &always_ignore,     \
                                          "" __VA_ARGS__))                                         \
            ng_break();                                                                            \
    } while (0)
#else
#define ng_assert(e, ...) do { if (!(e)) (void)("" __VA_ARGS__); } while (0)
#endif
#define ng_always_on_assert(e, ...) do {                                                           \
        bool i = false;                                                                            \
        if (!(e) && !ng::do_assert_failed(__FILE__, __LINE__, NG_FUNCTION, #e, &i, "" __VA_ARGS__))\
            ng_break();                                                                            \
    } while (0)
struct ng_dummy {};
template <class F> struct ng_deferrer { F f; ~ng_deferrer() { f(); } };
template <class F> ng_deferrer<F> operator*(ng_dummy, F f) { return {f}; }
#define NG_DEFER_(LINE) auto zz_defer##LINE = ng_dummy{} *[&]()
#define NG_DEFER(LINE) NG_DEFER_(LINE)
#define ng_defer NG_DEFER(__LINE__)
namespace ng {
namespace int_types {
using s8 = signed char;
using u8 = unsigned char;
using s16 = signed short;
using u16 = unsigned short;
using s32 = signed int;
using u32 = unsigned int;
using s64 = signed long long;
using u64 = unsigned long long;
using usize = decltype(sizeof 0);
using sptr = decltype((int *)1 - (int *)2);
using f32 = float;
using f64 = double;
static_assert(sizeof(s32) == 4, "s32 not 32-bit");
static_assert(sizeof(s64) == 8, "s64 not 64-bit");
static_assert(sizeof(f32) == 4, "f32 not 32-bit");
static_assert(sizeof(f64) == 8, "f64 not 64-bit");
} // namespace int_types
using namespace int_types;
extern bool has_static_init; // True if the CRT initialized globals at startup.
template <class T, usize N> ng_inline constexpr u64 countof(T (&)[N]) { return N; }
template <class T> using id = T;
#define ng_bitcast(...) *(ng::id<__VA_ARGS__> *) &
#define ng_swap(a, b)                                                                              \
    do {                                                                                           \
        auto &&a_{a}, &&b_{b};                                                                     \
        auto c_{a_};                                                                               \
        a_ = b_;                                                                                   \
        b_ = c_;                                                                                   \
    } while (0)
#define ng_min(a, b) ((a) < (b) ? (a) : (b))
#define ng_max(a, b) ((a) > (b) ? (a) : (b))
#define ng_clamp(t, min, max) ((t) >= (min) ? (t) <= (max) ? (t) : (max) : (min))
#define ng_lerp(a, b, t) ((a) * (1 - (t)) + (b) * (t))
#define ng_abs(x) ((x) > 0 ? (x) : -(x))
template <class T> void swap(T &a, T &b) { ng_swap(a, b); }
template <class T, class U> auto(min)(T a, U b) { return ng_min(a, b); }
template <class T, class U> auto(max)(T a, U b) { return ng_max(a, b); }
template <class T, class U, class V> T clamp(T t, U min, V max) { return ng_clamp(t, min, max); }
template <class T> T lerp(T a, T b, f32 t) { return ng_lerp(a, b, t); } 
template <class T> T abs(T x) { return ng_abs(x); }
template <class T> T sign(T x) { return x >= 0 ? x > 0 ? 1 : 0 : -1; }
// @Todo: Make MS intrinsics for these, turn these into fallbacks.
// _mm_cvtepi32_ps()
// _mm_cvtsi64x_si128(-0.0)
inline bool (isfinite)(f32 f) {
    return (ng_bitcast(u32)(f) & ((1u << 31) - 1)) < 0x7f800000;
}
inline bool (isfinite)(f64 f) {
    return (ng_bitcast(u64)(f) & ((1ull << 63) - 1)) < 0x7ff0000000000000;
}
inline bool (isinf)(f32 f) {
    return (ng_bitcast(u32)(f) & ((1u << 31) - 1)) == 0x7f800000;
}
inline bool (isinf)(f64 f) {
    return (ng_bitcast(u64)(f) & ((1ull << 63) - 1)) == 0x7ff0000000000000;
}
inline bool (isnan)(f32 f) {
    return (ng_bitcast(u32)(f) & ((1u << 31) - 1)) > 0x7f800000;
}
inline bool (isnan)(f64 f) {
    return (ng_bitcast(u64)(f) & ((1ull << 63) - 1)) > 0x7ff0000000000000;
}
constexpr f64 TAU = 6.2831853071795864769252867665590057683943387987502116419;
constexpr f64 SQRT2 = 1.41421356237309504880168872420969807856967187537694807;
constexpr f64 EULERS = 2.7182818284590452353602874713526624977572470936999595;
constexpr f64 PHI = 1.6180339887498948482045868343656381177203091798057628621;
constexpr f64 inf = 1e+300 * 1e+300;
constexpr f64 nan = -(1e+300 * 1e+300 * 0);
constexpr f32 max_frac_f32 = 8388607.5f;
constexpr f64 max_frac_f64 = 4503599627370495.5;
inline void memswap(void *a, void *b, usize n) { // @Todo: use rap xchg on x86
    auto asz = ng_cast(usize *) a, bsz = ng_cast(usize *) b;
    while (n >= sizeof(usize)) {
        ng_swap(*asz, *bsz);
        asz += 1;
        bsz += 1;
        n -= sizeof(usize);
    }
    auto a8 = ng_cast(u8 *) a, b8 = ng_cast(u8 *) b;
    while (n > 0) {
        ng_swap(*a8, *b8);
        a8 += 1;
        b8 += 1;
        n -= 1;
    }
}
inline s64 utf8strlen(const u8 *str) {
    s64 result = 0;
    while (*str) {
        if ((*str & 0xc0) ^ 0x80) result += 1;
        str += 1;
    }
    return result;
}
void exit(int code);
void *malloc(u64 size);
void *realloc(void *ptr, usize new_size);
void free(void *ptr);
#ifndef NG_NO_CRT
void *memset(void *destination, u8 value, usize size);
void *memcpy(void *destination, const void *source, usize size);
void *memmove(void *destination, const void *source, usize size);
int memcmp(const void *memblock_a, const void *memblock_b, usize size);
usize strlen(const char *c_string);
int strcmp(const char *c_string_a, const char *c_string_b);
int strncmp(const char *c_string_a, const char *c_string_b, usize size);
f32 abs(f32 x);
f64 abs(f64 x);
f32 round(f32 x);
f64 round(f64 x);
f32 floor(f32 x);
f64 floor(f64 x);
f32 ceil(f32 x);
f64 ceil(f64 x);
f32 mod(f32 x, f32 y);
f64 mod(f64 x, f64 y);
f32 sqrt(f32 x);
f64 sqrt(f64 x);
inline f64 rsqrt(f64 x) { return 1.0 / ng::sqrt(x); }
f32 pow(f32 base, f32 exponent);
f64 pow(f64 base, f64 exponent);
f32 sin(f32 x);
f64 sin(f64 x);
f32 cos(f32 x);
f64 cos(f64 x);
f32 tan(f32 x);
f64 tan(f64 x);
f32 asin(f32 x);
f64 asin(f64 x);
f32 acos(f32 x);
f64 acos(f64 x);
f32 atan(f32 x);
f64 atan(f64 x);
f32 atan2(f32 y, f32 x);
f64 atan2(f64 y, f64 x);    
// @Todo: make custom version of #include <time.h>; s64 get_unix_timestamp() for CRT-based path
#else
#if defined(_MSC_VER) && (defined(_M_IX86) || defined(_M_X64))
#define NG_MS_INTRINSICS
extern "C" {
// ======== __INTRIN_H_
extern void __stosb(ng::u8 *dest, ng::u8 data, ng::usize count);      
extern void __stosd(ng::u32 *dest, ng::u32 data, ng::usize count);    
extern void __movsb(ng::u8 *dest, const ng::u8 *source, ng::usize count);
extern void __movsd(ng::u32 *dest, const ng::u32 *source, ng::usize count);
#ifdef _M_X64
extern void __stosq(ng::u32 *dest, ng::u32 data, ng::usize count);
extern void __movsq(ng::u64 *dest, const ng::u64 *source, ng::usize count);
#endif // _M_X64   
// ======== _INCLUDED_MM2  
typedef union __declspec(intrin_type) __declspec(align(16)) __m128 {
    ng::f32 m128_f32[4];

    ng::s8  m128_i8[16];
    ng::s16 m128_i16[8];
    ng::s32 m128_i32[4];
    ng::s64 m128_i64[2];
    ng::u8  m128_u8[16];
    ng::u16 m128_u16[8];
    ng::s32 m128_u32[4];
    ng::u64 m128_u64[2];
} __m128;
extern float _mm_cvtss_f32(ng::__m128 a);
extern ng::__m128 _mm_sqrt_ss(ng::__m128 a);
extern ng::__m128 _mm_rsqrt_ss(ng::__m128 a);
extern ng::__m128 _mm_set_ss(ng::f32 a);   
// extern ng::__m128 _mm_set_ps1(ng::f32 a);
extern ng::__m128 _mm_and_ps(ng::__m128 a, ng::__m128 b); 
extern ng::__m128 _mm_andnot_ps(ng::__m128 a, ng::__m128 b);
extern ng::__m128 _mm_or_ps(ng::__m128 a, ng::__m128 b);
// ======== _INCLUDED_EMM  
typedef union __declspec(intrin_type) __declspec(align(16)) __m128i {
    ng::s8  m128i_i8[16];
    ng::s16 m128i_i16[8];
    ng::s32 m128i_i32[4];
    ng::s64 m128i_i64[2];
    ng::u8  m128i_u8[16];
    ng::u16 m128i_u16[8];
    ng::u32 m128i_u32[4];
    ng::u64 m128i_u64[2];
} __m128i;
typedef struct __declspec(intrin_type) __declspec(align(16)) __m128d {
    ng::f64 m128d_f64[2];
} __m128d;
extern ng::f64 _mm_cvtsd_f64(ng::__m128d a);
extern ng::__m128d _mm_sqrt_pd(ng::__m128d a);
extern ng::__m128d _mm_sqrt_sd(ng::__m128d a, ng::__m128d b);
extern ng::__m128d _mm_set_sd(ng::f64 w);
extern ng::__m128d _mm_andnot_pd(ng::__m128d a, ng::__m128d b);
extern ng::__m128d _mm_and_pd(ng::__m128d a, ng::__m128d b);    
extern ng::__m128d _mm_or_pd(ng::__m128d a, ng::__m128d b);
extern ng::__m128d _mm_cvtsi32_sd(ng::__m128d a, ng::s32 b);
extern ng::__m128i _mm_cvtsi64x_si128(ng::s64 a);
extern ng::__m128d _mm_cvtepi32_pd(ng::__m128i a);       
extern ng::__m128 _mm_cvtepi32_ps(ng::__m128i a);
}
#endif
// @Todo: unit test all of these
#ifdef NG_MS_INTRINSICS
// @Todo: __stosd/__stosq and __movsd/__movsq
ng_inline void *memset(void *p, u8 x, usize n) { return __stosb(ng_cast(u8 *) p, x, n), p; }
ng_inline void *memcpy(void *dest, const void *src, usize n) {
    __movsb(ng_cast(u8 *) dest, ng_cast(u8 *) src, n);
    return dest;
}
#elif defined(__GNUC__) || defined(__clang__)
ng_inline void *memset(void *destination, u8 value, usize size) {
    int d0, d1;
    __asm__ __volatile__("rep stosb"
                         : "=&c"(d0), "=&D"(d1)
                         : "a"(value), "1"(destination), "0"(size)
                         : "memory");
    return destination;
}
ng_inline void *memcpy(void *destination, const void *source, usize size) {
    __asm__ __volatile__("rep movsb" : "+D"(destination), "+S"(source), "+c"(size) : : "memory");
    return destination;
}
#else
ng_inline void *memset(void *destination, u8 value, usize size) {
    auto p = ng_cast(u8 *) destination;
    for (usize i = 0; i < size; i += 1) p[i] = value;
    return destination;
}
ng_inline void *memcpy(void *destination, const void *source, usize size) { 
    auto p1 = ng_cast(u8 *) destination, p2 = ng_cast(u8 *) source;
    for (usize i = 0; i < size; i += 1) p1[i] = p2[i];
    return destination;
}
#endif
void *memmove(void *destination, const void *source, usize size);
inline int memcmp(const void *a, const void *b, usize n) {
    auto u1 = ng_cast(u8 *) a, u2 = ng_cast(u8 *) b;
    while (n) {
        if (*u1 != *u2) {
            return *ng_cast(s8 *) u1 - *ng_cast(s8 *) u2;
        }
        u1 += 1;
        u2 += 1;
        n -= 1;
    }
    return 0;
}
ng_inline usize strlen(const char *s) {
    for (auto p = s;; p += 1) if (!*p) return p - s;
}
ng_inline int strcmp(const char *a, const char *b) {
    while (*a) {
        if (*a != *b) break;
        a += 1;
        b += 1;
    }
    return *ng_cast(u8 *) a - *ng_cast(u8 *) b;
}
ng_inline int strncmp(const char *a, const char *b, usize n) {
    while (n) {
        if (!*a || *a != *b) break;
        a += 1;
        b += 1;
        n -= 1;
    }
    return *ng_cast(u8 *) a - *ng_cast(u8 *) b;
}
#ifdef NG_MS_INTRINSICS
ng_inline f32 sign(f32 x) {
    return _mm_cvtss_f32(_mm_or_ps(_mm_and_ps(_mm_set_ss(-0.f), _mm_set_ss(x)), _mm_set_ss(1.f)));
}
ng_inline f64 sign(f64 x) {
    return _mm_cvtsd_f64(_mm_or_pd(_mm_and_pd(_mm_set_sd(-0.0), _mm_set_sd(x)), _mm_set_sd(1.0)));
}                 
ng_inline f32 abs(f32 x) {
    return _mm_cvtss_f32(_mm_andnot_ps(_mm_set_ss(-0.f), _mm_set_ss(x)));
}
ng_inline f64 abs(f64 x) {
    return _mm_cvtsd_f64(_mm_andnot_pd(_mm_set_sd(-0.0), _mm_set_sd(x)));
} 
#elif 0 // defined(__GNUC__) || defined(__clang__)
#else   
ng_inline f32 sign(f32 x) { return ng_bitcast(u32)(x) & (1u   << 31) ? -1.f : 1.f; }
ng_inline f64 sign(f64 x) { return ng_bitcast(u64)(x) & (1ull << 63) ? -1.0 : 1.0; }
ng_inline f32 abs(f32 x) {
    auto punned = (ng_bitcast(u32)(x)) & ((1u << 31) - 1);
    return ng_bitcast(f32)(punned);
}
ng_inline f64 abs(f64 x) {
    auto punned = (ng_bitcast(u64)(x)) & ((1ull << 63) - 1);
    return ng_bitcast(f64)(punned);
}
#endif
#ifdef NG_MS_INTRINSICS   
ng_inline f32 sqrt(f32 x) { return _mm_cvtss_f32(_mm_sqrt_ss(_mm_set_ss(x))); }
ng_inline f64 sqrt(f64 x) { return _mm_cvtsd_f64(_mm_sqrt_pd(_mm_set_sd(x))); }
ng_inline f32 rsqrt(f32 x) { return _mm_cvtss_f32(_mm_rsqrt_ss(_mm_set_ss(x))); }
ng_inline f64 rsqrt(f64 x) { return 1 / ng::sqrt(x); } // @Speed: 'rsqrtsd'?
#elif defined(__GNUC__) || defined(__clang__)
ng_inline f32 sqrt(f32 x) {
    __asm__("sqrtss %1, %0" : "=x"(x) : "x"(x));
    return x;
}
ng_inline f64 sqrt(f64 x) {
    __asm__("sqrtpd %1, %0" : "=x"(x) : "x"(x));
    return x;
}
ng_inline f32 rsqrt(f32 x) {
    __asm__("rsqrtss %1, %0" : "=x"(x) : "x"(x));
    return x;
}
ng_inline f32 rsqrt(f32 x) {
    __asm__("sqrtsd %1, %0" : "=x"(x) : "x"(x));
    return 1 / x;
}
#else
f64 rsqrt(f64 x);
ng_inline f32 sqrt(f32 x) { return x * ng::rsqrt(x); }
ng_inline f64 sqrt(f64 x) { return x * ng::rsqrt(x); }
#endif // platform
// These aren't assembly because they'd need, like, SSE4.
inline f32 round(f32 x) {
    if (!(ng::abs(x) <= max_frac_f32)) return x;
    return ng_cast(s32)(x > 0 ? x + 0.5f : x - 0.5f);
}
inline f64 round(f64 x) {
    if (!(ng::abs(x) <= max_frac_f64)) return x;
    return ng_cast(s64)(x > 0 ? x + 0.5 : x - 0.5);
}
inline f32 floor(f32 x) {
    if (!(ng::abs(x) <= max_frac_f32)) return x;
    auto intval = ng_cast(s32) x;              
    return intval - (x < intval);
}
inline f64 floor(f64 x) {
    if (!(ng::abs(x) <= max_frac_f64)) return x;
    auto intval = ng_cast(s64) x;            
    return intval - (x < intval);
}
inline f32 ceil(f32 x) {                                          
    if (!(ng::abs(x) <= max_frac_f32)) return x;
    auto intval = ng_cast(s32) x;                           
    return intval + (x > intval);
}
inline f64 ceil(f64 x) {                                          
    if (!(ng::abs(x) <= max_frac_f64)) return x;
    auto intval = ng_cast(s64) x;                           
    return intval + (x > intval);
}
inline f32 frac(f32 x) {
    if (x > max_frac_f32) return 0;
    return x - ng_cast(s32) x + (x < 0);
}
inline f64 frac(f64 x) {
    if (x > max_frac_f64) return 0;
    return x - ng_cast(s64) x + (x < 0);
}
inline f32 mod(f32 x, f32 y) { return x - ng::floor(x / y) * y; } // @Todo: Expand this out so it
inline f64 mod(f64 x, f64 y) { return x - ng::floor(x / y) * y; } // can be optimized, like frac().
inline f64 mod(f32 x, f64 y) { return ng::mod(ng_cast(f64) x, y); }
inline f64 mod(f64 x, f32 y) { return ng::mod(x, ng_cast(f64) y); }
f64 pow(f64 base, f64 exponent);      
// // cos(x) = sin(x + pi/2)
// addps xmm0, PI_2
// movaps xmm1, xmm0
// cmpnltps xmm1, PI
// andps xmm1, PIx2
// subps xmm0, xmm1
// 
// // Parabola
// movaps xmm1, xmm0
// andps xmm1, abs
// mulps xmm1, xmm0
// mulps xmm0, B
// mulps xmm1, C
// addps xmm0, xmm1
// 
// // Extra precision
// movaps xmm1, xmm0
// andps xmm1, abs
// mulps xmm1, xmm0
// subps xmm1, xmm0
// mulps xmm1, P
// addps xmm0, xmm1
f64 sin(f64 x);
f64 cos(f64 x);
f64 tan(f64 x);
f64 asin(f64 x);
f64 acos(f64 x);
f64 atan(f64 x);
inline f32 pow(f32 base, f32 exponent) {
    return ng::pow(ng_cast(f64) base, ng_cast(f64) exponent);
}
inline f32 sin(f32 x) { return ng::sin(ng_cast(f64) x); }
inline f32 cos(f32 x) { return ng::cos(ng_cast(f64) x); }
inline f32 tan(f32 x) { return ng::tan(ng_cast(f64) x); }
inline f32 asin(f32 x) { return ng::asin(ng_cast(f64) x); }
inline f32 acos(f32 x) { return ng::acos(ng_cast(f64) x); }
inline f32 atan(f32 x) { return ng::atan(ng_cast(f64) x); }
f32 atan2(f32 y, f32 x);
f64 atan2(f64 y, f64 x);  
#endif // NG_NO_CRT
inline f64 pow(f64 base, f32 exponent) { return ng::pow(base, ng_cast(f64) exponent); }
inline f64 pow(f32 base, f64 exponent) { return ng::pow(ng_cast(f64) base, exponent); }
inline f64 atan2(f64 y, f32 x) { return ng::atan2(y, ng_cast(f64) x); }
inline f64 atan2(f32 y, f64 x) { return ng::atan2(ng_cast(f64) y, x); }
// random numbers
inline u32 quadratic_residue(u32 x) { // @Attribution
    const u32 prime = 4294967291;
    if (x >= prime) return x;
    u32 residue = ng_cast(u64) x * x % prime;
    return x <= prime / 2 ? residue : prime - residue;
}
inline u32 rand_u32(u32 rng) { return quadratic_residue(quadratic_residue(rng) + 0x682f0161); }
inline u64 rand_u64(u32 rng) { return rand_u32(rng) | ng_cast(u64) rand_u32(rng + 1) << 32; }
inline f32 rand_f32(u32 rng) { return (rand_u32(rng) >> 8) * (1.f / (1ll << 24)); }
inline f64 rand_f64(u32 rng) { return (rand_u64(rng) >> 11) * (1.0 / (1ll << 53)); }
// print function
struct print_buffer {
    u8 *str = nullptr;
    u64 len = 0;
    u64 cap = 0;
    print_buffer() {}
    print_buffer(u8 *str, u64 len, u64 cap) : str{str}, len{len}, cap{cap} {}
    void putchar(u32 c); // @Unicode
    template <u64 N> void put_literal(const char (&lit)[N]) {
        if (NG_ASSERT_ENABLED && len + N > cap) ng_break();
        ng::memcpy(str + len, lit, ng_cast(usize) N - 1);
        len += N - 1;
    }
    void put_u8s(u8 *u8s, u64 count);
    void fill_u8(u8 byte, u64 count);
};
void output_print_buffer(print_buffer buffer);
void output_one_character(u32 c);
struct fmt_int {
    s64 x = 0;
    bool is_signed = false;
    int leading_zeroes = 0;
    int leading_spaces = 0;
    int radix = 10;
    fmt_int(s64 x, bool is_signed = true, int radix = 10, int leading_zeroes = 0,
            int leading_spaces = 0);
};
struct fmt_char {
    u32 c;
    fmt_char(u32 x) { c = x; }
};
// allocator construct
enum struct allocate_mode { allocate = 0, resize = 1, free = 2, free_all = 3 };
using allocator_proc = void *(allocate_mode mode, struct allocator *self, u64 size, u64 old_size,
                              void *block, s64 options);
struct allocator { // @Todo: switch this out for something more like { *proc; *data; }
    allocator_proc *proc;
    void *allocate(u64 size, s64 options = 0) {
        return proc(allocate_mode::allocate, this, size, 0, nullptr, options);
    }
    void *resize(void *block, u64 new_size, u64 old_size, s64 options = 0) {
        return proc(allocate_mode::resize, this, new_size, old_size, block, options);
    }
    void free(void *block, u64 old_size, s64 options = 0) {
        proc(allocate_mode::free, this, 0, old_size, block, options);
    }
    void free_all(s64 options = 0) {
        proc(allocate_mode::free_all, this, 0, 0, nullptr, options);
    }
};
extern allocator *default_allocator;
void allocator_init();

void *mallocator_proc(allocate_mode, allocator *, u64, u64, void *, s64);
struct mallocator : allocator {
    mallocator() : allocator{&mallocator_proc} {}
};

template <u64 STACK_SIZE> void *stack_allocator_proc(allocate_mode mode, allocator *self, u64 size, u64 old_size, void *old_block, s64);
template <u64 STACK_SIZE = 4096 * 16> struct stack_allocator : allocator { // @Todo: remove template, combine with pool allocator.
    stack_allocator() : allocator{&stack_allocator_proc<STACK_SIZE>} {
#ifndef NDEBUG
        ng::memset(data, 0xcc, STACK_SIZE);
#endif
    }
    alignas(alignof(sptr)) u8 data[STACK_SIZE];
    u8 *head = data;
};
template <u64 STACK_SIZE> void *stack_allocator_proc(allocate_mode mode, allocator *self, u64 size, u64 old_size, void *old_block, s64) {
    auto me = ng_cast(stack_allocator<STACK_SIZE> *) self;
    if (mode == allocate_mode::free_all) {
        // *me = {};
        me->head = me->data;
        return nullptr;
    }
    if (old_block && ng_cast(u8 *) old_block + old_size == me->head) { // Most recent alloc
        // grow (or shrink)
        auto new_head = me->head - old_size + size;
        if (new_head > me->data + STACK_SIZE) {
            // request is too big
            return nullptr;
        }
        me->head = new_head;
        return old_block;
    }
    if (me->head + size > me->data + STACK_SIZE) { // The request is too big
        return nullptr;
    }
    if (size <= old_size) { // new block fits in old one, but can't move head.
        return old_block;
    }
    auto result = ng_cast(void *) me->head;
    me->head += size;
    if (old_block) {
        ng::memcpy(result, old_block, old_size);
    }
    return result;
}

void *pool_allocator_proc(allocate_mode, allocator *, u64, u64, void *, s64);
struct pool_allocator : allocator {
    pool_allocator() : allocator{&pool_allocator_proc} {}
    allocator *parent = nullptr;
    void *pool = nullptr;
    char *head = nullptr;
};
pool_allocator create_pool(usize pool_size = 4096 * 32, allocator *parent = default_allocator);
void release_pool(pool_allocator *pool);

void *proxy_allocator_proc(allocate_mode, allocator *, u64, u64, void *, s64);
struct proxy_allocator : allocator {
    allocator *parent = nullptr;
    const char *name = nullptr;
    u64 allocations = 0;
    u64 allocated_bytes = 0;
    proxy_allocator() : allocator{&proxy_allocator_proc} {}
};
proxy_allocator create_proxy_allocator(const char *name, allocator *parent = default_allocator);
void destroy_proxy_allocator(proxy_allocator *proxy);
// strings
struct string {
    s64 len;// = 0;
    u8 *ptr;// = nullptr;
    //constexpr string() = default;
    //constexpr string(s64 len, u8 *ptr) : len{len}, ptr{ptr} {}
    //template <u64 N> constexpr string(const char (&literal)[N]) : len{N}, ptr{literal} {}
    explicit operator bool() { return len > 0 && ptr != nullptr; }
    u8 &operator[](s64 n);
    //string(const char *c_str);
    //string &operator=(const char *c_str);
    string substring(s64 index);
    string substring(s64 index, s64 length);
    auto begin() { return ptr; }
    auto end() { return ptr + len; }

    template <u64 N> constexpr static string c_str(const char (&lit)[N]) { return {N, (u8 *)lit}; }
    static string c_str(const char *c_str) { return {(s64)ng::strlen(c_str), (u8 *)c_str}; }

    //string(const struct print_buffer &b); 
	
	string &operator++() {
		ptr += 1;
		len -= 1;
		return *this;
	}
	string operator++(int) {
		auto result{*this};
		++*this;
		return result;
	}
};
constexpr string operator""_s(const char *str, usize length) { return string{ng_cast(s64) length, ng_cast(u8 *) str}; }
string c_str(string source, allocator *a);
string copy_string(string s, allocator *a);
void free_string(string *s, allocator *a);
s64 string_compare(string a, string b);
s64 string_compare(string a, string b, s64 (*comparator)(u8, u8, void *), void *userdata);
inline s64 string_compare_case_insensitive(string a, string b) {
    return ng::string_compare(a, b,
        [](u8 a, u8 b, void *) {
            if (a >= 'a' && a <= 'z') a &= ~0x20;
            if (b >= 'a' && b <= 'z') b &= ~0x20;
            return ng_cast(s64) a - b;
        }, nullptr);
}
template <class Func> s64 string_compare(string a, string b, Func comparator) {
    return string_compare(a, b, [](u8 a, u8 b, void *userdata) -> s64 {
            return (*ng_cast(Func *) userdata)(a, b);
        }, &comparator);
}
inline s64 utf8strlen(string str) {
    s64 result = 0;
    for (u8 c : str) result += (c >> 6 != 2);
    return result;
}
template <class... Strings> string concatenate(allocator *alloc, Strings &&... args) {
    constexpr auto num_args = sizeof...(Strings);
    string result = {};
    string strings[num_args] = {args...};
    for (auto s : strings) result.len += s.len;
    result.ptr = ng_cast(u8 *) alloc->allocate(result.len);
    if (!result.ptr) return string{};
    auto head = result.ptr;
    for (auto s : strings) {
        ng::memcpy(head, s.ptr, s.len);
        head += s.len;
    }
    return result;
}
// Convert a string to an integer. The string is advanced to the end of the
// number, or to where parsing errored. `result` cannot be null.
bool str_to_s64(string *str, s64 *result);
bool str_to_u64(string *str, u64 *result);
bool str_to_f64(string *str, f64 *result);
void print_item(print_buffer *buf, print_buffer src);
void print_item(print_buffer *buf, string s);
void print_item(print_buffer *buf, bool b);
void print_item(print_buffer *buf, s8 i);
void print_item(print_buffer *buf, u8 u);
void print_item(print_buffer *buf, s16 i);
void print_item(print_buffer *buf, u16 u);
void print_item(print_buffer *buf, s32 i);
void print_item(print_buffer *buf, u32 u);
void print_item(print_buffer *buf, s64 i);
void print_item(print_buffer *buf, u64 u);
void print_item(print_buffer *buf, f32 f);
void print_item(print_buffer *buf, f64 f); // @Todo: fmt_float
void print_item(print_buffer *buf, const fmt_int &fmt);
void print_item(print_buffer *buf, const fmt_char &fmt);
void print_item(print_buffer *buf, const char *ptr);
void print_item(print_buffer *buf, void *ptr);
u64 print_get_item_size(print_buffer src);
u64 print_get_item_size(string s);
u64 print_get_item_size(bool b);
u64 print_get_item_size(s8 i);
u64 print_get_item_size(u8 u);
u64 print_get_item_size(s16 i);
u64 print_get_item_size(u16 u);
u64 print_get_item_size(s32 i);
u64 print_get_item_size(u32 u);
u64 print_get_item_size(s64 i);
u64 print_get_item_size(u64 u);
u64 print_get_item_size(f32 f);
u64 print_get_item_size(f64 f);
u64 print_get_item_size(const fmt_int &f);
u64 print_get_item_size(const fmt_char &f);
u64 print_get_item_size(const char *ptr);
u64 print_get_item_size(void *ptr);
template <u64 N> ng_inline constexpr u64 print_get_item_size(const char (&s)[N]) { return N; }
// positional printing
template <class T, class... Ts>
void print_variant(print_buffer *b, u64 type, u64 ntypes, const T &t, const Ts &... ts) {
    u64 current = ntypes - sizeof...(Ts) - 1;
    if (type == current) print_item(b, t);
    else print_variant(b, type, ntypes, ts...);
}
template <class T> void print_variant(print_buffer *b, u64, u64, const T &t) { print_item(b, t); }
template <class... Args> u64 print_get_size(string fmt, const Args &... args) {
    u64 max = 0;
    u64 sizes[] = {print_get_item_size(fmt), print_get_item_size(args)...};
    for (u64 size : sizes) if (size > max) max = size;
    u64 num_percents = 1; // Always return at least (max * 1).
    for (u8 ch : fmt) if (ch == '%') num_percents += 1;
    return max * num_percents; // This may over-allocate, but skips parsing `fmt`.
}
template <class... Args> void sprint(print_buffer *buffer, string fmt, const Args &... args) {
    if (!fmt) return;
    constexpr auto num_args = ng_cast(s64) sizeof...(Args);
    string lazy_args[num_args] = {};
    s64 arg_index = 0;

    u8 *end = fmt.ptr + fmt.len;

scan:
    u8 *finder = fmt.ptr;
    for (; finder < end; finder += 1) {
        if (*finder == '%') goto percent;
    }
    // flush remainder
    buffer->put_u8s(fmt.ptr, fmt.len);
    return;
percent:
    auto skipped_len = finder - fmt.ptr;
    buffer->put_u8s(fmt.ptr, skipped_len);

    fmt.ptr = finder + 1;
    fmt.len -= skipped_len + 1;

    auto new_fmt = fmt;
    s64 new_arg_index = 0;
    if (str_to_s64(&new_fmt, &new_arg_index)) {
        fmt = new_fmt;
        arg_index = new_arg_index;
    }

    arg_index %= num_args;
    if (arg_index < 0) arg_index += num_args;

    auto lazy = &lazy_args[arg_index];
    if (lazy->ptr) { // even if len = 0, we never want to double-print.
        buffer->put_u8s(lazy->ptr, lazy->len);
    } else {
        lazy->ptr = buffer->str + buffer->len;
        print_variant<Args...>(buffer, arg_index, num_args, args...);
        lazy->len = (buffer->str + buffer->len) - lazy->ptr;
    }
    arg_index += 1;
    goto scan;
}
inline void sprint(print_buffer *buffer, string fmt) { print_item(buffer, fmt); }
template <class... Args> string aprint(allocator *alloc, string s, const Args &... args) {
    if (!alloc) return {};
    print_buffer buffer = {};
    buffer.cap = print_get_size(s, args...);
    if (!buffer.cap) return {};
    buffer.str = ng_cast(u8 *) (alloc->allocate(buffer.cap));
    if (buffer.str) sprint(&buffer, s, args...);
    string result = {};
    if (buffer.len) result.ptr = ng_cast(u8 *) (alloc->resize(buffer.str, buffer.len, buffer.cap));
    if (!result.ptr) { // failed or no data
        alloc->free(buffer.str, buffer.cap);
        return {};
    }
    result.len = buffer.len;
    return result;
}
template <class... Args> string mprint(string s, const Args &... args) {
    return aprint(ng::default_allocator, s, args...);
}
template <class... Args> void print(string s, const Args &... args) {
    const int STACK_BUFFER_SIZE = 1024;
    u8 stack_buffer[STACK_BUFFER_SIZE];             
    print_buffer buffer = {};
    buffer.cap = print_get_size(s, args...);
    bool local = (buffer.cap <= STACK_BUFFER_SIZE);
    if (local) {
        buffer.str = stack_buffer;
    } else {
        buffer.str = ng_cast(u8 *) ng::malloc(buffer.cap);
        if (!buffer.str) return;
    }
    sprint(&buffer, s, args...);
    output_print_buffer(buffer);
    if (!local) ng::free(buffer.str);
}
using assert_failed_proc = bool(const char *file, int line, const char *func, const char *e,
                                bool *always_ignore, string message, void *userdata);
extern assert_failed_proc *assert_failed;
extern void *assert_failed_userdata;
void assert_init();
template <class... Args>
bool do_assert_failed(const char *file, int line, const char *func, const char *e,
                      bool *always_ignore, const char *fmt, const Args &... args) {
    if (!assert_failed) return 0;
    auto str = mprint(ng::string::c_str(fmt), args...);
    auto result =
        ng::assert_failed(file, line, func, e, always_ignore, str, assert_failed_userdata);
    ng::free_string(&str, ng::default_allocator);
    return result;
}
// os
s64 get_unix_timestamp(); // @Incomplete @Todo: Implement for other platforms!
enum struct create_directory_result {
    success,
    already_exists,
    path_not_found,
    path_too_long,
    input_invalid
};
create_directory_result create_directory(string path_name);
// containers
template <class T> struct array {
    allocator *alloc = ng::default_allocator;
    s64 count = 0;
    u64 capacity = 0;
    T *data = nullptr;
    inline const T &operator[](s64 n) const {
        ng_assert(n < count && count >= 0 && ng_cast(u64) count <= capacity);
        return data[n];
    }
    inline T &operator[](s64 n) {
        ng_assert(n < count && count >= 0 && ng_cast(u64) count <= capacity);
        return data[n];
    }
    array copy() const { 
        ng_assert(alloc);
        array result{*this};
        result.data = alloc->allocate(count * sizeof(T));
        if (result.data && data) ng::memcpy(result.data, data, result.capacity * sizeof(T));
        return result;
    }
    bool amortize(s64 new_count) {                       
        ng_assert(alloc);
        if (new_count < 0) return false;
        auto new_capacity = capacity;
        while (new_capacity < ng_cast(u64) new_count) {
            new_capacity = new_capacity * 3 / 2 | 2;
        }
        return reserve(new_capacity);
    }
    bool reserve(u64 new_capacity) {
        ng_assert(alloc);
        if (new_capacity > capacity) {
            if (alloc) {
                auto resized = ng_cast(T *) alloc->resize(data, new_capacity * sizeof(T), capacity * sizeof(T));
                if (!resized) return false;
                data = resized;
            } else {
                alloc = ng::default_allocator;
                if (alloc) data = ng_cast(T *) alloc->allocate(new_capacity * sizeof(T));
                if (!data) return false;
            }
            capacity = new_capacity;
        }
        return true;
    }
    bool resize(s64 new_count, T value = {}) {
        if (!reserve(new_count)) return false;
        for (s64 i = count; i < new_count; i += 1) data[i] = value;
        count = new_count;
        return true;
    }
    void release() {
        ng_assert(alloc);
        if (alloc && data) alloc->free(data, capacity * sizeof(T));
        data = nullptr;
        capacity = 0;
        count = 0;
    }
    T *push(T value = {}) {
        if (count < 0) {
            count += 1;
            return nullptr;
        }
        if (ng_cast(u64) count >= capacity) {
            if (!amortize(count + 1)) return nullptr;
        }
        data[count] = value;
        count += 1;
        return &data[count - 1];
    }
    T pop() {
        ng_assert(count > 0, "Popped an empty ng::array");
        if (count > 0) {
            count -= 1;
            return data[count];
        }
        return T{};
    }
    T *insert(s64 index, T value = {}) {
        if (index >= count) return push(value);
        if (count >= 0 && ng_cast(u64) count >= capacity) amortize(count + 1);
        ng::memmove(data + index + 1, data + index, (count - index) * sizeof(T));
        count += 1;
        data[index] = value;
        return &data[index];
    }
    T remove(s64 index) {
        if (index < count && count > 0) data[index] = data[count - 1];
        ng_assert(index < count, "Removed from an empty ng::array (index = %, count = %)", index, count);
        return pop();
    }
    T remove_ordered(s64 index) {
        ng_assert(index >= 0 && index < count);
        auto result = data[index];
        ng::memmove(data + index, data + index + 1, (count - index - 1) * sizeof(T));
        count -= 1;
        return result;
    }
    void clear() { count = 0; }
    auto begin() const { return data; }
    auto end() const { return data + count; }
};
template <class T> void print_item(print_buffer *buf, const array<T> &a) {
    buf->put_literal("array(count = ");
    print_item(buf, a.count);
    buf->put_literal(", capacity = ");
    print_item(buf, a.capacity);
    buf->put_literal(") {");
    for (int i = 0; i < a.count; i += 1) {
        if (i) buf->put_literal(", ");
        print_item(buf, a.data[i]);
    }
    buf->putchar('}');
}
template <class T> u64 print_get_item_size(const array<T> &a) {
    u64 total = sizeof("array(count = , capacity = ) {");
    total += print_get_item_size(a.count);
    total += print_get_item_size(a.capacity);
    for (int i = 0; i < a.count; i += 1) {
        total += sizeof(", ");
        total += print_get_item_size(a.data[i]);
    }
    total += sizeof('}');
    return total;
}
bool is_prime(u64 x);
u64 next_prime(u64 x);
template <class Key, class Value> struct map {
    struct slot {
        bool occupied = false;
        u64 hash = 0;
        Key key = {};
        Value value = {};
    };
    allocator *alloc = ng::default_allocator;
    slot *slots = nullptr;
    u64 capacity = 0;
    s64 count = 0;
    map copy() {
        map result;
        result.alloc = alloc;
        if (capacity > 0) {
            result.allocate(capacity);
            if (result.slots && slots) ng::memcpy(result.slots, slots, capacity * sizeof(slot));
        }
        result.count = count;
        return result;
    }
    bool allocate(u64 new_capacity) {
        if (!alloc) alloc = ng::default_allocator;
        if (alloc) slots = ng_cast(slot *) alloc->allocate(new_capacity * sizeof(slot));
        if (slots) capacity = new_capacity;
        if (!slots) return false;
            return true;
        }
    void expand() {
        auto old_slots = slots;
        auto old_cap = capacity;
        auto new_cap = next_prime(capacity + 1);
        allocate(new_cap);
        count = 0;
        for (u64 i = 0; i < capacity; i += 1) slots[i].occupied = false;
        if (old_slots) {
            for (u64 i = 0; i < old_cap; i += 1) {
                auto slot = old_slots[i];
                if (slot.occupied) insert(slot.key, slot.value);
            }
            if (alloc) alloc->free(old_slots, old_cap * sizeof(slot));
        } else { 
            for (u64 i = 0; i < capacity; i += 1) {
                slots[i].key = {};
                slots[i].value = {};
            }
        }
    }
    void release() { if (slots && alloc) alloc->free(slots, capacity * sizeof(slot)); }
    Value *insert(Key key, Value value) {
        auto hash = get_hash(key);
        auto index = find_index(hash, key);
        if (index == -1) {
            if (ng_cast(u64) count >= capacity) expand();
            if (!slots) return nullptr;
            index = hash % capacity;
            while (slots[index].occupied) { // linear probe
                index += 1;
                if (ng_cast(u64) index >= capacity) index = 0;
            }
            count += 1;
        }
        auto slot = &slots[index];
        slot->occupied = true;
        slot->hash = hash;
        slot->key = key;
        slot->value = value;
        return &slot->value;
    }
    void remove(Key key) {
        auto hash = get_hash(key);
        auto index = find_index(hash, key);
        if (index != -1) slots[index].occupied = false;
    }
    s64 find_index(u64 hash, Key key) {
        if (count <= 0) return -1;
        auto slot = hash % capacity;
        auto index = slot;
        while (slots[index].occupied) {
            if (slots[index].hash == hash && slots[index].key == key) return index;
            // linear probe
            index += 1;
            if (index >= capacity) index = 0;
            if (index == slot) return -1; // Looped; all slots are full.
        }
        return -1;
    }
    struct find_result {
        bool found = false;
        Value *value = nullptr;
    };
    find_result operator[](Key key) {
        find_result result = {};
        auto hash = get_hash(key);
        auto index = find_index(hash, key);
        if (index != -1) {
            result.value = &slots[index].value;
            result.found = true;
        }
        return result;
    }
    struct slot_iterator {
        slot *ptr;
        slot *end;
        void operator++() {
            if (!ptr) return;
            do {
                ptr += 1;
            } while (ptr < end && !ptr->occupied);
        }
        bool operator!=(const slot_iterator &) { return ptr < end; }
        slot &operator*() { return *ptr; }
    };
    slot_iterator begin() {
        auto result = slot_iterator{slots, slots + capacity};
        if (result.ptr && !result.ptr->occupied) ++result;
        return result;
    }
    slot_iterator end() { return {}; } // slot *end() { return nullptr; }
};
u64 get_hash(string key);   
u64 get_hash(u64 key);
u64 get_hash(u32 key);
inline u64 get_hash(s64 key) { return get_hash(ng_cast(u64) key); }
inline u64 get_hash(s32 key) { return get_hash(ng_cast(u32) key); }
inline u64 get_hash(f64 key) { return get_hash(ng_bitcast(u64) key); }
inline u64 get_hash(f32 key) { return get_hash(ng_bitcast(u32) key); }
u64 get_hash(bool key); // Intentionally unimplemented. Why the hell are you hashing booleans?
u64 get_hash(u8 key);   // Intentionally unimplemented.
u64 get_hash(u16 key);  // Intentionally unimplemented.
} // namespace ng
inline bool operator==(ng::string lhs, ng::string rhs) { return ng::string_compare(lhs, rhs) == 0; }
inline bool operator!=(ng::string lhs, ng::string rhs) { return ng::string_compare(lhs, rhs) != 0; }
inline bool operator<=(ng::string lhs, ng::string rhs) { return ng::string_compare(lhs, rhs) <= 0; }
inline bool operator>=(ng::string lhs, ng::string rhs) { return ng::string_compare(lhs, rhs) >= 0; }
inline bool operator< (ng::string lhs, ng::string rhs) { return ng::string_compare(lhs, rhs) < 0; }
inline bool operator> (ng::string lhs, ng::string rhs) { return ng::string_compare(lhs, rhs) > 0; }
inline bool operator==(ng::string lhs, const char *rhs) { return lhs == ng::string::c_str(rhs); }
inline bool operator!=(ng::string lhs, const char *rhs) { return lhs != ng::string::c_str(rhs); }
inline bool operator==(const char *lhs, ng::string rhs) { return ng::string::c_str(lhs) == rhs; }
inline bool operator!=(const char *lhs, ng::string rhs) { return ng::string::c_str(lhs) != rhs; }
#ifndef NG_NO_PLACEMENT_NEW_INLINE
#ifdef _WIN32
#ifndef __PLACEMENT_NEW_INLINE
#define __PLACEMENT_NEW_INLINE
#define __PLACEMENT_VEC_NEW_INLINE
inline void *operator new(ng::usize, void *location) { return location; }
inline void operator delete(void *, void *) {}
inline void *operator new[](ng::usize, void *) = delete;
inline void operator delete[](void *, void *) = delete;
#endif // __PLACEMENT_VEC_NEW_INLINE
#else
inline void *operator new(ng::usize, void *location) { return location; }
inline void operator delete(void *, void *) {}
inline void *operator new[](ng::usize, void *) = delete;
inline void operator delete[](void *, void *) = delete;
#endif // platform
#endif // NG_NO_PLACEMENT_NEW_INLINE
#ifdef NG_DEFINE
#if defined(_MSC_VER)
#define NG_CDECL __cdecl
#define NG_RESTRICT __declspec(restrict)
#define NG_NORETURN __declspec(noreturn)
#if !defined(_KERNEL32_) && (defined(_M_IX86) || defined(_M_IA64) || defined(_M_AMD64) || defined(_M_ARM)) && !defined(MIDL_PASS)
#define NG_DECLSPEC __declspec(dllimport)
#else
#define NG_DECLSPEC
#endif
// #elif defined(__GNUC__) || defined(__clang__)
#else                
#define NG_CDECL
#define NG_RESTRICT
#define NG_NORETURN
#define NG_DECLSPEC
#endif // platform
#ifdef NG_NO_CRT
#ifdef _WIN32
#pragma comment(lib, "kernel32.lib")
// #define WIN32_LEAN_AND_MEAN
// #define NOMINMAX
// #include <windows.h>
extern "C" {
struct ng_FILETIME { // @Volatile: must match _FILETIME
    ng::u32 dwLowDateTime, dwHighDateTime; // DWORD = unsigned long
};
union ng_LARGE_INTEGER { // @Volatile: must match _LARGE_INTEGER
    struct {
        ng::u32 LowPart;
        ng::s32 HighPart;
    } u;
    ng::s64 QuadPart;
};
#if defined(_WIN64)
typedef unsigned __int64 ng_SIZE_T;
#else
#if !defined(__midl) && (defined(_X86_) || defined(_M_IX86) || defined(_ARM_) || defined(_M_ARM)) && _MSC_VER >= 1300
typedef __w64 unsigned long ng_SIZE_T;
#else
typedef unsigned long ng_SIZE_T;
#endif
#endif
#ifndef _MAC
typedef __wchar_t ng_WCHAR;
#else
typedef unsigned short ng_WCHAR;
#endif
NG_DECLSPEC void *__stdcall HeapAlloc(void *hHeap, unsigned long dwFlags, ng_SIZE_T dwBytes);
NG_DECLSPEC void *__stdcall HeapReAlloc(void *hHeap, unsigned long dwFlags, void *lpMem,
                                        ng_SIZE_T dwBytes);
NG_DECLSPEC int __stdcall HeapFree(void *hHeap, unsigned long dwFlags, void *lpMem);
NG_DECLSPEC void *__stdcall GetProcessHeap();
NG_DECLSPEC NG_NORETURN void __stdcall ExitProcess(unsigned int uExitCode);
NG_DECLSPEC int __stdcall WriteFile(void *hFile, const void *lpBuffer,
                                    unsigned long nNumberOfBytesToWrite, unsigned long *lpNumberOfBytesWritten,
                                    struct _OVERLAPPED *lpOverlapped);
NG_DECLSPEC void *__stdcall GetStdHandle(unsigned long nStdHandle);
NG_DECLSPEC void __stdcall GetSystemTimeAsFileTime(struct _FILETIME *lpSystemTimeAsFileTime);
NG_DECLSPEC int __stdcall CreateDirectoryW(const ng_WCHAR *lpPathName,
                                           struct _SECURITY_ATTRIBUTES *lpSecurityAttributes);
NG_DECLSPEC int __stdcall MultiByteToWideChar(unsigned int CodePage, unsigned long dwFlags,
                                              const char *lpMultiByteStr, int cbMultiByte,
                                              ng_WCHAR *lpWideCharStr, int cchWideChar);
NG_DECLSPEC int __stdcall WideCharToMultiByte(unsigned int CodePage, unsigned long dwFlags,
                                              ng_WCHAR *lpWideCharStr, int cchWideChar,
                                              const char *lpMultiByteStr, int cbMultiByte,
                                              const char *lpDefaultChar, int *lpUsedDefaultChar);
NG_DECLSPEC unsigned long __stdcall GetLastError();
}
namespace ng {
#ifndef NG_NO_CRT
// The compiler will generate code to run this function with the other static
// initializers for global variables. This function cannot be inlined, so the
// variable will have the default BSS value, 0, until the CRT runs the static
// initializers. If there is no CRT (like when compiling with cl.exe
// -nodefaultlib) then it will stay 0.
static bool run_static_init() {
    volatile bool result = true;
    return result;
}
bool has_static_init = run_static_init();
#else
bool has_static_init = false;
#endif // !NG_NO_CRT
void exit(int code) { ExitProcess(code); }
static void *raw_malloc(usize size) { return HeapAlloc(GetProcessHeap(), 0, size); }
void *raw_realloc(void *ptr, usize new_size) {
    if (!ptr) return HeapAlloc(GetProcessHeap(), 0, new_size);
    if (new_size) return HeapReAlloc(GetProcessHeap(), 0, ptr, new_size);
    HeapFree(GetProcessHeap(), 0, ng_cast(ng_SIZE_T *) ptr);
    return nullptr;
}
void raw_free(void *ptr) { HeapFree(GetProcessHeap(), 0, ng_cast(ng_SIZE_T *) ptr); }
void output_print_buffer(print_buffer buffer) {
    unsigned long bytes_written;
    unsigned long size_to_write = ng_cast(u32)(ng_min(buffer.len, buffer.cap));
    WriteFile(GetStdHandle(ng_cast(u32)(-11)), buffer.str, size_to_write, &bytes_written, nullptr);
}
s64 get_unix_timestamp() { // @Attribution: https://stackoverflow.com/a/46024468/3185819      
   // Note, author says "Code released into public domain; no attribution required."
   constexpr s64 UNIX_TIME_START = 11644473600; // seconds between 1601-01-01 and 1970-01-01
   constexpr s64 TICKS_PER_SECOND = 10000000;
   ng_FILETIME ft = {};
   GetSystemTimeAsFileTime(ng_cast(struct _FILETIME *)(&ft));
   // Copy the parts into a LARGE_INTEGER to access all 64-bits without an alignment fault
   ng_LARGE_INTEGER li = {};
   li.u.LowPart  = ft.dwLowDateTime;
   li.u.HighPart = ft.dwHighDateTime;
   return (li.QuadPart - (UNIX_TIME_START * TICKS_PER_SECOND)) / TICKS_PER_SECOND;
}
ng::array<u16> utf8_to_utf16(ng::string utf8, ng::allocator *alloc = ng::default_allocator) {
    ng::array<u16> utf16 = {};
    utf16.alloc = alloc;
    if (!utf8) return utf16;

    // CP_UTF8 == 65001
    auto size_needed = MultiByteToWideChar(65001, 0, ng_cast(const char *) utf8.ptr, ng_cast(int) utf8.len, nullptr, 0);
    if (!size_needed) return utf16;

    if (!utf16.reserve(size_needed + 1)) return utf16;
    
    ng_assert(utf16.capacity > size_needed);
    utf16.data[size_needed] = 0;

    utf16.count = MultiByteToWideChar(65001, 0, ng_cast(const char *) utf8.ptr, ng_cast(int) utf8.len, ng_cast(__wchar_t *) utf16.data, size_needed)
        + 1;
    return utf16;
}
ng::string utf16_to_utf8(u16 *utf16, u64 utf16_len, ng::allocator *alloc = ng::default_allocator) {
    ng::string utf8 = {};
    
    if (!utf16_len) return utf8;

    // CP_UTF8 == 65001
    auto size_needed = WideCharToMultiByte(65001, 0, ng_cast(__wchar_t *) utf16, ng_cast(int) utf16_len, nullptr, 0, nullptr, nullptr);
    if (!size_needed) return utf8;

    utf8.ptr = ng_cast(u8 *) alloc->allocate(size_needed + 1);
    if (!utf8.ptr) return utf8;
    utf8.len = size_needed + 1;
    
    utf8[size_needed] = 0;

    WideCharToMultiByte(65001, 0, ng_cast(__wchar_t *) utf16, ng_cast(int) utf16_len, ng_cast(const char *) utf8.ptr, utf8.len, nullptr, nullptr);
    return utf8;
}
create_directory_result create_directory(string path) {
    if (!path) return create_directory_result::input_invalid;
    if (path.len >= 32767) return create_directory_result::path_too_long;

    auto utf16 = utf8_to_utf16(path);
    ng_defer { utf16.release(); };
    if (!utf16.data) return create_directory_result::input_invalid;

    if (utf16.count < 0 || ng_cast(u64) utf16.count < utf16.capacity) return create_directory_result::input_invalid;

    auto result = CreateDirectoryW(ng_cast(__wchar_t *) utf16.data, nullptr);
    if (!result) {
        auto error = GetLastError();
        // ERROR_ALREADY_EXISTS == 183L, ERROR_PATH_NOT_FOUND == 3L
        if (error == 183L) return create_directory_result::already_exists;
        if (error == 3L) return create_directory_result::path_not_found;
        return create_directory_result::input_invalid; // @Inexplicable
    }
    return create_directory_result::success;
}
} // namespace ng
#else
#error This platform is not yet supported without the C Runtime.
#endif // platform
namespace ng {
void *malloc(usize size) { return raw_malloc(size); }
void *realloc(void *ptr, usize size) { return raw_realloc(ptr, size); }
void free(void *ptr) { raw_free(ptr); }
void *memmove(void *destination, const void *source, usize size) {
    if (source == destination) return destination;
    auto d = ng_cast(u8 *) destination, s = ng_cast(u8 *) source;
    if (s < d) { // copy from back
        s += size - 1;
        d += size - 1;
        while (size--) *d-- = *s--;
        return destination;
    }
    return ng::memcpy(destination, source, size);
}
#ifdef NG_MS_INTRINSICS
#elif defined(__GNUC__) || defined(__clang__)
#else
// https://en.wikipedia.org/wiki/Fast_inverse_square_root
f64 rsqrt(f64 x) { // Not fast, just portable.
    auto half_x = x / 2;
    auto i = ng_bitcast(s64)(x);
    i = 0x5fe6eb50c7b537a9 - (i >> 1);
    x = ng_bitcast(f64)(i);
    x *= 1.5 - half_x * x * x;
    x *= 1.5 - half_x * x * x;
    return x;
}
#endif
static f64 pow_whole(f64 b, f64 e) {
    while (e > max_frac_f64 * 2) {
        b *= b;
        e /= 2;
    }
    f64 result = 1;
    auto i = ng_cast(u64) e;
    do {
        if (i & 1) result *= b;
        b *= b;
    } while (i >>= 1);
    return result;
}
// e in (0, 1) can be approximated as 1/a + 1/b + 1/c... to arbitrary precision.
// So b^e = b^(1/a) * b^(1/b) * b^(1/c)... which we can easily calculate through successive runs
// of the nth-root algorithm (Kendall E Atkinson, An Introduction to Numerical Analysis).
static f64 pow_inner(f64 b, f64 e) {
    if (e == 2.0) return b * b; else if (e == 0.5) return ng::sqrt(b);
    auto result = pow_whole(b, e);
    e -= ng::floor(e);
    if (e == 0.0) return result;
    if (b < 0) return nan;
    f64 exp = e; // e is now in (0, 1)
#if 0
    while (exp / e > 1.0e-4) {
        f64 n = ng::ceil(1 / exp), root = 1, delta = 0;
        do {
            delta = (b / pow_whole(root, n - 1) - root) / n;
            root += delta;
        } while (ng::abs(delta) / root > 1.0e-4); // @Ugly @Hack @Speed. @Todo: infinite loop?
        result *= root;
        exp -= 1 / n;
    }
#else
    for (int i = 0; i < 4 && exp; i += 1) { // @Speed
        f64 n = ng::ceil(1 / exp);
        f64 root = 1;
        f64 delta = 0;
        for (int j = 0; j < 20; j += 1) {
            delta = (b / pow_whole(root, n - 1) - root) / n;
            root += delta;
        }
        result *= root;
        exp -= 1 / n;
    }
#endif
    return result;
}
// inf/nan/huge == 1 << 63, i.e. even.
static bool is_odd(f64 x) { return !ng::frac(x) && (ng_cast(s64)(x) & 1); }
f64 pow(f64 b, f64 e) {
    if (b == 1.0 || e == 0.0) return 1.0; else if (ng::isnan(b) || ng::isnan(e)) return nan;
    if (b == 0.0) return e > 0.0 ? !is_odd(e) * b : is_odd(e) && ng::sign(b) < 0.0 ? -inf : inf;
    if (ng::isinf(e)) return b == -1.0 ? 1.0 : (e > 0.0 == ng::abs(b) > 1.0) ? inf : 0.0;
    if (ng::isinf(b)) return (b < 0.0 && is_odd(e) ? -1.0 : 1.0) / (e > 0.0 ? 0.0 : inf);
    return e < 0.0 ? 1.0 / pow_inner(b, -e) : pow_inner(b, e);
}
constexpr f64 sinQ = 0.775991846456; // Max err: .092% abs, 1.20% rel
// constexpr f64 sinQ = .7824918; // Max err: .216% abs, .370% rel
static f64 sin_poly(f64 x) { return 8. / ng::TAU * x + -16. / ng::TAU / ng::TAU * x * ng::abs(x); }
static f64 sin_poly_corrector(f64 y) { return sinQ * y + (1 - sinQ) * y * ng::abs(y); }
static f64 mod_round(f64 x, f64 y) { return x - ng::round(x / y) * y; } // @Todo: Expand this out.
f64 sin(f64 x) {
    if (x == 0) return x; // IEEE says sin(-0) = -0.
    x = mod_round(x, ng::TAU);
    // Uncomment this line to improve correctness:
    // ng_clamp(x, -ng::TAU / 2, ng::TAU / 2); // Range hack for mega huge floats.
    x = sin_poly_corrector(sin_poly(x));
    return x;
}
f64 cos(f64 x) {                                                                
    x = mod_round(x, ng::TAU);
    // Uncomment this line to improve correctness:
    // ng_clamp(x, -ng::TAU / 2, ng::TAU / 2); // Range hack for mega huge floats.
    x += ng::TAU / 4 - (x > ng::TAU / 4) * ng::TAU;
    x = sin_poly_corrector(sin_poly(x));
    return x;
}
f64 tan(f64 x) { return ng::sin(x) / ng::cos(x); } // @Temporary
// constexpr f64 arcA = -0.0187293, arcB = 0.0742610, arcC = 0.2121144; // err=.0126% abs,.2487% rel
constexpr f64 arcA = -0.028594, arcB = 0.0848106, arcC = -0.2146018363; // err=.0275% abs,.0227% rel
static f64 arc_cub(f64 x) { return (((arcA * x) + arcB) * x + arcC) * x + ng::TAU / 4; }
f64 asin(f64 x) {
    if (x == 0) return x; // IEEE says asin(-0) = -0.
    auto negate = ng_cast(f64)(x < 0);
    x = ng::abs(x);
    auto result = arc_cub(x) * ng::sqrt(1 - x);
    result = ng::TAU / 4 - result;
    return result - 2 * negate * result;
}
f64 acos(f64 x) {
    auto negate = ng_cast(f64)(x < 0);
    x = ng::abs(x);
    auto result = arc_cub(x) * ng::sqrt(1 - x);
    result -= 2 * negate * result;
    return result + negate * ng::TAU / 2;
}
f64 atan(f64 x) {
    if (x == 0) return x; // IEEE says atan(-0) = -0.
    if (x < 0) return -atan(-x);
    if (x > 1) return ng::TAU / 4 - atan(1 / x);
    constexpr f64 k = 0.273081;
    return x * (x * -k + (k + ng::TAU / 8));
}
f64 atan2(f64 y, f64 x) { // @Todo: unit test IEEE constraints
    if (ng::isnan(y) || ng::isnan(x)) return ng::nan;
    if (y == 0) return ng::sign(x) < 0 ? ng::sign(y) * (ng::TAU / 2) : y;
    if (x == 0) return ng::sign(y) * (ng::TAU / 4);
    if (ng::isinf(y)) return ng::sign(y) * (ng::isinf(x) ? (ng::TAU / 8) * (2 - ng::sign(x)) : ng::TAU / 4);
    if (ng::isinf(x)) return ng::sign(y) * (x < 0 ? ng::TAU / 2 : 0);
    return atan(y / x);
}
} // namespace ng
#else
extern "C" {
NG_DECLSPEC NG_NORETURN void NG_CDECL exit(int);
NG_DECLSPEC NG_RESTRICT void *NG_CDECL malloc(ng::usize);
NG_DECLSPEC NG_RESTRICT void *NG_CDECL realloc(void *, ng::usize);
NG_DECLSPEC void NG_CDECL free(void *);
NG_DECLSPEC void *NG_CDECL memset(void *, int, ng::usize);
NG_DECLSPEC void *NG_CDECL memcpy(void *, void const *, ng::usize);
NG_DECLSPEC void *NG_CDECL memmove(void *, void const *, ng::usize);
NG_DECLSPEC int NG_CDECL memcmp(const void *, const void *, ng::usize);
NG_DECLSPEC ng::usize NG_CDECL strlen(const char *);
NG_DECLSPEC int NG_CDECL strcmp(const char *, const char *);
NG_DECLSPEC int NG_CDECL strncmp(const char *, const char *, ng::usize);
double NG_CDECL fabs(double);
float NG_CDECL fabsf(float);
double NG_CDECL fmod(double, double);
NG_DECLSPEC float NG_CDECL fmodf(float, float);
NG_DECLSPEC double NG_CDECL floor(double);
NG_DECLSPEC float NG_CDECL floorf(float);
NG_DECLSPEC double NG_CDECL ceil(double);
NG_DECLSPEC float NG_CDECL ceilf(float);
NG_DECLSPEC double NG_CDECL round(double);
NG_DECLSPEC float NG_CDECL roundf(float);
double NG_CDECL sqrt(double);
NG_DECLSPEC float NG_CDECL sqrtf(float);
double NG_CDECL pow(double, double);
NG_DECLSPEC float NG_CDECL powf(float, float);
double NG_CDECL sin(double);
NG_DECLSPEC float NG_CDECL sinf(float);
double NG_CDECL cos(double);
NG_DECLSPEC float NG_CDECL cosf(float);
double NG_CDECL tan(double);
NG_DECLSPEC float NG_CDECL tanf(float);    
double NG_CDECL asin(double);
NG_DECLSPEC float NG_CDECL asinf(float);
double NG_CDECL acos(double);
NG_DECLSPEC float NG_CDECL acosf(float);
double NG_CDECL atan(double);
NG_DECLSPEC float NG_CDECL atanf(float);
double NG_CDECL atan2(double, double);
NG_DECLSPEC float NG_CDECL atan2f(float, float);
int NG_CDECL printf(const char *, ...);
}
namespace ng {
void exit(int code) { ::exit(code); }
void *malloc(usize size) { return ::malloc(size); }
void *realloc(void *ptr, usize new_size) { return ::realloc(ptr, new_size); }
void free(void *ptr) { ::free(ptr); }
void *memset(void *destination, u8 value, usize size) {
    return ::memset(destination, ng_cast(int) value, size);
}
void *memcpy(void *destination, const void *source, usize size) {
    return ::memcpy(destination, source, size);
}
void *memmove(void *destination, const void *source, usize size) {
    return ::memmove(destination, source, size);
}
int memcmp(const void *a, const void *b, usize n) { return ::memcmp(a, b, n); }
usize strlen(const char *s) { return ::strlen(s); }
int strcmp(const char *a, const char *b) { return ::strcmp(a, b); }
int strncmp(const char *a, const char *b, usize n) { return ::strncmp(a, b, n); }
f32 abs(f32 x) { return ::fabsf(x); }
f64 abs(f64 x) { return ::fabs(x); }
// f32 round(f32 x) { return ::roundf(x); }
// f64 round(f64 x) { return ::round(x); }
f32 floor(f32 x) { return ::floorf(x); }
f64 floor(f64 x) { return ::floor(x); }
f32 ceil(f32 x) { return ::ceilf(x); }
f64 ceil(f64 x) { return ::ceil(x); }
f32 mod(f32 x, f32 y) { return ::fmodf(x, y); }
f64 mod(f64 x, f64 y) { return ::fmod(x, y); }
f32 sqrt(f32 x) { return ::sqrtf(x); }
f64 sqrt(f64 x) { return ::sqrt(x); }
f32 pow(f32 base, f32 exp) { return ::powf(base, exp); }
f64 pow(f64 base, f64 exp) { return ::pow(base, exp); }
f32 sin(f32 x) { return ::sinf(x); }
f64 sin(f64 x) { return ::sin(x); }
f32 cos(f32 x) { return ::cosf(x); }
f64 cos(f64 x) { return ::cos(x); }
f32 tan(f32 x) { return ::tanf(x); }
f64 tan(f64 x) { return ::tan(x); }                    
f32 asin(f32 x) { return ::asinf(x); }
f64 asin(f64 x) { return ::asin(x); }
f32 acos(f32 x) { return ::acosf(x); }
f64 acos(f64 x) { return ::acos(x); }
f32 atan(f32 x) { return ::atanf(x); }
f64 atan(f64 x) { return ::atan(x); }
f32 atan2(f32 y, f32 x) { return ::atan2f(y, x); }
f64 atan2(f64 y, f64 x) { return ::atan2(y, x); }
void output_print_buffer_default(ng::print_buffer buffer, void *userdata) {
    ::printf("%.*s", (int)(ng_min(buffer.len, buffer.cap)), buffer.str);
    (void)userdata;
}
} // namespace ng
#endif // NG_NO_CRT
namespace ng {
// print functions
void print_buffer::putchar(u32 c) {
    if (len + 1 > cap) {
        if (NG_ASSERT_ENABLED) ng_break(); // Can't call assert because it uses print
        return;
    }
    str[len] = c; // @Unicode: this truncates to u8
    len += 1;
}
void print_buffer::put_u8s(u8 *u8s, u64 count) {
    if (len + count > cap) {
        if (NG_ASSERT_ENABLED) ng_break();
        return;
    }
    ng::memcpy(str + len, u8s, ng_cast(ng::usize) count);
    len += count;
}
void print_buffer::fill_u8(u8 byte, u64 count) {
    if (len + count > cap) {
        if (NG_ASSERT_ENABLED) ng_break();
        return;
    }
    ng::memset(str + len, byte, ng_cast(ng::usize) count);
    len += count;
}
void output_one_character(u32 c) {
    auto c_u8 = static_cast<u8>(c); // @Unicode
    ng::output_print_buffer(ng::print_buffer{&c_u8, 1, 1});
}
fmt_int::fmt_int(s64 x, bool is_signed, int radix, int leading_zeroes, int leading_spaces)
    : x{x},
    is_signed{is_signed},
    leading_zeroes{leading_zeroes},
    leading_spaces{leading_spaces},
    radix{radix} {}
u64 print_get_item_size(const fmt_int &fmt) {
    if (fmt.radix < 2 || fmt.radix > 64) return 0;
    auto max_extras_size = fmt.leading_spaces + fmt.leading_zeroes;
    if (fmt.radix > 10) return 20 + max_extras_size;
    // floor(log_b(2^64)) for b = 2 to 10; +1 for sign
    constexpr int log_table[] = {65, 41, 33, 28, 25, 23, 22, 21, 20};
    return log_table[fmt.radix - 2] + max_extras_size;
}
void print_item(print_buffer *buf, const fmt_int &fmt) {
    if (fmt.radix < 2 || fmt.radix > 64) return;
    auto x = fmt.x;
    bool negative = x < 0 && fmt.is_signed;
    int leading_spaces = fmt.leading_spaces;
    if (negative) {
        x = -x;
        leading_spaces -= 1;
    }
    auto ux = ng_cast(u64) x;
    u8 num_buffer[64];
    const auto end = &num_buffer[ng::countof(num_buffer)];
    auto p = end - 1;
    bool has_printed_nonzeroes = false;
    do {
        auto digit = ux % fmt.radix;
        if (digit > 0 || has_printed_nonzeroes || p == end - 1) {
                 if (digit < 10)  *p = digit + '0';
            else if (digit < 36)  *p = digit + 'a' - 10;
            else if (digit < 62)  *p = digit + 'A' - 36;
            else if (digit == 63) *p = '_';
            else                  *p = '$';
            has_printed_nonzeroes = true;
            p -= 1;
        }
    } while (ux /= fmt.radix);
    p += 1;
    u64 len = end - p;
    for (int i = leading_spaces - fmt.leading_zeroes - len; i > 0; i -= 1) buf->putchar(' ');
    if (negative) buf->putchar('-');
    for (int i = fmt.leading_zeroes - len; i > 0; i -= 1) buf->putchar('0');
    buf->put_u8s(p, len);
}
// yields upper bound
static s64 log10_approx(f64 x) {
    s64 exp = (ng_bitcast(u64)(x) >> 52 & 0x7ff) - 1023;
    if (exp < 0) return exp *  617 / 2048;
    else         return exp * 1233 / 4096 + 1;
}
// clang-format off
static f64 pow10(s64 x) {
#define NG_0(x) x##0, x##1, x##2, x##3, x##4, x##5, x##6, x##7, x##8, x##9
#define NG_1(x) NG_0(x##0), NG_0(x##1), NG_0(x##2), NG_0(x##3), NG_0(x##4), NG_0(x##5), NG_0(x##6), NG_0(x##7), NG_0(x##8), NG_0(x##9)
    static const constexpr f64 plut[] = {NG_1(1e0), NG_1(1e1), NG_1(1e2), 1e300, 1e301, 1e302,
                                         1e303,     1e304,     1e305,     1e306, 1e307, 1e308};
    static const constexpr f64 nlut[] = {NG_1(1e-0), NG_1(1e-1), NG_1(1e-2), 1e-300, 1e-301, 1e-302,
                                         1e-303,     1e-304,     1e-305,     1e-306, 1e-307, 1e-308};
#undef NG_1
#undef NG_0
    if (x > 308) return ng::inf;
    if (x < -308) return 0;
    return x < 0 ? nlut[-x] : plut[x]; // return ng::pow(10.0, ng_cast(f64) x);
}
// clang-format on
void print_float(print_buffer *buf, f64 x, int precision) {
    if (ng_bitcast(u64)(x) & 0x8000000000000000) { // slow vs < 0, but finds -nan and -0
        buf->putchar('-');
        x = -x;
    }
    if (x == 0.0) {
        buf->putchar('0');
        return;
    }
    if ((ng_bitcast(u64)(x) >> 52 & 0x7ff) == 0) {
        buf->put_literal("(sub)");
        return;
    }
    if (ng::isinf(x)) return buf->put_literal("inf");
    if (ng::isnan(x)) return buf->put_literal("nan");

    // print large numbers
    auto log10_x = log10_approx(x);

    if (log10_x > 18) { // bigger than a u64
        auto msd = log10_x;
        auto lsd = ng_max(msd - precision, 0);
        auto first_digit = ng_cast(u64)(x * ng::pow10(-msd));
        if (first_digit) buf->putchar(first_digit + '0');
        for (auto i = msd - 1; i >= lsd; i -= 1) {
            auto divisor = ng::pow10(-i);
            auto digit = ng_cast(u64)(x * divisor) % 10;
            buf->putchar(digit + '0');
        }
        buf->fill_u8('0', lsd);
        return;
    }

    // print small numbers
    auto whole_part = ng_cast(u64) x;
    u64 divisor = 10000000000000000000ull;
    bool has_printed_nonzeroes = false;
    do {
        auto digit = whole_part / divisor;
        if (has_printed_nonzeroes || digit > 0 || divisor == 1) {
            has_printed_nonzeroes = true;
            buf->putchar(digit % 10 + '0');
        }
    } while (divisor /= 10);
    
    if (x <= whole_part) return;

    // print fraction

    buf->putchar('.');

    auto digits_to_skip = -log10_x;
    if (digits_to_skip > 1) buf->fill_u8('0', digits_to_skip - 1);

    f64 x_skipped = x * 10; // @ Why multiply by 10? Magic??

    if (digits_to_skip > 0) x_skipped = x * ng::pow10(digits_to_skip);

    auto min_difference = ng::pow10(-precision / 2 - 1);
    for (u64 i = 0; i < precision; i += 1) {
        f64 scaled = x_skipped * ng::pow10(i);
        auto digit = ng_cast(u64) scaled;
        buf->putchar(digit % 10 + '0');
        if (scaled - digit < min_difference) break;
    }
}
void print_item(print_buffer *buf, f32 f) { print_float(buf, ng_cast(f64) f, 9); }
u64 print_get_item_size(f32 f) { return print_get_item_size(ng_cast(f64) f); }
void print_item(print_buffer *buf, f64 x) { print_float(buf, x, 22); }
u64 print_get_item_size(f64 x) {
    if (x == 0) return 2; // +1 for minus sign if x == -0
    if ((ng_bitcast(u64)(x) >> 52 & 0x7ff) == 0) return 5; // "(sub)"
    if (ng::isfinite(x)) return (ng::abs(log10_approx(x)) + 2) + 24; // lots of slack
    return 5; // "inf"/"nan"/"-inf"/"-nan"
}
u64 print_get_item_size(const fmt_char &) { return 4; }
void print_item(print_buffer *buf, const fmt_char &fmt) { buf->putchar(fmt.c); }
void print_item(print_buffer *buf, print_buffer src) {
    // If len > cap, this buffer is invalid, but we can't complain...
    buf->put_u8s(src.str, ng_min(src.len, src.cap));
}
u64 print_get_item_size(print_buffer src) { return ng_min(src.len, src.cap); }
void print_item(print_buffer *buf, bool b) { print_item(buf, b ? "true" : "false"); }
u64 print_get_item_size(bool) { return 5; }
#define ng_print_item_int_size(n)                                                                  \
    void print_item(print_buffer *buf, s##n i) { print_item(buf, fmt_int(i)); }                    \
    void print_item(print_buffer *buf, u##n u) { print_item(buf, fmt_int(u, false)); }             \
    u64 print_get_item_size(u##n) { return (n / 3 + 1); }     /*  n/3+1 ~= ceil(n*log_10(2))    */ \
    u64 print_get_item_size(s##n) { return (n / 3 + 1) + 1; } /*        add 1 for sign          */
ng_print_item_int_size(8) ng_print_item_int_size(16);
ng_print_item_int_size(32) ng_print_item_int_size(64);
void print_item(print_buffer *buf, void *ptr) {
    print_item(buf, fmt_int(ng_cast(sptr) ptr, false, 16, sizeof(void *) * 2));
}
u64 print_get_item_size(void *) { return sizeof(void *) * 2; }
void print_item(print_buffer *buf, const char *ptr) {
    if (ptr) buf->put_u8s(ng_cast(u8 *) ptr, ng::strlen(ptr));
}
u64 print_get_item_size(const char *ptr) {
    if (ptr) return ng::strlen(ptr);
    return 0;
}
assert_failed_proc *assert_failed;
void *assert_failed_userdata;
static bool assert_failed_default(const char *file, int line, const char *func, const char *e,
                                  bool *ignore, string message, void *userdata) {
    ng::print("Assert fired at %:%(%): '%'\n\t%\n"_s, file, line, func, e, message);
    (void)(ignore, userdata);
    return 0;
}
void assert_init() { assert_failed = &assert_failed_default; }
// allocator construct
static char default_allocator_buffer[sizeof(mallocator)];
allocator *default_allocator;
void allocator_init() {
    auto default_allocator_ptr = ng_cast(mallocator *) default_allocator_buffer;
    *default_allocator_ptr = {};
    default_allocator = default_allocator_ptr;
}
void *mallocator_proc(allocate_mode, allocator *, u64 size, u64, void *old_block, s64) {
    return ng::realloc(old_block, size);
}
// @Todo @Incomplete
// void *pool_allocator_proc(allocate_mode mode, allocator *self, u64 size, u64 old_size, void *old_block,
//                             s64 options) {
//     auto me = ng_cast(pool_allocator *) self;
// 
// }
void *proxy_allocator_proc(allocate_mode mode, allocator *self, u64 size, u64 old_size, void *old_block,
                            s64 options) {
    auto me = ng_cast(proxy_allocator *) self;
    auto result = me->parent->proc(mode, me->parent, size, old_size, old_block, options);
    switch (mode) { // @Audit
    default: {
        if (result) {
            if (!old_block) { // is a new allocation
                ng_assert(old_size == 0);
                me->allocations += 1;
                me->allocated_bytes += size;
            } else {
                me->allocated_bytes -= old_size;
                me->allocated_bytes += size;
            }
        } else {
            if (size == 0 && old_block) {
                ng_assert(me->allocations > 0);
                me->allocations -= 1; // is a free
                ng_assert(me->allocated_bytes >= old_size);
                me->allocated_bytes -= old_size;
            } else {
                // failure
            }
        }
    } break;
    case (allocate_mode::free_all): {
        me->allocations = 0;
        me->allocated_bytes = 0;
    } break;
    }
    return result;
}
proxy_allocator create_proxy_allocator(const char *name, allocator *parent) {
    proxy_allocator result = {};
    result.parent = parent;
    result.name = name;
    return result;
}
void destroy_proxy_allocator(proxy_allocator *proxy) {
    ng_assert(proxy->allocations == 0 && proxy->allocated_bytes == 0,
              "Allocator '%' leaked % bytes over % allocations.\n", proxy->name,
              proxy->allocated_bytes, proxy->allocations);
}
// string functions
bool str_to_s64(string *str, s64 *result) { // @Attribution: Per Vognsen
    *result = 0;
    if (!str || !*str) return false;
    const s64 max = ng_cast(s64) ((1ull << 63) - 1);
    const s64 min = 1ll << 63;
    bool negative = false;
    bool is_number = false;
    switch (str->ptr[0]) {
    case '-': negative = true;
    case '+': str->ptr += 1;
              str->len -= 1;
    }                      
    for (; str->len > 0; str->ptr += 1, str->len -= 1) {
        auto ch = str->ptr[0];
        if (ch < '0' || ch > '9') break;
        is_number = true;
        s64 d = ch - '0';
        if (*result < (min + d) / 10) {
            is_number = false;
            *result = min;
            break;
        }
        *result *= 10;
        *result -= d;
    }
    if (!negative) {
        if (*result < -max) {
            *result = max;
            return false;
        }
        *result = -*result;
    }
    return is_number;
}
bool str_to_u64(string *str, u64 *result) {
    *result = 0;
    if (!str || !*str) return false;
    const u64 max = ~0ull;
    bool is_number = false;
    for (; str->len > 0; str->ptr += 1, str->len -= 1) {
        auto ch = str->ptr[0];
        if (ch < '0' || ch > '9') break;
        is_number = true;
        u64 d = ch - '0'; 
        if (*result > (max - d) / 10) {
            is_number = false;
            *result = max;
            break;
        }
        *result *= 10;
        *result += d;
    }
    return is_number;
}
bool str_to_f64(string *str, f64 *result) {
    *result = 0;
    if (!str || !*str) return false;
    bool is_number = false;
    bool negative = false;
    switch (str->ptr[0]) {
    case '-': negative = true;
    case '+': str->ptr += 1;
              str->len -= 1;
    }
    s64 fraction = 0;
    for (; str->len > 0; str->ptr += 1, str->len -= 1) {
        auto ch = str->ptr[0];
        if (ch == '.' && fraction == 0) {
            fraction = -1;
            continue;
        }
        if (ch < '0' || ch > '9') break;
        is_number = true;

        f64 d = ch - '0';
        if (fraction < 0) {
            d *= ng::pow10(fraction);
            fraction -= 1;
        } else {
            *result *= 10;
        }
        *result += d;
    }
    return is_number;
}
//string::string(const print_buffer &b) : len{ng_cast(s64) ng_min(b.len, b.cap)}, ptr{b.str} {} // @Todo @Temporary @Incomplete: change print_buffer to have a signed length/capacity.
void print_item(print_buffer *buf, string s) {
    if (s) buf->put_u8s(s.ptr, s.len);
}
u64 print_get_item_size(string s) { return s ? s.len : 0; }
u8 &string::operator[](s64 n) {
    ng_assert(n >= 0 && n < len);
    return ptr[n];
}
//string::string(const char *c_str) : ptr{ng_cast(u8 *) c_str}, len{ng_cast(s64)(c_str ? ng::strlen(c_str) : 0)} {}
//string &string::operator=(const char *c_str) { return operator=(string::c_str(c_str)); }
string string::substring(s64 index) {
    if (!*this) return {};
    auto real_index = ng_clamp(index, 0, len);
    return string{len - real_index, ptr + real_index};
}
string string::substring(s64 index, s64 length) {
    if (!*this) return {};
    auto real_index = ng_clamp(index, 0, len);
    auto real_length = length;
    if (real_index + length > len) real_length = len - real_index;
    return string{real_length, ptr + real_index};
}
string c_str(string source, allocator *a) {
    if (!source) return {};
    string result = {};
    result.len = source.len + 1;
    result.ptr = ng_cast(u8 *) a->allocate(result.len);
    ng_assert(result.ptr);
    if (!result.ptr) return {};
    ng::memcpy(result.ptr, source.ptr, source.len);
    result.ptr[source.len] = '\0';
    return result;
}
string copy_string(string s, allocator *a) {
    string result;
    result.ptr = ng_cast(u8 *) a->allocate(s.len);
    if (result.ptr) {
        result.len = s.len;
        ng::memcpy(result.ptr, s.ptr, ng_cast(ng::usize) s.len);
    }
    return result;
}
void free_string(string *s, allocator *a) {
    if (s->ptr && a) a->free(s->ptr, s->len);
    *s = {};
}      
s64 string_compare(string a, string b) {
    if (!a || !b) return ng_cast(bool) a - ng_cast(bool) b;
    auto cmp_len = ng_min(a.len, b.len);
    auto result = ng::memcmp(a.ptr, b.ptr, ng_cast(usize) cmp_len);
    if (result) return result;
    return a.len - b.len;
}
s64 string_compare(string a, string b, s64 (*comparator)(u8, u8, void *), void *userdata) {
    s64 result = 0;
    auto min_len = ng_min(a.len, b.len);
    if (min_len > 0) {
        auto n = min_len;
        auto u1 = a.ptr, u2 = b.ptr;
        while (n && comparator(*u1, *u2, userdata) == 0) --n, ++u1, ++u2;
        if (n) result = comparator(*u1, *u2, userdata);
        if (result) return result;
    }
    return a.len - b.len;
}
// hash table stuff
bool is_prime(u64 x) {
    u64 o = 4;
    for (u64 i = 5;; i += o) {
        auto q = x / i;
        if (q < i) return true;
        if (x == q * i) return false;
        o ^= 6;
    }
}
u64 next_prime(u64 x) {
    switch (x) {
        case 0: case 1:                     return 2;
        case 2:                             return 3;
        case 3: case 4:                     return 5;
        case 5: case 6:                     return 7;
        case 7: case 8: case 9: case 10:    return 11;
        case 11: case 12:                   return 13;
        case 13: case 14: case 15: case 16: return 17;
        case 17: case 18:                   return 19;
    }
    ++x;
    u64 k = x / 6;
    u64 i = x - 6 * k;
    u64 o = i < 2 ? 1 : 5;
    x = 6 * k + o;
    for (i = (3 + o) / 2; !is_prime(x); x += i) i ^= 6;
    return x;
}
u64 get_hash(string key) {
    u64 result = 0xcbf29ce484222325ull;
    for (auto c : key) result = (result ^ c) * 0x100000001b3ull;
    return result;
}
u64 get_hash(u64 x) {
    x = (x ^ (x >> 30)) * 0xbf58476d1ce4e5b9ull;
    x = (x ^ (x >> 27)) * 0x94d049bb133111ebull;
    x ^= x >> 31;
    return x;
}
u64 get_hash(u32 x) {
    x = ((x >> 16) ^ x) * 0x45d9f3b;
    x = ((x >> 16) ^ x) * 0x45d9f3b;
    x = (x >> 16) ^ x;
    return x;
}
namespace tests { // clang-format off
static void assert_equal(f64 result, f64 desired, string expr) { ng_always_on_assert(result == desired, "% should be % but was %.", expr, desired, result); };
static void assert_isnan(f64 result, string expr) { ng_always_on_assert(ng::isnan(result), "% should be nan but was %.", expr, result); };
void pow_ieee_constraints() {
#ifdef NG_TESTS
#define ng_test_assert_equal(test, should_be) (assert_equal)(test, should_be, #test##_s);
#define ng_test_assert_isnan(test) (assert_isnan)(test, #test##_s);
#define ng_test_pow_ieee(E, N) \
/* pow(+0, exp), where exp is a negative odd integer, returns +∞                                    */ E(ng::pow(0.0, -3.0), +inf)                                                                                                                                                                                                                                                                                                                 \
/* pow(-0, exp), where exp is a negative odd integer, returns -∞                                    */ E(ng::pow(-0., -3.0), -inf)                                                                                                                                                                                                                                                                                                                 \
/* pow(±0, exp), where exp is negative, finite, and is an even integer or a non-integer, returns +∞ */ E(ng::pow(0.0, -4.0), +inf) E(ng::pow(-0., -4.0), +inf) E(ng::pow(0.0, -3.5), +inf) E(ng::pow(-0., -3.5), +inf)                                                                                                                                                                                                                             \
/* pow(±0, -∞) returns +∞                                                                           */ E(ng::pow(0.0, -inf), +inf) E(ng::pow(-0., -inf), +inf)                                                                                                                                                                                                                                                                                     \
/* pow(+0, exp), where exp is a positive odd integer, returns +0                                    */ E(ng::pow(0.0, 3.0), 0.0)                                                                                                                                                                                                                                                                                                                   \
/* pow(-0, exp), where exp is a positive odd integer, returns -0                                    */ E(ng::pow(-0., 3.0), -0.)                                                                                                                                                                                                                                                                                                                   \
/* pow(±0, exp), where exp is positive non-integer or a positive even integer, returns +0           */ E(ng::pow(0.0, 3.5), 0.0) E(ng::pow(-0., 3.5), 0.0) E(ng::pow(0.0, 4.0), 0.0) E(ng::pow(-0., 4.0), 0.0)                                                                                                                                                                                                                                     \
/* pow(-1, ±∞) returns 1                                                                            */ E(ng::pow(-1.0, +inf), 1.0) E(ng::pow(-1.0, -inf), 1.0)                                                                                                                                                                                                                                                                                     \
/* pow(+1, exp) returns 1 for any exp, even when exp is NaN                                         */ E(ng::pow(1.0, 3.1415), 1.0) E(ng::pow(1.0, -123.0), 1.0) E(ng::pow(1.0, -inf), 1.0) E(ng::pow(1.0, -nan), 1.0) E(ng::pow(1.0, +inf), 1.0) E(ng::pow(1.0, +nan), 1.0)                                                                                                                                                                       \
/* pow(base, ±0) returns 1 for any base, even when base is NaN                                      */ E(ng::pow(3.1415, 0.0), 1.0) E(ng::pow(-123.0, 0.0), 1.0) E(ng::pow(-inf, 0.0), 1.0) E(ng::pow(-nan, 0.0), 1.0) E(ng::pow(+inf, 0.0), 1.0) E(ng::pow(+nan, 0.0), 1.0) E(ng::pow(3.1415, -0.), 1.0) E(ng::pow(-123.0, -0.), 1.0) E(ng::pow(-inf, -0.), 1.0) E(ng::pow(-nan, -0.), 1.0) E(ng::pow(+inf, -0.), 1.0) E(ng::pow(+nan, -0.), 1.0) \
/* pow(base, exp) returns NaN if base is finite and negative and exp is finite and non-integer.     */ N(ng::pow(-3.142, 2.718)) N(ng::pow(-123.0, 2.718))                                                                                                                                                                                                                                                                                         \
/* pow(base, -∞) returns +∞ for any |base|<1                                                        */ E(ng::pow(+0.9, -inf), +inf) E(ng::pow(-0.9, -inf), +inf)                                                                                                                                                                                                                                                                                   \
/* pow(base, -∞) returns +0 for any |base|>1                                                        */ E(ng::pow(+1.1, -inf), 0.0) E(ng::pow(-1.1, -inf), 0.0)                                                                                                                                                                                                                                                                                     \
/* pow(base, +∞) returns +0 for any |base|<1                                                        */ E(ng::pow(+0.9, +inf), 0.0) E(ng::pow(-0.9, +inf), 0.0)                                                                                                                                                                                                                                                                                     \
/* pow(base, +∞) returns +∞ for any |base|>1                                                        */ E(ng::pow(+1.1, +inf), +inf) E(ng::pow(-1.1, +inf), +inf)                                                                                                                                                                                                                                                                                   \
/* pow(-∞, exp) returns -0 if exp is a negative odd integer                                         */ E(ng::pow(-inf, -3.0), -0.)                                                                                                                                                                                                                                                                                                                 \
/* pow(-∞, exp) returns +0 if exp is a negative non-integer or even integer                         */ E(ng::pow(-inf, -3.5), 0.0) E(ng::pow(-inf, -4.0), 0.0)                                                                                                                                                                                                                                                                                     \
/* pow(-∞, exp) returns -∞ if exp is a positive odd integer                                         */ E(ng::pow(-inf, +3.0), -inf)                                                                                                                                                                                                                                                                                                                \
/* pow(-∞, exp) returns +∞ if exp is a positive non-integer or even integer                         */ E(ng::pow(-inf, +3.5), +inf) E(ng::pow(-inf, +4.0), +inf)                                                                                                                                                                                                                                                                                   \
/* pow(+∞, exp) returns +0 for any negative exp                                                     */ E(ng::pow(+inf, -3.5), 0.0) E(ng::pow(+inf, -3.0), 0.0) E(ng::pow(+inf, -4.0), 0.0) E(ng::pow(+inf, -inf), 0.0)                                                                                                                                                                                                                             \
/* pow(+∞, exp) returns +∞ for any positive exp                                                     */ E(ng::pow(+inf, +3.5), +inf) E(ng::pow(+inf, +3.0), +inf) E(ng::pow(+inf, +4.0), +inf) E(ng::pow(+inf, +inf), +inf)                                                                                                                                                                                                                         \
/* except where specified above, if any argument is NaN, NaN is returned                            */ N(ng::pow(+nan, +nan)) N(ng::pow(-nan, -nan)) N(ng::pow(-nan, +ng::TAU)) N(ng::pow(+nan, -ng::TAU)) N(ng::pow(-ng::TAU, +nan)) N(ng::pow(+ng::TAU, -nan))                                                                                                                                                                                   \
/* */
    ng_test_pow_ieee(ng_test_assert_equal, ng_test_assert_isnan);
#endif
} // clang-format on
// @Attribution: Bruce Dawson randomascii.wordpress.com/2014/01/27/
static void exhaustive_float_test(float (*test_proc)(float), float (*reference_proc)(float),
                                  u32 start, u32 stop, const char* desc) {
    auto start_f = ng_bitcast(f32)(start);
    auto stop_f = ng_bitcast(f32)(stop);
    ng::print("Testing % from % (%u) to % (%u) (inclusive).\n"_s, desc, start_f, start, stop_f, stop);
    s64 count = 0;
    s64 errors = 0;
    s64 i = start;
    s64 total_tested = ng_cast(s64) stop - start + 1;
    ng::print("% numbers will be tested.\n"_s, total_tested);
    for (;i <= stop; i += 1) {
        auto input = ng_bitcast(f32)(i);
        // if (ng::isnan(input)) continue;
        auto test_value = test_proc(input);
        auto reference_value = reference_proc(input);
        // If the results don't match then report an error.
#ifdef NG_TESTS_CHECK_SIGN_OF_ZERO
        // If we check the internal representations then we will detect when
        // +0 and -0 are mismatched. This an additional billion failures
        // for many of these functions, and is too fussy for many uses.
        if (ng_bitcast(u32)(test_value) != ng_bitcast(u32)(reference_value) &&
#else
        // Comparing the float values means that +0 and -0 will be treated
        // as equal.
        if (test_value != reference_value &&
#endif
            // If both results are NaNs then we treat that as a match.
            (test_value == test_value || reference_value == reference_value)) {
            ++errors;
            if (errors <= 200) {
                ng::print("Input %, expected %, got %\n"_s, input, reference_value, test_value);
            }
        }
        // Occasionally print out a progress message
        count += 1;
        if (count == 100000000) {
            count = 0;
            ng::print("At %, % errors               \r"_s, input, errors);
        }
    }

    ng::print("% errors (%%) detected in %."
              "                        \n\n"_s,
              errors, errors * 100.0 / total_tested, ng::fmt_char('%'), desc);
}
void round_floor_ceil(
    // float (*ref_round)(float),
    float (*ref_floor)(float),
    float (*ref_ceil)(float)) {
    // exhaustive_float_test(ng_cast(float (*)(float)) ng::round, ref_round, 0, ~0u, "ng::floor");
    exhaustive_float_test(ng_cast(float (*)(float)) ng::floor, ref_floor, 0, ~0u, "ng::round");
    exhaustive_float_test(ng_cast(float (*)(float)) ng::ceil, ref_ceil, 0, ~0u, "ng::ceil");
}
} // namespace tests
} // namespace ng
#endif // NG_DEFINE
