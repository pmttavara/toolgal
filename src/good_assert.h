#ifndef good_assert /* Better assert() for Windows - public domain */
#ifdef _WIN32
#ifdef __cplusplus
extern "C" {
#endif
#ifndef _WINDOWS_
__declspec(dllimport) int __stdcall ExitProcess(unsigned int a);
__declspec(dllimport) int __stdcall MessageBoxA(void *a, const char *b, 
                                                const char *c, unsigned int d);
#endif /* _WINDOWS_ */
static int good_assert_(const char *s) {
    int x = MessageBoxA(0, s, "Assertion Failed", 0x2102);
    if (x == 3) ExitProcess(1);
    return x == 4;
}
#define good_assert_STR_(LINE) #LINE
#define good_assert_STR(LINE) good_assert_STR_(LINE)
#ifdef NDEBUG
#define good_assert(ignore) ((void)0)
#else
#define good_assert(e) ((e) || good_assert_("At " __FILE__ ":" good_assert_STR \
    (__LINE__) ":\n\n" #e "\n\nPress Retry to debug.") && (__debugbreak(), 0))
#endif
#ifdef __cplusplus
}
#endif
#else
#include <assert.h>
#define good_assert assert
#endif /* _WIN32 */
#endif /* good_assert */