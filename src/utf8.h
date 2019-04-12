#pragma once /* utf8 encoding/decoding */
#ifndef utf8_h
#define utf8_h
const int utf8_decode_accept = 0;
const int utf8_decode_reject = 12;
/* Returns decode_accept once c is decoded, or _reject if invalid. */
inline unsigned int utf8_decode(unsigned int *fsm, unsigned int *c,
                                unsigned char byte) {
    /* Based on http://bjoern.hoehrmann.de/utf-8/decoder/dfa/         */
    /* Copyright (c) 2008-2010 Bjoern Hoehrmann <bjoern@hoehrmann.de> */
    static const char decode_table[] = {
        0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0, 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
        0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0, 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
        0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0, 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
        0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0, 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
        1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1, 9,9,9,9,9,9,9,9,9,9,9,9,9,9,9,9,
        7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7, 7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,
        8,8,2,2,2,2,2,2,2,2,2,2,2,2,2,2, 2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,
        10,3,3,3,3,3,3,3,3,3,3,3,3,4,3,3,11,6,6,6,5,8,8,8,8,8,8,8,8,8,8,8,

         0,12,24,36,60,96,84,12,12,12,48,72,12,12,12,12,12,12,12,12,12,12,12,12,
        12, 0,12,12,12,12,12, 0,12, 0,12,12,12,24,12,12,12,12,12,24,12,24,12,12,
        12,12,12,12,12,12,12,24,12,12,12,12,12,24,12,12,12,12,12,12,12,24,12,12,
        12,12,12,12,12,12,12,36,12,36,12,12,12,36,12,12,12,12,12,36,12,36,12,12,
        12,36,12,12,12,12,12,12,12,12,12,12,
    };
    unsigned int type = decode_table[byte];
    *c = *fsm ? (byte & 0x3fu) | (*c << 6) : (0xff >> type) & byte;
    *fsm = decode_table[256 | *fsm + type];
    return *fsm;
}
/* dest <- utf8(c), e <- e | errors. Only writes to as many bytes as needed. */
inline int utf8_encode(unsigned int c, void *dest, unsigned int *e) {
    *e |= ((c >> 11) == 0x1b) | (c > 0x10ffff) << 1 | ((c >> 1) == 0x7fff) << 2;
    char len = (c > 0x7f) + (c > 0x7ff) + (c > 0xffff), *p = (char *)dest;
    /* (decode: shiftc = 0x60c1200 >> len*8, shifte = 0x20406000 >> len*8.) */
    char head = 0xf0e0c000 >> (len << 3); // 0xf2ecc600
    p[len]           = 0x80 | c            &       0x3f;
    p[len >> 1]      = 0x80 | c >>      12 &       0x3f;
    p[1 << len >> 2] = 0x80 | c >>       6 &       0x3f;
    p[0]             = head | c >> len * 6 & ~head >> 1;
    return len + 1;
}
inline unsigned long long utf8_strlen(const char *str) {
    unsigned long long result = 0;
    while (*str++) result += (*str & 0xc0) != 0x80;
    return result;
}
#endif /* utf8_h */