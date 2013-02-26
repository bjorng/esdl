/*
 *  Copyright (c) 2001 Dan Gudmundsson
 *  See the file "license.terms" for information on usage and redistribution
 *  of this file, and for a DISCLAIMER OF ALL WARRANTIES.
 *
 *     $Id$
 */

/***************************
 Some Macros to the byte conversion 
 between the byte buffer and real types.
 This code is borrowed from the erl_interface 
 file putget.h 
****************************/

#ifdef __cplusplus
    extern "C" {
#endif 

#ifndef BYTECONV_H
#define BYTECONV_H

#define put8(s,n) do { \
  (s)[0] = (char)((n) & 0xff); \
  (s) += 1; \
} while (0) 
            
#define put16le(s,n) do { \
  (s)[0] = (n) & 0xff;  \
  (s)[1] = ((n) >>  8) & 0xff; \
  (s) += 2; \
} while (0) \
     
#define put32le(s,n) do { \
  (s)[0] = (n) & 0xff;  \
  (s)[1] = ((n) >>  8) & 0xff; \
  (s)[2] = ((n) >>  16) & 0xff; \
  (s)[3] = ((n) >>  24) & 0xff; \
  (s) += 4; \
} while (0)
            
#define put16be(s,n) do { \
  (s)[0] = ((n) >>  8) & 0xff; \
  (s)[1] = (n) & 0xff; \
  (s) += 2; \
} while (0)
     
#define put32be(s,n) do {  \
  (s)[0] = ((n) >>  24) & 0xff; \
  (s)[1] = ((n) >>  16) & 0xff; \
  (s)[2] = ((n) >>  8) & 0xff;  \
  (s)[3] = (n) & 0xff; \
  (s) += 4; \
} while (0)

#define get8(s) \
     ((s) += 1, \
      ((unsigned char *)(s))[-1] & 0xff)
     
#define get16le(s) \
     ((s) += 2, \
      (((((unsigned char *)(s))[-1] << 8) | \
        ((unsigned char *)(s))[-2])) & 0xffff)
     
#define get32le(s) \
     ((s) += 4, \
      ((((unsigned char *)(s))[-1] << 24) | \
       (((unsigned char *)(s))[-2] << 16) | \
       (((unsigned char *)(s))[-3] << 8) | \
       ((unsigned char *)(s))[-4]))

#define get16be(s) \
     ((s) += 2, \
      (((((unsigned char *)(s))[-2] << 8) | \
        ((unsigned char *)(s))[-1])) & 0xffff) 
     
#define get32be(s) \
     ((s) += 4, \
      ((((unsigned char *)(s))[-4] << 24) | \
       (((unsigned char *)(s))[-3] << 16) | \
       (((unsigned char *)(s))[-2] << 8) | \
       ((unsigned char *)(s))[-1]))

#if SDL_BYTEORDER == SDL_BIG_ENDIAN

#define putFloat32be(s,n) do {  \
  unsigned char * t = (unsigned char *) &n; \
  (s)[0] = t[0]; \
  (s)[1] = t[1];\
  (s)[2] = t[2];\
  (s)[3] = t[3];\
  (s) += 4; \
} while (0)

#define putFloat64be(s,n) do {  \
  unsigned char * t = (unsigned char *) &n; \
  (s)[0] = t[0]; \
  (s)[1] = t[1]; \
  (s)[2] = t[2]; \
  (s)[3] = t[3]; \
  (s)[4] = t[4]; \
  (s)[5] = t[5]; \
  (s)[6] = t[6]; \
  (s)[7] = t[7]; \
  (s) += 8; \
} while (0)

#else

#define putFloat32be(s,n) do {  \
  unsigned char * t = (unsigned char *) &n; \
  (s)[3] =  t[0];\
  (s)[2] =  t[1];\
  (s)[1] =  t[2];\
  (s)[0] =  t[3];\
  (s) += 4; \
} while (0)

#define putFloat64be(s,n) do {  \
  unsigned char * t = (unsigned char *) &n; \
  (s)[7] = t[0]; \
  (s)[6] = t[1]; \
  (s)[5] = t[2]; \
  (s)[4] = t[3]; \
  (s)[3] = t[4]; \
  (s)[2] = t[5]; \
  (s)[1] = t[6]; \
  (s)[0] = t[7]; \
  (s) += 8; \
} while (0)

#endif

#endif

#ifdef __cplusplus
    }
#endif 
