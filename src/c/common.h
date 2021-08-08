/* fastmatch - common types */

#ifndef FM_COMMON_H__
#define FM_COMMON_H__

/* for speed (should not really matter in this case as most time is spent in the hashing) */
#define USE_RINTERNALS 1
#include <Rinternals.h>

#ifndef R_SHORT_LEN_MAX /* for compatibility with old R */
#define XLENGTH(X) LENGTH(X)
#define IS_LONG_VEC(X) 0
typedef R_len_t R_xlen_t;
#endif

/* hash_index_t is big enough to cover long vectors */
#ifdef LONG_VECTOR_SUPPORT
typedef R_xlen_t hash_index_t;
#else
typedef int hash_index_t;
#endif

/* hashes are always 32-bit -- this is for compatibility with
   the hash function used in R.
   This means that long vectors are fine, but they may not have
   more than 2^32 - 1 unique values */
typedef unsigned int hash_value_t;

#endif
