/*
 *  fastmatch: fast implementation of match() in R using semi-permanent hash tables
 *
 *  Copyright (C) 2010, 2011  Simon Urbanek
 *
 *  This program is free software; you can redistribute it and/or modify
 *  it under the terms of the GNU General Public License as published by
 *  the Free Software Foundation; version 2 of the License.
 *
 *  This program is distributed in the hope that it will be useful,
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *  GNU General Public License for more details.
 */

#include "common.h"

/* for malloc/free since we handle our hash table memory separately from R */
#include <stdlib.h>
#include <string.h>
/* for hashing for pointers we need intptr_t */
#include <stdint.h>

static SEXP match_symbol;

/* match5 to fall-back to R's internal match for types we don't support */
static SEXP match5__(SEXP itable, SEXP ix, SEXP nmatch, SEXP incomp, SEXP env) {
    /* R doesn't like us using it directly so go via eval */
    if (!match_symbol)
	match_symbol = Rf_install("match");
    SEXP lc = PROTECT(Rf_lang5(match_symbol, ix, itable, nmatch, incomp));
    SEXP res = eval(lc, env);
    UNPROTECT(1);
    return res;
}

/* ".match.hash" symbol - cached on first use */
SEXP hs;

typedef struct hash {
    hash_index_t m, els; /* hash size, added elements (unused!) */
    int k;               /* used bits */
    SEXPTYPE type;       /* payload type */
    void *src;           /* the data array of the hashed object */
    SEXP prot;           /* object to protect along whith this hash */
    SEXP parent;         /* hashed object */
    struct hash *next;   /* next hash table - typically for another type */
    hash_index_t ix[];   /* actual table of indices */
} hash_t;

/* create a new hash table with the given source and length.
   we store only the index - values are picked from the source 
   so you must make sure the source is still alive when used */
static hash_t *new_hash(void *src, hash_index_t len) {
  hash_t *h;
  int k = 1;
  hash_index_t m = 2, desired = len * 2; /* we want a maximal load of 50% */
  while (m < desired) { m *= 2; k++; }
  h = (hash_t*) calloc(1, sizeof(hash_t) + (sizeof(hash_index_t) * m));
  if (!h) Rf_error("unable to allocate %.2fMb for a hash table", (double) sizeof(hash_index_t) * (double) m / (1024.0 * 1024.0));
  h->m = m;
  h->k = k;
  h->src = src;
  return h;
}

/* free the hash table (and all chained hash tables as well) */
static void free_hash(hash_t *h) {
  if (h->next) free_hash(h->next);
  if (h->prot) R_ReleaseObject(h->prot);
  free(h);
}

/* R finalized for the hash table object */
static void hash_fin(SEXP ho) {
  hash_t *h = (hash_t*) EXTPTR_PTR(ho);
  if (h) free_hash(h);
}

/* pi-hash fn */
/* NB: the value is shifted by k to guarantee that it will not exceed
   the hash table size (in hex the value is 0xBB40E64D) */
#define HASH(X) (3141592653U * ((unsigned int)(X)) >> (32 - h->k))

/* add the integer value at index i (0-based!) to the hash */
static hash_value_t add_hash_int(hash_t *h, hash_index_t i) {
    int *src = (int*) h->src;
    int val = src[i++];
    hash_value_t addr = HASH(val);
#ifdef PROFILE_HASH
    hash_value_t oa = addr;
#endif
    while (h->ix[addr] && src[h->ix[addr] - 1] != val) {
	addr++;
	if (addr == h->m) addr = 0;
    }
#ifdef PROFILE_HASH
    if (addr != oa) Rprintf("%d: dist=%d (addr=%d, oa=%d)\n", val,
			   (int) (addr - oa), (int) addr, (int) oa);
#endif
    if (!h->ix[addr])
	h->ix[addr] = i;
    return addr;
}

/* to avoid aliasing rules issues use a union */
union dint_u {
    double d;
    unsigned int u[2];
};

/* double is a bit tricky - we nave to normalize 0.0, NA and NaN */
static double norm_double(double val) {
    if (val == 0.0) return 0.0;
    if (R_IsNA(val)) return NA_REAL;
    if (R_IsNaN(val)) return R_NaN;
    return val;
}

/* add the double value at index i (0-based!) to the hash */
static hash_value_t add_hash_real(hash_t *h, hash_index_t i) {
    double *src = (double*) h->src;
    union dint_u val;
    hash_value_t addr;
    val.d = norm_double(src[i]);
    addr = HASH(val.u[0] + val.u[1]);
#ifdef PROFILE_HASH
    hash_value_t oa = addr;
#endif
    while (h->ix[addr] && src[h->ix[addr] - 1] != val.d) {
	addr++;
	if (addr == h->m) addr = 0;
    }
#ifdef PROFILE_HASH
    if (addr != oa)
	Rprintf("%g: dist=%d (addr=%d, oa=%d)\n", val.d,
	       (int) (addr - oa), (int)addr, (int)oa);
#endif
    if (!h->ix[addr])
	h->ix[addr] = i + 1;
    return addr;
}

/* add the pointer value at index i (0-based!) to the hash */
static int add_hash_ptr(hash_t *h, hash_index_t i) {
    hash_value_t addr;
    void **src = (void**) h->src;
    intptr_t val = (intptr_t) src[i++];
#if (defined _LP64) || (defined __LP64__) || (defined WIN64)
    addr = HASH((val & 0xffffffff) ^ (val >> 32));
#else
    addr = HASH(val);
#endif
#ifdef PROFILE_HASH
    hash_value_t oa = addr;
#endif
    while (h->ix[addr] && (intptr_t) src[h->ix[addr] - 1] != val) {
	addr++;
	if (addr == h->m) addr = 0;
    }
#ifdef PROFILE_HASH
    if (addr != oa)
	Rprintf("%p: dist=%d (addr=%d, oa=%d)\n", val,
		(int)(addr - oa), (int)addr, (int)oa);
#endif
    if (!h->ix[addr])
	h->ix[addr] = i;
    return addr;
}

/* NOTE: we are returning a 1-based index ! */
static hash_index_t get_hash_int(hash_t *h, int val, int nmv) {
    int *src = (int*) h->src;
    hash_value_t addr = HASH(val);
    while (h->ix[addr]) {
	if (src[h->ix[addr] - 1] == val)
	    return h->ix[addr];
	addr++;
	if (addr == h->m) addr = 0;
    }
    return nmv;
}

/* NOTE: we are returning a 1-based index ! */
static hash_index_t get_hash_real(hash_t *h, double val, int nmv) {
    double *src = (double*) h->src;
    hash_value_t addr;
    union dint_u val_u;
    val_u.d = norm_double(val);
    addr = HASH(val_u.u[0] + val_u.u[1]);
    while (h->ix[addr]) {
	double nv = norm_double(src[h->ix[addr] - 1]);
	if (!memcmp(&nv, &val_u.d, sizeof(nv)))
	    return h->ix[addr];
	addr++;
	if (addr == h->m) addr = 0;
    }
    return nmv;
}

/* NOTE: we are returning a 1-based index ! */
static hash_index_t get_hash_ptr(hash_t *h, void *val_ptr, int nmv) {
    void **src = (void **) h->src;
    intptr_t val = (intptr_t) val_ptr;
    hash_value_t addr;
#if (defined _LP64) || (defined __LP64__) || (defined WIN64)
    addr = HASH((val & 0xffffffff) ^ (val >> 32));
#else
    addr = HASH(val);
#endif
    while (h->ix[addr]) {
	if ((intptr_t) src[h->ix[addr] - 1] == val)
	    return h->ix[addr];
	addr ++;
	if (addr == h->m) addr = 0;
    }
    return nmv;
}

static SEXP asCharacter(SEXP s, SEXP env)
{
    SEXP call, r;
    PROTECT(call = lang2(install("as.character"), s));
    r = eval(call, env);
    UNPROTECT(1);
    return r;
}

static double NA_int2real(hash_index_t res) {
    return (res == NA_INTEGER) ? R_NaReal : ((double)  res);
}

/* the only externally visible function to be called from R */
SEXP fmatch(SEXP x, SEXP y, SEXP nonmatch, SEXP incomp, SEXP hashOnly) {
    SEXP a, x_orig = x;
    SEXPTYPE type;
    hash_t *h = 0;
    int nmv = asInteger(nonmatch), np = 0, y_to_char = 0, y_factor = 0, hash_only = asInteger(hashOnly);
    hash_index_t n = (x == R_NilValue) ? 0 : XLENGTH(x);

    /* edge-cases of 0 length */
    if (n == 0) return allocVector(INTSXP, 0);
    if (y == R_NilValue || XLENGTH(y) == 0) { /* empty table -> vector full of nmv */
	if (hash_only) {
	    return y; /* no hash table created, just pass-through y */
	} else {
	    int *ai;
	    hash_index_t ii;
	    a = allocVector(INTSXP, n);
	    ai = INTEGER(a);
	    for (ii = 0; ii < n; ii++) ai[ii] = nmv;
	    return a;
	}
    }

    /* if incomparables are used we fall back straight to match() */
    if (incomp != R_NilValue && !(isLogical(incomp) && LENGTH(incomp) == 1 && LOGICAL(incomp)[0] == 0)) {
	Rf_warning("incomparables used in fmatch(), falling back to match()");
	return match5__(y, x, nonmatch, incomp, R_BaseEnv);
    }

    /* implicitly convert factors/POSIXlt to character */
    if (OBJECT(x)) {
	if (inherits(x, "factor")) {
	    x = PROTECT(asCharacterFactor(x));
	    np++;
	} else if (inherits(x, "POSIXlt")) {
	    x = PROTECT(asCharacter(x, R_GlobalEnv)); /* FIXME: match() uses env properly - should we switch to .External ? */
	    np++;
	}
    }

    /* for y we may need to do that later */
    y_factor = OBJECT(y) && inherits(y, "factor");
    y_to_char = y_factor || (OBJECT(y) && inherits(y, "POSIXlt"));
    
    /* coerce to common type - in the order of SEXP types */
    if(TYPEOF(x) >= STRSXP || TYPEOF(y) >= STRSXP)
	type = STRSXP;
    else
	type = (TYPEOF(x) < TYPEOF(y)) ? TYPEOF(y) : TYPEOF(x);
    
    /* we only support INT/REAL/STR */
    if (type != INTSXP && type != REALSXP && type != STRSXP) {
	Rf_warning("incompatible type, fastmatch() is falling back to match()");
	SEXP res = match5__(y, x_orig, nonmatch, incomp, R_BaseEnv);
	if (np) UNPROTECT(np);
	return res;
    }

    if (y_to_char && type != STRSXP) /* y = factor -> character -> type must be STRSXP */
	type = STRSXP;

    /* coerce x - not y yet because we may get away with the existing cache */
    if (TYPEOF(x) != type) {
	x = PROTECT(coerceVector(x, type));
	np++;
    }

    /* find existing cache(s) */
    if (!hs) hs = Rf_install(".match.hash");
    a = Rf_getAttrib(y, hs);
    if (a != R_NilValue) { /* if there is a cache, try to find the matching type */
	h = (hash_t*) EXTPTR_PTR(a);
	/* could the object be out of sync ? If so, better remove the hash and ignore it */
	if (!h || h->parent != y) {
#if HASH_VERBOSE
	    Rprintf(" - DISCARDING hash, its parent and the bearer don't match, taking no chances.\n");
#endif
	    h = 0;
	    Rf_setAttrib(y, hs, R_NilValue);
	}
	while (h && h->type != type) h = h->next;
    }
#ifdef CHECKHASH
    hash_t *orig_h = h;
    h = 0; /* pretend that there is no hash */
    a = R_NilValue;
#endif
    /* if there is no cache or not of the needed coerced type, create one */
    if (a == R_NilValue || !h) {
	h = new_hash(DATAPTR(y), XLENGTH(y));
	h->type = type;
	h->parent = y;
#if HASH_VERBOSE
	Rprintf(" - creating new hash for type %d\n", type);
#endif
	if (a == R_NilValue || !EXTPTR_PTR(a)) { /* if there is no cache attribute, create one */
	    a = R_MakeExternalPtr(h, R_NilValue, R_NilValue);
	    Rf_setAttrib(y, hs, a);
	    Rf_setAttrib(a, R_ClassSymbol, Rf_mkString("match.hash"));
	    R_RegisterCFinalizer(a, hash_fin);
	} else { /* otherwise append the new cache */
	    hash_t *lh = (hash_t*) EXTPTR_PTR(a);
	    while (lh->next) lh = lh->next;
	    lh->next = h;
#if HASH_VERBOSE
	    Rprintf("   (appended to the cache list)\n");
#endif
	}

	if (TYPEOF(y) != type) {
#if HASH_VERBOSE
	    if (y_to_char)
		Rprintf("   (need to convert table factor/POSIXlt to strings\n");
	    else
		Rprintf("   (need to coerce table to %d)\n", type);
#endif
	    y = y_to_char ? (y_factor ? asCharacterFactor(y) : asCharacter(y, R_GlobalEnv)) : coerceVector(y, type);
	    R_PreserveObject(y);
	    h->src = DATAPTR(y); /* this is ugly, but we need to adjust the source since we changed it */
	    h->prot = y; /* since the coerced object is temporary, we let the hash table handle its life span */
	}
	/* make sure y doesn't go away while we create the hash */
	/* R_PreserveObject(y);     */
	/* spawn a thread to create the hash */
	/* nope - so far we do it serially */
	
	{ /* create the hash table */
	    hash_index_t i, n = XLENGTH(y);
	    if (type == INTSXP)
		for(i = 0; i < n; i++)
		    add_hash_int(h, i);
	    else if (type == REALSXP)
		for(i = 0; i < n; i++)
		    add_hash_real(h, i);
	    else
		for(i = 0; i < n; i++)
		    add_hash_ptr(h, i);
	}
#ifdef CHECKHASH
	if (orig_h) {
	    if (orig_h->type != type) /* this should never happen since we check the type */
		Rf_error("Hash type mistmatch on object %p (has %d, expected %d)", y, type, orig_h->type);
	    if (orig_h->m != h->m)
		Rf_error("Object %p modified, cached hash table has size %ld, but re-hashing has %ld", y, (long)orig_h->m, (long)h->m);
	    if (memcmp(orig_h->ix, h->ix, sizeof(hash_index_t) * h->m)) {
		hash_index_t i = 0, n = h->m, No = 0, Nn = 0;
		while (i < n) {
		    if (orig_h->ix[i])
			No++;
		    if (h->ix[i])
			Nn++;
		    i++;
		}
		if (No != Nn)
		    Rf_error("Object %p resized (from %ld to %ld) after the hash table has been created", y, (long)No, (long)Nn);
		Rf_error("Object %p modified after the hash table has been created (size %ld remained constant)", y, (long)No);
	    }
	}
#endif
    }

    if (hash_only) {
	if (np) UNPROTECT(np);
	return y;
    }

    { /* query the hash table */
	SEXP r;
#ifdef LONG_VECTOR_SUPPORT
	if (IS_LONG_VEC(x)) {
	    hash_index_t i, n = XLENGTH(x);
	    double *v = REAL(r = allocVector(REALSXP, n));
	    if (nmv == NA_INTEGER) {
		/* we have to treat nmv = NA differently,
		   because is has to be transformed into
		   NA_REAL in the result. To avoid checking
		   when nmv is different, we have two paths */
		if (type == INTSXP) {
		    int *k = INTEGER(x);
		    for (i = 0; i < n; i++)
			v[i] = NA_int2real(get_hash_int(h, k[i], NA_INTEGER));
		} else if (type == REALSXP) {
		    double *k = REAL(x);
		    for (i = 0; i < n; i++)
			v[i] = NA_int2real(get_hash_real(h, k[i], NA_INTEGER));
		} else {
		    SEXP *k = (SEXP*) DATAPTR(x);
		    for (i = 0; i < n; i++)
			v[i] = NA_int2real(get_hash_ptr(h, k[i], NA_INTEGER));
		}
	    } else { /* no need to transcode nmv */
		if (type == INTSXP) {
		    int *k = INTEGER(x);
		    for (i = 0; i < n; i++)
			v[i] = (double) get_hash_int(h, k[i], nmv);
		} else if (type == REALSXP) {
		    double *k = REAL(x);
		    for (i = 0; i < n; i++)
			v[i] = (double) get_hash_real(h, k[i], nmv);
		} else {
		    SEXP *k = (SEXP*) DATAPTR(x);
		    for (i = 0; i < n; i++)
			v[i] = (double) get_hash_ptr(h, k[i], nmv);
		}
	    }
	} else
#endif
	{
	    /* short vector - everything is int */
	    int i, n = LENGTH(x);
	    int *v = INTEGER(r = allocVector(INTSXP, n));
	    if (type == INTSXP) {
		int *k = INTEGER(x);
		for (i = 0; i < n; i++)
		    v[i] = get_hash_int(h, k[i], nmv);
	    } else if (type == REALSXP) {
		double *k = REAL(x);
		for (i = 0; i < n; i++)
		    v[i] = get_hash_real(h, k[i], nmv);
	    } else {
		SEXP *k = (SEXP*) DATAPTR(x);
		for (i = 0; i < n; i++)
		    v[i] = get_hash_ptr(h, k[i], nmv);
	    }
	}
	if (np) UNPROTECT(np);
	return r;
    }
}

/* FIXME: should we also attach the hash? */
SEXP coalesce(SEXP x) {
    SEXPTYPE type = TYPEOF(x);
    SEXP res;
    hash_index_t i, n = XLENGTH(x), dst = 0;
    hash_t *h;
    hash_index_t *count;

    res = PROTECT(allocVector(INTSXP, XLENGTH(x)));

    h = new_hash(DATAPTR(x), XLENGTH(x));
    h->type = type;
    h->parent = x;
 
    if (!(count = calloc(h->m, sizeof(*count)))) {
	free_hash(h);
	Rf_error("Unable to allocate memory for counts");
    }

    /* count the size of each category - we're using negative numbers
       since we will re-purpose the array later to hold the pointer to the
       index of the next entry to stroe which will be positive */
    if (type == INTSXP)
	for(i = 0; i < n; i++)
	    count[add_hash_int(h, i)]--;
    else if (type == REALSXP)
	for(i = 0; i < n; i++)
	    count[add_hash_real(h, i)]--;
    else
	for(i = 0; i < n; i++)
	    count[add_hash_ptr(h, i)]--;

    if (type == INTSXP)
	for(i = 0; i < n; i++) {
	    hash_value_t addr = add_hash_int(h, i);
	    if (count[addr] < 0) { /* this cat has not been used yet, reserve the index space for it*/
		hash_index_t ni = -count[addr];
		count[addr] = dst;
		dst += ni;
	    }
	    INTEGER(res)[count[addr]++] = i + 1;
	}
    else if (type == REALSXP)
	for(i = 0; i < n; i++) {
	    hash_value_t addr = add_hash_real(h, i);
	    if (count[addr] < 0) {
		hash_index_t ni = -count[addr];
		count[addr] = dst;
		dst += ni;
	    }
	    INTEGER(res)[count[addr]++] = i + 1;
	}
    else
	for(i = 0; i < n; i++) {
	    hash_value_t addr = add_hash_ptr(h, i);
	    if (count[addr] < 0) {
		hash_index_t ni = -count[addr];
		count[addr] = dst;
		dst += ni;
	    }
	    INTEGER(res)[count[addr]++] = i + 1;
	}
    
    free(count);
    free_hash(h);

    UNPROTECT(1);
    return res;
}
