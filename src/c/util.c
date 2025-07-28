#include <math.h>
#include <R.h>
#include "util.h"

void DescendZero(double *yvals, int *numin, int *istart,
                 int *ilower, int *iupper) {

    int i;

    for (i = *istart; i >= 0; i--)
        if (yvals[i] < 0)
            break;
    *ilower = i + 1;

    for (i = *istart; i < *numin; i++)
        if (yvals[i] < 0)
            break;
    *iupper = i - 1;
}

void DescendValue(const double *yvals, const int *numin, const int *istart,
                  const double *yval, int *ilower, int *iupper) {

    int i;

    for (i = *istart; i >= 0; i--)
        if (yvals[i] < *yval)
            break;
    *ilower = i + 1;

    for (i = *istart; i < *numin; i++)
        if (yvals[i] < *yval)
            break;
    *iupper = i - 1;
}

void DescendMin(double *yvals, int *numin, int *istart,
                int *ilower, int *iupper) {

    int i;

    for (i = *istart; i > 0; i--)
        if (yvals[i-1] >= yvals[i])
            break;
    *ilower = i;

    for (i = *istart; i < *numin-1; i++)
        if (yvals[i+1] >= yvals[i])
            break;
    *iupper = i;
}

void FindEqualGreaterM(const double *in, const int *size, const double *values,
                       const int *valsize, int *index) {

    int i, idx = 0;

    for (i = 0; i < *valsize; i++) {
        while (idx < *size && in[idx] < values[i])
            idx++;
        index[i] = idx;
    }
}

void FindEqualGreater(const double *in, const int *size, const double *target,
                      int *index) {

    int min = 0, max = *size-1, i = max/2;

    while (min < max) {
        if (in[i] < *target)
            min = i+1;
        else
            max = i;
        i = (min+max)/2;
    }

    *index = i;
}

void FindEqualLess(const double *in, const int *size, const double *target,
                   int *index) {

    int min = 0, max = *size-1, i = max/2;

    while (min < max) {
        if (in[i] > *target)
            max = i-1;
        else
            min = i;
        i = (int) ceil((min+max)/(float)2);
    }

    *index = i;
}

void ColMax(const double *in, const int *n, const int *dn, double *out) {

    int i, j;

    for (i = 0; i < *dn; i++) {
        out[i] = in[*n*i];
        for (j = 1; j < *n; j++)
            if (in[*n*i + j] > out[i])
                out[i] = in[*n*i + j];
    }
}

void RowMax(const double *in, const int *dn, const int *p, double *out) {

    int i, j;

    for (i = 0; i < *dn; i++) {
        out[i] = in[i];
        for (j = 1; j < *p; j++)
            if (in[i + *dn*j] > out[i])
                out[i] = in[i + *dn*j];
    }
}

void WhichColMax(const double *in, const int *n, const int *dn, int *out) {

    int i, j;

    for (i = 0; i < *dn; i++) {
        out[i] = *n*i;
        for (j = 1; j < *n; j++)
            if (in[*n*i + j] > in[out[i]])
                out[i] = *n*i + j;
    }
    for (i = 0; i < *dn; i++)
        out[i]++;
}

void WhichRowMax(const double *in, const int *dn, const int *p, int *out) {

    int i, j;

    for (i = 0; i < *dn; i++) {
        out[i] = i;
        for (j = 1; j < *p; j++)
            if (in[i + *dn*j] > in[out[i]])
                out[i] = i + *dn*j;
    }
    for (i = 0; i < *dn; i++)
        out[i]++;
}

int ComparIntIndex(void *numptr, const void *i1, const void *i2) {

    int *num = (int*)numptr;

    return num[*(int*)i1] - num[*(int*)i2];
}

void RectUnique(const double *m, const int *order, const int *nrow,
                const int *ncol, const double *xdiff, const double *ydiff,
                int *keep) {

    int    i, j, io, jo;
    int    x1 = 0, x2 = *nrow, y1 = *nrow*2, y2 = *nrow*3;

    for (i = 0; i < *nrow; i++) {
        io = order[i];
        keep[io] = 1;
        for (j = 0; j < i; j++) {
            jo = order[j];
            if (keep[jo] &&
                !(m[x1+io] - m[x2+jo] > *xdiff || m[x1+jo] - m[x2+io] > *xdiff ||
                  m[y1+io] - m[y2+jo] > *ydiff || m[y1+jo] - m[y2+io] > *ydiff)) {
                keep[io] = 0;
                /* Debuging Code
                printf("%i\t%3.1f\t%3.1f\t%4.0f\t%4.0f\t%i\t%3.1f\t%3.1f\t%4.0f\t%4.0f\n",
                       i, m[x1+io], m[x2+io], m[y1+io], m[y2+io],
                       j, m[x1+jo], m[x2+jo], m[y1+jo], m[y2+jo]);
                */
                break;
            }
        }
    }
}

SEXP DoubleMatrix(SEXP nrow, SEXP ncol) {

    SEXP matrix, dim;
	int i;
	double nrowint, ncolint;
    // unsigned int nrowint, ncolint, i;
    double *matrixptr, size;

    nrowint = INTEGER_POINTER(nrow)[0];
    ncolint = INTEGER_POINTER(ncol)[0];

    PROTECT(matrix = NEW_NUMERIC(nrowint*ncolint));
    PROTECT(dim = NEW_INTEGER(2));
    INTEGER_POINTER(dim)[0] = nrowint;
    INTEGER_POINTER(dim)[1] = ncolint;
    SET_DIM(matrix, dim);

    size = nrowint*ncolint;
    matrixptr = NUMERIC_POINTER(matrix);
    for (i = 0; i < size; i++)
        matrixptr[i] = 0;

    UNPROTECT(2);

    return matrix;
}

SEXP IntegerMatrix(SEXP nrow, SEXP ncol) {

    SEXP matrix, dim;
    unsigned int  nrowint, ncolint;

    nrowint = INTEGER_POINTER(nrow)[0];
    ncolint = INTEGER_POINTER(ncol)[0];

    PROTECT(matrix = NEW_INTEGER(nrowint*ncolint));
    PROTECT(dim = NEW_INTEGER(2));
    INTEGER_POINTER(dim)[0] = nrowint;
    INTEGER_POINTER(dim)[1] = ncolint;
    SET_DIM(matrix, dim);

    UNPROTECT(2);

    return matrix;
}

SEXP LogicalMatrix(SEXP nrow, SEXP ncol) {

    SEXP matrix, dim;
    unsigned int nrowint, ncolint;

    nrowint = INTEGER_POINTER(nrow)[0];
    ncolint = INTEGER_POINTER(ncol)[0];

    PROTECT(matrix = NEW_LOGICAL(nrowint*ncolint));
    PROTECT(dim = NEW_INTEGER(2));
    INTEGER_POINTER(dim)[0] = nrowint;
    INTEGER_POINTER(dim)[1] = ncolint;
    SET_DIM(matrix, dim);

    UNPROTECT(2);

    return matrix;
}

void continuousPtsAboveThreshold(double *x, int *istart, int *numin, double *threshold, int *num, int *n) {

    int i;
    int cnt = 0;

    for (i = *istart; i < *numin; i++) {
       if (x[i] > *threshold) cnt++;
            else cnt = 0;
       if (cnt >= *num) {
            *n = cnt;
            return ;
       }
    }

}

void continuousPtsAboveThresholdIdx(double *x, int *istart, int *numin, double *threshold, int *num, int *n) {

    int i;
    int j;
    int cnt = 0;
    int stidx = 0;
    int enidx = 0;

    for (i = *istart; i < *numin; i++) {

       if (x[i] > *threshold) {
           cnt++;
           if (cnt == 1) stidx=i;
               else enidx=i;
       }  else cnt = 0;

       if ((cnt==0 || i == *numin - 1) && (enidx - stidx + 1) >= *num) {
            for (j = stidx; j<= enidx; j++) {
                n[j] = 1;
            }
            stidx = 0;
            enidx = 0;
       }
    }

}


void FindEqualGreaterUnsorted(const double *in, const int *size, const double *target,
                      int *index) {
   int i;

   for (i=0; (i< *size-1 &&  in[i] < *target); i++) {}

   *index = i;
}
