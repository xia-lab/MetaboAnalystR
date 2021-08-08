#include <Rdefines.h>

void DescendZero(double *yvals, int *numin, int *istart,
                 int *ilower, int *iupper);

void DescendMin(double *yvals, int *numin, int *istart,
                int *ilower, int *iupper);

void FindEqualGreaterM(const double *in, const int *size, const double *values,
                       const int *valsize, int *index);

void FindEqualGreater(const double *in, const int *size, const double *target,
                      int *index);

void FindEqualLess(const double *in, const int *size, const double *target,
                   int *index);

void ColMax(const double *in, const int *n, const int *dn, double *out);

void RowMax(const double *in, const int *dn, const int *p, double *out);

void WhichColMax(const double *in, const int *n, const int *dn, int *out);

void WhichRowMax(const double *in, const int *dn, const int *p, int *out);

int ComparIntIndex(void *numptr, const void *i1, const void *i2);

void RectUnique(const double *m, const int *order, const int *nrow,
                const int *ncol, const double *xdiff, const double *ydiff,
                int *keep);

SEXP DoubleMatrix(SEXP nrow, SEXP ncol);

SEXP IntegerMatrix(SEXP nrow, SEXP ncol);

SEXP LogicalMatrix(SEXP nrow, SEXP ncol);
