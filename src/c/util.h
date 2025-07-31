#include <Rdefines.h>

void DescendMin(double *yvals, int *numin, int *istart,
                int *ilower, int *iupper);

void FindEqualGreaterM(const double *in, const int *size, const double *values,
                       const int *valsize, int *index);


void RectUnique(const double *m, const int *order, const int *nrow,
                const int *ncol, const double *xdiff, const double *ydiff,
                int *keep);
