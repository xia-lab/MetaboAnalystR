#include <math.h>
#include <R.h>
#include "util.h"


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
