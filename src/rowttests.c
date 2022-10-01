/*
 * Copyright W. Huber 2005
 */
 
#include <R.h>
#include <Rinternals.h>
#include <R_ext/Rdynload.h>
#include <R_ext/Utils.h> 

#include <stdlib.h>

/* #define DEBUG */

char errmsg[256];

/*-----------------------------------------------------------------
  which=0:  t-test by row
  which=1:  t-test by column
-----------------------------------------------------------------*/
void rowcolttests_c(double *x, int *fac, int nr, int nc, int no, int nt, 
                    int which, int nrgrp, int na_rm,
                    double *statistic, double *dm, double *df) {

    int i, j, grp;
    double z, delta, newmean, factor;

    /* Currently the following provides for one- and two-sample t-tests (nrgrp=1 or 2), 
       but it should be possible to generalize this code to more samples
       (F-test) without too many changes */

    int *n[2];
    double* s[2];
    double* ss[2];

    if(nrgrp>2)
	error("Please do not use 'nrgrp' >2 with 'rowcolttests'");

    /* allocate and initialize storage for intermediate quantities
       (namely first and second moments for each group) */
    for(grp=0; grp<nrgrp; grp++) {
	n[grp] = (int *) R_alloc(nt, sizeof(int));
	s[grp]  = (double*) R_alloc(nt, sizeof(double));
	ss[grp] = (double*) R_alloc(nt, sizeof(double));
	for(i=0; i<nt; i++)
	    n[grp][i] = s[grp][i] = ss[grp][i] = 0;
    }

    /* A numerically stable one-pass algorithm is used, see
       http://en.wikipedia.org/wiki/Algorithms_for_calculating_variance#On-line_algorithm 
       here: s ~ mean, ss ~ M2.
       Work through the large matrix x in the order in which it is in memory (column-wise) -
       in the hope that this may speed up getting it through the CPU. */
    switch(which) {
	case 0:  /* by row */
	    for(i=0; i<nr; i++) {
		for(j=0; j<nc; j++) {
		    grp = fac[j];
		    if(grp!=R_NaInt) {
			z = x[i+nr*j];
			if (na_rm && R_IsNA(z))
			    continue;
			n[grp][i]++;
			delta	= z - s[grp][i];
			newmean = s[grp][i] + delta/n[grp][i];
			s[grp][i]  = newmean;
			ss[grp][i] += delta*(z-newmean);
		    }
		} /* for j */
	    } /* for i */
	    break;
	case 1:  /* by column */
	    for(i=0; i<nr; i++) {
		grp = fac[i];
		if(grp!=R_NaInt) { 
		    for(j=0; j<nc; j++) {
			z = x[i+nr*j];
			if (na_rm && R_IsNA(z))
			    continue;
			n[grp][j]++;
			delta	= z - s[grp][j];
			newmean = s[grp][j] + delta/n[grp][j];
			s[grp][j]  = newmean;
			ss[grp][j] += delta*(z-newmean);
		    } /* for j */
		} /* if */ 
	    } /* for i */
	    break;
	default:
	    error("Bummer!");
    }

    switch(nrgrp) {
    case 1:
        for(i=0; i<nt; i++) {
	    if (n[0][i] == 0) {
		df[i] = dm[i] = statistic[i] = R_NaReal;
		continue;
	    }
	    df[i]	 = n[0][i]-1;
	    factor	 = sqrt((df[i]) * n[0][i]);
	    z            = ss[0][i];
	    dm[i]        = s[0][i];
	    statistic[i] =  factor * dm[i] / sqrt(z);
        }
        break;
    case 2:
	for(i=0; i<nt; i++) {
	    if ((n[0][i] == 0) || (n[1][i] == 0)) {
		df[i] = dm[i] = statistic[i] = R_NaReal;
		continue;
	    }
	    df[i]	 = n[0][i]+n[1][i]-2;
	    factor	 = sqrt((df[i]) * (double) n[0][i] *
				(double) n[1][i] / (n[0][i]+n[1][i]));
            z            = ss[0][i] + ss[1][i];
	    dm[i]        = s[0][i] - s[1][i];
	    statistic[i] =  factor * dm[i] / sqrt(z);
	}
        break;
    default:
      error("Bummer!");
    } /* switch */

    return;
} 

/*-----------------------------------------------------------------

   R interface 
   x :    matrix
   fac:   int with values 0 and 1, defining the two groups.
   which: int. For 0, do the tests along the rows, for 1, 
          along the columns 
------------------------------------------------------------------*/
SEXP rowcolttests(SEXP _x, SEXP _fac, SEXP _nrgrp, SEXP _which, SEXP _na_rm)
{
  SEXP dimx;  /* dimensions of x */
  SEXP res, namesres;      /* return value: a list */
  SEXP statistic, dm, df;  /* list elements for constructing 
                              the return value */

  double *x;
  int *fac, na_rm;
  int i, which, nrgrp;
  int nr;  /* number of rows     */
  int nc;  /* number of columns  */
  int no;  /* number of objects  */
  int nt;  /* number of tests    */

  /* check input argument x */
  PROTECT(dimx = getAttrib(_x, R_DimSymbol));
  if((!isReal(_x)) | isNull(dimx) | (LENGTH(dimx)!=2))
      error("Invalid argument 'x': must be a real matrix."); 
  x   = REAL(_x);
  nr  = INTEGER(dimx)[0];
  nc  = INTEGER(dimx)[1];
  UNPROTECT(1);          
  /* done with dimx */

  /* check input argument which */
  if(!isInteger(_which) || length(_which)!=1) 
      error("'which' must be integer of length 1.");
  which = INTEGER(_which)[0];

  /* check input argument nrgrp */
  if(!isInteger(_nrgrp) || length(_nrgrp)!=1) 
      error("'nrgrp' must be integer of length 1.");
  nrgrp = INTEGER(_nrgrp)[0];

  no = nt = -1; /* initialize - this is just to make some overeager compilers happy */

  /* check input argument fac */
  if(!isInteger(_fac))
      error("'fac' must be an integer.");
  switch(which) {
      case 0: 
	  if(length(_fac)!=nc) {
	      sprintf(errmsg, "length(fac)=%d, ncol(x)=%d, should be the same.",
		      length(_fac), nc);
	      error(errmsg);
	  }
          no = nc;
          nt = nr;
	  break;
      case 1:
	  if(length(_fac)!=nr) {
	      sprintf(errmsg, "length(fac)=%d, nrow(x)=%d, should be the same.",
		      length(_fac), nr);
	      error(errmsg);
	  }
          no = nr;
          nt = nc;
	  break;
      default:
	  error("'which' must be 0 or 1.");
  }
  
  fac = INTEGER(_fac);
  for(i=0; i<no; i++)
      if(! ((fac[i]==R_NaInt) || ((fac[i]>=0)&&(fac[i]<nrgrp))) )
	  error("Elements of 'fac' must be >=0 and < 'nrgrp'.");


  /* check input argument na_rm */
  if (!isLogical(_na_rm) || length(_na_rm) != 1 || LOGICAL(_na_rm)[0] == R_NaInt)
      error("'na.rm' must be TRUE or FALSE");
  na_rm = LOGICAL(_na_rm)[0];

  PROTECT(statistic = allocVector(REALSXP, nt));
  PROTECT(dm        = allocVector(REALSXP, nt));
  PROTECT(df        = allocVector(REALSXP, nt));

  /* Do it */
  rowcolttests_c(
      x, fac, nr, nc, no, nt, which, nrgrp, na_rm,
      REAL(statistic), REAL(dm), REAL(df));

  /* return value: a list with two elements, statistic and df */
  PROTECT(res = allocVector(VECSXP, 3));
  SET_VECTOR_ELT(res, 0, statistic);
  SET_VECTOR_ELT(res, 1, dm);
  SET_VECTOR_ELT(res, 2, df);

  PROTECT(namesres = allocVector(STRSXP, 3));
  SET_STRING_ELT(namesres, 0, mkChar("statistic"));
  SET_STRING_ELT(namesres, 1, mkChar("dm"));
  SET_STRING_ELT(namesres, 2, mkChar("df"));
  setAttrib(res, R_NamesSymbol, namesres);

  UNPROTECT(5); /* done with res, namesres, statistic, dm, df */
  return(res);
}

