#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include <R_ext/Print.h>

#define STEPREDN 0.2 
#define ACCTOL 0.0001 
#define RELTEST	10
#define INTOL 1.490116e-08
#define ABSTOL -1E+37
#define MAXIT 100
#define MAXCOL 1000

/*
//objective function A n x n
double objf(int n, double *x, double *A, double *b) 
{
  int i, k;
  double c[n];

  //crossprod(b - A%*%x) ##objective function
  for(i = 0; i < n; i++)
    {
      c[i] = -b[i];
      for(k = 0; k < n; k++)
	{	  
	  c[i] += A[i + k*n]*x[k]; //Convert row-column notation into vector notation
	}
      if(i == 0)
	c[0] = c[0]*c[0];
      else
	c[0] += c[i]*c[i];      
    }
  return(c[0]);
}
*/

//objective function A m x n
double objf(int n, int m, double *x, double *A, double *b) 
{
  int i, k;
  double c[m];

  //crossprod(b - A%*%x) ##objective function
  for(i = 0; i < m; i++)
    {
      c[i] = -b[i];
      for(k = 0; k < n; k++)
	{	  
	  c[i] += A[i + k*m]*x[k]; //Convert row-column notation into vector notation
	}
      if(i == 0)
	c[0] = c[0]*c[0];
      else
	c[0] += c[i]*c[i];      
    }
  return(c[0]);
}

/*
//gradient A n x n
void grad(int n, double *x, double *A, double *b, double *g) 
{
  int i, k;
  double c[n];

  //2*crossprod(A, A%*%x -b) ##gradient function 
  //Ax - b
  for(i = 0; i < n; i++)
    {
      c[i] = -b[i];
      for(k = 0; k < n; k++)
	{	  
	  c[i] += A[i + k*n]*x[k]; //Convert row-column notation into vector notation
	}
    }
  //AT(Ax-b)
  for(i = 0; i < n; i++)
    {
      g[i] = 0;
      for(k = 0; k < n; k++)
	{
	  g[i] += A[k + i*n]*c[k]; //Convert row-column notation into vector notation	
	}
      g[i] = 2*g[i];
    } 
}
*/

//gradient A m x n
void grad(int n, int m, double *x, double *A, double *b, double *g) 
{
  int i, k;
  double c[m];

  //2*crossprod(A, A%*%x -b) ##gradient function 
  //Ax - b
  for(i = 0; i < m; i++)
    {
      c[i] = -b[i];
      for(k = 0; k < n; k++)
	{	  
	  c[i] += A[i + k*m]*x[k]; //Convert row-column notation into vector notation
	}
    }
  //AT(Ax-b)
  for(i = 0; i < m; i++)
    {
      g[i] = 0;
      for(k = 0; k < n; k++)
	{
	  g[i] += A[k + i*m]*c[k]; //Convert row-column notation into vector notation	
	}
      g[i] = 2*g[i];
    } 
}

//TODO: adjust algorithm for handling A rectangular
//R's Conjugate Gradients algorithm
//Date: 26-09-2012
//Modified by M. van Iterson
void nncgc(int *Rn, double *x0, double *A, double *b, double *fmin, int *fail, int *Rtype, int *Rtrace, int *objfcount, int *gradcount)
{
  int accpoint, count, cycle, cyclimit, n = *Rn, m = *Rn;
  int fcount=0, gcount=0, i;
  int type = *Rtype;
  int trace = *Rtrace;
  double f, G1, G2, G3, gradproj;
  double newstep, oldstep, setstep, steplength=1.0;
  double tol;
  double c[n], g[n], t[n], x[n], xnn[n];
  int negative = 0;

  if (MAXIT <= 0) {
    *fmin = objf(n, m, x0, A, b);
    *objfcount = *gradcount = 0;
    *fail = 0;
    return;
  }
  if (trace) {
    Rprintf("  Conjugate gradients function minimizer\n");
    switch (type) {
    case 1:	    Rprintf("Method: Fletcher Reeves\n");	break;
    case 2:	    Rprintf("Method: Polak Ribiere\n");		break;
    case 3:	    Rprintf("Method: Beale Sorenson\n");	break;
    default:
      Rprintf("unknown 'type' in CG method of optim");
    }
  }

  setstep = 1.7;
  *fail = 0;
  cyclimit = n;
  tol = INTOL * n * sqrt(INTOL);

  if (trace) Rprintf("tolerance used in gradient test=%g\n", tol);

  for (i = 0; i < n; i++) 
    xnn[i] = x0[i];

  f = objf(n, m, x0, A, b);

  *fmin = f;
  fcount = 1;
  gcount = 0;
  do { //Outer iterations    
    for (i = 0; i < n; i++) {
      t[i] = 0.0;
      c[i] = 0.0;      
    }         
   
    cycle = 0;
    oldstep = 1.0;
    count = 0;
    do {   
      cycle++;
      if (trace) {
	Rprintf("%d %d %f\n", gcount, fcount, *fmin);
	/* Rprintf("parameters "); 
	   for (i = 1; i <= n; i++) { 
	   Rprintf("%10.5f ", x0[i - 1]); 
	   if (i / 7 * 7 == i && i < n) 
	   Rprintf("\n"); 
	   } 
	   Rprintf("\n"); */
      }
      gcount++;    
    
      //Hack to terminate once negative values are introduced in the solution vector    
      negative = 0;  
      for(i = 0; i < n; i++) { 	
	if(x0[i] < 0)	
	  negative = 1;
      }
      if(negative == 1)
	{
	  for(i = 0; i < n; i++) //restore valid solution 	 
	    x0[i] = xnn[i]; 
	  if (trace) {
	    Rprintf("Exiting from conjugate gradients minimizer\n");
	    Rprintf("    %d function evaluations used\n", fcount);
	    Rprintf("    %d gradient evaluations used\n", gcount);
	  }
	  return;
	}
      else
	{
	  for(i = 0; i < n; i++)  //store valid solution 	 
	    xnn[i] = x0[i];
	} 
 
      if (gcount > MAXIT) {
	*objfcount = fcount;
	*gradcount = gcount;
	*fail = 1;	       	 
	Rprintf("MAXIT reached. No non-negative solution possible!");
	return;
      }
      grad(n, m, x0, A, b, g);
      G1 = 0.0;
      G2 = 0.0;      

      for (i = 0; i < n; i++) {
	x[i] = x0[i];
	switch (type) {

	case 1: /* Fletcher-Reeves */
	  G1 += g[i] * g[i];
	  G2 += c[i] * c[i];
	  break;

	case 2: /* Polak-Ribiere */
	  G1 += g[i] * (g[i] - c[i]);
	  G2 += c[i] * c[i];
	  break;

	case 3: /* Beale-Sorenson */
	  G1 += g[i] * (g[i] - c[i]);
	  G2 += t[i] * (g[i] - c[i]);
	  break;

	default:
	  Rprintf("unknown type in CG method of optim");
	}	
	c[i] = g[i];       
      }

      if (G1 > tol) {

	if (G2 > 0.0)
	  G3 = G1 / G2;
	else
	  G3 = 1.0;
	gradproj = 0.0;
	for (i = 0; i < n; i++) {
	  t[i] = t[i] * G3 - g[i];
	  gradproj += t[i] * g[i];
	}
	steplength = oldstep;  		        
	accpoint = 0;
	do {
	  count = 0;

	  for (i = 0; i < n; i++) {
	    x0[i] = x[i] + steplength * t[i]; //update solution

	    if (RELTEST + x[i] == RELTEST + x0[i])
	      count++;
	  }
	  if (count < n) { /* point is different */
	    f = objf(n, m, x0, A, b);
	    fcount++;
	    accpoint = (f <= *fmin + gradproj * steplength * ACCTOL);	

	    if (!accpoint) {
	      steplength *= STEPREDN;
	      if (trace) Rprintf("*");
	    } else *fmin = f; /* we improved, so update value */
	  }
        } while (!(count == n || accpoint));
	if (count < n) {
	  newstep = 2 * (f - *fmin - gradproj * steplength);
	  if (newstep > 0) {
	    newstep = -(gradproj * steplength * steplength / newstep);

	    for (i = 0; i < n; i++)
	      x0[i] = x[i] + newstep * t[i];  //update solution

	    *fmin = f;
	    f = objf(n, m, x0, A, b);
	    fcount++;
	    if (f < *fmin) {
	      *fmin = f;
	      if (trace) Rprintf(" i< ");
	    } else { /* reset x0 to match lowest point */
	      if (trace) Rprintf(" i> ");

	      for (i = 0; i < n; i++)
		x0[i] = x[i] + steplength * t[i];  //update solution
	    }
	  }
	}
      }
      oldstep = setstep * steplength;
      if (oldstep > 1.0)
	oldstep = 1.0;

    } while ((count != n) && (G1 > tol) && (cycle != cyclimit));  
    
  } while ((cycle != 1) || ((count != n) && (G1 > tol) && *fmin > ABSTOL));

  if (trace) {
    Rprintf("Exiting from conjugate gradients minimizer\n");
    Rprintf("    %d function evaluations used\n", fcount);
    Rprintf("    %d gradient evaluations used\n", gcount);
  }
  *objfcount = fcount;
  *gradcount = gcount;
}

