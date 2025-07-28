#include <stdio.h>
#include <stdlib.h>
#include <stddef.h>
#include <string.h>
#include <math.h>
#include <R.h>
#include <Rinternals.h>
#include "R.h"
#include "Rdefines.h"

#undef TRUE
#define TRUE    1
#undef FALSE
#define FALSE    0

//#define DEBUG

#define ROI_INIT_LENGTH  1000
#define MZVAL_INIT_LENGTH 1000
#define ROI_ALLOC_INC 1.5         // allocation increment
#define MZVAL_ALLOC_INC 1.5       // allocation increment
#define N_NAMES 7

struct scanStruct
{
   double  mz;
   double  intensity;
};

struct scanBuf
{
  struct scanStruct * thisScan;
  double * nextScan;
  unsigned int thisScanLength;
  unsigned int nextScanLength;
} scbuf;

struct pickOptionsStruct
{
   unsigned int  minEntries;
   unsigned int  minimumInt;
   unsigned int  minimumIntValues;
   float dev;
} pickOptions;


struct mzROIStruct {
  double  mz;
  double  mzmin;
  double  mzmax;
  unsigned int scmin;
  unsigned int scmax;
  unsigned int intensity;
  unsigned int length;
  unsigned int kI;
  unsigned char deleteMe;
};

struct mzLengthStruct {
  unsigned int mzval;
  unsigned int mzvalTotal;
  unsigned int mzROI;
  unsigned int mzROITotal;
} mzLength;


struct mzROIStruct * checkmzROIBufSize(struct mzROIStruct *mzROI, const unsigned int newmzROILength, struct mzLengthStruct *mzLength){
  unsigned int newLength=0;

  if (newmzROILength > mzLength->mzROITotal) {
    newLength= mzLength->mzROITotal * ROI_ALLOC_INC;

    if (newmzROILength > newLength)
        newLength= newmzROILength;

#ifdef DEBUG
    Rprintf("realloc mzROI \n");
#endif

    mzROI = (struct mzROIStruct *) realloc(mzROI, newLength * sizeof(struct mzROIStruct));
    if (mzROI == NULL)
        error("findmzROI/realloc: buffer memory could not be allocated !\n");

    mzLength->mzROITotal = newLength;
  }
  return(mzROI);
}


struct mzROIStruct * checkmzvalBufSize(struct mzROIStruct *mzval, const unsigned int newmzvalLength, struct mzLengthStruct *mzLength) {
  unsigned int newLength=0;

  if (newmzvalLength > mzLength->mzvalTotal) {
     newLength= mzLength->mzvalTotal * MZVAL_ALLOC_INC;

    if (newmzvalLength >  newLength)
       newLength= newmzvalLength;

#ifdef DEBUG
       Rprintf("realloc mzval \n");
#endif

    mzval = (struct mzROIStruct *) realloc(mzval, newLength * sizeof(struct mzROIStruct));
    if (mzval == NULL)
      error("findmzROI/realloc: buffer memory could not be allocated !\n");

    mzLength->mzvalTotal = newLength;
  }
  return(mzval);
}

int lower_bound(double val,struct mzROIStruct *mzval,int first, int length){
int half,mid;  // mzval->mz[first]
  while (length > 0) {
    half = length >> 1;
    mid = first;
    mid += half;
    if ( mzval[mid].mz < val){
      first = mid;
      first ++;
      length = length - half -1;
    }
    else length = half;
  }
  return(first);
}

int upper_bound(double val,struct mzROIStruct *mzval,int first, int length){
int half,mid;
  while (length > 0) {
    half = length >> 1;
    mid = first;
    mid += half;
    if (val < mzval[mid].mz){
      length = half;
    }
    else {
      first = mid;
      first ++;
      length = length - half -1;
    }
  }
  return(first);
}

int lowerBound(double val,double *mzval,int first, int length){
int half,mid;
  while (length > 0) {
    half = length >> 1;
    mid = first;
    mid += half;
    if ( mzval[mid] < val){
      first = mid;
      first ++;
      length = length - half -1;
    }
    else length = half;
  }
  return(first);
}

int upperBound(double val,double *mzval,int first, int length){
int half,mid;
  while (length > 0) {
    half = length >> 1;
    mid = first;
    mid += half;
    if (val < mzval[mid]){
      length = half;
    }
    else {
      first = mid;
      first ++;
      length = length - half -1;
    }
  }
  return(first);
}

// Passes the m/z of an input spectrum (fMass) and checks if that m/z is
// close enough to an existing mzROI to enable inclusion (depending on the
// user defined ppm: difference mean m/z of ROI to fMass <= ppm * fMass / 1e6)
// otherwise a new mzROI is defined for fMass.
struct mzROIStruct * insertpeak(const double fMass, const double fInten,
				struct scanBuf * scanbuf, const int scan,
				const int LastScan, struct mzROIStruct *mzval,
				struct mzLengthStruct *mzLength,
				struct pickOptionsStruct *pickOptions)
{
  int i,wasfound = FALSE;
  double ddev = (pickOptions->dev * fMass);
  int lpos = lower_bound( fMass - ddev,mzval,0,mzLength->mzval);
  int hpos = upper_bound( fMass + ddev,mzval,lpos,mzLength->mzval - lpos);
  
  if (lpos >  mzLength->mzval-1)
      lpos = mzLength->mzval -1;
  if (hpos >  mzLength->mzval-1)
      hpos = mzLength->mzval -1 ;

  // loop through mz ROIs for which the m/z could be close to fMass
  for (i = lpos; i <= hpos; i++)
  {
    // check difference between fMass and the mz of the current ROI
    double ddiff = fabs(mzval[i].mz - fMass);

    if (ddiff <= ddev)
    { // match (smaller than defined ppm) -> extend this ROI
          if ( (i > hpos) || (i<lpos) ) error("! scan: %d \n",scan);
          wasfound = TRUE;
          //recursive m/z mean update
          mzval[i].mz = ((mzval[i].length * mzval[i].mz) + fMass) / (mzval[i].length + 1);
          if (fMass < mzval[i].mzmin)
              mzval[i].mzmin = fMass;
          if (fMass > mzval[i].mzmax)
              mzval[i].mzmax = fMass;
          mzval[i].scmax = scan;
          mzval[i].length++;
          mzval[i].intensity+=fInten;
          if (fInten >= pickOptions->minimumInt)
              mzval[i].kI++;
    }
  } // for
  
  // if not found
  if (wasfound == FALSE) {  // no, create new ROI for mz
    
    lpos=-1;hpos=-1;
    int doInsert=FALSE;
    if ((scan < LastScan) && (scanbuf->nextScanLength > 0)) {// check next scan
      int lpos = lowerBound( fMass - ddev,scanbuf->nextScan,0,scanbuf->nextScanLength);
      int hpos = upperBound( fMass + ddev,scanbuf->nextScan,lpos,scanbuf->nextScanLength - lpos);
      if (lpos < scanbuf->nextScanLength) {
          for (i=lpos; i <= hpos; i++) //
	    {
	      ddev = (pickOptions->dev *  scanbuf->nextScan[i]);
	      double ddiff = fabs(fMass - scanbuf->nextScan[i]);
	      
	      if (ddiff <= ddev)
		{
		  doInsert=TRUE;
		  break;
		}
	    }
      }
    } else
      doInsert=TRUE;
    
    if (doInsert == TRUE) {
      // get pos. for insert
      int i = lower_bound(fMass,mzval,0,mzLength->mzval);
      // check buffer size
      mzval=checkmzvalBufSize(mzval, mzLength->mzval + 1, mzLength);
      // elements to move
      int n = mzLength->mzval - i;
      // insert element
      if (n>0)
	memmove(mzval + i +1, mzval + i, n*sizeof(struct mzROIStruct));
      
      mzval[i].mz = fMass;
      mzval[i].mzmin = fMass;
      mzval[i].mzmax = fMass;
      mzval[i].intensity = fInten;
      mzval[i].scmin = scan;
      mzval[i].scmax = scan;
      mzval[i].length = 1;
      if (fInten >= pickOptions->minimumInt)
	mzval[i].kI = 1; else
	mzval[i].kI = 0;
      mzval[i].deleteMe = FALSE;
      
      mzLength->mzval++;
    }
  }
  
  return(mzval);
}

struct mzROIStruct * cleanup(const int ctScan, struct mzROIStruct *mzROI, struct mzROIStruct *mzval, struct mzLengthStruct *mzLength, int *scerr, struct pickOptionsStruct *pickOptions){
int i,p,del=0;

 // check all peaks in mzval
 for (i=0; i < mzLength->mzval; i++) {
    unsigned int lastscan=mzval[i].scmax;
    unsigned int entries=mzval[i].length;

    // finished (entries >= minEntries)  or just extended
    if ((entries >= pickOptions->minEntries) || (lastscan == ctScan)) // good feature
    { // is it finished ?
      if ((entries >= pickOptions->minEntries) && (lastscan < ctScan)) { //it's is not extended anymore
        if (mzval[i].kI >= pickOptions->minimumIntValues) {
             // copy values to set of completed ROI's
             p=mzLength->mzROI;
             mzROI=checkmzROIBufSize(mzROI, p+1 , mzLength);
             mzROI[p].mz = mzval[i].mz;
             mzROI[p].mzmin = mzval[i].mzmin;
             mzROI[p].mzmax = mzval[i].mzmax;
             mzROI[p].scmin = mzval[i].scmin;
             mzROI[p].scmax = mzval[i].scmax;
             mzROI[p].length =  mzval[i].length;
             mzROI[p].kI =  mzval[i].kI;
             mzROI[p].intensity =  mzval[i].intensity;

             mzLength->mzROI++;
             mzval[i].deleteMe=TRUE;
             del++;
            }
        else {
             mzval[i].deleteMe=TRUE;
             del++;
             }
      }
      else {
        //continue
      }
        if (entries > ctScan) {
            #ifdef DEBUG
                error("Warning : entries > ctScan (is this centroid data ?) i: %d m: %3.4f  %d entries, lastscan %d   (ctScan=%d)\n",i,mzval[i].mz,mzval[i].length,lastscan,ctScan);
            #endif
          (*scerr)++;
        }
    }
    else
    {
        mzval[i].deleteMe=TRUE;
        del++;
    }

 } // for i

 if (del > 0) {
    p=0;
    struct mzROIStruct * tmp = (struct mzROIStruct *) calloc(mzLength->mzval - del,  sizeof(struct mzROIStruct));
    if (tmp == NULL)
      error("findmzROI/cleanup: buffer memory could not be allocated !\n");
    for (i=0; i < mzLength->mzval; i++) {
        if (mzval[i].deleteMe == FALSE) {
            tmp[p].mz = mzval[i].mz;
            tmp[p].mzmin = mzval[i].mzmin;
            tmp[p].mzmax = mzval[i].mzmax;
            tmp[p].scmin = mzval[i].scmin;
            tmp[p].scmax = mzval[i].scmax;
            tmp[p].length =  mzval[i].length;
            tmp[p].kI =  mzval[i].kI;
            tmp[p].intensity =  mzval[i].intensity;
            tmp[p].deleteMe = FALSE;

            p++;
        }
    }
    for (i=0; i < p; i++) {
        mzval[i].mz = tmp[i].mz;
        mzval[i].mzmin =  tmp[i].mzmin;
        mzval[i].mzmax =  tmp[i].mzmax;
        mzval[i].scmin =  tmp[i].scmin;
        mzval[i].scmax =  tmp[i].scmax;
        mzval[i].length = tmp[i].length;
        mzval[i].kI =     tmp[i].kI;
        mzval[i].intensity = tmp[i].intensity;
        mzval[i].deleteMe = FALSE;
    }

    mzLength->mzval = p;
    free(tmp);
 }

 return(mzROI);
}

struct scanBuf * getScan(int scan, double *pmz, double *pintensity, int *pscanindex,int nmz, int lastScan, struct scanBuf *scanbuf) {
    int idx,idx1,idx2,i=0,N=0;
    idx1 =  pscanindex[scan -1] +1;

    if (scanbuf->thisScan != NULL)
        free(scanbuf->thisScan);

    if (scan == lastScan)
        idx2 =  nmz-1;  else
             idx2 =  pscanindex[scan];

    N=idx2 - idx1 + 1;
    if (N > 0) {
        scanbuf->thisScan= (struct scanStruct  *) calloc(N, sizeof(struct scanStruct));
        // scanbuf->thisScan= (struct scanStruct  *) malloc(N * sizeof(struct scanStruct));
        if (scanbuf->thisScan == NULL)
            error("findmzROI/getThisScan: Memory could not be allocated!\n");

        scanbuf->thisScanLength=N;

        for (idx=idx1;idx <= idx2; idx++)
        {
          scanbuf->thisScan[i].mz       = pmz[idx-1];
          scanbuf->thisScan[i].intensity = pintensity[idx-1];
          i++;
        }
    } else
    {
        scanbuf->thisScan = NULL;
        scanbuf->thisScanLength= 0;
    }

  //  also get  the m/z values of the following scan
    if (scan < lastScan) {
        scan++;
        i=0;N=0;
        idx1 =  pscanindex[scan -1] +1;
        if ((scanbuf->nextScan != NULL))
            free(scanbuf->nextScan);

        if (scan == lastScan)
            idx2 =  nmz-1;   else
                idx2 =  pscanindex[scan];

        N=idx2 - idx1 + 1;
        if (N > 0) {
            scanbuf->nextScan= (double *) calloc(N, sizeof(double));
            if (scanbuf->nextScan == NULL)
                error("findmzROI/getNextScan: Memory could not be allocated !\n");
            scanbuf->nextScanLength=N;

            for (idx=idx1;idx <= idx2; idx++)
            {
                scanbuf->nextScan[i]      = pmz[idx-1];
                i++;
            }
        } else
        {
            scanbuf->nextScan = NULL;
            scanbuf->nextScanLength= 0;
        }
    }
  return(scanbuf);
}

double getScanEIC(int scan, double from, double to, double *pmz, double *pintensity, int *pscanindex,int nmz, int lastScan) {
  int idx,idx1,idx2;
  double sum=0.0;

  idx1 =  pscanindex[scan -1] +1;
  if (scan == lastScan)  idx2 =  nmz-1;
    else idx2 =  pscanindex[scan];


  int idx1b = lowerBound(from, pmz, idx1-1, idx2-idx1);
  int idx2b = upperBound(to, pmz, idx1b, idx2-idx1b);

  for (idx=idx1b;idx <= idx2b; idx++)
  {
    double mzval = pmz[idx-1];
    if ((mzval <= to) && (mzval >= from)) sum += pintensity[idx-1];
  }
  return(sum);
}

SEXP getEIC(SEXP mz, SEXP intensity, SEXP scanindex, SEXP mzrange, SEXP scanrange, SEXP lastscan) {
  double *pmz, *pintensity,*p_vint, mzrangeFrom,mzrangeTo;
  int i,*pscanindex, *p_scan,scanrangeFrom, scanrangeTo,ilastScan,nmz,ctScan,buflength;
  SEXP list_names,reslist,vscan,vint;
  pmz = REAL(mz);
  nmz = GET_LENGTH(mz);
  pintensity = REAL(intensity);
  pscanindex = INTEGER(scanindex);
  int firstScan = 1;   // is always 1
  ilastScan = INTEGER(lastscan)[0];
  mzrangeFrom = REAL(mzrange)[0];
  mzrangeTo =  REAL(mzrange)[1];
  scanrangeFrom = INTEGER(scanrange)[0];
  scanrangeTo = INTEGER(scanrange)[1];
  if ((scanrangeFrom <  firstScan) || (scanrangeFrom > ilastScan) || (scanrangeTo < firstScan) || (scanrangeTo > ilastScan))
     error("Error in scanrange \n");
  char *names[2] = {"scan", "intensity"};
  PROTECT(list_names = allocVector(STRSXP, 2));
  for(i = 0; i < 2; i++)
    SET_STRING_ELT(list_names, i,  mkChar(names[i]));

  buflength = scanrangeTo - scanrangeFrom +1;
  PROTECT(reslist = allocVector(VECSXP, 2));
  PROTECT(vscan = NEW_INTEGER(buflength));
  p_scan = INTEGER_POINTER(vscan);
  PROTECT(vint = NEW_NUMERIC(buflength));
  p_vint = NUMERIC_POINTER(vint);

  i=0;
  for (ctScan=scanrangeFrom;ctScan<=scanrangeTo;ctScan++)
  {
    p_scan[i] = ctScan;
    p_vint[i] = getScanEIC(ctScan,mzrangeFrom,mzrangeTo,pmz, pintensity, pscanindex,nmz,ilastScan);
    i++;
  }

  SET_VECTOR_ELT(reslist, 0, vscan);// attaching integer vector scan to list
  SET_VECTOR_ELT(reslist, 1, vint); // attaching double vector m/z to list
  setAttrib(reslist, R_NamesSymbol, list_names); //and attaching the vector names

  UNPROTECT(4);
  return(reslist);
}

SEXP getMZ(SEXP mz, SEXP intensity, SEXP scanindex, SEXP mzrange, SEXP scanrange, SEXP lastscan) {
  //jo double *pmz, *pintensity,*p_res, mzrangeFrom,mzrangeTo;
  double *pmz, *p_res, mzrangeFrom,mzrangeTo;
  int i,*pscanindex,scanrangeFrom, scanrangeTo,ilastScan,nmz,ctScan,buflength;
  SEXP res;
  pmz = REAL(mz);
  nmz = GET_LENGTH(mz);
  //jo pintensity = REAL(intensity);
  pscanindex = INTEGER(scanindex);
  int firstScan = 1;   // is always 1
  ilastScan = INTEGER(lastscan)[0];
  mzrangeFrom = REAL(mzrange)[0];
  mzrangeTo =  REAL(mzrange)[1];
  scanrangeFrom = INTEGER(scanrange)[0];
  scanrangeTo = INTEGER(scanrange)[1];
  if ((scanrangeFrom <  firstScan) || (scanrangeFrom > ilastScan) || (scanrangeTo < firstScan) || (scanrangeTo > ilastScan))
     error("Error in scanrange \n");

  buflength = scanrangeTo - scanrangeFrom +1;
  PROTECT(res = NEW_NUMERIC(buflength));
  p_res = NUMERIC_POINTER(res);

  i=0;
  for (ctScan=scanrangeFrom;ctScan<=scanrangeTo;ctScan++)
  {
        int idx,idx1,idx2;
        idx1 =  pscanindex[ctScan -1] +1;
        if (ctScan == ilastScan)  idx2 =  nmz-1;
            else idx2 =  pscanindex[ctScan];
        int idx1b = lowerBound(mzrangeFrom, pmz, idx1-1, idx2-idx1-1);
        int idx2b = upperBound(mzrangeTo, pmz, idx1b, idx2-idx1b-1);

        int pc=0;
        p_res[i]=0;
        for (idx=idx1b;idx <= idx2b; idx++)
        {
            double mzval = pmz[idx];
            if ((mzval <= mzrangeTo) && (mzval >= mzrangeFrom)) {
               if (pc==0)
                 p_res[i] = mzval;
               else
                 p_res[i] = ((pc * p_res[i]) + mzval) / (pc+1);

               pc++;
            }
        }
    i++;
  }

  UNPROTECT(1);
  return(res);
}

SEXP findmzROI(SEXP mz, SEXP intensity, SEXP scanindex, SEXP mzrange,
	       SEXP scanrange, SEXP lastscan, SEXP dev, SEXP minEntries,
	       SEXP prefilter, SEXP noise) {
  //jo double *pmz, *pintensity, mzrangeFrom,mzrangeTo;
  double *pmz, *pintensity;
  int i,*pscanindex, scanrangeFrom, scanrangeTo, ctScan, nmz, lastScan, inoise;
  int scerr = 0;  // count of peak insertion errors, due to missing/bad centroidisation
  int perc, lp = -1;
  SEXP peaklist,entrylist,list_names,vmz,vmzmin,vmzmax,vscmin,vscmax,vlength,vintensity;

  pmz = REAL(mz);
  nmz = GET_LENGTH(mz);
  pintensity = REAL(intensity);
  pscanindex = INTEGER(scanindex);
  lastScan = INTEGER(lastscan)[0];
  inoise = INTEGER(noise)[0];

  pickOptions.dev = REAL(dev)[0];
  pickOptions.minEntries = INTEGER(minEntries)[0];
  pickOptions.minimumIntValues=INTEGER(prefilter)[0];
  pickOptions.minimumInt=INTEGER(prefilter)[1];

  //jo mzrangeFrom = REAL(mzrange)[0];
  //jo mzrangeTo =  REAL(mzrange)[1];
  scanrangeFrom = INTEGER(scanrange)[0];
  scanrangeTo = INTEGER(scanrange)[1];

  struct mzROIStruct * mzROI = (struct mzROIStruct *) calloc(ROI_INIT_LENGTH,  sizeof(struct mzROIStruct));
  if (mzROI == NULL)
      error("findmzROI/calloc: buffer memory could not be allocated !\n");

  struct mzROIStruct * mzval = (struct mzROIStruct *) calloc(MZVAL_INIT_LENGTH,  sizeof(struct mzROIStruct));
  if (mzval == NULL)
      error("findmzROI/calloc: buffer memory could not be allocated !\n");

  mzLength.mzvalTotal = MZVAL_INIT_LENGTH;
  mzLength.mzROITotal = ROI_INIT_LENGTH;
  mzLength.mzval = 0;
  mzLength.mzROI = 0;

  struct scanBuf * scanbuf = &scbuf;
  scanbuf->thisScan = NULL;
  scanbuf->nextScan = NULL;
  scanbuf->thisScanLength = 0;
  scanbuf->nextScanLength = 0;

  char *names[N_NAMES] = {"mz", "mzmin", "mzmax", "scmin", "scmax", "length", "intensity"};
  PROTECT(list_names = allocVector(STRSXP, N_NAMES));
  for(i = 0; i < N_NAMES; i++)
    SET_STRING_ELT(list_names, i,  mkChar(names[i]));

  Rprintf(" %% finished: ");
  // loop through scans/spectra
  for (ctScan=scanrangeFrom;ctScan<=scanrangeTo;ctScan++)
  {
     perc = (int) (ctScan* 100)/scanrangeTo;
     if ((perc % 10) == 0 && (perc != lp))
     {
       Rprintf("%d ",perc);
       lp = perc;
     }
    scanbuf=getScan(ctScan, pmz, pintensity, pscanindex,nmz,lastScan, scanbuf);

    if (scanbuf->thisScanLength > 0)
    {
        #ifdef DEBUG
         Rprintf("ScanLength %d ",scanbuf->thisScanLength);
         Rprintf("Scan Nr. %d of %d (%d %%) %d peaks -- working at %d m/z ROI's, %d ROI's completed.\n", ctScan, scanrangeTo,  (int)100.0*ctScan/scanrangeTo,scanbuf->thisScanLength,mzLength.mzval,mzLength.mzROI);
        #endif

      int p;
      double fMass,lastMass=-1;
      double fInten;

      // loop through m/z values of the current scan.
      for (p=0;p < scanbuf->thisScanLength;p++)
        {
          fMass  = scanbuf->thisScan[p].mz;
          fInten = scanbuf->thisScan[p].intensity;

          if (fMass < lastMass)
            error("m/z sort assumption violated ! (scan %d, p %d, current %2.4f (I=%2.2f), last %2.4f) \n",ctScan,p,fMass,fInten,lastMass);
          lastMass = fMass;

          if (fInten > inoise)
            mzval = insertpeak(fMass, fInten, scanbuf, ctScan, scanrangeTo,
			       mzval, &mzLength, &pickOptions);

        }
    }
    mzROI=cleanup(ctScan,mzROI,mzval,&mzLength,&scerr,&pickOptions);
    R_FlushConsole();
  } //for ctScan

  mzROI=cleanup(ctScan+1,mzROI,mzval,&mzLength,&scerr,&pickOptions);

  PROTECT(peaklist = allocVector(VECSXP, mzLength.mzROI));
  int total = 0;
  for (i=0;i<mzLength.mzROI;i++) {
      PROTECT(entrylist = allocVector(VECSXP, N_NAMES));

      PROTECT(vmz = NEW_NUMERIC(1));
      PROTECT(vmzmin = NEW_NUMERIC(1));
      PROTECT(vmzmax = NEW_NUMERIC(1));

      PROTECT(vscmin = NEW_INTEGER(1));
      PROTECT(vscmax = NEW_INTEGER(1));
      PROTECT(vlength = NEW_INTEGER(1));
      PROTECT(vintensity = NEW_INTEGER(1));

      NUMERIC_POINTER(vmz)[0]  = mzROI[i].mz;
      NUMERIC_POINTER(vmzmin)[0] = mzROI[i].mzmin;
      NUMERIC_POINTER(vmzmax)[0] = mzROI[i].mzmax;

      INTEGER_POINTER(vscmin)[0] = mzROI[i].scmin;
      INTEGER_POINTER(vscmax)[0] = mzROI[i].scmax;
      INTEGER_POINTER(vlength)[0] = mzROI[i].length;
      INTEGER_POINTER(vintensity)[0] = mzROI[i].intensity;

      SET_VECTOR_ELT(entrylist, 0, vmz);
      SET_VECTOR_ELT(entrylist, 1, vmzmin);
      SET_VECTOR_ELT(entrylist, 2, vmzmax);
      SET_VECTOR_ELT(entrylist, 3, vscmin);
      SET_VECTOR_ELT(entrylist, 4, vscmax);
      SET_VECTOR_ELT(entrylist, 5, vlength);
      SET_VECTOR_ELT(entrylist, 6, vintensity);

      setAttrib(entrylist, R_NamesSymbol, list_names); //attaching the vector names

      SET_VECTOR_ELT(peaklist, i, entrylist);
      UNPROTECT(N_NAMES + 1); //entrylist + values
      total++;
  }

  if (scerr > 0) Rprintf("Warning: There were %d peak data insertion problems. \n Please try lowering the \"ppm\" parameter.\n", scerr);

  Rprintf("\n %d m/z ROI's.\n", total);

  UNPROTECT(2); // peaklist,list_names

 // free(ptpeakbuf);

  if (scanbuf->thisScan != NULL)
    free(scanbuf->thisScan);
  if (scanbuf->thisScan != NULL)
    free(scanbuf->nextScan);

  free(mzval);
  free(mzROI);

  return(peaklist);
}
