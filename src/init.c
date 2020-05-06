#include <R.h>
#include <Rinternals.h>
#include <stdlib.h> // for NULL
#include <R_ext/Rdynload.h>

/* .C calls */
extern void imodwt(void *, void *, void *, void *, void *, void *, void *, void *);
extern void modwt(void *, void *, void *, void *, void *, void *, void *, void *);
extern void continuousPtsAboveThreshold(void *, void *, void *, void *, void *, void *);
extern void continuousPtsAboveThresholdIdx(void *, void *, void *, void *, void *, void *);
extern void DescendMin(void *, void *, void *, void *, void *);
extern void FindEqualGreaterM(void *, void *, void *, void *, void *);
extern void RectUnique(void *, void *, void *, void *, void *, void *, void *);
extern void WhichColMax(void *, void *, void *, void *);
extern void DescendZero(void *, void *, void *, void *, void *);
extern void ColMax(void *, void *, void *, void *);

/* .Call calls */
extern SEXP do_decorana(SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP);
extern SEXP findmzROI(SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP);
extern SEXP getMZ(SEXP, SEXP, SEXP, SEXP, SEXP, SEXP);
extern SEXP getEIC(SEXP, SEXP, SEXP, SEXP, SEXP, SEXP);
extern SEXP binYonX(SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP);
extern SEXP binYonX_multi(SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP);
extern SEXP impute_with_linear_interpolation(SEXP, SEXP);
extern SEXP impute_with_linear_interpolation_base(SEXP, SEXP, SEXP);
extern SEXP breaks_on_nBins(SEXP, SEXP, SEXP, SEXP);

static const R_CMethodDef CEntries[] = {
  {"imodwt",         (DL_FUNC) &imodwt,          8},
  {"modwt",          (DL_FUNC) &modwt,           8},
  {"continuousPtsAboveThreshold",          (DL_FUNC) &continuousPtsAboveThreshold,           6},
  {"continuousPtsAboveThresholdIdx",          (DL_FUNC) &continuousPtsAboveThresholdIdx,           6},
  {"DescendMin",          (DL_FUNC) &DescendMin,           5},
  {"FindEqualGreaterM",          (DL_FUNC) &FindEqualGreaterM,           5},
  {"RectUnique",          (DL_FUNC) &RectUnique,           7},
  {"WhichColMax",          (DL_FUNC) &WhichColMax,           4},
  {"DescendZero",          (DL_FUNC) &DescendZero,           5},
  {"ColMax",          (DL_FUNC) &ColMax,           4},
  {NULL, NULL, 0}
};

static const R_CallMethodDef CallEntries[] = {
    {"do_decorana",  (DL_FUNC) &do_decorana,  7},
    {"findmzROI",  (DL_FUNC) &findmzROI,  10},
    {"getMZ",  (DL_FUNC) &getMZ,  6},
    {"getEIC",  (DL_FUNC) &getEIC,  6},
    {"binYonX",  (DL_FUNC) &binYonX,  14},
    {"binYonX_multi",  (DL_FUNC) &binYonX_multi,  14},
    {"impute_with_linear_interpolation",  (DL_FUNC) &impute_with_linear_interpolation,  2},
    {"impute_with_linear_interpolation_base",  (DL_FUNC) &impute_with_linear_interpolation_base,  3},
    {"breaks_on_nBins",  (DL_FUNC) &breaks_on_nBins,  4},
    {NULL, NULL, 0}
};

 void R_init_MetaboAnalystR(DllInfo *dll)
 {
   R_registerRoutines(dll, CEntries, CallEntries,NULL,NULL);
   R_useDynamicSymbols(dll, FALSE);
 }

//void R_init_MetaboAnalystR(DllInfo *info) {
//  R_RegisterCCallable("MetaboAnalystR", "add",  (DL_FUNC) &CEntries);
//}
