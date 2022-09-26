#include <R.h>
#include <Rinternals.h>
#include <stdlib.h> // for NULL
#include <R_ext/Rdynload.h>

/* .C calls */
extern void imodwt(void *, void *, void *, void *, void *, void *, void *, void *);
extern void modwt(void *, void *, void *, void *, void *, void *, void *, void *);
extern void DescendMin(void *, void *, void *, void *, void *);
extern void FindEqualGreaterM(void *, void *, void *, void *, void *);
extern void RectUnique(void *, void *, void *, void *, void *, void *, void *);


/* .Call calls */
extern SEXP do_decorana(SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP);
extern SEXP rowcolttests(SEXP, SEXP, SEXP, SEXP, SEXP);

static const R_CMethodDef CEntries[] = {
  {"imodwt",         (DL_FUNC) &imodwt,          8},
  {"modwt",          (DL_FUNC) &modwt,           8},
  {"DescendMin",          (DL_FUNC) &DescendMin,           5},
  {"FindEqualGreaterM",          (DL_FUNC) &FindEqualGreaterM,           5},
  {"RectUnique",          (DL_FUNC) &RectUnique,           7},
  {NULL, NULL, 0}
};

static const R_CallMethodDef CallEntries[] = {
    {"do_decorana",  (DL_FUNC) &do_decorana,  7},
    {"rowcolttests",          (DL_FUNC) &rowcolttests,           5},
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
