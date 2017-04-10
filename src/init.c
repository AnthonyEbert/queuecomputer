#include <R.h>
#include <Rinternals.h>
#include <stdlib.h> // for NULL
#include <R_ext/Rdynload.h>

/* FIXME:
 Check these declarations against the C/Fortran source code.
 */

/* .Call calls */
extern SEXP queuecomputer_qloop_numeric(SEXP, SEXP, SEXP);
extern SEXP queuecomputer_qloop_qq(SEXP, SEXP, SEXP, SEXP);

static const R_CallMethodDef CallEntries[] = {
  {"queuecomputer_qloop_numeric", (DL_FUNC) &queuecomputer_qloop_numeric, 3},
  {"queuecomputer_qloop_qq",      (DL_FUNC) &queuecomputer_qloop_qq,      4},
  {NULL, NULL, 0}
};

void R_init_queuecomputer(DllInfo *dll)
{
  R_registerRoutines(dll, NULL, CallEntries, NULL, NULL);
  R_useDynamicSymbols(dll, FALSE);
}
