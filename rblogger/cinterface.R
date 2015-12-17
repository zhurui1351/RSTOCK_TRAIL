#http://adv-r.had.co.nz/C-interface.html
require('inline')
require('pryr')
require('Rcpp')
rinternals <- file.path(R.home("include"), "Rinternals.h")
file.show(rinternals)

add <- cfunction(c(a = "integer", b = "integer"), "
  SEXP result = PROTECT(allocVector(REALSXP, 1));
  REAL(result)[0] = asReal(a) + asReal(b);
  UNPROTECT(1);

  return result;
",verbose=T)

add(1,2)

library(pryr)
#查看R变量对应的c类型

sexp_type(10L)
sexp_type("a")
sexp_type(T)
sexp_type(list(a = 1))
sexp_type(pairlist(a = 1))

dummy <- cfunction(body = '
  SEXP dbls = PROTECT(allocVector(REALSXP, 4));
                   SEXP lgls = PROTECT(allocVector(LGLSXP, 4));
                   SEXP ints = PROTECT(allocVector(INTSXP, 4));
                   
                   SEXP vec = PROTECT(allocVector(VECSXP, 3));
                   SET_VECTOR_ELT(vec, 0, dbls);
                   SET_VECTOR_ELT(vec, 1, lgls);
                   SET_VECTOR_ELT(vec, 2, ints);
                   
                   UNPROTECT(4);
                   return vec;
                   ')

dummy()

zeroes <- cfunction(c(n_ = "integer"), '
  int n = asInteger(n_);

  SEXP out = PROTECT(allocVector(INTSXP, n));
  memset(INTEGER(out), 0, n * sizeof(int));
  UNPROTECT(1);

  return out;
')
zeroes(5)

is_na <- cfunction(c(x = "ANY"), '
                   int n = length(x);
                   
                   SEXP out = PROTECT(allocVector(LGLSXP, n));
                   
                   for (int i = 0; i < n; i++) {
                   switch(TYPEOF(x)) {
                   case LGLSXP:
                   LOGICAL(out)[i] = (LOGICAL(x)[i] == NA_LOGICAL);
                   break;
                   case INTSXP:
                   LOGICAL(out)[i] = (INTEGER(x)[i] == NA_INTEGER);
                   break;
                   case REALSXP:
                   LOGICAL(out)[i] = ISNA(REAL(x)[i]);
                   break;
                   case STRSXP:
                   LOGICAL(out)[i] = (STRING_ELT(x, i) == NA_STRING);
                   break;
                   default:
                   LOGICAL(out)[i] = NA_LOGICAL;
                   }
                   }
                   UNPROTECT(1);
                   
                   return out;
')

is_na(c(NA, 1L))
is_na(c(NA, 1))
is_na(c(NA, "a"))
is_na(c(NA, TRUE))

add_one <- cfunction(c(x = "numeric"), "
  int n = length(x);
  SEXP out = PROTECT(allocVector(REALSXP, n));
  
  for (int i = 0; i < n; i++) {
    REAL(out)[i] = REAL(x)[i] + 1;
  }
  UNPROTECT(1);

  return out;
")
add_one(as.numeric(1:10))


add_two <- cfunction(c(x = "numeric"), "
  int n = length(x);
  double *px, *pout;

  SEXP out = PROTECT(allocVector(REALSXP, n));

  px = REAL(x);
  pout = REAL(out);
  for (int i = 0; i < n; i++) {
    pout[i] = px[i] + 2;
  }
  UNPROTECT(1);

  return out;
")
add_two(as.numeric(1:10))


x <- as.numeric(1:1e6)

abc <- cfunction(NULL, '
  SEXP out = PROTECT(allocVector(STRSXP, 3));

  SET_STRING_ELT(out, 0, mkChar("a"));
  SET_STRING_ELT(out, 1, mkChar("b"));
  SET_STRING_ELT(out, 2, mkChar("c"));

  UNPROTECT(1);

  return out;
')
abc()


add_three <- cfunction(c(x = "numeric"), '
  REAL(x)[0] = REAL(x)[0] + 3;
  return x;
')
x <- 1
y <- x
add_three(x)
x
y


add_four <- cfunction(c(x = "numeric"), '
  SEXP x_copy = PROTECT(duplicate(x));
  REAL(x_copy)[0] = REAL(x_copy)[0] + 4;
  UNPROTECT(1);
  return x_copy;
')
x <- 1
y <- x
add_four(x)
x
y

#查看内部c代码
tabulate
pryr::show_c_source(.Internal(tabulate(bin, nbins)))
