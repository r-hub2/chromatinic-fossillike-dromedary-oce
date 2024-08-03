/* vim: set expandtab shiftwidth=2 softtabstop=2 tw=70: */

#include <Rcpp.h>
using namespace Rcpp;

// Cross-reference work:
// 1. update ../src/registerDynamicSymbol.c with an item for this
// 2. main code should use the autogenerated wrapper in ../R/RcppExports.R
//
// vim: set expandtab shiftwidth=2 softtabstop=2 tw=70:
//
// Try to average across bands; where both bands have any of the
// following codes, return 0xff which will be coded as land in
// plot.amsr().
//
//     0xff # land mass
//     0xfe # no observations
//     0xfd # bad observations
//     0xfc # sea ice
//     0xfb # missing SST or wind due to rain, or missing water vapour due to heavy rain

//#define DEBUG

/*

   system("R CMD shlib amsr.c")
   dyn.load("amsr.so")
   a <- as.raw(1:7)
   b <- as.raw(7:13)
   a[1] <- as.raw(0xff)
   a[4] <- as.raw(0xfb)
   a[5] <- as.raw(0xfb)
   b[1] <- as.raw(0xff)
   a[2] <- as.raw(0xff)
   b[3] <- as.raw(0xff)
   b[4] <- as.raw(0xfb)
   b[6] <- as.raw(0xfb)
   ab <- .Call("amsr_average", a, b)
   stopifnot(all.equal(ab, as.raw(c(0xff, 0xff, 0xff, 0xfb, 0x0b, 0x06, 0x0a))))

 */

// [[Rcpp::export]]
RawVector do_amsr_average(RawVector a, RawVector b)
{
  int na = a.size(), nb=b.size();
  if (na != nb)
     ::Rf_error("lengths must agree but length(a) is %d and length(b) is %d", na, nb);
  RawVector res(na);
  unsigned char A, B;
  for (int i = 0; i < na; i++) {
    A = a[i];
    B = b[i];
    if (A < 0xfb && B < 0xfb) { // A and B are both OK (the most common case, so put first here)
      res[i] = (unsigned char)(0.5+0.5*(A+B)); // note rounding

    } else if (A == 0xff) { // A is land; ignore B and return code for land
      res[i] = 0xff;
    } else if (B == 0xff) { // B is land; ignore A and return code for land
      res[i] = 0xff;

    } else if (A == 0xfe) { // 254
      res[i] = B; // no A observation, so use B, whatever it is
    } else if (B == 0xfe) {
      res[i] = A; // no B observation, so use A, whatever it is

    } else if (A == 0xfd) { // 253
      res[i] = B; // bad A observation, so use B, whatever it is
    } else if (B == 0xfd) {
      res[i] = A; // bad B observation, so use A, whatever it is

    } else if (A == 0xfc) { // 252
      res[i] = B; // A had sea ice; try B (although it is likely also ice)
    } else if (B == 0xfc) {
      res[i] = A; // A had sea ice; try A (although it is likely also ice)

    } else if (A == 0xfb) { // 251
      res[i] = B; // A was too rainy; try B, on the hope that rain is short-lived
    } else if (B == 0xfb) {
      res[i] = A; // B was too rainy; try A, on the hope that rain is short-lived

    } else {
      res[i] = 0xff; // Cannot get here
    }
  }
  return(res);
}


/*

   system("R CMD shlib amsr.c")
   dyn.load("amsr.so")
   a <- array(as.raw(0:255), dim=c(20,20,3))
   A <- .Call("amsr_composite", a)

*/


//old // a is an array with e.g. a[,,1] being a matrix of data in the first image
//old // [[Rcpp::export]]
//old RawVector do_amsr_composite(RawVector a, IntegerVector dim)
//old {
//old   //Rprintf("amsr_composite ...\n");
//old   if (dim.size() != 3)
//old     ::Rf_error("dim should be of length 3, but it is of length %d", dim.size());
//old   unsigned int n1 = dim[0];
//old   unsigned int n2 = dim[1];
//old   unsigned int n3 = dim[2];
//old   unsigned int n12 = n1 * n2;
//old   //Rprintf("amsr_composite n1=%d n2=%d n3=%d n12=%d\n", n1, n2, n3, n12);
//old   RawVector res(n12);
//old   unsigned char A = 'a'; // assignment prevents compiler warning at line 145
//old   for (unsigned int i = 0; i < n12; i++) {
//old     double sum = 0.0;
//old     int nsum = 0;
//old     //if (i < 300) Rprintf("i=%d:\n", i);
//old     for (unsigned int i3 = 0; i3 < n3; i3++) {
//old       A = a[i + n12*i3];
//old       if (A < 0xfb) {
//old         sum += A;
//old         nsum++;
//old         //if (i < 300) Rprintf("    i3=%3d A=%3d=0x%02x sum=%5.1f nsum=%d\n", i3, (int)A, A, sum, nsum);
//old       } else {
//old         //if (i < 300) Rprintf("    i3=%3d A=%3d=0x%02x SKIPPED\n", i3, (int)A, A);
//old       }
//old     }
//old     if (nsum)
//old       res[i] = (unsigned char)floor(0.5 + sum/nsum);
//old     else
//old       res[i] = A; // will be >= 0xfb ... we inherit the NA type from last image
//old     //if (i < 300) Rprintf("    res=%d=0x%02x\n", (int)res[i], res[i]);
//old   }
//old   return res;
//old }

