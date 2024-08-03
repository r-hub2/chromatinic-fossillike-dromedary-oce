// vim: set expandtab shiftwidth=2 softtabstop=2 tw=200:

#include <Rcpp.h>
#include <stdio.h>
#include <stdlib.h>
#include <time.h>

using namespace Rcpp;

// Cross-reference work:
// 1. update ../src/registerDynamicSymbol.c with an item for this
// 2. main code should use the autogenerated wrapper in ../R/RcppExports.R

static int warningsBadYear = 0;
static int warningsBytePair = 0;

// If memory-fault problems occur, look at the Calloc() and Realloc()
// calls, and at the spots where information is stored in the relevant
// arrays. Note that arrows grow by a factor of 3/2 whenever needed;
// this is close to the Golden Ratio, which some analysts feel is
// efficient. Apparently, Java uses 1.5 also, but 2 is another choice
// that we've used here, in an earlier version. The initial sizes of
// the arrays are chosen to be reasonable for small to medium files,
// to avoid wasting time reallocating over and over.

// The windows compiler lacks timegm(). I tried lots of tricks
// to try to access some of the supposed alternatives, namely
// _mkgmtime(), _mkgmtime32() or _mkgmtime64()), but had no
// success. Each test takes about a half hour on the winbuilder
// machine and I didn't want to overstay my welcome there, so I
// looked in the R source. Using
//     grep -R --include \*.[ch] timegm .
// gave me the idea of using R_timegm(). I'm not sure how to link
// with it, though. In the end, I tried a half-dozen schemes
// that tried to link with windows subroutines, and then decided it
// was not worth the hassle, and I just used code that used the number
// of days in each month, etc. It takes leap years into account, but
// not leap seconds. I'm basing this on code in Rcpp, which states the
// deeper source as R itself. Since these products and oce are all
// GPL, I reason that it's OK to use it here, modified from the C++
// form (using references) to a C form (likely similar to that used
// within R, but I didn't check on that).
// Note that this returns a double, which we cast to a time_t.
double oce_timegm(struct tm *t) {
  static const int days_in_month[12] = {31, 28, 31, 30, 31, 30,
                                        31, 31, 30, 31, 30, 31};
  static const int year_base = 1900;
#define isleap(y) ((((y) % 4) == 0 && ((y) % 100) != 0) || ((y) % 400) == 0)
#define days_in_year(year) (isleap(year) ? 366 : 365)
  int day = 0;
  int i, year, year0;
  double excess = 0.0;

  day = t->tm_mday - 1;
  year0 = year_base + t->tm_year;
  if (year0 > 2050) {
    warningsBadYear++;
    if (warningsBadYear < 6) {
      Rprintf(
          "      Warning: year=%d exceeds 2050, so subtracting 100 (at most 5 "
          "warnings will be issued)",
          year0);
    }
    year0 = year0 - 100;
  }
  /* safety check for unbounded loops */
  if (year0 > 3000) {
    excess = (int)(year0 / 2000) - 1;
    year0 -= excess * 2000;
  } else if (year0 < 0) {
    excess = -1 - (int)(-year0 / 2000);
    year0 -= excess * 2000;
  }

  for (i = 0; i < t->tm_mon; i++) {
    day += days_in_month[i];
  }
  if (t->tm_mon > 1 && isleap(year0)) {
    day++;
  }
  t->tm_yday = day;

  if (year0 > 1970) {
    for (year = 1970; year < year0; year++) {
      day += days_in_year(year);
    }
  } else if (year0 < 1970) {
    for (year = 1969; year >= year0; year--) {
      day -= days_in_year(year);
    }
  }

  /* weekday: Epoch day was a Thursday */
  if ((t->tm_wday = (day + 4) % 7) < 0) {
    t->tm_wday += 7;
  }

  return t->tm_sec + (t->tm_min * 60) + (t->tm_hour * 3600) +
         (day + excess * 730485) * 86400.0;
#undef isleap
#undef days_in_year
}

/*

Locate Data Chunk for RDI

@description

Read an RDI adp file, finding the start indices of ensembles (or
"profiles", in other words).  Use `from` and `to` to extract a subset
of the file.  Setting `from=1` and `to=0` means to read the entire
file.

@details

In the name, "ldc" refers to "Locate Data Chunk", and "rdi" refers to
RD Instruments.

@section history:

A version of this function, used before October 2016, was passed a
buffer that contained the whole file contents. This scheme was dropped
because of limitations on the size of vectors in R, which is set by
the R use of 32-bit integers. (Note: 2^32-1 corresponds to a data file
of roughly 4.3Gb.)

THIS IS A FUNCTION STILL IN DEVELOPMENT, and much of what is said
about the behaviour is aspirational. At present, it reads the *whole*
file, ignoring all arguments except the file name.

@param filename character string indicating the name of an RDI adp
file.

@param from integer giving the index of the first ensemble (AKA
profile) to retrieve. The R notation is used, i.e. from=1 means the
first profile.

@param to integer giving the index of the last ensemble to retrieve.
As a special case, setting this to 0 will retrieve *all* the data
within the file.

@param by integer giving increment of the sequence, as for seq(), i.e.
a value of 1 means to retrieve all the profiles, while a value of 2
means to get every second profile.

@param startIndex integer giving the location of the first 7f7f byte pair.

@param mode integer, 0 if 'from' etc are profile numbers or 1 if they
are the numerical values of unix times.

@param debug integer, 1 or higher to turn on printing. Note that the R function
subtracts 1 from the debug level, before calling this C++ fucction. In other
words, calling `read.adp.rdi(...,debug=1)` does not turn debuggin on,
but calling `read.adp.rdi(...,debug=2)` does.

@value a list containing "ensembleStart", "ensembleStart64", "time", "sec100",
and "buf", and "ensemble_in_file", which are used in the calling R function,
read.adp.rdi().

@examples

R CMD SHLIB ldc_rdi_in_file.c
f <- "/data/archive/sleiwex/2008/moorings/m09/adp/rdi_2615/raw/adp_rdi_2615.000"
dyn.load("src/ldc_rdi_in_file.so")
a <- .Call("ldc_rdi_in_file", f, 1, 0, 1) # whole file
b <- .Call("ldc_rdi_in_file", f, 1, 10, ) # first 10 ensembles
stopifnot(all.equal(length(a$ensembleStart), 79134))
stopifnot(all.equal(a$ensembleStart[1:10], b$ensembleStart))

@references

1. WorkHorse Commands and Output Data Format_Nov07.pdf

2. p124 of [1]: header structure (note that 'number of bytes in
ensemble' does *not* count the first 2 bytes; it's really an offset to
the checksum)

3. p158 (section 5.8) of [1] defines how to handle checksums

@author

Dan Kelley

*/

// [[Rcpp::export]]
List do_ldc_rdi_in_file(StringVector filename, IntegerVector from,
                        IntegerVector to, IntegerVector by,
                        IntegerVector startIndex, IntegerVector mode,
                        IntegerVector debug) {
  struct tm etime; // time of the ensemble under examination
  time_t ensemble_time =
      0; // integer-ish form of the above (only calculated if mode=1)
  time_t ensemble_time_last = 0; // we use this for 'by', if mode is 1
  std::string fn = Rcpp::as<std::string>(filename(0));

  FILE *fp = fopen(fn.c_str(), "rb");
  if (!fp) {
    ::Rf_error("cannot open file '%s'\n", fn.c_str());
  }
  if (from[0] < 0) {
    ::Rf_error("'from' must be positive");
  }
  unsigned long int from_value = from[0];
  if (to[0] < 0) {
    ::Rf_error("'to' must be positive");
  }
  unsigned long int to_value = to[0];
  if (by[0] < 0) {
    ::Rf_error("'by' must be positive");
  }
  unsigned long int by_value = by[0];
  unsigned long int start_index = startIndex[0];
  int mode_value = mode[0];
  if (mode_value != 0 && mode_value != 1) {
    ::Rf_error("'mode' must be 0 or 1");
  }
  int debug_value = debug[0];
  if (debug_value < 0) {
    debug_value = 0;
  }
  if (debug_value > 0) {
    Rprintf("    do_ldc_rdi_in_file() {\n");
  }
  // Rprintf("from=%d, to=%d, by=%d, mode_value=%d\n", from_value, to_value,
  // by_value, mode_value);
  int c, clast = 0x00;
  int byte1 = 0x7f;
  int byte2 = 0x7f;
  // int byte2 = 0x79;
  unsigned short int check_sum, desired_check_sum;
  unsigned int bytes_to_check = 0;
  unsigned int bytes_to_check_last = 0; // permit bad chunk length (issue 1437)
  unsigned long int cindex = 0;
  unsigned long int outEnsemblePointer = 1;
  if (start_index > 1) {
    Rprintf("      skipping %lu bytes at start of the file, seeking 7F7F byte "
            "pair\n",
            start_index - 1);
    for (unsigned int i = 1; i < start_index; i++) {
      fgetc(fp);
      cindex++;
    }
  }
  clast = fgetc(fp);
  cindex++;
  if (clast == EOF) {
    ::Rf_error("empty file '%s'", fn.c_str());
  }
  // 'obuf' is a growable C buffer to hold the output, which eventually
  // gets saved in the R item "buf".
  unsigned long int nobuf = 100000; // BUFFER SIZE
  unsigned char *obuf = (unsigned char *)R_Calloc((size_t)nobuf, unsigned char);
  unsigned long int iobuf = 0;

  // 'ensembles', 'times' and 'sec100s' are growable buffers of equal length,
  // with one element for each ensemble.  We also use 'ebuf', another growable
  // buffer, for storage of data within the ensemble that is under consideration
  // at the moment.
  //
  // Note that we do not check the Calloc() results because the R docs say that
  // Calloc() performs its own tests, and that R will handle any problems.
  unsigned long int nensembles =
      100000; // initial buffer size, increased if needed
  unsigned int *ensemble_in_files =
      (unsigned int *)R_Calloc((size_t)nensembles, unsigned int);
  int *ensembles = (int *)R_Calloc((size_t)nensembles, int);
  double *ensembles64 = (double *)R_Calloc((size_t)nensembles, double);
  int *times = (int *)R_Calloc((size_t)nensembles, int);
  int *sec100s = (int *)R_Calloc((size_t)nensembles, int);
  // ebuf is the buffer for storing a single ensembloe.  It is increased,
  // if needed
  unsigned long int nebuf = 50000;
  unsigned char *ebuf = (unsigned char *)R_Calloc((size_t)nebuf, unsigned char);

  unsigned long int in_ensemble = 1, out_ensemble = 0;
  int b1, b2;

  unsigned long int counter = 0, counter_last = 0;
  unsigned int long last7f7f = 0;

  while (1) {
    c = fgetc(fp);
    cindex++;
    if (c == EOF) {
      Rprintf(
          "      got to end of RDI file while trying to read the first header "
          "byte (cindex=%lu; last7f7f=%lu)\n",
          cindex, last7f7f);
      break;
    }
    // Locate "ensemble starts", spots where a 0x7f is followed by a second
    // 0x7f, then followed by data that match a checksum.
    if (clast == byte1 && c == byte2) {
      last7f7f = cindex - 2;
      // The checksum includes the starting (0x7f, 0x7f) sequence, the
      // two bytes that specify the number of bytes in the
      // ensemble, and the data in the ensemble (sans the two bytes
      // at the end of the data, which store the checksum).
      if (debug_value > 0) {
        Rprintf("      0x7f 0x7f at position %ld (cindex=%lu last7f7f=%lu)\n",
                ftell(fp), cindex, last7f7f);
      }
      check_sum = (unsigned short int)byte1;
      check_sum += (unsigned short int)byte2;
      b1 = fgetc(fp);
      cindex++;
      if (b1 == EOF) {
        Rprintf(
            "      EOF while seeking first 0x7F (cindex %lu; last7f7f=%lu)\n",
            cindex, last7f7f);
        break;
      }
      check_sum += (unsigned short int)b1;
      b2 = fgetc(fp);
      cindex++;
      if (b2 == EOF) {
        Rprintf(
            "      EOF while seeking second 0x7F (cindex=%lu; last7f7f=%lu)\n",
            cindex, last7f7f);
        break;
      }
      check_sum += (unsigned short int)b2;
      // Now we are ready to look at the rest of the bytes. Note that
      // our loop starts at index 4, because we have already handled
      // those 4 bytes of the ensemble (i.e. those 4 bytes are include
      // in the bytes_to_check value that we now calculate).
      bytes_to_check = (unsigned int)b1 + 256 * (unsigned int)b2;
      if (debug_value > 0) {
        Rprintf("        bytes_to_check=%d based on b1=%d(0x%02x) and "
                "b2=%d(0x%02x)\n",
                bytes_to_check, b1, b1, b2, b2);
      }
      if (bytes_to_check < 5) {
        // only happens with error; we check so bytes_to_read won't be crazy
        R_Free(ensembles);
        R_Free(ensembles64);
        R_Free(times);
        R_Free(sec100s);
        R_Free(ebuf);
        ::Rf_error("cannot decode the length of ensemble number %lu",
                   in_ensemble);
      }
      if (bytes_to_check < 4) {
        ::Rf_error("bytes_to_check should be >=4 but it is %d\n",
                   bytes_to_check);
      }
      unsigned int bytes_to_read =
          bytes_to_check - 4; // byte1&byte2&check_sum used 4 bytes already

      // Expand the ensemble buffer, ebuf, if need be.
      if (bytes_to_read > nebuf) {
        if (debug_value > 0) {
          Rprintf("      increasing 'ebuf' buffer size from %lu bytes to %d "
                  "bytes\n",
                  nebuf, bytes_to_read);
        }
        ebuf = (unsigned char *)R_Realloc(ebuf, bytes_to_read, unsigned char);
        nebuf = bytes_to_read;
      }
      // Read the bytes in one operation, because fgetc() is too slow.
      unsigned int bytesRead;
      bytesRead = fread(ebuf, bytes_to_read, sizeof(unsigned char), fp);
      if (feof(fp) || bytesRead == 0) {
        Rprintf("      EOF at cindex=%lu (last7f7f=%lu)\n", cindex, last7f7f);
        break;
      }
      cindex += bytes_to_read;
      for (unsigned int ib = 0; ib < bytes_to_read; ib++) {
        check_sum += (unsigned short int)ebuf[ib];
      }
      int cs1, cs2;
      cs1 = fgetc(fp);
      cindex++;
      if (cs1 == EOF) {
        Rprintf("      EOF while seeking 1st checksum byte (cindex %lu; "
                "last7f7f=%lu)\n",
                cindex, last7f7f);
        break;
      }
      cs2 = fgetc(fp);
      cindex++;
      if (cs2 == EOF) {
        Rprintf("      EOF while seeking 2nd checksum byte (cindex %lu; "
                "last7f7f=%lu)\n",
                cindex, last7f7f);
        break;
      }
      desired_check_sum =
          ((unsigned short int)cs1) | ((unsigned short int)(cs2 << 8));
      if (check_sum == desired_check_sum) {
        if (debug_value > 0) {
          Rprintf("        good checksum\n");
        }
        // use later, if find bad checksum (issue 1437)
        bytes_to_check_last = bytes_to_check;
        // The check_sum is ok, so we may want to store the results for
        // this profile.
        //
        // First, ensure that there will be sufficient storage to store results.
        // We do this before checking to see if we are actually going
        // to store the results, so possibly this might get done one
        // more time than required, before this function returns.
        if (out_ensemble >= nensembles) {
          // Enlarge the buffer. We do not check the Realloc() result, because
          // this is an R macro that is supposed to check for errors and handle
          // them.
          nensembles = 3 * nensembles / 2;
          if (debug_value > -1) {
            Rprintf("      increasing storage to hold up to %lu elements\n",
                    nensembles);
          }
          ensemble_in_files = (unsigned int *)R_Realloc(
              ensemble_in_files, nensembles, unsigned int);
          ensembles = (int *)R_Realloc(ensembles, nensembles, int);
          ensembles64 = (double *)R_Realloc(ensembles64, nensembles, double);
          times = (int *)R_Realloc(times, nensembles, int);
          sec100s = (int *)R_Realloc(sec100s, nensembles, int);
        }
        // We will decide whether to keep this ensemble, based on ensemble
        // number, if mode_value is 0 or on time, if mode_value is 1. That
        // means we only need to compute a time if mode_value is 1.
        unsigned int time_pointer =
            (unsigned int)ebuf[4] + 256 * (unsigned int)ebuf[5];
        etime.tm_year = 100 + (int)ebuf[time_pointer + 0];
        etime.tm_mon = -1 + (int)ebuf[time_pointer + 1];
        etime.tm_mday = (int)ebuf[time_pointer + 2];
        etime.tm_hour = (int)ebuf[time_pointer + 3];
        etime.tm_min = (int)ebuf[time_pointer + 4];
        etime.tm_sec = (int)ebuf[time_pointer + 5];
        etime.tm_isdst = 0;
        // Use local timegm code, which I suppose is risky, but it
        // does not seem that Microsoft Windows provides this function
        // in a workable form.
        ensemble_time = oce_timegm(&etime);
        // See whether we are past the 'from' condition. Note the "-1"
        // for the ensemble case, because R starts counts at 1, not 0,
        // and the calling R code is (naturally) in R notation.
        if (debug_value > 0) {
          Rprintf("        in_ensemble=%lu from_value=%lu counter=%lu "
                  "counter_last=%lu ensemble_time=%lu\n",
                  in_ensemble, from_value, counter, counter_last,
                  (long unsigned int)ensemble_time);
        }
        // Have we got to the starting location yet?
        if ((mode_value == 0 && in_ensemble >= (from_value - 1)) ||
            (mode_value == 1 && ensemble_time >= (time_t)from_value)) {
          //  Handle the 'by' value.
          if ((mode_value == 0 && (counter == from_value - 1 ||
                                   (counter - counter_last) >= by_value)) ||
              (mode_value == 1 &&
               (ensemble_time - ensemble_time_last) >= (time_t)by_value)) {
            if (debug_value > 0) {
              Rprintf("        satisfies 'from', 'by' and 'to' criteria\n");
            }
            // Copy ensemble to output buffer, after 6 bytes of header
            ensemble_in_files[out_ensemble] = 1 + last7f7f; // +1 for R notation
            ensembles[out_ensemble] = outEnsemblePointer;
            ensembles64[out_ensemble] = double(outEnsemblePointer);
            // 6 bytes for: 0x7f,0x7f,b1,b2,cs1,cs2
            outEnsemblePointer = outEnsemblePointer + 6 + bytes_to_read;
            times[out_ensemble] = ensemble_time;
            // Increment counter (can be of two types)
            if (mode_value == 1) {
              ensemble_time_last = ensemble_time;
            } else {
              counter_last = counter;
            }
            // Rprintf("saving at in_ensemble=%lu, counter=%d, by=%d\n",
            // in_ensemble, counter, by_value);
            //     ensembles[out_ensemble] = last_start;
            unsigned int timePointer =
                (unsigned int)ebuf[4] + 256 * (unsigned int)ebuf[5];
            sec100s[out_ensemble] = ebuf[timePointer + 6];
            out_ensemble++;
            // Save to output buffer.
            if ((iobuf + 100 + bytes_to_read) >= nobuf) {
              nobuf = nobuf + 100 + bytes_to_read + nobuf / 2;
              if (debug_value > 0) {
                Rprintf("about to enlarge obuf storage to %ld elements ...\n",
                        nobuf);
              }
              obuf = (unsigned char *)R_Realloc(obuf, nobuf, unsigned char);
              if (debug_value > 0) {
                Rprintf("    ... allocation was successful\n");
              }
            }
            obuf[iobuf++] = byte1; // 0x7f
            obuf[iobuf++] = byte2; // 0x7f
            obuf[iobuf++] = b1;    // length of ensemble, byte 1
            obuf[iobuf++] = b2;    // length of ensemble, byte 1
            for (unsigned int i = 0; i < bytes_to_read; i++) {
              obuf[iobuf++] = ebuf[i]; // data, not including the checksum
            }
            obuf[iobuf++] = cs1; // checksum  byte 1
            obuf[iobuf++] = cs2; // checksum  byte 2
          } else {
            if (debug_value > 0) {
              Rprintf(
                  "      does not satisfy 'from', 'by' and 'to' criteria\n");
            }
          }
          counter++;
        }
        in_ensemble++;
        // If 'max' is positive, check that we return only that many
        // ensemble pointers.
        //> Rprintf("L417 in_ensemble=%lu from_value=%lu to_value=%lu\n",
        // in_ensemble, from_value, to_value);
        if ((mode_value == 0 && (to_value > 0 && in_ensemble > to_value)) ||
            (mode_value == 1 && (ensemble_time >= (time_t)to_value))) {
          break;
        }
      } else {
        // synch up, just to be sure (cost is low since this rarely happens)
        cindex = ftell(fp);
        if (debug_value > 0) {
          Rprintf("        bad checksum %d (expected %d)\n", check_sum,
                  desired_check_sum);
        }
        // maybe the number of bytes to check was wrong (issue 1437)
        if (bytes_to_check_last != bytes_to_check) {
          if (bytes_to_check == 0) {
            ::Rf_error("bad file: first ensemble has a checksum error\n");
          }
          if (debug_value > 0) {
            Rprintf(
                "      length (%d bytes) disagrees with previous (%d bytes)\n",
                bytes_to_check, bytes_to_check_last);
          }
          // Skip to just past the last 7f7f, and then start looking
          // for the next valid 7f7f.
          cindex = last7f7f;
          fseek(fp, last7f7f, SEEK_SET);
          clast = 0;
          for (unsigned int iii = 0; iii < 2 * bytes_to_check_last; iii++) {
            c = fgetc(fp);
            cindex++;
            if (debug_value > 0) {
              Rprintf("      realign iii=%d cindex=%lu c=0x%02x clast=0x%02x\n",
                      iii, cindex, c, clast);
            }
            if (clast == byte1 && c == byte2) {
              if (debug_value > 0) {
                Rprintf("      got 7f 7f again at byte %ld, after realigning\n",
                        ftell(fp));
              }
              // check if next two bytes give the expected length as
              // before
              b1 = fgetc(fp);
              cindex++;
              b2 = fgetc(fp);
              cindex++;
              bytes_to_check = (unsigned int)b1 + 256 * (unsigned int)b2;
              if (bytes_to_check == bytes_to_check_last) {
                fseek(fp, -2L, SEEK_CUR);
                // synch up, just to be sure (cost is low since this rarely
                // happens)
                cindex = ftell(fp);
                Rprintf("      recovered from bad checksum, restarting at "
                        "index %lu\n",
                        cindex);
                break;
              } else {
                if (debug_value > 0)
                  Rprintf(" ACCIDENTALLY MATCH since bytes_to_check=%d, not "
                          "expected %d\n",
                          bytes_to_check, bytes_to_check_last);
              }
            }
            clast = c;
          }
        }
        if (debug_value > 0) {
          Rprintf("      in_ensemble=%lu; from_value=%lu\n", in_ensemble,
                  from_value);
          Rprintf("      counter=%lu; counter_last=%lu\n", counter,
                  counter_last);
        }
      }
      R_CheckUserInterrupt(); // only check once per ensemble, for speed
      // fseek(fp,-1L,SEEK_CUR);Rprintf("OK  test at %d c=0x%02x\n", ftell(fp),
      // fgetc(fp));
      clast = c;
    } else {
      // Either clast != byte1 or c != byte2) {
      // Rprintf("skipping byte. c=0x%02x clast=0x%02x  cindex %lld (ftell()
      // says at pointer %d)\n", c, clast, cindex, ftell(fp));
      // fseek(fp,-1L,SEEK_CUR);Rprintf("BAD test at %d c=0x%02x\n", ftell(fp),
      // fgetc(fp));
      cindex = ftell(fp); // synch up, just to be sure (cost is low since this
                          // rarely happens)
      warningsBytePair++;
      if (warningsBytePair < 6) {
        Rprintf("Warning: bad ensemble-start at index %lu (at most 5 warnings "
                "will be issued)\n",
                cindex);
      }
      if (debug_value > 0) {
        Rprintf("try skipping to get 0x7f 0x7f pair\n");
      }
      for (unsigned int iii = 0; iii < 2 * bytes_to_check_last; iii++) {
        c = fgetc(fp);
        cindex++;
        if (debug_value > 0) {
          Rprintf("skipping iii=%d cindex=%lu c=0x%02x clast=0x%02x\n", iii,
                  cindex, c, clast);
        }
        if (clast == byte1 && c == byte2) {
          if (debug_value > 0) {
            Rprintf(" got 7f 7f again at byte %ld, after skipping. FYI "
                    "bytes_to_check_last=%d\n",
                    ftell(fp), bytes_to_check_last);
            Rprintf("    ... recovered from bad ensemble-start byte-pair by "
                    "restarting at byte %lu in file\n",
                    cindex);
          }
          // adjust file pointer
          fseek(fp, -2L, SEEK_CUR);
          cindex = ftell(fp); // synch again, for code clarity (cost is low
                              // since this rarely happens)
          break;
        }
        clast = c;
      }
    }
    c = fgetc(fp);
    cindex++;
    clast = c;
    if (c == EOF) {
      break;
    }
  }
  fclose(fp);

  // Finally, copy into some R memory. Possibly we should have been
  // using this all along, but I wasn't clear on how to reallocate it.
  IntegerVector ensemble_in_file(out_ensemble);
  IntegerVector ensemble(out_ensemble);
  NumericVector ensemble64(out_ensemble);
  IntegerVector sec100(out_ensemble);
  IntegerVector time(out_ensemble);
  RawVector buf(iobuf);
  // Copy data into return fields
  for (unsigned long int i = 0; i < out_ensemble; i++) {
    ensemble_in_file[i] = ensemble_in_files[i];
    ensemble[i] = ensembles[i];
    ensemble64[i] = ensembles64[i];
    time[i] = times[i];
    sec100[i] = sec100s[i];
  }
  for (unsigned long int i = 0; i < iobuf; i++) {
    buf[i] = obuf[i];
  }
  // Clear up space.
  R_Free(ensemble_in_files);
  R_Free(ensembles);
  R_Free(ensembles64);
  R_Free(times);
  R_Free(sec100s);
  R_Free(ebuf);
  R_Free(obuf);
  // Report warning counts.
  if (warningsBadYear > 0) {
    Rprintf("      NOTE: year (>2050) encountered and addressed %d times\n",
            warningsBadYear);
  }
  if (warningsBytePair > 0) {
    Rprintf("      NOTE: bad ensemble-start encountered %d times\n",
            warningsBytePair);
  }
  if (debug_value > 0) {
    Rprintf("    } # do_ldc_rdi_in_file()\n");
  }
  return (List::create(
      Named("ensembleStart") = ensemble, Named("ensembleStart64") = ensemble64,
      Named("time") = time, Named("sec100") = sec100, Named("buf") = buf,
      Named("ensemble_in_file") = ensemble_in_file));
}