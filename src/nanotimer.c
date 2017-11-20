#include <stdint.h>
#include <R.h>
#include <Rmath.h>
#include <Rinternals.h>
#include <R_ext/Applic.h>
#include <R_ext/Memory.h>

#include "sexp_macros.h"
#include "do_nothing.h"

/* FIXME: Find portable 64 bit integer type. */
typedef uint64_t nanotime_t;

#if defined(WIN32)
#include "nanotimer_windows.h"
#elif defined(__MACH__) || defined(__APPLE__)
#include "nanotimer_macosx.h"
#elif defined(linux) || defined(__linux) || defined(__FreeBSD__) || defined(__OpenBSD__)
#include "nanotimer_gettime.h"
#elif defined(sun) || defined(__sun) || defined(_AIX)
#include "nanotimer_rtposix.h"
#else /* Unsupported OS */
#error "Unsupported OS."
#endif

#if defined(__GNUC__)
#define NOINLINE  __attribute__((noinline))
#else
#define NOINLINE
#endif

nanotime_t estimate_overhead(SEXP s_rho, int rounds) {
    int i, n_back_in_time = 0;
    int observed_overhead = FALSE;
    /* Estimate minimal overhead and warm up the machine ... */
    nanotime_t start, end, overhead = UINT64_MAX;
    for (i = 0; i < rounds; ++i) {
        start = get_nanotime();
        do_nothing();
        end = get_nanotime();

        const nanotime_t diff = end - start;
        if (start < end && diff < overhead) {
            observed_overhead = TRUE;
            overhead = diff;
        } else if (start > end) {
            n_back_in_time++;
        }
    }
    if (!observed_overhead) {
        warning("Could not measure overhead. Your clock might lack precision.");
        overhead = 0;
    } else if (UINT64_MAX == overhead) {
        error("Observed overhead too large.");
    }
    if (n_back_in_time > 0) {
        warning("Observed negative overhead in %i cases.",
                n_back_in_time);
    }
    return overhead;
}

SEXP do_microtiming_precision(SEXP s_rho, SEXP s_times, SEXP s_warmup) {
    UNPACK_INT(s_warmup, warmup);
    UNPACK_INT(s_times, times);
    int n = 0;
    nanotime_t overhead = estimate_overhead(s_rho, warmup);
    nanotime_t start, end;
    SEXP s_ret;
    PROTECT(s_ret = allocVector(REALSXP, times));
    while (n < times) {
        start = get_nanotime();
        end = get_nanotime();
        if (start < end) {
            REAL(s_ret)[n] = end - start - overhead;
            n++;
        }
    }
    UNPROTECT(1); /* s_ret */
    return s_ret;
}

SEXP do_get_nanotime() {
    return ScalarReal(get_nanotime() * 1.0);
}


// For microbenchmark, unedited by me
SEXP do_microtiming(SEXP s_exprs, SEXP s_rho, SEXP s_warmup) {
  nanotime_t start, end, overhead;
  int i, n_under_overhead = 0, n_start_end_equal = 0;
  R_len_t n_exprs = 0;
  SEXP s_ret, s_expr;
  double *ret;
  
  UNPACK_INT(s_warmup, warmup);
  
  /* Expressions */
  n_exprs = LENGTH(s_exprs);
  
  /* Environment in which to evaluate */
  if(!isEnvironment(s_rho))
    error("'s_rho' should be an environment");
  
  /* Return value: */
  PROTECT(s_ret = allocVector(REALSXP, n_exprs));
  ret = REAL(s_ret);
  
  /* Estimate minimal overhead and warm up the machine ... */
  overhead = estimate_overhead(s_rho, warmup);
  
  /* Actual timing... */
  for (i = 0; i < n_exprs; ++i) {
    s_expr = VECTOR_ELT(s_exprs, i);
    start = get_nanotime();
    eval(s_expr, s_rho);
    end = get_nanotime();
    
    if (start < end) {
      const nanotime_t diff = end - start;
      if (diff < overhead) {
        ret[i] = 0.0;
        n_under_overhead++;
      } else {
        ret[i] = diff - overhead;
      }
    } else if (start == end) {
      ++n_start_end_equal;
      ret[i] = 0.0;
    } else {
      error("Measured negative execution time! Please investigate and/or "
              "contact the package author.");
    }
    
    /* Housekeeping */
    R_CheckUserInterrupt();
    /* R_gc(); */
  }
  
  /* Issue waring if we observed some timings below the estimated
  * overhead.
  */
  if (n_under_overhead > 0) {
    if (n_under_overhead == 1) {
      warning("Estimated overhead was greater than measured evaluation "
                "time in 1 run.");
    } else {
      warning("Estimated overhead was greater than measured evaluation "
                "time in %i runs.", n_under_overhead);
    }
  }
  if (n_start_end_equal > 0) {
    if (n_start_end_equal == 1) {
      warning("Could not measure a positive execution time for one "
                "evaluation.");
    } else {
      warning("Could not measure a positive execution time for %i "
                "evaluations.", n_start_end_equal);
    }
  }
  if (n_under_overhead + n_start_end_equal == n_exprs) {
    error("All timed evaluations were either smaller than the estimated "
            "overhead or zero. The most likely cause is a low resolution "
            "clock. Feel free to contact the package maintainer for debug "
            "the issue further.");
  }
  UNPROTECT(1); /* s_ret */
  return s_ret;
}

// For mbc, my edited version of do_microtiming
SEXP do_microtiming2(SEXP s_exprs, SEXP s_rho, SEXP s_warmup, int giveinint) {
  //error("Starting do_microtiming, error");
  //warning("Starting do_microtiming");
  nanotime_t start, end, overhead;
  int i, n_under_overhead = 0, n_start_end_equal = 0;
  R_len_t n_exprs = 0;
  SEXP s_ret, s_expr;
  double *ret;
  UNPACK_INT(s_warmup, warmup);

  /* Expressions */
  n_exprs = LENGTH(s_exprs);

  /* Environment in which to evaluate */
  if(!isEnvironment(s_rho))
    error("'s_rho' should be an environment");

  /* Return value: */
  PROTECT(s_ret = allocVector(REALSXP, n_exprs));
  ret = REAL(s_ret);

  /* Estimate minimal overhead and warm up the machine ... */
  overhead = estimate_overhead(s_rho, warmup);

  // Collin trying to save output
  SEXP out_test;
  SEXP s_ret2;
  //SEXP *ret2;
  PROTECT(s_ret2 = allocVector(VECSXP, n_exprs));
  //ret2 = VEC(s_ret2);
  
  /* Actual timing... */
  for (i = 0; i < n_exprs; ++i) {
    s_expr = VECTOR_ELT(s_exprs, i);
    start = get_nanotime();
    out_test = eval(s_expr, s_rho);
    //eval(s_expr, s_rho);
    end = get_nanotime();
    SET_VECTOR_ELT(s_ret2, i, out_test);

    if (start < end) {
      const nanotime_t diff = end - start;
      if (diff < overhead) {
        ret[i] = 0.0;
        n_under_overhead++;
      } else {
        //s_ret2[i] = out_test;//diff - overhead;
        //SET_VECTOR_ELT(s_ret2, i, out_test);
        ret[i] = diff - overhead;
      }
    } else if (start == end) {
      ++n_start_end_equal;
      ret[i] = 0.0;
    } else {
      error("Measured negative execution time! Please investigate and/or "
          "contact the package author.");
    }

    /* Housekeeping */
    R_CheckUserInterrupt();
    /* R_gc(); */
  }

  /* Issue waring if we observed some timings below the estimated
   * overhead.
   */
  if (n_under_overhead > 0) {
    if (n_under_overhead == 1) {
      warning("Estimated overhead was greater than measured evaluation "
          "time in 1 run.");
    } else {
      warning("Estimated overhead was greater than measured evaluation "
          "time in %i runs.", n_under_overhead);
    }
  }
  if (n_start_end_equal > 0) {
    if (n_start_end_equal == 1) {
      warning("Could not measure a positive execution time for one "
          "evaluation.");
    } else {
      warning("Could not measure a positive execution time for %i "
          "evaluations.", n_start_end_equal);
    }
  }
  if (n_under_overhead + n_start_end_equal == n_exprs) {
    error("All timed evaluations were either smaller than the estimated "
        "overhead or zero. The most likely cause is a low resolution "
        "clock. Feel free to contact the package maintainer for debug "
        "the issue further.");
  }
  //UNPROTECT(1); /* s_ret */
  //UNPROTECT(2); /* s_ret and s_ret2 */
  //warning("Returning s_ret2");
  
  // Combine run times and output to single SEXP to return
  SEXP time_and_output;
  PROTECT(time_and_output = allocVector(VECSXP, 2));
  SET_VECTOR_ELT(time_and_output, 0, s_ret);
  SET_VECTOR_ELT(time_and_output, 1, s_ret2);
  UNPROTECT(3); /* s_ret and s_ret2 and time_and_output */
  
  return time_and_output;
  //return s_ret2;
  //return out_test;
  //return s_ret;
}


// For mbc, my edited version of do_microtiming
SEXP do_microtiming3(SEXP s_exprs, SEXP s_rho, SEXP s_warmup, int giveinint, SEXP ord) {
  //error("Starting do_microtiming, error");
  //warning("Starting do_microtiming");
  nanotime_t start, end, overhead;
  int i, n_under_overhead = 0, n_start_end_equal = 0;
  R_len_t n_exprs = 0;
  SEXP s_ret, s_expr;
  double *ret;
  UNPACK_INT(s_warmup, warmup);
  
  /* Expressions */
  n_exprs = LENGTH(s_exprs);
  
  /* Environment in which to evaluate */
  //if(!isEnvironment(s_rho))
  //  error("'s_rho' should be an environment");
  if(!isList(s_rho) && !isEnvironment(VECTOR_ELT(s_rho, 0)))
    error("'s_rho' should be a list of environments");
  
  /* Return value: */
  PROTECT(s_ret = allocVector(REALSXP, n_exprs));
  ret = REAL(s_ret);
  
  /* Estimate minimal overhead and warm up the machine ... */
  overhead = estimate_overhead(s_rho, warmup);
  
  // Collin trying to save output
  SEXP out_test;
  SEXP s_ret2;
  //SEXP *ret2;
  PROTECT(s_ret2 = allocVector(VECSXP, n_exprs));
  //ret2 = VEC(s_ret2);
  
  /* Actual timing... */
  for (i = 0; i < n_exprs; ++i) {
    s_expr = VECTOR_ELT(s_exprs, i);
    start = get_nanotime();
    out_test = eval(s_expr, s_rho);
    //eval(s_expr, s_rho);
    end = get_nanotime();
    SET_VECTOR_ELT(s_ret2, i, out_test);
    
    if (start < end) {
      const nanotime_t diff = end - start;
      if (diff < overhead) {
        ret[i] = 0.0;
        n_under_overhead++;
      } else {
        //s_ret2[i] = out_test;//diff - overhead;
        //SET_VECTOR_ELT(s_ret2, i, out_test);
        ret[i] = diff - overhead;
      }
    } else if (start == end) {
      ++n_start_end_equal;
      ret[i] = 0.0;
    } else {
      error("Measured negative execution time! Please investigate and/or "
              "contact the package author.");
    }
    
    /* Housekeeping */
    R_CheckUserInterrupt();
    /* R_gc(); */
  }
  
  /* Issue waring if we observed some timings below the estimated
  * overhead.
  */
  if (n_under_overhead > 0) {
    if (n_under_overhead == 1) {
      warning("Estimated overhead was greater than measured evaluation "
                "time in 1 run.");
    } else {
      warning("Estimated overhead was greater than measured evaluation "
                "time in %i runs.", n_under_overhead);
    }
  }
  if (n_start_end_equal > 0) {
    if (n_start_end_equal == 1) {
      warning("Could not measure a positive execution time for one "
                "evaluation.");
    } else {
      warning("Could not measure a positive execution time for %i "
                "evaluations.", n_start_end_equal);
    }
  }
  if (n_under_overhead + n_start_end_equal == n_exprs) {
    error("All timed evaluations were either smaller than the estimated "
            "overhead or zero. The most likely cause is a low resolution "
            "clock. Feel free to contact the package maintainer for debug "
            "the issue further.");
  }
  //UNPROTECT(1); /* s_ret */
  //UNPROTECT(2); /* s_ret and s_ret2 */
  //warning("Returning s_ret2");
  
  // Combine run times and output to single SEXP to return
  SEXP time_and_output;
  PROTECT(time_and_output = allocVector(VECSXP, 2));
  SET_VECTOR_ELT(time_and_output, 0, s_ret);
  SET_VECTOR_ELT(time_and_output, 1, s_ret2);
  UNPROTECT(3); /* s_ret and s_ret2 and time_and_output */
  
  return time_and_output;
  //return s_ret2;
  //return out_test;
  //return s_ret;
}
