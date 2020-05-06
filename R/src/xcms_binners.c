#include "xcms_binners.h"
/*
 * C code from xcms package, which contains binning utils. 
 * Ref: Smith, C.A., Want, E.J., O'Maille, G., Abagyan,R., Siuzdak, G. (2006). 
 * “XCMS: Processing mass spectrometry data for metabolite profiling using nonlinear 
 * peak alignment, matching and identification.” Analytical Chemistry, 78, 779–787.
 */

/* 

/*
 * ----------------------- R ENTRY POINTS -----------------------
 */

/*
 * Bin vector x into bins and aggregate values in y for x-values falling within a
 * certain bin. Definition of breaks for the bins depends on input arguments breaks,
 * nBins and binSize (pre-defined breaks are used, breaks are calculated based on
 * the number of bins or the specified bin size).
 * Binning is defined based on the number of bins (nBin) and the range of values
 * in x that should be binned (fromX to toX).
 * This binning corresponds to seq(fromX, toX, length.out = (nBins + 1))
 * Arguments:
 * x: numeric vector of values on which y should be binned.
 * y: numeric vector that should be binned.
 * breaks: numeric vector of length (number of bins + 1) specifying the lower and
 *     upper boundary for the bins. If specified, arguments nBins, binSize, fromX
 *     and toX are ignored.
 * nBins: number of bins.
 * binSize: the size of the bin.
 * fromX: the lowest x-value form which binning should start (only used if nBins
 *     or binSize are specified).
 * toX: the largest x-value to be included in the binning (only used if nBins or
 *     binSize are specified).
 * fromIdx, toIdx: indices in array x (0-based) that allow to specify a sub-set
 *     of x/y on which the binning should be done.
 * shiftByHalfBinSize: either 0 or 1. If 0 breaks are defined from fromX to toX,
 *     if 1 breaks are defined from fromX - (bin_size / 2) to toX + (bin_size / 2).
 *     The bin_size is calculated as (toX - fromX) / (nBins - 1), such that the
 *     number of bins corresponds to nBins. If 1 binning is performed similarly to
 *     the profBin method.
 * method: integer to select the aggregation method: 1: max, 2: min, 3: sum, 4: mean.
 * baseValue: value to be used for empty bins.
 * getIndex: 0 or 1, whether an index should be returned or not.
 * The function returns a list with the first element (x) being the bin
 * midpoints and the second element (y) the max y-value within each bin.
 *
 * Notes: we have the following memory demanding allocVectors:
 * 1) ans, REAL, the return value $y. Required.
 * 2) index, INTEGER, only optional and only required if getIndex is 1.
 * 3) brks, REAL, created if no breaks is pre-defined.
 * 4) bin_mids, REAL, the return value $x... always required?
 *
 * Updates: to reduce excessive memory demand (issue #191) we're:
 * a) creating the index vector ONLY if getIndex is 1.
 * b) allow the user to specify that no $x will be created/returned.
 */
SEXP binYonX(SEXP x, SEXP y, SEXP breaks, SEXP nBins, SEXP binSize,
	     SEXP fromX, SEXP toX, SEXP fromIdx, SEXP toIdx,
	     SEXP shiftByHalfBinSize, SEXP method, SEXP baseValue,
	     SEXP getIndex, SEXP getX) {
  SEXP ans, brks, bin_mids, ans_list, names, index;
  int n_bin, from_idx, to_idx, the_method, get_x,
    shift_by_half_bin_size, count_protect, have_index, *p_index, get_index;
  double from_x, to_x, bin_size, *p_ans, *p_brks, base_value;

  /* Initializeing variables */
  count_protect = 0;  // To count the PROTECT calls.
  have_index = 0;     // If an index with the min and max value is returned too.
  get_index = asInteger(getIndex);  // Whether we want the index to be returned at all.
  get_x = asInteger(getX);
  from_idx = asInteger(fromIdx);
  to_idx = asInteger(toIdx);
  the_method = asInteger(method);
  shift_by_half_bin_size = asInteger(shiftByHalfBinSize);
  base_value = REAL(baseValue)[0];

  if (from_idx < 0 || to_idx < 0)
    error("'fromIdx' and 'toIdx' have to be >= 0!");
  if (from_idx > to_idx)
    error("'fromIdx' has to be smaller than 'toIdx'!");
  if (to_idx >= LENGTH(x))
    error("'toIdx' can not be larger than length(x)!");

  /* Define from_x and to_x */
  if (ISNA(REAL(fromX)[0])) {
    from_x = REAL(x)[from_idx];
  } else {
    from_x = REAL(fromX)[0];
  }
  if (ISNA(REAL(toX)[0])) {
    to_x = REAL(x)[to_idx];
  } else {
    to_x = REAL(toX)[0];
  }
  /* Binning: define breaks. */
  if (!ISNA(REAL(breaks)[0])) {
    /* Using the provided breaks */
    n_bin = (LENGTH(breaks) - 1);
    p_brks = REAL(breaks);
    if (n_bin < 1)
      error("Not enough breaks defined!");
  } else if (INTEGER(nBins)[0] != NA_INTEGER) {
    /* Calculating breaks based on the number of bins. */
    n_bin = asInteger(nBins);
    if (n_bin <= 0)
      error("'nBins' must be larger 1!");
    PROTECT(brks = allocVector(REALSXP, n_bin + 1));
    count_protect++;
    /* Calculate the breaks */
    p_brks = REAL(brks);
    _breaks_on_nBins(from_x, to_x, n_bin, p_brks, shift_by_half_bin_size);
  } else {
    /* Calculating breaks based on bin size. */
    bin_size = REAL(binSize)[0];
    if (bin_size < 0)
      error("'binSize' has to be > 0!");
    if (shift_by_half_bin_size > 0) {
      from_x = from_x - (bin_size / 2);
      to_x = to_x + (bin_size / 2);
    }
    /* Also here: note that we might want to use round instead. */
    n_bin = (int)round((to_x - from_x) / bin_size);
    PROTECT(brks = allocVector(REALSXP, n_bin + 1));
    count_protect++;
    p_brks = REAL(brks);
    _breaks_on_binSize(from_x, to_x, n_bin, bin_size, p_brks);
  }

  /* Create output */
  PROTECT(ans = allocVector(REALSXP, n_bin));
  count_protect++;

  /* Do the binning. */
  p_ans = REAL(ans);
  for(int i = 0; i < n_bin; i++) {
    p_ans[i] = NA_REAL;
  }
  switch (the_method) {
  case 2:
    if (get_index) {
      PROTECT(index = allocVector(INTSXP, n_bin));
      p_index = INTEGER(index);
      _bin_y_on_x_with_breaks_min_idx(REAL(x), REAL(y), p_brks, p_ans, n_bin,
				      from_idx, to_idx, p_index);
      have_index = 1;
    } else {
      PROTECT(index = allocVector(INTSXP, 1));
      _bin_y_on_x_with_breaks_min(REAL(x), REAL(y), p_brks, p_ans, n_bin,
				  from_idx, to_idx);
    }
    count_protect++;
    break;
  case 3:
    PROTECT(index = allocVector(INTSXP, 1));
    count_protect++;
    _bin_y_on_x_with_breaks_sum(REAL(x), REAL(y), p_brks, p_ans, n_bin,
				from_idx, to_idx);
    break;
  case 4:
    PROTECT(index = allocVector(INTSXP, 1));
    count_protect++;
    _bin_y_on_x_with_breaks_mean(REAL(x), REAL(y), p_brks, p_ans, n_bin,
				 from_idx, to_idx);
    break;
  default:
    if (get_index) {
      PROTECT(index = allocVector(INTSXP, n_bin));
      p_index = INTEGER(index);
      _bin_y_on_x_with_breaks_max_idx(REAL(x), REAL(y), p_brks, p_ans, n_bin,
				      from_idx, to_idx, p_index);
      have_index = 1;
    } else {
      PROTECT(index = allocVector(INTSXP, 1));
      _bin_y_on_x_with_breaks_max(REAL(x), REAL(y), p_brks, p_ans, n_bin,
				  from_idx, to_idx);
    }
    count_protect++;
  }

  /* Replace NAs with the "default" value. */
  if (!ISNA(base_value)) {
    _fill_missing_with_value(p_ans, base_value, n_bin);
  }

  int current_idx = 0;
  /* Now create the result list. */
  PROTECT(ans_list = allocVector(VECSXP, (1 + get_x + have_index)));
  count_protect++;
  /* Setting names */
  PROTECT(names = allocVector(STRSXP, (1+ get_x + have_index)));
  count_protect++;
  if (get_x) {
    /* Calculate bin mid-points */
    PROTECT(bin_mids = allocVector(REALSXP, n_bin));
    count_protect++;
    _bin_midPoint(p_brks, REAL(bin_mids), n_bin);
    SET_VECTOR_ELT(ans_list, current_idx, bin_mids);
    SET_STRING_ELT(names, current_idx, mkChar("x"));
    current_idx++;
  }
  SET_VECTOR_ELT(ans_list, current_idx, ans);
  SET_STRING_ELT(names, current_idx, mkChar("y"));
  current_idx++;
  /* For max and min we can also return the index of the min/max for each bin. */
  if (have_index) {
    SET_STRING_ELT(names, current_idx, mkChar("index"));
    SET_VECTOR_ELT(ans_list, current_idx, index);
  }
  setAttrib(ans_list, R_NamesSymbol, names);
  
  UNPROTECT(count_protect);
  return ans_list;
}

/*
 * Performs binning on sub-sets of x and y vectors. Subsets are defined with
 * arguments subsetFromIdx, subsetToIdx speficying the (0-based) start and end
 * index of the subsets.
 * Returns a list, each element containing the results from the binning within
 * each sub-set.
 * Get fromX and toX if not provided from the vector x.
 */
SEXP binYonX_multi(SEXP x, SEXP y, SEXP breaks, SEXP nBins, SEXP binSize,
		   SEXP fromX, SEXP toX, SEXP subsetFromIdx, SEXP subsetToIdx,
		   SEXP shiftByHalfBinSize, SEXP method,
		   SEXP baseValue, SEXP getIndex, SEXP getX) {
  SEXP res, from_idx, to_idx, current_res;
  int n_subsets, *p_subset_from_idx, *p_subset_to_idx;
  if (LENGTH(subsetFromIdx) != LENGTH(subsetToIdx)) {
    error("Arguments 'subsetFromIdx' and 'subsetToIdx' have to have the same length!");
  }
  n_subsets = LENGTH(subsetFromIdx);
  p_subset_from_idx = INTEGER(subsetFromIdx);
  p_subset_to_idx = INTEGER(subsetToIdx);

  PROTECT(res = allocVector(VECSXP, n_subsets));

  /* Loop through the subsets and perform the binning.
   * If breaks is not provided, we have to infer the fromX and toX
   * from the data in each loop and overwrite eventually pre-defined values.
   */
  for (int i = 0; i < n_subsets; i++) {
    PROTECT(from_idx = ScalarInteger(p_subset_from_idx[i]));
    PROTECT(to_idx = ScalarInteger(p_subset_to_idx[i]));
    PROTECT(current_res = binYonX(x, y, breaks, nBins, binSize, fromX, toX,
    				  from_idx, to_idx, shiftByHalfBinSize,
    				  method, baseValue, getIndex, getX));
    SET_VECTOR_ELT(res, i, current_res);
    UNPROTECT(3);
  }

  UNPROTECT(1);
  return(res);
}

SEXP impute_with_linear_interpolation(SEXP x, SEXP noInterAtEnds) {
  SEXP x_copy;
  int x_length = LENGTH(x);
  int no_inter_at_ends = asInteger(noInterAtEnds);
  double *p_x;
  x_copy = PROTECT(duplicate(x));
  p_x = REAL(x_copy);
  _impute_linearly_interpolate_x(p_x, x_length, no_inter_at_ends);
  UNPROTECT(1);
  return x_copy;
}

SEXP impute_with_linear_interpolation_base(SEXP x, SEXP baseValue, SEXP interBin) {
  SEXP x_copy;
  int x_length = LENGTH(x);
  double *p_x, base_value;
  int inter_bin = asInteger(interBin);
  base_value = REAL(baseValue)[0];

  x_copy = PROTECT(duplicate(x));
  p_x = REAL(x_copy);
  _impute_linearly_interpolate_base_x(p_x, x_length, base_value, inter_bin);
  UNPROTECT(1);
  return x_copy;
}

/*
 * Get breaks given from x, to x and number of bins.
 */
SEXP breaks_on_nBins(SEXP fromX, SEXP toX, SEXP nBins, SEXP shift) {
  SEXP ans;
  int n_bin;
  double from_x, to_x;
  n_bin = asInteger(nBins);
  from_x = REAL(fromX)[0];
  to_x = REAL(toX)[0];
  PROTECT(ans = allocVector(REALSXP, n_bin + 1));
  _breaks_on_nBins(from_x, to_x, n_bin, REAL(ans), asInteger(shift));
  UNPROTECT(1);
  return ans;
}

/*
 * Get breaks given from x, to x and number of bins.
 */
SEXP breaks_on_binSize(SEXP fromX, SEXP toX, SEXP binSize) {
  SEXP ans;
  int n_bin;
  double from_x, to_x, bin_size;
  bin_size = REAL(binSize)[0];
  from_x = REAL(fromX)[0];
  to_x = REAL(toX)[0];
  /* Use round to define the number of bins.
   */
  n_bin = (int)round((to_x - from_x) / bin_size);
  PROTECT(ans = allocVector(REALSXP, n_bin + 1));
  _breaks_on_binSize(from_x, to_x, n_bin, bin_size, REAL(ans));
  UNPROTECT(1);
  return ans;
}

/*
 * ----------------------- INTERNAL FUNCTIONS -----------------------
 */

/*
 * Create breaks for binning: seq(from_val, to_val, length.out = (n_bin + 1))
 * shift_by_half_bin_size: either 0 or 1, if 1 the bin mid-points will be shifted left
 * by half of the bin_size.
 */
void _breaks_on_nBins(double from_val, double to_val, int n_bin,
		      double *brks, int shift_by_half_bin_size) {
  int i;
  double current_val, bin_size;

  /* If we're going to shift the bin mid-points we have to ensure that the bin_size
   * will be slightly larger to include also the to_val (+ bin_size/2).
   */
  if (shift_by_half_bin_size > 0) {
    bin_size = (to_val - from_val) / (double)(n_bin - 1.0f);
    current_val = from_val - (bin_size / 2.0f);
  } else {
    bin_size = (to_val - from_val) / (double)n_bin;
    current_val = from_val;
  }
  for (i = 0; i <= n_bin; i++) {
    /* Use multiplication to be conform with R*/
    brks[i] = current_val + i * bin_size;
    /* brks[i] = current_val; */
    /* current_val = current_val + bin_size; */
  }
  return;
}


/*
 * Create breaks for binning: seq(from_val, to_val, by = bin_size).
 */
void _breaks_on_binSize(double from_val, double to_val, int n_bin,
			double bin_size, double *brks) {
  // We have to make a decision here, the max break should be to_val!
  // no of breaks: ceil(from_val - to_val / bin_size)
  // We have to assume that the *brks array has already been sized to the
  // correct length (i.e. ceil((from_val - to_val) / bin_size))
  int idx = 0;
  for (int i = 0; i < n_bin; i++) {
    brks[i] = from_val + i * bin_size;
  }
  /* while (current_val < to_val) { */
  /*   brks[idx] = from_val + idx * bin_size; */
  /*   /\* current_val = current_val + bin_size; *\/ */
  /*   idx++; */
  /* } */
  // The last one should be to_val.
  brks[n_bin] = to_val;
  return;

  /* current_val = from_val; */
  /* for (int i = 0; i < n_bin; i++) { */
  /*   brks[i] = current_val; */
  /*   current_val += bin_size; */
  /* } */
  /* brks[n_bin] = to_val; */
  /* return; */
}

/*
 * Bin Y on X based on defined breaks.
 * x and breaks are expected to be sorted incrementally.
 * brks is supposed to be an array with length n_bin + 1 defining the start
 * and end values for the bins.
 * ans is an array with length = n_bin which should have been initialized
 * with 0s!
 * x_start_idx and x_end_idx are the start and end indices in array x in
 * which we're looking for values to be within the bins. This allows to
 * bin on a subset of the x/y arrays. We suppose these have been checked
 * BEFORE (i.e. both being positive and x_end_idx <= length(x)).
 * NA handling: skips any NAs in y.
 * *index keeps track of the index of the max value within each bin in x.
 */
static void _bin_y_on_x_with_breaks_max_idx(double *x, double *y, double *brks,
					    double *ans, int n_bin, int x_start_idx,
					    int x_end_idx, int *index)
{
  int x_current_idx, last_bin_idx;
  double x_current_value;
  last_bin_idx = n_bin - 1;
  x_current_idx = x_start_idx;

  // o Loop through the bins/brks
  for (int i = 0; i < n_bin; i++) {
    index[i] = NA_INTEGER;
    // loop through the x values; assumes x sorted increasingly
    while (x_current_idx <= x_end_idx) {
      x_current_value = x[x_current_idx];
      if (x_current_value >= brks[i]) {
	/* OK, now check if the value is smaller the upper border
	 * OR if we're in the last bin, whether the value matches the upper border.
	 */
	if ((x_current_value < brks[i + 1]) || (x_current_value == brks[i + 1] &&
					       i == last_bin_idx)) {
	  /* Check if the corresponding y value is larger than the one we have and
	   * replace if so.
	   * NA handling: is the current y value is NA, ignore it (na.rm = TRUE),
	   * if the current bin value is NA, replace it automatically.
	   */
	  if (!ISNA(y[x_current_idx])) {
	    if (ISNA(ans[i]) || (y[x_current_idx] > ans[i])) {
	      ans[i] = y[x_current_idx];
	      index[i] = x_current_idx + 1;
	    }
	  }
	} else {
	  /* Break without incrementing the x_current_idx, thus the same value will
	   * be evaluated for the next bin i.
	   */
	  break;
	}
      }
      x_current_idx++;
    }
  }
  return;
}

static void _bin_y_on_x_with_breaks_max(double *x, double *y, double *brks,
					double *ans, int n_bin, int x_start_idx,
					int x_end_idx)
{
  int x_current_idx, last_bin_idx;
  double x_current_value;
  last_bin_idx = n_bin - 1;
  x_current_idx = x_start_idx;

  // o Loop through the bins/brks
  for (int i = 0; i < n_bin; i++) {
    // loop through the x values; assumes x sorted increasingly
    while (x_current_idx <= x_end_idx) {
      x_current_value = x[x_current_idx];
      if (x_current_value >= brks[i]) {
	/* OK, now check if the value is smaller the upper border
	 * OR if we're in the last bin, whether the value matches the upper border.
	 */
	if ((x_current_value < brks[i + 1]) || (x_current_value == brks[i + 1] &&
					       i == last_bin_idx)) {
	  /* Check if the corresponding y value is larger than the one we have and
	   * replace if so.
	   * NA handling: is the current y value is NA, ignore it (na.rm = TRUE),
	   * if the current bin value is NA, replace it automatically.
	   */
	  if (!ISNA(y[x_current_idx])) {
	    if (ISNA(ans[i]) || (y[x_current_idx] > ans[i])) {
	      ans[i] = y[x_current_idx];
	    }
	  }
	} else {
	  /* Break without incrementing the x_current_idx, thus the same value will
	   * be evaluated for the next bin i.
	   */
	  break;
	}
      }
      x_current_idx++;
    }
  }
  return;
}

static void _bin_y_on_x_with_breaks_min(double *x, double *y, double *brks,
					double *ans, int n_bin, int x_start_idx,
					int x_end_idx)
{
  int x_current_idx, last_bin_idx;
  double x_current_value;
  last_bin_idx = n_bin - 1;
  x_current_idx = x_start_idx;

  // o Loop through the bins/brks
  for (int i = 0; i < n_bin; i++) {
    // loop through the x values; assumes x sorted increasingly
    while (x_current_idx <= x_end_idx) {
      x_current_value = x[x_current_idx];
      if (x_current_value >= brks[i]) {
	/* OK, now check if the value is smaller the upper border
	 * OR if we're in the last bin, whether the value matches the upper border.
	 */
	if ((x_current_value < brks[i + 1]) || (x_current_value == brks[i + 1] &&
					       i == last_bin_idx)) {
	  /*
	   * NA handling: is the current y value is NA, ignore it (na.rm = TRUE),
	   * if the current bin value is NA, replace it automatically.
	   */
	  if (!ISNA(y[x_current_idx])) {
	    if (ISNA(ans[i]) || (y[x_current_idx] < ans[i])) {
	      ans[i] = y[x_current_idx];
	    }
	  }
	} else {
	  /* Break without incrementing the x_current_idx, thus the same value will
	   * be evaluated for the next bin i.
	   */
	  break;
	}
      }
      x_current_idx++;
    }
  }
  return;
}

static void _bin_y_on_x_with_breaks_min_idx(double *x, double *y, double *brks,
					    double *ans, int n_bin, int x_start_idx,
					    int x_end_idx, int *index)
{
  int x_current_idx, last_bin_idx;
  double x_current_value;
  last_bin_idx = n_bin - 1;
  x_current_idx = x_start_idx;

  // o Loop through the bins/brks
  for (int i = 0; i < n_bin; i++) {
    index[i] = NA_INTEGER;
    // loop through the x values; assumes x sorted increasingly
    while (x_current_idx <= x_end_idx) {
      x_current_value = x[x_current_idx];
      if (x_current_value >= brks[i]) {
	/* OK, now check if the value is smaller the upper border
	 * OR if we're in the last bin, whether the value matches the upper border.
	 */
	if ((x_current_value < brks[i + 1]) || (x_current_value == brks[i + 1] &&
					       i == last_bin_idx)) {
	  /*
	   * NA handling: is the current y value is NA, ignore it (na.rm = TRUE),
	   * if the current bin value is NA, replace it automatically.
	   */
	  if (!ISNA(y[x_current_idx])) {
	    if (ISNA(ans[i]) || (y[x_current_idx] < ans[i])) {
	      ans[i] = y[x_current_idx];
	      index[i] = x_current_idx + 1;
	    }
	  }
	} else {
	  /* Break without incrementing the x_current_idx, thus the same value will
	   * be evaluated for the next bin i.
	   */
	  break;
	}
      }
      x_current_idx++;
    }
  }
  return;
}

static void _bin_y_on_x_with_breaks_sum(double *x, double *y, double *brks,
					double *ans, int n_bin, int x_start_idx,
					int x_end_idx) {
  int x_current_idx, last_bin_idx;
  double x_current_value;
  last_bin_idx = n_bin - 1;
  x_current_idx = x_start_idx;

  // o Loop through the bins/brks
  for (int i = 0; i < n_bin; i++) {
    // loop through the x values; assumes x sorted increasingly
    while (x_current_idx <= x_end_idx) {
      x_current_value = x[x_current_idx];
      if (x_current_value >= brks[i]) {
	/* OK, now check if the value is smaller the upper border
	 * OR if we're in the last bin, whether the value matches the upper border.
	 */
	if ((x_current_value < brks[i + 1]) || (x_current_value == brks[i + 1] &&
					       i == last_bin_idx)) {
	  /*
	   * NA handling: is the current y value is NA, ignore it (na.rm = TRUE),
	   * if the current bin value is NA, replace it automatically.
	   */
	  if (!ISNA(y[x_current_idx])) {
	    if(ISNA(ans[i])) {
	      ans[i] = y[x_current_idx];
	    } else {
	      ans[i] = ans[i] + y[x_current_idx];
	    }
	  }
	} else {
	  /* Break without incrementing the x_current_idx, thus the same value will
	   * be evaluated for the next bin i.
	   */
	  break;
	}
      }
      x_current_idx++;
    }
  }
  return;
}

static void _bin_y_on_x_with_breaks_mean(double *x, double *y, double *brks,
					 double *ans, int n_bin, int x_start_idx,
					 int x_end_idx) {
  int x_current_idx, last_bin_idx;
  double x_current_value;
  last_bin_idx = n_bin - 1;
  x_current_idx = x_start_idx;
  // make an element counter.
  int el_count[n_bin];

  // o Loop through the bins/brks
  for (int i = 0; i < n_bin; i++) {
    el_count[i] = 0;
    // loop through the x values; assumes x sorted increasingly
    while (x_current_idx <= x_end_idx) {
      x_current_value = x[x_current_idx];
      if (x_current_value >= brks[i]) {
	/* OK, now check if the value is smaller the upper border
	 * OR if we're in the last bin, whether the value matches the upper border.
	 */
	if ((x_current_value < brks[i + 1]) || (x_current_value == brks[i + 1] &&
					       i == last_bin_idx)) {
	  /*
	   * NA handling: is the current y value is NA, ignore it (na.rm = TRUE),
	   * if the current bin value is NA, replace it automatically.
	   */
	  if (!ISNA(y[x_current_idx])) {
	    if(ISNA(ans[i])) {
	      ans[i] = y[x_current_idx];
	    } else {
	      ans[i] = ans[i] + y[x_current_idx];
	    }
	    el_count[i] = el_count[i] + 1;
	  }
	} else {
	  /* Break without incrementing the x_current_idx, thus the same value will
	   * be evaluated for the next bin i.
	   */
	  break;
	}
      }
      x_current_idx++;
    }
  }
  // Now dividing by el_count.
  for (int i = 0; i < n_bin; i++) {
    if (el_count[i] > 0)
      ans[i] = ans[i] / (double)el_count[i];
  }
  return;
}




static void _bin_midPoint(double *brks, double *bin_mids, int n_bin) {
  for (int i = 0; i < n_bin; i++) {
    bin_mids[i] = (brks[i] + brks[i+1]) / 2;
  }
  return;
}


/* Simply replace all NAs in a double vector with the provided init value */
static void _fill_missing_with_value(double *ans, double init_value, int n_bin) {
  if (!ISNA(init_value)) {
    for(int i = 0; i < n_bin; i++) {
      if (ISNA(ans[i]))
	ans[i] = init_value;
    }
  }
  return;
}

/*
 * This linearly interpolates missing bin values based on the non-missing bin
 * values left and right of the bin. This uses the already binned values,
 * but it would be easy to consider the actual values BEFORE binning easily
 * by just storing the first and last value for each bin and using these.
 * That would actually be better, but does not fit with the original
 * xcms binLin implementation.
 */
static void _impute_linearly_interpolate_x(double *x, int n_bin, int no_interpol_at_ends) {
  int is_empty_bin = 0;  // To check whether we are in a series of empty bins.
  int start_x = 0;  // index of the first empty bin of a potential empty bin series.
  int idx_last_non_empty_bin = -1;  // index of the last bin with a value.
  int current_x = 0;
  double base_value = 0;
  double last_bin_value = base_value;
  //double incrementer = 0;
  //double new_value;
  while (current_x < n_bin) {
    if (ISNA(x[current_x])) {
      /* Found a bin without a value, now I have to look for the next with
       * a value. If the first one is empty, I have to set start_x to -1.
       */
      if (is_empty_bin == 0) {
	start_x = current_x;
	is_empty_bin = 1;
      }
    } else {
      if (is_empty_bin == 1) {
	/* Gotcha! Will use last_bin_value and this one to interpolate the
	 * missing bins in between. First calculate the increment per bin and
	 * then loop through the empty bin to fill with values.
	 */
	if (idx_last_non_empty_bin < 0 && no_interpol_at_ends > 0) {
	  /* No interpolation at the start, fill with base_value */
	  for (int i = start_x; i < current_x; i++) {
	    x[i] = base_value;
	  }
	} else {
	  for (int i = start_x; i < current_x; i++) {
	    x[i] = last_bin_value + (x[current_x] - last_bin_value) /
	      (double)(current_x - idx_last_non_empty_bin) * (double)(i - idx_last_non_empty_bin);
	  }
	}
      }
      /* Keep track of the last non-empty bin value. */
      last_bin_value = x[current_x];
      is_empty_bin = 0;
      idx_last_non_empty_bin = current_x;
    }
    current_x++;
  }
  /* Check if the last bin is empty too */
  if (is_empty_bin == 1) {
    if (no_interpol_at_ends > 0) {
      for (int i = start_x; i < current_x; i++) {
	x[i] = base_value;
      }
    } else {
      for (int i = start_x; i < current_x; i++) {
	x[i] = last_bin_value + (base_value - last_bin_value) /
	  (double)(current_x - idx_last_non_empty_bin) * (double)(i - idx_last_non_empty_bin);
      }
    }
  }
  return;
}

/*
 * The name might be misleading; this does essentially what the binLinBase from xcms
 * does/did, i.e. setting empty bin values to a baselevel and interpolating bin
 * values for empty bins if they are close enough to non-empty bins.
 *
 * - loop through the bins.
 * - There are 3 cases:
 *   1) interpolate from a bin value to base level
 *   2) interpolate form base level to a bin value
 *   3) interpolate from a bin value to a bin value: if the difference between the indices
 *      is smaller 2 * ibase + 1
 * base_value is the base value to which we set all empty bins, if we're not intepolating.
 * inter_bin is the number of neighboring bins for an empty one which we consider for
 *   interpolation.
 */
static void _impute_linearly_interpolate_base_x(double *x, int n_bin, double base_value,
						int inter_bin) {
  /*
   * base_value: the value to be inserted into empty bins, if not imputed by interpolation.
   * inter_bin: for how many neighboring bins should we interpolate. If 1: only one empty bin
   *   next to a non-empty bin is interpolated. If the index between two non-empty bins is
   *   <= 2 * inter_bin they are directly interpolated.
   */
  int is_empty_bin = 0;            // To check whether we are in a series of empty bins.
  int idx_start_empty_bin = 0;     // index of the first empty bin of a potential empty bin series.
  int idx_last_non_empty_bin = -1; // index of the last non-empty bin.
  int current_idx = 0;             // the current index; incremented in loop.
  double last_bin_value = base_value;  // the value of the last non-empty bin.
  //jo double incrementer = 0;
  //jo double new_value;

  while (current_idx < n_bin) {
    if (ISNA(x[current_idx])) {
      /* Found a bin without a value, now I have to look for the next with
       * a value.
       */
      if (is_empty_bin == 0) {
	idx_start_empty_bin = current_idx;
	is_empty_bin = 1;
      }
    } else {
      if (is_empty_bin == 1) {
	/* Gotcha! Depending on the difference between the current index and the one from the
	 * last non-empty bin I'm going to decide whether we're going to interpolate or not.
	 * Loop throught the empty bin(s) and decide which value to add.
	 */
	for (int i = idx_start_empty_bin; i < current_idx; i++) {
	  if (idx_last_non_empty_bin == -1 && ((current_idx - i) <= inter_bin)) {
	    /* That's the special case when the stretch of empty values starts
	     * at the beginning of the vector. We're interpolating from base_value to
	     * the current value.*/
	    x[i] = x[current_idx] + (x[current_idx] - base_value) /
	      (double)(inter_bin + 1) * (double)(i - current_idx);
	  }
	  else if ((idx_last_non_empty_bin >= 0) &&
		   ((current_idx - idx_last_non_empty_bin) <= (2 * inter_bin + 1))) {
	    /* Interpolating between two non-empty bins as they are close enough. */
	    x[i] = last_bin_value + (x[current_idx] - last_bin_value) /
	      (double)(current_idx - idx_last_non_empty_bin) *
	      (double)(i - idx_last_non_empty_bin);
	  } else if ((idx_last_non_empty_bin >= 0) &&
		     ((i - idx_last_non_empty_bin) <= inter_bin) &&
		     ((current_idx - i) > inter_bin)) {
	    /* Current bin is close enough to last non-empty bin and far enough from
	     * next non-empty bin: interpolate from last value to base value */
	    x[i] = last_bin_value + (base_value - last_bin_value) /
	      (double)(inter_bin + 1) *
	      (double)(i - idx_last_non_empty_bin);
	  } else if ((idx_last_non_empty_bin >= 0) &&
		     ((i - idx_last_non_empty_bin) > inter_bin) &&
		     ((current_idx - i) <= inter_bin)) {
	    /* Current bin is far enough from last non-empty bin and close enough to
	     * next non-empty bin: interpolate from base value to current value. */
	    x[i] = x[current_idx] + (x[current_idx] - base_value) /
	      (double)(inter_bin + 1) * (double)(i - current_idx);
	  } else {
	    /* Just set the base value. */
	    x[i] = base_value;
	  }
	}
      }
      /* Keep track of the last non-empty bin value. */
      last_bin_value = x[current_idx];
      idx_last_non_empty_bin = current_idx;
      is_empty_bin = 0;
    }
    current_idx++;
  }
  /* If the last bin is empty. In that case we assume that the next value is = base_value. */
  if (is_empty_bin == 1) {
    //jo double next_bin_value = base_value;
    for (int i = idx_start_empty_bin; i < current_idx; i++) {
      if ((i - idx_last_non_empty_bin) <= inter_bin) {
	x[i] = last_bin_value + (base_value - last_bin_value) /
	  (double)(inter_bin + 1) * (double)(i - idx_last_non_empty_bin);
      } else {
	x[i] = base_value;
      }
    }
  }

  /* Check if the last bin is empty too */
  /* if (is_empty_bin == 1) { */
  /* 	incrementer = (base_value - last_bin_value) / (double)(current_x - start_x + 1); */
  /* 	for (int i = start_x; i <= current_x; i++) { */
  /* 	  last_bin_value = last_bin_value + incrementer; */
  /* 	  x[i] = last_bin_value; */
  /* 	} */
  /* } */
  return;
}

/*
 * Some simple functions to check passing of arguments.
 */
SEXP test_integer(SEXP x) {
  int x_val = asInteger(x);
  Rprintf("input asInteger(x): %d\n", x_val);

  //
  int *p_ans;
  int *p_x = INTEGER(x);
  Rprintf("getting the first value from the pointer: %d\n", p_x[0]);

  SEXP ans = allocVector(INTSXP, LENGTH(x));
  p_ans = INTEGER(ans);
  p_ans[0] = x_val;
  return ans;
}

SEXP test_real(SEXP x) {
  int x_val = asReal(x);
  Rprintf("input asReal(x): %f\n", x_val);

  //
  double *p_ans;
  double *p_x = REAL(x);
  Rprintf("getting the first value from the pointer: %f\n", p_x[0]);

  SEXP ans = allocVector(REALSXP, LENGTH(x));
  p_ans = REAL(ans);
  p_ans[0] = x_val;
  return ans;
}


