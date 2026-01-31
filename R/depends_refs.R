#' melt
#' Convert an object into a molten data frame. This function is from reshape2 package.
#' 
#' This the generic melt function. See the following functions
#' for the details about different data structures:
#' @param data Data set to melt
#' @param na.rm Should NA values be removed from the data set? This will
#'   convert explicit missings to implicit missings.
#' @param ... further arguments passed to or from other methods.
#' @param value.name name of variable used to store values
melt <- function(data, ..., na.rm = FALSE, value.name = "value") {
  UseMethod("melt", data)
}


melt.default <- function(data, ..., na.rm = FALSE, value.name = "value") {
  if (na.rm) data <- data[!is.na(data)]
  setNames(data.frame(data), value.name)
}

melt.list <- function(data, ..., level = 1) {
  require(plyr);
  parts <- lapply(data, melt.default, level = level + 1, ...)
  result <- rbind.fill(parts)
  
  # Add labels
  names <- names(data) %||% seq_along(data)
  lengths <- vapply(parts, nrow, integer(1))
  labels <- rep(names, lengths)
  
  label_var <- attr(data, "varname") %||% paste("L", level, sep = "")
  result[[label_var]] <- labels
  
  # result <- cbind(labels, result)
  # result[, c(setdiff(names(result), "value"), "value")]
  
  result
}


melt.data.frame <- function(data, 
                            id.vars, 
                            measure.vars, 
                            variable.name = "variable", ..., 
                            na.rm = FALSE, 
                            value.name = "value", 
                            factorsAsStrings = TRUE) {
  
  ## Get the names of id.vars, measure.vars
  vars <- melt_check(data, id.vars, measure.vars, variable.name, value.name)
  
  ## Match them to indices in the data
  id.ind <- match(vars$id, names(data))
  measure.ind <- match(vars$measure, names(data))
  
  ## Return early if we have id.ind but no measure.ind
  if (!length(measure.ind)) {
    return(data[id.vars])
  }
  
  ## Get the attributes if common, NULL if not.
  args <- normalize_melt_arguments(data, measure.ind, factorsAsStrings)
  measure.attributes <- args$measure.attributes
  factorsAsStrings <- args$factorsAsStrings
  valueAsFactor <- "factor" %in% measure.attributes$class
  
  df <- melt_dataframe(
    data,
    as.integer(id.ind-1),
    as.integer(measure.ind-1),
    as.character(variable.name),
    as.character(value.name),
    as.pairlist(measure.attributes),
    as.logical(factorsAsStrings),
    as.logical(valueAsFactor)
  )
  
  if (na.rm) {
    return(df[ !is.na(df[[value.name]]), ])
  } else {
    return(df)
  }
}


melt.array <- function(data, varnames = names(dimnames(data)), ...,
                       na.rm = FALSE, as.is = FALSE, value.name = "value") {
  var.convert <- function(x) {
    if (!is.character(x)) return(x)
    
    x <- type.convert(x, as.is = TRUE)
    if (!is.character(x)) return(x)
    
    factor(x, levels = unique(x))
  }
  
  dn <- plyr::amv_dimnames(data)
  names(dn) <- varnames
  if (!as.is) {
    dn <- lapply(dn, var.convert)
  }
  
  labels <- expand.grid(dn, KEEP.OUT.ATTRS = FALSE,
                        stringsAsFactors = FALSE)
  
  if (na.rm) {
    missing <- is.na(data)
    data <- data[!missing]
    labels <- labels[!missing, ]
  }
  
  value_df <- setNames(data.frame(as.vector(data)), value.name)
  cbind(labels, value_df)
}

melt.table <- melt.array

melt.matrix <- melt.array

melt_check <- function(data, id.vars, measure.vars, variable.name, value.name) {
  varnames <- names(data)
  
  # Convert positions to names
  if (!missing(id.vars) && is.numeric(id.vars)) {
    id.vars <- varnames[id.vars]
  }
  if (!missing(measure.vars) && is.numeric(measure.vars)) {
    measure.vars <- varnames[measure.vars]
  }
  
  # Check that variables exist
  if (!missing(id.vars)) {
    unknown <- setdiff(id.vars, varnames)
    if (length(unknown) > 0) {
      vars <- paste(unknown, collapse=", ")
      stop("id variables not found in data: ", vars, call. = FALSE)
    }
  }
  
  if (!missing(measure.vars)) {
    unknown <- setdiff(measure.vars, varnames)
    if (length(unknown) > 0) {
      vars <- paste(unknown, collapse=", ")
      stop("measure variables not found in data: ", vars, call. = FALSE)
    }
  }
  
  # Fill in missing pieces
  if (missing(id.vars) && missing(measure.vars)) {
    discrete <- sapply(data, plyr::is.discrete)
    id.vars <- varnames[discrete]
    measure.vars <- varnames[!discrete]
    if (length(id.vars) != 0) {
      message("Using ", paste(id.vars, collapse = ", "), " as id variables")
    } else {
      message("No id variables; using all as measure variables")
    }
  } else if (missing(id.vars)) {
    id.vars <- setdiff(varnames, measure.vars)
  } else if (missing(measure.vars)) {
    measure.vars <- setdiff(varnames, id.vars)
  }
  
  # Ensure variable names are characters of length one
  if (!is.string(variable.name))
    stop("'variable.name' should be a string", call. = FALSE)
  if (!is.string(value.name))
    stop("'value.name' should be a string", call. = FALSE)
  
  list(id = id.vars, measure = measure.vars)
}
melt_dataframe <- function(data, id_ind, measure_ind, variable_name, value_name, measure_attributes, factorsAsStrings, valueAsFactor) {
    # Pro bypass: use MetaboAnalystR backend to avoid XiaLabCppLib in Master session
    if(exists(".pro.cpp.bypass", envir = .GlobalEnv) && isTRUE(get(".pro.cpp.bypass", envir = .GlobalEnv))){
        res <- .Call('_MetaboAnalystR_melt_dataframe', PACKAGE = 'MetaboAnalystR',
            data, id_ind, measure_ind, variable_name, value_name,
            measure_attributes, factorsAsStrings, valueAsFactor)
    } else if(.on.public.web){
        require("XiaLabCppLib")
        res <- .Call('_XiaLabCppLib_melt_dataframe', PACKAGE = 'XiaLabCppLib',
            data, id_ind, measure_ind, variable_name, value_name,
            measure_attributes, factorsAsStrings, valueAsFactor)
    } else {
        res <- .Call('_MetaboAnalystR_melt_dataframe', PACKAGE = 'MetaboAnalystR',
            data, id_ind, measure_ind, variable_name, value_name,
            measure_attributes, factorsAsStrings, valueAsFactor)
    }
}
normalize_melt_arguments <- function(data, measure.ind, factorsAsStrings) {
  
  measure.attributes <- lapply(measure.ind, function(i) {
    attributes(data[[i]])
  })
  
  ## Determine if all measure.attributes are equal
  measure.attrs.equal <- all_identical(measure.attributes)
  
  if (measure.attrs.equal) {
    measure.attributes <- measure.attributes[[1]]
  } else {
    warning("attributes are not identical across measure variables; ",
            "they will be dropped", call. = FALSE)
    measure.attributes <- NULL
  }
  
  if (!factorsAsStrings && !measure.attrs.equal) {
    warning("cannot avoid coercion of factors when measure attributes not identical",
            call. = FALSE)
    factorsAsStrings <- TRUE
  }
  
  ## If we are going to be coercing any factors to strings, we don't want to
  ## copy the attributes
  any.factors <- any( sapply( measure.ind, function(i) {
    is.factor( data[[i]] )
  }))
  
  if (factorsAsStrings && any.factors) {
    measure.attributes <- NULL
  }
  
  list(
    measure.attributes = measure.attributes,
    factorsAsStrings = factorsAsStrings
  )
  
}
is.string <- function(x) {
  is.character(x) && length(x) == 1
}
all_identical <- function(xs) {
  if (length(xs) <= 1) return(TRUE)
  for (i in seq(2, length(xs))) {
    if (!identical(xs[[1]], xs[[i]])) return(FALSE)
  }
  TRUE
}
"%||%" <- function(a, b) if (!is.null(a)) a else b

#' vars_pull
#' vars_pull is from tidyselect package
#' @noRd
#' @importFrom rlang eval_tidy
#' @importFrom vctrs vec_as_subscript2 vec_as_location2
vars_pull <- function (vars, var = -1, error_call = rlang::caller_env(), error_arg = rlang::caller_arg(var)) {
    expr <- tidyselect::enquo(var)
    if (rlang::quo_is_missing(expr)) {
        cli::cli_abort("{.arg var} is absent but must be supplied.", 
            call = error_call)
    }
    tidyselect:::local_vars(vars)
    n <- length(vars)
    tidyselect:::with_chained_errors(loc <- rlang::eval_tidy(expr,  rlang::set_names(seq_along(vars), 
        vars)), call = error_call, eval_expr = expr)
    loc <- pull_as_location2(loc, n, vars)
    if (loc < 0L) {
        loc <- n + 1L + loc
    }
    vars[[loc]]
}

pull_as_location2 <- function(i, n, names) {
  tidyselect:::with_subscript_errors(type = "pull", {
    i <- vctrs::vec_as_subscript2(i, arg = "var", logical = "error")
    
    if (is.numeric(i)) {
      vctrs::num_as_location2(
        i,
        n = n,
        negative = "ignore",
        arg = "var"
      )
    } else {
      vctrs::vec_as_location2(
        i,
        n = n,
        names = names,
        arg = "var"
      )
    }
  })
}

