#' Arrow Utilities for Zero-Copy Java Integration
#' Part of the Rserve/qs to Apache Arrow migration
#' @author MetaboAnalyst Team

#' Save object to both qs and Arrow formats
#' Use this for numeric matrices where all columns are the same type
#' @param obj Matrix or data frame to save
#' @param file Path to .qs file (Arrow path derived by replacing .qs with .arrow)
shadow_save <- function(obj, file) {
  # Always save qs format for backward compatibility
  qs::qsave(obj, file)

  # Derive Arrow path
  arrow_path <- sub("\\.qs$", ".arrow", file)

  tryCatch({
    if (is.matrix(obj) || is.data.frame(obj)) {
      df <- as.data.frame(obj)
      rn <- rownames(obj)
      if (!is.null(rn)) {
        # Prepend row_names_id as first column
        df <- cbind(row_names_id = as.character(rn), df)
      }
      arrow::write_feather(df, arrow_path, compression = "uncompressed")
    }
  }, error = function(e) {
    warning(paste("Arrow save failed:", e$message))
  })
}

#' Save object with mixed types (strings and numerics)
#' Handles factors by converting to character
#' @param obj Data frame or matrix to save
#' @param file Path to .qs file
shadow_save_mixed <- function(obj, file) {
  # Always save qs format for backward compatibility
  qs::qsave(obj, file)

  # Derive Arrow path
  arrow_path <- sub("\\.qs$", ".arrow", file)

  tryCatch({
    if (is.matrix(obj) || is.data.frame(obj)) {
      df <- as.data.frame(obj, stringsAsFactors = FALSE)

      # Convert factors to character for Arrow compatibility
      for (col in names(df)) {
        if (is.factor(df[[col]])) {
          df[[col]] <- as.character(df[[col]])
        }
      }

      rn <- rownames(obj)
      if (!is.null(rn)) {
        # Prepend row_names_id as first column
        df <- cbind(row_names_id = as.character(rn), df)
      }
      arrow::write_feather(df, arrow_path, compression = "uncompressed")
    }
  }, error = function(e) {
    warning(paste("Arrow save (mixed) failed:", e$message))
  })
}

#' Export normalized data matrix to Arrow format
#' Called after normalization to enable zero-copy access
#' @param mSetObj mSet object
#' @return 1 on success, 0 on failure
ExportNormDataArrow <- function(mSetObj = NA) {
  mSetObj <- .get.mSet(mSetObj)

  tryCatch({
    if (!is.null(mSetObj$dataSet$norm)) {
      mat <- mSetObj$dataSet$norm
      df <- as.data.frame(mat)
      rn <- rownames(mat)
      if (!is.null(rn)) {
        df <- cbind(row_names_id = as.character(rn), df)
      }
      arrow::write_feather(df, "norm_data.arrow", compression = "uncompressed")
      return(1)
    }
    return(0)
  }, error = function(e) {
    warning(paste("ExportNormDataArrow failed:", e$message))
    return(0)
  })
}

#' Export prenorm data matrix to Arrow format
#' Called during data preparation
#' @param mSetObj mSet object
#' @return 1 on success, 0 on failure
ExportPrenormDataArrow <- function(mSetObj = NA) {
  mSetObj <- .get.mSet(mSetObj)

  tryCatch({
    if (!is.null(mSetObj$dataSet$prenorm)) {
      mat <- mSetObj$dataSet$prenorm
      df <- as.data.frame(mat)
      rn <- rownames(mat)
      if (!is.null(rn)) {
        df <- cbind(row_names_id = as.character(rn), df)
      }
      arrow::write_feather(df, "prenorm_data.arrow", compression = "uncompressed")
      return(1)
    }
    return(0)
  }, error = function(e) {
    warning(paste("ExportPrenormDataArrow failed:", e$message))
    return(0)
  })
}

#' Export statistical results matrix to Arrow format
#' For t-test, fold change, volcano, ANOVA results
#' @param result_mat Result matrix to export
#' @param filename Output filename (without .arrow extension)
#' @return 1 on success, 0 on failure
ExportResultMatArrow <- function(result_mat, filename) {
  tryCatch({
    df <- as.data.frame(result_mat)

    # Convert factors to character
    for (col in names(df)) {
      if (is.factor(df[[col]])) {
        df[[col]] <- as.character(df[[col]])
      }
    }

    rn <- rownames(result_mat)
    if (!is.null(rn)) {
      df <- cbind(row_names_id = as.character(rn), df)
    }

    arrow_path <- paste0(filename, ".arrow")
    arrow::write_feather(df, arrow_path, compression = "uncompressed")
    return(1)
  }, error = function(e) {
    warning(paste("ExportResultMatArrow failed:", e$message))
    return(0)
  })
}
