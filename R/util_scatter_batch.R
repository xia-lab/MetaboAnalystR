##################################################
## Batch Encasing Computation
## Optimized to compute multiple groups in single R call
##################################################

#' Compute Encasing for Multiple Groups (Batch Version)
#' @description Processes all groups in a single R call instead of N separate calls
#' @param filenm Base output filename
#' @param type Encasing type (ellipse only)
#' @param groups_json JSON string containing array of groups with their sample IDs
#' @param level Confidence level (default 0.95)
#' @param omics Omics type (default "NA")
#' @return JSON filename
#' @export
ComputeEncasingBatch <- function(filenm, type, groups_json, level = 0.95, omics = "NA") {
  tryCatch({
    level <- as.numeric(level)

    # Read data in master
    res <- .get.rdt.set()
    pos.xyz <- res$pos.xyz

    if (is.null(pos.xyz) || nrow(pos.xyz) == 0) {
      sink(filenm); cat("{}"); sink()
      return(filenm)
    }

    # Parse groups and extract coords per group in master
    groups_list <- RJSONIO::fromJSON(groups_json)
    if (is.data.frame(groups_list)) {
      groups_list <- split(groups_list, seq_len(nrow(groups_list)))
    }

    coords_per_group <- vector("list", length(groups_list))
    group_names <- character(length(groups_list))

    for (i in seq_along(groups_list)) {
      group_info <- groups_list[[i]]
      if (is.character(group_info)) {
        group_names[i] <- unname(group_info["group"])
        names_vec <- unname(group_info["ids"])
      } else if (is.data.frame(group_info)) {
        group_names[i] <- group_info$group[1]
        names_vec <- group_info$ids[1]
      } else {
        group_names[i] <- group_info$group
        names_vec <- group_info$ids
      }

      nms <- strsplit(names_vec, "; ")[[1]]
      inx <- rownames(pos.xyz) %in% nms
      coords_per_group[[i]] <- as.matrix(pos.xyz[inx, c(1:3)])
    }

    # Single subprocess call for all ellipsoid computations
    mesh_results <- rsclient_isolated_exec(
      func_body = function(input_data) {
        Sys.setenv(RGL_USE_NULL = TRUE)
        coords_list <- input_data$coords_per_group
        level <- input_data$level
        group_names <- input_data$group_names

        result_list <- vector("list", length(coords_list))
        for (i in seq_along(coords_list)) {
          coords <- coords_list[[i]]
          if (nrow(coords) < 4) {
            result_list[[i]] <- list(group = group_names[i], mesh = list(), error = "Insufficient points")
            next
          }
          tryCatch({
            pos <- cov(coords, y = NULL, use = "everything")
            center <- colMeans(coords)
            t_val <- sqrt(qchisq(level, 3))
            mesh <- list()
            mesh[[1]] <- rgl::ellipse3d(x = as.matrix(pos), centre = center, t = t_val)
            result_list[[i]] <- list(group = group_names[i], mesh = mesh, error = NULL)
          }, error = function(e) {
            result_list[[i]] <<- list(group = group_names[i], mesh = list(), error = e$message)
          })
        }
        result_list
      },
      input_data = list(coords_per_group = coords_per_group, level = level, group_names = group_names),
      packages = c("rgl", "qs"),
      timeout = 120,
      output_type = "qs"
    )

    # Write JSON in master (subprocess may have different wd)
    if (is.list(mesh_results) && !isFALSE(mesh_results$success)) {
      sink(filenm); cat(RJSONIO::toJSON(mesh_results)); sink()
    } else {
      sink(filenm); cat("{}"); sink()
    }
  }, error = function(e) {
    message("[ComputeEncasingBatch] ", e$message)
    sink(filenm); cat("{}"); sink()
  })
  return(filenm)
}
