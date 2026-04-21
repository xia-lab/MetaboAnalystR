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
    # Use confidence level from session if available
    level <- tryCatch(.get.confidence.level(), error = function(e) as.numeric(level))
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

    # Single subprocess call for all ellipsoid computations via bridge files
    bridge_in <- paste0(tempdir(), "/bridge_", paste0(sample(letters,6,replace=TRUE), collapse=""), "_in.qs")
    bridge_out <- sub("_in.qs", "_out.qs", bridge_in)
    ov_qs_save(list(coords_per_group = coords_per_group, level = level, group_names = group_names), bridge_in, preset = "fast")
    on.exit(unlink(c(bridge_in, bridge_out)), add = TRUE)

    run_func_via_rsclient(
      func = function(wd, bridge_in, bridge_out) {
        setwd(wd)
        Sys.setenv(RGL_USE_NULL = TRUE)
        require(rgl)
        input <- ov_qs_read(bridge_in)
        coords_list <- input$coords_per_group
        level <- input$level
        group_names <- input$group_names

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
            mesh <- list()
            mesh[[1]] <- rgl::ellipse3d(x = as.matrix(pos), centre = center, t = t_vals[i])
            result_list[[i]] <- list(group = group_names[i], mesh = mesh, error = NULL)
          }, error = function(e) {
            result_list[[i]] <<- list(group = group_names[i], mesh = list(), error = e$message)
          })
        }
        ov_qs_save(result_list, bridge_out, preset = "fast")
      },
      args = list(wd = getwd(), bridge_in = bridge_in, bridge_out = bridge_out),
      timeout_sec = 120
    )

    mesh_results <- if (file.exists(bridge_out)) ov_qs_read(bridge_out) else NULL

    # Write JSON in master (subprocess may have different wd)
    if (!is.null(mesh_results)) {
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
