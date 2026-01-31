##################################################
## Batch Encasing Computation
## Optimized to compute multiple groups in single R call
##################################################

#' Compute Encasing for Multiple Groups (Batch Version)
#' @description Processes all groups in a single R call instead of N separate calls
#' @param filenm Base output filename
#' @param type Encasing type ("alpha", "ellipse", "contour")
#' @param groups_json JSON string containing array of groups with their sample IDs
#' @param level Confidence level (default 0.95)
#' @param omics Omics type (default "NA")
#' @return JSON filename
#' @export
ComputeEncasingBatch <- function(filenm, type, groups_json, level = 0.95, omics = "NA") {
  level <- as.numeric(level)

  groups_list <- RJSONIO::fromJSON(groups_json)
  if (is.data.frame(groups_list)) {
    groups_list <- split(groups_list, seq_len(nrow(groups_list)))
  }

  res <- .get.rdt.set()
  pos.xyz <- res$pos.xyz

  result_list <- vector("list", length(groups_list))

  for (i in seq_along(groups_list)) {
    group_info <- groups_list[[i]]
    if (is.character(group_info)) {
      group_name <- unname(group_info["group"])
      names_vec <- unname(group_info["ids"])
    } else if (is.data.frame(group_info)) {
      group_name <- group_info$group[1]
      names_vec <- group_info$ids[1]
    } else {
      group_name <- group_info$group
      names_vec <- group_info$ids
    }

    names <- strsplit(names_vec, "; ")[[1]]
    inx <- rownames(pos.xyz) %in% names
    coords <- as.matrix(pos.xyz[inx, c(1:3)])

    if (nrow(coords) < 4 && type == "contour") {
      result_list[[i]] <- list(group = group_name, mesh = list(), error = "Insufficient points")
      next
    }

    mesh <- list()
    tryCatch({
      if (type == "alpha") {
        library(alphashape3d)
        library(rgl)
        sh <- ashape3d(coords, 1.0, pert = FALSE, eps = 1e-09)
        mesh[[1]] <- as.mesh3d(sh, triangles = TRUE)
      } else if (type == "ellipse") {
        library(rgl)
        pos <- cov(coords, y = NULL, use = "everything")
        mesh[[1]] <- ellipse3d(x = as.matrix(pos), level = level)
      } else {
        library(ks)
        res <- kde(coords)
        r <- plot(res, cont = level * 100, display = "rgl")
        sc <- scene3d()
        mesh <- sc$objects
      }

      result_list[[i]] <- list(group = group_name, mesh = mesh, error = NULL)
    }, error = function(e) {
      result_list[[i]] <- list(group = group_name, mesh = list(), error = e$message)
    })
  }

  library(RJSONIO)
  sink(filenm)
  cat(RJSONIO::toJSON(result_list))
  sink()

  return(filenm)
}
