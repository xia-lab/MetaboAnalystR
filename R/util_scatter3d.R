##################################################
## R script for OmicsAnalyst
## Description: Compute 3D scatter plot from dimension reduction results
## Authors: 
## G. Zhou (guangyan.zhou@mail.mcgill.ca) 
## J. Xia, jeff.xia@mcgill.ca
###################################################

my.json.scatter <- function(filenm, containsLoading=F, scaleMode=NULL){
  library(igraph);
  res <- ov_qs_read("score3d.qs")

  # Read scaleMode from session if not passed explicitly
  if (is.null(scaleMode)) {
    opts <- tryCatch(Get3DScatterOptions(), error = function(e) list(scaleMode = "independent"))
    scaleMode <- opts$scaleMode
  }

  nodes <- vector(mode="list");
  names <- res$name;
  if(ncol(res$xyz) > nrow(res$xyz)){
  orig.pos.xyz <- t(res$xyz);
  }else{
  orig.pos.xyz <- res$xyz;
  }

  # Widen the axes to hold every group's confidence ellipsoid, as the 2D score
  # plots widen xlim/ylim to hold their ellipses -- otherwise a small or elongated
  # group's ellipsoid runs far outside the plotted box.
  ell.range <- .ellipsoid.axis.ranges(orig.pos.xyz, res$facA)
  ticksX <- pretty(range(orig.pos.xyz[,1]*1.2, ell.range[,1]),10, bound=F);
  ticksY <- pretty(range(orig.pos.xyz[,2]*1.2, ell.range[,2]),10, bound=F);
  ticksZ <- pretty(range(orig.pos.xyz[,3]*1.2, ell.range[,3]),10, bound=F);

  if (scaleMode == "uniform") {
    # Uniform scaling: preserves proportions between axes
    maxVal <- max(abs(rbind(orig.pos.xyz, ell.range)))
    if (maxVal > 0) {
      pos.xyz <- orig.pos.xyz / maxVal
    } else {
      pos.xyz <- orig.pos.xyz
    }
  } else {
    # Independent scaling (default): each axis fills [-1, 1]
    minNode <-  c(min(ticksX), min(ticksY), min(ticksZ));
    maxNode <-  c(max(ticksX), max(ticksY), max(ticksZ));
    orig.pos.xyz.mod <- rbind(orig.pos.xyz, minNode)
    orig.pos.xyz.mod <- rbind(orig.pos.xyz.mod, maxNode)
    pos.xyz <- orig.pos.xyz.mod;
    pos.xyz[,1] <- scale_range(orig.pos.xyz.mod[,1], -1,1);
    pos.xyz[,2] <- scale_range(orig.pos.xyz.mod[,2], -1,1);
    pos.xyz[,3] <- scale_range(orig.pos.xyz.mod[,3], -1,1);
    pos.xyz <- pos.xyz[1:(nrow(pos.xyz) - 2), ]
  }
  
  metadf <- res$facA
  if(!is.factor(metadf)){
    metadf <- as.factor(metadf);
  }

  col = vector();
  
  meta.vec <- as.vector(metadf)
  meta.vec.num <- as.integer(as.factor(metadf))
  col.s <- rgb_array_to_hex_array(res$colors)
  for(i in 1:length(meta.vec.num)){
    col[i] = col.s[meta.vec.num[i]];
  }

  legendData <- list(label=levels(metadf),color=col.s, type = res$color_legend_type)
  #for IPCA in multifactor

  # print("legendData:")
  # print(legendData)
  
if ("facB" %in% names(res)) {

  ## factor B values, in original order
  meta.vec2 <- as.factor(res$facB)           # keep factor to preserve level order
  lvlB      <- levels(meta.vec2)             # unique levels, in displayed order

  ## pick a shape palette (extend if needed)
  shape.palette <- c("circle", "triangle", "square", "diamond",
                     "cross", "star", "hexagon", "asterisk")

  shape.map <- setNames(shape.palette[seq_along(lvlB)], lvlB)  # level → glyph map

  ## assign point shapes using the mapping
  shape <- unname(shape.map[meta.vec2])

  ## legend – matching order and length
  legendData2 <- list(
    label = names(shape.map),     # same level order
    shape = unname(shape.map)     # corresponding glyphs
  )

  ## Debug
  # print("legendData2:")
  # print(legendData2)
}

  
  nodeSize = 18;
  
  for(i in 1:length(names)){
    nodes[[i]] <- list(
      id=names[i],
      label=names[i],
      size=nodeSize,
      meta=meta.vec[i],
      fx = unname(pos.xyz[i,1])*1000,
      fy = unname(pos.xyz[i,2])*1000,
      fz = unname(pos.xyz[i,3])*1000,
      origX = unname(orig.pos.xyz[i,1]),
      origY = unname(orig.pos.xyz[i,2]),
      origZ = unname(orig.pos.xyz[i,3]),
      colorb=col[i]
    );
    
    if("facB" %in% names(res)){
      nodes[[i]][["meta2"]] <- meta.vec2[i]
      nodes[[i]][["shape"]] <- shape[i]

    }
  }
  
  ticks <- list(x=ticksX, y=ticksY, z=ticksZ);
  library(RJSONIO)
  
  if(!containsLoading){
  netData <- list(nodes=nodes,
  edges="NA",
  meta=metadf, 
  loading="NA",
  axis=res$axis, 
  ticks=ticks,
  metaCol = legendData);
  }else{
    res2 <- ov_qs_read("loading3d.qs");

    if(ncol(res2$xyz) > nrow(res2$xyz)){
    orig.load.xyz <- t(res2$xyz);
    }else{
    orig.load.xyz <- res2$xyz;
    }
    
    ticksX <- pretty(range(orig.load.xyz[,1]),10);
    ticksY <- pretty(range(orig.load.xyz[,2]),10);
    ticksZ <- pretty(range(orig.load.xyz[,3]),10);
    
    if (scaleMode == "uniform") {
      maxVal <- max(abs(orig.load.xyz))
      if (maxVal > 0) {
        load.xyz <- orig.load.xyz / maxVal
      } else {
        load.xyz <- orig.load.xyz
      }
    } else {
      minNode <-  c(min(ticksX), min(ticksY), min(ticksZ));
      maxNode <-  c(max(ticksX), max(ticksY), max(ticksZ));
      orig.load.xyz.mod <- rbind(orig.load.xyz, minNode)
      orig.load.xyz.mod <- rbind(orig.load.xyz.mod, maxNode)
      load.xyz <- orig.load.xyz.mod;
      load.xyz[,1] <- scale_range(orig.load.xyz.mod[,1], -1,1);
      load.xyz[,2] <- scale_range(orig.load.xyz.mod[,2], -1,1);
      load.xyz[,3] <- scale_range(orig.load.xyz.mod[,3], -1,1);
      #remove last two rows (padding nodes for independent scaling)
      load.xyz <- load.xyz[1:(nrow(load.xyz) - 2), ];
    }
    
    names <- res2$name;
    if("entrez" %in% names(res2)){
    ids <- res2$entrez;
    }else{
    ids <- res2$name;
    }
    colres <- rgba_to_hex_opacity(res2$cols);
    colorb <- colres[[1]];
    opacity_array <- colres[[2]];
    nodes2 <- vector(mode="list");
    
    for(i in 1:length(names)){
      nodes2[[i]] <- list(
        id=ids[i],
        label=names[i],
        size=24,
        opacity=opacity_array[i],
        fx = unname(load.xyz[i,1])*1000,
        fy = unname(load.xyz[i,2])*1000,
        fz = unname(load.xyz[i,3])*1000,
        origX = unname(orig.load.xyz[i,1]),
        origY = unname(orig.load.xyz[i,2]),
        origZ = unname(orig.load.xyz[i,3]),
        seedArr = "notSeed",
        colorb=colorb[i],
        colorw=colorb[i]
      );
    }
    
    ticksLoading <- list(x=ticksX, y=ticksY, z=ticksZ);
    
    netData <- list(omicstype="", 
                    nodes=nodes,
                    meta=metadf, 
                    loading=nodes2, 
                    misc="", ticks=ticks,
                    ticksLoading=ticksLoading,
                    axis=res$axis,
                    axisLoading=res2$axis, 
                    metaCol = legendData);
  }

  if("facB" %in% names(res)){
    netData$metaShape <- legendData2;
}

  rownames(pos.xyz) <- res$name;
  res$pos.xyz <- pos.xyz;
  # Preserve scatter options — check rdt.set.qs first, then GlobalEnv fallback
  prev.opts <- tryCatch(.get.rdt.set()$scatter.opts, error = function(e) NULL)
  if (is.null(prev.opts) && exists(".scatter.opts", envir = .GlobalEnv)) {
    prev.opts <- get(".scatter.opts", envir = .GlobalEnv)
  }
  if (!is.null(prev.opts)) res$scatter.opts <- prev.opts;
  .set.rdt.set(res);



  sink(filenm);
  cat(toJSON(netData));
  sink();
  return(1);
}

rgb_array_to_hex_array <- function(rgb_array) {
  # Split the input array into individual RGB strings
    rgb_strings <- gsub("rgba\\(", "",rgb_array);
    rgb_strings <- gsub("\\)", "",rgb_strings);
  
  # Convert each RGB string to a hex code
  hex_array <- sapply(rgb_strings, function(rgb_string) {
    rgb_to_hex(rgb_string)
  })
  return(unname(hex_array));
}

rgb_to_hex <- function(rgb_string) {
  # Extract the RGB values from the input string
  rgb_values <- unlist(strsplit(gsub("[rgb()]", "", rgb_string), ","))
  r <- as.numeric(rgb_values[1])
  g <- as.numeric(rgb_values[2])
  b <- as.numeric(rgb_values[3])
  
  # Convert the RGB values to hex format
  hex_code <- paste0("#", sprintf("%02X%02X%02X", r, g, b))
  
  return(hex_code)
}


# Define a function to convert RGBA to Hex and opacity values
rgba_to_hex_opacity <- function(rgba_array) {
  
  # Define an empty vector to store the hex color values
  hex_values <- vector("character", length = length(rgba_array))
  
  # Define an empty vector to store the opacity values
  opacity_values <- vector("numeric", length = length(rgba_array))
  
  # Loop through each element in the input array
  for (i in seq_along(rgba_array)) {
    rgba <- rgba_array[i]
    rgba <- gsub("rgba\\(", "",rgba);
    rgba <- gsub("\\)", "",rgba);
    # Convert the RGBA value to a list of numeric values
    rgba <- strsplit(rgba, ",")[[1]]
    
    rgba <- as.numeric(rgba)
    # Convert the RGB values to hexadecimal notation
    hex <- rgb(rgba[1], rgba[2], rgba[3], maxColorValue = 255)
    hex_values[i] <- hex
    
    # Extract the opacity value from the RGBA string
    opacity_values[i] <- rgba[4]
  }
  
  # Return a list containing the hex color values and opacity values
  return(list(hex_values = hex_values, opacity_values = opacity_values))
}


scale_range <- function(x, new_min = 0, new_max = 1) {
  range <- pretty(x,10);
  old_min <- min(range);
  old_max <- max(range);
  
  
  (x - old_min) / (old_max - old_min) * (new_max - new_min) + new_min
}


# Scatter plot options stored in session (affects both 2D and 3D)
# scaleMode: "independent" (per-axis) or "uniform" (proportional)
# confidenceLevel: 0.50-0.99 (default 0.95)
# confMethod: "chisq" (chi-squared, large samples) or "f" (F-distribution, small samples)
# hideTightEllipse: FALSE (default) shows every group's ellipse, including a thin
#   "sliver" for a near-collinear group; TRUE hides that group's ellipse instead
SetScatterOptions <- function(scaleMode="independent", confidenceLevel=0.95, confMethod="chisq", hideTightEllipse=FALSE) {
  opts <- list(scaleMode = scaleMode, confidenceLevel = as.numeric(confidenceLevel), confMethod = confMethod,
               hideTightEllipse = isTRUE(as.logical(hideTightEllipse)))
  # Always store in GlobalEnv as reliable fallback
  assign(".scatter.opts", opts, envir = .GlobalEnv)
  # Also try to persist in rdt.set.qs
  tryCatch({
    res <- .get.rdt.set()
    res$scatter.opts <- opts
    .set.rdt.set(res)
  }, error = function(e) {})
  return(1)
}

Set3DScatterOptions <- SetScatterOptions

GetScatterOptions <- function() {
  opts <- tryCatch({
    res <- .get.rdt.set()
    res$scatter.opts
  }, error = function(e) NULL)
  if (is.null(opts) && exists(".scatter.opts", envir = .GlobalEnv)) {
    opts <- get(".scatter.opts", envir = .GlobalEnv)
  }
  if (is.null(opts)) {
    opts <- list(scaleMode = "independent", confidenceLevel = 0.95, confMethod = "chisq", hideTightEllipse = FALSE)
  }
  return(opts)
}

Get3DScatterOptions <- GetScatterOptions

# Get confidence level from session
.get.confidence.level <- function() {
  opts <- GetScatterOptions()
  return(opts$confidenceLevel)
}

# Compute the t-value (radius scaling) for ellipsoid given k dimensions and n samples
# chisq method: sqrt(qchisq(level, k)) — asymptotic, for large samples
# Compute t-value (radius scaling) for ellipsoid
# 2D ellipses use k=2 (chi-sq df=2), 3D ellipsoids use k=3 (chi-sq df=3)
# At the same confidence level, 3D ellipsoids appear smaller in projection
# because the probability is distributed across 3 dimensions instead of 2
# chisq method: sqrt(qchisq(level, k)) — asymptotic, for large samples
# f method: sqrt(k * qf(level, k, n-1)) — sample-adjusted, for small samples
.get.ellipsoid.t <- function(level, k, n) {
  opts <- GetScatterOptions()
  if (opts$confMethod == "f" && n > k) {
    return(sqrt(k * qf(level, k, n - 1)))
  } else {
    return(sqrt(qchisq(level, k)))
  }
}

# Axis-aligned extent of each group's confidence ellipsoid, in the same units as xyz.
# The bounding box of {x : (x-c)' S^-1 (x-c) <= t^2} is c +/- t*sqrt(diag(S)), so no
# mesh is needed. Groups mirror the viewer: one ellipsoid per level of a discrete
# factor, or one over all samples for a continuous one; groups with < 4 samples get
# no ellipsoid in the viewer and are skipped here too.
# Returns a matrix of min/max rows (empty when there is no ellipsoid).
.ellipsoid.axis.ranges <- function(xyz, fac) {
  xyz <- as.matrix(xyz[, 1:3, drop = FALSE])
  if (is.null(fac) || length(fac) != nrow(xyz)) return(xyz[0, , drop = FALSE])
  grp <- if (is.numeric(fac) && !is.factor(fac)) rep("all", nrow(xyz)) else as.character(fac)
  level <- .resolve.encasing.level(NA)
  out <- lapply(split(seq_len(nrow(xyz)), grp), function(idx) {
    if (length(idx) < 4) return(NULL)
    S <- cov(xyz[idx, , drop = FALSE])
    if (any(!is.finite(S))) return(NULL)
    half <- .get.ellipsoid.t(level, 3, length(idx)) * sqrt(pmax(diag(S), 0))
    ctr <- colMeans(xyz[idx, , drop = FALSE])
    rbind(ctr - half, ctr + half)
  })
  out <- do.call(rbind, out)
  if (is.null(out)) xyz[0, , drop = FALSE] else out
}

# Resolve the confidence level for an ellipsoid request: the level passed from the
# 3D viewer wins when it is a valid probability; otherwise fall back to the session
# option (default 0.95). Previously the session value always won, so the viewer's
# confidence control had no effect.
.resolve.encasing.level <- function(level) {
  level <- suppressWarnings(as.numeric(level))
  if (length(level) != 1 || is.na(level) || level <= 0 || level >= 1) {
    level <- tryCatch(as.numeric(.get.confidence.level()), error = function(e) 0.95)
  }
  if (length(level) != 1 || is.na(level) || level <= 0 || level >= 1) level <- 0.95
  level
}

ComputeEncasing <- function(filenm, type, names.vec, level=0.95, omics="NA"){
  tryCatch({
    level <- .resolve.encasing.level(level)
    names <- strsplit(names.vec, "; ")[[1]]

    # Read data in master
    res <- .get.rdt.set()
    pos.xyz <- res$pos.xyz

    if (is.null(pos.xyz) || nrow(pos.xyz) == 0) {
      sink(filenm); cat("{}"); sink()
      return(filenm)
    }

    inx <- rownames(pos.xyz) %in% names
    coords <- as.matrix(pos.xyz[inx, c(1:3)])

    if (nrow(coords) < 4) {
      sink(filenm); cat(RJSONIO::toJSON(list())); sink()
      return(filenm)
    }

    # Only rgl::ellipse3d in subprocess via bridge files
    bridge_in <- paste0(tempdir(), "/bridge_", paste0(sample(letters,6,replace=TRUE), collapse=""), "_in.qs")
    bridge_out <- sub("_in.qs", "_out.qs", bridge_in)
    # Radius computed here (master) so the session's chisq/F choice is honoured
    t_val <- .get.ellipsoid.t(level, 3, nrow(coords))
    ov_qs_save(list(coords = coords, t_val = t_val), bridge_in, preset = "fast")
    on.exit(unlink(c(bridge_in, bridge_out)), add = TRUE)

    run_func_via_microservice(
      func = function(wd, bridge_in, bridge_out) {
        setwd(wd)
        Sys.setenv(RGL_USE_NULL = TRUE)
        require(rgl)
        input <- ov_qs_read(bridge_in)
        pos <- cov(input$coords, y = NULL, use = "everything")
        center <- colMeans(input$coords)
        t_val <- input$t_val
        mesh <- list()
        mesh[[1]] <- rgl::ellipse3d(x = as.matrix(pos), centre = center, t = t_val)
        ov_qs_save(mesh, bridge_out, preset = "fast")
      },
      args = list(wd = getwd(), bridge_in = bridge_in, bridge_out = bridge_out),
      timeout_sec = 120
    )

    mesh <- if (file.exists(bridge_out)) ov_qs_read(bridge_out) else NULL

    # Write JSON in master (subprocess may have different wd)
    if (!is.null(mesh)) {
      sink(filenm); cat(RJSONIO::toJSON(mesh)); sink()
    }
  }, error = function(e) {
    message("[ComputeEncasing] ", e$message)
    sink(filenm); cat("{}"); sink()
  })
  return(filenm)
}

.get.rdt.set <- function(){
  return(ov_qs_read("rdt.set.qs"));
}

.set.rdt.set <- function(my.set){
  ov_qs_save(my.set, file="rdt.set.qs");
}
