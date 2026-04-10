##################################################
## R script for OmicsAnalyst
## Description: Compute 3D scatter plot from dimension reduction results
## Authors: 
## G. Zhou (guangyan.zhou@mail.mcgill.ca) 
## J. Xia, jeff.xia@mcgill.ca
###################################################

my.json.scatter <- function(filenm, containsLoading=F, scaleMode=NULL){
  library(igraph);
  res <- qs::qread("score3d.qs")

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

  ticksX <- pretty(range(orig.pos.xyz[,1]*1.2),10, bound=F);
  ticksY <- pretty(range(orig.pos.xyz[,2]*1.2),10, bound=F);
  ticksZ <- pretty(range(orig.pos.xyz[,3]*1.2),10, bound=F);

  if (scaleMode == "uniform") {
    # Uniform scaling: preserves proportions between axes
    maxVal <- max(abs(orig.pos.xyz))
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
    res2 <- qs::qread("loading3d.qs");

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
SetScatterOptions <- function(scaleMode="independent", confidenceLevel=0.95, confMethod="chisq") {
  opts <- list(scaleMode = scaleMode, confidenceLevel = as.numeric(confidenceLevel), confMethod = confMethod)
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
    opts <- list(scaleMode = "independent", confidenceLevel = 0.95, confMethod = "chisq")
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

ComputeEncasing <- function(filenm, type, names.vec, level=0.95, omics="NA"){
  tryCatch({
    # Use confidence level from session if available
    level <- tryCatch(.get.confidence.level(), error = function(e) as.numeric(level))
    level <- as.numeric(level)
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

    # Compute t-value using session method (chisq or F-distribution)
    n <- nrow(coords)
    t_val <- .get.ellipsoid.t(level, 3, n)

    mesh <- rsclient_isolated_exec(
      func_body = function(input_data) {
        Sys.setenv(RGL_USE_NULL = TRUE)
        pos <- cov(input_data$coords, y = NULL, use = "everything")
        center <- colMeans(input_data$coords)
        mesh <- list()
        mesh[[1]] <- rgl::ellipse3d(x = as.matrix(pos), centre = center, t = input_data$t_val)
        mesh
      },
      input_data = list(coords = coords, t_val = t_val),
      packages = c("rgl", "qs"),
      timeout = 120,
      output_type = "qs"
    )

    # Write JSON in master (subprocess may have different wd)
    if (!is.list(mesh) || !isFALSE(mesh$success)) {
      sink(filenm); cat(RJSONIO::toJSON(mesh)); sink()
    }
  }, error = function(e) {
    message("[ComputeEncasing] ", e$message)
    sink(filenm); cat("{}"); sink()
  })
  return(filenm)
}

.get.rdt.set <- function(){
  return(qs::qread("rdt.set.qs"));
}

.set.rdt.set <- function(my.set){
  qs::qsave(my.set, file="rdt.set.qs");
}
