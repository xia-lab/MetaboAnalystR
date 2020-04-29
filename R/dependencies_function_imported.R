#' I. Internal Functions from Plotly Package
#' THe functions are from Plotly Package and was called internally only
#' @references https://cran.r-project.org/package=plotly
#' @references Sievert C (2020). Interactive Web-Based Data Visualization with R, plotly, and shiny. Chapman and Hall/CRC. ISBN 9781138331457, https://plotly-r.com.
#' @import htmltools
#' @import jsonlite
#' @importFrom graphics layout
#' @importFrom tibble as_tibble
#' @importFrom purrr transpose
#' 

# (1). Add_markers & add_data
add_trace <- function(p, ...,data = NULL, inherit = TRUE) {
  
  # "native" plotly arguments
  attrs <- list(...)
  attrs$inherit <- inherit
  
  if (!is.null(attrs[["group"]])) {
    warning("The group argument has been deprecated. Use group_by() or split instead.")
  }
  
  p <- add_data(p, data)
  
  # inherit attributes from the "first layer" (except the plotly_eval class)
  if (inherit) {
    attrs <- modify_list(unclass(p$x$attrs[[1]]), attrs)
  }
  
  p$x$attrs <- c(
    p$x$attrs %||% list(), 
    setNames(list(attrs), p$x$cur_data)
  )
  
  p
}
add_markers <- function(p, x = NULL, y = NULL, z = NULL, ..., data = NULL, inherit = TRUE) {
  if (inherit) {
    x <- x %||% p$x$attrs[[1]][["x"]]
    y <- y %||% p$x$attrs[[1]][["y"]]
    z <- z %||% p$x$attrs[[1]][["z"]]
  }
  if (is.null(x) || is.null(y)) {
    stop("Must supply `x` and `y` attributes", call. = FALSE)
  }
  type <- if (!is.null(z)) "scatter3d" else "scatter"
  add_trace(
    p, x = x, y = y, z = z, type = type, mode = "markers", ...,
    data = data, inherit = inherit
  )
}
add_data <- function(p, data = NULL) {
  if (is.null(data)) return(p)
  if (!is.plotly(p)) {
    stop("Don't know how to add traces to an object of class: ", 
         class(p), call. = FALSE)
  }
  id <- new_id()
  p$x$visdat[[id]] <- function() data
  p$x$cur_data <- id
  # TODO: should this also override the data used for the most recent trace?
  p
}

# (2). layout
layout <- function(p, ..., data = NULL) {
  UseMethod("layout")
}
layout.matrix <- function(p, ..., data = NULL) {
  # workaround for the popular graphics::layout() function
  # https://github.com/ropensci/plotly/issues/464
  graphics::layout(p, ...)
}
layout.shiny.tag.list <- function(p, ..., data = NULL) {
  idx <- which(vapply(p, is.plotly, logical(1)))
  for (i in idx) {
    p[[i]] <- layout.plotly(p[[i]], ..., data = NULL)
  }
  p
}
layout.plotly <- function(p, ..., data = NULL) {
  p <- add_data(p, data)
  attrs <- list(...)
  if (!is.null(attrs[["height"]]) || !is.null(attrs[["width"]])) {
    warning("Specifying width/height in layout() is now deprecated.\n", 
            "Please specify in ggplotly() or plot_ly()", call. = FALSE)
  }
  # similar to add_trace()
  p$x$layoutAttrs <- c(
    p$x$layoutAttrs %||% list(), 
    setNames(list(attrs), p$x$cur_data)
  )
  p
}

# (3). plotly
plot_ly <- function(data = data.frame(), ..., type = NULL, name,
                    color, colors = NULL, alpha = NULL, 
                    stroke, strokes = NULL, alpha_stroke = 1,
                    size, sizes = c(10, 100), 
                    span, spans = c(1, 20),
                    symbol, symbols = NULL, 
                    linetype, linetypes = NULL,
                    split, frame, 
                    width = NULL, height = NULL, source = "A") {
  
  if (!is.data.frame(data) && !crosstalk::is.SharedData(data)) {
    stop("First argument, `data`, must be a data frame or shared data.", call. = FALSE)
  }
  
  # "native" plotly arguments
  attrs <- list(...)
  
  # warn about old arguments that are no longer supported
  for (i in c("filename", "fileopt", "world_readable")) {
    if (is.null(attrs[[i]])) next
    warning("Ignoring ", i, ". Use `plotly_POST()` if you want to post figures to plotly.")
    attrs[[i]] <- NULL
  }
  if (!is.null(attrs[["group"]])) {
    warning(
      "The group argument has been deprecated. Use `group_by()` or split instead.\n",
      "See `help('plotly_data')` for examples"
    )
    attrs[["group"]] <- NULL
  }
  if (!is.null(attrs[["inherit"]])) {
    warning("The inherit argument has been deprecated.")
    attrs[["inherit"]] <- NULL
  }
  
  # tack on variable mappings
  attrs$name <- if (!missing(name)) name
  attrs$color <- if (!missing(color)) color
  attrs$stroke <- if (!missing(stroke)) stroke
  attrs$size <- if (!missing(size)) size
  attrs$span <- if (!missing(span)) span
  attrs$symbol <- if (!missing(symbol)) symbol
  attrs$linetype <- if (!missing(linetype)) linetype
  attrs$split <- if (!missing(split)) split
  attrs$frame <- if (!missing(frame)) frame
  
  # tack on scale ranges
  attrs$colors <- colors
  attrs$strokes <- strokes
  attrs$alpha <- alpha
  attrs$alpha_stroke <- alpha_stroke
  attrs$sizes <- sizes
  attrs$spans <- spans
  attrs$symbols <- symbols
  attrs$linetypes <- linetypes
  
  # and, of course, the trace type
  attrs$type <- type
  
  # id for tracking attribute mappings and finding the most current data
  id <- new_id()
  # avoid weird naming clashes
  plotlyVisDat <- data
  p <- list(
    visdat = setNames(list(function() plotlyVisDat), id),
    cur_data = id,
    attrs = setNames(list(attrs), id),
    # we always deal with a _list_ of traces and _list_ of layouts 
    # since they can each have different data
    layout = list(
      width = width, 
      height = height,
      # sane margin defaults (mainly for RStudio)
      margin = list(b = 40, l = 60, t = 25, r = 10)
    ),
    source = source
  )
  # ensure the collab button is shown (and the save/edit button is hidden) by default
  config(as_widget(p))
}

# (4). new_id
new_id <- function() {
  basename(tempfile(""))
}

# (5). config
config <- function(p, ..., cloud = FALSE, showSendToCloud = cloud, locale = NULL, mathjax = NULL) {
  
  if (!is.null(locale)) {
    p$x$config$locale <- locale
    # Plotly.js defaults to US English (en-US) and includes 
    # British English (en) in the standard bundle.
    if (!locale %in% c("en", "en-US")) {
      p$dependencies <- c(
        p$dependencies,
        list(locale_dependency(locale))
      )
    }
  }
  
  if (!is.null(mathjax)) {
    mj <- switch(
      match.arg(mathjax, c("cdn", "local")),
      cdn = mathjax_cdn(),
      local = mathjax_local()
    )
    # if mathjax is already supplied overwrite it; otherwise, prepend it
    depNames <- sapply(p$dependencies, "[[", "name")
    if (any(idx <- depNames %in% "mathjax")) {
      p$dependencies[[which(idx)]] <- mathjax
    } else {
      p$dependencies <- c(list(mj), p$dependencies)
    }
  }
  
  args <- list(...)
  if ("collaborate" %in% names(args)) warning("The collaborate button is no longer supported")
  p$x$config <- modify_list(p$x$config, args)
  if (cloud) warning("The `cloud` argument is deprecated. Use `showSendToCloud` instead.")
  p$x$config$showSendToCloud <- showSendToCloud
  
  p
}

# (6). modify_list & modifyList (which is in R base package)
modify_list <- function(x, y, ...) {
  modifyList(x %||% list(), y %||% list())
}
"%||%" <- function(x, y) {
  if (length(x) > 0 || is_blank(x)) x else y
}
is_blank <- function(x) {
  inherits(x, "element_blank") && inherits(x, "element")
}

# (7). as_widget
as_widget <- function(x, ...) {
  if (inherits(x, "htmlwidget")) return(x)
  # add plotly class mainly for printing method
  # customize the JSON serializer (for htmlwidgets)
  attr(x, 'TOJSON_FUNC') <- to_JSON
  htmlwidgets::createWidget(
    name = "plotly",
    x = x,
    width = x$layout$width,
    height = x$layout$height,
    sizingPolicy = htmlwidgets::sizingPolicy(
      browser.fill = TRUE,
      defaultWidth = '100%',
      defaultHeight = 400
    ),
    preRenderHook = plotly_build,
    dependencies = c(
      # phantomjs doesn't support Object.setPrototypeOf() and a
      # plotly.js dependency (buffer) uses it to detect TypedArray support.
      # Thus, we add a polyfill if this is running in shinytest, but otherwise
      # we shouldn't need it because Object.setPrototypeOf() is pretty widely supported
      # https://github.com/plotly/plotly.js/issues/4556#issuecomment-583061419
      # https://caniuse.com/#search=setPrototypeOf
      if (isTRUE(getOption("shiny.testmode"))) {
        list(setPrototypeOfPolyfill())
      },
      list(typedArrayPolyfill()),
      crosstalk::crosstalkLibs(),
      list(plotlyHtmlwidgetsCSS()),
      list(plotlyMainBundle())
    )
  )
}
to_JSON <- function(x, ...) {
  jsonlite::toJSON(x, digits = 50, auto_unbox = TRUE, force = TRUE,
                   null = "null", na = "null", ...)
}
typedArrayPolyfill <- function() {
  htmltools::htmlDependency(
    name = "typedarray", 
    version = "0.1",
    package = "plotly",
    src = dependency_dir("typedarray"),
    script = "typedarray.min.js",
    all_files = FALSE
  )
}
dependency_dir <- function(...) {
  file.path('htmlwidgets', 'lib', ...)
}
plotlyHtmlwidgetsCSS <- function() {
  htmltools::htmlDependency(
    name = "plotly-htmlwidgets-css", 
    version = plotlyMainBundle()$version,
    package = "plotly",
    src = dependency_dir("plotlyjs"),
    stylesheet = "plotly-htmlwidgets.css",
    all_files = FALSE
  )
}
plotlyMainBundle <- function() {
  htmltools::htmlDependency(
    name = "plotly-main", 
    version = "1.52.2",
    package = "plotly",
    src = dependency_dir("plotlyjs"),
    script = "plotly-latest.min.js",
    all_files = FALSE
  )
}

# (8). plotly build
plotly_build <- function(p, registerFrames = TRUE) {
  UseMethod("plotly_build")
}
plotly_build.NULL <- function(...) {
  htmltools::browsable(htmltools::div(...))
}
plotly_build.list <- function(p, registerFrames = TRUE) {
  plotly_build(as_widget(p))
}
plotly_build.gg <- function(p, registerFrames = TRUE) {
  # note: since preRenderHook = plotly_build in as_widget(),
  # plotly_build.plotly() will be called on gg objects as well
  plotly_build(ggplotly(p))
}
plotly_build.plotly <- function(p, registerFrames = TRUE) {
  
  # make this plot retrievable
  set_last_plot(p)
  
  layouts <- Map(function(x, y) {
    
    d <- plotly_data(p, y)
    x <- rapply(x, eval_attr, data = d, how = "list")
    
    # if an annotation attribute is an array, expand into multiple annotations
    nAnnotations <- max(lengths(x$annotations) %||% 0)
    if (!is.null(names(x$annotations))) {
      # font is the only list object, so store it, and attach after transposing
      font <- x$annotations[["font"]]
      x$annotations <- purrr::transpose(lapply(x$annotations, function(x) {
        as.list(rep(x, length.out = nAnnotations))
      }))
      for (i in seq_len(nAnnotations)) {
        x$annotations[[i]][["font"]] <- font
      }
    }
    
    x[lengths(x) > 0]
    
  }, p$x$layoutAttrs, names2(p$x$layoutAttrs))
  
  # get rid of the data -> layout mapping
  p$x$layoutAttrs <- NULL
  
  # accumulate, rather than override, annotations.
  annotations <- Reduce(c, c(
    list(p$x$layout$annotations),
    setNames(compact(lapply(layouts, "[[", "annotations")), NULL)
  ))
  
  # merge layouts into a single layout (more recent layouts will override older ones)
  p$x$layout <- modify_list(p$x$layout, Reduce(modify_list, layouts))
  p$x$layout$annotations <- annotations
  
  # keep frame mapping for populating layout.slider.currentvalue in animations
  frameMapping <- unique(unlist(
    lapply(p$x$attrs, function(x) deparse2(x[["frame"]])),
    use.names = FALSE
  ))
  if (length(frameMapping) > 1) {
    warning("Only one `frame` variable is allowed", call. = FALSE)
  }
  
  # Attributes should be NULL if none exist (rather than an empty list)
  if (length(p$x$attrs) == 0) p$x$attrs <- NULL
  
  # If there is just one (unevaluated) trace, and the data is sf, add an sf layer
  if (length(p$x$attrs) == 1 && !inherits(p$x$attrs[[1]], "plotly_eval") && is_sf(plotly_data(p))) {
    p <- add_sf(p)
  }
  
  # If type was not specified in plot_ly(), it doesn't create a trace unless
  # there are no other traces
  if (is.null(p$x$attrs[[1]][["type"]]) && length(p$x$attrs) > 1) {
    p$x$attrs[[1]] <- NULL
  }
  
  # have the attributes already been evaluated?
  is.evaled <- function(x) inherits(x, "plotly_eval")
  attrsToEval <- p$x$attrs[!vapply(p$x$attrs, is.evaled, logical(1))]
  
  # trace type checking and renaming for plot objects
  if (is_mapbox(p) || is_geo(p)) {
    p <- geo2cartesian(p)
    attrsToEval <- lapply(attrsToEval, function(tr) {
      if (!grepl("scatter|choropleth", tr[["type"]] %||% "scatter")) {
        stop("Cant add a '", tr[["type"]], "' trace to a map object", call. = FALSE)
      }
      if (is_mapbox(p)) tr[["type"]] <- tr[["type"]] %||% "scattermapbox"
      if (is_geo(p)) {
        tr[["type"]] <- if (!is.null(tr[["z"]])) "choropleth" else "scattergeo"
      }
      tr
    })
  }
  
  dats <- Map(function(x, y) {
    
    # grab the data for this trace
    dat <- plotly_data(p, y)
    
    # formula/symbol/attribute evaluation
    trace <- structure(
      rapply(x, eval_attr, data = dat, how = "list"),
      class = oldClass(x)
    )
    
    # determine trace type (if not specified, can depend on the # of data points)
    # note that this should also determine a sensible mode, if appropriate
    trace <- verify_type(trace)
    # verify orientation of boxes/bars
    trace <- verify_orientation(trace)
    # supply sensible defaults based on trace type
    trace <- coerce_attr_defaults(trace, p$x$layout)
    
    
    
    # attach crosstalk info, if necessary
    if (crosstalk_key() %in% names(dat) && isTRUE(trace[["inherit"]] %||% TRUE)) {
      trace[["key"]] <- trace[["key"]] %||% dat[[crosstalk_key()]]
      trace[["set"]] <- trace[["set"]] %||% attr(dat, "set")
    }
    
    # if appropriate, tack on a group index
    grps <- if (has_group(trace)) tryNULL(dplyr::group_vars(dat))
    if (length(grps) && any(lengths(trace) == NROW(dat))) {
      trace[[".plotlyGroupIndex"]] <- interaction(dat[, grps, drop = F])
    }
    
    # add sensible axis names to layout
    for (i in c("x", "y", "z")) {
      nm <- paste0(i, "axis")
      idx <- which(names(trace) %in% i)
      if (length(idx) == 1) {
        title <- default(deparse2(x[[idx]]))
        if (is3d(trace$type) || i == "z") {
          p$x$layout$scene[[nm]]$title <<- p$x$layout$scene[[nm]]$title %||% title
        } else {
          p$x$layout[[nm]]$title <<- p$x$layout[[nm]]$title %||% title
        }
      }
    }
    
    if (inherits(trace, c("plotly_surface", "plotly_contour"))) {
      # TODO: generate matrix for users?
      # (1) if z is vector, and x/y are null throw error
      # (2) if x/y/z are vectors and length(x) * length(y) == length(z), convert z to matrix
      if (!is.matrix(trace[["z"]]) || !is.numeric(trace[["z"]])) {
        stop("`z` must be a numeric matrix", call. = FALSE)
      }
    }
    
    # collect non-positional scales, plotly.js data_arrays, and "special"
    # array attributes for "data training"
    Attrs <- Schema$traces[[trace[["type"]]]]$attributes
    isArray <- vapply(Attrs, function(x) {
      tryFALSE(identical(x[["valType"]], "data_array"))
    }, logical(1))
    arrayOk <- vapply(Attrs, function(x) tryNULL(x[["arrayOk"]]) %||% FALSE, logical(1))
    # "non-tidy" traces allow x/y of different lengths, so ignore those
    dataArrayAttrs <- if (is_tidy(trace)) names(Attrs)[isArray | arrayOk]
    allAttrs <- c(
      dataArrayAttrs, special_attrs(trace), npscales(), "frame",
      # for some reason, text isn't listed as a data array in some traces
      # I'm looking at you scattergeo...
      ".plotlyGroupIndex", "text", "key", "fillcolor", "name", "legendgroup"
    )
    tr <- trace[names(trace) %in% allAttrs]
    # TODO: does it make sense to "train" matrices/2D-tables (e.g. z)?
    tr <- tr[vapply(tr, function(x) is.null(dim(x)) && is.atomic(x), logical(1))]
    # white-list customdata as this can be a non-atomic vector
    tr$customdata <- trace$customdata
    builtData <- tibble::as_tibble(tr)
    # avoid clobbering I() (i.e., variables that shouldn't be scaled)
    for (i in seq_along(tr)) {
      if (inherits(tr[[i]], "AsIs")) builtData[[i]] <- I(builtData[[i]])
    }
    
    if (NROW(builtData) > 0) {
      # Build the index used to split one "trace" into multiple traces
      isAsIs <- vapply(builtData, function(x) inherits(x, "AsIs"), logical(1))
      isDiscrete <- vapply(builtData, is.discrete, logical(1))
      # note: can only have one linetype per trace
      isSplit <- names(builtData) %in% c("split", "linetype", "frame", "fillcolor", "name") |
        !isAsIs & isDiscrete & names(builtData) %in% c("symbol", "color")
      if (any(isSplit)) {
        paste2 <- function(x, y) if (identical(x, y)) x else paste(x, y, sep = br())
        splitVars <- builtData[isSplit]
        builtData[[".plotlyTraceIndex"]] <- Reduce(paste2, splitVars)
        # in registerFrames() we need to strip the frame from .plotlyTraceIndex
        # so keep track of which variable it is...
        trace$frameOrder <- which(names(splitVars) %in% "frame")
      }
      # Build the index used to determine grouping (later on, NAs are inserted
      # via group2NA() to create the groups). This is done in 3 parts:
      # 1. Sort data by the trace index since groups are nested within traces.
      # 2. Translate missing values on positional scales to a grouping variable.
      #    If grouping isn't relevant for this trace, a warning is thrown since
      #    NAs are removed.
      # 3. The grouping from (2) and any groups detected via dplyr::groups()
      #    are combined into a single grouping variable, .plotlyGroupIndex
      builtData <- arrange_safe(builtData, ".plotlyTraceIndex")
      isComplete <- complete.cases(builtData[names(builtData) %in% c("x", "y", "z")])
      # warn about missing values if groups aren't relevant for this trace type
      if (any(!isComplete) && !has_group(trace)) {
        warning("Ignoring ", sum(!isComplete), " observations", call. = FALSE)
      }
      builtData[[".plotlyMissingIndex"]] <- cumsum(!isComplete)
      builtData <- builtData[isComplete, ]
      if (length(grps) && has_group(trace) && isTRUE(trace[["connectgaps"]])) {
        stop(
          "Can't use connectgaps=TRUE when data has group(s).", call. = FALSE
        )
      }
      builtData[[".plotlyGroupIndex"]] <- interaction(
        builtData[[".plotlyGroupIndex"]] %||% "",
        builtData[[".plotlyMissingIndex"]]
      )
      builtData <- arrange_safe(builtData,
                                c(".plotlyTraceIndex", ".plotlyGroupIndex",
                                  if (inherits(trace, "plotly_line")) "x")
      )
      builtData <- train_data(builtData, trace)
      trace[[".plotlyVariableMapping"]] <- names(builtData)
      # copy over to the trace data
      for (i in names(builtData)) {
        trace[[i]] <- builtData[[i]]
      }
    }
    # TODO: provide a better way to clean up "high-level" attrs
    trace[c("ymin", "ymax", "yend", "xend")] <- NULL
    trace[lengths(trace) > 0]
    
  }, attrsToEval, names2(attrsToEval))
  
  p$x$attrs <- lapply(p$x$attrs, function(x) structure(x, class = "plotly_eval"))
  
  # traceify by the interaction of discrete variables
  traces <- list()
  for (i in seq_along(dats)) {
    d <- dats[[i]]
    scaleAttrs <- names(d) %in% paste0(npscales(), "s")
    traces <- c(traces, traceify(d[!scaleAttrs], d$.plotlyTraceIndex))
    if (i == 1) traces[[1]] <- c(traces[[1]], d[scaleAttrs])
  }
  
  # insert NAs to differentiate groups
  traces <- lapply(traces, function(x) {
    d <- tibble::as_tibble(x[names(x) %in% x$.plotlyVariableMapping])
    d <- group2NA(
      d, if (has_group(x)) ".plotlyGroupIndex",
      ordered = if (inherits(x, "plotly_line")) "x",
      retrace.first = inherits(x, "plotly_polygon")
    )
    for (i in x$.plotlyVariableMapping) {
      # try to reduce the amount of data we have to send for non-positional scales
      entry <- if (i %in% npscales()) uniq(d[[i]]) else d[[i]]
      if (is.null(entry)) {
        x[[i]] <- NULL  
      } else {
        x[[i]] <- structure(entry, class = oldClass(x[[i]]))  
      }
    }
    x
  })
  
  # Map special plot_ly() arguments to plotly.js trace attributes.
  # Note that symbol/linetype can modify the mode, so those are applied first
  # TODO: use 'legends 2.0' to create legends for these discrete mappings
  # https://github.com/plotly/plotly.js/issues/1668
  if (length(traces)) {
    traces <- map_symbol(traces)
    traces <- map_linetype(traces)
    traces <- map_size(traces)
    traces <- map_size(traces, stroke = TRUE) #i.e., span
    colorTitle <- unlist(lapply(p$x$attrs, function(x) { deparse2(x[["color"]] %||% x[["z"]]) }))
    strokeTitle <- unlist(lapply(p$x$attrs, function(x) deparse2(x[["stroke"]])))
    traces <- map_color(traces, title = paste(colorTitle, collapse = br()), colorway = colorway(p))
    traces <- map_color(traces, stroke = TRUE, title = paste(strokeTitle, collapse = br()), colorway = colorway(p))
  }
  
  for (i in seq_along(traces)) {
    # remove special mapping attributes
    mappingAttrs <- c(
      "alpha", "alpha_stroke", npscales(), paste0(npscales(), "s"),
      ".plotlyGroupIndex", ".plotlyMissingIndex",
      ".plotlyTraceIndex", ".plotlyVariableMapping", "inherit"
    )
    for (j in mappingAttrs) {
      traces[[i]][[j]] <- NULL
    }
  }
  
  # .crossTalkKey -> key
  traces <- lapply(traces, function(x) {
    setNames(x, sub(crosstalk_key(), "key", names(x), fixed = TRUE))
  })
  
  # it's possible that the plot object already has some traces
  # (like figures pulled from a plotly server)
  p$x$data <- setNames(c(p$x$data, traces), NULL)
  
  # supply linked highlighting options/features
  p <- supply_highlight_attrs(p)
  
  # supply trace anchor and domain information
  p <- supply_defaults(p)
  
  # attribute naming corrections for "geo-like" traces
  p <- cartesian2geo(p)
  
  # Compute sensible bounding boxes for each mapbox/geo subplot
  p <- fit_bounds(p)
  
  # polar charts don't like null width/height keys
  if (is.null(p$x$layout[["height"]])) p$x$layout[["height"]] <- NULL
  if (is.null(p$x$layout[["width"]])) p$x$layout[["width"]] <- NULL
  
  # ensure we get the order of categories correct
  # (plotly.js uses the order in which categories appear by default)
  p <- populate_categorical_axes(p)
  # translate '\n' to '<br />' in text strings
  p <- translate_linebreaks(p)
  # if it makes sense, add markers/lines/text to mode
  p <- verify_mode(p)
  # annotations & shapes must be an array of objects
  # TODO: should we add anything else to this?
  p <- verify_arrays(p)
  # set a sensible hovermode if it hasn't been specified already
  p <- verify_hovermode(p)
  # try to convert to webgl if toWebGl was used
  p <- verify_webgl(p)
  # throw warning if webgl is being used in shinytest
  # currently, shinytest won't rely this warning, but it should
  # https://github.com/rstudio/shinytest/issues/146
  if (isTRUE(getOption("shiny.testmode"))) {
    if (is.webgl(p)) warning("shinytest can't currently render WebGL-based graphics.")
  }
  # crosstalk dynamically adds traces, meaning that a legend could be dynamically
  # added, which is confusing. So here we populate a sensible default.
  p <- verify_showlegend(p)
  
  # NOTE: this needs to occur *before* registering frames so simple/nested key
  # flags get passed onto frame data.
  p <- verify_key_type(p)
  
  if (registerFrames) {
    p <- registerFrames(p, frameMapping = frameMapping)
  }
  
  # set the default plotly.js events to register in shiny
  p <- shiny_defaults_set(p)
  
  p <- verify_guides(p)
  
  # verify colorscale attributes are in a sensible data structure
  p <- verify_colorscale(p)
  
  # verify plot attributes are legal according to the plotly.js spec
  p <- verify_attr_names(p)
  # box up 'data_array' attributes where appropriate
  p <- verify_attr_spec(p)
  
  # make sure we're including mathjax (if TeX() is used)
  p <- verify_mathjax(p)
  
  # if a partial bundle was specified, make sure it supports the visualization
  p <- verify_partial_bundle(p)
  
  # scattergl currently doesn't render in RStudio on Windows
  # https://github.com/ropensci/plotly/issues/1214
  p <- verify_scattergl_platform(p)
  
  # make sure plots don't get sent out of the network (for enterprise)
  p$x$base_url <- get_domain()
  p
}
