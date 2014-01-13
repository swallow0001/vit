sampvarGUIHandler <- function(e){
    e$method <- "sampvar"
    # Rewriting the menus to link only to other modules
    svalue(e$g.menu) <- getModuleMenu(e)

    e$data.boxes <- FALSE
    e$replace <- FALSE
    e$stat.scale <- TRUE
    tbl <- glayout(container = e$upper)
    tbl[1, 1] <- glabel("Quantity: ", container = tbl)
    tbl[1, 2] <- (e$stat <- gcombobox(c("mean", "median"),
                                      editable = TRUE, container = tbl,
                                      handler = function(h, ...) {
                                        if (!is.null(e$c1$levels) & !is.categorical(e$c1$x)){
                                            if (length(unique(e$c1$levels)) == 2) {
                                                svalue(e$sampvar.choices) <- "difference"
                                                if (svalue(e$stat) == "mean") {
                                                    e$sampvar.choices[] <- c("difference", "t-pooled", "t-Welch")
                                                    enabled(e$sampvar.label) <- TRUE
                                                    enabled(e$sampvar.choices) <- TRUE
                                                }
                                                if (svalue(e$stat) %in% c("median", "lower quartile", "upper quartile")) {
                                                    enabled(e$sampvar.choices) <- FALSE
                                                    enabled(e$sampvar.label) <- FALSE
                                                }
                                                if (svalue(e$stat) %in% c("IQR ratio", "default")) {
                                                    svalue(e$sampvar.choices) <- ""
                                                    enabled(e$sampvar.label) <- FALSE
                                                    enabled(e$sampvar.choices) <- FALSE
                                                }
                                            }
                                        }
                                      }))
    tbl[2, 1] <- (e$sampvar.label <- glabel("Statistic: ", container = tbl))
    tbl[2, 2] <- (e$sampvar.choices <- gcombobox("", editable = TRUE, container = tbl))
    tbl[3, 1] <- (e$loi.label <- glabel("Level of interest: ", container = tbl))
    tbl[3, 2] <- (e$loi.choices <- gcombobox("", editable = TRUE, container = tbl,
                                             handler = function(h, ...) {
                                             enabled(e$obj) <- FALSE
                                             if (!e$loaded){
                                                 e$resetCanvas()
                                                 loadStatDetails(e)
                                                 e$c1$drawImage()
                                                 e$loaded <- FALSE
                                             }
                                             enabled(e$obj) <- TRUE
                                         }))
    tbl[4, 1] <- (e$size.label <- glabel("Sample size: ", container = tbl))
    tbl[4, 2] <- (e$ssize <- gedit("10", container = tbl))
    if (is.categorical(e$c1$x) & ! is.null(e$c1$levels))
        e$loi.choices[] <- sort(unique(e$c1$x))
    gbutton("Record my choices", container = e$upper, expand = TRUE,
            handler = function(h, ...) {
                # Resetting the GUI elements
                enabled(e$obj) <- FALSE
                e$buildCanvas()
                e$c1$data.file <- tag(e$obj, "data.file") # Grabbing data filename for labels
                e$c1$drawImage()
                loadStatDetails(e)
                if (e$loaded){
                    if (is.categorical(e$c1$x) | e$c1$stat.in.use == "IQR ratio"
                        | e$c1$stat.in.use == "slope"){
                        enabled(e$fade.data) <- FALSE
                        e$can.fade <- FALSE
                    } else {
                        enabled(e$fade.data) <- TRUE
                        e$can.fade <- TRUE
                        e$fade <- TRUE
                    }
                    if (!is.null(e$c1$levels)) {
                        min.ssize <- 2
                    } else {
                        min.ssize <- 1
                    }
                    ssize <- as.numeric(svalue(e$ssize))
                    if (ssize < min.ssize | ssize > e$c1$n){
                           grid.newpage()
                        grid.text(paste("Sample size must be between", min.ssize, "and", e$c1$n,
                                        sep = " "))
                        enabled(e$obj) <- TRUE
                        return()
                    } else {
                        e$c1$n <- ssize
                    }
                    e$c1$makeSamples(e$replace, sample.both = e$difference,
                                     perm.paired.samples = FALSE)
                    if (svalue(e$stat) %in% c("default", "median", "upper quartile", "lower quartile", "IQR ratio")){
                        e$c1$makeQuartiles()
                        if (!is.null(e$c1$levels))
                            e$c1$orderLevels()
                    }
                    e$c1$makeStatistics()
                    if (e$difference & ! is.categorical(e$xData)) {
                        y.scale <- NULL
                        stat.y.scale <- NULL
                        extra.box <- FALSE
                        if (svalue(e$stat) == "IQR ratio") {
                            # Level 2 is, on average, larger than level 1
                            # therefore create stats relative to that.
                            # Use a log10 scale to plot stats.
                            valid.xs <- apply(e$c1$stat.dist, 1, function (x) all(x != 0))
                            stats <- log10(e$c1$stat.dist[valid.xs, 2] / e$c1$stat.dist[valid.xs, 1])
                            stat.scale <- c(min(stats), max(stats))

                            # We want nice labels for the scale of our stat viewport.
                            # As a result, we need to rescale the viewport to ensure that
                            # our nice labels can be created, whilst also ensuring that the distribution
                            # spans as much of the stat viewport as possible.
                            unlogged.range <- 10^stat.scale
                            log2.range <- log(unlogged.range, base = 2)
                            twos.ranges <- c(floor(log2.range[1]), ceiling(log2.range[2]))
                            logged.scale <- twos.ranges[1]:twos.ranges[2]
                            nice.scale <- 2^logged.scale
                            stat.scale <- range(log10(nice.scale))
                            ## Sorting viewports for scatterplots.
                        } else if (svalue(e$stat) == "slope"){
                            if (diff(range(e$c1$x)) == 0){
                                x.scale <- range(e$c1$x) + c(-1, 1)
                            } else {
                                x.scale <- range(e$c1$x) + c(-0.04, 0.04)*diff(range(e$c1$x))
                            }
                            ## y.scale as defined by data.
                            if (diff(range(e$c1$levels)) == 0){
                                y.scale <- range(e$c1$levels) + c(-1, 1)
                            } else {
                                y.scale <- range(e$c1$levels) +
                                    c(-0.04, 0.04)*diff(range(e$c1$levels))
                            }
                            e$c1$calcxScaleYs(e, x.scale)
                            coeffs <- coefficients(lm(e$c1$levels ~ e$c1$x))
                            current.stats <- coeffs[1] + coeffs[2]*x.scale
                            all.ys <- rbind(e$c1$x.scale.ys, current.stats)
                            x.scale.y.diffs <- apply(all.ys, 1, diff)
                            y.diff.1 <- diff(range(c(y.scale, all.ys)))
                            y.diff.2 <- diff(c(min(c(0, x.scale.y.diffs)),
                                             max(c(0, x.scale.y.diffs))))
                            y.diff <- max(c(y.diff.1, y.diff.2))
                            if (y.diff.2 < y.diff.1){
                                y.scale <- range(c(y.scale, all.ys))
                                extra.on.stat <- diff(c(y.diff.2, y.diff))/2
                                stat.y.scale <- (c(min(c(0, x.scale.y.diffs)),
                                             max(c(0, x.scale.y.diffs))) +
                                                 c(-1, 1)*extra.on.stat)/diff(x.scale)
                            } else if (y.diff.2 >= y.diff.1){
                                extra.on.data <- diff(c(y.diff.1, y.diff))/2
                                y.scale <- range(c(y.scale, all.ys)) + c(-1, 1)*extra.on.data
                                stat.y.scale <- c(min(c(0, x.scale.y.diffs)),
                                             max(c(0, x.scale.y.diffs)))/diff(x.scale)
                            }
                            stats <- e$c1$stat.dist[, 2]
                            stat.scale <- c(0, 1)
                            extra.box <- TRUE
                        } else if (svalue(e$stat) == "mean" & (svalue(e$sampvar.choices) == "t-Welch" | svalue(e$sampvar.choices) == "t-pooled")){
                            stats <- e$c1$stat.dist[, 2]
                            stats <- na.omit(stats)
                            meanstat <- mean(stats)
                            stat.scale <- range(stats)
                        } else {
                            stats <- apply(e$c1$stat.dist, 1, diff)
                            stats <- na.omit(stats)
                            meanstat <- mean(stats)
                            stat.scale <- meanstat + c(-0.5, 0.5)*diff(range(e$xData))
                        }
                        if (svalue(e$stat) != "slope"){
                            x.scale <- NULL
                            if (any(min(0, stats) < stat.scale[1] | max(0, stats) > stat.scale[2]) &
                                svalue(e$stat) != "IQR ratio") {
                                stat.scale <- range(c(0, stats))
                                scale.diff <- diff(stat.scale) - diff(range(e$xData))
                                if (scale.diff > 0){
                                    x.scale <- range(e$xData) + c(-0.5, 0.5)*scale.diff
                                } else {
                                    stat.scale <- stat.scale + c(-0.5, 0.5)*abs(scale.diff)
                                }
                            }
                        }
                        buildViewports(e$c1, e$xData, e$yData, e$data.boxes, x.scale = x.scale,
                                       y.scale = y.scale, stat.scale = stat.scale,
                                       stat.y.scale = stat.y.scale, extra.box = extra.box)
                    } else if (e$difference) {
                        stats <- apply(e$c1$stat.dist, 1, diff)
                        meanstat <- mean(stats)
                        stat.scale <- c(-0.5, 0.5)
                        x.scale <- NULL
                        if (any(stats < stat.scale[1] | stats > stat.scale[2])){
                            stat.scale <- range(stats)
                            scale.diff <- (diff(stat.scale) - 1) / 2
                            if (scale.diff > 0) {
                                x.scale <- c(0-scale.diff, 1+scale.diff)#* scale.diff
                            } else {
                                stat.scale <- stat.scale + c(-0.5, 0.5)*abs(scale.diff)
                            }
                        }
                        buildViewports(e$c1, e$c1$x, e$c1$levels, e$data.boxes,
                                       x.scale = x.scale, stat.scale = stat.scale)
                    } else {
                        buildViewports(e$c1, e$c1$x, e$c1$levels, e$data.boxes,
                                       x.scale = NULL, stat.scale = NULL)
                    }
                    e$c1$buildImage(!is.null(e$c1$levels) & !is.categorical(e$c1$levels))
                    if (is.categorical(e$xData)) {
                        e$c1$image <- editGrob(e$c1$image, gPath("dataAxis"), at = seq(0, 1, by = 0.2))
                        e$c1$image <- editGrob(e$c1$image, gPath("sampleAxis"), at = seq(0, 1, by = 0.2))
                    } else if (e$c1$stat.in.use == "slope"){
                        e$c1$image <- editGrob(e$c1$image, gPath("statAxis"),
                                               at = c(0, 1))
                    }
                    if (svalue(e$stat) == "default") {
                        e$c1$rmGrobs("statAxis")
                        enabled(e$stat.label) <- FALSE
                        enabled(e$diff.box.cont) <- FALSE
                        enabled(e$get.dist) <- FALSE
                    } else {
                        enabled(e$stat.label) <- TRUE
                        enabled(e$diff.box.cont) <- TRUE
                        enabled(e$get.dist) <- TRUE
                    }
                    if (svalue(e$stat) == "IQR ratio") {
                        nice.labels <- sapply(nice.scale, function(x) {
                            if (x < 1) {
                                paste("1 :", round(1 / x, 2))
                            } else {
                                paste(x, ": 1")
                            }
                        })
                        e$c1$image <- editGrob(e$c1$image, gPath("statAxis"),
                                               at = log10(nice.scale), label = nice.labels)
                    }
                    pushViewport(e$c1$viewports)
                    e$c1$plotData()
                    e$c1$plotDataStat(e)
                    e$c1$showLabels()
                    e$c1$drawImage()
                    e$c1$getStatYPos()
                    enabled(e$lower) <- TRUE
                }
                enabled(e$show.dist) <- FALSE
                enabled(e$obj) <- TRUE
            })
    e$vit.samp <- glabel("Sampling", container = e$lower)
    e$samp.frame.cont <- ggroup(container = e$lower)
    vit.sampbox <- gframe("Number of repetitions",
                          expand = TRUE,
                          container = e$samp.frame.cont)
    e$redraw.radio <- gradio(c(1, 5, 20, 1000), horizontal=FALSE)
    add(vit.sampbox, e$redraw.radio)
    add(e$samp.frame.cont, vit.sampbox)
    buttons1 <- ggroup(container = e$lower)
    e$clear.stat <- FALSE
    e$points <- FALSE
    e$get.sample <- gbutton("Go", container = e$lower, expand = TRUE,
                            handler = function(h, ...){
                                if (svalue(e$get.sample) == "Go") {
                                    e$c1$animationPaused <- FALSE
                                    # Resetting the widgets to whatever settings are currently in use
                                    svalue(e$stat) <- e$c1$stat.in.use
                                    svalue(e$ssize) <- e$c1$n
                                    if (is.categorical(e$c1$x))
                                        svalue(e$loi.choices) <- e$c1$loi
                                    enabled(e$stop.button) <- TRUE
                                    enabled(e$upper) <- FALSE
                                    enabled(e$vit.samp) <- FALSE
                                    enabled(e$samp.frame.cont) <- FALSE
                                    enabled(e$stat.label) <- FALSE
                                    enabled(e$diff.box.cont) <- FALSE
                                    if (e$can.fade){
                                        enabled(e$fade.data) <- FALSE
                                    }
                                    enabled(e$get.dist) <- FALSE
                                    enabled(e$show.dist) <- FALSE
                                    svalue(e$get.sample) <- "Pause"
                                    if (svalue(e$redraw.radio) == 1000){
                                        e$clearPanel(panel = "stat")
                                        e$clearPanel(panel = "sample")
                                        e$c1$handle1000(e, points = FALSE)
                                        e$clear.stat <- TRUE
                                    } else {
                                        if (e$clear.stat){
                                            e$clearPanel("stat")
                                            e$clearPanel("sample")
                                            e$clear.stat <- FALSE
                                        }
                                        n <- svalue(e$redraw.radio)
                                        for (i in 1:n){
                                            # When we have stopped, don't bother animating further
                                            if (! e$c1$stopAnimation) {
                                                if (n == 1){
                                                    e$c1$animateSample(e, n.steps = 10, n.slow = 10,
                                                                       opts = list(keep.plot = TRUE,
                                                                                   move = TRUE,
                                                                                   drop.points = TRUE))
                                                } else if (n == 5) {
                                                    e$c1$animateSample(e, n.steps = 10, n.slow = 0,
                                                                       opts = list(keep.plot = TRUE,
                                                                                   move = TRUE,
                                                                                   drop.points = FALSE))
                                                }
                                                # If we have stopped during our animation don't do anything more
                                                if (! e$c1$stopAnimation) {
                                                    e$c1$plotSample(e)
                                                    e$c1$showLabels()
                                                    e$c1$drawImage()
                                                    e$c1$advanceWhichSample()
                                                }
                                            }
                                        }
                                    }

                                    # If we have stopped at some point, ensure we clean up
                                    # to avoid an inconsistent state
                                    if (e$c1$stopAnimation) {
                                        e$c1$reset()
                                        e$c1$resetPlots(e)
                                        e$c1$stopAnimation <- FALSE
                                        e$c1$drawImage()
                                    }

                                    enabled(e$stop.button) <- FALSE
                                    enabled(e$upper) <- TRUE
                                    enabled(e$vit.samp) <- TRUE
                                    enabled(e$samp.frame.cont) <- TRUE
                                    if (e$can.fade){
                                        enabled(e$fade.data) <- TRUE
                                    }
                                    if (svalue(e$stat) != "default") {
                                        enabled(e$stat.label) <- TRUE
                                        enabled(e$diff.box.cont) <- TRUE
                                        enabled(e$get.dist) <- TRUE
                                    }
                                    svalue(e$get.sample) <- "Go"
                                    return()
                                }

                                if (svalue(e$get.sample) == "Pause") {
                                    e$c1$animationPaused <- TRUE
                                    svalue(e$get.sample) <- "Play"
                                    return()
                                }

                                if (svalue(e$get.sample) == "Play") {
                                    e$c1$animationPaused <- FALSE
                                    svalue(e$get.sample) <- "Pause"
                                    return()
                                }
                            })
    e$fade.data <- gbutton(text = "Fade population", expand = TRUE,
                           container = e$lower,
                           handler = function(h, ...){
                               svalue(e$stat) <- e$c1$stat.in.use
                               svalue(e$ssize) <- e$c1$n
                               if (is.categorical(e$c1$x))
                                   svalue(e$loi.choices) <- e$c1$loi
                               enabled(e$upper) <- FALSE
                               enabled(e$vit.samp) <- FALSE
                               enabled(e$samp.frame.cont) <- FALSE
                               enabled(e$stat.label) <- FALSE
                               enabled(e$diff.box.cont) <- FALSE
                               enabled(e$get.sample) <- FALSE
                               if (e$can.fade){
                                   enabled(e$fade.data) <- FALSE
                               }
                               if.showdist.enabled <- enabled(e$show.dist)
                               enabled(e$get.dist) <- FALSE
                               enabled(e$show.dist) <- FALSE
                               e$c1$fadePlots(e)
                               e$c1$drawImage()
                               enabled(e$upper) <- TRUE
                               enabled(e$vit.samp) <- TRUE
                               enabled(e$samp.frame.cont) <- TRUE
                               enabled(e$get.sample) <- TRUE
                               if (e$can.fade){
                                   enabled(e$fade.data) <- TRUE
                               }
                               if (svalue(e$stat) != "default") {
                                   enabled(e$stat.label) <- TRUE
                                   enabled(e$diff.box.cont) <- TRUE
                                   enabled(e$get.dist) <- TRUE
                                   enabled(e$show.dist) <- if.showdist.enabled
                               }
                           })
    addSpace(e$lower, 20, horizontal=FALSE)

    e$stat.label <- glabel("Include sampling distribution", container = e$lower)
    e$diff.box.cont <- ggroup(container = e$lower)
    vit.diffbox <- gframe("Number of repetitions",
                          expand = TRUE,
                          container = e$diff.box.cont)
    e$sample.radio <- gradio(c(1, 5, 20, 1000),
                                horizontal = FALSE)
    add(vit.diffbox,e$sample.radio)
    add(e$diff.box.cont, vit.diffbox)

    buttons2 <- ggroup(horizontal = FALSE, container = e$lower)
    e$get.dist <- gbutton(text = "Go", expand = TRUE,
                          container = buttons2, handler = function(h, ...) {
                              if (svalue(e$get.dist) == "Go") {
                                  e$c1$animationPaused <- FALSE
                                  ## Resetting the widgets to whatever settings are currently in use
                                  svalue(e$stat) <- e$c1$stat.in.use
                                  svalue(e$ssize) <- e$c1$n
                                  if (is.categorical(e$c1$x)){
                                      svalue(e$loi.choices) <- e$c1$loi
                                  }
                                  enabled(e$stop.button) <- TRUE
                                  enabled(e$upper) <- FALSE
                                  enabled(e$vit.samp) <- FALSE
                                  enabled(e$samp.frame.cont) <- FALSE
                                  enabled(e$stat.label) <- FALSE
                                  enabled(e$diff.box.cont) <- FALSE
                                  enabled(e$get.sample) <- FALSE
                                  if (e$can.fade){
                                      enabled(e$fade.data) <- FALSE
                                  }
                                  enabled(e$show.dist) <- FALSE
                                  svalue(e$get.dist) <- "Pause"
                                  if (e$clear.stat){
                                      e$clearPanel("stat")
                                      e$clearPanel("sample")
                                      e$clear.stat <- FALSE
                                  }
                                  if (svalue(e$sample.radio) == 1000){
                                      e$clearPanel("stat")
                                      e$clearPanel("sample")
                                      e$c1$handle1000(e, points = TRUE)
                                      e$clear.stat <- TRUE
                                      if (e$c1$stat.in.use == "mean"){
                                          if (!(e$sampvar.method == "t-pooled" |
                                                e$sampvar.method == "t-Welch")){
                                              enabled(e$show.dist) <- TRUE
                                          }
                                      } else if (e$c1$stat.in.use == "proportion") {
                                          enabled(e$show.dist) <- TRUE
                                      } else if (e$c1$stat.in.use == "slope"){
                                          enabled(e$show.dist) <- TRUE
                                      }
                                  } else {
                                      n <- svalue(e$sample.radio)
                                      for (i in 1:n){
                                          if (! e$c1$stopAnimation) {
                                              if (n == 1)
                                                  e$c1$animateSample(e, n.steps = 10, n.slow = 0,
                                                                     opts = list(keep.plot = FALSE, move = TRUE, drop.points = TRUE))
                                              if (n == 5 & e$c1$stat.in.use == "slope")
                                                  e$c1$animateSample(e, n.steps = 3, n.slow = 0,
                                                                     opts = list(keep.plot = FALSE, move = TRUE, drop.points = TRUE))
                                              if (! e$c1$stopAnimation) {
                                                  e$c1$plotSample(e)
                                                  e$c1$showLabels()
                                                  if (n != 20) {
                                                      e$c1$animateStat(e, n.steps = 15)
                                                  } else if (e$c1$stat.in.use == "slope"){
                                                      e$c1$animateStat(e, 3, move.point = FALSE)
                                                  }
                                                  e$c1$plotStatDist(e)
                                                  e$c1$advanceWhichSample()
                                                  e$c1$drawImage()
                                              }
                                          }
                                      }
                                  }

                                  # If we have stopped at some point, ensure we clean up
                                  # to avoid an inconsistent state
                                  if (e$c1$stopAnimation) {
                                      e$c1$reset()
                                      e$c1$resetPlots(e)
                                      e$c1$stopAnimation <- FALSE
                                      e$c1$drawImage()
                                  }

                                  enabled(e$stop.button) <- FALSE
                                  enabled(e$upper) <- TRUE
                                  enabled(e$vit.samp) <- TRUE
                                  enabled(e$samp.frame.cont) <- TRUE
                                  enabled(e$stat.label) <- TRUE
                                  enabled(e$diff.box.cont) <- TRUE
                                  enabled(e$get.sample) <- TRUE
                                  if (e$can.fade){
                                      enabled(e$fade.data) <- TRUE
                                  }
                                  svalue(e$get.dist) <- "Go"
                                  return()
                              }

                              if (svalue(e$get.dist) == "Pause") {
                                  e$c1$animationPaused <- TRUE
                                  svalue(e$get.dist) <- "Play"
                                  return()
                              }

                              if (svalue(e$get.dist) == "Play") {
                                  e$c1$animationPaused <- FALSE
                                  svalue(e$get.dist) <- "Pause"
                                  return()
                              }
                          })
    e$show.dist <- gbutton("Show theoretical distribution", expand = TRUE,
                           container = e$lower,
                           handler = function(h, ...){
                               e$c1$animationPaused <- FALSE
                               e$c1$displayResult(e)
                               enabled(e$show.dist) <- FALSE
                           })
}

