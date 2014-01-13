permvarGUIHandler <- function(e) {
    e$method <- "permvar"

    # Rewriting the menus to link only to other modules
    svalue(e$g.menu) <- getModuleMenu(e)

    e$data.boxes <- TRUE
    e$replace <- FALSE
    e$stat.scale <- FALSE
    tbl <- glayout(container = e$upper)
    tbl[1, 1] <- glabel("Quantity: ", container = tbl)
    tbl[1, 2] <- (e$stat <- gcombobox(c("mean", "median"), editable = TRUE, container = tbl,
                  handler = function(h, ...){
                      if (! is.categorical(e$c1$x)) {
                          if (svalue(e$ngroups.choices) > 2) {
                              if (svalue(e$stat) == "mean"){
                                  enabled(e$perm.choices) <- TRUE
                                  svalue(e$perm.choices) <- "F-statistic"
                                  e$perm.choices[] <- c("F-statistic", "average deviation")
                              } else if (svalue(e$stat) == "median"){
                                  enabled(e$perm.choices) <- TRUE
                                  svalue(e$perm.choices) <- "pseudo F-statistic"
                                  e$perm.choices[] <- c("pseudo F-statistic", "average deviation")
                              }
                          } else if (svalue(e$ngroups.choices) == 2) {
                              if (svalue(e$stat) == "mean") {
                                  enabled(e$perm.choices) <- TRUE
                                  e$perm.choices[] <- c("difference", "t-pooled", "t-Welch")
                                  svalue(e$perm.choices) <- "difference"
                              } else if (svalue(e$stat) == "median") {
                                  svalue(e$perm.choices) <- "difference"
                                  e$perm.choices[] <- "difference"
                                  enabled(e$perm.choices) <- FALSE
                              }
                          }
                      }
                  }))
    tbl[2, 1] <- (e$perm.label <- glabel("Statistic: ", container = tbl))
    tbl[2, 2] <- (e$perm.choices <- gcombobox("", editable = TRUE, container = tbl))
    tbl[3, 1] <- (e$loi.label <- glabel("Level of interest: ", container = tbl))
    tbl[3, 2] <- (e$loi.choices <- gcombobox("", editable = TRUE, container = tbl,
                                             handler = function(h, ...)
                                         {
                                             enabled(e$obj) <- FALSE
                                             if (!e$loaded){
                                                 e$resetCanvas()
                                                 loadStatDetails(e)
                                                 e$c1$drawImage()
                                                 e$loaded <- FALSE
                                             }
                                             enabled(e$obj) <- TRUE
                                         }))
    tbl[4, 1] <- (e$ngroups.label <- glabel("Number of groups: ", container = tbl))
    tbl[4, 2] <- (e$ngroups.choices <- gcombobox(2:9, editable = FALSE, container = tbl,
                                                 handler = function(h, ...) {
                                                     if (is.categorical(e$c1$x)) {
                                                         if (svalue(e$ngroups.choices) == 2) {
                                                             e$perm.choices[] <- "difference"
                                                             svalue(e$perm.choices) <- "difference"
                                                             enabled(e$perm.choices) <- FALSE
                                                         }
                                                         if (svalue(e$ngroups.choices) > 2) {
                                                             e$perm.choices[] <- c("average deviation", "chi-square statistic")
                                                             svalue(e$perm.choices) <- "average deviation"
                                                             enabled(e$perm.choices) <- TRUE
                                                         }
                                                     } else {
                                                         if (svalue(e$ngroups.choices) == 2) {
                                                             svalue(e$perm.choices) <- "difference"
                                                             if (svalue(e$stat) == "mean"){
                                                                 e$perm.choices[] <- c("difference", "t-pooled", "t-Welch")
                                                                 enabled(e$perm.choices) <- TRUE
                                                             } else if (svalue(e$stat) == "median") {
                                                                 enabled(e$perm.choices) <- FALSE
                                                             }
                                                         }
                                                         if (svalue(e$ngroups.choices) > 2) {
                                                             if (svalue(e$stat) == "mean") {
                                                                 svalue(e$perm.choices) <- "F-statistic"
                                                                 e$perm.choices[] <- c("F-statistic", "average deviation")
                                                             }
                                                             if (svalue(e$stat) == "median") {
                                                                 svalue(e$perm.choices) <- "pseudo F-statistic"
                                                                 e$perm.choices[] <- c("pseudo F-statistic", "average deviation")
                                                             }
                                                             enabled(e$perm.choices) <- TRUE
                                                         }
                                                     }
                                                 }))
    if (is.categorical(e$c1$x) & ! is.null(e$c1$levels)) {
        e$loi.choices[] <- sort(unique(e$c1$x))
        e$perm.choices[] <- "average deviation"
    }
    gbutton("Record my choices", container = e$upper, expand = TRUE,
            handler = function(h, ...) {
                enabled(e$obj) <- FALSE
                # Need to do the following twice to build viewports
                # correctly and setting levels again after the canvas
                # has been replaced with a new one.
                e$c1$ngroups <- svalue(e$ngroups.choices)
                e$c1$levels <- LETTERS[rep(e$c1$ngroups:1, length.out = length(e$c1$x))]
                e$c1$ylevels <- LETTERS[e$c1$ngroups:1]
                e$buildCanvas()
                e$c1$data.file <- tag(e$obj, "data.file") # Grabbing data filename for labels
                # Setting again...
                e$c1$ngroups <- svalue(e$ngroups.choices)
                e$c1$levels <- LETTERS[rep(e$c1$ngroups:1, length.out = length(e$c1$x))]
                e$c1$ylevels <- LETTERS[e$c1$ngroups:1]
                e$c1$buildBoxes()
                e$c1$drawImage()
                loadStatDetails(e)
                if (e$loaded) {
                    e$c1$makeSamples(e$replace)
                    e$c1$makeStatistics()
                    if (!is.categorical(e$c1$x)) {
                        if (!is.categorical(e$c1$levels)){
                            ## Viewport code for scatterplots.
                            ## Calculating x.scale.
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
                            ## Calculating y-coordinates of lines at x.scale.
                            e$c1$calcxScaleYs(e, x.scale)
                            coeffs <- coefficients(lm(e$c1$levels ~ e$c1$x))
                            current.stats <- coeffs[1] + coeffs[2]*x.scale
                            all.x.scale.ys <- rbind(e$c1$x.scale.ys, current.stats)
                            x.scale.y.diffs <- max(abs(apply(all.x.scale.ys, 1, diff)))
                            y.point.diffs <- max(abs(y.scale - mean(e$c1$levels)))
                            y.scale.diff <- max(x.scale.y.diffs, y.point.diffs)
                            y.scale <- mean(e$c1$levels) + c(-1, 1)*y.scale.diff
                            buildViewports(e$c1, e$c1$x, e$c1$levels, boxes = e$data.boxes,
                                           x.scale = x.scale, y.scale = y.scale,
                                           stat.scale = c(0, 1),
                                           stat.y.scale = c(-1, 1)*y.scale.diff/diff(x.scale),
                                           extra.box = TRUE)
                        } else if (length(e$c1$ylevels) > 2) {
                            if (svalue(e$perm.choices) == "F-statistic" |
                                svalue(e$perm.choices) == "pseudo F-statistic"){
                                maxstat <- max(c(c(e$c1$stat.dist, recursive = TRUE),
                                                 e$c1$calcAllStats(e$c1$x, e$c1$levels)))
                                buildViewports(e$c1, e$c1$x, e$c1$levels, e$data.boxes,
                                               x.scale = NULL, stat.scale = c(0, maxstat))
                            } else if (svalue(e$perm.choices) == "average deviation"){
                                buildViewports(e$c1, e$c1$x, e$c1$levels, e$data.boxes,
                                               x.scale = NULL,
                                               stat.scale = c(0, diff(range(e$xData))))
                            }
                        } else if (length(e$c1$ylevels) == 2) {
                            if (svalue(e$perm.choices) == "difference"){
                                maxstat <- max(abs(sapply(e$c1$stat.dist, diff)))
                                if (maxstat > (max(e$c1$x) - mean(range(e$c1$x)))) {
                                    e$extra <- maxstat - (max(e$c1$x) - mean(range(e$c1$x)))
                                    x.scale <- c(min(e$c1$x) - e$extra, max(e$c1$x) + e$extra)
                                    buildViewports(e$c1, e$c1$x, e$c1$levels, e$data.boxes,
                                                   x.scale, stat.scale = x.scale - mean(x.scale))
                                } else {
                                    stats <- sapply(e$c1$stat.dist, diff)
                                    meanstat <- mean(stats)
                                    stat.scale <- c(-0.5, 0.5)
                                    x.range <- range(e$c1$x)
                                    if (any(stats < stat.scale[1] | stats > stat.scale[2])){
                                        stat.scale <- range(stats)
                                        scale.diff <- (diff(stat.scale) - 1) / 2
                                        if (scale.diff > 0) {
                                            x.scale <- c(0-scale.diff, 1+scale.diff)
                                        } else {
                                            stat.scale <- range(e$c1$x) + c(-0.5, 0.5)*abs(scale.diff)
                                        }
                                    }
                                    buildViewports(e$c1, e$c1$x, e$c1$levels, e$data.boxes,
                                                   x.scale = NULL, stat.scale = x.range - mean(x.range))
                                }
                            } else {
                                statrange <- range(c(c(e$c1$stat.dist, recursive = TRUE),
                                                     e$c1$calcAllStats(e$c1$x, e$c1$levels)))
                                buildViewports(e$c1, e$c1$x, e$c1$levels, e$data.boxes,
                                               x.scale = NULL, stat.scale = statrange)
                            }
                        }
                    } else {
                        if (length(e$c1$ylevels) > 2) {
                            if (svalue(e$perm.choices) == "chi-square statistic"){
                                maxstat <- max(c(c(e$c1$stat.dist, recursive = TRUE),
                                                 e$c1$calcAllStats(e$c1$x, e$c1$levels)))
                                buildViewports(e$c1, e$c1$x, e$c1$levels, e$data.boxes,
                                               x.scale = NULL, stat.scale = c(0, maxstat))
                            } else if (svalue(e$perm.choices) == "average deviation"){
                                buildViewports(e$c1, e$c1$x, e$c1$levels, e$data.boxes,
                                               x.scale = NULL, stat.scale = NULL)
                            }
                        } else {
                            stats <- sapply(e$c1$stat.dist, diff)
                            meanstat <- mean(stats)
                            stat.scale <- c(-0.5, 0.5)
                            x.scale <- NULL
                            if (any(stats < stat.scale[1] | stats > stat.scale[2])){
                                stat.scale <- range(stats)
                                scale.diff <- (diff(stat.scale) - 1) / 2
                                if (scale.diff > 0) {
                                    x.scale <- c(0-scale.diff, 1+scale.diff)
                                } else {
                                    stat.scale <- stat.scale + c(-0.5, 0.5)*abs(scale.diff)
                                }
                            }
                            buildViewports(e$c1, e$c1$x, e$c1$levels, e$data.boxes,
                                           x.scale = x.scale, stat.scale = stat.scale)
                        }
                    }

                    e$c1$buildImage(!is.null(e$c1$levels) & !is.categorical(e$c1$levels))
                    if (is.categorical(e$xData))
                        e$c1$image <- editGrob(e$c1$image, gPath("sampleAxis"),
                                               at = seq(0, 1, by = 0.2))
                    if (e$c1$stat.in.use == "slope")
                        e$c1$image <- editGrob(e$c1$image, gPath("statAxis"),
                                               at = c(0, 1))
                    e$c1$image <- editGrob(e$c1$image, gPath("dataAxis"), label = FALSE)
                    e$c1$buildBoxes()
                    pushViewport(e$c1$viewports)
                    e$c1$plotData()
                    e$c1$drawImage()
                    e$c1$showLabels()
                    e$c1$plotDataStat(e)
                    e$c1$drawImage()
                    e$c1$getStatYPos()
                    enabled(e$lower) <- TRUE
                }
                enabled(e$obj) <- TRUE
            })
    e$vit.resamp <- glabel("Randomly grouping", container = e$lower)
    e$boot.frame.cont <- ggroup(container = e$lower)
    vit.bootbox <- gframe("Number of repetitions",
                          expand = TRUE,
                          container = e$boot.frame.cont)
    e$redraw.radio <- gradio(c(1, 5, 20), horizontal = FALSE)
    add(vit.bootbox, e$redraw.radio)
    add(e$boot.frame.cont, vit.bootbox)
    buttons1 <- ggroup(container = e$lower)

    ## Handler to go in here.
    e$get.sample <- gbutton("Go", container = buttons1, expand = TRUE,
                            handler = function(h, ...){
                                if (svalue(e$get.sample) == "Go") {
                                    # Resetting the widgets to whatever settings are currently in use
                                    svalue(e$stat) <- e$c1$stat.in.use
                                    svalue(e$perm.choices) <- e$perm.method
                                    if (is.categorical(e$c1$x))
                                        svalue(e$loi.choices) <- e$c1$loi

                                    enabled(e$stop.button) <- TRUE
                                    enabled(e$upper) <- FALSE
                                    enabled(e$vit.resamp) <- FALSE
                                    enabled(e$boot.frame.cont) <- FALSE
                                    enabled(e$stat.label) <- FALSE
                                    enabled(e$diff.box.cont) <- FALSE
                                    enabled(e$get.dist) <- FALSE
                                    svalue(e$get.sample) <- "Pause"
                                    if (e$clear.stat){
                                        e$clearPanel(panel = "stat")
                                        e$clear.stat <- FALSE
                                    }
                                    n <- svalue(e$redraw.radio)
                                    for (i in 1:n){
                                        if (! e$c1$stopAnimation) {
                                          if (n != 20){
                                              if (n == 1){
                                                  e$c1$animateSample(e, n.steps = if (is.categorical(e$c1$x)) 3 else 10, mix = TRUE)
                                              } else {
                                                  e$c1$animateSample(e, n.steps = if (is.categorical(e$c1$x)) 3 else 10, mix = FALSE)
                                              }
                                          }
                                          if (! e$c1$stopAnimation) {
                                              e$c1$plotSample(e)
                                              e$c1$showLabels()
                                              if (n != 5) e$c1$drawImage() else e$c1$pauseImage(10)
                                              e$c1$advanceWhichSample()
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
                                    enabled(e$vit.resamp) <- TRUE
                                    enabled(e$boot.frame.cont) <- TRUE
                                    enabled(e$stat.label) <- TRUE
                                    enabled(e$diff.box.cont) <- TRUE
                                    enabled(e$get.dist) <- TRUE
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

    addSpace(e$lower, 20, horizontal = FALSE)

    e$stat.label <- glabel("Include statistic distribution", container = e$lower)
    e$diff.box.cont <- ggroup(container = e$lower)
    vit.diffbox <- gframe("Number of repetitions",
                          expand = TRUE,
                          container = e$diff.box.cont)
    e$bootstrap.radio <- gradio(c(1, 5, 20, 1000),
                                horizontal = FALSE)
    add(vit.diffbox,e$bootstrap.radio)
    add(e$diff.box.cont, vit.diffbox)
    e$clear.stat <- FALSE
    buttons2 <- ggroup(horizontal = FALSE, container = e$lower)
    e$get.dist <- gbutton(text = "Go", expand = TRUE, container = buttons2,
                          handler = function(h, ...){
                              if (svalue(e$get.dist) == "Go") {
                                  e$c1$animationPaused <- FALSE
                                  # Resetting the widgets to whatever settings are currently in use
                                  svalue(e$stat) <- e$c1$stat.in.use
                                  svalue(e$perm.choices) <- e$perm.method
                                  if (is.categorical(e$c1$x))
                                      svalue(e$loi.choices) <- e$c1$loi

                                  enabled(e$stop.button) <- TRUE
                                  enabled(e$upper) <- FALSE
                                  enabled(e$vit.resamp) <- FALSE
                                  enabled(e$boot.frame.cont) <- FALSE
                                  enabled(e$stat.label) <- FALSE
                                  enabled(e$diff.box.cont) <- FALSE
                                  enabled(e$get.sample) <- FALSE
                                  svalue(e$get.dist) <- "Pause"
                                  n <- svalue(e$bootstrap.radio)
                                  if (n == 1000){
                                      e$clearPanel("stat")
                                      e$clearPanel("sample")
                                      e$c1$handle1000(e)
                                      if (! e$c1$stopAnimation) {
                                          e$clear.stat <- TRUE
                                      }
                                  } else {
                                      if (e$clear.stat){
                                         e$clearPanel("stat")
                                         e$clearPanel("sample")
                                         e$clear.stat <- FALSE
                                     }
                                      for (i in 1:n){
                                          if (! e$c1$stopAnimation) {
                                              if (n == 1)
                                                  e$c1$animateSample(e, n.steps = if (is.categorical(e$c1$x)) 3 else 10, mix = FALSE)
                                              if (! e$c1$stopAnimation) {
                                                  e$c1$plotSample(e)
                                                  e$c1$showLabels()
                                                  if (n != 20) e$c1$animateStat(e, 10)
                                                  if (! e$c1$stopAnimation) {
                                                      e$c1$plotStatDist(e)
                                                      e$c1$advanceWhichSample()
                                                      e$c1$drawImage()
                                                  }
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
                                  enabled(e$vit.resamp) <- TRUE
                                  enabled(e$boot.frame.cont) <- TRUE
                                  enabled(e$stat.label) <- TRUE
                                  enabled(e$diff.box.cont) <- TRUE
                                  enabled(e$get.sample) <- TRUE
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
}
