CIGUIHandler <- function(e){
    e$method <- "ci"

    # Rewriting the menus to link only to other modules
    svalue(e$g.menu) <- getModuleMenu(e)

    e$data.boxes <- FALSE
    e$replace <- FALSE
    e$stat.scale <- TRUE

    ci.methods <- c("bootstrap: percentile",
                    "bootstrap: +/- 2 s.e.",
                    "bootstrap: +/- t s.e.",
                    "normal: +/- 1/sqrt(n)",
                    "normal: +/- 2 s.e.",
                    "normal: +/- t s.e.")
    if (! is.categorical(e$c1$x))
        ci.methods <- ci.methods[-4]

    tbl <- glayout(container = e$upper)
    tbl[1,1] <- glabel("Quantity: ", container = tbl)
    tbl[1,2] <- (e$stat <- gcombobox(c("mean", "median"),
                                     editable = TRUE, container = tbl,
                                     handler = function(h, ...) {
                                             if (svalue(e$stat) == "median") {
                                                 e$cimeth[] <- c("bootstrap: percentile",
                                                                 "bootstrap: +/- 2 s.e.",
                                                                 "bootstrap: +/- t s.e.")
                                                 svalue(e$cimeth) <- "bootstrap: percentile"
                                             } else if (svalue(e$stat) == "mean") {
                                                 e$cimeth[] <- ci.methods
                                                 svalue(e$cimeth) <- "normal: +/- t s.e."
                                             }
                                         }
                                     ))
    tbl[2, 1] <- (e$loi.label <- glabel("Level of interest: ", container = tbl))
    tbl[2, 2] <- (e$loi.choices <- gcombobox("", editable = TRUE, container = tbl,
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
    if (is.categorical(e$c1$x) & ! is.null(e$c1$levels))
        e$loi.choices[] <- sort(unique(e$c1$x))
    tbl[3,1] <- (e$cilabel <- glabel("CI Method: ", container = tbl))
    tbl[3,2] <- (e$cimeth <- gcombobox(ci.methods, editable = TRUE,
                                       container = tbl))

    tbl[4,1] <- (e$sizelabel <- glabel("Sample Size:", container = tbl))
    tbl[4,2] <- (e$ssize <- gedit("10", container = tbl))
    tbl[5,2] <- advancedOptions <- gexpandgroup("Advanced options", horizontal = FALSE,
                                                container = tbl)
    e$holdSample <- gcheckbox("Hold samples")
    add(advancedOptions, e$holdSample)

    gbutton("Record my choices", container = e$upper, expand = TRUE,
            handler = function(h,...) {
                enabled(e$obj) <- FALSE
                if (svalue(e$ssize) != e$c1$n) svalue(e$holdSample) <- FALSE
                keep.samples <- (!(class(e$c1$samples) == "uninitializedField") &
                                 svalue(e$holdSample))
                if (keep.samples)
                    e$resetCanvasKeepSample(e$c1) else e$resetCanvas()
                e$c1$n <- min(c(length(e$c1$x), as.numeric(svalue(e$ssize))))
                e$c1$data.file <- tag(e$obj, "data.file") # Grabbing data filename for labels
                loadStatDetails(e)
                if (e$loaded){
                    valid.sample <- e$sample_check()
                    if (valid.sample) {
                        if (!keep.samples)
                            e$c1$makeSamples(e$replace)
                        slowMethod <- svalue(e$stat) == "median" |
                                (svalue(e$stat) == "proportion" &
                                 strsplit(svalue(e$cimeth), ":")[[1]][1] == "bootstrap")
                        if (slowMethod)
                            e$c1$progressType <- "tk" # This is a slow method, show graphical progress
                        e$c1$makeStatistics()
                        if (slowMethod)
                            e$c1$progressType <- "text" # Set back to text
                        e$c1$plotDataStat(e) #use this to rerun PLOT_DATA for your method if necessary
                        e$c1$showLabels()
                        e$c1$drawImage()
                        enabled(e$lower) <- TRUE
                    } else {
                        enabled(e$upper)
                    }
                }
                enabled(e$obj) <- TRUE
            })

    svalue(e$cimeth) <- "normal: +/- t s.e."
    addSpace(e$upper, 10, horizontal = FALSE)
    e$vit.popsamp <- glabel("Population and sample", container = e$lower)
    e$boot.frame.cont <- ggroup(container = e$lower)
    vit.bootbox <- gframe("Number of repetitions",
                          expand = TRUE,
                          container = e$boot.frame.cont)
    e$redraw.radio <- gradio(c(1, 5, 20), horizontal=FALSE)
    add(vit.bootbox, e$redraw.radio)
    add(e$boot.frame.cont, vit.bootbox)
    e$advance <- FALSE

    buttons1 <- ggroup(container = e$lower)
    e$get.sample <- gbutton(text = "Go", expand = TRUE,
                            container = buttons1, handler = function(h, ...) {
                                if (svalue(e$get.sample) == "Go") {
                                    e$c1$animationPaused <- FALSE
                                    # Resetting the widgets to whatever settings are currently in use
                                    svalue(e$stat) <- e$c1$stat.in.use
                                    if (is.categorical(e$c1$x))
                                        svalue(e$loi.choices) <- e$c1$loi
                                    svalue(e$cimeth) <- e$cimethod
                                    svalue(e$ssize) <- e$c1$n

                                    e$c1$animationPaused <- FALSE
                                    enabled(e$stop.button) <- TRUE
                                    enabled(e$upper) <- FALSE
                                    enabled(e$vit.popsamp) <- FALSE
                                    enabled(e$boot.frame.cont) <- FALSE
                                    enabled(e$stat.label) <- FALSE
                                    enabled(e$diff.box.cont) <- FALSE
                                    enabled(e$get.dist) <- FALSE
                                    svalue(e$get.sample) <- "Pause"
                                    n <- svalue(e$redraw.radio)
                                    pause = FALSE
                                    for (i in 1:n) {
                                        if (! e$c1$stopAnimation) {
                                            if (e$advance) e$c1$advanceWhichSample()
                                            if (n == 1) {
                                                e$c1$animateSample(15, 5, TRUE, TRUE)
                                                pause <- TRUE
                                            }
                                            if (n == 5) {
                                                e$c1$animateSample(15, 0, TRUE, TRUE)
                                                pause <- TRUE
                                            }
                                            if (n == 20)
                                                e$c1$animateSample(15, 0, TRUE, FALSE)
                                            if (! e$c1$stopAnimation) {
                                                e$c1$plotSample(e)
                                                e$c1$plotSampleStat(e, pause = pause)
                                                if (n == 5 & ! e$c1$stopAnimation) e$c1$pauseImage(15)
                                                else e$c1$drawImage()
                                                e$advance <- TRUE
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
                                    enabled(e$vit.popsamp) <- TRUE
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

    addSpace(e$lower, 20, horizontal=FALSE)

    e$stat.label <- glabel("Include confidence interval history", container = e$lower)
    e$diff.box.cont <- ggroup(container = e$lower)
    vit.diffbox <- gframe("Number of repetitions",
                          expand = TRUE,
                          container = e$diff.box.cont)
    e$bootstrap.radio <- gradio(c(1, 5, 20, 1000),
                                horizontal = FALSE)
    add(vit.diffbox,e$bootstrap.radio)
    add(e$diff.box.cont, vit.diffbox)


    buttons2 <- ggroup(horizontal = FALSE, container = e$lower)
    e$get.dist <- gbutton(text = "Go", expand = TRUE,
                          container = buttons2, handler = function(h, ...) {
                              if (svalue(e$get.dist) == "Go") {
                                  e$c1$animationPaused <- FALSE
                                  # Resetting the widgets to whatever settings are currently in use
                                  svalue(e$stat) <- e$c1$stat.in.use
                                  if (is.categorical(e$c1$x))
                                      svalue(e$loi.choices) <- e$c1$loi
                                  svalue(e$cimeth) <- e$cimethod
                                  svalue(e$ssize) <- e$c1$n

                                  e$c1$animationPaused <- FALSE
                                  enabled(e$stop.button) <- TRUE
                                  enabled(e$upper) <- FALSE
                                  enabled(e$vit.popsamp) <- FALSE
                                  enabled(e$boot.frame.cont) <- FALSE
                                  enabled(e$stat.label) <- FALSE
                                  enabled(e$diff.box.cont) <- FALSE
                                  enabled(e$get.sample) <- FALSE
                                  svalue(e$get.dist) <- "Pause"
                                  if (svalue(e$bootstrap.radio) == 1000) e$c1$handle1000(e)
                                  else {
                                      n <- svalue(e$bootstrap.radio)
                                      for (i in 1:n) {
                                          if (! e$c1$stopAnimation) {
                                              if (n == 1)
                                                  e$c1$animateSample(15, 0, FALSE, TRUE)
                                              else
                                                  e$c1$animateSample(15, 0, FALSE, FALSE)
                                              if (! e$c1$stopAnimation) {
                                                  e$c1$plotSample(e)
                                                  e$c1$plotSampleStat(e, pause = FALSE)
                                                  if (n != 20) e$c1$pauseImage(15)
                                                  if (! e$c1$stopAnimation) {
                                                      if (svalue(e$bootstrap.radio) != 20) e$c1$animateStat(e, 10)
                                                      if (! e$c1$stopAnimation) {
                                                          e$c1$plotStatDist(e)
                                                          e$c1$displayResult(e, n == 1 | n == 5)
                                                          e$c1$showLabels()
                                                          e$c1$drawImage()
                                                          e$c1$advanceWhichSample()
                                                      }
                                                  }
                                              }
                                          }
                                      }
                                      e$advance <- FALSE
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
                                  enabled(e$vit.popsamp) <- TRUE
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
