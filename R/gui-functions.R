## Sets up data and analysis tabs.
setupGUI <- function(e){
    e$win <- gwindow(paste("Visual Inference Tools", e$title, sep = " - "), visible = TRUE,
                     height = 700, width = 375, handler = function(...) {
                         # Do stuff here if we try closing.
                     })

    addHandlerUnrealize(e$win, handler = function(h, ...) {
        # We want to just quit R any time the home screen is closed.
        if (e$disposeR) {
        e$confirmDialog("Do you wish to quit VIT?",
                        handler = function(...) {
                            q(save = "no")
                        })
        } else {
            dispose(e$win)
            tmp <- dev.off()
        }
        return(TRUE)
    })

    e$loaded <- FALSE
    e$tab.loaded <- FALSE

    # Instantiating menu elements
    mSave <- gaction("Save Graphics", icon = "save", handler = function(...) {
                        e$specifyFileForSaving()
                     })
    sep <- list(separator = TRUE)
    mHome <- gaction("Home", icon = "home", handler = function(...) {
                     e$confirmDialog("Do you wish to return to the home menu?",
                                     handler = function(...) {
                         dispose(e$win)
                         dispose(e$window)
                         try(dev.off())
                         e$homefun()
                     })})

    mReload <- gaction("Reload Module", icon = "refresh", handler = function(...) {
                       e$confirmDialog("Do you wish to reload the current module?",
                                       handler = function(...) {
                           dispose(e$win)
                           dispose(e$window)
                           dev.off()
                           if (e$method == "ci") {
                               newdevice(height = 7.5, width = 7.5)
                               e$title <- "Confidence interval coverage"
                           }
                           if (e$method == "sampvar") {
                               newdevice(height = 7.5, width = 7.5)
                               e$title <- "Sample variation"
                           }
                           if (e$method == "bootstrap") {
                               newdevice(height = 7.5, width = 11.25)
                               e$title <- "Bootstrap confidence intervals"
                           }
                           if (e$method == "permutation") {
                               newdevice(height = 7.5, width = 11.25)
                               e$title <- "Randomisation tests"
                           }
                           if (e$method == "permvar") {
                               newdevice(height = 7.5, width = 11.25)
                               e$title <- "Randomisation variation"
                           }
                           setupGUI(e)
                           if (e$method == "ci")
                               CIGUIHandler(e)
                           if (e$method == "sampvar")
                               sampvarGUIHandler(e)
                           if (e$method == "bootstrap")
                               bootstrapGUIHandler(e)
                           if (e$method == "permutation")
                               permGUIHandler(e)
                           if (e$method == "permvar")
                               permvarGUIHandler(e)
                           dataGUI(e)
                       })})

    mQuit <- gaction("Quit", icon = "quit", handler = function(...) {
        if (e$disposeR) {
            e$confirmDialog("Do you wish to quit VIT?", handler = function(...) {
                q(save = "no")})
        } else {
            dispose(e$win)
            tmp <- dev.off()
        }
        })

    # Compiling elements into a menu
    e$g.menu <- gmenu(list(File = list(Save = mSave,
                                       sep = sep,
                                       Home = mHome, Reload = mReload, "VIT Modules" = list(),
                                       qsep = sep,
                                       Quit = mQuit)),
                      container = e$win)

    win.layout <- glayout(container = e$win, spacing = 0)

    playback.buttons <- ggroup(horizontal = TRUE, expand = FALSE)
    e$stop.button <- gbutton("Stop", icon = "stop", handler = function(...) {
                                  e$c1$animationPaused <- FALSE
                                  e$c1$stopAnimation <- TRUE
                                  enabled(e$stop.button) <- FALSE

                                  if (e$method == "bootstrap") {
                                      enabled(e$upper) <- TRUE
                                      enabled(e$vit.resamp) <- TRUE
                                      enabled(e$boot.frame.cont) <- TRUE
                                      svalue(e$animate.points) <- if (svalue(e$redraw.radio) == 1)
                                                                      svalue(e$animate.points)
                                                                  else FALSE
                                      enabled(e$stat.label) <- TRUE
                                      enabled(e$diff.box.cont) <- TRUE
                                      enabled(e$get.sample) <- TRUE
                                      enabled(e$get.dist) <- TRUE
                                      enabled(e$fade.plots) <- FALSE
                                      enabled(e$show.summary) <- FALSE
                                      enabled(e$show.ci) <- FALSE
                                      svalue(e$get.sample) <- "Go"
                                      svalue(e$get.dist) <- "Go"
                                      svalue(e$show.ci) <- "Show CI"
                                  }

                                  if (e$method == "ci") {
                                      enabled(e$upper) <- TRUE
                                      enabled(e$vit.popsamp) <- TRUE
                                      enabled(e$boot.frame.cont) <- TRUE
                                      enabled(e$stat.label) <- TRUE
                                      enabled(e$diff.box.cont) <- TRUE
                                      enabled(e$get.sample) <- TRUE
                                      enabled(e$get.dist) <- TRUE
                                      svalue(e$get.sample) <- "Go"
                                      svalue(e$get.dist) <- "Go"
                                  }

                                  if (e$method == "permutation") {
                                      enabled(e$upper) <- TRUE
                                      enabled(e$vit.resamp) <- TRUE
                                      enabled(e$boot.frame.cont) <- TRUE
                                      enabled(e$stat.label) <- TRUE
                                      enabled(e$diff.box.cont) <- TRUE
                                      enabled(e$get.sample) <- TRUE
                                      enabled(e$get.dist) <- TRUE
                                      enabled(e$show.tail) <- FALSE
                                      svalue(e$get.sample) <- "Go"
                                      svalue(e$get.dist) <- "Go"
                                      svalue(e$show.tail) <- "Show tail proportion"
                                  }

                                  if (e$method == "permvar") {
                                      enabled(e$upper) <- TRUE
                                      enabled(e$vit.resamp) <- TRUE
                                      enabled(e$boot.frame.cont) <- TRUE
                                      enabled(e$stat.label) <- TRUE
                                      enabled(e$diff.box.cont) <- TRUE
                                      enabled(e$get.sample) <- TRUE
                                      enabled(e$get.dist) <- TRUE
                                      svalue(e$get.sample) <- "Go"
                                      svalue(e$get.dist) <- "Go"
                                  }

                                  if (e$method == "sampvar") {
                                      enabled(e$upper) <- TRUE
                                      enabled(e$vit.samp) <- TRUE
                                      enabled(e$samp.frame.cont) <- TRUE
                                      enabled(e$stat.label) <- TRUE
                                      enabled(e$diff.box.cont) <- TRUE
                                      enabled(e$get.sample) <- TRUE
                                      enabled(e$get.dist) <- TRUE
                                      svalue(e$get.sample) <- "Go"
                                      svalue(e$get.dist) <- "Go"
                                      if (svalue(e$stat) == "default") {
                                          enabled(e$stat.label) <- FALSE
                                          enabled(e$diff.box.cont) <- FALSE
                                          enabled(e$get.dist) <- FALSE
                                      }
                                  }
                             }, expand = TRUE, container = playback.buttons)
    font(e$stop.button) <- list(weight = "bold", family = "normal",
                                color = "navy")
    win.layout[1, 3] <- playback.buttons

    # Adding a label relating to the method that we're using currently
    title.label <- glabel(e$title)
    font(title.label) <- list(weight = "bold", family = "normal", color = "black")
    win.layout[1, 1:2] <- title.label

    ## separates space into controls and plot
    g <- ggroup(expand = TRUE, use.scrollwindow = TRUE)
    win.layout[2, 1:3, expand = TRUE] <- g
    e$obj <- g

    g.controls <- gnotebook(container = g, expand = TRUE)
    ## control panel has a read data tab inspired from iNZight
    ## and a VIT tab
    e$controls.iNZight <- ggroup(horizontal = FALSE, container = g.controls,
                               expand = TRUE, label = "Data")
    e$controls.vit <- ggroup(horizontal = FALSE, container = g.controls,
                             expand = TRUE, label = "Analyse")
    svalue(g.controls) <- 1
    e$upper <- ggroup(horizontal = FALSE, container = e$controls.vit, expand = TRUE)
    e$lower <- ggroup(horizontal = FALSE, container = e$controls.vit, expand = TRUE)

    # Creating a handler so that we can do work as we switch tabs.
    # Useful for determining things as we switch from 'Data' to 'Analyse'
    # where computations are dependent on the form of the data in the 'Data'
    # tab.
    addhandlerchanged(g.controls, handler = function(h, ...) {
        currentTab <- names(h$obj)[h$pageno]
        # Don't bother doing anything on a switch to the 'Data' tab
        if (currentTab != "Analyse")
            return()

        # In the case where we have already set reasonable starting
        # values for gui elements, don't bother setting again, return.
        # We can assume this because when we switch tabs the first time,
        # we set reasonable values, and any handlers we have on the gui
        # elements never put us in an inconsistent state.
        if (e$tab.loaded & e$loaded)
            return()

        # When performing the CI method we need to ensure certain methods
        # are available and sample sizes are valid
        if (e$method %in% c("ci", "sampvar")) {
            # Ensure that a sample size is no larger than our current sample
            svalue(e$ssize) <- min(as.numeric(svalue(e$ssize)), length(e$c1$x))
            if (e$method == "ci") {
                ci.methods <- c("bootstrap: percentile",
                                "bootstrap: +/- 2 s.e.",
                                "bootstrap: +/- t s.e.",
                                "normal: +/- 1/sqrt(n)",
                                "normal: +/- 2 s.e.",
                                "normal: +/- t s.e.")
                if (! is.null(e$c1$x) && ! is.categorical(e$c1$x)) {
                    ci.methods <- ci.methods[-4]
                }
                e$cimeth[] <- ci.methods
                svalue(e$cimeth) <- "normal: +/- t s.e."
            }
        }

        if (! is.null(e$c1$x)) {
            # Single categorical variable
            if (is.categorical(e$c1$x) & is.null(e$c1$levels)) {
                svalue(e$stat) <- "proportion"
                enabled(e$stat) <- FALSE
                if (e$method == "permutation") {
                    enabled(e$perm.label) <- FALSE
                    enabled(e$perm.choices) <- FALSE
                    svalue(e$perm.choices) <- ""
                    enabled(e$paired.samples) <- FALSE
                    svalue(e$paired.samples) <- FALSE
                } else if (e$method == "bootstrap") {
                    enabled(e$resamp.within) <- FALSE
                    svalue(e$resamp.within) <- FALSE
                    enabled(e$paired.samples) <- FALSE
                    svalue(e$paired.samples) <- FALSE
                } else if (e$method == "sampvar") {
                    enabled(e$sampvar.label) <- FALSE
                    enabled(e$sampvar.choices) <- FALSE
                    svalue(e$sampvar.choices) <- ""
                } else if (e$method == "permvar") {
                    # Depending on whether we are doing 2 sample or k-sample,
                    # modify the drop-down box
                    enabled(e$perm.label) <- TRUE
                    enabled(e$perm.choices) <- FALSE
                    svalue(e$perm.choices) <- "difference"
                    svalue(e$ngroups.choices) <- 2
                }
                enabled(e$loi.choices) <- TRUE
                enabled(e$loi.label) <- TRUE
                sx <- sort(unique(e$c1$x))
                e$loi.choices[] <- sx
                e$c1$loi <- sx[1]
                svalue(e$loi.choices) <- e$c1$loi
                if (length(sx) > 2) {
                    e$c1$loi.alt <- "All Else"
                    e$c1$loi.data <- e$c1$x
                    e$c1$loi.data[e$c1$x != e$c1$loi] <- e$c1$loi.alt
                } else {
                    e$c1$loi.alt <- sx[2]
                    e$c1$loi.data <- e$c1$x
                }
            } else {
                # Single continuous variable
                svalue(e$stat) <- "mean"
                enabled(e$stat) <- TRUE
                if (e$method == "permutation") {
                    enabled(e$perm.label) <- FALSE
                    enabled(e$perm.choices) <- FALSE
                    svalue(e$perm.choices) <- ""
                    enabled(e$paired.samples) <- FALSE
                    svalue(e$paired.samples) <- FALSE
                } else if (e$method == "bootstrap") {
                    enabled(e$resamp.within) <- FALSE
                    svalue(e$resamp.within) <- FALSE
                    enabled(e$paired.samples) <- FALSE
                    svalue(e$paired.samples) <- FALSE
                    boot.statmethod <- c("mean", "median", "lower quartile", "upper quartile")
                    e$stat[] <- boot.statmethod
                    svalue(e$stat) <- boot.statmethod[1]
                } else if (e$method == "sampvar") {
                    sampvar.statmethod <- c("default", "mean", "median", "lower quartile", "upper quartile")
                    e$stat[] <- sampvar.statmethod
                    svalue(e$stat) <- sampvar.statmethod[1]
                    enabled(e$sampvar.label) <- FALSE
                    enabled(e$sampvar.choices) <- FALSE
                    svalue(e$sampvar.choices) <- ""
                    enabled(e$stat.label) <- FALSE
                    enabled(e$diff.box.cont) <- FALSE
                    enabled(e$get.dist) <- FALSE
                } else if (e$method == "permvar") {
                    enabled(e$perm.label) <- TRUE
                    enabled(e$perm.choices) <- TRUE
                    e$perm.choices[] <- c("difference", "t-pooled", "t-Welch")
                    svalue(e$perm.choices) <- "difference"
                    svalue(e$ngroups.choices) <- 2
                }
                enabled(e$loi.choices) <- FALSE
                enabled(e$loi.label) <- FALSE
                e$loi.choices[] <- ""
                svalue(e$loi.choices) <- ""
            }
        }

        if (! is.null(e$c1$x) & ! is.null(e$c1$levels)) {
            ys <- unique(e$c1$levels)
            # When we have two categorical variables
            if (is.categorical(e$c1$x) & is.categorical(e$c1$levels)) {
                svalue(e$stat) <- "proportion"
                enabled(e$stat) <- FALSE
                enabled(e$loi.choices) <- TRUE
                enabled(e$loi.label) <- TRUE
                sx <- sort(unique(e$c1$x))
                e$loi.choices[] <- sx
                e$c1$loi <- sx[1]
                svalue(e$loi.choices) <- e$c1$loi
                if (length(sx) > 2) {
                    e$c1$loi.alt <- "All Else"
                    e$c1$loi.data <- e$c1$x
                    e$c1$loi.data[e$c1$x != e$c1$loi] <- e$c1$loi.alt
                } else {
                    e$c1$loi.alt <- sx[2]
                    e$c1$loi.data <- e$c1$x
                }
                if (e$method == "permutation") {
                    enabled(e$paired.samples) <- FALSE
                    svalue(e$paired.samples) <- FALSE
                    # Depending on whether we are doing 2 sample or k-sample,
                    # modify the drop-down box
                    if (length(ys) == 2) {
                        enabled(e$perm.label) <- TRUE
                        enabled(e$perm.choices) <- FALSE
                        svalue(e$perm.choices) <- "difference"
                    } else if (length(ys) > 2) {
                        enabled(e$perm.label) <- TRUE
                        enabled(e$perm.choices) <- TRUE
                        perm.statmethod <- c("average deviation", "chi-square statistic")
                        e$perm.choices[] <- perm.statmethod
                        svalue(e$perm.choices) <- perm.statmethod[1]
                    }
                } else if (e$method == "sampvar") {
                    # Depending on whether we are doing 2 sample or k-sample,
                    # modify the drop-down box
                    if (length(ys) == 2) {
                        enabled(e$sampvar.label) <- FALSE
                        enabled(e$sampvar.choices) <- FALSE
                        svalue(e$sampvar.choices) <- "difference"
                    }
                } else if (e$method == "bootstrap") {
                    enabled(e$resamp.within) <- TRUE
                    svalue(e$resamp.within) <- FALSE
                    enabled(e$paired.samples) <- FALSE
                    svalue(e$paired.samples) <- FALSE
                }
            } else if (!is.categorical(e$c1$x) & is.categorical(e$c1$levels)){
                # We have a continuous variable to analyse the mean with
                svalue(e$stat) <- "mean"
                enabled(e$stat) <- TRUE
                enabled(e$loi.choices) <- FALSE
                enabled(e$loi.label) <- FALSE
                e$loi.choices[] <- ""
                svalue(e$loi.choices) <- ""

                if (e$method == "permutation") {
                    enabled(e$paired.samples) <- FALSE
                    svalue(e$paired.samples) <- FALSE
                    # Depending on whether we are doing 2 sample or k-sample,
                    # modify the drop-down box
                    if (length(ys) == 2) {
                        enabled(e$perm.label) <- TRUE
                        enabled(e$perm.choices) <- TRUE
                        e$perm.choices[] <- c("difference", "t-pooled", "t-Welch")
                        svalue(e$perm.choices) <- "difference"
                    } else if (length(ys) > 2) {
                        enabled(e$perm.label) <- TRUE
                        enabled(e$perm.choices) <- TRUE
                        perm.statmethod <- c("average deviation", "F-statistic")
                        e$perm.choices[] <- perm.statmethod
                        svalue(e$perm.choices) <- perm.statmethod[1]
                    }
                } else if (e$method == "sampvar") {
                    # Depending on whether we are doing 2 sample or k-sample,
                    # modify the drop-down box
                    if (length(ys) == 2) {
                        e$stat[] <- c("default", "mean", "median", "lower quartile", "upper quartile", "IQR ratio")
                        svalue(e$stat) <- "default"
                        enabled(e$sampvar.label) <- FALSE
                        enabled(e$sampvar.choices) <- FALSE
                        e$sampvar.choices[] <- c("difference", "t-pooled", "t-Welch")
                        svalue(e$sampvar.choices) <- ""
                    }
                } else if (e$method == "bootstrap") {
                    enabled(e$resamp.within) <- TRUE
                    svalue(e$resamp.within) <- FALSE
                    enabled(e$paired.samples) <- FALSE
                    svalue(e$paired.samples) <- FALSE
                    boot.statmethod <- c("mean", "median", "lower quartile", "upper quartile", "IQR ratio")
                    e$stat[] <- boot.statmethod
                    svalue(e$stat) <- boot.statmethod[1]
                }
            } else if (!is.categorical(e$c1$x) & !is.categorical(e$c1$levels))
                # We have two continuous variables.
                if (e$method == "permutation") {
                    e$perm.choices[] <- ""
                    svalue(e$perm.choices) <- ""
                    e$stat[] <- c("slope")
                    svalue(e$stat) <- "slope"
                    enabled(e$paired.samples) <- TRUE
                    svalue(e$paired.samples) <- FALSE
                    e$pairedSamples <- FALSE
                } else if (e$method == "sampvar") {
                    e$sampvar.choices[] <- ""
                    svalue(e$sampvar.choices) <- ""
                    e$stat[] <- c("slope")
                    svalue(e$stat) <- "slope"
                } else if (e$method == "bootstrap"){
                    e$stat[] <- "slope"
                    svalue(e$stat) <- "slope"
                    enabled(e$paired.samples) <- TRUE
                    svalue(e$paired.samples) <- FALSE
                    e$pairedSamples <- FALSE
                }
        }
    })

    ## Disable GUI until data loaded.
    enabled(e$stop.button) <- FALSE
    enabled(e$upper) <- FALSE
    enabled(e$lower) <- FALSE
    addSpace(e$upper, 10, horizontal = FALSE)

    # Acknowledging that we've set values appropriately
    e$tab.loaded <- TRUE
}

## Fills in data tab with iNZight controls.
dataGUI <- function(e) {
    ## adding iNZight controls
    ## top three buttons
    iNZ.view <- ggroup()
    e$importData <- gbutton("Import Data",
                            handler = function(h,...) e$fileReader())
    e$dataView <- gbutton("View Data Set",
                          handler = function(h,...) e$viewData(h,...))
    e$listView <- gbutton("View Variables",
                          handler = function(h,...) e$viewList(h,...))
    font(e$importData) <- list(weight = "bold", family = "normal",
                               color = "navy")
    font(e$dataView) <- list(weight = "bold", family = "normal",
                             color = "navy")
    font(e$listView) <- list(weight = "bold", family = "normal",
                             color = "navy")
    add(iNZ.view, e$importData)
    add(iNZ.view, e$dataView)
    add(iNZ.view, e$listView)
    add(e$controls.iNZight, iNZ.view)

    ## Table of data frame or variables
    e$dataGp <- ggroup(horizontal = TRUE, expand = TRUE)
    add(e$controls.iNZight, e$dataGp, expand = TRUE)

    ## At start-up an (near)empty dataframe is embedded
    tag(e$obj,"dataSet") = data.frame(empty = " ")
    tag(e$obj,"originalDataSet") = data.frame(empty = " ")
    e$dataSt <- gdf(tag(e$obj,"dataSet"), expand = TRUE)
    add(e$dataGp, e$dataSt, expand = TRUE)
    e$dataList1 <- NULL
    e$dataList2 <- NULL

    ## Variable 1 and 2 display
    ## column 1
    tbl <- glayout(expand = FALSE)
    e$label.var1 <- glabel(" Variable 1 :")
    tbl[3,1, anchor = c(0,0)] <- e$label.var1
    e$label.var2 <- glabel(" Variable 2 :")
    tbl[5,1, anchor = c(0,0)] <- e$label.var2

    ## column 2
    e$xVar <- gcombobox(names(e$dataSt), handler = function(h, ...) {
                            if (is.null(svalue(e$xVar, index = TRUE)))
                                return()
                            # In the case where we have the "null" value selected
                            # basically perform the same action as clearing the variable.
                            # Code is basically ripped from the gbutton handlers.
                            if (svalue(e$xVar, index = TRUE) == 1) {
                                e$xData <- NULL
                                e$variable_check()
                                ## Only try to plot something if data is loaded.
                                if (e$data.loaded) {
                                    e$buildCanvas()
                                    e$c1$drawImage()
                                } else enabled(e$upper) <- FALSE
                                enabled(e$lower) <- FALSE
                                e$loaded <- FALSE
                            } else {
                                # We have the case where we're actually trying to load
                                # a new variable, most of this code is taken from the drop
                                # handlers which may not work in future with gdfs.
                                enabled(e$obj) <- FALSE
                                e$xName <- svalue(e$xVar)
                                e$xData <- tag(e$obj, "dataSet")[, e$xName]
                                if (is.integer(e$xData)) e$xData <- as.numeric(e$xData)
                                if (is.factor(e$xData)) e$xData <- as.character(e$xData)
                                e$na_check(TRUE)
                                e$variable_check()
                                xlevels <- sort(unique(e$xData))
                                e$loi.choices[] <- xlevels
                                svalue(e$loi.choices) <- xlevels[1]
                                e$buildCanvas()
                                if (e$data.boxes) e$c1$buildBoxes()
                                e$c1$drawImage()
                                enabled(e$upper) <- TRUE
                                enabled(e$lower) <- FALSE
                                e$loaded <- FALSE
                                e$tab.loaded <- FALSE
                                enabled(e$obj) <- TRUE
                            }
                        })
    font(e$xVar) <- list(weight = "bold", family = "normal")
    tag(e$obj,"e$xVarData") <- NULL
    tbl[3,3, anchor = c(0,0)] <- e$xVar

    e$yVar <- gcombobox(names(e$dataSt), handler = function(h, ...) {
                  if (is.null(svalue(e$yVar, index = TRUE)))
                      return()
                  # In the case where we have the "null" value selected
                  # basically perform the same action as clearing the variable.
                  # Code is basically ripped from the gbutton handlers.
                  if (svalue(e$yVar, index = TRUE) == 1) {
                      e$yData <- NULL
                      e$variable_check()
                      ## Only try to plot something if data is loaded.
                      if (e$data.loaded) {
                          e$buildCanvas()
                          e$c1$drawImage()
                      } else enabled(e$upper) <- FALSE
                      enabled(e$lower) <- FALSE
                      e$loaded <- FALSE
                  } else {
                      # We have the case where we're actually trying to load
                    # a new variable, most of this code is taken from the drop
                    # handlers which may not work in future with gdfs.
                    enabled(e$obj) <- FALSE
                    e$yName <- svalue(e$yVar)
                    e$yData <- tag(e$obj, "dataSet")[, e$yName]
                    if (is.integer(e$yData)) e$yData <- as.numeric(e$yData)
                    if (is.factor(e$yData)) e$yData <- as.character(e$yData)
                    e$na_check(FALSE)
                    e$variable_check()
                    xlevels <- sort(unique(e$xData))
                    e$loi.choices[] <- xlevels
                    svalue(e$loi.choices) <- xlevels[1]
                    e$buildCanvas()
                    if (e$data.boxes) e$c1$buildBoxes()
                    e$c1$drawImage()
                    enabled(e$upper) <- TRUE
                    enabled(e$lower) <- FALSE
                    e$loaded <- FALSE
                    e$tab.loaded <- FALSE
                    enabled(e$obj) <- TRUE
                }
            })
    font(e$yVar) <- list(weight = "bold", family = "normal")
    tag(e$obj,"e$yVarData") <- NULL
    tbl[5,3, anchor = c(0,0)] <- e$yVar

    ## column 3
    e$clear.xVar <- gbutton("clear", handler = function(h,...) {
        e$xData <- NULL
        svalue(e$xVar, index = TRUE) <- 1
        e$variable_check()
        ## Only try to plot something if data is loaded.
        if (e$data.loaded) {
            e$buildCanvas()
            e$c1$drawImage()
        } else enabled(e$upper) <- FALSE
        enabled(e$lower) <- FALSE
        e$loaded <- FALSE
    })
    e$clear.yVar <- gbutton("clear", handler = function(h,...) {
        e$yData <- NULL
        svalue(e$yVar, index = TRUE) <- 1
        e$variable_check()
        ## Only try to plot something if data is loaded.
        if (e$data.loaded) {
            e$buildCanvas()
            e$c1$drawImage()
        } else enabled(e$upper) <- FALSE
        enabled(e$lower) <- FALSE
        e$loaded <- FALSE
    })
    tbl[3, 7, anchor = c(0, 0)] <- e$clear.xVar
    tbl[5, 7, anchor = c(0, 0)] <- e$clear.yVar

    tbl[3,8] <- ""

    add(e$controls.iNZight, tbl, expand = FALSE)
    addSpace(e$controls.iNZight, 10, horizontal = TRUE)
    visible(tbl) <- TRUE

    # Because we only ever have to deal with one variable at a time with the
    # permvar method, we disable the drop label and clear button for the
    # second variable.
    if (e$title %in% c("Randomisation variation", "Confidence interval coverage")) {
        enabled(e$yVar) <- FALSE
        enabled(e$clear.yVar) <- FALSE
        enabled(e$label.var2) <- FALSE
    }
}
