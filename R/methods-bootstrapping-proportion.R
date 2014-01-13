canvasBootProp <- setRefClass("canvasBootPropClass", contains = "canvasPlotClass",
                              methods = list(
    plotSample = function(env, i = which.sample) {
        plotSampleProportions(.self, env, i)
    },

    showLabels = function() {
        bootLabels(.self)
    },

    calcStat = function(i = which.sample, y = NULL, canvas = .self) {
        calcProportion(samples[[i]], y, canvas)
    },

    calcAllStats = function(a, b = NULL, canvas = .self) {
        calcProportion(a, b, canvas)
    },

    plotDataStat = function(env, ...) {
        showPropDataStat(.self)
    },

    plotStatDist = function(env, ...) {
        plotBootDist(.self, env)
    },

    animateSample = function(...) {
        moveDataTextAndDropPointsProportion(.self, ...)
    },

    trackSample = function(env) {
        trackBootstrapProportion(.self)
    },

    animateStat = function(env, n.steps) {
        dropStat(.self, env, n.steps, linelength = 0.5)
    },

    displayResult = function(env, ...) {
        showCIandStats(.self, env, ...)
    },

    handle1000 = function(env, ...) {
        boot1000proportion(.self, env, ...)
    },

    fadePlots = function(env, ...) {
        fadeSampleAndStat(.self, env, ...)
    }))

load_bootstrap_proportion <- function(e) {
    e$c1$stat.in.use <- svalue(e$stat)
    # There is something messy going on with viewports and
    # gTrees, such that when attempting to import them, we
    # end up with "uninitializedField"s. Assign to a temp
    # var and reassign later.
    tmp.vps <- e$c1$viewports
    tmp.image <- e$c1$image
    e$c1$viewports <- NULL
    e$c1$image <- NULL
    tmp.canvas <- canvasBootProp$new()
    tmp.canvas$import(e$c1)
    e$c1 <- tmp.canvas
    e$c1$viewports <- tmp.vps
    e$c1$image <- tmp.image
    e$difference <- FALSE
}

showPropDataStat <- function(canvas) {
    totalProp <- calcProportion(canvas$x, canvas$levels, canvas)
    canvas$image <- addGrob(canvas$image, textGrob
                            (label = format(round(totalProp, canvas$dp), nsmall = canvas$dp),
                             x = unit(totalProp, "native"),
                             y = unit(0.6, "npc"),
                             just = c("centre", "bottom"),
                             gp = gpar(lwd = 2, col = "red", fontface = "bold"),
                             vp = canvas$graphPath("data", 1),
                             name = "dataPlot.stat.text"))
}

calcProportion <- function(x, y = NULL, canvas) {
    data <- as.character(x)
    n1 <- data[data == canvas$loi]
    prop <- length(n1) / length(data)
    prop
}

moveDataTextAndDropPointsProportion <- function(canvas, e, drop.points = FALSE, n.steps = 10, n.slow = 5, max = 50){
    canvas$rmGrobs(c("dataPlot.ci.1", "samplePlot.text.sample", "samplePlot.databox.text.1",
                     "samplePlot.points.1", "samplePlot.lines.1", "samplePlot.databox.text.2",
                     "samplePlot.propbar.1", "samplePlot.blank.1"))
    index <- canvas$indexes[[canvas$which.sample]]
    x <- canvas$x[index]
    n <- canvas$n
    dataPoints <- getGrob(canvas$image, gPath("dataPlot"))
    animate.points <- dataPoints$points.present
    levels <- sort(unique(canvas$x))

    # Attempt to animate points if possible, otherwise randomly generate
    # points to create and drop down.
    if (animate.points) {
        # Converting units in this viewport as we had issues using
        # the sample viewport when creating propbar grobs.
        seekViewport("data.data.1")
        dpx <- dataPoints$x[index]
        dpy <- dataPoints$y[index]
        dpx <- convertX(dpx, "native", valueOnly = TRUE)
        dpy <- convertY(dpy, "native", valueOnly = TRUE)
        upViewport(0)
        samplePoints <- propbarGrob(data = x,
                                    y = 0.1,
                                    lois = c(canvas$loi, canvas$loi.alt),
                                    height = 0.4,
                                    name = "samplePlot",
                                    vp = canvas$graphPath("sample"))
        spx <- samplePoints$x
        spy <- samplePoints$y
        seekViewport("sample.data.1")
        spx <- convertX(spx, "native", valueOnly = TRUE)
        spy <- convertY(spy, "native", valueOnly = TRUE)
        upViewport(0)
        y.start <- dpy + 2
        y.end <- spy + 1
        y.step <- (y.end - y.start)/n.steps
        x.step <- (spx - dpx) / n.steps
    }

    if (n < 100) {
        n.slow <- min(n.slow, n)
        ## Calculating the position of text in text boxes.
        ntext <- min(n, max)
        npcs <- (ntext:0)/ntext
        yunit <- (unit(npcs, "npc") - unit(4*(npcs - 0.5), "mm") + unit(1 - npcs, "lines"))
        x.text.start <- 0.25
        x.text.end <- 0.75
        x.text.step <- (x.text.end - x.text.start) / n.steps
        pointCols <- ifelse(x == canvas$loi, "deepskyblue", "red3")

        if (drop.points & animate.points)
            canvas$image <- addGrob(canvas$image, rectGrob
                                    (x = unit(0, "native"), y = unit(0.1, "native"),
                                     width = unit(1, "native"), height = unit(0.4, "native"),
                                     just = c("left", "bottom"),
                                     gp = gpar(col = "black",
                                               fill = "transparent", lwd = 3),
                                     name = paste("samplePlot.boundRect", 1, sep = "."),
                                     vp = canvas$graphPath("sample")))

        ## Animation of slow points.
        for (i in seq(from = 1, by = 1, length.out = n.slow)) {
            y.text.start <- yunit[min(c(max + 1, index[i] + 1))]
            y.text.end <- yunit[i + 1]
            y.text.step <- convertY(y.text.end - y.text.start, "npc", valueOnly = TRUE)/n.steps

            # If we can, just draw a datatextGrob
            if (index[i] < ntext) {
                canvas$image <-  addGrob(canvas$image, datatextGrob
                                         (data = canvas$x, title = canvas$x.name, max = 50,
                                          name = "text.sample", vp = canvas$graphPath("databox", 1),
                                          gp = gpar(col = c(rep("black", index[i]), "red", rep("black", n - index[i])),
                                                    fontface = c(rep(1, index[i]), 2, rep(1, n - index[i])))))
            } else {
                # If the text is below the `...' redraw the datatextGrob
                # and draw a piece of text above the `...'
                canvas$image <-  addGrob(canvas$image, datatextGrob
                                         (data = canvas$x, title = canvas$x.name, max = 50,
                                          name = "text.sample", vp = canvas$graphPath("databox", 1),
                                          gp = gpar(col = "black",
                                                    fontface = 1)))
                canvas$image <- addGrob(canvas$image, textGrob
                                        (label = x[i],
                                         y = y.text.start, just = "top",
                                         gp = gpar(col = "red", fontface = 2), name = "samplePlot.temp.text",
                                         vp = canvas$graphPath("databox", 1)))
            }
            canvas$image <- addGrob(canvas$image, linesGrob
                                    (x = c(0.9, 0.8),
                                     y = y.text.start - unit(0.5, "lines"),
                                     gp = gpar(lwd = 3, col = "red"),
                                     arrow = arrow(length = unit(0.1, "inches")), name = "samplePlot.temp.arrow",
                                     vp = canvas$graphPath("databox", 1)))

            ## Light up point to drop
            if (drop.points & animate.points) {
                temp.point <- pointsGrob(x = dpx[i], y = dpy[i], pch = 19, gp = gpar(col = pointCols[i]),
                                         vp = canvas$graphPath("data"), name = "samplePlot.temp.point")
                canvas$image <- addGrob(canvas$image, temp.point)
            }

            if (canvas$stopAnimation)
                return()
            canvas$pauseImage(5)

            for (j in 1:n.steps) {
                # Move text when we can't just colour and embolden the datatextGrob
                if (j < n.steps) {
                    canvas$image <- addGrob(canvas$image, textGrob
                                            (label = x[i],
                                             y = y.text.start + unit(j*y.text.step, "npc"),
                                             x = unit(x.text.start + j*x.text.step, "npc"),
                                             just = "top", gp = gpar(col = "red", fontface = 2),
                                             name = "samplePlot.temp.text",
                                             vp = vpPath("canvas.all", "canvas.boxes")))
                } else {
                    # Can just embolden the datatextGrob on last step
                    canvas$image <- removeGrob(canvas$image, gPath("samplePlot.temp.text"))
                    canvas$image <- addGrob(canvas$image, datatextGrob
                                            (data = x, title = "Re-sample", max = 50,
                                             name = "samplePlot.databox.text.2", vp = canvas$graphPath("databox", 2),
                                             gp = gpar(col = "red",
                                                       alpha = c(rep(1, i + 1), rep(0, n - i)),
                                                       fontface = c(rep(1, i), 2, rep(1, n - i)))))
                }

                # Drop point
                if (drop.points & animate.points) {
                    canvas$image <- addGrob(canvas$image, pointsGrob
                                            (x = dpx[i] + j*x.step[i], y = y.start[i] + j*y.step[i], pch = 19,
                                             gp = gpar(col = pointCols[i]), vp = canvas$graphPath("animation.field"), name = "samplePlot.temp"))
                }
                if (canvas$stopAnimation)
                    return()
                canvas$drawImage()
                if (j == n.steps & drop.points & animate.points)
                    canvas$rmGrobs("samplePlot.temp")
            }

            ## Make points permanent if dropping
            if (drop.points & animate.points)
                plotPoints(canvas, spx[1:i], y.end[1:i] - 1, canvas$graphPath("sample"),
                           col = ifelse(x[1:i] == canvas$loi, "blue", "red"),
                           "samplePlot", black = FALSE)
            if (canvas$stopAnimation)
                return()
            canvas$pauseImage(5)
            # Make text in resample databox permanent
            canvas$rmGrobs("samplePlot.temp.text")
            canvas$image <- addGrob(canvas$image, datatextGrob
                                    (data = x, title = "Re-sample", max = 50,
                                     name = "samplePlot.databox.text.2", vp = canvas$graphPath("databox", 2),
                                     gp = gpar(col = "red",
                                               alpha = c(rep(1, i + 1), rep(0, n - i)),
                                               fontface = 1)))
        }

        ## Animation of fast points.
        if (n == ntext) length.out <- ntext - n.slow else length.out <- ntext - n.slow - 1
        for (i in seq(from = n.slow + 1, by = 1, length.out = length.out)) {
            y.text.start <- yunit[min(c(max + 1, index[i] + 1))]

            # If we do not encounter a `...' then redraw a datatextGrob
            if (index[i] < ntext) {
                canvas$rmGrobs("samplePlot.temp.text")
                canvas$image <- addGrob(canvas$image, datatextGrob
                                       (data = canvas$x, title = canvas$x.name, max = 50,
                                        name = "text.sample", vp = canvas$graphPath("databox", 1),
                                        gp = gpar(col = c(rep("black", index[i]), "red", rep("black", n - index[i])),
                                                  fontface = c(rep(1, index[i]), 2, rep(1, n - index[i])))))
            } else {
                # Just draw a plain datatextGrob but overlay the number
                # on top of the `...'
                canvas$image <- addGrob(canvas$image, datatextGrob
                                        (data = canvas$x, title = canvas$x.name, max = 50,
                                         name = "text.sample", vp = canvas$graphPath("databox", 1),
                                         gp = gpar(col = "black",
                                                   fontface = 1)))
                canvas$image <- addGrob(canvas$image, textGrob
                                        (label = x[i],
                                         y = y.text.start, just = "top",
                                         gp = gpar(col = "red", fontface = 2), name = "samplePlot.temp.text",
                                         vp = canvas$graphPath("databox", 1)))
            }
            canvas$image <- addGrob(canvas$image, linesGrob(x = c(0.9, 0.8),
                                    y = y.text.start - unit(0.5, "lines"),
                                    gp = gpar(lwd = 3, col = "red"),
                                    arrow = arrow(length = unit(0.1, "inches")), name = "samplePlot.temp.arrow",
                                    vp = canvas$graphPath("databox", 1)))

            ## Light up point to drop.
            if (drop.points & animate.points) {
                temp.point <- pointsGrob(x = dpx[i], y = dpy[i], pch = 19,
                                         gp = gpar(col = pointCols[i]),
                                         vp = canvas$graphPath("data"), name = "samplePlot.temp.point")
                canvas$image <- addGrob(canvas$image, temp.point)
            }

            # Getting rid of the bolding
            canvas$image <- addGrob(canvas$image, datatextGrob
                                    (data = x, title = "Re-sample", max = 50,
                                     name = "samplePlot.databox.text.2", vp = canvas$graphPath("databox", 2),
                                     gp = gpar(col = "red",
                                               alpha = c(rep(1, i + 1), rep(0, n - i)),
                                               fontface = 1)))

            # Plot dropped point.
            if (drop.points & animate.points)
                plotPoints(canvas, spx[1:i], y.end[1:i] - 1, canvas$graphPath("sample"),
                           col = ifelse(x[1:i] == canvas$loi, "blue", "red"),
                           name = "samplePlot", black = FALSE)

            if (! is.null(e$slowIteration)) {
                if (e$slowIteration) {
                    if (canvas$stopAnimation)
                        return()
                    canvas$pauseImage(5)
                } else {
                    if (canvas$stopAnimation)
                        return()
                    canvas$drawImage()
                }
            } else {
                if (canvas$stopAnimation)
                    return()
                canvas$pauseImage(5)
            }
        }

        # Adding the `...' to the re-sample datatextGrob
        canvas$image <- addGrob(canvas$image, datatextGrob
                                (data = x, title = "Re-sample", max = 50,
                                 name = "samplePlot.databox.text.2", vp = canvas$graphPath("databox", 2),
                                 gp = gpar(col = "red",
                                           alpha = 1,
                                           fontface = 1)))

        ## Animation of points outside databox.
        for (i in seq(from = ntext, by = 1, length.out = n - ntext)) {
            y.text.start <- yunit[min(c(max + 1, index[i] + 1))]

            # If we do not encounter a `...' then redraw a datatextGrob
            if (index[i] < ntext) {
                canvas$rmGrobs("samplePlot.temp.text")
                canvas$image <- addGrob(canvas$image, datatextGrob
                                       (data = canvas$x, title = canvas$x.name, max = 50,
                                        name = "text.sample", vp = canvas$graphPath("databox", 1),
                                        gp = gpar(col = c(rep("black", index[i]), "red", rep("black", n - index[i])),
                                                  fontface = c(rep(1, index[i]), 2, rep(1, n - index[i])))))
            } else {
                # Just draw a plain datatextGrob but overlay the number
                # on top of the `...'
                canvas$image <- addGrob(canvas$image, datatextGrob
                                        (data = canvas$x, title = canvas$x.name, max = 50,
                                         name = "text.sample", vp = canvas$graphPath("databox", 1),
                                         gp = gpar(col = "black",
                                                   fontface = 1)))
                canvas$image <- addGrob(canvas$image, textGrob
                                        (label = x[i],
                                         y = y.text.start, just = "top",
                                         gp = gpar(col = "red", fontface = 2), name = "samplePlot.temp.text",
                                         vp = canvas$graphPath("databox", 1)))
            }
            canvas$image <- addGrob(canvas$image, linesGrob(x = c(0.9, 0.8),
                                    y = y.text.start - unit(0.5, "lines"),
                                    gp = gpar(lwd = 3, col = "red"),
                                    arrow = arrow(length = unit(0.1, "inches")), name = "samplePlot.temp.arrow",
                                    vp = canvas$graphPath("databox", 1)))

            ## Light up point to drop.
            if (drop.points & animate.points) {
                temp.point <- pointsGrob(x = dpx[i], y = dpy[i], pch = 19,
                                         gp = gpar(col = pointCols[i]),
                                         vp = canvas$graphPath("data"), name = "samplePlot.temp.point")
                canvas$image <- addGrob(canvas$image, temp.point)
            }

            ## Plot dropped point.
            if (drop.points & animate.points)
                plotPoints(canvas, spx[1:i], y.end[1:i] - 1, canvas$graphPath("sample"),
                           col = ifelse(x[1:i] == canvas$loi, "blue", "red"),
                           name = "samplePlot", black = FALSE)

            if (! is.null(e$slowIteration)) {
                if (e$slowIteration) {
                    if (canvas$stopAnimation)
                        return()
                    canvas$pauseImage(5)
                } else {
                    if (canvas$stopAnimation)
                        return()
                    canvas$drawImage()
                }
            } else {
                if (canvas$stopAnimation)
                    return()
                canvas$pauseImage(5)
            }
        }
        plotPoints(canvas, spx, y.end - 1, canvas$graphPath("sample"),
                   "samplePlot", black = FALSE)
        canvas$rmGrobs("samplePlot.boundRect.1")
        canvas$plotSample(e, canvas$which.sample)
        if (canvas$stopAnimation)
            return()
        canvas$pauseImage(10)
        canvas$rmGrobs("samplePlot.temp.text")
        canvas$image <- addGrob(canvas$image, datatextGrob
                                (data = canvas$x, title = canvas$x.name, max = 50,
                                 name = "text.sample", vp = canvas$graphPath("databox", 1),
                                 gp = gpar(col = "black",
                                           fontface = 1)))
        canvas$image <- removeGrob(canvas$image, gPath("samplePlot.temp.arrow"))
        if (drop.points & animate.points)
            canvas$image <- removeGrob(canvas$image, gPath("samplePlot.temp.point"))
    }
}

trackBootstrapProportion <- function(canvas) {
    # Not useful to do tracking in large samples
    if (canvas$n >= 100)
        return()

    index <- canvas$indexes[[canvas$which.sample]]
    sample <- canvas$x
    resample <- sample[index]
    n <- canvas$n
    ntext <- min(n, 50)
    npcs <- (ntext:0)/ntext
    yunit <- (unit(npcs, "npc") - unit(4*(npcs - 0.5), "mm") + unit(1 - npcs, "lines"))
    canvas$image <- addGrob(canvas$image, datatextGrob
                            (data = resample, title = "Re-sample", max = 50,
                             name = "samplePlot.databox.text.2", vp = canvas$graphPath("databox", 2)))
    max.width <- max(convertX(stringWidth(resample), "cm", valueOnly = TRUE))
    max.width <- unit(max.width, "cm")

    dataPoints <- getGrob(canvas$image, gPath("dataPlot"))
    animate.points <- dataPoints$points.present
    pointCols <- ifelse(sample == canvas$loi, "deepskyblue", "red3")

    if (animate.points) {
        dpx <- dataPoints$x
        dpy <- dataPoints$y
        samplePoints <- getGrob(canvas$image, gPath("samplePlot.propbar.1"))
        spx <- samplePoints$x
        spy <- samplePoints$y
    }

    if (n <= 50) finalobs <- n else finalobs <- 49
    for (i in 1:finalobs) {
        value <- sample[i]
        canvas$image <- addGrob(canvas$image, datatextGrob
                                (data = sample, title = canvas$x.name, max = 50,
                                 name = "text.sample", vp = canvas$graphPath("databox", 1),
                                 gp = gpar(col = c(rep("black", i), "red", rep("black", n - i)),
                                           fontface = c(rep(1, i), 2, rep(1, n - i)))))
        if (animate.points)
            canvas$image <- addGrob(canvas$image, pointsGrob
                                    (x = dpx[i], y = dpy[i], pch = 19, gp = gpar(col = pointCols[i]),
                                     name = "samplePlot.temp.point", vp = canvas$graphPath("data")))
        resamp.ys <- c(FALSE, index == i)
        if (any(resamp.ys)) {
            subset.resamp <- c(resamp.ys[1:(ntext + 1)])
            if (n != ntext) subset.resamp <- c(resamp.ys[1:ntext], any(resamp.ys[(ntext + 1):(n + 1)]))

            # Creating red bolded text only for labels which represent a match
            cols <- rep("black", ntext + 1)
            cols[resamp.ys] <- "red"
            ff <- rep(1, ntext + 1)
            ff[resamp.ys] <- 2
            canvas$image <- addGrob(canvas$image, datatextGrob
                                    (data = resample, title = "Re-sample", max = 50,
                                     name = "samplePlot.databox.text.2", vp = canvas$graphPath("databox", 2),
                                     gp = gpar(col = cols,
                                               fontface = ff)))

            # If we have a ... at the end of our grob, add a textGrob
            if (n != ntext & any(resamp.ys[(ntext + 1):(n + 1)])) {
                canvas$image <- addGrob(canvas$image, textGrob
                                        (label = value,
                                         y = min(yunit), just = "top",
                                         gp = gpar(col = "red", fontface = 2), name = "samplePlot.temp.resamp",
                                         vp = canvas$graphPath("databox", 2)))
            }

            canvas$image <- addGrob(canvas$image, segmentsGrob
                                    (x0 = unit(0.25, "npc") + max.width*0.5 + unit(1, "mm"),
                                     x1 = unit(0.75, "npc") - max.width*0.5 - unit(1, "mm"),
                                     y0 = yunit[i + 1] - unit(0.5, "lines"),
                                     y1 = yunit[subset.resamp] - unit(0.5, "lines"),
                                     name = "samplePlot.temp.segments", gp = gpar(lwd = 2, col = "red"),
                                     vp = vpPath("canvas.all", "canvas.boxes")))
            if (animate.points)
                canvas$image <- addGrob(canvas$image, pointsGrob
                                        (x = spx[index == i], y = spy[index == i],
                                         pch = 19,
                                         gp = gpar(col = pointCols[i]), name = "samplePlot.temp.resamplepoints",
                                         vp = canvas$graphPath("sample")))
            if (canvas$stopAnimation)
                return()
            canvas$pauseImage(10)
            # Only removing a textGrob if it is known to appear on a ...
            if (n != ntext & any(resamp.ys[(ntext + 1):(n + 1)]))
                canvas$image <- removeGrob(canvas$image, gPath("samplePlot.temp.resamp"))
            canvas$rmGrobs(c("samplePlot.temp.segments", "samplePlot.temp.resamplepoints"))
        } else {
            cols <- rep("black", ntext + 1)
            ff <- rep(1, ntext + 1)
            canvas$image <- addGrob(canvas$image, datatextGrob
                                    (data = resample, title = "Re-sample", max = 50,
                                     name = "samplePlot.databox.text.2", vp = canvas$graphPath("databox", 2),
                                     gp = gpar(col = cols,
                                               fontface = ff)))
            if (canvas$stopAnimation)
                return()
            canvas$pauseImage(10)
        }
    }

    canvas$rmGrobs(c("samplePlot.temp.samp", "samplePlot.temp.point"))
    # Resetting datatextGrobs after animation has been completed
    canvas$image <- addGrob(canvas$image, datatextGrob
                            (data = sample, title = canvas$x.name, max = 50, gp = gpar(col = "black"),
                             name = "text.sample", vp = canvas$graphPath("databox", 1)))
    canvas$image <- addGrob(canvas$image, datatextGrob
                            (data = resample, title = "Re-sample", max = 50, gp = gpar(col = "red"),
                             name = "samplePlot.databox.text.2", vp = canvas$graphPath("databox", 2)))
}

plotSampleProportions <- function(canvas, e, i) {
    canvas$rmGrobs(c("dataPlot.ci.1", "samplePlot.rect.1", "samplePlot.propbar.1"))
    x <- canvas$samples[[i]]
    canvas$sampled.stats <- c(canvas$sampled.stats, canvas$which.sample)
    allinfo <- c(canvas$stat.dist, recursive = TRUE)
    alpha <- 0.2

    canvas$image <- addGrob(canvas$image,
                            rectGrob(x = unit(allinfo[canvas$sampled.stats], "npc"),
                                     y = unit(0, "npc"), height = unit(0.3, "npc"),
                                     width = unit(0, "npc"), just = c("left", "bottom"),
                                     gp = gpar(alpha = alpha, col = "blue", lwd = 2),
                                     vp = canvas$graphPath("sample"),
                                     name = "samplePlot.ghosts.1"))
    # We add a blank white rect to obscure most of the ghosting lines
    # so that they are not visible through the propbar.
    canvas$image <- addGrob(canvas$image,
                            rectGrob(x = unit(0, "npc"), y = unit(0.1, "npc"),
                                     width = unit(1, "npc"), height = unit(0.4, "npc"),
                                     just = c("left", "bottom"),
                                     gp = gpar(fill = "white", col = "white"),
                                     vp = canvas$graphPath("sample"),
                                     name = "samplePlot.blank.1"))
    canvas$image <- addGrob(canvas$image,
                            propbarGrob(data = x,
                                        y = 0.1,
                                        height = 0.4,
                                        lois = c(canvas$loi, canvas$loi.alt),
                                        draw.points = length(x) <= 100,
                                        name = "samplePlot.propbar.1",
                                        vp = canvas$graphPath("sample")))
    canvas$image <- addGrob(canvas$image, datatextGrob(data = x, title = "Re-sample",
                                                       max = 50, name = "samplePlot.databox.text.2",
                                                       vp = canvas$graphPath("databox", 2), gp = gpar(col = "red")))
}

boot1000proportion <- function(canvas, e, points = FALSE) {
    # Starting from scratch with ghosting & points
    canvas$sampled.stats <- NULL
    canvas$plotted.stats <- NULL
    canvas$rmGrobs(c("samplePlot.databox.text.2", "dataPlot.ci.1"))
    allx <- c(canvas$stat.dist, recursive = TRUE)
    y.max <- unit(1, "npc") - unit(2, "lines") - unit(0.5, "char")

    for (i in 20*(1:50)) {
        x <- canvas$samples[[i]]
        xs <- allx[1:i] # All of the proportions so far
        y <- canvas$stat.ypos[1:i]
        if (points)
            plotPoints(canvas, xs, y, canvas$graphPath("stat"),
                       "statPlot", black = FALSE, alpha = 0.7)
        canvas$rmGrobs("samplePlot.propbar.1")
        alpha <- 0.02
        canvas$image <- addGrob(canvas$image,
                                rectGrob(x = unit(xs, "npc"),
                                         y = unit(0, "npc"), height = unit(0.3, "npc"),
                                         width = unit(0, "npc"), just = c("left", "bottom"),
                                         gp = gpar(alpha = alpha, col = "blue", lwd = 2),
                                         vp = canvas$graphPath("sample"),
                                         name = "samplePlot.ghosts.1"))
        # We add a blank white rect to obscure most of the ghosting lines
        # so that they are not visible through the propbar.
        canvas$image <- addGrob(canvas$image,
                                rectGrob(x = unit(0, "npc"), y = unit(0.1, "npc"),
                                         width = unit(1, "npc"), height = unit(0.4, "npc"),
                                         just = c("left", "bottom"),
                                         gp = gpar(fill = "white", col = "white"),
                                         vp = canvas$graphPath("sample"),
                                         name = "samplePlot.blank.1"))
        canvas$image <- addGrob(canvas$image,
                                propbarGrob(data = x,
                                            y = 0.1,
                                            height = 0.4,
                                            lois = c(canvas$loi, canvas$loi.alt),
                                            draw.points = FALSE,
                                            name = "samplePlot.propbar.1",
                                            vp = canvas$graphPath("sample")))
        canvas$image <- addGrob(canvas$image, datatextGrob(data = x, title = "Re-sample",
                                                           max = 50, name = "samplePlot.databox.text.2",
                                                           vp = canvas$graphPath("databox", 2), gp = gpar(col = "red")))
        canvas$showLabels()
        if (canvas$stopAnimation)
            return()
        canvas$drawImage()
    }

    canvas$rmGrobs(c("samplePlot.blank.1", "samplePlot.propbar.1", "samplePlot.databox.text.2"))
    canvas$drawImage()
    ## Remove 1000 statistics next time something is plotted to avoid
    ## further statistics being plotted on top.
    #canvas$image <- removeGrob(canvas$image, gPath("statPlot.points.1"))
    ## Reset CI counter
    canvas$sampled.stats <- NULL
    canvas$plotted.stats <- NULL
}
