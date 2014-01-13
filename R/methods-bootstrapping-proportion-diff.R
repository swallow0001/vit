canvasBootPropDiff <- setRefClass("canvasBootPropDiffClass", contains = "canvasPlotClass",
                             methods = list(
    plotSample = function(env, i = which.sample, alpha = 0.1) {
        plotBootPropDiffSample(.self, env, i, alpha = alpha)
    },

    showLabels = function() {
        bootLabelsTwoSample(.self)
    },

    calcStat = function(i = which.sample, ys = NULL, canvas = .self) {
        props <- calcPropDiff(samples[[i]], level.samples[[i]], canvas)
        # Sorting in ylevel order but reversing so we end up
        # with correct differences
        rev(props[canvas$ylevels])
    },

    calcAllStats = function(xs, ys = NULL, canvas = .self) {
        props <- calcPropDiff(xs, ys, canvas)
        # Sorting in ylevel order but reversing so we end up
        # with correct differences
        rev(props[canvas$ylevels])
    },

    plotDataStat = function(env, ...) {
        dataDiffArrowProp(.self, env)
    },

    plotStatDist = function(env, ...) {
        plotBootDiffDist(.self, env)
    },

    animateSample = function(...) {
        dropBootPropDiffSample(.self, ...)
    },

    trackSample = function(...) {
        trackBootstrapPropDiff(.self)
    },

    animateStat = function(env, n.steps) {
        dropBootPropDiffArrow(.self, env, n.steps)
    },

    displayResult = function(env, ...) {
        showCIandStatsDiff(.self, env, ...)
    },

    handle1000 = function(env, ...) {
        bootPropDiff1000(.self, env, ...)
    },

    fadePlots = function(env, ...) {
        fadeAllCI(.self, env, ...)
    }))

load_bootstrap_proportion_diff <- function(e) {
    e$c1$stat.in.use <- svalue(e$stat)
    # There is something messy going on with viewports and
    # gTrees, such that when attempting to import them, we
    # end up with "uninitializedField"s. Assign to a temp
    # var and reassign later.
    tmp.vps <- e$c1$viewports
    tmp.image <- e$c1$image
    e$c1$viewports <- NULL
    e$c1$image <- NULL
    tmp.canvas <- canvasBootPropDiff$new()
    tmp.canvas$import(e$c1)
    e$c1 <- tmp.canvas
    e$c1$viewports <- tmp.vps
    e$c1$image <- tmp.image
    ## Gives a value for the additional room given to each end of the x axis scale.
    e$extra <- 0
    e$difference <- TRUE
}

bootPropDiff1000 <- function(canvas, e, points) {
    stats <- apply(canvas$stat.dist, 1, diff)
    y.max <- unit(1, "npc") - unit(2, "lines") - unit(0.5, "char")
    for (i in 20*(1:50)) {
        canvas$sampled.stats <- 1:i
        plotBootPropDiffSample(canvas, e, i, alpha = 0.02, points1000 = TRUE)
        if (points) {
            x <- stats[1:i]
            y <- canvas$stat.ypos[1:i]
            plotPoints(canvas, x, y, canvas$graphPath("stat"),
                       "statPlot", black = FALSE, alpha = 0.7)
        }
        canvas$showLabels()
        if (canvas$stopAnimation)
            return()
        canvas$drawImage()
    }
    canvas$rmGrobs("databox.text.2")
    for (i in 1:2) {
        canvas$rmGrobs(c(paste(c("samplePlot.line", "samplePlot.blank",
                                 "samplePlot.points", "samplePlot.propbar"), i, sep = ".")))
    }
    canvas$rmGrobs("samplePlot.stat.2")
    canvas$drawImage()
    canvas$sampled.stats <- NULL
    canvas$plotted.stats <- NULL
}

dropBootPropDiffSample <- function(canvas, e, drop.points = FALSE, n.steps = 10, n.slow = 5, max = 50) {
    canvas$rmGrobs(c("dataPlot.ci.1", "samplePlot.text.sample", "databox.text.1", "samplePlot.databox.text.2"))
    rmvec <- 1:length(canvas$ylevels)
    canvas$rmGrobs(c(paste("samplePlot.blank", rmvec, sep = "."),
                     paste("samplePlot.propbar", rmvec, sep = "."),
                     "samplePlot.line.1"))

    n <- canvas$n
    index <- canvas$indexes[[canvas$which.sample]]
    sample <- canvas$x
    levels <- as.character(canvas$levels)
    resample <- sample[index]
    resample.levels <- levels[index]
    ylevels <- canvas$ylevels
    dataProp1 <- getGrob(canvas$image, gPath("dataPlot.propbar.1"))
    dataProp2 <- getGrob(canvas$image, gPath("dataPlot.propbar.2"))
    animate.points <- dataProp1$points.present & dataProp2$points.present
    pointCols <- ifelse(resample.levels == ylevels[1],
                        ifelse(resample == canvas$loi,
                               getColour(1, 2, l = 85),
                               getColour(1, 2, l = 45)),
                        ifelse(resample == canvas$loi,
                               getColour(2, 2, l = 85),
                               getColour(2, 2, l = 45)))

    # In the case that we're tracking, get rid of the sample plots
    if (drop.points & animate.points)
        canvas$rmGrobs(c("samplePlot.propbar.1", "samplePlot.propbar.2",
                         "samplePlot.ghosts.1", "samplePlot.ghosts.2",
                         "samplePlot.blank.1", "samplePlot.blank.2",
                         "samplePlot.line.1"))

    if (n < 100) {
        if (animate.points) {
            # Need to dive into each VP and grab the size of the units relative
            # animation.field VP. Because we're dealing with two data VPs, each
            # will be scaled by a half.
            depth <- downViewport(canvas$graphPath("data", 1))
            prop1.dpx <- convertX(dataProp1$x, "native", valueOnly = TRUE)
            prop1.dpy <- convertY(dataProp1$y, "native", valueOnly = TRUE) / length(ylevels)
            upViewport(depth)
            depth <- downViewport(canvas$graphPath("data", 2))
            prop2.dpx <- convertX(dataProp2$x, "native", valueOnly = TRUE)
            prop2.dpy <- convertY(dataProp2$y, "native", valueOnly = TRUE) / length(ylevels)
            upViewport(depth)

            # Creating sample propbars and grabbing the point locations
            # from each of the propbars.
            sampleProp1 <- kpropbarGrob(resample[resample.levels == ylevels[1]],
                                        ylab = ylevels[1],
                                        draw.points = animate.points,
                                        lois = c(canvas$loi, canvas$loi.alt),
                                        y = 0.1, height = 0.5,
                                        name = paste("samplePlot.propbar", 1, sep="."),
                                        vp = canvas$graphPath("sample", 1))
            sampleProp2 <- kpropbarGrob(resample[resample.levels == ylevels[2]],
                                        ylab = ylevels[2],
                                        draw.points = animate.points,
                                        lois = c(canvas$loi, canvas$loi.alt),
                                        y = 0, height = 0.5,
                                        name = paste("samplePlot.propbar", 2, sep="."),
                                        vp = canvas$graphPath("sample", 2))
            depth <- downViewport(canvas$graphPath("sample", 1))
            prop1.spx <- convertX(sampleProp1$x, "native", valueOnly = TRUE)
            prop1.spy <- convertY(sampleProp1$y, "native", valueOnly = TRUE) / length(ylevels)
            upViewport(depth)
            depth <- downViewport(canvas$graphPath("sample", 2))
            prop2.spx <- convertX(sampleProp2$x, "native", valueOnly = TRUE)
            prop2.spy <- convertY(sampleProp2$y, "native", valueOnly = TRUE) / length(ylevels)
            upViewport(depth)

            # Need to translate the coordinates so that they fit in the
            # animation.field viewport
            prop1.dpy <- prop1.dpy + 2
            prop2.dpy <- prop2.dpy + 2.5
            prop1.spy <- prop1.spy + 1
            prop2.spy <- prop2.spy + 1.5
        }

        first.group <- levels == ylevels[1]
        ntext <- min(n, 50)
        npcs <- (ntext:0)/ntext
        yunit <- (unit(npcs, "npc") - unit(4*(npcs - 0.5), "mm") + unit(1 - npcs, "lines"))
        max.width.x <- max(convertX(stringWidth(sample),
                                    "cm", valueOnly = TRUE))
        max.width.y <- max(convertX(stringWidth(ylevels),
                                    "cm", valueOnly = TRUE))
        max.width.x <- unit(max.width.x, "cm")
        max.width.y <- unit(max.width.y, "cm")

        if (drop.points & animate.points) {
            # Drawing labels to more easily identify which level each point belongs to.
            depth <- downViewport(canvas$graphPath("sample", 1))
            ylab.height <- convertHeight(max(stringHeight(ylevels)), "native", valueOnly = TRUE)
            upViewport(depth)
            for (i in 1:length(ylevels)) {
                canvas$image <- addGrob(canvas$image, textGrob
                                        (ylevels[i],
                                         x = unit(1, "npc"),
                                         y = unit(0.7, "native"),
                                         just = c("right", "bottom"),
                                         gp = gpar(col = getColour(i, length(ylevels))),
                                         name = paste("samplePlot.yLab", i, sep = "."),
                                         vp = canvas$graphPath("sample", i)))
            }
        }

        if (animate.points) {
            # Calculating index vectors so that the i'th
            # value corresponds to the correct location within
            # each of the data propbars.
            first.level.ind <- 1
            second.level.ind <- 1
            data.index <- integer(length(levels))
            for (i in 1:length(levels)) {
                if (levels[i] == ylevels[1]) {
                    data.index[i] <- first.level.ind
                    first.level.ind <- first.level.ind + 1
                } else {
                    data.index[i] <- second.level.ind
                    second.level.ind <- second.level.ind + 1
                }
            }

            first.level.ind <- 1
            second.level.ind <- 1
            sample.index <- integer(length(levels))
            for (i in 1:length(resample)) {
                if (resample.levels[i] == ylevels[1]) {
                    sample.index[i] <- first.level.ind
                    first.level.ind <- first.level.ind + 1
                } else {
                    sample.index[i] <- second.level.ind
                    second.level.ind <- second.level.ind + 1
                }
            }
        }

        n.slow <- min(n.slow, n)
        ## Calculating the position of text in text boxes.
        ntext <- min(n, max)
        npcs <- (ntext:0)/ntext
        yunit <- (unit(npcs, "npc") - unit(4*(npcs - 0.5), "mm") + unit(1 - npcs, "lines"))
        x.text.start <- c(0.25, 0.75)/2
        x.text.end <- 0.5 + x.text.start
        x.text.step <- (x.text.end[1] - x.text.start[1]) / n.steps
        datatexttitle <- datatextGrob(data = "", title = "Re-sample", name = "samplePlot.resamp.text",
                                      vp = canvas$graphPath("databox", 2))
        canvas$image <- addGrob(canvas$image, datatexttitle)
        # Initialising vectors for points so that we can draw them more easily later
        points.xs <- numeric(length(resample))
        points.ys <- numeric(length(resample))
        points.cols <- character(length(resample))

        if (drop.points & animate.points) {
            for (j in 1:length(ylevels)) {
                canvas$image <- addGrob(canvas$image, rectGrob
                                        (x = unit(0, "native"), y = unit(if (j == 1) 0.1 else 0, "native"),
                                         width = unit(1, "native"), height = unit(0.5, "native"),
                                         just = c("left", "bottom"),
                                         gp = gpar(col = getColour(j, length(ylevels)),
                                                   fill = "transparent", lwd = 3),
                                         name = paste("samplePlot.boundRect", j, sep = "."),
                                         vp = canvas$graphPath("sample", j)))
                canvas$image <- addGrob(canvas$image, rectGrob
                                        (x = unit(0, "native"), y = unit(0.1, "native"),
                                         width = unit(1, "native"), height = unit(0.6, "native"),
                                         just = c("left", "bottom"),
                                         gp = gpar(col = getColour(j, length(ylevels)),
                                                   fill = "transparent", lwd = 3),
                                         name = paste("samplePlot.databoundRect", j, sep = "."),
                                         vp = canvas$graphPath("data", j)))
            }
        }
        # Ensure labels are drawn on top of new shell rects
        canvas$showLabels()

        # Animation of slow points.
        for (i in seq(from = 1, by = 1, length.out = n.slow)) {
            y.text.start <- rep(yunit[min(c(max + 1, index[i] + 1))], 2)
            y.text.end <- rep(yunit[i + 1], 2)
            y.text.step <- convertY(y.text.end - y.text.start, "npc", valueOnly = TRUE) / n.steps
            temp.text <- textGrob(label = c(resample[i], resample.levels[i]),
                                  x = x.text.start, y = y.text.start, just = "top",
                                  gp = gpar(col = "red", fontface = 2), name = "samplePlot.temp.datatext",
                                  vp = vpPath("canvas.all", "canvas.boxes"))

            # Light up point to drop
            if (drop.points & animate.points) {
                temp.point <- pointsGrob(x = if (resample.levels[i] == ylevels[1]) prop1.dpx[data.index[index[i]]]
                                             else prop2.dpx[data.index[index[i]]],
                                         y = if (resample.levels[i] == ylevels[1]) prop1.dpy[data.index[index[i]]]
                                             else prop2.dpy[data.index[index[i]]],
                                         pch = 19, gp = gpar(col = pointCols[i]),
                                         vp = canvas$graphPath("animation.field"), name = "samplePlot.temp.point")
                canvas$image <- addGrob(canvas$image, temp.point)
            }
            canvas$image <- addGrob(canvas$image, temp.text)
            if (canvas$stopAnimation)
                return()
            canvas$pauseImage(5)

            # Because the position of the point is dependent on both the ylevel and the level
            # we need to work out the appropriate stepping from the data to the sample.
            if (resample.levels[i] == ylevels[1]) {
                if (levels[index[i]] == ylevels[1]) {
                    xs.step <- (prop1.dpx[data.index[index[i]]] - prop1.spx[sample.index[i]]) / n.steps
                    ys.step <- (prop1.dpy[data.index[index[i]]] - prop1.spy[sample.index[i]]) / n.steps
                } else {
                    xs.step <- (prop2.dpx[data.index[index[i]]] - prop1.spx[sample.index[i]]) / n.steps
                    ys.step <- (prop2.dpy[data.index[index[i]]] - prop1.spy[sample.index[i]]) / n.steps
                }
                points.xs[i] <- prop1.spx[sample.index[i]]
                points.ys[i] <- prop1.spy[sample.index[i]]
                points.cols[i] <- if (resample[i] == canvas$loi) "blue" else "red"
            } else {
                if (levels[index[i]] == ylevels[1]) {
                    xs.step <- (prop1.dpx[data.index[index[i]]] - prop2.spx[sample.index[i]]) / n.steps
                    ys.step <- (prop1.dpy[data.index[index[i]]] - prop2.spy[sample.index[i]]) / n.steps
                } else {
                    xs.step <- (prop2.dpx[data.index[index[i]]] - prop2.spx[sample.index[i]]) / n.steps
                    ys.step <- (prop2.dpy[data.index[index[i]]] - prop2.spy[sample.index[i]]) / n.steps
                }
                points.xs[i] <- prop2.spx[sample.index[i]]
                points.ys[i] <- prop2.spy[sample.index[i]]
                points.cols[i] <- if (resample[i] == canvas$loi) "blue" else "red"
            }

            for (j in 1:n.steps) {
                canvas$image <- editGrob(canvas$image, gPath("samplePlot.temp.datatext"),
                                         gp = gpar(fontface = 1))

                canvas$image <- addGrob(canvas$image, textGrob
                                        (label = c(resample[i], resample.levels[i]),
                                         y = y.text.start + unit(j*y.text.step, "npc"),
                                         x = unit(x.text.start + j*x.text.step, "npc"),
                                         just = "top", gp = gpar(col = "red", fontface = 2),
                                         name = "samplePlot.temp.text",
                                         vp = vpPath("canvas.all", "canvas.boxes")))
                # Drop point
                if (drop.points & animate.points) {
                    canvas$image <- addGrob(canvas$image, pointsGrob
                                            (x = if (resample.levels[i] == ylevels[1]) prop1.dpx[data.index[index[i]]] - j*xs.step
                                                 else prop2.dpx[data.index[index[i]]] - j*xs.step,
                                             y = if (resample.levels[i] == ylevels[1]) prop1.dpy[data.index[index[i]]] - j*ys.step
                                                 else prop2.dpy[data.index[index[i]]] - j*ys.step,
                                             pch = 19, gp = gpar(col = pointCols[i]),
                                             vp = canvas$graphPath("animation.field"), name = "samplePlot.temp"))
                }
                if (canvas$stopAnimation)
                    return()
                canvas$drawImage()
                if (j == n.steps & animate.points)
                    canvas$rmGrobs("samplePlot.temp")
            }

            # Make points permanent if dropping
            if (drop.points & animate.points)
                plotPoints(canvas, points.xs[1:i], points.ys[1:i], canvas$graphPath("animation.field"),
                           col = points.cols, name = "samplePlot", black = FALSE)
            if (canvas$stopAnimation)
                return()
            canvas$pauseImage(5)
            # Make text in resample databox permanent
            canvas$rmGrobs("samplePlot.temp.text")
            resamp.text1 <- textGrob(label = c("", resample[1:i]),
                                     x = unit(0.25, "npc"),
                                     y = yunit[1:(i + 1)], just = "top", gp = gpar(col = "red"),
                                     name = "databox.xtext.2", vp = canvas$graphPath("databox", 2))
            resamp.text2 <- textGrob(label = c("", resample.levels[1:i]),
                                     x = unit(0.75, "npc"),
                                     y = yunit[1:(i + 1)], just = "top", gp = gpar(col = "red"),
                                     name = "databox.ltext.2", vp = canvas$graphPath("databox", 2))
            resamp.text <- grobTree(datatexttitle, resamp.text1, resamp.text2, name = "samplePlot.resamp.text")
            canvas$image <- addGrob(canvas$image, resamp.text)
        }

        # Animation of fast points.
        if (n == ntext) length.out <- ntext - n.slow else length.out <- ntext - n.slow - 1
        for (i in seq(from = n.slow + 1, by = 1, length.out = length.out)) {
            y.text.start <- rep(yunit[min(c(max + 1, index[i] + 1))], 2)

            temp.text <- textGrob(label = c(resample[i], resample.levels[i]),
                                  y = y.text.start, x = x.text.start, just = "top",
                                  gp = gpar(col = "red", fontface = 2), name = "samplePlot.temp.datatext",
                                  vp = vpPath("canvas.all", "canvas.boxes"))
            resamp.text1 <- textGrob(label = c("", resample[1:i]),
                                     x = unit(0.25, "npc"),
                                     y = yunit[1:(i + 1)], just = "top", gp = gpar(col = "red"),
                                     name = "samplePlot.databox.xtext.2", vp = canvas$graphPath("databox", 2))
            resamp.text2 <- textGrob(label = c("", resample.levels[1:i]),
                                     x = unit(0.75, "npc"),
                                     y = yunit[1:(i + 1)], just = "top", gp = gpar(col = "red"),
                                     name = "samplePlot.databox.ltext.2", vp = canvas$graphPath("databox", 2))

            # Light up point to drop
            if (drop.points & animate.points) {
                temp.point <- pointsGrob(x = if (resample.levels[i] == ylevels[1]) prop1.dpx[data.index[index[i]]]
                                             else prop2.dpx[data.index[index[i]]],
                                         y = if (resample.levels[i] == ylevels[1]) prop1.dpy[data.index[index[i]]]
                                             else prop2.dpy[data.index[index[i]]],
                                         pch = 19, gp = gpar(col = pointCols[i]),
                                         vp = canvas$graphPath("animation.field"), name = "samplePlot.temp.point")
                canvas$image <- addGrob(canvas$image, temp.point)
            }

            # Because the position of the point is dependent on both the ylevel and the level
            # we need to work out the appropriate stepping from the data to the sample.
            if (resample.levels[i] == ylevels[1]) {
                if (levels[index[i]] == ylevels[1]) {
                    xs.step <- (prop1.dpx[data.index[index[i]]] - prop1.spx[sample.index[i]]) / n.steps
                    ys.step <- (prop1.dpy[data.index[index[i]]] - prop1.spy[sample.index[i]]) / n.steps
                } else {
                    xs.step <- (prop2.dpx[data.index[index[i]]] - prop1.spx[sample.index[i]]) / n.steps
                    ys.step <- (prop2.dpy[data.index[index[i]]] - prop1.spy[sample.index[i]]) / n.steps
                }
                points.xs[i] <- prop1.spx[sample.index[i]]
                points.ys[i] <- prop1.spy[sample.index[i]]
                points.cols[i] <- if (resample[i] == canvas$loi) "blue" else "red"
            } else {
                if (levels[index[i]] == ylevels[1]) {
                    xs.step <- (prop1.dpx[data.index[index[i]]] - prop2.spx[sample.index[i]]) / n.steps
                    ys.step <- (prop1.dpy[data.index[index[i]]] - prop2.spy[sample.index[i]]) / n.steps
                } else {
                    xs.step <- (prop2.dpx[data.index[index[i]]] - prop2.spx[sample.index[i]]) / n.steps
                    ys.step <- (prop2.dpy[data.index[index[i]]] - prop2.spy[sample.index[i]]) / n.steps
                }
                points.xs[i] <- prop2.spx[sample.index[i]]
                points.ys[i] <- prop2.spy[sample.index[i]]
                points.cols[i] <- if (resample[i] == canvas$loi) "blue" else "red"
            }

            resamp.text <- grobTree(datatexttitle, resamp.text1, resamp.text2, name = "samplePlot.resamp.text")
            canvas$image <- addGrob(canvas$image, temp.text)
            canvas$image <- addGrob(canvas$image, resamp.text)

            # Plot the additional point that has been sampled.
            if (drop.points & animate.points)
                plotPoints(canvas, points.xs[1:i], points.ys[1:i], canvas$graphPath("animation.field"),
                           col = points.cols, name = "samplePlot", black = FALSE)
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
            # Make text in resample databox permanent
            canvas$rmGrobs("samplePlot.temp.text")
        }

        # Animation of points outside databox.
        for (i in seq(from = ntext, by = 1, length.out = n - ntext)) {
            y.text.start <- rep(yunit[min(c(max + 1, index[i] + 1))], 2)
            temp.text <- textGrob(label = c(resample[i], resample.levels[i]),
                                  x = x.text.start, y = y.text.start, just = "top",
                                  gp = gpar(col = "red", fontface = 2), name = "samplePlot.temp.datatext",
                                  vp = vpPath("canvas.all", "canvas.boxes"))
            resamp.text1 <- textGrob(label = c("", resample[1:(ntext - 1)], "..."),
                                     x = unit(0.25, "npc"), y = yunit,
                                     just = "top", gp = gpar(col = "red"),
                                     name = "samplePlot.databox.xtext.2", vp = canvas$graphPath("databox", 2))
            resamp.text2 <- textGrob(label = c("", levels[1:(ntext - 1)], "..."),
                                     x = unit(0.75, "npc"), y = yunit,
                                     just = "top", gp = gpar(col = "red"),
                                     name = "samplePlot.databox.ltext.2", vp = canvas$graphPath("databox", 2))
            resamp.text <- grobTree(datatexttitle, resamp.text1, resamp.text2, name = "samplePlot.resamp.text")

            # Light up point to drop
            if (drop.points & animate.points) {
                temp.point <- pointsGrob(x = if (resample.levels[i] == ylevels[1]) prop1.dpx[data.index[index[i]]]
                                             else prop2.dpx[data.index[index[i]]],
                                         y = if (resample.levels[i] == ylevels[1]) prop1.dpy[data.index[index[i]]]
                                             else prop2.dpy[data.index[index[i]]],
                                         pch = 19, gp = gpar(col = pointCols[i]),
                                         vp = canvas$graphPath("animation.field"), name = "samplePlot.temp.point")
                canvas$image <- addGrob(canvas$image, temp.point)
            }

            canvas$image <- addGrob(canvas$image, temp.text)
            canvas$image <- addGrob(canvas$image, resamp.text)

            # Because the position of the point is dependent on both the ylevel and the level
            # we need to work out the appropriate stepping from the data to the sample.
            if (resample.levels[i] == ylevels[1]) {
                if (levels[index[i]] == ylevels[1]) {
                    xs.step <- (prop1.dpx[data.index[index[i]]] - prop1.spx[sample.index[i]]) / n.steps
                    ys.step <- (prop1.dpy[data.index[index[i]]] - prop1.spy[sample.index[i]]) / n.steps
                } else {
                    xs.step <- (prop2.dpx[data.index[index[i]]] - prop1.spx[sample.index[i]]) / n.steps
                    ys.step <- (prop2.dpy[data.index[index[i]]] - prop1.spy[sample.index[i]]) / n.steps
                }
                points.xs[i] <- prop1.spx[sample.index[i]]
                points.ys[i] <- prop1.spy[sample.index[i]]
                points.cols[i] <- if (resample[i] == canvas$loi) "blue" else "red"
            } else {
                if (levels[index[i]] == ylevels[1]) {
                    xs.step <- (prop1.dpx[data.index[index[i]]] - prop2.spx[sample.index[i]]) / n.steps
                    ys.step <- (prop1.dpy[data.index[index[i]]] - prop2.spy[sample.index[i]]) / n.steps
                } else {
                    xs.step <- (prop2.dpx[data.index[index[i]]] - prop2.spx[sample.index[i]]) / n.steps
                    ys.step <- (prop2.dpy[data.index[index[i]]] - prop2.spy[sample.index[i]]) / n.steps
                }
                points.xs[i] <- prop2.spx[sample.index[i]]
                points.ys[i] <- prop2.spy[sample.index[i]]
                points.cols[i] <- if (resample[i] == canvas$loi) "blue" else "red"
            }

            # Plot the additional point that has been sampled.
            if (drop.points & animate.points)
                plotPoints(canvas, points.xs[1:i], points.ys[1:i], canvas$graphPath("animation.field"),
                           col = points.cols, name = "samplePlot", black = FALSE)

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
    }

    # Removing animation grobs before plotting the sample
    # Because we're plotting within the animation.field VP, there is no vpNumber()
    # associated with our samplePlot.points, thus, we omit the numeric index
    canvas$rmGrobs(c("samplePlot.resamp.text", "samplePlot.temp.datatext",
                     "samplePlot.temp.point", "samplePlot.points.",
                     paste("samplePlot.yLab", 1:length(ylevels), sep="."),
                     paste("samplePlot.boundRect", 1:length(ylevels), sep="."),
                     paste("samplePlot.databoundRect", 1:length(ylevels), sep=".")))

    # Now that animation grobs have been removed, draw the sample
    canvas$plotSample(e, canvas$which.sample)
    canvas$showLabels()
    canvas$pauseImage(5)
}

plotBootPropDiffSample <- function(canvas, e, i, alpha = 0.2, points1000 = FALSE) {
    x <- canvas$samples[[i]]
    x[x != canvas$loi] <- canvas$loi.alt
    levels <- canvas$level.samples[[i]]
    ylevels <- canvas$ylevels
    props <- rev(calcPropDiff(x, levels, canvas)[ylevels])
    seekViewport(canvas$graphPath("sample", 1))
    max.str.width <- stringWidth(ylevels)
    max.str.width <- convertWidth(max(max.str.width), "npc", valueOnly = TRUE)
    upViewport(0)
    draw.points <- length(x) <= 100
    canvas$sampled.stats <- c(canvas$sampled.stats, canvas$which.sample)
    stats <- canvas$stat.dist[canvas$sampled.stats, , drop = FALSE]
    stats[, 1:2] <- stats[, 2:1]

    for (j in 1:length(ylevels)) {
        canvas$image <- addGrob(canvas$image,
                                rectGrob(x = unit(stats[, j], "native"),
                                         y = unit(if (j == 1) 0 else -0.1, "native"), height = unit(0.7, "native"),
                                         width = unit(0, "native"), just = c("left", "bottom"),
                                         gp = gpar(alpha = alpha, col = "blue", lwd = 2),
                                         vp = canvas$graphPath("sample", j),
                                         name = paste("samplePlot.ghosts", j, sep = ".")))
        # We add a blank white rect to obscure most of the ghosting lines
        # so that they are not visible through the propbar.
        canvas$image <- addGrob(canvas$image,
                                rectGrob(x = unit(0, "native"),
                                         y = unit(if (j == 1) 0.1 else 0, "native"),
                                         width = unit(1, "native"),
                                         height = unit(0.5, "native"),
                                         just = c("left", "bottom"),
                                         gp = gpar(fill = "white", col = "white"),
                                         vp = canvas$graphPath("sample", j),
                                         name = paste("samplePlot.blank", j, sep = ".")))
        canvas$image <- addGrob(canvas$image, kpropbarGrob
                                (x[levels == ylevels[j]],
                                 ylab = ylevels[j],
                                 ylab.pos = 1 - max.str.width,
                                 ylab.col = getColour(j, length(ylevels)),
                                 draw.points = if (points1000) FALSE else draw.points,
                                 lois = c(canvas$loi, canvas$loi.alt),
                                 y = if (j == 1) 0.1 else 0, height = 0.5,
                                 name = paste("samplePlot.propbar", j, sep="."),
                                 vp = canvas$graphPath("sample", j)))
    }

    # If we're not doing ANOVA, show the difference
    if (length(ylevels) == 2) {
        canvas$image <- addGrob(canvas$image, linesGrob
                                (x = unit(props, "native"),
                                 y = unit(0.8, "native"),
                                 gp = gpar(lwd = 2, col = "red"),
                                 arrow = arrow(length = unit(0.1, "inches")),
                                 vp = canvas$graphPath("sample", 1),
                                 name = "samplePlot.line.1"))
        # Filling sample databox.
        # To keep colours consistent with points
        # we need to order the cols parameters so that the group with the
        # lower statistic value is green.
        cols <- getColour(1:length(canvas$ylevels), length(canvas$ylevels))
        names(cols) <- canvas$ylevels
        cols <- cols[order(canvas$ylevels)]
        text <- as.character(levels[canvas$indexes[[i]]])
        # Just using canvas$samples here because it doesn't really make sense to
        # have the data with canvas$loi.alt in it.
        text.sample1 <- datatextGrob(data = canvas$samples[[i]], title = "", x = 0.25,
                                     max = 50, name = "text.sample1",
                                     vp = canvas$graphPath("databox", 2))
        text.sample2 <- coldatatextGrob(data = levels, title = "",
                                        cols = cols, xpos = 0.75,
                                        max = 50, name = "text.sample2",
                                        vp = canvas$graphPath("databox", 2))
        text.title <- datatextGrob(data = "", title = "Re-sample",
                                   name = "text.sampletitle", vp = canvas$graphPath("databox", 2))
        text.sample <- grobTree(text.sample1, text.sample2, text.title, name = "samplePlot.databox.text.2")
        canvas$image <- addGrob(canvas$image, text.sample)
    }
}

dropBootPropDiffArrow <- function(canvas, e, n.steps) {
    arrowbounds <- canvas$stat.dist[c(canvas$plotted.stats, canvas$which.sample), 1:2, drop = FALSE]
    stats <- apply(arrowbounds, 1, diff)
    curr.arrow <- arrowbounds[length(stats), ]
    curr.stat <- stats[length(stats)]
    y.max <- unit(1, "npc") - unit(2, "lines") - unit(0.5, "char")
    y <- stackPoints(stats, vp = canvas$graphPath("stat"), y.min = 0, y.max = y.max)
    # Need to find out the location of the value of native
    # units relative to the animation.field viewport
    depth <- downViewport(canvas$graphPath("stat"))
    x.zero <- convertX(unit(0, "native"), "npc", valueOnly = TRUE)
    upViewport(depth)
    depth <- downViewport(canvas$graphPath("sample"))
    curr.arrow <- convertX(unit(curr.arrow, "native"), "npc", valueOnly = TRUE)
    upViewport(depth)

    y.start <- 1 + (0.8 / 2)
    y.end <- y[length(stats)]
    y.step <- (y.start - y.end)/n.steps
    xs.start <- curr.arrow
    xs.end <- c(x.zero, x.zero + diff(curr.arrow))
    xs.step <- (xs.start - xs.end)/n.steps
    for (i in 0:n.steps) {
        temp.arrow <- linesGrob(x = unit(xs.start - i*xs.step, "npc"),
                                y = unit(rep(y.start - i*y.step, 2), "native"),
                                gp = gpar(lwd = 2, col = "red"),
                                arrow = arrow(length = unit(0.1, "inches")),
                                vp = canvas$graphPath("animation.field"),
                                name = "samplePlot.temp.arrow")
        canvas$image <- addGrob(canvas$image, temp.arrow)
        if (canvas$stopAnimation)
            return()
        canvas$drawImage()
    }
    canvas$image <- removeGrob(canvas$image, gPath("samplePlot.temp.arrow"))
}

trackBootstrapPropDiff <- function(canvas) {
    if (canvas$n < 100) {
        index <- canvas$indexes[[canvas$which.sample]]
        sample <- canvas$x
        levels <- as.character(canvas$levels)
        resample <- sample[index]
        resample.levels <- levels[index]
        ylevels <- canvas$ylevels
        dataProp1 <- getGrob(canvas$image, gPath("dataPlot.propbar.1"))
        dataProp2 <- getGrob(canvas$image, gPath("dataPlot.propbar.2"))
        drop.points <- dataProp1$points.present & dataProp2$points.present

        pointCols <- ifelse(levels == ylevels[1],
                            ifelse(sample == canvas$loi,
                                   getColour(1, 2, l = 85),
                                   getColour(1, 2, l = 45)),
                            ifelse(sample == canvas$loi,
                                   getColour(2, 2, l = 85),
                                   getColour(2, 2, l = 45)))

        if (drop.points) {
            # Need to dive into each VP and grab the size of the units relative
            # animation.field VP. Because we're dealing with two data VPs, each
            # will be scaled by a half.
            depth <- downViewport(canvas$graphPath("data", 1))
            prop1.dpx <- convertX(dataProp1$x, "native")
            prop1.dpy <- convertY(dataProp1$y, "native", valueOnly = TRUE) / length(ylevels)
            upViewport(depth)
            depth <- downViewport(canvas$graphPath("data", 2))
            prop2.dpx <- convertX(dataProp2$x, "native")
            prop2.dpy <- convertY(dataProp2$y, "native", valueOnly = TRUE) / length(ylevels)
            upViewport(depth)

            # Creating sample propbars and grabbing the point locations
            # from each of the propbars.
            sampleProp1 <- kpropbarGrob(resample[resample.levels == ylevels[1]],
                                        ylab = ylevels[1],
                                        draw.points = drop.points,
                                        lois = c(canvas$loi, canvas$loi.alt),
                                        y = 0.1, height = 0.5,
                                        name = paste("samplePlot.propbar", 1, sep="."),
                                        vp = canvas$graphPath("sample", 1))
            sampleProp2 <- kpropbarGrob(resample[resample.levels == ylevels[2]],
                                        ylab = ylevels[2],
                                        draw.points = drop.points,
                                        lois = c(canvas$loi, canvas$loi.alt),
                                        y = 0, height = 0.5,
                                        name = paste("samplePlot.propbar", 2, sep="."),
                                        vp = canvas$graphPath("sample", 2))
            depth <- downViewport(canvas$graphPath("sample", 1))
            prop1.spx <- convertX(sampleProp1$x, "native")
            prop1.spy <- convertY(sampleProp1$y, "native", valueOnly = TRUE) / length(ylevels)
            upViewport(depth)
            depth <- downViewport(canvas$graphPath("sample", 2))
            prop2.spx <- convertX(sampleProp2$x, "native")
            prop2.spy <- convertY(sampleProp2$y, "native", valueOnly = TRUE) / length(ylevels)
            upViewport(depth)

            # Need to translate the coordinates so that they fit in the
            # animation.field viewport
            prop1.dpy <- unit(prop1.dpy + 2, "native")
            prop2.dpy <- unit(prop2.dpy + 2.5, "native")
            prop1.spy <- unit(prop1.spy + 1, "native")
            prop2.spy <- unit(prop2.spy + 1.5, "native")
        }

        n <- canvas$n
        first.group <- levels == ylevels[1]
        ntext <- min(n, 50)
        npcs <- (ntext:0)/ntext
        yunit <- (unit(npcs, "npc") - unit(4*(npcs - 0.5), "mm") + unit(1 - npcs, "lines"))
        max.width.x <- max(convertX(stringWidth(sample),
                                    "cm", valueOnly = TRUE))
        max.width.y <- max(convertX(stringWidth(ylevels),
                                    "cm", valueOnly = TRUE))
        max.width.x <- unit(max.width.x, "cm")
        max.width.y <- unit(max.width.y, "cm")

        if (drop.points) {
            # Calculating a index vectors so that the i'th
            # value corresponds to the correct location within
            # each of the data propbars.
            first.level.ind <- 1
            second.level.ind <- 1
            data.index <- integer(length(levels))
            for (i in 1:length(levels)) {
                if (levels[i] == ylevels[1]) {
                    data.index[i] <- first.level.ind
                    first.level.ind <- first.level.ind + 1
                } else {
                    data.index[i] <- second.level.ind
                    second.level.ind <- second.level.ind + 1
                }
            }

            first.level.ind <- 1
            second.level.ind <- 1
            sample.index <- integer(length(levels))
            for (i in 1:length(resample.levels)) {
                if (resample.levels[i] == ylevels[1]) {
                    sample.index[i] <- first.level.ind
                    first.level.ind <- first.level.ind + 1
                } else {
                    sample.index[i] <- second.level.ind
                    second.level.ind <- second.level.ind + 1
                }
            }
        }

        first.group <- levels == ylevels[1]
        finalobs <- if (n <= 50) n else 49
        for (i in 1:finalobs) {
            # Current data values
            value <- sample[i]
            lev <- levels[i]
            # Current values from sample
            resample.inds <- index == i
            resamp.first.group <- lev == ylevels[1]

            canvas$image <- addGrob(canvas$image, textGrob
                                    (label = value,
                                     x = 0.25, y = yunit[i + 1], just = "top",
                                     gp = gpar(col = "red", fontface = 2), name = "samplePlot.temp.samp",
                                     vp = canvas$graphPath("databox", 1)))
            canvas$image <- addGrob(canvas$image, textGrob
                                    (label = lev,
                                     x = 0.75, y = yunit[i + 1], just = "top",
                                     gp = gpar(col = "red", fontface = 2), name = "samplePlot.temp.level",
                                     vp = canvas$graphPath("databox", 1)))
            if (drop.points)
                canvas$image <- addGrob(canvas$image, pointsGrob
                                        (x = if (first.group[i]) prop1.dpx[data.index[i]]
                                             else prop2.dpx[data.index[i]],
                                         y = if (first.group[i]) prop1.dpy[data.index[i]]
                                             else prop2.dpy[data.index[i]],
                                         pch = 19, gp = gpar(col = pointCols[i]),
                                         name = "samplePlot.temp.point", vp = canvas$graphPath("animation.field")))

            resamp.ys <- c(FALSE, resample.inds)
            subset.resamp <- c(resamp.ys[1:(ntext + 1)])
            if (any(resample.inds)) {
                subset.resamp <- c(resamp.ys[1:(ntext + 1)])
                if (n != ntext) subset.resamp <- c(resamp.ys[1:ntext], any(resamp.ys[(ntext + 1):(n + 1)]))
                canvas$image <- addGrob(canvas$image, textGrob
                                        (label = value,
                                         x = 0.25, y = yunit[subset.resamp], just = "top",
                                         gp = gpar(col = "red", fontface = 2), name = "samplePlot.temp.resamp",
                                         vp = canvas$graphPath("databox", 2)))
                canvas$image <- addGrob(canvas$image, textGrob
                                        (label = lev, x = 0.75,
                                         y = yunit[subset.resamp], just = "top",
                                         gp = gpar(col = "red", fontface = 2),
                                         name = "samplePlot.temp.relevel",
                                         vp = canvas$graphPath("databox", 2)))
                canvas$image <- addGrob(canvas$image, segmentsGrob
                                        (x0 = unit(0.375, "npc") + max.width.y*0.5 + unit(1, "mm"),
                                         x1 = unit(0.625, "npc") - max.width.x*0.5 - unit(1, "mm"),
                                         y0 = yunit[i + 1] - unit(0.5, "lines"),
                                         y1 = yunit[subset.resamp] - unit(0.5, "lines"),
                                         name = "samplePlot.temp.segments", gp = gpar(lwd = 2, col = "red"),
                                         vp = vpPath("canvas.all", "canvas.boxes")))
                if (drop.points)
                    canvas$image <- addGrob(canvas$image, pointsGrob
                                            (x = if (resamp.first.group) prop1.spx[sample.index[resample.inds]]
                                                 else prop2.spx[sample.index[resample.inds]],
                                             y = if (resamp.first.group) prop1.spy[sample.index[resample.inds]]
                                                 else prop2.spy[sample.index[resample.inds]],
                                             pch = 19,
                                             gp = gpar(col = pointCols[i]), name = "samplePlot.temp.resamplepoints",
                                             vp = canvas$graphPath("animation.field")))
                if (canvas$stopAnimation)
                    return()
                canvas$pauseImage(5)
                canvas$rmGrobs(c("samplePlot.temp.resamp", "samplePlot.temp.relevel",
                                 "samplePlot.temp.segments", "samplePlot.temp.resamplepoints"))
            } else {
                if (canvas$stopAnimation)
                    return()
                canvas$pauseImage(5)
            }
        }
        canvas$rmGrobs(c("samplePlot.temp.samp", "samplePlot.temp.level"))
        if (drop.points)
            canvas$image <- removeGrob(canvas$image, gPath("samplePlot.temp.point"))
        canvas$image <- addGrob(canvas$image, datatextGrob
                                (data = resample, title = "Re-sample", max = 50, gp = gpar(col = "red"),
                                 name = "samplePlot.databox.text.2", vp = canvas$graphPath("databox", 2)))
    }
}
