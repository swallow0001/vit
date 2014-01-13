canvasSampvarMeanDiff <- setRefClass("canvasSampvarMeanDiffClass", contains = "canvasPlotClass",
                                     methods = list(
    calcStat = function(i = which.sample, ys = NULL, canvas = .self) {
        if (stat.method == "difference") {
            if (all(ylevels == sort(unique(ys)))) {
                calcDiff(samples[[i]], ys, fun = mean)
            } else {
                rev(calcDiff(samples[[i]], ys, fun = mean))
            }
        } else if (stat.method == "t-pooled") {
            if (all(ylevels == sort(unique(ys)))) {
                -calcT(samples[[i]], ys, fun = mean)
            } else {
                calcT(samples[[i]], ys, fun = mean)
            }
        } else if (stat.method == "t-Welch") {
            if (all(ylevels == sort(unique(ys)))) {
                -calcTWelch(samples[[i]], ys, fun = mean)
            } else {
                calcTWelch(samples[[i]], ys, fun = mean)
            }
        } else {
            notYetImplemented()
        }
    },

    calcAllStats = function(xs, ys = NULL, canvas = .self) {
        if (stat.method == "difference") {
            if (all(ylevels == sort(unique(ys)))) {
                calcDiff(xs, ys, fun = mean)
            } else {
                rev(calcDiff(xs, ys, fun = mean))
            }
        } else if (stat.method == "t-pooled") {
            if (all(ylevels == sort(unique(ys)))) {
                -calcT(as.numeric(xs), ys, fun = mean)
            } else {
                calcT(as.numeric(xs), ys, fun = mean)
            }
        } else if (stat.method == "t-Welch") {
            if (all(ylevels == sort(unique(ys)))) {
                -calcTWelch(as.numeric(xs), ys, fun = mean)
            } else {
                calcTWelch(as.numeric(xs), ys, fun = mean)
            }
        } else {
            notYetImplemented()
        }
    },

    animateStat = function(env, n.steps) {
        if (stat.method == "difference") {
            dropBootArrow(.self, env, n.steps)
        } else if (stat.method %in% c("t-pooled", "t-Welch")) {
            animateTStat(.self, env, n.steps)
        } else {
            notYetImplemented()
        }
    },

    plotSample = function(env, i = which.sample, ...) {
        plotSampvarLevels(.self, i, env, ...)
    },

    showLabels = function() {
        sampvarDiffLabels(.self)
    },

    fadePlots = function(env, ...) {
        fadeDataTwoSample(.self, env)
    },

    plotDataStat = function(env, ...) {
        if (!(stat.method %in% c("t-pooled", "t-Welch"))){
            showStatDiffSampvarDiff(.self, env)
        }
    },

    plotStatDist = function(env, ...) {
        plotBootDiffDist(.self, env)
    },

    animateSample = function(e, n.steps, n.slow, opts) {
        dropSampvarPointsDiff(.self, e, n.steps, n.slow)
    },

    handle1000 = function(env, ...) {
        sampvarDiff1000(.self, env, ...)
    },

    displayResult = function(env, ...) {
        plotTheoDistTwoSample(env)
    }))

load_sampvar_mean_diff <- function(e) {
    e$c1$stat.in.use <- svalue(e$stat)
    e$sampvar.method <- svalue(e$sampvar.choices)
    e$c1$stat.method <- e$sampvar.method
    # There is something messy going on with viewports and
    # gTrees, such that when attempting to import them, we
    # end up with "uninitializedField"s. Assign to a temp
    # var and reassign later.
    tmp.vps <- e$c1$viewports
    tmp.image <- e$c1$image
    e$c1$viewports <- NULL
    e$c1$image <- NULL
    tmp.canvas <- canvasSampvarMeanDiff$new()
    tmp.canvas$import(e$c1)
    e$c1 <- tmp.canvas
    e$c1$viewports <- tmp.vps
    e$c1$image <- tmp.image
    e$difference <- TRUE
}

showStatDiffSampvarDiff <- function(canvas, e){
    dataDiffArrowMean(canvas, e)
    x <- canvas$x
    levels <- canvas$levels
    ylevels <- canvas$ylevels
    mean1 <- mean(x[levels == ylevels[1]])
    mean2 <- mean(x[levels != ylevels[1]])
    diff <- diff(c(mean2, mean1))
    statline <- linesGrob(x = unit(rep(diff, 2), "native"),
                          y = unit(c(0, 1), "npc") - unit(c(2, 0), "lines"),
                          gp = gpar(lty = "dashed", lwd = 2),
                          vp = canvas$graphPath("stat"),
                          name = "statline.1")
    stattext <- textGrob(label = format(round(diff, canvas$dp), nsmall = canvas$dp),
                         x = unit(diff, "native"),
                         y = unit(0, "npc") - unit(2, "lines"),
                         just = "top", gp = gpar(fontface = 2, col = "red"),
                         vp = canvas$graphPath("stat"), name = "stattext.1")
    canvas$image <- addGrob(canvas$image, statline)
    canvas$image <- addGrob(canvas$image, stattext)
}


sampvarDiff1000 <- function(canvas, e, points) {
    canvas$rmGrobs("statPlot.theodist.1")
    stats <- apply(canvas$stat.dist, 1, diff)
    for (i in 20*(1:50)){
        canvas$sampled.stats <- 1:i
        canvas$plotSample(e, i, alpha = 0.05)
        if (points) {
            x <- stats[1:i]
            y <- canvas$stat.ypos[1:i]
            plotPoints(canvas, x, y, canvas$graphPath("stat"),
                       "statPlot", black = FALSE, alpha = 0.7)
        }
        if (canvas$stopAnimation)
            return()
        canvas$showLabels()
        canvas$drawImage()
    }
    canvas$rmGrobs(c("samplePlot.stat.2", "samplePlot.temp.data.points.",
                     "samplePlot.datapoints"))
    for (i in 1:2){
        canvas$rmGrobs(paste("samplePlot", c("line", "points", "boxplot", "propbar", "blank"), i, sep = "."))
    }
    canvas$drawImage()
    canvas$sampled.stats <- NULL
    canvas$plotted.stats <- NULL
}

dropSampvarPointsDiff <- function(canvas, e, n.steps = 10, n.slow = 5, pause = 10) {
    canvas$rmGrobs(c("samplePlot.stat.2", "samplePlot.labelrect.1", "statPlot.theodist.1",
                     "samplePlot.labeltext.1", "samplePlot.temp.data.points."))
    canvas$rmGrobs(paste("samplePlot", c("points", "lines", "line", "boxplot", "text"),
                         rep(1:2, each = 5), sep = "."))
    index <- canvas$indexes[[canvas$which.sample]]
    x <- canvas$x[index]
    levels <- as.character(canvas$levels[index])
    y <- 0.5*canvas$y[index] + 0.5*(levels == canvas$ylevels[2])
    n <- canvas$n
    if (n < 100) {
        y.start <- y + 2
        y.end <- 0.5*old.stackPoints(x, levels = levels, vp = canvas$graphPath("sample")) + 1 +
            0.5*(levels == canvas$ylevels[2])
        y.step <- (y.end - y.start)/n.steps

        sampSelectLabel <- textGrob("Selecting sample...",
                                    x = unit(0.5, "npc"), y = unit(1.4, "native"),
                                    vp = canvas$graphPath("animation.field"),
                                    gp = gpar(fontface = 2),
                                    name = "samplePlot.sampSelectLabel")
        canvas$image <- addGrob(canvas$image, sampSelectLabel)
        cols <- character(n)
        cols[levels == canvas$ylevels[1]] <- getColour(1, 2, l = 35)
        cols[levels == canvas$ylevels[2]] <- getColour(2, 2, l = 35)
        ## Animation of slow points.
        for (i in seq(from = 1, by = 1, length.out = n.slow)) {
            ## Light up point to drop
            temp.point <- pointsGrob(x = x[1:i], y = y.start[1:i], pch = 19,
                                     vp = canvas$graphPath("animation.field"),
                                     gp = gpar(col = cols[1:i]),
                                     name = "samplePlot.temp.point")
            canvas$image <- addGrob(canvas$image, temp.point)

            if (canvas$stopAnimation)
                return()
            if (!e$fade){
                canvas$image <- addGrob(canvas$image, getGrob(canvas$image, gPath("fadebox1")))
                canvas$image <- addGrob(canvas$image, getGrob(canvas$image, gPath("fadebox2")))
                canvas$image <- addGrob(canvas$image, getGrob(canvas$image, gPath("sampvarlabels")))
                canvas$image <- addGrob(canvas$image, getGrob(canvas$image, gPath("dataAxis")))
            }
            canvas$pauseImage(10)
        }

        ## Animation of fast points.
        length.out <- if (n <= n.slow) 0 else n - n.slow
        for (i in seq(from = n.slow + 1, by = 1, length.out = length.out)){
            temp.point <- pointsGrob(x = x[1:i], y = y.start[1:i], pch = 19,
                                     vp = canvas$graphPath("animation.field"),
                                     gp = gpar(col = cols[1:i]),
                                     name = "samplePlot.temp.point")
            canvas$image <- addGrob(canvas$image, temp.point)

            if (canvas$stopAnimation)
                return()
            if (!e$fade){
                canvas$image <- addGrob(canvas$image, getGrob(canvas$image, gPath("fadebox1")))
                canvas$image <- addGrob(canvas$image, getGrob(canvas$image, gPath("fadebox2")))
                canvas$image <- addGrob(canvas$image, getGrob(canvas$image, gPath("sampvarlabels")))
                canvas$image <- addGrob(canvas$image, getGrob(canvas$image, gPath("dataAxis")))
            }
            canvas$drawImage()
        }

        # Dropping the points
        for (i in 1:n.steps) {
            temp.point <- pointsGrob(x = unit(x, "native"),
                                     y = unit(y.start + (y.step * i), "native"),
                                     pch = 19,
                                     vp = canvas$graphPath("animation.field"),
                                     gp = gpar(col = cols),
                                     name = "samplePlot.temp.point.2")
            canvas$image <- addGrob(canvas$image, temp.point)
            canvas$showLabels()
            if (canvas$stopAnimation)
                return()
            if (!e$fade){
                canvas$image <- addGrob(canvas$image, getGrob(canvas$image, gPath("fadebox1")))
                canvas$image <- addGrob(canvas$image, getGrob(canvas$image, gPath("fadebox2")))
                canvas$image <- addGrob(canvas$image, getGrob(canvas$image, gPath("sampvarlabels")))
                canvas$image <- addGrob(canvas$image, getGrob(canvas$image, gPath("dataAxis")))
            }
            canvas$drawImage()
        }
        canvas$rmGrobs(c("samplePlot.sampSelectLabel"))
        if (!e$fade){
            canvas$image <- addGrob(canvas$image, getGrob(canvas$image, gPath("fadebox1")))
            canvas$image <- addGrob(canvas$image, getGrob(canvas$image, gPath("fadebox2")))
            canvas$image <- addGrob(canvas$image, getGrob(canvas$image, gPath("sampvarlabels")))
            canvas$image <- addGrob(canvas$image, getGrob(canvas$image, gPath("dataAxis")))
        }
        canvas$pauseImage(10)

        canvas$rmGrobs(c("samplePlot.temp.point", "samplePlot.temp.point.2", "samplePlot.points."))
        canvas$plotSample(e, canvas$which.sample)
        canvas$showLabels()
        canvas$pauseImage(pause)
    }
}

plotSampvarLevels <- function(canvas, i, e, alpha = 0.25) {
    canvas$rmGrobs(c("samplePlot.temp.data.points.", "statPlot.theodist.1"))
    index <- canvas$indexes[[i]]
    x <- canvas$x[index]
    levels <- canvas$levels[index]
    ylevels <- canvas$ylevels
    y <- old.stackPoints(x, levels, vp = canvas$graphPath("sample"))
    cols <- character(canvas$n)
    cols[levels == canvas$ylevels[1]] <- getColour(1, 2, l = 35)
    cols[levels == canvas$ylevels[2]] <- getColour(2, 2, l = 35)
    ## Plotting samples, labels and ghosts
    for (j in 1:length(ylevels)) {
        plotPoints(canvas, x[levels == ylevels[j]],
                   y[levels == ylevels[j]],
                   col = getColour(j, length(ylevels)),
                   vp = canvas$graphPath("sample", j),
                   name = "samplePlot")
        canvas$image <- addGrob(canvas$image, linesGrob
                                (x = unit(rep(mean(x[levels == ylevels[j]]), 2),
                                 "native"),
                                 y = unit(c(0.1, 0.55), "npc"),
                                 gp = gpar(lwd = 3, col = "blue"),
                                 vp = canvas$graphPath("sample", j),
                                 name = paste("samplePlot", "line", j, sep = ".")))
        canvas$image <- addGrob(canvas$image, textGrob
                                (ylevels[j], x = 1, y = unit(1, "mm"), just = c("right", "bottom"),
                                 vp = canvas$graphPath("sample", j),
                                 name = paste("samplePlot.text", j, sep = ".")))
    }

    # Plotting data points
    data.y <- 2 + 0.5*canvas$y[index] + 0.5*(levels == canvas$ylevels[2])
    canvas$image <- addGrob(canvas$image,
                            pointsGrob(x = x, y = data.y, vp = canvas$graphPath("animation.field"),
                                       name = "samplePlot.temp.point",
                                       gp = gpar(col = cols), pch = 19))
    ## Plotting arrow difference
    canvas$image <- addGrob(canvas$image, linesGrob(x = unit(c(mean(x[levels == ylevels[2]]),
                                                               mean(x[levels == ylevels[1]])),
                                                    "native"),
                                                    y = unit(0.8, "npc"),
                                                    gp = gpar(lwd = 2, col = "red"),
                                                    arrow = arrow(length = unit(0.1, "inches")),
                                                    vp = canvas$graphPath("sample", 1),
                                                    name = "samplePlot.stat.2"))
    if (!e$fade){
        canvas$image <- addGrob(canvas$image, getGrob(canvas$image, gPath("fadebox1")))
        canvas$image <- addGrob(canvas$image, getGrob(canvas$image, gPath("fadebox2")))
        canvas$image <- addGrob(canvas$image, getGrob(canvas$image, gPath("sampvarlabels")))
        canvas$image <- addGrob(canvas$image, getGrob(canvas$image, gPath("dataAxis")))
    }
}

sampvarDiffLabels <- function(canvas) {
    poplabel <- textGrob("Population",
                          x = unit(0, "npc") + unit(1, "mm"),
                          y = unit(1, "npc") - unit(1, "mm"),
                          just = c("left", "top"),
                          name = "dataLabel",
                          vp = canvas$graphPath("data", 2),
                          gp = gpar(fontface = 2))
    methlabel <- textGrob("Module: 2 Sample Sampling Variation",
                          x = unit(0, "npc"),
                          just = "left",
                          gp = gpar(fontsize = 10, fontface = "italic"),
                          name = "methodLabel",
                          vp = canvas$graphPath("canvas.header"))
    if (is.categorical(canvas$x)) {
        vlabels <- c("Variable: ", canvas$x.name, " (",
                     canvas$loi, " | ",
                     canvas$loi.alt, ")")
        vlabelXs <- unit(0, "npc")
        for (i in 1:(length(vlabels) - 1))
            vlabelXs <- unit.c(vlabelXs, vlabelXs[i] + stringWidth(vlabels[i]))
        varlabel <- textGrob(vlabels,
                             x = vlabelXs + stringWidth(methlabel$label) + unit(6, "mm"),
                             just = "left",
                             gp = gpar(col = c(rep("black", 3), "blue", "black", "red", "black"),
                                       fontsize = 10, fontface = "italic"),
                             name = "varLabel",
                             vp = canvas$graphPath("canvas.header"))
    } else {
        varlabel <- textGrob(paste("Variable:", canvas$x.name),
                             x = unit(0, "npc") + stringWidth(methlabel$label) + unit(6, "mm"),
                             just = "left",
                             gp = gpar(fontsize = 10, fontface = "italic"),
                             name = "varLabel",
                             vp = canvas$graphPath("canvas.header"))
    }
    quantitylabel <- textGrob(paste("Quantity:", canvas$stat.in.use),
                              x = varlabel$x[1] + stringWidth(paste(varlabel$label, collapse = "")) + unit(6, "mm"),
                              just = "left",
                              name = "quantityLabel",
                              gp = gpar(fontsize = 10, fontface = "italic"),
                              vp = canvas$graphPath("canvas.header"))
    if (! is.null(canvas$stat.method)) {
        quantmethlabel <- textGrob(paste("Statistic:", canvas$stat.method),
                                   x = quantitylabel$x + stringWidth(quantitylabel$label) + unit(6, "mm"),
                                   just = "left",
                                   name = "quantmethLabel",
                                   gp = gpar(fontsize = 10, fontface = "italic"),
                                   vp = canvas$graphPath("canvas.header"))
        filelabel <- textGrob(paste("File:", canvas$data.file),
                              x = quantmethlabel$x + stringWidth(quantmethlabel$label) + unit(6, "mm"),
                              just = "left",
                              name = "fileLabel",
                              gp = gpar(fontsize = 10, fontface = "italic"),
                              vp = canvas$graphPath("canvas.header"))
    } else {
        filelabel <- textGrob(paste("File:", canvas$data.file),
                              x = quantitylabel$x + stringWidth(quantitylabel$label) + unit(6, "mm"),
                              just = "left",
                              name = "fileLabel",
                              gp = gpar(fontsize = 10, fontface = "italic"),
                              vp = canvas$graphPath("canvas.header"))
    }
    infosep <- linesGrob(x = unit(0:1, "npc"), y = unit(0, "npc"),
                         name = "infoSeparatorLine",
                         vp = canvas$graphPath("canvas.header"))
    samplabel <- textGrob("Sample",
                            x = unit(0, "npc") + unit(1, "mm"),
                            y = unit(0.6, "npc"),
                            just = c("left", "top"),
                            name = "sampleLabel",
                            vp = canvas$graphPath("sample", 2),
                            gp = gpar(fontface = 2))
    dataLabelBackground <- rectGrob(x = unit(0, "npc"),
                                    y = unit(1, "npc") - unit(1, "lines") - unit(1, "mm"),
                                    width = stringWidth(samplabel$label) + unit(2, "mm"),
                                    height = stringHeight(samplabel$label) + unit(2, "mm"),
                                    just = c("left", "bottom"),
                                    gp = gpar(col = "white", fill = "white", alpha = 0.7),
                                    name = "dataLabelBackground",
                                    vp = canvas$graphPath("data", 2))
    sampLabelBackground <- rectGrob(x = unit(0, "npc"),
                                    y = unit(0.6, "npc") + unit(0.5, "lines") - unit(1, "mm"),
                                    width = stringWidth(samplabel$label) + unit(4, "mm"),
                                    height = stringHeight(samplabel$label) + unit(2, "mm"),
                                    just = c("left", "top"),
                                    gp = gpar(col = "white", fill = "white", alpha = 0.7),
                                    name = "resampLabelBackground",
                                    vp = canvas$graphPath("sample", 2))
    statlabel <- textGrob("Sampling distribution",
                          x = unit(0, "npc") + unit(1, "mm"),
                          y = unit(0.8, "npc"),
                          just = c("left", "top"),
                          name = "statLabel",
                          vp = canvas$graphPath("stat"),
                          gp = gpar(fontface = 2))
    if (! is.null(canvas$stat.method)) {
        sampvarlabels <- grobTree(dataLabelBackground,
                                  poplabel,
                                  methlabel, varlabel, quantitylabel, quantmethlabel, filelabel, infosep,
                                  sampLabelBackground,
                                  samplabel, statlabel,
                                  name = "sampvarlabels")
    } else {
        sampvarlabels <- grobTree(dataLabelBackground,
                                  poplabel,
                                  methlabel, varlabel, quantitylabel, filelabel, infosep,
                                  sampLabelBackground,
                                  samplabel, statlabel,
                                  name = "sampvarlabels")
    }
    canvas$image <- addGrob(canvas$image, sampvarlabels)
}

fadeDataTwoSample <- function(canvas, e){
    if (e$fade){
        canvas$image <- addGrob(canvas$image, rectGrob
                                (vp = canvas$graphPath("data", 1),
                                 width = unit(1, "npc") + unit(1, "char"),
                                 height = unit(1, "npc"),
                                 gp = gpar(col = "white", fill = "white", alpha = 0.9),
                                 name = "fadebox1"))
        canvas$image <- addGrob(canvas$image, rectGrob
                                (y = 0, just = "bottom", vp = canvas$graphPath("data", 2),
                                 width = unit(1, "npc") + unit(1, "char"),
                                 height = unit(1, "npc") + unit(0.5, "char"),
                                 gp = gpar(col = "white", fill = "white", alpha = 0.9),
                                 name = "fadebox2"))
        canvas$image <- addGrob(canvas$image, getGrob(canvas$image, gPath("sampvarlabels")))
        canvas$image <- addGrob(canvas$image, getGrob(canvas$image, gPath("dataAxis")))
        e$fade <- FALSE
    } else {
        canvas$rmGrobs(c("fadebox1", "fadebox2"))
        e$fade <- TRUE
    }
    canvas$drawImage()
}

plotTheoDistTwoSample <- function(e){
    canvas <- e$c1
    ## Replotting statistic distribution
    x <- apply(canvas$stat.dist, 1, diff)
    y <- canvas$stat.ypos
    plotPoints(canvas, x, y, canvas$graphPath("stat"),
               "statPlot", black = FALSE, alpha = 0.7)
    mean1 <- mean(canvas$x[canvas$levels == canvas$ylevels[1]])
    mean2 <- mean(canvas$x[canvas$levels == canvas$ylevels[2]])
    sd1 <- pop.sd(canvas$x[canvas$levels == canvas$ylevels[1]])/
        sqrt((canvas$n/length(canvas$x))*sum(canvas$levels == canvas$ylevels[1]))
    sd2 <- pop.sd(canvas$x[canvas$levels == canvas$ylevels[2]])/
        sqrt((canvas$n/length(canvas$x))*sum(canvas$levels == canvas$ylevels[2]))
    mean <- mean1 - mean2
    sd <- sqrt(sd1^2 + sd2^2)
    ## Getting statistic panel x-scale.
    top.level <- downViewport(canvas$graphPath("stat"))
    stat.scale <- current.viewport()$xscale
    upViewport(top.level)
    ## Calculating normal density under the CLT.
    xs <- seq(stat.scale[1], stat.scale[2], length.out = 300)
    ys <- dnorm(xs, mean, sd)
    ## We need a sense of "density scale" for the y-axis. Fitting a
    ## kernel density estimator can provide this. We calculate the
    ## KDE, find the maximum point, map this to meet up with the top
    ## of the topmost point in the statistic panel, and scale the
    ## normal density curve accordingly. This ensures that the normal
    ## density curve has about the same area below it as the area
    ## taken up by the points; the normal density will have the same
    ## area as the KDE, which, in turn, will have a similar area to
    ## the points.
    dens <- density(x, from = stat.scale[1], to = stat.scale[2])
    stat.ys <- dens$y/max(dens$y)
    stat.xs <- dens$x
    ys <- ys/max(dens$y)
    y.max <- unit(1, "npc") - unit(2, "lines") - unit(0.5, "char")
    canvas$image <- addGrob(canvas$image, linesGrob
                            (x = unit(xs, "native"), y = (y.max*ys) + ys*unit(0.5, "char"),
                             gp = gpar(lwd = 2, col = "red"),
                             name = "statPlot.theodist.1",
                             vp = canvas$graphPath("stat")))
    canvas$showLabels()
    canvas$drawImage()
    canvas$rmGrobs(c("samplePlot.points.1", "samplePlot.lines.1",
                     "samplePlot.datapoints.points.1", "samplePlot.databox.text.2",
                     "samplePlot.stat.1", "samplePlot.hist.1"))
}
