canvasPermMeanDiff <- setRefClass("canvasPermMeanDiffClass", contains = "canvasPlotClass",
                                  methods = list(
    calcStat = function(i = which.sample, ys = NULL, canvas = .self) {
        if (stat.method == "difference") {
            if (all(ylevels == sort(unique(ys)))) {
                calcDiff(samples[[i]], ys, fun = mean)
            } else {
                rev(calcDiff(samples[[i]], ys, fun = mean))
            }
        } else if (stat.method == "t-pooled") {
            if (all(ylevels != sort(unique(ys)))) {
                -calcT(samples[[i]], ys, fun = mean)
            } else {
                calcT(samples[[i]], ys, fun = mean)
            }
        } else if (stat.method == "t-Welch") {
            if (all(ylevels != sort(unique(ys)))) {
                -calcTWelch(samples[[i]], ys, fun = mean)
            } else {
                calcTWelch(samples[[i]], ys, fun = mean)
            }
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
            if (all(ylevels != sort(unique(ys)))) {
                -calcT(xs, ys, fun = mean)
            } else {
                calcT(xs, ys, fun = mean)
            }
        } else if (stat.method == "t-Welch") {
            if (all(ylevels != sort(unique(ys)))) {
                -calcTWelch(xs, ys, fun = mean)
            } else {
                calcTWelch(xs, ys, fun = mean)
            }
        }
    },

    animateStat = function(env, n.steps) {
        if (stat.method == "difference") {
            dropPermArrow(.self, env, n.steps)
        } else if (stat.method %in% c("t-pooled", "t-Welch")) {
            animateTStat(.self, env, n.steps)
        }
    },

    displayResult = function(env, ...) {
        if (stat.method == "difference") {
            showTailPerm2Sample(.self, env, fun = mean, ...)
        } else if (stat.method %in% c("t-pooled", "t-Welch")) {
            showTailTStat(.self, env, ...)
        }
    },

    ## plotKSample comes from methods-permutation-mean-ksample.R
    plotSample = function(env, i = which.sample) {
        plotKSample(.self, env, i, fun = mean)
    },

    showLabels = function() {
        permLabels(.self)
    },

    plotDataStat = function(env, ...) {
         dataDiffArrowMean(.self, env)
    },

    plotStatDist = function(env, ...) {
        plotDiffDist(.self, env)
    },

    animateSample = function(...) {
        permTwoSample(.self, ...)
    },

    handle1000 = function(env, ...) {
        perm1000(.self, env, ...)
    }))

load_permutation_mean <- function(e) {
    e$c1$orderLevels(FUN = mean)
    e$c1$stat.in.use <- svalue(e$stat)
    e$perm.method <- svalue(e$perm.choices)
    e$c1$stat.method <- e$perm.method
    # There is something messy going on with viewports and
    # gTrees, such that when attempting to import them, we
    # end up with "uninitializedField"s. Assign to a temp
    # var and reassign later.
    tmp.vps <- e$c1$viewports
    tmp.image <- e$c1$image
    e$c1$viewports <- NULL
    e$c1$image <- NULL
    tmp.canvas <- canvasPermMeanDiff$new()
    tmp.canvas$import(e$c1)
    e$c1 <- tmp.canvas
    e$c1$viewports <- tmp.vps
    e$c1$image <- tmp.image
    e$extra <- 0
}

calcTWelch <- function(x, y, fun = mean){
    uy <- sort(unique(y))
    s1 <- x[y == uy[1]]
    s2 <- x[y == uy[2]]
    m1 <- fun(s1)
    m2 <- fun(s2)
    n1 <- length(s1)
    n2 <- length(s2)
    diff <- m1 - m2
    se <- sqrt(var(s1)/n1 + var(s2)/n2)
    c(0, diff/se)
}

calcT <- function(x, y, fun = mean){
    uy <- sort(unique(y))
    s1 <- x[y == uy[1]]
    s2 <- x[y == uy[2]]
    m1 <- fun(s1)
    m2 <- fun(s2)
    n1 <- length(s1)
    n2 <- length(s2)
    diff <- m1 - m2
    se <- sqrt(((n1 - 1)*var(s1) + (n2 - 1)*var(s2))/(n1 + n2 - 2))*
        sqrt(1/n1 + 1/n2)
    c(0, diff/se)
}

permLabels <- function(canvas){
    samplabel <- textGrob("Data",
                          x = unit(0, "npc") + unit(1, "mm"),
                          y = unit(1, "npc") - unit(1, "mm"),
                          just = c("left", "top"),
                          name = "dataLabel",
                          vp = canvas$graphPath("data", 2),
                          gp = gpar(fontface = 2))
    methlabel <- textGrob("Module: Randomisation test",
                          x = unit(0, "npc"),
                          just = "left",
                          gp = gpar(fontsize = 10, fontface = "italic"),
                          name = "methodLabel",
                          vp = canvas$graphPath("canvas.header"))
    varlabel <- textGrob(paste("Variable:", canvas$x.name),
                         x = unit(0, "npc") + stringWidth(methlabel$label) + unit(6, "mm"),
                         just = "left",
                         gp = gpar(fontsize = 10, fontface = "italic"),
                         name = "varLabel",
                         vp = canvas$graphPath("canvas.header"))
    quantitylabel <- textGrob(paste("Quantity:", canvas$stat.in.use),
                              x = varlabel$x[1] + stringWidth(paste(varlabel$label, collapse = "")) + unit(6, "mm"),
                              just = "left",
                              name = "quantityLabel",
                              gp = gpar(fontsize = 10, fontface = "italic"),
                              vp = canvas$graphPath("canvas.header"))
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
    infosep <- linesGrob(x = unit(0:1, "npc"), y = unit(0, "npc"),
                         name = "infoSeparatorLine",
                         vp = canvas$graphPath("canvas.header"))
    resamplabel <- textGrob("Re-randomised data",
                            x = unit(0, "npc") + unit(1, "mm"),
                            y = unit(0.6, "npc"),
                            just = c("left", "top"),
                            name = "sampleLabel",
                            vp = canvas$graphPath("sample", 2),
                            gp = gpar(fontface = 2))
    statlabel <- textGrob("Re-randomisation distribution",
                          x = unit(0, "npc") + unit(1, "mm"),
                          y = unit(0.8, "npc"),
                          just = c("left", "top"),
                          name = "statLabel",
                          vp = canvas$graphPath("stat"),
                          gp = gpar(fontface = 2))
    permlabels <- grobTree(methlabel, varlabel, quantitylabel, quantmethlabel, filelabel,
                           infosep,
                           samplabel, resamplabel, statlabel,
                           name = "permlabels")
    canvas$image <- addGrob(canvas$image, permlabels)
}

calcDiff <- function(x, y, fun = mean){
    ylevels <- sort(unique(y))
    stat1 <- fun(as.numeric(x[y == ylevels[1]]))
    stat2 <- fun(as.numeric(x[y != ylevels[1]]))
    c(stat2, stat1)
}

dataDiffArrowMean <- function(canvas, e){
    x <- canvas$x
    levels <- canvas$levels
    ylevels <- canvas$ylevels
    mean1 <- mean(x[levels == ylevels[1]])
    mean2 <- mean(x[levels != ylevels[1]])
    y <- old.stackPoints(x, levels, vp = canvas$graphPath("data"))
    n <- 1
    ## Plotting coloured data.
    for (i in ylevels) {
        canvas$image <- addGrob(canvas$image, linesGrob
                                (x = unit(rep(mean(x[levels == i]), 2),
                                 "native"),
                                 y = unit(c(0.1, 0.55), "npc"),
                                 gp = gpar(lwd = 3, col = "purple3"),
                                 vp = canvas$graphPath("data", as.character(n)),
                                 name = paste("dataPlot", "line", n, sep = ".")))
        n <- n + 1
    }
    ## Plotting arrow on data plot.
    # Moving the diff arrows down in the case when we are using histograms
    # because histograms are level on y = 0
    y.locs <- c(0.8, 0.9)
    if (length(x) >= canvas$hist.cutoff)
        y.locs <- y.locs - 0.1
    canvas$image <- addGrob(canvas$image, linesGrob(x = unit(c(mean2, mean1), "native"),
                                                    y = unit(y.locs[1], "npc"),
                                                    gp = gpar(lwd = 2, col = "red"),
                                                    arrow = arrow(length = unit(0.1, "inches")),
                                                    vp = canvas$graphPath("data", 1),
                                                    name = "dataPlot.stat.2"))
    canvas$image <- addGrob(canvas$image, textGrob
                            (label = format(round(diff(c(mean2, mean1)), canvas$dp), nsmall = canvas$dp),
                             x = unit(mean(c(mean2, mean1)), "native"),
                             y = unit(y.locs[2], "npc"),
                             just = c("centre", "bottom"),
                             gp = gpar(lwd = 2, col = "red", fontface = "bold"),
                             vp = canvas$graphPath("data", 1),
                             name = "dataPlot.stat.text"))

    # Add a dashed line at zero in certain cases
    zeroline <- linesGrob(x = unit(rep(0, 2), "native"),
                          y = unit(c(0, 0.4), "npc"),
                          gp = gpar(lty = "dashed"),
                          vp = canvas$graphPath("stat"),
                          name = "zeroline.1")
    if (e$method == "bootstrap") {
        canvas$image <- addGrob(canvas$image, zeroline)
    } else if (e$method == "permutation") {
        if (e$perm.method == "difference") {
            canvas$image <- addGrob(canvas$image, zeroline)
        }
    } else if (e$method == "sampvar") {
        if (e$sampvar.method == "difference") {
            canvas$image <- addGrob(canvas$image, zeroline)
        }
    }
}

permTwoSample <- function(canvas, e, n.steps, mix = TRUE){
    canvas$rmGrobs("samplePlot.databox.text.2")
    e$clearPanel("sample")
    ## Drop samples down to middle plot.
    x <- canvas$x
    levels <- canvas$levels
    ylevels <- canvas$ylevels
    y <- old.stackPoints(canvas$x, canvas$levels, vp = canvas$graphPath("data"), y.min = 0.3, y.max = 1)
    y.start <- y
    y.end <- y - 2
    y.step <- (y.start - y.end)/n.steps
    if (mix){
        ## Dropping samples
        for (i in 1:n.steps){
            plotPointGroups(canvas, x, y.start - i*y.step, levels, "data",
                            cols = getColour(1:length(ylevels), length(ylevels)), name = "samplePlot.temp")
            if (canvas$stopAnimation)
                return()
            canvas$drawImage()
        }
        if (canvas$stopAnimation)
            return()
        canvas$pauseImage(10)
        canvas$rmGrobs(paste("samplePlot.temp.points", 1:2, sep = "."))
        ## Mixing samples
        y.end <- old.stackPoints(x, vp = canvas$graphPath("sample"), y.min = 0, y.max = 0.2)
        y.end.1 <- y.end[levels == ylevels[1]] + 0.8
        y.end.2 <- y.end[levels == ylevels[2]] - 0.2
        y.start.1 <- y.start[levels == ylevels[1]]
        y.start.2 <- y.start[levels == ylevels[2]]
        y.step.1 <- (y.start.1 - y.end.1)/n.steps
        y.step.2 <- (y.start.2 - y.end.2)/n.steps
        for (i in 1:n.steps){
            plotPoints(canvas, x[levels == ylevels[1]],
                       y.start.1 - i*y.step.1,
                       vp = canvas$graphPath("sample", 1), name = "samplePlot.tempjoin",
                       col = getColour(1, 2))
            plotPoints(canvas, x[levels == ylevels[2]],
                       y.start.2 - i*y.step.2,
                       vp = canvas$graphPath("sample", 2), name = "samplePlot.tempjoin",
                       col = getColour(2, 2))
            if (canvas$stopAnimation)
                return()
            canvas$drawImage()
        }
        canvas$rmGrobs(paste("samplePlot.tempjoin.points", 1:2, sep = "."))
    }
    ## Separating samples
    x.sample <- canvas$samples[[canvas$which.sample]]
    y.start <- old.stackPoints(x, vp = canvas$graphPath("sample", 1),
                               y.min = 0.8, y.max = 1)
    y.start <- y.start[canvas$indexes[[canvas$which.sample]]]
    y.start.1 <- y.start[levels == ylevels[1]]
    y.start.2 <- y.start[levels == ylevels[2]]
    plotPoints(canvas, x.sample, y.start,
               vp = canvas$graphPath("sample", 1), name = "samplePlot.join",
               col = "black")
    if (canvas$stopAnimation)
        return()
    canvas$pauseImage(10)
    canvas$rmGrobs("samplePlot.join.points.1")
    text <-  as.character(levels[order(canvas$indexes[[canvas$which.sample]])])
    cols <- getColour(1:length(ylevels), length(ylevels))
    names(cols) <- ylevels
    cols <- cols[sort(ylevels)]
    canvas$image <- addGrob(canvas$image, coldatatextGrob
                            (data = text,
                             title = if (e$method == "permvar") "Random groups" else "Re-randomised",
                             cols = cols,
                             xpos = 0.5, max = 50,
                             name = "samplePlot.databox.text.2", vp = canvas$graphPath("databox", 2)))
    plotPoints(canvas, x.sample[levels == ylevels[1]],
               y.start.1, alpha = 0.25,
               vp = canvas$graphPath("sample", 1), name = "samplePlot.join1", col = getColour(1, 2))
    plotPoints(canvas, x.sample[levels == ylevels[2]],
               y.start.2, alpha = 0.25,
               vp = canvas$graphPath("sample", 1), name = "samplePlot.join2", col = getColour(2, 2))
    y.sample <- old.stackPoints(x.sample, levels, vp = canvas$graphPath("sample"))
    y.end.1 <- y.sample[levels == ylevels[1]]
    y.end.2 <- y.sample[levels == ylevels[2]] + 1
    y.step.1 <- (y.start.1 - y.end.1)/n.steps
    y.step.2 <- (y.start.2 - y.end.2)/n.steps
    plotPoints(canvas, x.sample[levels == ylevels[1]],
               y.start.1,
               vp = canvas$graphPath("sample", 1), name = "samplePlot.tempjoin1",
               col = getColour(1, 2))
    plotPoints(canvas, x.sample[levels == ylevels[2]],
               y.start.2,
               vp = canvas$graphPath("sample", 1), name = "samplePlot.tempjoin2",
               col = getColour(2, 2))
    canvas$pauseImage(5)
    for (i in 1:n.steps) {
        plotPoints(canvas, x.sample[levels == ylevels[1]],
                   y.start.1 - i*y.step.1,
                   vp = canvas$graphPath("sample", 1), name = "samplePlot.tempjoin1",
                   col = getColour(1, 2))
        plotPoints(canvas, x.sample[levels == ylevels[2]],
                   y.start.2 - i*y.step.2,
                   vp = canvas$graphPath("sample", 1), name = "samplePlot.tempjoin2",
                   col = getColour(2, 2))
        if (canvas$stopAnimation)
            return()
        canvas$drawImage()
    }
    canvas$rmGrobs(paste(c("samplePlot.tempjoin1", "samplePlot.tempjoin2", "samplePlot.join1", "samplePlot.join2"),
                           "points.1", sep = "."))
}

dropPermArrow <- function(canvas, e, n.steps, fun = mean){
    arrowbounds <- canvas$stat.dist[c(canvas$plotted.stats, canvas$which.sample)]
    stats <- sapply(arrowbounds, diff)
    curr.stat <- stats[length(stats)]
    curr.arrow <- arrowbounds[[length(stats)]]
    y.max <- unit(1, "npc") - unit(2, "lines") - unit(0.5, "char")
    y <- stackPoints(stats, vp = canvas$graphPath("stat"), y.min = 0, y.max = y.max)
    y.start <- 1.4
    y.end <- y[length(stats)]
    y.step <- (y.start - y.end)/n.steps
    xs.start <- curr.arrow
    xs.end <- curr.arrow - (curr.arrow[1] - mean(range(canvas$x)))
    xs.step <- (xs.start - xs.end)/n.steps
    for (i in 0:n.steps){
        temp.arrow <- linesGrob(x = unit(xs.start - i*xs.step, "native"),
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
    canvas$rmGrobs("samplePlot.temp.arrow")
}

plotDiffDist <- function(canvas, e) {
    canvas$plotted.stats <- c(canvas$plotted.stats, canvas$which.sample)
    stats <- sapply(canvas$stat.dist[canvas$plotted.stats], diff)
    y.max <- unit(1, "npc") - unit(2, "lines") - unit(0.5, "char")
    y <- stackPoints(stats, vp = canvas$graphPath("stat"), y.min = 0, y.max = y.max)
    plotPoints(canvas, stats, y, vp = canvas$graphPath("stat"), name = "statPlot")
}

perm1000 <- function(canvas, e){
    if (length(canvas$stat.dist[[1]]) == 2){
        stats <- sapply(canvas$stat.dist, diff)
    } else {
        stats <- c(canvas$stat.dist, recursive = TRUE)
    }
    for (i in 20*(1:50)){
        canvas$plotSample(e, i)
        x <- stats[1:i]
        y <- canvas$stat.ypos[1:i]
        plotPoints(canvas, x, y, canvas$graphPath("stat"),
                   "statPlot", black = FALSE, alpha = 0.7)
        if (! is.null(canvas$ylevels))
            canvas$showLabels()
        if (canvas$stopAnimation)
            return()
        canvas$drawImage()
    }
    canvas$plotted.stats <- NULL
    e$clearPanel("sample")
    canvas$rmGrobs(c("samplePlot.databox.text.2", paste("samplePlot.propbar", 1:length(canvas$ylevels), sep = ".")))
    if (canvas$stopAnimation)
        return()
    canvas$drawImage()
    canvas$rmGrobs("statPlot.points.1")
}

showTailPerm2Sample <- function(canvas, e, fun = mean){
    n.steps <- 10
    x <- canvas$x
    levels <- canvas$levels
    ylevels <- canvas$ylevels
    stat1 <- fun(x[levels == ylevels[1]])
    stat2 <- fun(x[levels != ylevels[1]])
    ## Replotting statistic distribution
    stats <- sapply(canvas$stat.dist, diff)
    y.stats <- canvas$stat.ypos
    plotPoints(canvas, stats, y.stats, canvas$graphPath("stat"),
               "statPlot", black = FALSE, alpha = 0.7)
    y.start <- 2.4
    y.end <- 0
    y.step <- (y.start - y.end)/n.steps
    x.start <- stat2
    x.end <- mean(range(x))
    x.step <- (x.start - x.end)/n.steps
    for (i in 0:10){
        canvas$image <- addGrob(canvas$image, linesGrob
                                (x = unit(c(stat2, stat1) - i*x.step, "native"),
                                 y = unit(y.start - i*y.step, "native"),
                                 gp = gpar(lwd = 2, col = "red"),
                                 arrow = arrow(length = unit(0.1, "inches")),
                                 vp = canvas$graphPath("animation.field"),
                                 name = "statPlot.temp.arrow"))
        if (canvas$stopAnimation)
            return()
        canvas$drawImage()
    }
    canvas$rmGrobs("statPlot.temp.arrow")
    diff <- diff(c(stat2, stat1))
    ## We only consider one-tailed p-values - the statistics
    ## considered "more extreme" that what is observed therefore
    ## depends on the original direction of the observed effect.
    if (diff > 0){
        x.in <- stats[stats < diff]
        x.out <- stats[stats >= diff]
        y.in <- y.stats[stats < diff]
        y.out <- y.stats[stats >= diff]
        tot <- sum(stats >= diff)
        p <- mean(stats >= diff)
    } else {
        x.in <- stats[stats > diff]
        x.out <- stats[stats <= diff]
        y.in <- y.stats[stats > diff]
        y.out <- y.stats[stats <= diff]
        tot <- sum(stats <= diff)
        p <- mean(stats <= diff)
    }
    if (length(x.in) > 0){
        canvas$image <- addGrob(canvas$image, pointsGrob
                                (x.in, y.in, gp = gpar(col = "lightgrey", lwd = 2, alpha = 0.7),
                                 vp = canvas$graphPath("stat"), name = "statPlot.lightpoints"))
    }
    if (length(x.out) > 0){
        canvas$image <- addGrob(canvas$image, pointsGrob
                                (x.out, y.out, gp = gpar(col = "grey60", lwd = 2, alpha = 0.7),
                                         vp = canvas$graphPath("stat"), name = "statPlot.darkpoints"))
    }
    ## Calculating whether or not p-value text is printed off screen.
    arrow <- linesGrob(x = unit(c(0, diff), "native"),
                       y = unit(y.end, "native"),
                       gp = gpar(lwd = 2, col = "red"),
                       arrow = arrow(length = unit(0.1, "inches")),
                       name = "statPlot.arrow.1")
    top.level <- downViewport(canvas$graphPath("stat"))
    text1 <- paste(tot, "/ 1000", sep = " ")
    text2 <- paste("= ",p)
    text.width <- max(stringWidth(text1), stringWidth(text2))
    arrow.width <- grobWidth(arrow)
    pos <- convertX(arrow.width + unit(5, "mm") + text.width, "npc", valueOnly = TRUE)
    if (pos > 0.5){
        xpos <- unit(1, "npc")
        hjust <- 1
    } else {
        xpos <- unit(rep(diff, 2), "native") + unit(5, "mm")
        hjust <- 0
    }
    upViewport(top.level)
    arrow <- editGrob(arrow, vp = canvas$graphPath("stat"))
    canvas$image <- addGrob(canvas$image, rectGrob
                            (x = xpos, y = unit(0.5, "npc"), width = text.width,
                             height = unit(2, "lines"), hjust = hjust, vjust = 0,
                             gp = gpar(col = "white", fill = "white"),
                             vp = canvas$graphPath("stat"), name = "statPlot.proprect"))
    canvas$image <- addGrob(canvas$image, textGrob
                            (c(text1, text2),
                             x = xpos,
                             y = unit.c(unit(1, "lines") + unit(0.5, "npc") + unit(1, "mm"),
                             unit(0.5, "npc") + unit(1, "mm")),
                             hjust = hjust, vjust = 0, vp = canvas$graphPath("stat"),
                             name = "statPlot.proptext"))
    canvas$image <- addGrob(canvas$image, linesGrob
                            (x = unit(rep(diff, 2), "native"),
                             y = unit(c(0, 0.5), "npc") - unit(1, "mm"),
                             vp = canvas$graphPath("stat"), name = "statPlot.propline"))
    canvas$image <- addGrob(canvas$image, arrow)
    canvas$showLabels()
    if (canvas$stopAnimation)
        return()
    canvas$drawImage()
    canvas$rmGrobs(c("statPlot.proptext", "statPlot.proprect", "statPlot.propline", "statPlot.arrow.1", "statPlot.points.1",
                     "statPlot.lightpoints", "statPlot.darkpoints"))
}

animateTStat <- function(canvas, e, n.steps, fun = mean){
    x <- canvas$samples[[canvas$which.sample]]
    if (is.list(canvas$stat.dist))
        levels <- as.character(canvas$levels)
    else
        levels <- as.character(canvas$level.samples[[canvas$which.sample]])
    ylevels <- canvas$ylevels
    curr.arrow <- c(fun(x[levels == ylevels[2]]), fun(x[levels == ylevels[1]]))
    arrowwidth <- diff(curr.arrow)
    if (is.list(canvas$stat.dist))
        currstat <- canvas$stat.dist[[canvas$which.sample]][2]
    else
        currstat <- canvas$stat.dist[canvas$which.sample, 2]
    canvas$image <- addGrob(canvas$image, textGrob
                            (label = "Calculating t-statistic...",
                             x = unit(0.5, "npc"), y = unit(0.6, "native") - unit(0.15, "inches"),
                             just = "top", gp = gpar(fontface = 2), name = "samplePlot.calc.message",
                             vp = canvas$graphPath("animation.field")))
    y.start <- 1.4
    y.end <- 0.6
    y.step <- (y.start - y.end)/n.steps
    xs.start <- unit(curr.arrow, "native")
    xs.end <- unit(0.5, "npc") + c(-0.5, 0.5)*(xs.start[2] - xs.start[1])
    xs.step <- (xs.start - xs.end)*(1/n.steps)
    for (i in 0:n.steps){
        temp.arrow <- linesGrob(x = xs.start - i*xs.step,
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
    if (canvas$stopAnimation)
        return()
    canvas$pauseImage(4)
    canvas$rmGrobs("samplePlot.temp.arrow")
    canvas$image <- addGrob(canvas$image, roundrectGrob
                            (x = unit(0.5, "npc"), y = unit(0.6, "native"),
                             width = unit(abs(arrowwidth), "native"),
                             height = unit(0.2, "inches"), name = "samplePlot.temp.rect",
                             gp = gpar(fill = "black"), r = unit(0.2, "snpc"),
                             vp = canvas$graphPath("animation.field")))
    if (canvas$stopAnimation)
        return()
    canvas$drawImage()
    canvas$rmGrobs("samplePlot.calc.message")
    if (is.list(canvas$stat.dist))
        stats <- c(canvas$stat.dist, recursive = TRUE)[c(FALSE, TRUE)]
    else
        stats <- canvas$stat.dist[, 2]
    stats <- na.omit(stats)
    minstat <- min(abs(stats))
    maxstat <- max(abs(stats))
    sizemult <- (abs(currstat) - minstat)/(maxstat - minstat)
    halfheight <- 0.03 + 0.16*sizemult
    fontsize <- 12 + 64*sizemult

    stat.text.label <- if (is.na(currstat)) "NA" else format(round(currstat, canvas$dp), nsmall = canvas$dp)
    stattext <- textGrob(label = stat.text.label, x = unit(0.5, "npc"),
                         y = unit(0.6, "npc"), gp = gpar(col = "red", fontsize = fontsize),
                         name = "samplePlot.temp.text")
    canvas$image <- addGrob(canvas$image, roundrectGrob
                            (x = unit(0.5, "npc"), y = unit(0.6, "native"),
                             width = unit(halfheight/2, "npc"),
                             height = unit(halfheight, "npc"), name = "samplePlot.temp.rect",
                             gp = gpar(fill = "black"), r = unit(0.2, "snpc"),
                             vp = canvas$graphPath("animation.field")))
    if (canvas$stopAnimation)
        return()
    canvas$drawImage()
    canvas$image <- addGrob(canvas$image, roundrectGrob
                            (x = unit(0.5, "npc"), y = unit(0.6, "native"),
                             width = unit(halfheight, "npc"),
                             height = unit(halfheight/2, "npc"), name = "samplePlot.temp.rect",
                             gp = gpar(fill = "black"), r = unit(0.2, "snpc"),
                             vp = canvas$graphPath("animation.field")))
    if (canvas$stopAnimation)
        return()
    canvas$drawImage()
    canvas$image <- addGrob(canvas$image, roundrectGrob
                            (x = unit(0.5, "npc"), y = unit(0.6, "native"),
                             width = grobWidth(stattext) + unit(5, "mm"),
                             height = unit(halfheight, "npc"), name = "samplePlot.temp.rect",
                             gp = gpar(fill = "black"), r = unit(0.2, "snpc"),
                             vp = canvas$graphPath("animation.field")))
    stattext <- editGrob(stattext, vp = canvas$graphPath("stat"))
    canvas$image <- addGrob(canvas$image, stattext)
    canvas$pauseImage(5)
    canvas$rmGrobs("samplePlot.temp.rect")

    if (! is.na(currstat)) {
        n.steps <- round(n.steps/2)
        x.start <- unit(0.5, "npc")
        x.end <- unit(currstat, "native")
        x.step <- (x.end - x.start)*(1/n.steps)
        y.start <- 0.6
        y.end <- 0
        y.step <- (y.end - y.start)/n.steps
        for (i in 1:n.steps){
            canvas$image <- editGrob(canvas$image, gPath("samplePlot.temp.text"),
                                     x = x.start + i*x.step,
                                     y = unit(y.start + i*y.step, "npc"),
                                     vjust = 0.5*(1 - i/n.steps))
            if (canvas$stopAnimation)
                return()
            canvas$drawImage()
        }
        if (canvas$stopAnimation)
            return()
        canvas$pauseImage(4)
        canvas$rmGrobs("samplePlot.temp.text")
    } else {
        canvas$drawImage()
    }
}

showTailTStat <- function(canvas, e, fun = mean){
    n.steps <- 10
    x <- canvas$x
    levels <- canvas$levels
    ylevels <- canvas$ylevels
    stats <- c(canvas$stat.dist, recursive = TRUE)[c(FALSE, TRUE)]
    curr.arrow <- c(fun(x[levels == ylevels[2]]), fun(x[levels == ylevels[1]]))
    arrowwidth <- diff(curr.arrow)
    y.max <- unit(1, "npc") - unit(2, "lines") - unit(0.5, "char")
    y <- canvas$stat.ypos
    plotPoints(canvas, stats, y, vp = canvas$graphPath("stat"), name = "statPlot",
               alpha = 0.7)
    canvas$showLabels()
    canvas$image <- addGrob(canvas$image, textGrob
                            (label = "Calculating t-statistic...",
                             x = unit(0.5, "npc"), y = unit(0.6, "native") - unit(0.15, "inches"),
                             just = "top", gp = gpar(fontface = 2), name = "statPlot.calc.message",
                             vp = canvas$graphPath("animation.field")))
    currstat <- canvas$calcAllStats(canvas$x, canvas$levels)[2]
    y.start <- 2.4
    y.end <- 0.6
    y.step <- (y.start - y.end)/n.steps
    xs.start <- unit(curr.arrow, "native")
    xs.end <- unit(0.5, "npc") + c(-0.5, 0.5)*(xs.start[2] - xs.start[1])
    xs.step <- (xs.start - xs.end)*(1/n.steps)
    for (i in 0:n.steps){
        temp.arrow <- linesGrob(x = xs.start - i*xs.step,
                                y = unit(rep(y.start - i*y.step, 2), "native"),
                                gp = gpar(lwd = 2, col = "red"),
                                arrow = arrow(length = unit(0.1, "inches")),
                                vp = canvas$graphPath("animation.field"),
                                name = "statPlot.temp.arrow")
        canvas$image <- addGrob(canvas$image, temp.arrow)
        if (canvas$stopAnimation)
            return()
        canvas$drawImage()
    }
    canvas$pauseImage(4)
    canvas$rmGrobs("statPlot.temp.arrow")
    canvas$image <- addGrob(canvas$image, roundrectGrob
                            (x = unit(0.5, "npc"), y = unit(0.6, "native"),
                             width = unit(abs(arrowwidth), "native"),
                             height = unit(0.2, "inches"), name = "statPlot.temp.rect",
                             gp = gpar(fill = "black"), r = unit(0.2, "snpc"),
                             vp = canvas$graphPath("animation.field")))
    if (canvas$stopAnimation)
        return()
    canvas$drawImage()
    canvas$rmGrobs("statPlot.calc.message")
    minstat <- min(abs(stats))
    maxstat <- max(abs(stats))
    sizemult <- min(1, (abs(currstat) - minstat)/(maxstat - minstat))
    halfheight <- 0.03 + 0.16*sizemult
    fontsize <- 12 + 64*sizemult
    stattext1 <- textGrob(label = format(round(currstat, canvas$dp), nsmall = canvas$dp), x = unit(0.5, "npc"),
                         y = unit(0.6, "npc"), gp = gpar(col = "red", fontsize = fontsize),
                         name = "statPlot.temp.text")
    canvas$image <- addGrob(canvas$image, roundrectGrob
                            (x = unit(0.5, "npc"), y = unit(0.6, "native"),
                             width = unit(halfheight/2, "npc"),
                             height = unit(halfheight, "npc"), name = "statPlot.temp.rect",
                             gp = gpar(fill = "black"), r = unit(0.2, "snpc"),
                             vp = canvas$graphPath("animation.field")))
    if (canvas$stopAnimation)
        return()
    canvas$drawImage()
    canvas$image <- addGrob(canvas$image, roundrectGrob
                            (x = unit(0.5, "npc"), y = unit(0.6, "native"),
                             width = unit(halfheight, "npc"),
                             height = unit(halfheight/2, "npc"), name = "statPlot.temp.rect",
                             gp = gpar(fill = "black"), r = unit(0.2, "snpc"),
                             vp = canvas$graphPath("animation.field")))
    if (canvas$stopAnimation)
        return()
    canvas$drawImage()
    canvas$image <- addGrob(canvas$image, roundrectGrob
                            (x = unit(0.5, "npc"), y = unit(0.6, "native"),
                             width = grobWidth(stattext1) + unit(5, "mm"),
                             height = unit(halfheight, "npc"), name = "statPlot.temp.rect",
                             gp = gpar(fill = "black"), r = unit(0.2, "snpc"),
                             vp = canvas$graphPath("animation.field")))
    stattext2 <- editGrob(stattext1, vp = canvas$graphPath("stat"))
    canvas$image <- addGrob(canvas$image, stattext2)
    if (canvas$stopAnimation)
        return()
    canvas$pauseImage(5)
    canvas$rmGrobs("statPlot.temp.rect")
    top.level <- downViewport(canvas$graphPath("stat"))
    text.width <- grobWidth(stattext1)
    pos <- convertX(unit(currstat, "native") + 0.5*text.width,
                    "npc", valueOnly = TRUE)
    upViewport(top.level)
    if (pos > 1){
        hjust.end <- 1
    } else {
        hjust.end <- 0.5
    }
    n.steps <- round(n.steps/2)
    hjust.step <- (hjust.end - 0.5)/n.steps
    x.start <- unit(0.5, "npc")
    x.end <- unit(currstat, "native")
    x.step <- (x.end - x.start)*(1/n.steps)
    y.start <- 0.6
    y.end <- 0
    y.step <- (y.end - y.start)/n.steps
    for (i in 1:n.steps){
        canvas$image <- editGrob(canvas$image, gPath("statPlot.temp.text"),
                                 x = x.start + i*x.step,
                                 y = unit(y.start + i*y.step, "npc"),
                                 vjust = 0.5*(1 - i/n.steps),
                                 hjust = 0.5 + i*hjust.step)
        if (canvas$stopAnimation)
            return()
        canvas$drawImage()
    }
    canvas$pauseImage(4)
    y.max <- unit(1, "npc") - unit(2, "lines") - unit(0.5, "char")
    y <- stackPoints(stats, vp = canvas$graphPath("stat"), y.min = 0,
                         y.max = y.max)
    x.out <- stats[stats >= currstat]
    x.in <- stats[stats < currstat]
    y.out <- y[stats >= currstat]
    y.in <- y[stats < currstat]
    tot <- sum(stats >= currstat)
    p <- mean(stats >= currstat)
    if (length(x.in) > 0){
        canvas$image <- addGrob(canvas$image, pointsGrob
                                (x.in, y.in, gp = gpar(col = "lightgrey", lwd = 2, alpha = 0.7),
                                 vp = canvas$graphPath("stat"), name = "statPlot.lightpoints"))
    }
    if (length(x.out) > 0){
        canvas$image <- addGrob(canvas$image, pointsGrob
                                (x.out, y.out, gp = gpar(col = "grey60", lwd = 2, alpha = 0.7),
                                         vp = canvas$graphPath("stat"), name = "statPlot.darkpoints"))
    }
    canvas$showLabels()
    top.level <- downViewport(canvas$graphPath("stat"))
    text1 <- paste(tot, "/ 1000", sep = " ")
    text2 <- paste("= ",p)
    text.width <- max(stringWidth(text1), stringWidth(text2))
    pos <- convertX(unit(currstat, "native") + text.width, "npc", valueOnly = TRUE)
    if (pos > 1){
        xpos <- unit(1, "npc")
        hjust <- 1
    } else {
        xpos <- unit(rep(currstat, 2), "native") + unit(5, "mm")
        hjust <- 0
    }
    upViewport(top.level)
    canvas$image <- addGrob(canvas$image, rectGrob
                            (x = xpos, y = unit(0.5, "npc"), width = text.width,
                             height = unit(2, "lines"), hjust = hjust, vjust = 0,
                             gp = gpar(col = "white", fill = "white"),
                             vp = canvas$graphPath("stat"), name = "statPlot.proprect"))
    canvas$image <- addGrob(canvas$image, textGrob
                            (c(text1, text2),
                             x = xpos,
                             y = unit.c(unit(1, "lines") + unit(0.5, "npc") + unit(1, "mm"),
                             unit(0.5, "npc") + unit(1, "mm")),
                             hjust = hjust, vjust = 0, vp = canvas$graphPath("stat"),
                             name = "statPlot.proptext"))
    canvas$image <- addGrob(canvas$image, linesGrob
                            (x = unit(rep(currstat, 2), "native"),
                             y = unit(c(0, 0.5), "npc") - unit(1, "mm"),
                             vp = canvas$graphPath("stat"), name = "statPlot.propline"))
    canvas$image <- addGrob(canvas$image, pointsGrob
                            (x = unit(currstat, "native"), y = unit(0, "npc"),
                             pch = 19, gp = gpar(col = "red"), vp = canvas$graphPath("stat"),
                             name = "statPlot.statpoint.1"))
    canvas$rmGrobs(c("statPlot.points.1", "statPlot.temp.text"))
    if (canvas$stopAnimation)
        return()
    canvas$pauseImage(5)
    canvas$showLabels()
    canvas$drawImage()
    canvas$rmGrobs(c("statPlot.lightpoints", "statPlot.darkpoints", "statPlot.statpoint.1",
                     "statPlot.proprect", "statPlot.proptext", "statPlot.propline"))
}
