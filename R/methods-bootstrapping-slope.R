canvasBootSlope <- setRefClass("canvasBootSlopeClass", contains = "canvasPlotClass",
                               methods = list(
    plotSample = function(env, i = which.sample, ...) {
        plotSampleScatterBoot(.self, env, i)
    },

    showLabels = function() {
        bootRegressionLabels(.self)
    },

    calcStat = function(i = which.sample, ys = NULL, canvas = .self) {
        calcCoeffs(as.numeric(samples[[i]]), as.numeric(ys))
    },

    calcAllStats = function(xs, ys = NULL, canvas = .self) {
        calcCoeffs(as.numeric(xs), as.numeric(ys))
    },

    ## showExtraRect() from methods-permutation-slope.R
    plotDataStat = function(env, ...) {
        showExtraRect(env)
    },

    plotStatDist = function(env, ...) {
        plotSlopeBoot(.self, env)
    },

    animateSample = function(...) {
        animateSampleBootSlope(.self, ...)
    },

    trackSample = function(...) {
        trackBootstrapSlope(.self)
    },

    animateStat = function(env, n.steps, move.point = TRUE) {
        movePointBoot(.self, env, n.steps, move.point)
    },

    displayResult = function(canvas, ...) {
        showSlopeCI(.self, env, ...)
    },

    handle1000 = function(env, points) {
        slope1000boot(.self, env, points)
    },

    fadePlots = function(env, ...) {
        fadeRegression(.self, env, ...)
    },

    calcxScaleYs = function(env, x.scale) {
        x.scale.ys <<- aaply(stat.dist, 1, function(x) x[1] + x[2]*x.scale)
    }))

load_bootstrap_slope <- function(e) {
    e$c1$stat.in.use <- svalue(e$stat)
    e$resampWithin <- FALSE
    # There is something messy going on with viewports and
    # gTrees, such that when attempting to import them, we
    # end up with "uninitializedField"s. Assign to a temp
    # var and reassign later.
    tmp.vps <- e$c1$viewports
    tmp.image <- e$c1$image
    e$c1$viewports <- NULL
    e$c1$image <- NULL
    tmp.canvas <- canvasBootSlope$new()
    tmp.canvas$import(e$c1)
    e$c1 <- tmp.canvas
    e$c1$viewports <- tmp.vps
    e$c1$image <- tmp.image
    e$extra <- 0
    e$difference <- TRUE
}

## Helper function used in plotSampleScatterBoot and animateSampleBootSlope
plotMiddleSampleOnly <- function(canvas, e, i){
    canvas$sampled.stats <- unique(c(canvas$sampled.stats, i))
    plot.symb <- table(canvas$indexes[[i]])
    plot.ind <- as.numeric(names(plot.symb))
    x <- canvas$x[plot.ind]
    y <- as.numeric(canvas$levels)[plot.ind]
    text1 <- as.character(format(round(canvas$samples[[i]], canvas$dp),
                                nsmall = canvas$dp))
    text2 <- as.character(format(round(as.numeric(canvas$level.samples[[i]]), canvas$dp),
                                nsmall = canvas$dp))
    plotFreqScatterPlot(canvas, x, y, plot.symb, "sample", "blue")
    canvas$image <- addGrob(canvas$image, datatextGrob
                            (data = text2, title = "",
                             x = 0.75, max = 50,
                             name = "samplePlot.databox.text.2",
                             vp = canvas$graphPath("databox", 2)))
    canvas$image <- addGrob(canvas$image, datatextGrob
                            (data = text1, title = "",
                             x = 0.25, max = 50,
                             name = "samplePlot.databox.text.1",
                             vp = canvas$graphPath("databox", 2)))
    canvas$image <- addGrob(canvas$image, segmentsGrob
                            (x0 = unit(rep(1.5, 2), "lines"),
                             x1 = unit.c(unit(1.5, "lines"), unit(0.9, "npc")),
                             y0 = unit(rep(1/3, 2), "npc") + unit(rep(1.5, 2), "lines"),
                             y1 = unit(rep(1/3, 2), "npc") + unit.c(unit(0.9/3, "npc"),
                             unit(1.5, "lines")),
                             arrow = arrow(length = unit(0.075, "inches")),
                             vp = canvas$graphPath("stat.extrabox"),
                             name = "samplePlot.labelaxes.1"))
    canvas$image <- addGrob(canvas$image, textGrob
                            (label = canvas$x.name, x = 0.5,
                             y = unit(1/3, "npc") + unit(1.25, "lines"),
                             just = "top",
                             vp = canvas$graphPath("stat.extrabox"),
                             name = "samplePlot.xlabel.1"))
    canvas$image <- addGrob(canvas$image, textGrob
                            (label = canvas$y.name, x = unit(0.25, "lines"),
                             y = 0.5, just = "top", rot = 90,
                             vp = canvas$graphPath("stat.extrabox"),
                             name = "samplePlot.ylabel.1"))
    slope <- coefficients(lm(y ~ x))[2]
    canvas$image <- addGrob(canvas$image, textGrob
                            (label = c("slope =", format(round(slope, canvas$dp),
                             nsmall = canvas$dp)),
                             x = 0.5, y = unit(0.5, "npc") + unit(c(0.5, -0.5), "lines"),
                             gp = gpar(col = c("black", "red"), fontface = 1:2),
                             vp = canvas$graphPath("stat.extrabox"),
                             name = "samplePlot.slopetext.1"))
}

plotSampleScatterBoot <- function(canvas, e, i) {
    plotMiddleSampleOnly(canvas, e, i)
    nsamps <- length(canvas$sampled.stats)
    slopes <- canvas$stat.dist[, 2][canvas$sampled.stats]
    stat <- slopes[length(slopes)]
    x0 <- rep(0, nsamps)
    x1 <- rep(1, nsamps)
    y0 <- unit(rep(0, nsamps), "native")
    y1 <- unit(slopes, "native")
    canvas$image <- addGrob(canvas$image, segmentsGrob
                            (x0 = x0, x1 = x1, y0 = y0, y1 = y1,
                             gp = gpar(col = "skyblue2"),
                             name = "statPlot.statlines.1",
                             vp = canvas$graphPath("stat")))
    canvas$image <- addGrob(canvas$image, linesGrob
                            (x = 0:1,
                             y = unit(c(0, stat), "native"),
                             gp = gpar(lwd = 2, col = "blue"), name = "statPlot.line.1",
                             vp = canvas$graphPath("stat")))
    sampleAxis <- getGrob(canvas$image, gPath("sampleAxis"))
    canvas$image <- addGrob(canvas$image, sampleAxis)
    statlabel <- getGrob(canvas$image, gPath("statLabel"))
    canvas$image <- addGrob(canvas$image, statlabel)
}


plotSampleScatterBootAlpha <- function(canvas, e, i) {
    plotMiddleSampleOnly(canvas, e, i)
    nsamps <- length(canvas$sampled.stats)
    slopes <- canvas$stat.dist[, 2][canvas$sampled.stats]
    stat <- slopes[length(slopes)]
    x0 <- rep(0, nsamps)
    x1 <- rep(1, nsamps)
    y0 <- unit(rep(0, nsamps), "native")
    y1 <- unit(slopes, "native")
    canvas$image <- addGrob(canvas$image, segmentsGrob
                            (x0 = x0, x1 = x1, y0 = y0, y1 = y1,
                             gp = gpar(col = "blue", alpha = 0.25),
                             name = "statPlot.statlines.1",
                             vp = canvas$graphPath("stat")))
    canvas$image <- addGrob(canvas$image, linesGrob
                            (x = 0:1,
                             y = unit(c(0, stat), "native"),
                             gp = gpar(lwd = 2, col = "blue"), name = "statPlot.line.1",
                             vp = canvas$graphPath("stat")))
    sampleAxis <- getGrob(canvas$image, gPath("sampleAxis"))
    canvas$image <- addGrob(canvas$image, sampleAxis)
    statlabel <- getGrob(canvas$image, gPath("statLabel"))
    canvas$image <- addGrob(canvas$image, statlabel)
}

bootRegressionLabels <- function(canvas) {
    samplabel <- textGrob("Sample",
                          x = unit(0, "npc") + unit(1, "mm"),
                          y = unit(1, "npc") - unit(1, "mm"),
                          just = c("left", "top"),
                          name = "dataPlotLabel",
                          vp = canvas$graphPath("data"),
                          gp = gpar(fontface = 2))
    methlabel <- textGrob("Module: Regression Bootstrapping",
                          x = unit(0, "npc"),
                          just = "left",
                          gp = gpar(fontsize = 10, fontface = "italic"),
                          name = "dataPlotmethodLabel",
                          vp = canvas$graphPath("canvas.header"))
    varlabel <- textGrob("Parameter: slope",
                         x = methlabel$x + stringWidth(methlabel$label) + unit(6, "mm"),
                         just = "left",
                         name = "dataPlotvarLabel",
                         gp = gpar(fontsize = 10, fontface = "italic"),
                         vp = canvas$graphPath("canvas.header"))
    filelabel <- textGrob(paste("File:", canvas$data.file),
                          x = varlabel$x + stringWidth(varlabel$label) + unit(6, "mm"),
                          just = "left",
                          name = "fileLabel",
                          gp = gpar(fontsize = 10, fontface = "italic"),
                          vp = canvas$graphPath("canvas.header"))
    infosep <- linesGrob(x = unit(0:1, "npc"), y = unit(0, "npc"),
                         name = "infoSeparatorLine",
                         vp = canvas$graphPath("canvas.header"))
    dataLabelBackground <- rectGrob(x = unit(0, "npc") + unit(1, "mm"),
                                    y = unit(1, "npc") - unit(1, "lines") - unit(1, "mm"),
                                    width = stringWidth(samplabel$label) + unit(2, "mm"),
                                    height = stringHeight(samplabel$label) + unit(2, "mm"),
                                    just = c("left", "bottom"),
                                    gp = gpar(col = "white", fill = "white", alpha = 0.7),
                                    name = "dataPlotLabelBackground",
                                    vp = canvas$graphPath("data"))
    resamplabel <- textGrob("Re-sample",
                            x = unit(0, "npc") + unit(1, "mm"),
                            y = unit(1, "npc") - unit(2, "mm"),
                            just = c("left", "top"),
                            name = "sampleLabel",
                            vp = canvas$graphPath("sample"),
                            gp = gpar(fontface = 2))
    resampLabelBackground <- rectGrob(x = unit(0, "npc") + unit(1, "mm"),
                                      y = unit(1, "npc") - unit(2, "mm"),
                                      width = stringWidth(resamplabel$label) + unit(4, "mm"),
                                      height = stringHeight(resamplabel$label) + unit(2, "mm"),
                                      just = c("left", "top"),
                                      gp = gpar(col = "white", fill = "white", alpha = 0.7),
                                      name = "resampLabelBackground",
                                      vp = canvas$graphPath("sample"))
    statlabel <- textGrob("Bootstrap distribution",
                          x = unit(0, "npc") + unit(1, "mm"),
                          y = unit(1, "npc") - unit(2, "lines"),
                          just = c("left", "top"),
                          name = "statLabel",
                          vp = canvas$graphPath("stat"),
                          gp = gpar(fontface = 2))
    bootlabels <- grobTree(methlabel, varlabel, filelabel,
                           infosep,
                           dataLabelBackground,
                           samplabel,
                           resampLabelBackground,
                           resamplabel, statlabel,
                           name = "bootlabels")
    canvas$image <- addGrob(canvas$image, bootlabels)
    canvas$image <- addGrob(canvas$image, segmentsGrob
                            (x0 = unit(rep(1.5, 2), "lines"),
                             x1 = unit.c(unit(1.5, "lines"), unit(0.9, "npc")),
                             y0 = unit(rep(2/3, 2), "npc") + unit(rep(1.5, 2), "lines"),
                             y1 = unit(rep(2/3, 2), "npc") + unit.c(unit(0.9/3, "npc"),
                             unit(1.5, "lines")),
                             arrow = arrow(length = unit(0.075, "inches")),
                             vp = canvas$graphPath("stat.extrabox"),
                             name = "dataPlot.labelaxes.1"))
    canvas$image <- addGrob(canvas$image, textGrob
                            (label = canvas$x.name, x = 0.5,
                             y = unit(2/3, "npc") + unit(1.25, "lines"),
                             just = "top",
                             vp = canvas$graphPath("stat.extrabox"),
                             name = "dataPlot.xlabel.1"))
    canvas$image <- addGrob(canvas$image, textGrob
                            (label = canvas$y.name, x = unit(0.25, "lines"),
                             y = 5/6, just = "top", rot = 90,
                             vp = canvas$graphPath("stat.extrabox"),
                             name = "dataPlot.ylabel.1"))
    x <- canvas$x
    y <- canvas$levels
    slope <- coefficients(lm(y ~ x))[2]
    canvas$image <- addGrob(canvas$image, textGrob
                            (label = c("slope =", format(round(slope, canvas$dp),
                             nsmall = canvas$dp)),
                             x = 0.5, y = unit(5/6, "npc") + unit(c(0.5, -0.5), "lines"),
                             gp = gpar(col = c("black", "red"), fontface = 1:2),
                             vp = canvas$graphPath("stat.extrabox"),
                             name = "dataPlot.slopetext.1"))
    ## Sample textbox title.
    canvas$image <- addGrob(canvas$image, datatextGrob
                            (data = "", title = "Re-sample",
                             x = 0.5,
                             name = "sample.title",
                             vp = canvas$graphPath("databox", 2)))
}

slope1000boot <- function(canvas, e, points) {
    slopes <- canvas$stat.dist[, 2]
    x <- canvas$x
    y <- canvas$levels
    if.plotted <- rep(c(TRUE, rep(FALSE, 8)), length.out = 1000)[order(order(slopes))]
    x0 <- rep(0, 1000)
    x1 <- rep(1, 1000)
    y0 <- unit(rep(0, 1000), "native")
    y1 <- unit(slopes, "native")
    if (diff(range(x)) == 0){
        x.scale <- range(canvas$x) + c(-1, 1)
    } else {
        x.scale <- range(canvas$x) + c(-0.04, 0.04)*diff(range(x))
    }
    n <- 1
    runs <- if (if.plotted[1000])
                which(if.plotted)
            else
                c(which(if.plotted), 1000)
    canvas$image <- addGrob(canvas$image, segmentsGrob
                            (x0 = unit(rep(1.5, 2), "lines"),
                             x1 = unit.c(unit(1.5, "lines"), unit(0.9, "npc")),
                             y0 = unit(rep(1/3, 2), "npc") + unit(rep(1.5, 2), "lines"),
                             y1 = unit(rep(1/3, 2), "npc") + unit.c(unit(0.9/3, "npc"),
                             unit(1.5, "lines")),
                             arrow = arrow(length = unit(0.075, "inches")),
                             vp = canvas$graphPath("stat.extrabox"),
                             name = "samplePlot.labelaxes.1"))
    canvas$image <- addGrob(canvas$image, textGrob
                            (label = canvas$x.name, x = 0.5,
                             y = unit(1/3, "npc") + unit(1.25, "lines"),
                             just = "top",
                             vp = canvas$graphPath("stat.extrabox"),
                             name = "samplePlot.xlabel.1"))
    canvas$image <- addGrob(canvas$image, textGrob
                            (label = canvas$y.name, x = unit(0.25, "lines"),
                             y = 0.5, just = "top", rot = 90,
                             vp = canvas$graphPath("stat.extrabox"),
                             name = "samplePlot.ylabel.1"))
    for (i in runs) {
        plot.symb <- table(canvas$indexes[[i]])
        plot.ind <- as.numeric(names(plot.symb))
        xpoints <- x[plot.ind]
        ypoints <- as.numeric(canvas$levels)[plot.ind]
        text1 <- as.character(format(round(canvas$samples[[i]], canvas$dp),
                                     nsmall = canvas$dp))
        text2 <- as.character(format(round(as.numeric(canvas$level.samples[[i]]), canvas$dp),
                                     nsmall = canvas$dp))
        canvas$image <- addGrob(canvas$image, datatextGrob
                                (data = text2, title = "",
                                 x = 0.75, max = 50,
                                 name = "samplePlot.databox.text.2",
                                 vp = canvas$graphPath("databox", 2)))
        canvas$image <- addGrob(canvas$image, datatextGrob
                                (data = text1, title = "",
                                 x = 0.25, max = 50,
                                 name = "samplePlot.databox.text.1",
                                 vp = canvas$graphPath("databox", 2)))
        plotFreqScatterPlot(canvas, xpoints, ypoints, plot.symb, "sample", "blue")
        canvas$image <- addGrob(canvas$image, segmentsGrob
                                (x0 = x0[1:i][if.plotted[1:i]], x1 = x1[1:i][if.plotted[1:i]],
                                 y0 = y0[1:i][if.plotted[1:i]], y1 = y1[1:i][if.plotted[1:i]],
                                 gp = gpar(col = "skyblue2"), name = "statPlot.statlines.1",
                                 vp = canvas$graphPath("stat")))
        canvas$image <- addGrob(canvas$image, linesGrob
                                (x = 0:1,
                                 y = unit(c(0, slopes[i]), "native"),
                                 gp = gpar(lwd = 2, col = "blue"), name = "statPlot.line.1",
                                 vp = canvas$graphPath("stat")))
        sampleAxis <- getGrob(canvas$image, gPath("sampleAxis"))
        canvas$image <- addGrob(canvas$image, sampleAxis)
        statlabel <- getGrob(canvas$image, gPath("statLabel"))
        canvas$image <- addGrob(canvas$image, statlabel)
        if (points){
            plotPoints(canvas, canvas$stat.ypos[1:i]*0.8 + 0.2, unit(slopes[1:i], "native")*(1/3),
                       vp = canvas$graphPath("stat.extrabox"), name = "statPlot", alpha = 0.7)
        }
        slope <- slopes[i]
        canvas$image <- addGrob(canvas$image, textGrob
                                (label = c("slope =", format(round(slope, canvas$dp),
                                 nsmall = canvas$dp)),
                                 x = 0.5, y = unit(0.5, "npc") + unit(c(0.5, -0.5), "lines"),
                                 gp = gpar(col = c("black", "red"), fontface = 1:2),
                                 vp = canvas$graphPath("stat.extrabox"),
                                 name = "samplePlot.slopetext.1"))
        if (canvas$stopAnimation)
            return()
        canvas$drawImage()
        n <- n + 1
    }
    canvas$sampled.stats <- NULL
    canvas$plotted.stats <- NULL
    canvas$rmGrobs(c("statPlot.line.1", "samplePlot.databox.text.2",
                     "samplePlot.databox.text.1", "samplePlot.points.1",
                     "samplePlot.line.1", "samplePlot.ylabel.1", "samplePlot.xlabel.1",
                     "samplePlot.labelaxes.1", "samplePlot.slopetext.1"))
    if (canvas$stopAnimation)
        return()
    canvas$drawImage()
    canvas$rmGrobs(c("statPlot.statlines.1", "samplePlot.ghosts.1", "statPlot.points.2"))
}

slope1000bootAlpha <- function(canvas, e, points) {
    slopes <- canvas$stat.dist[, 2]
    x <- canvas$x
    y <- canvas$levels
    x0 <- rep(0, 1000)
    x1 <- rep(1, 1000)
    y0 <- unit(rep(0, 1000), "native")
    y1 <- unit(slopes, "native")
    if (diff(range(x)) == 0){
        x.scale <- range(canvas$x) + c(-1, 1)
    } else {
        x.scale <- range(canvas$x) + c(-0.04, 0.04)*diff(range(x))
    }
    n <- 1
    canvas$image <- addGrob(canvas$image, segmentsGrob
                            (x0 = unit(rep(1.5, 2), "lines"),
                             x1 = unit.c(unit(1.5, "lines"), unit(0.9, "npc")),
                             y0 = unit(rep(1/3, 2), "npc") + unit(rep(1.5, 2), "lines"),
                             y1 = unit(rep(1/3, 2), "npc") + unit.c(unit(0.9/3, "npc"),
                             unit(1.5, "lines")),
                             arrow = arrow(length = unit(0.075, "inches")),
                             vp = canvas$graphPath("stat.extrabox"),
                             name = "samplePlot.labelaxes.1"))
    canvas$image <- addGrob(canvas$image, textGrob
                            (label = canvas$x.name, x = 0.5,
                             y = unit(1/3, "npc") + unit(1.25, "lines"),
                             just = "top",
                             vp = canvas$graphPath("stat.extrabox"),
                             name = "samplePlot.xlabel.1"))
    canvas$image <- addGrob(canvas$image, textGrob
                            (label = canvas$y.name, x = unit(0.25, "lines"),
                             y = 0.5, just = "top", rot = 90,
                             vp = canvas$graphPath("stat.extrabox"),
                             name = "samplePlot.ylabel.1"))
    for (i in seq(1, 1000, 50)) {
        plot.symb <- table(canvas$indexes[[i]])
        plot.ind <- as.numeric(names(plot.symb))
        xpoints <- x[plot.ind]
        ypoints <- as.numeric(canvas$levels)[plot.ind]
        text1 <- as.character(format(round(canvas$samples[[i]], canvas$dp),
                                     nsmall = canvas$dp))
        text2 <- as.character(format(round(as.numeric(canvas$level.samples[[i]]), canvas$dp),
                                     nsmall = canvas$dp))
        canvas$image <- addGrob(canvas$image, datatextGrob
                                (data = text2, title = "",
                                 x = 0.75, max = 50,
                                 name = "samplePlot.databox.text.2",
                                 vp = canvas$graphPath("databox", 2)))
        canvas$image <- addGrob(canvas$image, datatextGrob
                                (data = text1, title = "",
                                 x = 0.25, max = 50,
                                 name = "samplePlot.databox.text.1",
                                 vp = canvas$graphPath("databox", 2)))
        plotFreqScatterPlot(canvas, xpoints, ypoints, plot.symb, "sample", "blue")
        line.filter <- rep(c(TRUE, rep(FALSE, 8)), length.out = 1000) 
        canvas$image <- addGrob(canvas$image, segmentsGrob
                                (x0 = x0[1:i][line.filter[1:i]], x1 = x1[1:i][line.filter[1:i]],
                                 y0 = y0[1:i][line.filter[1:i]], y1 = y1[1:i][line.filter[1:i]],
                                 gp = gpar(col = "blue", alpha = 0.2), name = "statPlot.statlines.1",
                                 vp = canvas$graphPath("stat")))
        canvas$image <- addGrob(canvas$image, linesGrob
                                (x = 0:1,
                                 y = unit(c(0, slopes[i]), "native"),
                                 gp = gpar(lwd = 2, col = "blue"), name = "statPlot.line.1",
                                 vp = canvas$graphPath("stat")))
        sampleAxis <- getGrob(canvas$image, gPath("sampleAxis"))
        canvas$image <- addGrob(canvas$image, sampleAxis)
        statlabel <- getGrob(canvas$image, gPath("statLabel"))
        canvas$image <- addGrob(canvas$image, statlabel)
        if (points){
            plotPoints(canvas, canvas$stat.ypos[1:i]*0.8 + 0.2, unit(slopes[1:i], "native")*(1/3),
                       vp = canvas$graphPath("stat.extrabox"), name = "statPlot", alpha = 0.7)
        }
        slope <- slopes[i]
        canvas$image <- addGrob(canvas$image, textGrob
                                (label = c("slope =", format(round(slope, canvas$dp),
                                 nsmall = canvas$dp)),
                                 x = 0.5, y = unit(0.5, "npc") + unit(c(0.5, -0.5), "lines"),
                                 gp = gpar(col = c("black", "red"), fontface = 1:2),
                                 vp = canvas$graphPath("stat.extrabox"),
                                 name = "samplePlot.slopetext.1"))
        if (canvas$stopAnimation)
            return()
        canvas$drawImage()
        n <- n + 1
    }
    canvas$sampled.stats <- NULL
    canvas$plotted.stats <- NULL
    canvas$rmGrobs(c("statPlot.line.1", "samplePlot.databox.text.2",
                     "samplePlot.databox.text.1", "samplePlot.points.1",
                     "samplePlot.line.1", "samplePlot.ylabel.1", "samplePlot.xlabel.1",
                     "samplePlot.labelaxes.1", "samplePlot.slopetext.1"))
    if (canvas$stopAnimation)
        return()
    canvas$drawImage()
    canvas$rmGrobs(c("statPlot.statlines.1", "samplePlot.ghosts.1", "statPlot.points.2"))
}

animateSampleBootSlope <- function(canvas, e, drop.points = FALSE, n.steps = 10, n.slow = 5, max = 50, slope.only = FALSE){
    e$clearPanel("sample")
    canvas$rmGrobs("statPlot.line.1")
    canvas$sampled.stats <- unique(c(canvas$sampled.stats, canvas$which.sample))
    index <- canvas$indexes[[canvas$which.sample]]
    x <- canvas$x[index]
    n <- canvas$n
    samptextx <- as.character(format(round(x, canvas$dp), nsmall = canvas$dp))
    levels <- as.numeric(canvas$levels[index])
    samptexty <- as.character(format(round(levels, canvas$dp), nsmall = canvas$dp))
    datatexttitle <- datatextGrob(data = "", title = "Re-sample", name = "samplePlot.resamp.text",
                                  vp = canvas$graphPath("databox", 2))
    canvas$image <- addGrob(canvas$image, datatexttitle)
    ## Initialising datatext grobs
    canvas$image <- addGrob(canvas$image, datatextGrob
                            (data = numeric(0), title = "", x = 0.25,
                             gp = gpar(col = "red"), max = max,
                             name = "samplePlot.databox.text.1",
                             vp = canvas$graphPath("databox", 2)))
    canvas$image <- addGrob(canvas$image, datatextGrob
                            (data = numeric(0), title = "", x = 0.75,
                             gp = gpar(col = "red"), max = max,
                             name = "samplePlot.databox.text.2",
                             vp = canvas$graphPath("databox", 2)))
    if (n < 100){
        if (!slope.only){
            n.slow <- min(n.slow, n)
            ## Calculating the position of text in text boxes.
            ntext <- min(n, max)
            npcs <- (ntext:0)/ntext
            yunit <- unit(npcs, "npc") - unit(4*(npcs - 0.5), "mm") + unit(1 - npcs, "lines")
            x.text.start <- c(0.25, 0.75)/2
            x.text.end <- 0.5 + x.text.start
            x.text.step <- (x.text.end[1] - x.text.start[1])/n.steps
            ## Animation of slow points.
            for (i in seq(from = 1, by = 1, length.out = n.slow)){
                ind <- min(c(index[i], max))
                if (ind >= max){
                    datatextx <- format(round(c(canvas$x[1:(max - 1)], canvas$x[index[i]]), canvas$dp),
                                        nsmall = canvas$dp)
                    datatexty <- format(round(c(canvas$levels[1:(max - 1)], canvas$levels[index[i]]),
                                              canvas$dp), nsmall = canvas$dp)
                } else {
                    datatextx <- format(round(canvas$x, canvas$dp), nsmall = canvas$dp)
                    datatexty <- format(round(canvas$levels, canvas$dp), nsmall = canvas$dp)
                }
                y.text.start <- rep(yunit[min(c(max + 1, ind + 1))], 2)
                y.text.end <- rep(yunit[i + 1], 2)
                y.text.step <- convertY(y.text.end - y.text.start, "npc", valueOnly = TRUE)/n.steps
                cols <- c(rep("black", ind), "red", rep("black", ntext - ind))
                fface <- c(rep(1, ind), 2, rep(1, ntext - ind))
                canvas$image <- editGrob(canvas$image, gPath("text.sample1"),
                                         data = datatextx, gp = gpar(col = cols, fontface = fface))
                canvas$image <- editGrob(canvas$image, gPath("text.sample2"),
                                         data = datatexty, gp = gpar(col = cols, fontface = fface))
                if (drop.points){
                    x.point <- unit(x[i], "native")
                    y.start <- unit(levels[i], "native")
                    y.end <- y.start - unit(1, "npc")
                    y.step <- (y.end - y.start)*(1/n.steps)
                    canvas$image <- addGrob(canvas$image,
                                            pointsGrob(x = x.point, y = y.start,
                                                       vp = canvas$graphPath("data"),
                                                       name = "samplePlot.temppoint.1",
                                                       gp = gpar(col = "red", lwd = 2), pch = 19))
                }
                if (canvas$stopAnimation)
                    return()
                canvas$pauseImage(5)
                for (j in 1:(n.steps)){
                    canvas$image <- editGrob(canvas$image, gPath("text.sample1"),
                                             gp = gpar(fontface = 1))
                    canvas$image <- editGrob(canvas$image, gPath("text.sample2"),
                                             gp = gpar(fontface = 1))
                    ## Move text and points
                    canvas$image <- addGrob(canvas$image, textGrob
                                            (label = c(format(round(x[i], canvas$dp),
                                             nsmall = canvas$dp),
                                             format(round(levels[i], canvas$dp), nsmall = canvas$dp)),
                                             y = y.text.start + unit(j*y.text.step, "npc"),
                                             x = unit(x.text.start + j*x.text.step, "npc"),
                                             just = "top", gp = gpar(col = "red", fontface = 2),
                                             name = "samplePlot.temp.text",
                                             vp = vpPath("canvas.all", "canvas.boxes")))
                    if (drop.points){
                        canvas$image <- addGrob(canvas$image,
                                                pointsGrob(x = x.point, y = y.start + j*y.step,
                                                           vp = canvas$graphPath("data"),
                                                           name = "samplePlot.temppoint.1",
                                                           gp = gpar(col = "red", lwd = 2), pch = 19))
                    }
                    if (canvas$stopAnimation)
                        return()
                    canvas$drawImage()
                }
                canvas$rmGrobs(c("samplePlot.temp.text", "samplePlot.temppoint.1"))
                ## Making points permanent
                if (drop.points){
                    plot.symb <- table(index[1:i])
                    plot.ind <- as.numeric(names(plot.symb))
                    temp.x <- canvas$x[plot.ind]
                    temp.y <- as.numeric(canvas$levels)[plot.ind]
                    latest.point <- plot.ind == index[i]
                    cols <- rep("grey60", length(plot.ind))
                    cols[latest.point] <- "red"
                    textcols <- cols[plot.symb > 1]
                    pointcols <- cols[plot.symb == 1]
                    any.text <- any(plot.symb > 1)
                    any.points <- any(plot.symb == 1)
                    if (any.text){
                        text <- textGrob(label = plot.symb[plot.symb > 1], x = temp.x[plot.symb > 1],
                                         y = temp.y[plot.symb > 1],
                                         default.units = "native", gp = gpar(lwd = 2, col = textcols),
                                         vp = canvas$graphPath("sample"))
                    } else {
                        text <- NULL
                    }
                    if (any.points){
                        points <- pointsGrob(x = temp.x[plot.symb == 1], y = temp.y[plot.symb == 1],
                                             default.units = "native",
                                             gp = gpar(lwd = 2, col = pointcols),
                                             vp = canvas$graphPath("sample"))
                    } else {
                        points <- NULL
                    }
                    textpoints <- grobTree(gList(text, points),
                                           name = "samplePlot.points.1")
                    canvas$image <- addGrob(canvas$image, textpoints)
                    ## canvas$image <- addGrob(canvas$image, textGrob
                    ##             (label = plot.symb, x = temp.x, y = temp.y, default.units = "native",
                    ##              name = "samplePlot.points.1", gp = gpar(col = cols),
                    ##              vp = canvas$graphPath("sample")))
                }
                canvas$image <- addGrob(canvas$image, datatextGrob
                                        (data = c(samptextx[1:i], rep("", ntext - i)), title = "", x = 0.25,
                                         gp = gpar(col = "red", fontface = c(rep(1, i), 2)), max = max,
                                         name = "samplePlot.databox.text.1", vp = canvas$graphPath("databox", 2)))
                canvas$image <- addGrob(canvas$image, datatextGrob
                                        (data = c(samptexty[1:i], rep("", ntext - i)), title = "", x = 0.75,
                                         gp = gpar(col = "red", fontface = c(rep(1, i), 2)), max = max,
                                         name = "samplePlot.databox.text.2", vp = canvas$graphPath("databox", 2)))
                canvas$pauseImage(5)
                canvas$image <- editGrob(canvas$image, gPath("samplePlot.databox.text.1"),
                                         gp = gpar(fontface = 1))
                canvas$image <- editGrob(canvas$image, gPath("samplePlot.databox.text.2"),
                                         gp = gpar(fontface = 1))
                if (drop.points){
                    if (any.text){
                        text <- textGrob(label = plot.symb[plot.symb > 1], x = temp.x[plot.symb > 1],
                                         y = temp.y[plot.symb > 1],
                                         default.units = "native", gp = gpar(lwd = 2, col = "grey60"),
                                         vp = canvas$graphPath("sample"))
                    } else {
                        text <- NULL
                    }
                    if (any.points){
                        points <- pointsGrob(x = temp.x[plot.symb == 1], y = temp.y[plot.symb == 1],
                                             default.units = "native",
                                             gp = gpar(lwd = 2, col = "grey60"),
                                             vp = canvas$graphPath("sample"))
                    } else {
                        points <- NULL
                    }
                    textpoints <- grobTree(gList(text, points),
                                           name = "samplePlot.points.1")
                    canvas$image <- addGrob(canvas$image, textpoints)
                    ## canvas$image <- editGrob(canvas$image, gPath("samplePlot.points.1"),
                    ##                          gp = gpar(col = "black"))
                }
            }
            ## Animation of fast points.
            if (n == ntext) length.out <- ntext - n.slow else length.out <- ntext - n.slow - 1
            for (i in seq(from = n.slow + 1, by = 1, length.out = length.out)){
                ind <- min(c(index[i], max))
                if (ind >= max){
                    datatextx <- format(round(c(canvas$x[1:(max - 1)], canvas$x[index[i]]), canvas$dp),
                                        nsmall = canvas$dp)
                    datatexty <- format(round(c(canvas$levels[1:(max - 1)], canvas$levels[index[i]]),
                                              canvas$dp), nsmall = canvas$dp)
                } else {
                    datatextx <- format(round(canvas$x, canvas$dp), nsmall = canvas$dp)
                    datatexty <- format(round(canvas$levels, canvas$dp), nsmall = canvas$dp)
                }
                cols <- c(rep("black", ind), "red", rep("black", ntext - ind))
                fface <- c(rep(1, ind), 2, rep(1, ntext - ind))
                canvas$image <- editGrob(canvas$image, gPath("text.sample1"),
                                         data = datatextx, gp = gpar(col = cols, fontface = fface))
                canvas$image <- editGrob(canvas$image, gPath("text.sample2"),
                                         data = datatexty, gp = gpar(col = cols, fontface = fface))
                canvas$image <- editGrob(canvas$image, gPath("samplePlot.databox.text.1"),
                                         data = c(samptextx[1:i], rep("", ntext - i)),
                                         gp = gpar(col = "red", fontface = c(rep(1, i), 2)))
                canvas$image <- editGrob(canvas$image, gPath("samplePlot.databox.text.2"),
                                         data = c(samptexty[1:i], rep("", ntext - i)),
                                         gp = gpar(col = "red", fontface = c(rep(1, i), 2)))
                if (drop.points){
                    x.point <- unit(x[i], "native")
                    y.start <- unit(levels[i], "native")
                    canvas$image <- addGrob(canvas$image,
                                            pointsGrob(x = x.point, y = y.start,
                                                       vp = canvas$graphPath("data"),
                                                       name = "samplePlot.temppoint.1",
                                                       gp = gpar(col = "red", lwd = 2), pch = 19))
                    plot.symb <- table(index[1:i])
                    plot.ind <- as.numeric(names(plot.symb))
                    temp.x <- canvas$x[plot.ind]
                    temp.y <- as.numeric(canvas$levels)[plot.ind]
                    latest.point <- plot.ind == index[i]
                    cols <- rep("grey60", length(plot.ind))
                    cols[latest.point] <- "red"
                    textcols <- cols[plot.symb > 1]
                    pointcols <- cols[plot.symb == 1]
                    any.text <- any(plot.symb > 1)
                    any.points <- any(plot.symb == 1)
                    if (any.text){
                        text <- textGrob(label = plot.symb[plot.symb > 1], x = temp.x[plot.symb > 1],
                                         y = temp.y[plot.symb > 1],
                                         default.units = "native", gp = gpar(lwd = 2, col = textcols),
                                         vp = canvas$graphPath("sample"))
                    } else {
                        text <- NULL
                    }
                    if (any.points){
                        points <- pointsGrob(x = temp.x[plot.symb == 1], y = temp.y[plot.symb == 1],
                                             default.units = "native",
                                             gp = gpar(lwd = 2, col = pointcols),
                                             vp = canvas$graphPath("sample"))
                    } else {
                        points <- NULL
                    }
                    textpoints <- grobTree(gList(text, points),
                                           name = "samplePlot.points.1")
                    canvas$image <- addGrob(canvas$image, textpoints)
                }
                if (i <= (n.slow + 10)){
                    canvas$pauseImage(5)
                } else {
                    canvas$drawImage()
                }
            }
            ## Animation of points outside databox.
            sampfface <- c(rep(1, max), 2)
            for (i in seq(from = ntext, by = 1, length.out = n - ntext)){
                ind <- min(c(index[i], max))
                if (ind >= max){
                    datatextx <- format(round(c(canvas$x[1:(max - 1)], canvas$x[index[i]]), canvas$dp),
                                        nsmall = canvas$dp)
                    datatexty <- format(round(c(canvas$levels[1:(max - 1)], canvas$levels[index[i]]),
                                              canvas$dp), nsmall = canvas$dp)
                } else {
                    datatextx <- format(round(canvas$x, canvas$dp), nsmall = canvas$dp)
                    datatexty <- format(round(canvas$levels, canvas$dp), nsmall = canvas$dp)
                }
                cols <- c(rep("black", ind), "red", rep("black", ntext - ind))
                fface <- c(rep(1, ind), 2, rep(1, ntext - ind))
                samptextx.out <- c(samptextx[1:(max - 1)], samptextx[i])
                samptexty.out <- c(samptexty[1:(max - 1)], samptexty[i])
                canvas$image <- editGrob(canvas$image, gPath("text.sample1"),
                                         data = datatextx, gp = gpar(col = cols, fontface = fface))
                canvas$image <- editGrob(canvas$image, gPath("text.sample2"),
                                         data = datatexty, gp = gpar(col = cols, fontface = fface))
                canvas$image <- editGrob(canvas$image, gPath("samplePlot.databox.text.1"),
                                         data = samptextx.out,
                                         gp = gpar(col = "red", fontface = sampfface))
                canvas$image <- editGrob(canvas$image, gPath("samplePlot.databox.text.2"),
                                         data = samptexty.out,
                                         gp = gpar(col = "red", fontface = sampfface))
                if (drop.points){
                    x.point <- unit(x[i], "native")
                    y.start <- unit(levels[i], "native")
                    canvas$image <- addGrob(canvas$image,
                                            pointsGrob(x = x.point, y = y.start,
                                                       vp = canvas$graphPath("data"),
                                                       name = "samplePlot.temppoint.1",
                                                       gp = gpar(col = "red", lwd = 2), pch = 19))
                    plot.symb <- table(index[1:i])
                    plot.ind <- as.numeric(names(plot.symb))
                    temp.x <- canvas$x[plot.ind]
                    temp.y <- as.numeric(canvas$levels)[plot.ind]
                    latest.point <- plot.ind == index[i]
                    cols <- rep("grey60", length(plot.ind))
                    cols[latest.point] <- "red"
                    textcols <- cols[plot.symb > 1]
                    pointcols <- cols[plot.symb == 1]
                    any.text <- any(plot.symb > 1)
                    any.points <- any(plot.symb == 1)
                    if (any.text){
                        text <- textGrob(label = plot.symb[plot.symb > 1], x = temp.x[plot.symb > 1],
                                         y = temp.y[plot.symb > 1],
                                         default.units = "native", gp = gpar(lwd = 2, col = textcols),
                                         vp = canvas$graphPath("sample"))
                    } else {
                        text <- NULL
                    }
                    if (any.points){
                        points <- pointsGrob(x = temp.x[plot.symb == 1], y = temp.y[plot.symb == 1],
                                             default.units = "native",
                                             gp = gpar(lwd = 2, col = pointcols),
                                             vp = canvas$graphPath("sample"))
                    } else {
                        points <- NULL
                    }
                textpoints <- grobTree(gList(text, points),
                                       name = "samplePlot.points.1")
                    canvas$image <- addGrob(canvas$image, textpoints)
                }
                canvas$drawImage()
            }
        }
        ## Plotting final image.
        datatextx <- format(round(canvas$x, canvas$dp), nsmall = canvas$dp)
        datatexty <- format(round(canvas$levels, canvas$dp), nsmall = canvas$dp)
        canvas$image <- editGrob(canvas$image, gPath("text.sample1"),
                                 data = datatextx,
                                 gp = gpar(fontface = 1, col = "black"))
        canvas$image <- editGrob(canvas$image, gPath("text.sample2"),
                                 data = datatexty,
                                  gp = gpar(fontface = 1, col = "black"))
        canvas$image <- editGrob(canvas$image, gPath("samplePlot.databox.text.1"),
                                 data = samptextx, gp = gpar(fontface = 1))
        canvas$image <- editGrob(canvas$image, gPath("samplePlot.databox.text.2"),
                                 data = samptexty, gp = gpar(fontface = 1))
        canvas$rmGrobs("samplePlot.temppoint.1")
        ## Plotting sample in sample panel.
        plotMiddleSampleOnly(canvas, e, canvas$which.sample)
        canvas$showLabels()
        if (slope.only){
            canvas$drawImage()
        } else {
            canvas$pauseImage(10)
        }
        ## Dropping slope.
        ## Calculating all plotted slopes.
        stat <- canvas$stat.dist[[canvas$which.sample]][2]
        slopes <- canvas$stat.dist[, 2][canvas$sampled.stats]
        slopes <- c(slopes, stat)
        y.bounds <- canvas$x.scale.ys[canvas$which.sample, ]
        seekViewport(canvas$graphPath("sample"))
        sampunit <- convertY(unit(y.bounds, "native"), "npc", valueOnly = TRUE)
        seekViewport("animation.field2")
        seekViewport(canvas$graphPath("stat"))
        y.end <- convertY(unit(0, "native"), "npc", valueOnly = TRUE)/3
        seekViewport("animation.field2")
        y.start <- 1/3 + sampunit/3
        y.end <- c(y.end, y.end + diff(y.start))
        y.step <- (y.end - y.start)/n.steps
        for (i in 0:n.steps){
            canvas$image <- addGrob(canvas$image, linesGrob
                                    (x = 0:1, y = y.start + i*y.step,
                                     gp = gpar(lwd = 2, col = "blue"), name = "statPlot.line.1",
                                     vp = canvas$graphPath("animation.field")))
            canvas$drawImage()
        }
        canvas$plotSample(e, canvas$which.sample)
        if (!slope.only){
            canvas$pauseImage(20)
        }
    }
}

trackBootstrapSlope <- function(canvas){
    ## No tracking if sample size is greater than 100.
    if (canvas$n < 100){
        index <- canvas$indexes[[canvas$which.sample]]
        sample <- canvas$x
        levels <- as.numeric(canvas$levels)
        resample <- sample[index]
        resample.levels <- levels[index]
        n <- canvas$n
        ntext <- min(n, 50)
        npcs <- (ntext:0)/ntext
        ## Calculating y-positions of values in databoxes.
        yunit <- (unit(npcs, "npc") - unit(4*(npcs - 0.5), "mm") + unit(1 - npcs, "lines"))
        ## Calculating maximum width of values.
        max.width.x <- max(convertX(stringWidth(format(round(sample, canvas$dp),
                                                       nsmall = canvas$dp)),
                                    "cm", valueOnly = TRUE))
        max.width.y <- max(convertX(stringWidth(format(round(levels, canvas$dp),
                                                       nsmall = canvas$dp)),
                                    "cm", valueOnly = TRUE))
        max.width.x <- unit(max.width.x, "cm")
        max.width.y <- unit(max.width.y, "cm")
        ## Setting the final observation to track (don't track anything outside the databox).
        if (n <= 50) finalobs <- n else finalobs <- 49
        for (i in 1:finalobs){
            value <- sample[i]
            lev <- levels[i]
            cols <- c(rep("black", i), "red", rep("black", ntext - i))
            fface <- c(rep(1, i), 2, rep(1, ntext - i))
            ## Turning appropriate value red.
            canvas$image <- editGrob(canvas$image, gPath("text.sample1"),
                                     gp = gpar(col = cols, fontface = fface))
            canvas$image <- editGrob(canvas$image, gPath("text.sample2"),
                                     gp = gpar(col = cols, fontface = fface))
            resamp.ys <- c(FALSE, index == i)
            samptextx <- format(round(resample, canvas$dp), nsmall = canvas$dp)
            samptexty <- format(round(resample.levels, canvas$dp), nsmall = canvas$dp)
            x.point <- unit(canvas$x[i], "native")
            y.start <- unit(as.numeric(canvas$levels)[i], "native")
            ## Lighting up appropriate point in data panel.
            canvas$image <- addGrob(canvas$image,
                                    pointsGrob(x = x.point, y = y.start,
                                               vp = canvas$graphPath("data"),
                                               name = "samplePlot.temppoint.1",
                                               gp = gpar(col = "red", lwd = 2), pch = 19))
            ## If point has been resampled we want to light up point
            ## in sample plot and in sample databox.
            if (any(resamp.ys)){
                subset.resamp <- c(resamp.ys[1:(ntext + 1)])
                if (n != ntext){
                    subset.resamp <- c(resamp.ys[1:ntext], any(resamp.ys[(ntext + 1):(n + 1)]))
                    if (subset.resamp[length(subset.resamp)]){
                        samptextx <- c(samptextx[1:(50 - 1)],
                                       format(round(value, canvas$dp), nsmall = canvas$dp))
                        samptexty <- c(samptexty[1:(50 - 1)],
                                       format(round(lev, canvas$dp), nsmall = canvas$dp))
                    }
                }
                sampcol <- character(ntext + 1)
                sampcol[subset.resamp] <- "red"
                sampcol[!subset.resamp] <- "black"
                sampfface <- numeric(ntext + 1)
                sampfface[subset.resamp] <- 2
                sampfface[!subset.resamp] <- 1
                ## Lighting up values in sample databox.
                canvas$image <- editGrob(canvas$image, gPath("samplePlot.databox.text.1"),
                                         data = samptextx, gp = gpar(fontface = sampfface,
                                                           col = sampcol))
                canvas$image <- editGrob(canvas$image, gPath("samplePlot.databox.text.2"),
                                         data = samptexty, gp = gpar(fontface = sampfface,
                                                           col = sampcol))
                ## Plotting current sample plot.
                canvas$image <- addGrob(canvas$image, segmentsGrob
                                        (x0 = unit(0.375, "npc") + max.width.y*0.5 + unit(1, "mm"),
                                         x1 = unit(0.625, "npc") - max.width.x*0.5 - unit(1, "mm"),
                                         y0 = yunit[i + 1] - unit(0.5, "lines"),
                                         y1 = yunit[subset.resamp] - unit(0.5, "lines"),
                                         name = "samplePlot.temp.segments",
                                         gp = gpar(lwd = 2, col = "red"),
                                         vp = vpPath("canvas.all", "canvas.boxes")))
                plot.symb <- sum(index == i)
                x.pos <- canvas$x[i]
                y.pos <- as.numeric(canvas$levels)[i]
                if (plot.symb == 1){
                     canvas$image <- addGrob(canvas$image, pointsGrob
                                            (x = x.pos, y = y.pos,
                                             default.units = "native", pch = 19,
                                             name = "samplePlot.temptext.1",
                                             gp = gpar(col = "red"),
                                             vp = canvas$graphPath("sample")))
                } else {
                    canvas$image <- addGrob(canvas$image, textGrob
                                            (label = plot.symb, x = x.pos,
                                             y = y.pos,
                                             default.units = "native",
                                             name = "samplePlot.temptext.1",
                                             gp = gpar(col = "red"),
                                             vp = canvas$graphPath("sample")))
                }

                if (canvas$stopAnimation)
                    return()
                if (i > 10){
                    canvas$pauseImage(5)
                } else {
                    canvas$pauseImage(20)
                }
                canvas$rmGrobs("samplePlot.temp.segments")
                canvas$image <- editGrob(canvas$image, gPath("samplePlot.databox.text.1"),
                                         gp = gpar(fontface = 1))
                canvas$image <- editGrob(canvas$image, gPath("samplePlot.databox.text.2"),
                                         gp = gpar(fontface = 1))
            } else {
                canvas$image <- editGrob(canvas$image, gPath("samplePlot.databox.text.1"),
                                         data = samptextx, gp = gpar(fontface = 1, col = "black"))
                canvas$image <- editGrob(canvas$image, gPath("samplePlot.databox.text.2"),
                                         data = samptexty, gp = gpar(fontface = 1, col = "black"))
                canvas$rmGrobs(c("samplePlot.temptext.1"))
                if (canvas$stopAnimation)
                    return()
                if (i > 10){
                    canvas$pauseImage(5)
                } else {
                    canvas$pauseImage(20)
                }
            }
        }
        ## Cleaning up grobs that are no longer required.
        canvas$rmGrobs(c("samplePlot.temp.samp", "samplePlot.temp.level",
                         "samplePlot.temp.point", "samplePlot.points.",
                         "samplePlot.temptext.1", "samplePlot.temppoint.1"))
        canvas$image <- editGrob(canvas$image, gPath("text.sample1"),
                                 gp = gpar(col = "black", fontface = 1))
        canvas$image <- editGrob(canvas$image, gPath("text.sample2"),
                                 gp = gpar(col = "black", fontface = 1))
    }
}

showSlopeCI <- function(canvas, e, ci = TRUE, points = TRUE){
    ## Replotting what is shown following running 1000 repetitions.
    slopes <- canvas$stat.dist[, 2]
    x <- canvas$x
    y <- canvas$levels
    if.plotted <- rep(c(TRUE, rep(FALSE, 8)), length.out = 1000)[order(order(slopes))]
    x0 <- rep(0, 1000)[if.plotted]
    x1 <- rep(1, 1000)[if.plotted]
    y0 <- unit(rep(0, 1000)[if.plotted], "native")
    y1 <- unit(slopes[if.plotted], "native")
    coeffs <- coefficients(lm(y ~ x))
    sampleAxis <- getGrob(canvas$image, gPath("sampleAxis"))
    canvas$image <- addGrob(canvas$image, segmentsGrob
                            (x0 = x0, x1 = x1,
                             y0 = y0, y1 = y1,
                             gp = gpar(col = "skyblue2"),
                             name = "statPlot.statlines.1",
                             vp = canvas$graphPath("stat")))
    canvas$image <- addGrob(canvas$image, sampleAxis)
    statlabel <- getGrob(canvas$image, gPath("statLabel"))
    canvas$image <- addGrob(canvas$image, statlabel)
    plotPoints(canvas, canvas$stat.ypos*0.8 + 0.2, unit(slopes, "native")*(1/3),
               vp = canvas$graphPath("stat.extrabox"), name = "statPlot", alpha = 0.7)
    if (ci){
        ## Animation of the CI appearing.
        top.level <- downViewport(canvas$graphPath("data"))
        x.scale <- current.viewport()$xscale
        y.bounds <- coeffs[1] + coeffs[2]*x.scale
        dataunit <- convertY(unit(y.bounds, "native"), "npc", valueOnly = TRUE)
        upViewport(top.level)
        top.level <- downViewport(canvas$graphPath("stat"))
        y.end <- convertY(unit(0, "native"), "npc", valueOnly = TRUE)/3
        upViewport(top.level)
        n.steps <- 5
        y.start <- 2/3 + dataunit/3
        y.end <- c(y.end, y.end + diff(y.start))
        y.step <- (y.end - y.start)/n.steps
        ## Moving original regression line.
        for (i in 0:n.steps){
            canvas$image <- addGrob(canvas$image, linesGrob
                                    (x = 0:1, y = y.start + i*y.step,
                                     gp = gpar(lwd = 2, col = "purple3"), name = "statPlot.line.1",
                                     vp = canvas$graphPath("animation.field")))
            canvas$image <- addGrob(canvas$image, sampleAxis)
            if (i == n.steps){
                canvas$image <- addGrob(canvas$image, textGrob
                                        (label = format(round(coeffs[2], canvas$dp),
                                         nsmall = canvas$dp),
                                         gp = gpar(col = "red", fontface = 2),
                                         x = 0.01, y = unit(coeffs[2], "native")*(1/3),
                                         just = "left", name = "statPlot.stattext.1",
                                         vp = canvas$graphPath("stat.extrabox")))
            }
            canvas$drawImage()
        }
        ## Changing colours of points and ghost lines.
        ci <- quantile(slopes, c(0.025, 0.975), type = 1)
        cols <- rep("grey60", 1000)
        cols[slopes < ci[1] | slopes > ci[2]] <- "lightgrey"
        linecols <- rep("skyblue2", sum(if.plotted))
        linecols[slopes[if.plotted] < ci[1] | slopes[if.plotted] > ci[2]] <- "lightcyan2"
        canvas$image <- editGrob(canvas$image, gPath("statPlot.points.2"),
                                 gp = gpar(col = cols))
        canvas$image <- editGrob(canvas$image, gPath("statPlot.statlines.1"),
                                 gp = gpar(col = linecols))
        canvas$pauseImage(3)
        ## Introducing CI, arrows and text.
        canvas$image <- addGrob(canvas$image, segmentsGrob
                                (x0 = rep(0.4, 2), x1 = rep(0, 2),
                                 y0 = unit(ci, "native")*(1/3), y1 = unit(ci, "native")*(1/3),
                                 gp = gpar(col = "red", lwd = 2),
                                 arrow = arrow(length = unit(0.1, "inches")),
                                 vp = canvas$graphPath("stat.extrabox"), name = "statPlot.cilines.1"))
        canvas$pauseImage(3)
        canvas$image <- addGrob(canvas$image, rectGrob
                                (x = 0.4, y = unit(max(ci), "native")*(1/3),
                                 height = unit(diff(ci), "native")*(1/3),
                                 width = 0.05, just = c("right", "top"),
                                 gp = gpar(col = "red", fill = "red"),
                                 vp = canvas$graphPath("stat.extrabox"),
                                 name = "statPlot.ci.1"))
        canvas$pauseImage(3)
        canvas$image <- addGrob(canvas$image, textGrob
                                (label = format(round(ci, canvas$dp), nsmall = canvas$dp),
                                 x = unit(0.99, "npc"), y = unit(ci, "native"),
                                 gp = gpar(col = "red", fontface = 2), just = "right",
                                 name = "statPlot.citext.1",
                                 vp = canvas$graphPath("stat")))
        canvas$drawImage()
    } else {
        ## Redrawing what is shown if CI has already been displayed.
        if ("statPlot.ci.1" %in% childNames(canvas$image)){
            cilines <- getGrob(canvas$image, gPath("statPlot.cilines.1"))
            cirect <- getGrob(canvas$image, gPath("statPlot.ci.1"))
            citext <- getGrob(canvas$image, gPath("statPlot.citext.1"))
            canvas$image <- addGrob(canvas$image, cilines)
            canvas$image <- addGrob(canvas$image, cirect)
            canvas$image <- addGrob(canvas$image, citext)
            ci <- quantile(slopes, c(0.025, 0.975), type = 1)
            cols <- rep("grey60", 1000)
            cols[slopes < ci[1] | slopes > ci[2]] <- "lightgrey"
            linecols <- rep("skyblue2", sum(if.plotted))
            linecols[slopes[if.plotted] < ci[1] | slopes[if.plotted] > ci[2]] <- "lightcyan2"
            canvas$image <- editGrob(canvas$image, gPath("statPlot.points.2"),
                                 gp = gpar(col = cols))
            canvas$image <- editGrob(canvas$image, gPath("statPlot.statlines.1"),
                                     gp = gpar(col = linecols))
            canvas$image <- addGrob(canvas$image, getGrob(canvas$image, gPath("statPlot.line.1")))
        }
        ## Plotting bootstrap distribution stats.
        ## Summary stats code.
        mean.x <- round(mean(slopes), canvas$dp)
        sd.x <- round(sd(slopes), canvas$dp)
        ## Calculating maximum text line width.
        widths <- c(convertX(stringWidth("Mean:"), "cm", valueOnly = TRUE),
                    convertX(stringWidth(mean.x), "cm", valueOnly = TRUE),
                    convertX(stringWidth(sd.x), "cm", valueOnly = TRUE))
        max.width <- stringWidth(c("Mean:", mean.x, sd.x)[widths == max(widths)])
        xunit <- unit(1, "npc") - unit(1, "mm") - 0.5*max.width
        summarytext1 <- textGrob("Mean:", x = xunit, y = unit(1/3, "npc") - unit(0.5, "lines"),
                                 vp = canvas$graphPath("stat.extrabox"), gp = gpar(fontface = 2),
                                 name = "summarytext1")
        summarytext2 <- textGrob(format(round(mean.x, canvas$dp), nsmall = canvas$dp), x = xunit,
                                 y = unit(1/3, "npc") - unit(1.5, "lines"),
                                 vp = canvas$graphPath("stat.extrabox"),
                                 name = "summarytext2")
        summarytext3 <- textGrob("SD:",
                                 x = xunit, y = unit(1/3, "npc") - unit(2.5, "lines"),
                                 vp = canvas$graphPath("stat.extrabox"), gp = gpar(fontface = 2),
                                 name = "summarytext3")
        summarytext4 <- textGrob(format(round(sd.x, canvas$dp), nsmall = canvas$dp), x = xunit,
                                 y = unit(1/3, "npc") - unit(3.5, "lines"),
                                 vp = canvas$graphPath("stat.extrabox"),
                                 name = "summarytext4")
        summaryborder <- rectGrob(x = 1, y = unit(1/3, "npc"),
                                  width = max.width + unit(4, "mm"),
                                  height = unit(2, "mm") + unit(4, "lines"),
                                  gp = gpar(fill = "white"),
                                  just = c("right", "top"),
                                  vp = canvas$graphPath("stat.extrabox"),
                                  name = "summaryborder")
        summarytext <- grobTree(summaryborder, summarytext1, summarytext2, summarytext3,
                                summarytext4, name = "statPlot.summary.1")
        canvas$image <- addGrob(canvas$image,  summarytext)
        canvas$showLabels()
        canvas$drawImage()
    }
}

showSlopeCIAlpha <- function(canvas, e, ci = TRUE, points = TRUE) {
    ## Replotting what is shown following running 1000 repetitions.
    slopes <- canvas$stat.dist[, 2]
    x <- canvas$x
    y <- canvas$levels
    line.filter <- rep(c(TRUE, rep(FALSE, 8)), length.out = 1000) 
    x0 <- rep(0, 1000)[line.filter]
    x1 <- rep(1, 1000)[line.filter]
    y0 <- unit(rep(0, 1000)[line.filter], "native")
    y1 <- unit(slopes[line.filter], "native")
    coeffs <- coefficients(lm(y ~ x))
    sampleAxis <- getGrob(canvas$image, gPath("sampleAxis"))
    canvas$image <- addGrob(canvas$image, segmentsGrob
                            (x0 = x0, x1 = x1,
                             y0 = y0, y1 = y1,
                             gp = gpar(col = "blue", alpha = 0.2),
                             name = "statPlot.statlines.1",
                             vp = canvas$graphPath("stat")))
    canvas$image <- addGrob(canvas$image, sampleAxis)
    statlabel <- getGrob(canvas$image, gPath("statLabel"))
    canvas$image <- addGrob(canvas$image, statlabel)
    plotPoints(canvas, canvas$stat.ypos*0.8 + 0.2, unit(slopes, "native")*(1/3),
               vp = canvas$graphPath("stat.extrabox"), name = "statPlot", alpha = 0.7)
    if (ci){
        ## Animation of the CI appearing.
        top.level <- downViewport(canvas$graphPath("data"))
        x.scale <- current.viewport()$xscale
        y.bounds <- coeffs[1] + coeffs[2]*x.scale
        dataunit <- convertY(unit(y.bounds, "native"), "npc", valueOnly = TRUE)
        upViewport(top.level)
        top.level <- downViewport(canvas$graphPath("stat"))
        y.end <- convertY(unit(0, "native"), "npc", valueOnly = TRUE)/3
        upViewport(top.level)
        n.steps <- 5
        y.start <- 2/3 + dataunit/3
        y.end <- c(y.end, y.end + diff(y.start))
        y.step <- (y.end - y.start)/n.steps
        ## Moving original regression line.
        for (i in 0:n.steps){
            canvas$image <- addGrob(canvas$image, linesGrob
                                    (x = 0:1, y = y.start + i*y.step,
                                     gp = gpar(lwd = 4, col = "purple3", alpha = 1), name = "statPlot.line.1",
                                     vp = canvas$graphPath("animation.field")))
            canvas$image <- addGrob(canvas$image, sampleAxis)
            if (i == n.steps){
                canvas$image <- addGrob(canvas$image, textGrob
                                        (label = format(round(coeffs[2], canvas$dp),
                                         nsmall = canvas$dp),
                                         gp = gpar(col = "red", fontface = 2),
                                         x = 0.01, y = unit(coeffs[2], "native")*(1/3),
                                         just = "left", name = "statPlot.stattext.1",
                                         vp = canvas$graphPath("stat.extrabox")))
            }
            canvas$drawImage()
        }
        ## Changing colours of points and ghost lines.
        ci <- quantile(slopes, c(0.025, 0.975), type = 1)
        cols <- rep("grey60", 1000)
        cols[slopes < ci[1] | slopes > ci[2]] <- "#E5AA70"
        linecols <- rep("blue", 1000)
        linecols[slopes < ci[1] | slopes > ci[2]] <- "#E5AA70"
        linealphas <- rep(0.2, 1000)
        linealphas[slopes < ci[1] | slopes > ci[2]] <- 0.5
        canvas$image <- editGrob(canvas$image, gPath("statPlot.points.2"),
                                 gp = gpar(col = cols))
        canvas$image <- editGrob(canvas$image, gPath("statPlot.statlines.1"),
                                 gp = gpar(col = linecols[line.filter], alpha = linealphas[line.filter]))
        canvas$pauseImage(3)
        ## Introducing CI, arrows and text.
        canvas$image <- addGrob(canvas$image, segmentsGrob
                                (x0 = rep(0.4, 2), x1 = rep(0, 2),
                                 y0 = unit(ci, "native")*(1/3), y1 = unit(ci, "native")*(1/3),
                                 gp = gpar(col = "red", lwd = 2),
                                 arrow = arrow(length = unit(0.1, "inches")),
                                 vp = canvas$graphPath("stat.extrabox"), name = "statPlot.cilines.1"))
        canvas$pauseImage(3)
        canvas$image <- addGrob(canvas$image, rectGrob
                                (x = 0.4, y = unit(max(ci), "native")*(1/3),
                                 height = unit(diff(ci), "native")*(1/3),
                                 width = 0.05, just = c("right", "top"),
                                 gp = gpar(col = "red", fill = "red"),
                                 vp = canvas$graphPath("stat.extrabox"),
                                 name = "statPlot.ci.1"))
        canvas$pauseImage(3)
        canvas$image <- addGrob(canvas$image, textGrob
                                (label = format(round(ci, canvas$dp), nsmall = canvas$dp),
                                 x = unit(0.99, "npc"), y = unit(ci, "native"),
                                 gp = gpar(col = "red", fontface = 2), just = "right",
                                 name = "statPlot.citext.1",
                                 vp = canvas$graphPath("stat")))
        canvas$drawImage()
    } else {
        ## Redrawing what is shown if CI has already been displayed.
        if ("statPlot.ci.1" %in% childNames(canvas$image)){
            cilines <- getGrob(canvas$image, gPath("statPlot.cilines.1"))
            cirect <- getGrob(canvas$image, gPath("statPlot.ci.1"))
            citext <- getGrob(canvas$image, gPath("statPlot.citext.1"))
            canvas$image <- addGrob(canvas$image, cilines)
            canvas$image <- addGrob(canvas$image, cirect)
            canvas$image <- addGrob(canvas$image, citext)
            ci <- quantile(slopes, c(0.025, 0.975), type = 1)
            cols <- rep("grey60", 1000)
            cols[slopes < ci[1] | slopes > ci[2]] <- "#E5AA70"
            linecols <- rep("blue", 1000)
            linecols[slopes < ci[1] | slopes > ci[2]] <- "#E5AA70"
            linealphas <- rep(0.2, 1000)
            linealphas[slopes < ci[1] | slopes > ci[2]] <- 0.3
            canvas$image <- editGrob(canvas$image, gPath("statPlot.points.2"),
                                 gp = gpar(col = cols))
            canvas$image <- editGrob(canvas$image, gPath("statPlot.statlines.1"),
                                     gp = gpar(col = linecols[line.filter], alpha = linealphas[line.filter]))
            canvas$image <- addGrob(canvas$image, getGrob(canvas$image, gPath("statPlot.line.1")))
        }
        ## Plotting bootstrap distribution stats.
        ## Summary stats code.
        mean.x <- round(mean(slopes), canvas$dp)
        sd.x <- round(sd(slopes), canvas$dp)
        ## Calculating maximum text line width.
        widths <- c(convertX(stringWidth("Mean:"), "cm", valueOnly = TRUE),
                    convertX(stringWidth(mean.x), "cm", valueOnly = TRUE),
                    convertX(stringWidth(sd.x), "cm", valueOnly = TRUE))
        max.width <- stringWidth(c("Mean:", mean.x, sd.x)[widths == max(widths)])
        xunit <- unit(1, "npc") - unit(1, "mm") - 0.5*max.width
        summarytext1 <- textGrob("Mean:", x = xunit, y = unit(1/3, "npc") - unit(0.5, "lines"),
                                 vp = canvas$graphPath("stat.extrabox"), gp = gpar(fontface = 2),
                                 name = "summarytext1")
        summarytext2 <- textGrob(format(round(mean.x, canvas$dp), nsmall = canvas$dp), x = xunit,
                                 y = unit(1/3, "npc") - unit(1.5, "lines"),
                                 vp = canvas$graphPath("stat.extrabox"),
                                 name = "summarytext2")
        summarytext3 <- textGrob("SD:",
                                 x = xunit, y = unit(1/3, "npc") - unit(2.5, "lines"),
                                 vp = canvas$graphPath("stat.extrabox"), gp = gpar(fontface = 2),
                                 name = "summarytext3")
        summarytext4 <- textGrob(format(round(sd.x, canvas$dp), nsmall = canvas$dp), x = xunit,
                                 y = unit(1/3, "npc") - unit(3.5, "lines"),
                                 vp = canvas$graphPath("stat.extrabox"),
                                 name = "summarytext4")
        summaryborder <- rectGrob(x = 1, y = unit(1/3, "npc"),
                                  width = max.width + unit(4, "mm"),
                                  height = unit(2, "mm") + unit(4, "lines"),
                                  gp = gpar(fill = "white"),
                                  just = c("right", "top"),
                                  vp = canvas$graphPath("stat.extrabox"),
                                  name = "summaryborder")
        summarytext <- grobTree(summaryborder, summarytext1, summarytext2, summarytext3,
                                summarytext4, name = "statPlot.summary.1")
        canvas$image <- addGrob(canvas$image,  summarytext)
        canvas$showLabels()
        canvas$drawImage()
    }
}

fadeRegression <- function(canvas, e){
    slopes <- canvas$stat.dist[, 2]
    ci <- quantile(slopes, prob = c(0.025, 0.975), type = 1)
    ## Adding fadebox.
    cilabs <- format(round(ci, canvas$dp), nsmall = canvas$dp)
    canvas$image <- addGrob(canvas$image, rectGrob
                            (x = unit(-0.0375, "npc"), y = unit(2/3, "npc")
                             + unit(0.5 + 1/3, "lines"),
                             width = unit(1.0375, "npc"),
                             height = unit(2/3, "npc"),
                             just = c("left", "top"),
                             gp = gpar(col = "white", fill = "white", alpha = 0.75),
                             vp = vpPath("canvas.all", "canvas.plots"),
                             name = "fadebox"))
    ## Plotting data axis over the top of fadebox.
    dataGrobs <- getGrob(canvas$image, "dataPlot*", grep = TRUE, global = TRUE)
    dataGrobs <- gList(dataGrobs,
                       getGrob(canvas$image, gPath("dataAxis")),
                       getGrob(canvas$image, gPath("dataYAxis")),
                       getGrob(canvas$image, gPath("dataRect")))
    for (i in 1:length(dataGrobs)){
        canvas$image <- addGrob(canvas$image, dataGrobs[[i]])
    }
    ## Plotting ghost lines corresponding to CI
    canvas$image <- addGrob(canvas$image, segmentsGrob
                            (x0 = rep(0, 2), x1 = rep(1, 2),
                             y0 = unit(0, "native"), y1 = unit(ci, "native"),
                             gp = gpar(col = "blue"),
                             vp = canvas$graphPath("stat"),
                             name = "statPlot.ciraylines.1"))
    canvas$image <- addGrob(canvas$image, getGrob(canvas$image, gPath("statPlot.line.1")))
    cilines <- getGrob(canvas$image, gPath("statPlot.cilines.1"))
    cirect <- getGrob(canvas$image, gPath("statPlot.ci.1"))
    citext <- getGrob(canvas$image, gPath("statPlot.citext.1"))
    stattext <- getGrob(canvas$image, gPath("statPlot.stattext.1"))
    canvas$image <- addGrob(canvas$image, cilines)
    canvas$image <- addGrob(canvas$image, cirect)
    canvas$image <- addGrob(canvas$image, citext)
    canvas$image <- addGrob(canvas$image, stattext)
    canvas$image <- addGrob(canvas$image, textGrob
                            (label = "CI for the population slope",
                             x = 0.2,
                             y = max(cilines$y0) + unit(3, "mm"),
                             just = "bottom",
                             name = "statPlot.cifadetext.1",
                             vp = canvas$graphPath("stat.extrabox")))
    canvas$drawImage()
    canvas$rmGrobs(c("fadebox", "statPlot.cifadetext.1", "statPlot.ciraylines.1"))
}

plotSlopeBoot <- function(canvas, e) {
    canvas$sampled.stats <- unique(c(canvas$sampled.stats, canvas$which.sample))
    canvas$plotted.stats <- unique(c(canvas$plotted.stats, canvas$which.sample))
    nsamps <- length(canvas$plotted.stats)
    sampled.slopes <- canvas$stat.dist[, 2][canvas$sampled.stats]
    plotted.slopes <- canvas$stat.dist[, 2][canvas$plotted.stats]
    x0 <- rep(0, nsamps)
    x1 <- rep(1, nsamps)
    y0 <- unit(rep(0, nsamps), "native")
    y1 <- unit(sampled.slopes, "native")
    canvas$image <- addGrob(canvas$image, segmentsGrob
                            (x0 = x0, x1 = x1, y0 = y0, y1 = y1,
                             gp = gpar(col = "skyblue2"),
                             name = "statPlot.statlines.1",
                             vp = canvas$graphPath("stat")))
    canvas$image <- addGrob(canvas$image, linesGrob
                            (x = 0:1,
                             y = unit(c(0, sampled.slopes[length(sampled.slopes)]), "native"),
                             gp = gpar(lwd = 2, col = "blue"), name = "statPlot.line.1",
                             vp = canvas$graphPath("stat")))
    sampleAxis <- getGrob(canvas$image, gPath("sampleAxis"))
    y <- stackPoints(plotted.slopes, vp = canvas$graphPath("stat.extrabox"), y.min = 0.2)
    plotPoints(canvas, y, unit(plotted.slopes, "native")*(1/3), vp = canvas$graphPath("stat.extrabox"),
               name = "statPlot")
    canvas$image <- addGrob(canvas$image, sampleAxis)
    statlabel <- getGrob(canvas$image, gPath("statLabel"))
    canvas$image <- addGrob(canvas$image, statlabel)
}

plotSlopeBootAlpha <- function(canvas, e) {
    canvas$sampled.stats <- unique(c(canvas$sampled.stats, canvas$which.sample))
    canvas$plotted.stats <- unique(c(canvas$plotted.stats, canvas$which.sample))
    nsamps <- length(canvas$plotted.stats)
    sampled.slopes <- canvas$stat.dist[, 2][canvas$sampled.stats]
    plotted.slopes <- canvas$stat.dist[, 2][canvas$plotted.stats]
    x0 <- rep(0, nsamps)
    x1 <- rep(1, nsamps)
    y0 <- unit(rep(0, nsamps), "native")
    y1 <- unit(sampled.slopes, "native")
    canvas$image <- addGrob(canvas$image, segmentsGrob
                            (x0 = x0, x1 = x1, y0 = y0, y1 = y1,
                             gp = gpar(col = "blue", alpha = 0.2),
                             name = "statPlot.statlines.1",
                             vp = canvas$graphPath("stat")))
    canvas$image <- addGrob(canvas$image, linesGrob
                            (x = 0:1,
                             y = unit(c(0, sampled.slopes[length(sampled.slopes)]), "native"),
                             gp = gpar(lwd = 2, col = "blue"), name = "statPlot.line.1",
                             vp = canvas$graphPath("stat")))
    sampleAxis <- getGrob(canvas$image, gPath("sampleAxis"))
    y <- stackPoints(plotted.slopes, vp = canvas$graphPath("stat.extrabox"), y.min = 0.2)
    plotPoints(canvas, y, unit(plotted.slopes, "native")*(1/3), vp = canvas$graphPath("stat.extrabox"),
               name = "statPlot")
    canvas$image <- addGrob(canvas$image, sampleAxis)
    statlabel <- getGrob(canvas$image, gPath("statLabel"))
    canvas$image <- addGrob(canvas$image, statlabel)
}

movePointBoot <- function(canvas, env, n.steps, move.point = TRUE){
    stat <- canvas$stat.dist[canvas$which.sample, 2]
    slopes <- canvas$stat.dist[, 2][canvas$plotted.stats]
    slopes <- c(slopes, stat)
    if (move.point){
        canvas$image <- addGrob(canvas$image, pointsGrob
                                (x = 0, y = unit(stat, "native")*(1/3), pch = 19,
                                 gp = gpar(col = "red"), vp = canvas$graphPath("stat.extrabox"),
                                 name = "statPlot.point.1"))
        canvas$pauseImage(5)
        y <- stackPoints(slopes, vp = canvas$graphPath("stat.extrabox"), y.min = 0.2)
        x.start <- 0
        x.end <- y[length(y)]
        x.step <- (x.end - x.start)/round(n.steps/2)
        for (i in 1:round(n.steps/2)){
            canvas$image <- editGrob(canvas$image, gPath("statPlot.point.1"),
                                     x = unit(i*x.step, "npc"))
            canvas$drawImage()
        }
        canvas$rmGrobs("statPlot.point.1")
    }
}
