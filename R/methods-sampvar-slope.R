canvasSampvarSlope <- setRefClass("canvasSampvarSlopeClass", contains = "canvasPlotClass",
                                  methods = list(
    calcStat = function(i = which.sample, ys = NULL, canvas = .self) {
        calcCoeffs(as.numeric(samples[[i]]), as.numeric(ys))
    },

    calcAllStats = function(xs, ys = NULL, canvas = .self) {
        calcCoeffs(as.numeric(xs), as.numeric(ys))
    },

    plotSample = function(env, i = which.sample) {
        plotSampleScatterSampvar(.self, env, i)
    },

    showLabels = function() {
        sampvarRegressionLabels(.self)
    },

    plotDataStat = function(env, ...) {
        showExtraRectSampvar(env)
    },

    plotStatDist = function(env, ...) {
        plotSlopeBoot(.self, env)
    },

    animateSample = function(e, n.steps, n.slow, opts) {
        dropPoints2d(.self, n.steps, n.slow, keep.plot = opts$keep.plot,
                     move = opts$move)
    },

    animateStat = function(env, n.steps, move.point = TRUE) {
        movePointBoot(.self, env, n.steps, move.point)
    },

    handle1000 = function(env, ...) {
        slope1000sampvar(.self, env, ...)
    },

    calcxScaleYs = function(env, x.scale) {
        x.scale.ys <<- aaply(stat.dist, 1, function(x) x[1] + x[2]*x.scale)
    },

    displayResult = function(env, ...){
        plotTheoDistSlope(env)
    }))

load_sampvar_slope <- function(e) {
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
    tmp.canvas <- canvasSampvarSlope$new()
    tmp.canvas$import(e$c1)
    e$c1 <- tmp.canvas
    e$c1$viewports <- tmp.vps
    e$c1$image <- tmp.image
    e$extra <- 0
    e$difference <- TRUE
}

dropPoints2d <- function(canvas, n.steps, n.slow, keep.plot, move = TRUE, pause = 10) {
    canvas$rmGrobs(c("samplePlot.points.1", "samplePlot.sampledpoints.1",
                     "samplePlot.line.1", "samplePlot.slopetext.1", "statPlot.line.1"))
    if (!keep.plot){
        canvas$rmGrobs(c("samplePlot.boxplot.1", "samplePlot.boxplot", "samplePlot.stat.1"))
    }
    index <- canvas$indexes[[canvas$which.sample]]
    x <- canvas$x[index]
    y.start <- y.pos <- unit(canvas$levels[index], "native")# to place in data vp
    y.end <- unit(canvas$levels[index], "native") - unit(1, "npc")
    y.step <- (y.start - y.end)*(1/n.steps)
    n.slow <- min(n.slow, length(x))
    ## Lighting up of sampled points.
    if (move & n.steps > 3) {
        sampSelectLabel <- textGrob("Selecting sample...", x = unit(0.5, "npc"),
                                    y = unit(0.6, "npc"),
                                    just = c("centre", "top"), vp = canvas$graphPath("sample"),
                                    gp = gpar(fontface = 2), name = "samplePlot.sampSelectLabel")
        canvas$image <- addGrob(canvas$image, sampSelectLabel)
        ## Don't bother animating more than 50.
        for (i in 1:min(c(50, length(x)))){
            canvas$image <- addGrob(canvas$image,
                                    pointsGrob(x = x[1:i], y = (canvas$levels[index])[1:i],
                                               vp = canvas$graphPath("data"),
                                               pch = 19,
                                               name = "samplePlot.sampledpoints.1"))
            if (i <= n.slow) speed = 10 else speed = 1
            if (canvas$stopAnimation)
                return()
            canvas$pauseImage(speed)
        }
    }
    canvas$image <- addGrob(canvas$image,
                            pointsGrob(x, y = canvas$levels[index], vp = canvas$graphPath("data"),
                                       pch = 19,
                                       name = "samplePlot.sampledpoints.1"))
    ## Force pause before points drop.
    if (move){
        if (canvas$stopAnimation)
            return()
        canvas$pauseImage(if (n.steps > 3) 20 else 5)
    }
    ## Dropping of points.
    if (move){
        for (i in 1:n.steps){
            y.pos <- y.pos - y.step
            canvas$image <- addGrob(canvas$image,
                                    pointsGrob(x, y.pos, vp = canvas$graphPath("data"),
                                               pch = 19, name = "samplePlot.temp"))
            if (canvas$stopAnimation)
                return()
            canvas$drawImage()
        }
        canvas$rmGrobs("samplePlot.sampSelectLabel")
        canvas$pauseImage(4)
        canvas$rmGrobs("samplePlot.temp")
    }
    plotSamplePanelSampvar(canvas, NULL, canvas$which.sample)
    if (move){
        canvas$pauseImage(pause)
    }
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
    canvas$plotSample(NULL, canvas$which.sample)
    if (move){
        canvas$pauseImage(20)
    } else {
        canvas$drawImage()
    }
}

sampvarRegressionLabels <- function(canvas){
    poplabel <- textGrob("Population",
                         x = unit(0, "npc") + unit(1, "mm"),
                         y = unit(1, "npc") - unit(1, "mm"),
                         just = c("left", "top"),
                         name = "dataLabel",
                         vp = canvas$graphPath("data"),
                         gp = gpar(fontface = 2))
    methlabel <- textGrob("Module: Sampling variation",
                          x = unit(0, "npc"),
                          just = "left",
                          gp = gpar(fontsize = 10, fontface = "italic"),
                          name = "methodLabel",
                          vp = canvas$graphPath("canvas.header"))
    varlabel <- textGrob("Parameter: slope",
                         x = unit(0, "npc") + stringWidth(methlabel$label) + unit(6, "mm"),
                         just = "left",
                         gp = gpar(fontsize = 10, fontface = "italic"),
                         name = "varLabel",
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
                                    width = stringWidth(poplabel$label) + unit(2, "mm"),
                                    height = stringHeight(poplabel$label) + unit(2, "mm"),
                                    just = c("left", "bottom"),
                                    gp = gpar(col = "white", fill = "white", alpha = 0.7),
                                    name = "dataLabelBackground",
                                    vp = canvas$graphPath("data"))
    resamplabel <- textGrob("Sample",
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
    statlabel <- textGrob("Sampling Distribution",
                          x = unit(0, "npc") + unit(1, "mm"),
                          y = unit(1, "npc") - unit(2, "lines"),
                          just = c("left", "top"),
                          name = "statLabel",
                          vp = canvas$graphPath("stat"),
                          gp = gpar(fontface = 2))
    permlabels <- grobTree(methlabel, varlabel, filelabel,
                           infosep,
                           dataLabelBackground,
                           poplabel,
                           resampLabelBackground,
                           resamplabel, statlabel,
                           name = "permlabels")
    canvas$image <- addGrob(canvas$image, permlabels)
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
}

## Helper function
plotSamplePanelSampvar <- function(canvas, e, i){
    canvas$sampled.stats <- c(canvas$sampled.stats, i)
    x <- canvas$samples[[i]]
    y <- canvas$level.samples[[i]]
    if (diff(range(x)) == 0){
        x.scale <- range(x) + c(-1, 1)
    } else {
        x.scale <- range(x) + c(-0.04, 0.04)*diff(range(x))
    }
    ## Plotting sample scatterplot.
    plotScatterPlot(canvas, x, y, "sample", "blue")
    canvas$image <- addGrob(canvas$image,
                            pointsGrob(x = x, y = y, vp = canvas$graphPath("data"),
                                       name = "samplePlot.sampledpoints.1",
                                       pch = 19))
    ## Drawing and labelling axes and writing slope value.
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
    slope <- coefficients(lm(as.numeric(y) ~ as.numeric(x)))[2]
    canvas$image <- addGrob(canvas$image, textGrob
                            (label = c("slope =", format(round(slope, canvas$dp),
                             nsmall = canvas$dp)),
                             x = 0.5, y = unit(0.5, "npc") + unit(c(0.5, -0.5), "lines"),
                             gp = gpar(col = c("black", "red"), fontface = 1:2),
                             vp = canvas$graphPath("stat.extrabox"),
                             name = "samplePlot.slopetext.1"))
}

plotSampleScatterSampvar <- function(canvas, e, i) {
    plotSamplePanelSampvar(canvas, e, i)
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

plotSampleScatterSampvarAlpha <- function(canvas, e, i) {
    plotSamplePanelSampvar(canvas, e, i)
    nsamps <- length(canvas$sampled.stats)
    slopes <- canvas$stat.dist[, 2][canvas$sampled.stats]
    stat <- slopes[length(slopes)]
    x0 <- rep(0, nsamps)
    x1 <- rep(1, nsamps)
    y0 <- unit(rep(0, nsamps), "native")
    y1 <- unit(slopes, "native")
    canvas$image <- addGrob(canvas$image, segmentsGrob
                            (x0 = x0, x1 = x1, y0 = y0, y1 = y1,
                             gp = gpar(col = "blue", alpha = 0.2),
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

slope1000sampvar <- function(canvas, e, points) {
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
    x0s <- rep(x.scale[1], 1000)
    x1s <- rep(x.scale[2], 1000)
    y0s <- canvas$x.scale.ys[, 1]
    y1s <- canvas$x.scale.ys[, 2]
    n <- 1
    # As there seem to be issues with drawing line/segment grobs with
    # an alpha value other than 0 or 1 in the windows() device, we only
    # plot some of the lines from the samples. `Runs` works out which
    # lines we want to plot.
    runs <- if (if.plotted[1000])
                which(if.plotted)
            else
                c(which(if.plotted), 1000)
    ## Updating sample axes, labels, and slope value.
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
    ## Running through each of the samples we wish to plot.
    for (i in runs) {
        xsample <- canvas$samples[[i]]
        ysample <- canvas$level.samples[[i]]
        canvas$image <- addGrob(canvas$image,
                                pointsGrob(x = xsample, y = ysample,
                                           vp = canvas$graphPath("data"),
                                           name = "samplePlot.sampledpoints.1",
                                           pch = 19))
        plotScatterPlot(canvas, canvas$samples[[i]], canvas$level.samples[[i]],
                        "sample", "blue")
        canvas$image <- addGrob(canvas$image, segmentsGrob
                                (x0 = x0[1:i][if.plotted[1:i]], x1 = x1[1:i][if.plotted[1:i]],
                                 y0 = y0[1:i][if.plotted[1:i]], y1 = y1[1:i][if.plotted[1:i]],
                                 gp = gpar(col = "skyblue2"), name = "statPlot.statlines.1",
                                 vp = canvas$graphPath("stat")))
        # Replotting the purple line over the top
        tmp <- getGrob(canvas$image, "hlineslope")
        canvas$image <- addGrob(canvas$image, tmp)
        canvas$image <- addGrob(canvas$image, linesGrob
                                (x = 0:1,
                                 y = unit(c(0, slopes[i]), "native"),
                                 gp = gpar(lwd = 2, col = "blue"), name = "statPlot.line.1",
                                 vp = canvas$graphPath("stat")))
        sampleAxis <- getGrob(canvas$image, gPath("sampleAxis"))
        statlabel <- getGrob(canvas$image, gPath("statLabel"))
        canvas$image <- addGrob(canvas$image, sampleAxis)
        canvas$image <- addGrob(canvas$image, statlabel)
        if (points)
            plotPoints(canvas, canvas$stat.ypos[1:i]*0.8 + 0.2, unit(slopes[1:i], "native")*(1/3),
                       vp = canvas$graphPath("stat.extrabox"), name = "statPlot", alpha = 0.7)
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
    ## Cleaning up unwanted grobs.
    canvas$rmGrobs(c("statPlot.line.1", "samplePlot.points.1",
                     "samplePlot.line.1", "samplePlot.ylabel.1", "samplePlot.xlabel.1",
                     "samplePlot.labelaxes.1", "samplePlot.slopetext.1",
                     "samplePlot.sampledpoints.1"))
    if (canvas$stopAnimation)
        return()
    canvas$drawImage()
    canvas$rmGrobs(c("statPlot.statlines.1", "statPlot.points.2"))
}

slope1000sampvarAlpha <- function(canvas, e, points) {
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
    x0s <- rep(x.scale[1], 1000)
    x1s <- rep(x.scale[2], 1000)
    y0s <- canvas$x.scale.ys[, 1]
    y1s <- canvas$x.scale.ys[, 2]
    n <- 1
    ## Updating sample axes, labels, and slope value.
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
    ## Running through each of the samples we wish to plot.
    for (i in seq(1, 1000, 50)) {
        xsample <- canvas$samples[[i]]
        ysample <- canvas$level.samples[[i]]
        canvas$image <- addGrob(canvas$image,
                                pointsGrob(x = xsample, y = ysample,
                                           vp = canvas$graphPath("data"),
                                           name = "samplePlot.sampledpoints.1",
                                           pch = 19))
        plotScatterPlot(canvas, canvas$samples[[i]], canvas$level.samples[[i]],
                        "sample", "blue")
        line.filter <- rep(c(TRUE, rep(FALSE, 8)), length.out = 1000)
        canvas$image <- addGrob(canvas$image, segmentsGrob
                                (x0 = x0[1:i][line.filter[1:i]], x1 = x1[1:i][line.filter[1:i]],
                                 y0 = y0[1:i][line.filter[1:i]], y1 = y1[1:i][line.filter[1:i]],
                                 gp = gpar(col = "blue", alpha = 0.2), name = "statPlot.statlines.1",
                                 vp = canvas$graphPath("stat")))
        # Replotting the purple line over the top
        tmp <- getGrob(canvas$image, "hlineslope")
        canvas$image <- addGrob(canvas$image, tmp)
        canvas$image <- addGrob(canvas$image, linesGrob
                                (x = 0:1,
                                 y = unit(c(0, slopes[i]), "native"),
                                 gp = gpar(lwd = 2, col = "blue"), name = "statPlot.line.1",
                                 vp = canvas$graphPath("stat")))
        sampleAxis <- getGrob(canvas$image, gPath("sampleAxis"))
        statlabel <- getGrob(canvas$image, gPath("statLabel"))
        canvas$image <- addGrob(canvas$image, sampleAxis)
        canvas$image <- addGrob(canvas$image, statlabel)
        if (points)
            plotPoints(canvas, canvas$stat.ypos[1:i]*0.8 + 0.2, unit(slopes[1:i], "native")*(1/3),
                       vp = canvas$graphPath("stat.extrabox"), name = "statPlot", alpha = 0.7)
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
    ## Cleaning up unwanted grobs.
    canvas$rmGrobs(c("statPlot.line.1", "samplePlot.points.1",
                     "samplePlot.line.1", "samplePlot.ylabel.1", "samplePlot.xlabel.1",
                     "samplePlot.labelaxes.1", "samplePlot.slopetext.1",
                     "samplePlot.sampledpoints.1"))
    if (canvas$stopAnimation)
        return()
    canvas$drawImage()
    canvas$rmGrobs(c("statPlot.statlines.1", "statPlot.points.2"))
}

showExtraRectSampvar <- function(e){
    canvas <- e$c1
    x <- as.numeric(canvas$x)
    y <- as.numeric(canvas$levels)
    canvas$image <- addGrob(canvas$image, rectGrob
                            (y = 0, height = 1/3, just = "bottom",
                             vp = canvas$graphPath("stat.extrabox"),
                             name = "extraboxRect"))
    ## Plotting population slope in stat panel and extrabox.
    slope <- coefficients(lm(y ~ x))[2]
    canvas$image <- addGrob(canvas$image, linesGrob
                            (x = 0:1, y = unit(c(0, slope), "native"),
                             gp = gpar(col = "purple3", lwd = 2),
                             vp = canvas$graphPath("stat"),
                             name = "hlineslope"))
    canvas$image <- addGrob(canvas$image, linesGrob
                            (x = 0:1, y = unit(rep(slope, 2), "native")*(1/3),
                             gp = gpar(col = "grey60", lty = "dashed"),
                             vp = canvas$graphPath("stat.extrabox"),
                             name = "hline"))
}

plotTheoDistSlope <- function(e){
    canvas <- e$c1
    xdat <- canvas$x
    ydat <- canvas$levels
    fit <- lm(ydat ~ xdat)
    slope <- coefficients(fit)[2]
    ## Replotting statistic distribution
    x <- canvas$stat.dist[, 2]
    y <- canvas$stat.ypos
    plotPoints(canvas, y*0.8 + 0.2, unit(x, "native")*(1/3), canvas$graphPath("stat.extrabox"),
               "statPlot", black = FALSE, alpha = 0.7)
    ## Calculate denominator for slope SD.
    xdev <- canvas$n*mean((xdat - mean(xdat))^2)
    sigma <- pop.sd(fit$residuals)
    slopesd <- sqrt((sigma^2)/xdev)
    ## Getting statistic panel y-scale.
    top.level <- downViewport(canvas$graphPath("stat.extrabox"))
    stat.scale <- current.viewport()$yscale
    upViewport(top.level)
    ## Calculating normal density under the CLT.
    xs <- seq(stat.scale[1], stat.scale[2], length.out = 300)
    ys <- dnorm(xs, slope, slopesd)
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
    ys <- ys/max(dens$y)
    canvas$image <- addGrob(canvas$image, linesGrob
                            (y = unit(xs, "native")*(1/3), x = 0.2 + 0.8*ys,
                             gp = gpar(lwd = 2, col = "red"),
                             name = "statPlot.theodist.1",
                             vp = canvas$graphPath("stat.extrabox")))
    canvas$showLabels()
    canvas$drawImage()
    canvas$rmGrobs(c("samplePlot.points.1", "samplePlot.lines.1",
                     "samplePlot.datapoints.points.1", "samplePlot.databox.text.2",
                     "samplePlot.stat.1", "samplePlot.hist.1"))
}
