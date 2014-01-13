canvasBootMeanDiff <- setRefClass("canvasBootMeanDiffClass", contains = "canvasPlotClass",
                                  methods = list(
    plotSample = function(env, i = which.sample, ...) {
        plotSampleLevelsMean(.self, env, i, ...)
    },

    showLabels = function() {
        bootLabelsTwoSample(.self)
    },
    ## calcMeanDiff taken from methods-permutation-mean.R
    calcStat = function(i = which.sample, ys = NULL, canvas = .self) {
        ys <- level.samples[[i]]
        if (all(ylevels == sort(unique(ys)))) {
            calcDiff(samples[[i]], ys, fun = mean)
        } else {
            rev(calcDiff(samples[[i]], ys, fun = mean))
        }
    },

    calcAllStats = function(xs, ys = NULL, canvas = .self) {
        if (all(ylevels == sort(unique(ys)))) {
            calcDiff(xs, ys, fun = mean)
        } else {
            rev(calcDiff(xs, ys, fun = mean))
        }
    },
    ## dataDiffArrowMean taken from mehtods-permutation-mean.R
    plotDataStat = function(env, ...) {
        dataDiffArrowMean(.self, env)
    },

    plotStatDist = function(env, ...) {
        plotBootDiffDist(.self, env)
    },

    animateSample = function(...) {
        dropPointsDiff(.self, ...)
    },

    trackSample = function(...) {
        trackBootstrapDiff(.self)
    },

    animateStat = function(env, n.steps) {
        dropBootArrow(.self, env, n.steps)
    },

    displayResult = function(env, ...) {
        showCIandStatsDiff(.self, env, ...)
    },

    handle1000 = function(env, ...) {
        diff1000(.self, env, ...)
    },

    fadePlots = function(env, ...) {
        fadeAllCI(.self, env, ...)
    }))

load_bootstrap_mean_diff<- function(e) {
    e$c1$orderLevels(FUN = mean)
    e$c1$stat.in.use <- svalue(e$stat)
    e$resampWithin <- enabled(e$resamp.within) & svalue(e$resamp.within)
    # There is something messy going on with viewports and
    # gTrees, such that when attempting to import them, we
    # end up with "uninitializedField"s. Assign to a temp
    # var and reassign later.
    tmp.vps <- e$c1$viewports
    tmp.image <- e$c1$image
    e$c1$viewports <- NULL
    e$c1$image <- NULL
    tmp.canvas <- canvasBootMeanDiff$new()
    tmp.canvas$import(e$c1)
    e$c1 <- tmp.canvas
    e$c1$viewports <- tmp.vps
    e$c1$image <- tmp.image
    e$extra <- 0
    e$difference <- TRUE
}

plotSampleLevelsMean <- function(canvas, e, i, alpha = 0.25){
    x <- canvas$samples[[i]]
    levels <- canvas$level.samples[[i]]
    ylevels <- canvas$ylevels
    y <- old.stackPoints(x, levels, vp = canvas$graphPath("sample"))
    n <- 1
    canvas$sampled.stats <- unique(c(canvas$sampled.stats, canvas$which.sample))
    stats <- canvas$stat.dist[canvas$sampled.stats, , drop = FALSE]
    stats[, 1:2] <- stats[, 2:1]
    ## Plotting samples, labels and ghosts
    for (j in ylevels) {
        if (length(x) >= canvas$hist.cutoff) {
            plotHist(canvas, x[levels == j], canvas$graphPath("sample", n), "samplePlot",
                     fill = getColour(n, length(ylevels), c = 50))
        } else {
            plotPoints(canvas, x[levels == j],
                       y[levels == j], col = getColour(1:length(ylevels), length(ylevels))[n],
                       vp = canvas$graphPath("sample", as.character(n)),
                       name = "samplePlot")
        }
        canvas$image <- addGrob(canvas$image, rectGrob
                                (x = unit(stats[, n], "native"),
                                 y = unit(0.15, "npc"), height = unit(0.2, "npc"),
                                 width = 0, gp = gpar(alpha = alpha, col = "blue", lwd = 2),
                                 vp = canvas$graphPath("sample", as.character(n)),
                                 name = paste("samplePlot.ghosts", n, sep = ".")))
        canvas$image <- addGrob(canvas$image, linesGrob
                                (x = unit(rep(mean(x[levels == j]), 2),
                                 "native"),
                                 y = unit(c(0.1, 0.55), "npc"),
                                 gp = gpar(lwd = 3, col = "blue"),
                                 vp = canvas$graphPath("sample", as.character(n)),
                                 name = paste("samplePlot", "line", n, sep = ".")))
        canvas$image <- addGrob(canvas$image, roundrectGrob
                                (x = unit(1, "npc") + unit(1, "mm"),
                                 y = unit(0, "npc"), just = c("right", "bottom"),
                                 height = stringHeight(j) + unit(2, "mm"),
                                 width = stringWidth(j) + unit(2, "mm"),
                                 vp = canvas$graphPath("sample", n),
                                 gp = gpar(fill = "white", col = "white", alpha = 0.75),
                                 name = paste("samplePlot.blankrect", n, sep = ".")))
        canvas$image <- addGrob(canvas$image, textGrob
                                (j, x = 1, y = unit(1, "mm"), just = c("right", "bottom"),
                                 vp = canvas$graphPath("sample", as.character(n)),
                                 name = paste("samplePlot.text", n, sep = ".")))
        n <- n + 1
    }
    ## Plotting arrow difference
    canvas$image <- addGrob(canvas$image, linesGrob(x = unit(canvas$stat.dist[i, ], "native"),
                                                    y = unit(0.8, "npc"),
                                                    gp = gpar(lwd = 2, col = "red"),
                                                    arrow = arrow(length = unit(0.1, "inches")),
                                                    vp = canvas$graphPath("sample", 1),
                                                    name = "samplePlot.stat.2"))
    ## Filling sample databox.
    ## To keep colours consistent with points
    ## we need to order the cols parameters so that the group with the
    ## lower statistic value is green.
    cols <- getColour(1:length(canvas$ylevels), length(canvas$ylevels))
    names(cols) <- canvas$ylevels
    cols <- cols[order(canvas$ylevels)]
    text.sample1 <- datatextGrob(data = x, title = "", x = 0.25,
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

bootLabelsTwoSample <- function(canvas) {
    samplabel <- textGrob("Sample",
                          x = unit(0, "npc") + unit(1, "mm"),
                          y = unit(1, "npc") - unit(1, "mm"),
                          just = c("left", "top"),
                          name = "dataLabel",
                          vp = canvas$graphPath("data", 2),
                          gp = gpar(fontface = 2))
    methlabel <- textGrob("Module: 2 Sample Bootstrapping",
                          x = unit(0, "npc"),
                          just = "left", 
                          name = "methodLabel",
                          gp = gpar(fontsize = 10, fontface = "italic"),
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
                             x = methlabel$x + stringWidth(methlabel$label) + unit(6, "mm"),
                             just = "left",
                             name = "varLabel",
                             gp = gpar(fontsize = 10, fontface = "italic"),
                             vp = canvas$graphPath("canvas.header"))
    }
    quantitylabel <- textGrob(paste("Quantity:", canvas$stat.in.use),
                              x = varlabel$x[1] + stringWidth(paste(varlabel$label, collapse = "")) + unit(6, "mm"),
                              just = "left",
                              name = "quantityLabel",
                              gp = gpar(fontsize = 10, fontface = "italic"),
                              vp = canvas$graphPath("canvas.header")) 
    filelabel <- textGrob(paste("File:", canvas$data.file),
                          x = quantitylabel$x + stringWidth(quantitylabel$label) + unit(6, "mm"),
                          just = "left",
                          name = "fileLabel",
                          gp = gpar(fontsize = 10, fontface = "italic"),
                          vp = canvas$graphPath("canvas.header"))
    infosep <- linesGrob(x = unit(0:1, "npc"), y = unit(0, "npc"),
                         name = "infoSeparatorLine",
                         vp = canvas$graphPath("canvas.header"))
    dataLabelBackground <- rectGrob(x = unit(0, "npc"),
                                    y = unit(1, "npc") - unit(1, "lines") - unit(1, "mm"),
                                    width = stringWidth(samplabel$label) + unit(2, "mm"),
                                    height = stringHeight(samplabel$label) + unit(2, "mm"),
                                    just = c("left", "bottom"),
                                    gp = gpar(col = "white", fill = "white", alpha = 0.7),
                                    name = "dataLabelBackground",
                                    vp = canvas$graphPath("data", 2))
    resamplabel <- textGrob("Re-sample",
                            x = unit(0, "npc") + unit(1, "mm"),
                            y = unit(0.6, "npc"),
                            just = c("left", "top"),
                            name = "sampleLabel",
                            vp = canvas$graphPath("sample", 2),
                            gp = gpar(fontface = 2))
    resampLabelBackground <- rectGrob(x = unit(0, "npc"),
                                      y = unit(0.6, "npc") + unit(0.5, "lines") - unit(1, "mm"),
                                      width = stringWidth(resamplabel$label) + unit(4, "mm"),
                                      height = stringHeight(resamplabel$label) + unit(2, "mm"),
                                      just = c("left", "top"),
                                      gp = gpar(col = "white", fill = "white", alpha = 0.7),
                                      name = "resampLabelBackground",
                                      vp = canvas$graphPath("sample", 2))
    statlabel <- textGrob("Bootstrap distribution",
                          x = unit(0, "npc") + unit(1, "mm"),
                          y = unit(0.8, "npc"),
                          just = c("left", "top"),
                          name = "statLabel",
                          vp = canvas$graphPath("stat"),
                          gp = gpar(fontface = 2))
    bootlabels <- grobTree(methlabel, varlabel, quantitylabel, filelabel,
                           infosep,
                           dataLabelBackground,
                           samplabel,
                           resampLabelBackground,
                           resamplabel, statlabel,
                           name = "bootlabels")
    canvas$image <- addGrob(canvas$image, bootlabels)
}

plotBootDiffDist <- function(canvas, e){
    canvas$plotted.stats <- c(canvas$plotted.stats, canvas$which.sample)
    stats <- apply(canvas$stat.dist[canvas$plotted.stats, , drop = FALSE], 1, diff)
    y.max <- unit(1, "npc") - unit(2, "lines") - unit(0.5, "char")
    y <- stackPoints(stats, vp = canvas$graphPath("stat"), y.min = 0, y.max = y.max)
    plotPoints(canvas, stats, y, vp = canvas$graphPath("stat"), name = "statPlot")
}

dropPointsDiff <- function(canvas, e, drop.points = FALSE, n.steps = 10, n.slow = 5, max = 50){
    canvas$rmGrobs(c("samplePlot.databox.text.2", "samplePlot.stat.2", "samplePlot.labelrect.1", "samplePlot.labeltext.1"))
    canvas$rmGrobs(paste("samplePlot", c("points", "line", "boxplot", "text"), rep(1:2, each = 4), sep = "."))
    index <- canvas$indexes[[canvas$which.sample]]
    x <- canvas$x[index]
    levels <- as.character(canvas$levels[index])
    y <- 0.5*canvas$y[index] + 0.5*(levels == canvas$ylevels[2])
    n <- canvas$n
    datatexttitle <- datatextGrob(data = "", title = "Re-sample", name = "samplePlot.resamp.text",
                                  vp = canvas$graphPath("databox", 2))
    canvas$image <- addGrob(canvas$image, datatexttitle)
    if (n < 100){
        n.slow <- min(n.slow, n)
        ## Calculating the position of text in text boxes.
        ntext <- min(n, max)
        npcs <- (ntext:0)/ntext
        yunit <- unit(npcs, "npc") - unit(4*(npcs - 0.5), "mm") + unit(1 - npcs, "lines")
        x.text.start <- c(0.25, 0.75)/2
        x.text.end <- 0.5 + x.text.start
        x.text.step <- (x.text.end[1] - x.text.start[1])/n.steps
        y.start <- y + 2
        y.end <- 0.5*old.stackPoints(x, levels = levels, vp = canvas$graphPath("sample")) + 1 +
            0.5*(levels == canvas$ylevels[2])
        y.step <- (y.end - y.start)/n.steps

        # Drawing text showing levels prior to dropping points into the appropriate level
        for (i in 1:length(canvas$ylevels)) {
            canvas$image <- addGrob(canvas$image, textGrob
                                    (canvas$ylevels[i],
                                     x = unit(1, "npc"), y = unit(1, "mm"),
                                     just = c("right", "bottom"),
                                     name = paste("samplePlot.text", i, sep = "."),
                                     vp = canvas$graphPath("sample", i)))
        }

        ## Animation of slow points.
        for (i in seq(from = 1, by = 1, length.out = n.slow)){
            y.text.start <- rep(yunit[min(c(max + 1, index[i] + 1))], 2)
            y.text.end <- rep(yunit[i + 1], 2)
            y.text.step <- convertY(y.text.end - y.text.start, "npc", valueOnly = TRUE)/n.steps
            temp.text <- textGrob(label = c(format(round(x[i], canvas$dp), nsmall = canvas$dp), levels[i]),
                                  y = y.text.start, x = x.text.start, just = "top",
                                  gp = gpar(col = "red", fontface = 2), name = "samplePlot.temp.datatext",
                                  vp = vpPath("canvas.all", "canvas.boxes"))
            ## Light up point to drop
            if (drop.points){
                temp.point <- pointsGrob(x = x[i], y = y.start[i], pch = 19,
                                         vp = canvas$graphPath("animation.field"),
                                         name = "samplePlot.temp.point")
                canvas$image <- addGrob(canvas$image, temp.point)
            }
            canvas$image <- addGrob(canvas$image, temp.text)
            if (canvas$stopAnimation)
                return()
            canvas$pauseImage(5)
            for (j in 1:n.steps){
                canvas$image <- editGrob(canvas$image, gPath("samplePlot.temp.datatext"),
                                         gp = gpar(fontface = 1))
                ## Move text
                canvas$image <- addGrob(canvas$image, textGrob
                                        (label = c(format(round(x[i], canvas$dp), nsmall = canvas$dp), levels[i]),
                                         y = y.text.start + unit(j*y.text.step, "npc"),
                                         x = unit(x.text.start + j*x.text.step, "npc"),
                                         just = "top", gp = gpar(col = "red", fontface = 2),
                                         name = "samplePlot.temp.text",
                                         vp = vpPath("canvas.all", "canvas.boxes")))
                ## Drop point
                if (drop.points){
                    canvas$image <- addGrob(canvas$image, pointsGrob
                                            (x = x[i], y = y.start[i] + j*y.step[i], pch = 19,
                                             vp = canvas$graphPath("animation.field"),
                                             name = "samplePlot.temp"))
                }
                if (canvas$stopAnimation)
                    return()
                canvas$drawImage()
                if (j == n.steps & drop.points){
                    canvas$rmGrobs("samplePlot.temp")
                }
            }
            ## Make points permanent if dropping
            if (drop.points){
                colvec <- getColour(1:length(canvas$ylevels), length(canvas$ylevels))
                names(colvec) <- canvas$ylevels
                colvec <- colvec[levels]
                plotPoints(canvas, x[1:i], y.end[1:i], canvas$graphPath("animation.field"),
                           "samplePlot", black = FALSE,
                           col = colvec)
            }
            canvas$pauseImage(5)
            canvas$rmGrobs("samplePlot.temp.text")
            resamp.text1 <- textGrob(label = c("", format(round(x[1:i], canvas$dp), nsmall = canvas$dp)),
                                     x = unit(0.25, "npc"),
                                    y = yunit[1:(i + 1)], just = "top", gp = gpar(col = "red"),
                                    name = "databox.xtext.2", vp = canvas$graphPath("databox", 2))
            resamp.text2 <- textGrob(label = c("", levels[1:i]),
                                     x = unit(0.75, "npc"),
                                     y = yunit[1:(i + 1)], just = "top", gp = gpar(col = "red"),
                                    name = "databox.ltext.2", vp = canvas$graphPath("databox", 2))
            resamp.text <- grobTree(datatexttitle, resamp.text1, resamp.text2, name = "samplePlot.resamp.text")
            canvas$image <- addGrob(canvas$image, resamp.text)
        }
        ## Animation of fast points.
        if (n == ntext) length.out <- ntext - n.slow else length.out <- ntext - n.slow - 1
        for (i in seq(from = n.slow + 1, by = 1, length.out = length.out)){
            y.text.start <- rep(yunit[min(c(max + 1, index[i] + 1))], 2)
            temp.text <- textGrob(label = c(format(round(x[i], canvas$dp), nsmall = canvas$dp), levels[i]),
                                  y = y.text.start, x = x.text.start, just = "top",
                                  gp = gpar(col = "red", fontface = 2), name = "samplePlot.temp.datatext",
                                  vp = vpPath("canvas.all", "canvas.boxes"))
            resamp.text1 <- textGrob(label = c("", format(round(x[1:i], canvas$dp), nsmall = canvas$dp)),
                                     x = unit(0.25, "npc"),
                                    y = yunit[1:(i + 1)], just = "top", gp = gpar(col = "red"),
                                    name = "databox.xtext.2", vp = canvas$graphPath("databox", 2))
            resamp.text2 <- textGrob(label = c("", levels[1:i]),
                                     x = unit(0.75, "npc"),
                                     y = yunit[1:(i + 1)], just = "top", gp = gpar(col = "red"),
                                    name = "databox.ltext.2", vp = canvas$graphPath("databox", 2))
            ## Light up point to drop
            if (drop.points){
                temp.point <- pointsGrob(x = x[i], y = y.start[i], pch = 19,
                                         vp = canvas$graphPath("animation.field"),
                                         name = "samplePlot.temp.point")
                canvas$image <- addGrob(canvas$image, temp.point)
            }
            resamp.text <- grobTree(datatexttitle, resamp.text1, resamp.text2, name = "samplePlot.resamp.text")
            canvas$image <- addGrob(canvas$image, temp.text)
            canvas$image <- addGrob(canvas$image, resamp.text)
            ## Plot dropped point.
            if (drop.points){
                colvec <- getColour(1:length(canvas$ylevels), length(canvas$ylevels))
                names(colvec) <- canvas$ylevels
                colvec <- colvec[levels]
                plotPoints(canvas, x[1:i], y.end[1:i], canvas$graphPath("animation.field"),
                           "samplePlot", black = FALSE,
                           col = colvec)
            }
            if (canvas$stopAnimation)
                return()
            canvas$pauseImage(5)
        }
        ## Animation of points outside databox.
        for (i in seq(from = ntext, by = 1, length.out = n - ntext)){
            y.text.start <- rep(yunit[min(c(max + 1, index[i] + 1))], 2)
            temp.text <- textGrob(label = c(format(round(x[i], canvas$dp), nsmall = canvas$dp), levels[i]),
                                  y = y.text.start, x = x.text.start, just = "top",
                                  gp = gpar(col = "red", fontface = 2), name = "samplePlot.temp.datatext",
                                  vp = vpPath("canvas.all", "canvas.boxes"))
            resamp.text1 <- textGrob(label = c("", format(round(x[1:(ntext - 1)], canvas$dp), nsmall = canvas$dp),
                                     "..."), x = unit(0.25, "npc"),
                                    y = yunit, just = "top", gp = gpar(col = "red"),
                                    name = "databox.xtext.2", vp = canvas$graphPath("databox", 2))
            resamp.text2 <- textGrob(label = c("", levels[1:(ntext - 1)], "..."),
                                     x = unit(0.75, "npc"),
                                     y = yunit, just = "top", gp = gpar(col = "red"),
                                    name = "databox.ltext.2", vp = canvas$graphPath("databox", 2))
            resamp.text <- grobTree(datatexttitle, resamp.text1, resamp.text2, name = "samplePlot.resamp.text")
            ## Light up point to drop.
            if (drop.points){
                temp.point <- pointsGrob(x = x[i], y = y.start[i], pch = 19,
                                         vp = canvas$graphPath("animation.field"),
                                         name = "samplePlot.temp.point")
                canvas$image <- addGrob(canvas$image, temp.point)
            }
            canvas$image <- addGrob(canvas$image, temp.text)
            canvas$image <- addGrob(canvas$image, resamp.text)
            ## Plot dropped point.
             if (drop.points){
                colvec <- getColour(1:length(canvas$ylevels), length(canvas$ylevels))
                names(colvec) <- canvas$ylevels
                colvec <- colvec[levels]
                plotPoints(canvas, x[1:i], y.end[1:i], canvas$graphPath("animation.field"),
                           "samplePlot", black = FALSE,
                           col = colvec)
            }
            if (canvas$stopAnimation)
                return()
            canvas$pauseImage(5)
        }
        canvas$rmGrobs(c("samplePlot.resamp.text", "samplePlot.temp.datatext", "samplePlot.temp.point", "samplePlot.points."))
        canvas$plotSample(e, canvas$which.sample)
        canvas$showLabels()
        canvas$pauseImage(20)
    }
}

trackBootstrapDiff <- function(canvas){
    if (canvas$n < 100){
        index <- canvas$indexes[[canvas$which.sample]]
        sample <- canvas$x
        levels <- as.character(canvas$levels)
        sample.y <- 0.5*canvas$y + 0.5*(levels == canvas$ylevels[2]) + 2
        resample <- sample[index]
        resample.levels <- levels[index]
        resample.y <- 0.5*old.stackPoints(resample, levels = resample.levels,
                                          vp = canvas$graphPath("sample")) +
                                          0.5*(resample.levels == canvas$ylevels[2]) + 1
        n <- canvas$n
        ntext <- min(n, 50)
        npcs <- (ntext:0)/ntext
        yunit <- (unit(npcs, "npc") - unit(4*(npcs - 0.5), "mm") + unit(1 - npcs, "lines"))
        max.width.x <- max(convertX(stringWidth(format(round(sample, canvas$dp), nsmall = canvas$dp)),
                                    "cm", valueOnly = TRUE))
        max.width.y <- max(convertX(stringWidth(canvas$ylevels),
                                    "cm", valueOnly = TRUE))
        max.width.x <- unit(max.width.x, "cm")
        max.width.y <- unit(max.width.y, "cm")
        if (n <= 50) finalobs <- n else finalobs <- 49
        for (i in 1:finalobs){
            value <- sample[i]
            lev <- levels[i]
            canvas$image <- addGrob(canvas$image, textGrob
                                    (label = format(round(value, canvas$dp), nsmall = canvas$dp),
                                     x = 0.25, y = yunit[i + 1], just = "top",
                                     gp = gpar(col = "red", fontface = 2), name = "samplePlot.temp.samp",
                                     vp = canvas$graphPath("databox", 1)))
            canvas$image <- addGrob(canvas$image, textGrob
                                    (label = lev,
                                     x = 0.75, y = yunit[i + 1], just = "top",
                                     gp = gpar(col = "red", fontface = 2), name = "samplePlot.temp.level",
                                     vp = canvas$graphPath("databox", 1)))
            canvas$image <- addGrob(canvas$image, pointsGrob
                                    (x = sample[i], y = sample.y[i], pch = 19, gp = gpar(col = "red"),
                                     name = "samplePlot.temp.point", vp = canvas$graphPath("animation.field")))
            resamp.ys <- c(FALSE, index == i)
            if (any(resamp.ys)){
                subset.resamp <- c(resamp.ys[1:(ntext + 1)])
                if (n != ntext) subset.resamp <- c(resamp.ys[1:ntext], any(resamp.ys[(ntext + 1):(n + 1)]))
                canvas$image <- addGrob(canvas$image, textGrob
                                        (label = format(round(value, canvas$dp), nsmall = canvas$dp),
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
                canvas$image <- addGrob(canvas$image, pointsGrob
                                        (x = rep(value, sum(resamp.ys)), y = resample.y[index == i],
                                         pch = 19,
                                         gp = gpar(col = "red"), name = "samplePlot.temp.resamplepoints",
                                         vp = canvas$graphPath("animation.field")))
                if (canvas$stopAnimation)
                    return()
                canvas$pauseImage(20)
                canvas$rmGrobs(c("samplePlot.temp.resamp", "samplePlot.temp.relevel", "samplePlot.temp.segments",
                                 "samplePlot.temp.resamplepoints"))
            } else {
                if (canvas$stopAnimation)
                    return()
                canvas$pauseImage(20)
            }
        }
        canvas$rmGrobs(c("samplePlot.temp.samp", "samplePlot.temp.level",
                         "samplePlot.temp.point", "samplePlot.points."))
        canvas$image <- addGrob(canvas$image, datatextGrob
                                (data = resample, title = "Re-sample", max = 50,
                                 gp = gpar(col = "red"), name = "samplePlot.databox.text.2",
                                 vp = canvas$graphPath("databox", 2)))
    }
}

dropBootArrow <- function(canvas, e, n.steps){
    top.level <- downViewport(canvas$graphPath("animation.field"))
    ani.xscale <- current.viewport()$xscale
    upViewport(top.level)
    top.level <- downViewport(canvas$graphPath("stat"))
    stat.xscale <- current.viewport()$xscale
    upViewport(top.level)
    scale.diff <- ani.xscale[1] - stat.xscale[1]
    arrowbounds <- canvas$stat.dist[c(canvas$plotted.stats, canvas$which.sample), , drop = FALSE]
    stats <- apply(arrowbounds, 1, diff)
    curr.arrow <- arrowbounds[length(stats),]
    curr.stat <- stats[length(stats)]
    y.max <- unit(1, "npc") - unit(2, "lines") - unit(0.5, "char")
    y <- stackPoints(stats, vp = canvas$graphPath("stat"), y.min = 0, y.max = y.max)
    y.start <- 1.4
    y.end <- y[length(stats)]
    y.step <- (y.start - y.end)/n.steps
    xs.start <- curr.arrow
    xs.end <- curr.arrow - curr.arrow[1] + scale.diff
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

showCIandStatsDiff <- function(canvas, e, ci = TRUE, points = TRUE){
    if (points) vp <- canvas$graphPath("stat") else vp <- canvas$graphPath("sample")
    if (ci){
        n.steps = 10
        top.level <- downViewport(canvas$graphPath("animation.field"))
        ani.xscale <- current.viewport()$xscale
        upViewport(top.level)
        top.level <- downViewport(canvas$graphPath("stat"))
        stat.xscale <- current.viewport()$xscale
        upViewport(top.level)
        scale.diff <- ani.xscale[1] - stat.xscale[1]
        xs <- canvas$x
        ys <- canvas$levels
        curr.arrow <- canvas$calcAllStats(xs, ys)
        y.start <- if (length(xs) >= canvas$hist.cutoff) 2.35 else 2.4
        y.end <- 0.10
        y.step <- (y.start - y.end)/n.steps
        xs.start <- curr.arrow
        xs.end <- curr.arrow - curr.arrow[1] + scale.diff
        xs.step <- (xs.start - xs.end)/n.steps
        col = "red"
        ## Moving arrow.
        for (i in 0:n.steps){
            temp.arrow <- linesGrob(x = unit(xs.start - i*xs.step, "native"),
                                    y = unit(rep(y.start - i*y.step, 2), "native"),
                                    gp = gpar(lwd = 2, col = col),
                                    arrow = arrow(length = unit(0.1, "inches")),
                                    vp = canvas$graphPath("animation.field"),
                                    name = "samplePlot.temp.arrow")
            canvas$image <- addGrob(canvas$image, temp.arrow)
            if (canvas$stopAnimation)
                return()
            canvas$drawImage()
        }
        if (is.categorical(canvas$x)) dps = 3 else dps = canvas$dp
        canvas$rmGrobs(c("samplePlot.temp.arrow", "dataPlot.ci.1"))
        stat <- xs.end[2] - scale.diff
        statline <- linesGrob(x = unit(xs.end, "native"),
                              y = unit(rep(y.end, 2), "native"),
                              gp = gpar(lwd = 2, col = "red"),
                              arrow = arrow(length = unit(0.1, "inches")),
                              vp = canvas$graphPath("animation.field"),
                              name = "statPlot.line")
        statarrow <- linesGrob(x = unit(rep(stat, 2), "native"),
                               y = unit.c(unit(0.10, "npc"), unit(0, "lines")),
                               gp = gpar(col = "red"),
                               arrow = arrow(length = unit(0.1, "inches")),
                               vp = canvas$graphPath("stat"),
                               name = "statPlot.arrow")
        stattext <- textGrob(#format(stat, digits = ciLabelDigits(stat)),
                             format(round(stat, dps), nsmall = dps),
                             x = unit(stat, "native"), y = 0,
                             just = c("centre", "top"),
                             gp = gpar(fontface = 2, col = "red"),
                             vp = canvas$graphPath("stat"),
                             name = "statPlot.text")
        canvas$image <- addGrob(canvas$image, statline)
        canvas$image <- addGrob(canvas$image, statarrow)
        canvas$image <- addGrob(canvas$image, stattext)
        if (canvas$stopAnimation)
            return()
        canvas$pauseImage(5)
        ## CI code.
        x <- apply(canvas$stat.dist, 1, diff)
        ci <- quantile(x, prob = c(0.025, 0.975), type = 1)
        #cilabs = format(ci, digits = ciLabelDigits(ci))
        cilabs <- format(round(ci, dps), nsmall = dps)
        start <- 5
        if (points){
            start <- 1
            y.max <- unit(1, "npc") - unit(2, "lines") - unit(0.5, "char")
            ## Set points outside interval to a lighter shade of grey.
            y <- canvas$stat.ypos
            x.in <- x[x >= ci[1] & x <= ci[2]]
            x.out <- x[x < ci[1] | x > ci[2]]
            y.in <- y[x >= ci[1] & x <= ci[2]]
            y.out <- y[x < ci[1] | x > ci[2]]
            points.in <- pointsGrob(x = x.in, y = y.in, gp = gpar(col = "grey60", lwd = 2, alpha = 0.7),
                                    vp = canvas$graphPath("stat"))
            if (length(x.out) > 0 & length(y.out) > 0) {
                points.out <- pointsGrob(x = x.out, y = y.out, gp = gpar(col = "lightgrey", lwd = 2, alpha = 1),
                                         vp = canvas$graphPath("stat"))
                points.all <- grobTree(points.out, points.in, name = "statPlot.points.1")
            } else {
                points.all <- grobTree(points.in, name = "statPlot.points.1")
            }
            canvas$image <- addGrob(canvas$image, points.all)
        }
        ## Replot statistic arrow to avoid it disappearing behind points.
        canvas$image <- addGrob(canvas$image, statline)
        canvas$image <- addGrob(canvas$image, statarrow)
        canvas$image <- addGrob(canvas$image, stattext)
        if (canvas$stopAnimation)
            return()
        canvas$pauseImage(5)
        ## Plot CI without bar.
        canvas$image <- addGrob(canvas$image, confintGrob(ci = ci, bar = FALSE, cilabs = cilabs,
                                                          height = unit(0.03, "npc"),
                                                          name = "statPlot.ci.1",
                                                          vp = vp))
        if (canvas$stopAnimation)
            return()
        canvas$pauseImage(5)
        ## Plot CI with bar.
        canvas$image <- editGrob(canvas$image, gPath("statPlot.ci.1"), bar = TRUE)
        canvas$showLabels()
        canvas$drawImage()

    } else {
        ## Summary stats code.
        x <- apply(canvas$stat.dist, 1, diff)
        mean.x <- round(mean(x), canvas$dp)
        sd.x <- round(sd(x), canvas$dp)
        ## Calculating maximum text line width.
        widths <- c(convertX(stringWidth("Mean:"), "cm", valueOnly = TRUE),
                    convertX(stringWidth(mean.x), "cm", valueOnly = TRUE),
                    convertX(stringWidth(sd.x), "cm", valueOnly = TRUE))
        max.width <- stringWidth(c("Mean:", mean.x, sd.x)[widths == max(widths)])
        xunit <- unit(0, "npc") + unit(1, "mm") + 0.5*max.width
        summarytext1 <- textGrob("Mean:", x = xunit, y = unit(0.6, "npc"),
                                 vp = vp, gp = gpar(fontface = 2),
                                 name = "summarytext1")
        summarytext2 <- textGrob(format(round(mean.x, canvas$dp), nsmall = canvas$dp), x = xunit,
                                 y = unit(0.6, "npc") - unit(1, "lines"),
                                 vp = vp, #gp = gpar(fontface = 2),
                                 name = "summarytext2")
        summarytext3 <- textGrob("SD:",
                                 x = xunit, y = unit(0.6, "npc") - unit(2, "lines"),
                                 vp = vp, gp = gpar(fontface = 2),
                                 name = "summarytext3")
        summarytext4 <- textGrob(format(round(sd.x, canvas$dp), nsmall = canvas$dp), x = xunit,
                                 y = unit(0.6, "npc") - unit(3, "lines"),
                                 vp = vp, #gp = gpar(fontface = 2),
                                 name = "summarytext4")
        summaryborder <- rectGrob(x = xunit, y = unit(0.6, "npc") + unit(0.5, "lines"),
                                  width = max.width + unit(2, "mm"),
                                  height = unit(2, "mm") + unit(4, "lines"),
                                  gp = gpar(fill = "white"),
                                  just = c("centre", "top"), vp = vp,
                                  name = "summaryborder")
        summarytext <- grobTree(summaryborder, summarytext1, summarytext2, summarytext3,
                                summarytext4, name = "statPlot.summary.1")
        canvas$image <- addGrob(canvas$image,  summarytext)
        canvas$showLabels()
        canvas$drawImage()
    }
}

diff1000 <- function(canvas, e, points){
    stats <- apply(canvas$stat.dist, 1, diff)
    for (i in 20*(1:50)){
        canvas$sampled.stats <- 1:i
        canvas$plotSample(e, i, alpha = 0.05)
        if (points){
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
    canvas$rmGrobs(c("samplePlot.databox.text.2", "samplePlot.stat.2",
                     "samplePlot.temp.data.points."))
    for (i in 1:2){
        canvas$rmGrobs(paste("samplePlot", c("line", "points", "boxplot"), i, sep = "."))
    }
    canvas$drawImage()
    canvas$sampled.stats <- NULL
    canvas$plotted.stats <- NULL
}

fadeAllCI <- function(canvas, e){
    ci <- getGrob(canvas$image, gPath("statPlot.ci.1"))
    line <- getGrob(canvas$image, gPath("statPlot.line"))
    arrow <- getGrob(canvas$image, gPath("statPlot.arrow"))
    text <- getGrob(canvas$image, gPath("statPlot.text"))
    writing <- textGrob(label = paste("CI for the difference between population ", canvas$stat.in.use, "s", sep = ""),
                        y = 0.2, just = "bottom", name = "statPlot.writing.1", vp = canvas$graphPath("stat"))
    canvas$image <- addGrob(canvas$image, rectGrob
                            (x = unit(0.5, "npc"), y = unit(2/3, "npc") - unit(5/3, "lines"),
                             width = unit(1, "npc"),
                             height = unit(2/3, "npc") - unit(1, "lines"),
                             just = "top",
                             gp = gpar(col = "white", fill = "white", alpha = 0.75),
                             vp = vpPath("canvas.all", "canvas.plots"),
                             name = "fadebox"))

    canvas$image <- addGrob(canvas$image, ci)
    canvas$image <- addGrob(canvas$image, line)
    canvas$image <- addGrob(canvas$image, arrow)
    canvas$image <- addGrob(canvas$image, text)
    canvas$image <- addGrob(canvas$image, writing)
    canvas$drawImage()
    canvas$rmGrobs(c("fadebox", "statPlot.writing.1"))
}

