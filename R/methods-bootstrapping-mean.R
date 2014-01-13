canvasBootMean <- setRefClass("canvasBootMeanClass", contains = "canvasPlotClass",
                             methods = list(
    plotSample = function(env, i = which.sample, ...) {
        plotSamplePointsAndBoxplotGhostMean(.self, env, i, ...)
    },

    showLabels = function() {
        bootLabels(.self)
    },

    calcStat = function(i = which.sample, y = NULL, canvas = .self) {
        calcMean(samples[[i]], y)
    },

    calcAllStats = function(a, b = NULL, canvas = .self) {
        calcMean(a, b)
    },

    plotDataStat = function(env, ...) {
        lineOnBoxplotMean(.self, env)
    },

    plotStatDist = function(env, ...) {
        plotBootDist(.self, env)
    },

    animateSample = function(...) {
        moveDataTextAndDropPoints(.self, ...)
    },

    trackSample = function(env) {
        trackBootstrap(.self, env)
    },

    animateStat = function(env, n.steps) {
        dropStat(.self, env, n.steps)
    },

    displayResult = function(env, ...) {
        showCIandStats(.self, env, ...)
    },

    handle1000 = function(env, ...) {
        boot1000mean(.self, env, ...)
    },

    fadePlots = function(env, ...) {
        fadeSampleAndStat(.self, env, ...)
    }))

load_bootstrap_mean <- function(e) {
    e$c1$stat.in.use <- svalue(e$stat)
    e$pairedSamples <- svalue(e$paired.samples)
    # There is something messy going on with viewports and
    # gTrees, such that when attempting to import them, we
    # end up with "uninitializedField"s. Assign to a temp
    # var and reassign later.
    tmp.vps <- e$c1$viewports
    tmp.image <- e$c1$image
    e$c1$viewports <- NULL
    e$c1$image <- NULL
    tmp.canvas <- canvasBootMean$new()
    tmp.canvas$import(e$c1)
    e$c1 <- tmp.canvas
    e$c1$viewports <- tmp.vps
    e$c1$image <- tmp.image
    e$difference <- FALSE
}

calcMean <- function(x, y = NULL)
    mean(x)

bootLabels <- function(canvas) {
    samplabel <- textGrob("Sample",
                          x = unit(0, "npc") + unit(1, "mm"),
                          y = unit(1, "npc") - unit(1, "mm"),
                          just = c("left", "top"),
                          name = "dataLabel",
                          vp = canvas$graphPath("data"),
                          gp = gpar(fontface = 2))
    methlabel <- textGrob("Module: 1 Sample Bootstrapping",
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
        varlabel <- textGrob(if (! is.null(canvas$paired.data))
                                 paste("Variable:", canvas$x.name, "-", canvas$y.name)
                             else
                                 paste("Variable:", canvas$x.name),
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
                                    vp = canvas$graphPath("data"))
    resamplabel <- textGrob("Re-sample",
                            x = unit(0, "npc") + unit(1, "mm"),
                            y = unit(0.8, "npc"),
                            just = c("left", "top"),
                            name = "sampleLabel",
                            vp = canvas$graphPath("sample"),
                            gp = gpar(fontface = 2))
    resampLabelBackground <- rectGrob(x = unit(0, "npc"),
                                      y = unit(0.8, "npc") + unit(1, "mm"),
                                      width = stringWidth(resamplabel$label) + unit(4, "mm"),
                                      height = stringHeight(resamplabel$label) + unit(2, "mm"),
                                      just = c("left", "top"),
                                      gp = gpar(col = "white", fill = "white", alpha = 0.7),
                                      name = "resampLabelBackground",
                                      vp = canvas$graphPath("sample"))
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

lineOnBoxplotMean <- function(canvas, e) {
    plotBoxplot(canvas, canvas$x, stat = mean, stat.color = "purple3",
                canvas$graphPath("data"), "dataPlot")
    mean.x <- mean(canvas$x)
    canvas$image <- addGrob(canvas$image, textGrob
                            (label = format(round(mean.x, canvas$dp), nsmall = canvas$dp),
                             x = unit(mean.x, "native"),
                             y = unit(0.05, "npc"),
                             just = c("centre", "centre"),
                             gp = gpar(col = "red", fontface = "bold"),
                             name = "dataPlot.stat.text",
                             vp = canvas$graphPath("data")))
}

plotSamplePointsAndBoxplotGhostMean <- function(canvas, e, i) {
    canvas$rmGrobs(c("dataPlot.ci.1", "samplePlot.rect.1"))
    alpha = 0.25
    canvas$sampled.stats <- c(canvas$sampled.stats, i)
    x <- canvas$samples[[i]]
    if (length(x) >= canvas$hist.cutoff) {
        plotHist(canvas, x, canvas$graphPath("sample"), "samplePlot")
    } else {
        y <- old.stackPoints(x, vp = canvas$graphPath("sample"))
        plotPoints(canvas, x, y, canvas$graphPath("sample"), "samplePlot", black = FALSE)
    }
    allinfo <- c(canvas$stat.dist, recursive = TRUE)
    canvas$image <- addGrob(canvas$image, rectGrob
                            (x = unit(allinfo[canvas$sampled.stats], "native"),
                             y = unit(0.15, "npc"), height = unit(0.2, "npc"),
                             width = 0, gp = gpar(alpha = alpha, col = "blue", lwd = 2),
                             vp = canvas$graphPath("sample"), name = "samplePlot.ghosts.1"))
    canvas$image <- addGrob(canvas$image, linesGrob
                            (x = unit(canvas$stat.dist[i], "native"),
                             y = unit(c(0.05, 0.5), "npc"), gp = gpar(lwd = 4, col = "blue"),
                             vp = canvas$graphPath("sample"), name = "samplePlot.lines.1"))
    canvas$image <- addGrob(canvas$image, datatextGrob(data = x, title = "Re-sample",
                                                       max = 50, name = "samplePlot.databox.text.2",
                                                       vp = canvas$graphPath("databox", 2), gp = gpar(col = "red")))
}

plotBootDist <- function(canvas, e){
    canvas$plotted.stats <- c(canvas$plotted.stats, canvas$which.sample)
    x <- c(canvas$stat.dist, recursive = TRUE)[canvas$plotted.stats]
    y.max <- unit(1, "npc") - unit(2, "lines") - unit(0.5, "char")
    y <- stackPoints(x, vp = canvas$graphPath("stat"), y.min = 0, y.max = y.max)
    plotPoints(canvas, x, y, canvas$graphPath("stat"), "statPlot", black = FALSE)
}

moveDataTextAndDropPoints <- function(canvas, e, drop.points = FALSE, n.steps = 10, n.slow = 5, max = 50){
    canvas$rmGrobs(c("dataPlot.ci.1", "samplePlot.databox.text.2", "samplePlot.points.1",
                     "samplePlot.lines.1", "samplePlot.boxplot.1"))
    paired.samples <- e$pairedSamples
    index <- canvas$indexes[[canvas$which.sample]]
    x <- canvas$x[index]
    y <- canvas$y[index]
    n <- canvas$n
    if (n < 100){
        n.slow <- min(n.slow, n)
        ## Calculating the position of text in text boxes.
        ntext <- min(n, max)
        npcs <- (ntext:0)/ntext
        yunit <- (unit(npcs, "npc") - unit(4*(npcs - 0.5), "mm") + unit(1 - npcs, "lines"))
        if (paired.samples){
            ## Calculating position of differences in datatext box.
            x.name <- canvas$x.name
            y.name <- canvas$y.name
            if (nchar(x.name) > 5){
                title1 <- paste(substr(x.name, 1, 5), "...", sep = "")
            } else {
                title1 <- x.name
            }
            if (nchar(y.name) > 5){
                title2 <- paste(substr(y.name, 1, 5), "...", sep = "")
            } else {
                title2 <- y.name
            }
            xsw <- max(stringWidth(c(title1, format(round(canvas$paired.data[, 1], canvas$dp),
                                                    nsmall = canvas$dp))))
            ysw <- max(stringWidth(c(title2, format(round(canvas$paired.data[, 2], canvas$dp),
                                                    nsmall = canvas$dp))))
            dsw <- max(stringWidth(c("diff", format(round(canvas$x, canvas$dp), nsmall = canvas$dp))))
            maxw <- max(xsw, ysw, dsw)
            sparespace1 <- (unit(0.5, "npc") - 3*maxw - unit(2, "mm"))*(1/6)
            sparespace2 <- (unit(1, "npc") - 3*maxw - unit(2, "mm"))*(1/6)
            x.text.start <- unit(0.5, "npc") - unit(1, "mm") - sparespace1 - 0.5*maxw
            x.text <- unit(1, "npc") - unit(1, "mm") - sparespace2 - 0.5*maxw
            arrowpos <- c(1.1, 1.0)
        } else {
            x.text.start <- unit(0.25, "npc")
            x.text <- unit(0.5, "npc")
            arrowpos <- c(0.9, 0.8)
        }
        x.text.end <- unit(0.75, "npc")
        x.text.step <- (x.text.end - x.text.start)*(1/n.steps)
        y.start <- y + 2
        y.end <- old.stackPoints(x, vp = canvas$graphPath("sample")) + 1
        y.step <- (y.end - y.start)/n.steps

        ## Animation of slow points.
        for (i in seq(from = 1, by = 1, length.out = n.slow)){
            y.text.start <- yunit[min(c(max + 1, index[i] + 1))]
            y.text.end <- yunit[i + 1]
            y.text.step <- convertY(y.text.end - y.text.start, "npc", valueOnly = TRUE)/n.steps

            # If we can, just draw a datatextGrob
            if (index[i] < ntext) {
                canvas$image <-  addGrob(canvas$image, datatextGrob
                                         (data = canvas$x,
                                          title = if (paired.samples) "diff" else canvas$x.name,
                                          max = 50, x = x.text,
                                          name = if (paired.samples) "text.sample3" else "text.sample",
                                          vp = canvas$graphPath("databox", 1),
                                          gp = gpar(col = c(rep("black", index[i]), "red",
                                                    rep("black", n - index[i])),
                                                    fontface = c(rep(1, index[i]),
                                                    2, rep(1, n - index[i])))))
            } else {
                # If the text is below the `...' redraw the datatextGrob
                # and draw a piece of text above the `...'
                canvas$image <-  addGrob(canvas$image, datatextGrob
                                         (data = canvas$x,
                                          title = if (paired.samples) "diff" else canvas$x.name,
                                          max = 50, x = x.text,
                                          name = if (paired.samples) "text.sample3" else "text.sample",
                                          vp = canvas$graphPath("databox", 1),
                                          gp = gpar(col = "black",
                                                    fontface = 1)))
                canvas$image <- addGrob(canvas$image, textGrob
                                        (label = format(round(x[i], canvas$dp), nsmall = canvas$dp),
                                         x = x.text,
                                         y = y.text.start, just = "top",
                                         gp = gpar(col = "red", fontface = 2), name = "samplePlot.temp.text",
                                         vp = canvas$graphPath("databox", 1)))
            }
            canvas$image <- addGrob(canvas$image, linesGrob
                                    (x = arrowpos,
                                     y = y.text.start - unit(0.5, "lines"),
                                     gp = gpar(lwd = 3, col = "red"),
                                     arrow = arrow(length = unit(0.1, "inches")), name = "samplePlot.temp.arrow",
                                     vp = canvas$graphPath("databox", 1)))
            ## Light up point to drop
            if (drop.points) {
                temp.point <- pointsGrob(x = x[i], y = (canvas$y[index])[i], pch = 19,
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
                                            (label = format(round(x[i], canvas$dp), nsmall = canvas$dp),
                                             y = y.text.start + unit(j*y.text.step, "npc"),
                                             x = x.text.start + j*x.text.step,
                                             just = "top", gp = gpar(col = "red", fontface = 2),
                                             name = "samplePlot.temp.text",
                                             vp = vpPath("canvas.all", "canvas.boxes")))
                } else {
                    # Can just embolden the datatextGrob on last step
                    canvas$rmGrobs("samplePlot.temp.text")
                    canvas$image <- addGrob(canvas$image, datatextGrob
                                            (data = x, title = "Re-sample", max = 50,
                                             name = "samplePlot.databox.text.2", vp = canvas$graphPath("databox", 2),
                                             gp = gpar(col = "red",
                                                       alpha = c(rep(1, i + 1), rep(0, n - i)),
                                                       fontface = c(rep(1, i), 2, rep(1, n - i)))))
                }
                # Drop point
                if (drop.points) {
                    canvas$image <- addGrob(canvas$image, pointsGrob
                                            (x = x[i], y = y.start[i] + j*y.step[i], pch = 19,
                                             vp = canvas$graphPath("animation.field"), name = "samplePlot.temp"))
                }
                if (canvas$stopAnimation)
                    return()
                canvas$drawImage()
                if (drop.points)
                    canvas$rmGrobs("samplePlot.temp")
            }

            ## Make points permanent if dropping
            if (drop.points)
                plotPoints(canvas, x[1:i], y.end[1:i] - 1, canvas$graphPath("sample"),
                           "samplePlot", black = FALSE)
            if (canvas$stopAnimation)
                return()
            canvas$pauseImage(5)
            ## Make text in resample databox permanent
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
        for (i in seq(from = n.slow + 1, by = 1, length.out = length.out)){
            y.text.start <- yunit[min(c(max + 1, index[i] + 1))]

            # If we do not encounter a `...' then redraw a datatextGrob
            if (index[i] < ntext) {
                canvas$rmGrobs("samplePlot.temp.text")
                canvas$image <- addGrob(canvas$image, datatextGrob
                                       (data = canvas$x,
                                        title = if (paired.samples) "diff" else canvas$x.name,
                                        max = 50, x = x.text,
                                        name = if (paired.samples) "text.sample3" else "text.sample",
                                        vp = canvas$graphPath("databox", 1),
                                        gp = gpar(col = c(rep("black", index[i]), "red",
                                                  rep("black", n - index[i])),
                                                  fontface = c(rep(1, index[i]), 2, rep(1, n - index[i])))))
            } else {
                # Just draw a plain datatextGrob but overlay the number
                # on top of the `...'
                canvas$image <- addGrob(canvas$image, datatextGrob
                                        (data = canvas$x,
                                         title = if (paired.samples) "diff" else canvas$x.name,
                                         max = 50, x = x.text,
                                         name = if (paired.samples) "text.sample3" else "text.sample",
                                         vp = canvas$graphPath("databox", 1),
                                         gp = gpar(col = "black",
                                                   fontface = 1)))
                canvas$image <- addGrob(canvas$image, textGrob
                                        (label = format(round(x[i], canvas$dp), nsmall = canvas$dp), x = x.text,
                                         y = y.text.start, just = "top",
                                         gp = gpar(col = "red", fontface = 2), name = "samplePlot.temp.text",
                                         vp = canvas$graphPath("databox", 1)))
            }
            canvas$image <- addGrob(canvas$image, linesGrob(x = arrowpos,
                                                            y = y.text.start - unit(0.5, "lines"),
                                                            gp = gpar(lwd = 3, col = "red"),
                                                            arrow = arrow(length = unit(0.1,
                                                                          "inches")),
                                                            name = "samplePlot.temp.arrow",
                                                            vp = canvas$graphPath("databox", 1)))
            ## Light up point to drop.
            if (drop.points){
                temp.point <- pointsGrob(x = x[i], y = (canvas$y[index])[i], pch = 19,
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
            if (drop.points)
                plotPoints(canvas, x[1:i], y.end[1:i] - 1, canvas$graphPath("sample"),
                           "samplePlot", black = FALSE)
            if (canvas$stopAnimation)
                return()
            canvas$pauseImage(5)
        }

        # Adding the `...' to the re-sample datatextGrob
        canvas$image <- addGrob(canvas$image, datatextGrob
                                (data = x, title = "Re-sample", max = 50,
                                 name = "samplePlot.databox.text.2", vp = canvas$graphPath("databox", 2),
                                 gp = gpar(col = "red",
                                           alpha = 1,
                                           fontface = 1)))

        ## Animation of points outside databox.
        for (i in seq(from = ntext, by = 1, length.out = n - ntext)){
            y.text.start <- yunit[min(c(max + 1, index[i] + 1))]

            # If we do not encounter a `...' then redraw a datatextGrob
            if (index[i] < ntext) {
                canvas$rmGrobs("samplePlot.temp.text")
                canvas$image <- addGrob(canvas$image, datatextGrob
                                       (data = canvas$x,
                                        title = if (paired.samples) "diff" else canvas$x.name,
                                        max = 50, x = x.text,
                                        name = if (paired.samples) "text.sample3" else "text.sample",
                                        vp = canvas$graphPath("databox", 1),
                                        gp = gpar(col = c(rep("black", index[i]), "red", rep("black", n - index[i])),
                                                  fontface = c(rep(1, index[i]), 2, rep(1, n - index[i])))))
            } else {
                # Just draw a plain datatextGrob but overlay the number
                # on top of the `...'
                canvas$image <- addGrob(canvas$image, datatextGrob
                                        (data = canvas$x,
                                         title = if (paired.samples) "diff" else canvas$x.name,
                                         max = 50, x = x.text,
                                         name = if (paired.samples) "text.sample3" else "text.sample",
                                         vp = canvas$graphPath("databox", 1),
                                         gp = gpar(col = "black",
                                                   fontface = 1)))
                canvas$image <- addGrob(canvas$image, textGrob
                                        (label = format(round(x[i], canvas$dp), nsmall = canvas$dp),
                                         x = x.text, y = y.text.start, just = "top",
                                         gp = gpar(col = "red", fontface = 2), name = "samplePlot.temp.text",
                                         vp = canvas$graphPath("databox", 1)))
            }
            canvas$image <- addGrob(canvas$image, linesGrob(x = arrowpos,
                                                            y = y.text.start - unit(0.5, "lines"),
                                                            gp = gpar(lwd = 3, col = "red"),
                                                            arrow = arrow(length = unit(0.1,
                                                                          "inches")),
                                                            name = "samplePlot.temp.arrow",
                                                            vp = canvas$graphPath("databox", 1)))

            ## Light up point to drop.
            if (drop.points){
                temp.point <- pointsGrob(x = x[i], y = (canvas$y[index])[i], pch = 19,
                                         vp = canvas$graphPath("data"), name = "samplePlot.temp.point")
                canvas$image <- addGrob(canvas$image, temp.point)
            }

            ## Plot dropped point.
            if (drop.points)
                plotPoints(canvas, x[1:i], y.end[1:i] - 1, canvas$graphPath("sample"),
                           "samplePlot", black = FALSE)
            if (canvas$stopAnimation)
                return()
            canvas$pauseImage(5)
        }
        plotPoints(canvas, x, y.end - 1, canvas$graphPath("sample"),
                   "samplePlot", black = FALSE)
        canvas$plotSample(e, canvas$which.sample)
        if (canvas$stopAnimation)
            return()
        canvas$pauseImage(20)
        canvas$rmGrobs("samplePlot.temp.text")
        canvas$image <- addGrob(canvas$image, datatextGrob
                                (data = canvas$x,
                                 title = if (paired.samples) "diff" else canvas$x.name,
                                 max = 50, x = x.text,
                                 name = if (paired.samples) "text.sample3" else "text.sample",
                                 vp = canvas$graphPath("databox", 1),
                                 gp = gpar(col = "black", fontface = 1)))
        canvas$rmGrobs("samplePlot.temp.arrow")
        if (drop.points){
            canvas$rmGrobs("samplePlot.temp.point")
        }
    }
}

trackBootstrap <- function(canvas, e){
    paired.samples <- e$pairedSamples
    if (canvas$n < 100){
        index <- canvas$indexes[[canvas$which.sample]]
        sample <- canvas$x
        sample.y <- canvas$y
        resample <- sample[index]
        resample.y <- old.stackPoints(resample, vp = canvas$graphPath("sample"))
        n <- canvas$n
        ntext <- min(n, 50)
        npcs <- (ntext:0)/ntext
        if (paired.samples){
            x.name <- canvas$x.name
            y.name <- canvas$y.name
            if (nchar(x.name) > 5){
                title1 <- paste(substr(x.name, 1, 5), "...", sep = "")
            } else {
                title1 <- x.name
            }
            if (nchar(y.name) > 5){
                title2 <- paste(substr(y.name, 1, 5), "...", sep = "")
            } else {
                title2 <- y.name
            }
            xsw <- max(stringWidth(c(title1, format(round(canvas$paired.data[, 1], canvas$dp),
                                                    nsmall = canvas$dp))))
            ysw <- max(stringWidth(c(title2, format(round(canvas$paired.data[, 2], canvas$dp),
                                                    nsmall = canvas$dp))))
            dsw <- max(stringWidth(c("diff", format(round(canvas$x, canvas$dp), nsmall = canvas$dp))))
            maxw <- max(xsw, ysw, dsw)
            sparespace1 <- (unit(0.5, "npc") - 3*maxw - unit(2, "mm"))*(1/6)
            sparespace2 <- (unit(1, "npc") - 3*maxw - unit(2, "mm"))*(1/6)
            x.text.start <- unit(0.5, "npc") - unit(1, "mm") - sparespace1 - 0.5*maxw
            x.text <- unit(1, "npc") - unit(1, "mm") - sparespace2 - 0.5*maxw
        } else {
            x.text.start <- unit(0.25, "npc")
            x.text <- unit(0.5, "npc")
        }
        yunit <- (unit(npcs, "npc") - unit(4*(npcs - 0.5), "mm") + unit(1 - npcs, "lines"))
        canvas$image <- addGrob(canvas$image, datatextGrob
                                (data = resample, title = "Re-sample", max = 50, x = x.text,
                                 name = "samplePlot.databox.text.2", vp = canvas$graphPath("databox", 2)))
        max.width <- max(convertX(stringWidth(format(round(sample, canvas$dp), nsmall = canvas$dp)), "cm", valueOnly = TRUE))
        max.width <- unit(max.width, "cm")
        if (n <= 50) finalobs <- n else finalobs <- 49
        for (i in 1:finalobs) {
            value <- sample[i]
            canvas$image <- addGrob(canvas$image, datatextGrob
                                    (data = sample,
                                     title = if (paired.samples) "diff" else canvas$x.name,
                                     max = 50, x = x.text,
                                     name = if (paired.samples) "text.sample3" else "text.sample",
                                     vp = canvas$graphPath("databox", 1),
                                     gp = gpar(col = c(rep("black", i), "red", rep("black", n - i)),
                                               fontface = c(rep(1, i), 2, rep(1, n - i)))))
            canvas$image <- addGrob(canvas$image, pointsGrob
                                    (x = sample[i], y = sample.y[i], pch = 19, gp = gpar(col = "red"),
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
                                         name = "samplePlot.databox.text.2",
                                         vp = canvas$graphPath("databox", 2),
                                         gp = gpar(col = cols,
                                                   fontface = ff)))

                # If we have a ... at the end of our grob, add a textGrob
                if (n != ntext & any(resamp.ys[(ntext + 1):(n + 1)])) {
                    canvas$image <- addGrob(canvas$image, textGrob
                                            (label = format(round(value, canvas$dp), nsmall = canvas$dp),
                                             x = unit(0.5, "npc"), y = min(yunit), just = "top",
                                             gp = gpar(col = "red", fontface = 2), name = "samplePlot.temp.resamp",
                                             vp = canvas$graphPath("databox", 2)))
                }

                canvas$image <- addGrob(canvas$image, segmentsGrob
                                        (x0 = x.text.start + max.width*0.5 + unit(1, "mm"),
                                         x1 = unit(0.75, "npc") - max.width*0.5 - unit(1, "mm"),
                                         y0 = yunit[i + 1] - unit(0.5, "lines"),
                                         y1 = yunit[subset.resamp] - unit(0.5, "lines"),
                                         name = "samplePlot.temp.segments", gp = gpar(lwd = 2, col = "red"),
                                         vp = vpPath("canvas.all", "canvas.boxes")))
                canvas$image <- addGrob(canvas$image, pointsGrob
                                        (x = rep(value, sum(resamp.ys)), y = resample.y[index == i],
                                         pch = 19,
                                         gp = gpar(col = "red"), name = "samplePlot.temp.resamplepoints",
                                         vp = canvas$graphPath("sample")))
                if (canvas$stopAnimation)
                    return()
                canvas$pauseImage(20)
                # Only removing a textGrob if it is known to appear on a ...
                if (n != ntext & any(resamp.ys[(ntext + 1):(n + 1)])){
                    canvas$rmGrobs("samplePlot.temp.resamp")
                }
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
                canvas$pauseImage(20)
            }
        }
        canvas$rmGrobs(c("samplePlot.temp.samp", "samplePlot.temp.point", "text.sample"))
        # Resetting datatextGrobs after animation has been completed
        canvas$image <- addGrob(canvas$image, datatextGrob
                                (data = sample,
                                 title = if (paired.samples) "diff" else canvas$x.name,
                                 max = 50, x = x.text,
                                 gp = gpar(col = "black"),
                                 name = if (paired.samples) "text.sample3" else "text.sample",
                                 vp = canvas$graphPath("databox", 1)))
        canvas$image <- addGrob(canvas$image, datatextGrob
                                (data = resample, title = "Re-sample", max = 50, gp = gpar(col = "red"),
                                 name = "samplePlot.databox.text.2", vp = canvas$graphPath("databox", 2)))
    }
}
dropStat <- function(canvas, e, n.steps, linelength = 0.2){
    xs <- c(canvas$stat.dist, recursive = TRUE)[c(canvas$plotted.stats,
                              canvas$which.sample)]
    x <- xs[length(xs)]
    y.max <- unit(1, "npc") - unit(2, "lines") - unit(0.5, "char")
    ys <- stackPoints(xs, vp = canvas$graphPath("stat"), y.min = 0,
                      y.max = y.max)
    y.start <- 1
    y.end <- ys[length(ys)]
    y.step <- (y.start - y.end)/n.steps
    linediff <- linelength - 0.05
    for (i in 0:n.steps){
        canvas$image <- addGrob(canvas$image, linesGrob
                                (x = unit(rep(x, 2), "native"),
                                 y = unit(c(y.start - i*y.step, y.start - i*y.step + linelength),
                                 "native"),
                                 gp = gpar(lwd = 4, col = "red"),
                                 vp = canvas$graphPath("animation.field"),
                                 name = "samplePlot.temp"))
        if (canvas$stopAnimation)
            return()
        canvas$drawImage()
        linelength <- linelength - linediff/n.steps
    }
    canvas$rmGrobs("samplePlot.temp")
}


boot1000mean <- function(canvas, e, points = FALSE){
    canvas$rmGrobs(c("samplePlot.databox.text.2", "dataPlot.ci.1",
                     "samplePlot.datapoints.points.1", "samplePlot.data.samp"))
    allx <- c(canvas$stat.dist, recursive = TRUE)
    for (i in 20*(1:50)){
        canvas$rmGrobs(c("samplePlot.datapoints.points.1", "samplePlot.data.samp"))
        x <- allx[1:i]
        canvas$plotSample(e, i)
        if (points){
            y <- canvas$stat.ypos[1:i]
            plotPoints(canvas, x, y, canvas$graphPath("stat"),
                       "statPlot", black = FALSE, alpha = 0.7)
        }
        canvas$image <- addGrob(canvas$image, rectGrob
                                (x = unit(x, "native"),
                                 y = unit(0.15, "npc"), height = unit(0.2, "npc"),
                                 width = 0, gp = gpar(alpha = 0.05, col = "blue", lwd = 2),
                                 vp = canvas$graphPath("sample"), name = "samplePlot.ghosts.1"))
        if (canvas$stopAnimation){
            return()
        }
        canvas$showLabels()
        canvas$drawImage()
    }
    canvas$rmGrobs(c("samplePlot.points.1", "samplePlot.lines.1",
                     "samplePlot.datapoints.points.1", "samplePlot.databox.text.2",
                     "samplePlot.stat.1", "samplePlot.hist.1"))
    canvas$drawImage()
    canvas$sampled.stats <- NULL
    canvas$plotted.stats <- NULL
}

showCIandStats <- function(canvas, e, ci = TRUE, points = TRUE){
    if (points) vp <- canvas$graphPath("stat") else vp <-canvas$graphPath("sample")
    if (ci){
        ## CI code.
        canvas$rmGrobs("dataPlot.ci.1")
        x <- c(canvas$stat.dist, recursive = TRUE)
        ci <- quantile(x, prob = c(0.025, 0.975), type = 1)
        #cilabs <- format(ci, digits = ciLabelDigits(ci))
        if (is.categorical(canvas$x)) dps <- 3 else dps = canvas$dp
        cilabs <- format(round(ci, dps), nsmall = dps)
        start <- 5
        if (points) {
            start <- 1
            ## Set points outside interval to a lighter shade of grey.
            y.max <- unit(1, "npc") - unit(2, "lines") - unit(0.5, "char")
            y <- canvas$stat.ypos
            x.in <- x[x >= ci[1] & x <= ci[2]]
            x.out <- x[x < ci[1] | x > ci[2]]
            y.in <- y[x >= ci[1] & x <= ci[2]]
            y.out <- y[x < ci[1] | x > ci[2]]
            points.in <- pointsGrob(x = x.in, y = y.in, gp = gpar(col = "grey60", lwd = 2, alpha = 0.7),
                                    vp = canvas$graphPath("stat"))
            # When we have no points outside the boundaries (often the
            # case in medians) don't show grey points
            if (length(x.out) > 0 & length(y.out) > 0) {
                points.out <- pointsGrob(x = x.out, y = y.out, gp = gpar(col = "lightgrey", lwd = 2, alpha = 1),
                                         vp = canvas$graphPath("stat"))
                points.all <- grobTree(points.out, points.in, name = "statPlot.points.1")
            } else {
                points.all <- grobTree(points.in, name = "statPlot.points.1")
            }
            canvas$image <- addGrob(canvas$image, points.all)
        }
        if (canvas$stopAnimation)
            return()
        canvas$showLabels()
        canvas$pauseImage(5)
        ## Plot CI without bar.
        # Need to check that the labels don't exceed the bounds
        depth <- downViewport("stat.data.1")
        conf.xs <- convertX(unit(ci, "native"), "npc", valueOnly = TRUE)
        str.widths <- convertWidth(stringWidth(cilabs), "npc", valueOnly = TRUE)
        label.edges <- conf.xs + c(-1, 1) * str.widths
        upViewport(depth)
        fix.label.pos <- logical(2)
        if (label.edges[1] < 0)
            fix.label.pos[1] <- TRUE
        if (label.edges[2] > 1)
            fix.label.pos[2] <- TRUE
        canvas$image <- addGrob(canvas$image, confintGrob(ci = ci, bar = FALSE,
                                                          cilabs = cilabs,
                                                          fix.label.pos = fix.label.pos,
                                                          name = "statPlot.ci.1",
                                                          vp = vp))
        if (canvas$stopAnimation)
            return()
        canvas$pauseImage(5)
        ## Plot CI with bar.
        canvas$image <- editGrob(canvas$image, gPath("statPlot.ci.1"), bar = TRUE)
        if (canvas$stopAnimation)
            return()
        canvas$pauseImage(5)
        ## Animate CI.
        for (i in start:10) {
            # When categorical, don't draw within the propbar
            offset <- if (i == 10 & is.categorical(canvas$x)) 0 else 0.15
            canvas$image <- addGrob(canvas$image, rectGrob(x = unit(ci[1], "native"),
                                                           y = unit(offset + i*0.2, "native"),
                                                           height = unit(0.01, "native"),
                                                           width = unit(diff(ci), "native"),
                                                           just = c("left", "centre"), vp =
                                                           canvas$graphPath("animation.field"),
                                                           gp = gpar(col = "red", fill = "red"),
                                                           name = "samplePlot.temp"))
            if (canvas$stopAnimation)
                return()
            canvas$drawImage()
            if (i == 5){
                canvas$pauseImage(5)
                canvas$image <- addGrob(canvas$image, rectGrob(x = unit(ci[1], "native"),
                                                       y = unit(0.15, "npc"),
                                                       height = unit(0.015, "npc"),
                                                       width = unit(diff(ci), "native"),
                                                       just = c("left", "centre"), vp =
                                                       canvas$graphPath("sample"),
                                                       gp = gpar(col = "red", fill = "red"),
                                                       name = "samplePlot.rect.1"))
            }
        }
        canvas$rmGrobs("samplePlot.temp")
        confat <- unit(if (is.categorical(canvas$x)) 0 else 0.15, "npc")
        canvas$image <- addGrob(canvas$image, confintGrob(ci = ci,
                                                          cilabs = cilabs,
                                                          at = confat,
                                                          fix.label.pos = fix.label.pos,
                                                          name = "dataPlot.ci.1",
                                                          vp = canvas$graphPath("data")))
        canvas$drawImage()

    } else {
        ## Summary stats code.
        x <- c(canvas$stat.dist, recursive = TRUE)
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

fadeSampleAndStat <- function(canvas, e){
    x <- c(canvas$stat.dist, recursive = TRUE)
    ci <- quantile(x, prob = c(0.025, 0.975), type = 1)
    #cilabs <- format(ci, digits = ciLabelDigits(ci))
    if (is.categorical(canvas$x)) dps <- 3 else dps = canvas$dp
    cilabs <- format(round(ci, dps), nsmall = dps)
    confat <- unit(if (is.categorical(canvas$x)) 0 else 0.15, "npc")
    canvas$image <- addGrob(canvas$image, rectGrob
                            (x = unit(0.5, "npc"), y = unit(2/3, "npc") - unit(5/3, "lines"),
                             width = unit(1, "npc"),
                             height = unit(2/3, "npc") - unit(1, "lines"),
                             just = "top",
                             gp = gpar(col = "white", fill = "white", alpha = 0.75),
                             vp = vpPath("canvas.all", "canvas.plots"),
                             name = "fadebox"))

    # Need to check that the labels don't exceed the bounds
    depth <- downViewport("stat.data.1")
    conf.xs <- convertX(unit(ci, "native"), "npc", valueOnly = TRUE)
    str.widths <- convertWidth(stringWidth(cilabs), "npc", valueOnly = TRUE)
    label.edges <- conf.xs + c(-1, 1) * str.widths
    upViewport(depth)
    fix.label.pos <- logical(2)
    if (label.edges[1] < 0)
        fix.label.pos[1] <- TRUE
    if (label.edges[2] > 1)
        fix.label.pos[2] <- TRUE

    canvas$image <- addGrob(canvas$image,
                            confintGrob(ci = ci,
                                        cilabs = cilabs,
                                        at = confat,
                                        fix.label.pos = fix.label.pos,
                                        name = "dataPlot.ci.1",
                                        vp = canvas$graphPath("data")))
    canvas$drawImage()
    canvas$rmGrobs("fadebox")
}
