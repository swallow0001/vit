canvasBootIQR <- setRefClass("canvasBootIQRClass", contains = "canvasPlotClass",
                             methods = list(
    plotSample = function(env, i = which.sample, ...) {
        plotSampleLevelsIQR(.self, env, i, ...)
    },

    showLabels = function() {
        bootLabelsTwoSample(.self)
    },

    calcStat = function(i = which.sample, ys = NULL, canvas = .self) {
        ys <- level.samples[[i]]
        calcIQR(samples[[i]], ys, .self)
    },

    calcAllStats = function(xs, ys = NULL, canvas = .self) {
        calcIQR(xs, ys, .self)
    },

    plotDataStat = function(env, ...) {
        linesOnQuartiles(.self, env)
    },

    plotStatDist = function(env, ...) {
        plotIQRRatioDist(.self, env)
    },

    animateSample = function(...) {
        dropPointsIQR(.self, ...)
    },

    trackSample = function(env) {
        trackBootstrapDiff(.self)
    },

    animateStat = function(env, n.steps) {
        dropIQRStat(.self, env, n.steps)
    },

    displayResult = function(env, ...) {
        showCIandStatsIQR(.self, env, ...)
    },

    handle1000 = function(env, ...) {
        IQRRatio1000(.self, env, ...)
    },

    fadePlots = function(env, ...) {
        fadeIQRRatioCI(.self, env, ...)
    }))

load_bootstrap_iqr_ratio <- function(e) {
    e$c1$stat.in.use <- svalue(e$stat)
    e$c1$orderLevels()
    e$resampWithin <- enabled(e$resamp.within) & svalue(e$resamp.within)
    # There is something messy going on with viewports and
    # gTrees, such that when attempting to import them, we
    # end up with "uninitializedField"s. Assign to a temp
    # var and reassign later.
    tmp.vps <- e$c1$viewports
    tmp.image <- e$c1$image
    e$c1$viewports <- NULL
    e$c1$image <- NULL
    tmp.canvas <- canvasBootIQR$new()
    tmp.canvas$import(e$c1)
    e$c1 <- tmp.canvas
    e$c1$viewports <- tmp.vps
    e$c1$image <- tmp.image
    e$difference <- TRUE
}

dropIQRStat <- function(canvas, e, n.steps) {
    top.level <- downViewport(canvas$graphPath("animation.field"))
    ani.xscale <- current.viewport()$xscale
    upViewport(top.level)
    top.level <- downViewport(canvas$graphPath("stat"))
    stat.xscale <- current.viewport()$xscale
    upViewport(top.level)
    scale.diff <- ani.xscale[1] - stat.xscale[1]
    index <- canvas$indexes[[canvas$which.sample]]
    xs <- canvas$x[index]
    ys <- as.character(canvas$levels[index])
    ylevels <- canvas$ylevels
    # Start with top arrow
    curr.arrows <- fivenum(xs[ys == ylevels[2]])[c(2, 4)]
    curr.arrows <- c(curr.arrows, fivenum(xs[ys == ylevels[1]])[c(2, 4)])
    iqrs <- c(diff(curr.arrows[1:2]), diff(curr.arrows[3:4]))
    iqr.ratio <- iqrs[1] / iqrs[2]
    maxarrow <- max(iqrs)
    y.start <- c(1.525, 1.125)
    y.end <- c(0.825, 0.675)
    y.step <- (y.start - y.end)/n.steps
    x0.start <- unit(curr.arrows[c(1, 3)], "native")
    x1.start <- unit(curr.arrows[c(2, 4)], "native")
    x0.end <- unit(0.5, "npc") - 0.5*(x1.start - x0.start)
    x.step <- (x0.end - x0.start)*(1/n.steps)
    for (i in 0:n.steps){
        arrows <- segmentsGrob(x0 = x0.start + i*x.step, y0 = y.start - i*y.step,
                               x1 = x1.start + i*x.step, y1 = y.start - i*y.step,
                               default.units = "native",
                               arrow = arrow(length = unit(0.1, "inches")),
                               name = "samplePlot.temp.arrows", gp = gpar(lwd = 3, col = "red"),
                               vp = canvas$graphPath("animation.field"))
        canvas$image <- addGrob(canvas$image, arrows)
        if (canvas$stopAnimation)
            return()
        canvas$drawImage()
    }

    temp.rect <- roundrectGrob(x = unit(0.5, "npc"), y = unit(0.75, "native"),
                               width = unit(maxarrow, "native") + unit(0.2, "inches"),
                               height = unit(0.1, "native") + unit(0.4, "inches"),
                               name = "samplePlot.temp.rect",
                               gp = gpar(fill = "black"), r = unit(0.2, "snpc"),
                               vp = canvas$graphPath("animation.field"))
    canvas$image <- addGrob(canvas$image, temp.rect)

    currstat <- iqr.ratio
    if (! any(iqrs == 0)) {
        sizemult <- (log10(currstat - stat.xscale[1]))/ diff(stat.xscale)
        halfheight <- 0.03 + 0.16*sizemult
        fontsize <- 12 + 64*sizemult
        stattext <- textGrob(label = format(round(currstat, canvas$dp), nsmall = canvas$dp),
                             x = unit(0.5, "npc"),
                             y = unit(0.75, "npc"), gp = gpar(col = "red", fontsize = fontsize),
                             name = "samplePlot.temp.text")
    } else {
        stattext <- textGrob(label = "NA", x = unit(0.5, "npc"),
                             y = unit(0.75, "npc"), gp = gpar(col = "red", fontsize = 36),
                             name = "samplePlot.temp.text")
    }
    divchar <- pointsGrob(pch = as.hexmode("00F7"),
                          x = unit(0.5, "npc"), y = unit(0.75, "npc"),
                          gp = gpar(col = "red", cex = 36 / 10),
                          name = "samplePlot.temp.divchar",
                          vp = canvas$graphPath("stat"))
    canvas$image <- addGrob(canvas$image, arrows) # Adding arrows above the black rect
    canvas$image <- addGrob(canvas$image, divchar)
    if (canvas$stopAnimation)
        return()
    canvas$pauseImage(10)

    top.level <- downViewport(canvas$graphPath("animation.field"))
    rect.width <- convertWidth(unit(0.5 * maxarrow, "native") + unit(0.1, "inches"), "npc", valueOnly = TRUE)
    rect.right.boundary <- unit(0.5 + rect.width, "npc")
    rrb.npc <- 0.5 + rect.width
    mm.in.npc <- convertWidth(unit(1, "mm"), "npc", valueOnly = TRUE)
    upViewport(top.level)

    top.level <- downViewport(canvas$graphPath("stat"))
    stat.in.npc <- convertX(unit(log10(currstat), "native"), "npc", valueOnly = TRUE)
    upViewport(top.level)

    canvas$image <- addGrob(canvas$image, textGrob
                            (label = "=",
                             x = rect.right.boundary + unit(3, "mm"),
                             y = unit(0.75, "native"),
                             just = c("left", "centre"),
                             gp = gpar(col = "red", fontsize = 36),
                             name = "samplePlot.temp.equals",
                             vp = canvas$graphPath("animation.field")))
    stattext <- editGrob(stattext, vp = canvas$graphPath("stat"),
                         x = unit(rrb.npc + (13 * mm.in.npc), "npc"),
                         just = c("left", "centre"))
    canvas$image <- addGrob(canvas$image, stattext)
    if (canvas$stopAnimation)
        return()
    canvas$pauseImage(5)

    if (! any(iqrs == 0)) {
        stattext$label <- as.numeric(stattext$label)
        tmp.label <- as.numeric(stattext$label)
        if (tmp.label < 1) {
            tmp.label <- paste("1 :", format(round(1 / tmp.label, canvas$dp), nsmall = canvas$dp))
        } else {
            tmp.label <- paste(format(round(tmp.label, canvas$dp), nsmall = canvas$dp), ": 1")
        }
        stattext$label <- tmp.label
        canvas$image <- addGrob(canvas$image, stattext)
        if (canvas$stopAnimation)
            return()
        canvas$pauseImage(10)
        stattext <- editGrob(stattext, vp = canvas$graphPath("stat"),
                             x = unit(rrb.npc + (13 * mm.in.npc), "npc"),
                             just = c("left", "centre"))
        if (canvas$stopAnimation)
            return()
        canvas$pauseImage(5)

        x.start <- rrb.npc + (13 * mm.in.npc)
        x.end <- stat.in.npc
        x.step <- (x.end - x.start) / n.steps
        y.start <- 0.75
        y.end <- 0
        y.step <- (y.end - y.start) / n.steps
        for (i in 1:n.steps) {
            stattext <- editGrob(stattext,
                                 x = unit(x.start + i*x.step, "npc"),
                                 y = unit(y.start + i*y.step, "npc"),
                                 hjust = 0 + 0.5*(i / n.steps),
                                 vjust = 0.5*(1 - i/n.steps))
            canvas$image <- addGrob(canvas$image, stattext)
            if (canvas$stopAnimation)
                return()
            canvas$drawImage()
        }

        if (canvas$stopAnimation)
            return()
        canvas$pauseImage(5)
    } else {
        canvas$pauseImage(10)
    }

    canvas$rmGrobs(c("samplePlot.temp.text", "samplePlot.temp.arrows",
                     "samplePlot.temp.rect", "samplePlot.temp.divchar", "samplePlot.temp.equals"))
}

dropPointsIQR <- function(canvas, e, drop.points = FALSE, n.steps = 10, n.slow = 5, max = 50) {
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
    iqrs <- calcIQR(x, levels, canvas)

    if (n < 100) {
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
            if (canvas$stopAnimation)
                return()
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

        canvas$rmGrobs(c("samplePlot.resamp.text", "samplePlot.temp.datatext", "samplePlot.temp.point"))
        plotSampleLevelsIQR(canvas, e, canvas$which.sample, show.iqr = FALSE)
        if (canvas$stopAnimation)
            return()
        canvas$pauseImage(5)

        # Now that points have been dropped, show calculation of the IQR ratio
        # Only do this on the '1' sample animation
        if (n.slow > 0) {
            n.steps <- 10
            ylevels <- canvas$ylevels
            # Start with top arrow
            curr.arrows <- fivenum(x[levels == ylevels[2]])[c(2, 4)]
            curr.arrows <- c(curr.arrows, fivenum(x[levels == ylevels[1]])[c(2, 4)])
            maxarrow <- max(abs(c(diff(curr.arrows[1:2]), diff(curr.arrows[3:4]))))
            y.start <- c(1.525, 1.125)
            y.end <- c(1.475, 1.325)
            y.step <- (y.start - y.end)/n.steps
            x0.start <- unit(curr.arrows[c(1, 3)], "native")
            x1.start <- unit(curr.arrows[c(2, 4)], "native")
            x0.end <- unit(0.5, "npc") - 0.5*(x1.start - x0.start)
            x.step <- (x0.end - x0.start)*(1/n.steps)
            for (i in 0:n.steps){
                arrows <- segmentsGrob(x0 = x0.start + i*x.step, y0 = y.start - i*y.step,
                                       x1 = x1.start + i*x.step, y1 = y.start - i*y.step,
                                       default.units = "native",
                                       arrow = arrow(length = unit(0.1, "inches")),
                                       name = "samplePlot.temp.arrows", gp = gpar(lwd = 3, col = "red"),
                                       vp = canvas$graphPath("animation.field"))
                canvas$image <- addGrob(canvas$image, arrows)
                if (canvas$stopAnimation)
                    return()
                canvas$drawImage()
            }

            canvas$rmGrobs(c("samplePlot.temp.arrows", "dataPlot.ci.1"))
            canvas$image <- addGrob(canvas$image, roundrectGrob
                                    (x = unit(0.5, "npc"), y = unit(1.4, "native"),
                                     width = unit(maxarrow, "native") + unit(0.2, "inches"),
                                     height = unit(0.1, "native") + unit(0.4, "inches"),
                                     name = "samplePlot.temp.rect",
                                     gp = gpar(fill = "black"), r = unit(0.2, "snpc"),
                                     vp = canvas$graphPath("animation.field")))
            if (canvas$stopAnimation)
                return()
            canvas$drawImage()

            currstat <- iqrs[2] / iqrs[1]
            if (any(iqrs == 0))
                currstat <- NA
            halfheight <- 0.10
            fontsize <- 36
            stattext <- textGrob(label = format(round(currstat, canvas$dp), nsmall = canvas$dp), x = unit(0.5, "npc"),
                                 y = unit(1.4, "native"), gp = gpar(col = "red", fontsize = fontsize),
                                 name = "samplePlot.temp.text")
            divchar <- pointsGrob(pch = as.hexmode("00F7"),
                                  x = unit(0.5, "npc"), y = unit(1.4, "native"),
                                  gp = gpar(col = "red", cex = fontsize / 10),
                                  name = "samplePlot.temp.divchar",
                                  vp = canvas$graphPath("animation.field"))
            canvas$image <- addGrob(canvas$image, arrows) # Adding arrows above the black rect
            canvas$image <- addGrob(canvas$image, divchar)
            if (canvas$stopAnimation)
                return()
            canvas$pauseImage(10)
            canvas$rmGrobs(c("samplePlot.temp.divchar", "samplePlot.temp.text", "samplePlot.temp.arrows"))
            canvas$image <- addGrob(canvas$image, roundrectGrob
                                    (x = unit(0.5, "npc"), y = unit(1.4, "native"),
                                     width = unit(halfheight/2, "npc"),
                                     height = unit(halfheight, "npc"), name = "samplePlot.temp.rect",
                                     gp = gpar(fill = "black"), r = unit(0.2, "snpc"),
                                     vp = canvas$graphPath("animation.field")))
            canvas$drawImage()
            canvas$image <- addGrob(canvas$image, roundrectGrob
                                    (x = unit(0.5, "npc"), y = unit(1.4, "native"),
                                     width = unit(halfheight, "npc"),
                                     height = unit(halfheight/2, "npc"), name = "samplePlot.temp.rect",
                                     gp = gpar(fill = "black"), r = unit(0.2, "snpc"),
                                     vp = canvas$graphPath("animation.field")))
            canvas$drawImage()
            canvas$image <- addGrob(canvas$image, roundrectGrob
                                    (x = unit(0.5, "npc"), y = unit(1.4, "native"),
                                     width = grobWidth(stattext) + unit(5, "mm"),
                                     height = unit(halfheight, "npc"), name = "samplePlot.temp.rect",
                                     gp = gpar(fill = "black"), r = unit(0.2, "snpc"),
                                     vp = canvas$graphPath("animation.field")))
            stattext <- editGrob(stattext, vp = canvas$graphPath("animation.field"))
            canvas$image <- addGrob(canvas$image, stattext)
            if (canvas$stopAnimation)
                return()
            canvas$pauseImage(20)
            canvas$rmGrobs(c("samplePlot.temp.text", "samplePlot.temp.rect"))
        }

        canvas$plotSample(e, canvas$which.sample)
        if (canvas$stopAnimation)
            return()
        canvas$pauseImage(20)
    }
}

fadeIQRRatioCI <- function(canvas, e){
    ci <- getGrob(canvas$image, gPath("statPlot.ci.1"))
    line <- getGrob(canvas$image, gPath("statPlot.line"))
    arrow <- getGrob(canvas$image, gPath("statPlot.arrow"))
    text <- getGrob(canvas$image, gPath("statPlot.text"))
    writing <- textGrob(label = "CI for the ratios between population IQRs",
                        y = 0.2, just = "bottom", name = "statPlot.writing.1", vp = canvas$graphPath("stat"))
    canvas$image <- addGrob(canvas$image, rectGrob
                            (x = unit(0.5, "npc"), y = unit(2/3, "npc") - unit(1, "lines"),
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

showCIandStatsIQR <- function(canvas, e, ci = TRUE, points = TRUE){
    if (points) vp <- canvas$graphPath("stat") else vp <- canvas$graphPath("sample")
    if (ci) {
        n.steps <- 10
        top.level <- downViewport(canvas$graphPath("animation.field"))
        ani.xscale <- current.viewport()$xscale
        upViewport(top.level)
        top.level <- downViewport(canvas$graphPath("stat"))
        stat.xscale <- current.viewport()$xscale
        upViewport(top.level)
        scale.diff <- ani.xscale[1] - stat.xscale[1]
        xs <- canvas$x
        ys <- canvas$levels
        ylevels <- canvas$ylevels
        # Start with top arrow
        curr.arrows <- fivenum(xs[ys == ylevels[2]])[c(2, 4)]
        curr.arrows <- c(curr.arrows, fivenum(xs[ys == ylevels[1]])[c(2, 4)])
        maxarrow <- max(abs(c(diff(curr.arrows[1:2]), diff(curr.arrows[3:4]))))
        y.start <- c(2.525, 2.125)
        y.end <- c(0.825, 0.675)
        y.step <- (y.start - y.end)/n.steps
        x0.start <- unit(curr.arrows[c(1, 3)], "native")
        x1.start <- unit(curr.arrows[c(2, 4)], "native")
        x0.end <- unit(0.5, "npc") - 0.5*(x1.start - x0.start)
        x.step <- (x0.end - x0.start)*(1/n.steps)
        for (i in 0:n.steps){
            arrows <- segmentsGrob(x0 = x0.start + i*x.step, y0 = y.start - i*y.step,
                                   x1 = x1.start + i*x.step, y1 = y.start - i*y.step,
                                   default.units = "native",
                                   arrow = arrow(length = unit(0.1, "inches")),
                                   name = "samplePlot.temp.arrows", gp = gpar(lwd = 3, col = "red"),
                                   vp = canvas$graphPath("animation.field"))
            canvas$image <- addGrob(canvas$image, arrows)
            if (canvas$stopAnimation)
                return()
            canvas$drawImage()
        }

        canvas$rmGrobs(c("samplePlot.temp.arrows", "dataPlot.ci.1"))
        canvas$image <- addGrob(canvas$image, roundrectGrob
                                (x = unit(0.5, "npc"), y = unit(0.75, "native"),
                                 width = unit(maxarrow, "native") + unit(0.2, "inches"),
                                 height = unit(0.1, "native") + unit(0.4, "inches"),
                                 name = "samplePlot.temp.rect",
                                 gp = gpar(fill = "black"), r = unit(0.2, "snpc"),
                                 vp = canvas$graphPath("animation.field")))
        if (canvas$stopAnimation)
            return()
        canvas$drawImage()

        iqrs <- calcIQR(xs, ys, canvas)
        currstat <- iqrs[2] / iqrs[1]
        if (any(iqrs == 0)) {
            currstat <- NA
            sizemult <- 1
        } else {
            sizemult <- (log10(currstat - stat.xscale[1])) / diff(stat.xscale)
        }
        halfheight <- 0.03 + 0.16*sizemult
        fontsize <- 12 + 64*sizemult
        stattext <- textGrob(label = format(round(currstat, canvas$dp), nsmall = canvas$dp), x = unit(0.5, "npc"),
                             y = unit(0.75, "npc"), gp = gpar(col = "red", fontsize = fontsize),
                             name = "samplePlot.temp.text")
        divchar <- pointsGrob(pch = as.hexmode("00F7"),
                              x = unit(0.5, "npc"), y = unit(0.75, "npc"),
                              gp = gpar(col = "red", cex = fontsize / 10),
                              name = "samplePlot.temp.divchar",
                              vp = canvas$graphPath("stat"))
        canvas$image <- addGrob(canvas$image, arrows) # Adding arrows above the black rect
        canvas$image <- addGrob(canvas$image, divchar)
        if (canvas$stopAnimation)
            return()
        canvas$pauseImage(10)
        canvas$rmGrobs(c("samplePlot.temp.divchar", "samplePlot.temp.text", "samplePlot.temp.arrows"))
        canvas$image <- addGrob(canvas$image, roundrectGrob
                                (x = unit(0.5, "npc"), y = unit(0.75, "native"),
                                 width = unit(halfheight/2, "npc"),
                                 height = unit(halfheight, "npc"), name = "samplePlot.temp.rect",
                                 gp = gpar(fill = "black"), r = unit(0.2, "snpc"),
                                 vp = canvas$graphPath("animation.field")))
        canvas$drawImage()
        canvas$image <- addGrob(canvas$image, roundrectGrob
                                (x = unit(0.5, "npc"), y = unit(0.75, "native"),
                                 width = unit(halfheight, "npc"),
                                 height = unit(halfheight/2, "npc"), name = "samplePlot.temp.rect",
                                 gp = gpar(fill = "black"), r = unit(0.2, "snpc"),
                                 vp = canvas$graphPath("animation.field")))
        canvas$drawImage()
        canvas$image <- addGrob(canvas$image, roundrectGrob
                                (x = unit(0.5, "npc"), y = unit(0.75, "native"),
                                 width = grobWidth(stattext) + unit(5, "mm"),
                                 height = unit(halfheight, "npc"), name = "samplePlot.temp.rect",
                                 gp = gpar(fill = "black"), r = unit(0.2, "snpc"),
                                 vp = canvas$graphPath("animation.field")))
        stattext <- editGrob(stattext, vp = canvas$graphPath("stat"))
        canvas$image <- addGrob(canvas$image, stattext)
        if (canvas$stopAnimation)
            return()
        canvas$pauseImage(10)
        canvas$rmGrobs(c("samplePlot.temp.text", "samplePlot.temp.rect"))

        # CI code.
        valid.xs <- apply(canvas$stat.dist, 1, function(x) all(x != 0))
        x <- log10(canvas$stat.dist[valid.xs, 2] /
                   canvas$stat.dist[valid.xs, 1])
        ci <- quantile(x, prob = c(0.025, 0.975), type = 1)

        #cilabs = format(ci, digits = ciLabelDigits(ci))
        cilabs <- format(round(10^ci, canvas$dp), nsmall = canvas$dp)
        start <- 5

        statline <- linesGrob(x = unit(c(0, log10(currstat)), "native"),
                              y = unit(rep(0.1, 2), "native"),
                              gp = gpar(lwd = 2, col = "red"),
                              arrow = arrow(length = unit(0.1, "inches")),
                              vp = canvas$graphPath("stat"),
                              name = "statPlot.line")
        statarrow <- linesGrob(x = unit(rep(log10(currstat), 2), "native"),
                               y = unit.c(unit(0.10, "npc"), unit(0, "lines")),
                               gp = gpar(col = "red"),
                               arrow = arrow(length = unit(0.1, "inches")),
                               vp = canvas$graphPath("stat"),
                               name = "statPlot.arrow")
        stattext <- textGrob(#format(stat, digits = ciLabelDigits(stat)),
                             format(round(currstat, canvas$dp), nsmall = canvas$dp),
                             x = unit(log10(currstat), "native"), y = 0,
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

        if (points) {
            start <- 1
            y.max <- unit(1, "npc") - unit(2, "lines") - unit(0.5, "char")
            ## Set points outside interval to a lighter shade of grey.
            y <- canvas$stat.ypos
            y <- y[valid.xs]
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
        if (canvas$stopAnimation)
            return()
        canvas$pauseImage(5)
        canvas$rmGrobs("samplePlot.temp.text")
        canvas$showLabels()
        canvas$drawImage()
    } else {
        ## Summary stats code.
        x <- canvas$stat.dist[, 2] / canvas$stat.dist[, 1]
        x <- x[! is.na(x) & ! is.nan(x) & is.finite(x)]
        mean.x <- round(10^(mean(log10(x))), canvas$dp)
        sd.x <- round(10^(sd(log10(x))), canvas$dp)
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

plotIQRRatioDist <- function(canvas, e) {
    canvas$plotted.stats <- c(canvas$plotted.stats, canvas$which.sample)
    stats <- log10(canvas$stat.dist[canvas$plotted.stats, 2] / canvas$stat.dist[canvas$plotted.stats, 1])
    ys <- stackPoints(stats, vp = canvas$graphPath("stat"), y.min = 0, y.max = 0.8)
    plotPoints(canvas, stats, ys, vp = canvas$graphPath("stat"), name = "statPlot")
}

IQRRatio1000 <- function(canvas, e, points) {
    stats <- log10(canvas$stat.dist[, 2] / canvas$stat.dist[, 1])
    for (i in 20*(1:50)){
        canvas$sampled.stats <- 1:i
        canvas$plotSample(e, i, alpha = 0.05)
        if (points){
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
    canvas$rmGrobs(c("samplePlot.databox.text.2", "samplePlot.stat.2",
                     "samplePlot.labeltext.1", "samplePlot.labelrect.1",
                     "samplePlot.temp.data.points."))
    for (i in 1:2){
        canvas$rmGrobs(paste("samplePlot", c("line", "points", "boxplot"), i, sep = "."))
    }
    if (canvas$stopAnimation)
        return()
    canvas$drawImage()
    canvas$sampled.stats <- NULL
    canvas$plotted.stats <- NULL
}

calcIQR <- function(x, y = NULL, canvas = NULL) {
    x <- as.numeric(x)
    y <- as.character(y)
    ylevels <- canvas$ylevels
    stat1 <- diff(fivenum(x[y == ylevels[1]])[c(2, 4)])
    stat2 <- diff(fivenum(x[y != ylevels[1]])[c(2, 4)])

    # Here we calculate the ratios and ensure that we have at least `1`
    # for each level.
    if (stat1 >= stat2 & stat2 != 0) {
        stat1 <- stat1 / stat2
        stat2 <- 1
    } else if (stat2 >= stat1 & stat1 != 0) {
        stat2 <- stat2 / stat1
        stat1 <- 1
    }

    c(stat1, stat2)
}

plotSampleLevelsIQR <- function(canvas, e, i, alpha = 0.25, show.iqr = TRUE) {
    canvas$rmGrobs(c("samplePlot.labeltext.1", "samplePlot.labelrect.1"))
    x <- canvas$samples[[i]]
    levels <- canvas$level.samples[[i]]
    ylevels <- canvas$ylevels
    y <- old.stackPoints(x, levels, vp = canvas$graphPath("sample"))
    # Plotting samples and labels
    for (j in 1:length(ylevels)) {
        plotPoints(canvas, x[levels == ylevels[j]], y[levels == ylevels[j]],
                   col = getColour(j, length(ylevels)),
                   vp = canvas$graphPath("sample", j),
                   name = "samplePlot")
        canvas$image <- addGrob(canvas$image, boxplotGrob
                                (x[levels == ylevels[j]],
                                 stat = NULL,
                                 show.w = FALSE, show.m = FALSE,
                                 gp = gpar(lwd = 2),
                                 name = paste("samplePlot.boxplot", j, sep = "."),
                                 vp = canvas$graphPath("sample", j)))
        canvas$image <- addGrob(canvas$image, linesGrob
                                (x = unit(c(canvas$quartiles[i, 1, ylevels[j]],
                                            canvas$quartiles[i, 3, ylevels[j]]),
                                          "native"),
                                 y = unit(if (j == 1) c(0.25, 0.25) else c(0.05, 0.05), "npc"),
                                 arrow = arrow(length = unit(0.1, "inches")),
                                 gp = gpar(lwd = 3, col = "purple"),
                                 vp = canvas$graphPath("sample", j),
                                 name = paste("samplePlot", "line", j, sep = ".")))
        canvas$image <- addGrob(canvas$image, textGrob
                                (ylevels[j], x = 1, y = unit(1, "mm"),
                                 just = c("right", "bottom"),
                                 vp = canvas$graphPath("sample", j),
                                 name = paste("samplePlot.text", j, sep = ".")))
    }

    if (show.iqr) {
        iqrs <- calcIQR(x, levels, canvas)

        # Handling the case where an IQR ratio is invalid
        if (any(iqrs == 0))
            iqr.labeltext <- "NA"
        else
            iqr.labeltext <- paste(round(iqrs[2], canvas$dp), ":", round(iqrs[1], canvas$dp), collapse = "")

        iqr.labeltext <- c("IQR Ratio:", iqr.labeltext)
        iqr.label <- textGrob(iqr.labeltext,
                              x = unit(0.8, "npc"),
                              y = unit.c(unit(1.4, "native") + unit(1, "lines"),
                                         unit(1.4, "native")),
                              just = c("centre", "bottom"),
                              name = "samplePlot.labeltext.1",
                              vp = canvas$graphPath("animation.field"))
        canvas$image <- addGrob(canvas$image, roundrectGrob
                                (x = unit(0.8, "npc"),
                                 y = unit(1.4, "native") - unit(1, "mm"),
                                 width = max(stringWidth(iqr.labeltext)) + unit(2, "mm"),
                                 height = unit(2, "lines"),
                                 just = c("centre", "bottom"),
                                 gp = gpar(col = "black", fill = "white", alpha = 0.8),
                                 name = "samplePlot.labelrect.1",
                                 vp = canvas$graphPath("animation.field")))
        canvas$image <- addGrob(canvas$image, iqr.label)
    }

    ## Filling sample databox
    cols <- getColour(1:length(ylevels), length(ylevels))
    names(cols) <- ylevels
    cols <- cols[order(ylevels)]
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

linesOnQuartiles <- function(canvas, e) {
    canvas$plotData()
    x <- canvas$x
    levels <- canvas$levels
    ylevels <- canvas$ylevels
    for (i in 1:length(ylevels)) {
        canvas$image <- addGrob(canvas$image, boxplotGrob
                                        (x[levels == ylevels[i]],
                                         stat = NULL, show.m = FALSE,
                                         show.w = FALSE, gp = gpar(lwd = 2),
                                         name = paste("dataPlot.boxplot", i, sep = "."),
                                         vp = canvas$graphPath("data", i)))
        canvas$image <- addGrob(canvas$image, linesGrob
                                (x = unit(fivenum(x[levels == ylevels[i]])[c(2, 4)],
                                          "native"),
                                 y = unit(if (i == 1) c(0.25, 0.25) else c(0.05, 0.05), "npc"),
                                 arrow = arrow(length = unit(0.1, "inches")),
                                 gp = gpar(lwd = 3, col = "purple"),
                                 vp = canvas$graphPath("data", i),
                                 name = paste("dataPlot", "line", i, sep = ".")))
    }
    iqrs <- calcIQR(x, levels, canvas)
    # In the case where an invalid ratio is present just show NA
    if (any(iqrs == 0))
        iqr.labeltext <- "NA"
    else
        iqr.labeltext <- paste(round(iqrs[2], canvas$dp), ":", round(iqrs[1], canvas$dp), collapse = "")
    iqr.labeltext <- c("IQR Ratio:", iqr.labeltext)

    iqr.label <- textGrob(iqr.labeltext,
                          x = unit(0.8, "npc"),
                          y = unit.c(unit(2.4, "native") + unit(1, "lines"),
                                     unit(2.4, "native")),
                          just = c("centre", "bottom"),
                          name = "dataPlot.labeltext.1",
                          vp = canvas$graphPath("animation.field"))
    canvas$image <- addGrob(canvas$image, roundrectGrob
                            (x = unit(0.8, "npc"),
                             y = unit(2.4, "native") - unit(1, "mm"),
                             width = max(stringWidth(iqr.labeltext)) + unit(2, "mm"),
                             height = unit(2, "lines"),
                             just = c("centre", "bottom"),
                             gp = gpar(col = "black", fill = "white", alpha = 0.8),
                             name = "dataPlot.labelrect.1",
                             vp = canvas$graphPath("animation.field")))
    canvas$image <- addGrob(canvas$image, iqr.label)
    canvas$image <- addGrob(canvas$image, linesGrob(x = unit(rep(0, 2), "native"),
                                                    y = unit(c(0, 0.7), "npc"),
                                                    gp = gpar(lty = "dashed", lwd = 3),
                                                    vp = canvas$graphPath("stat"),
                                                    name = "zeroline.1"))
}
