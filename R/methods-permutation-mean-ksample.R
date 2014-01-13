canvasPermMeanKSample <- setRefClass("canvasPermMeanKSampleClass", contains = "canvasPlotClass",
                                     methods = list(
    calcStat = function(i = which.sample, ys = NULL, canvas = .self) {
        if (stat.method == "F-statistic") {
            calcF(samples[[i]], ys, fun = mean)
        } else if (stat.method == "average deviation") {
            calcAveDev(samples[[i]], ys, fun = mean)
        }
    },

    calcAllStats = function(xs, ys = NULL, canvas = .self) {
        if (stat.method == "F-statistic") {
            calcF(xs, ys, fun = mean)
        } else if (stat.method == "average deviation") {
            calcAveDev(xs, ys, fun = mean)
        } 
    },

    plotSample = function(env, i = which.sample) {
        plotKSample(.self, env, i, mean)
    },

    showLabels = function() {
        permPropLabels(.self)
    },

    plotDataStat = function(env, ...) {
        addLinesKSamp(canvas = .self, e = env, fun = mean)
    },

    plotStatDist = function(env, ...) {
        plotPermStat(.self, env)
    },

    animateSample = function(...) {
        animateKSample(.self, ...)
    },

    animateStat = function(env, n.steps) {
        animateFStat(.self, env, n.steps, fun = mean,
                     fstat = env$perm.method == "F-statistic")
    },

    displayResult = function(env, ...) {
        showTailKSample(.self, env, fun = mean,
                        fstat = env$perm.method == "F-statistic")
    },
    ## perm1000 taken from methods-permutation-mean.R
    handle1000 = function(env, ...) {
        perm1000(.self, env, ...)
    }))

load_permutation_mean_ksample <- function(e) {
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
    tmp.canvas <- canvasPermMeanKSample$new()
    tmp.canvas$import(e$c1)
    e$c1 <- tmp.canvas
    e$c1$viewports <- tmp.vps
    e$c1$image <- tmp.image
    e$extra <- 0
}

plotKSample <- function(canvas, e, i, fun = mean){
    x <- canvas$samples[[i]]
    expx <- fun(x)
    levels <- canvas$levels
    ylevels <- canvas$ylevels
    ngroups <- length(ylevels)
    y <- old.stackPoints(x, levels, vp = canvas$graphPath("sample"))
    cols <- getColour(1:ngroups, ngroups)
    n <- 1
    sstats <- numeric(ngroups)
    ## Plotting samples, labels, mean lines and arrows.
    for (j in ylevels) {
        if (identical(fun, median) & ngroups == 2){
            plotBoxplot(canvas, x[levels == j],
                        stat = median, stat.color = "blue",
                        vp = canvas$graphPath("sample", as.character(n)),
                        name = "samplePlot")
        }
        plotPoints(canvas, x[levels == j],
                   y[levels == j], col = cols[n],
                   vp = canvas$graphPath("sample", as.character(n)),
                   name = "samplePlot")
        canvas$image <- addGrob(canvas$image, linesGrob
                                (x = unit(rep(fun(x[levels == j]), 2),
                                 "native"),
                                 y = unit(c(0.1, 0.55), "npc"),
                                 gp = gpar(lwd = 3, col = "blue"),
                                 vp = canvas$graphPath("sample", as.character(n)),
                                 name = paste("samplePlot", "line", n, sep = ".")))
        canvas$image <- addGrob(canvas$image, textGrob
                                (j, x = 1, y = unit(1, "mm"), just = c("right", "bottom"),
                                 vp = canvas$graphPath("sample", as.character(n)),
                                 name = paste("samplePlot.text", n, sep = ".")))
        sstats[n] <- fun(x[levels == j])
        if (ngroups > 2){
            canvas$image <- addGrob(canvas$image, linesGrob
                                    (x = unit(c(expx, sstats[n]), "native"),
                                     y = unit(rep(0.3, 2), "npc"),
                                     gp = gpar(lwd = 3, col = "red"),
                                     arrow = arrow(length = unit(0.1, "inches")),
                                     vp = canvas$graphPath("sample", as.character(n)),
                                     name = paste("samplePlot", "arrow", n, sep = ".")))
        } else {
            y.start <- old.stackPoints(canvas$x, vp = canvas$graphPath("sample", 1),
                           y.min = 0.8, y.max = 1)
            y.start <- y.start[canvas$indexes[[i]]]
            y.pos <- y.start[levels == j]
            plotPoints(canvas, x[levels == j],
                       y.pos, alpha = 0.25,
                       vp = canvas$graphPath("sample", 1),
                       name = paste("samplePlot.join", n, sep = ""),
                       col = getColour(n, 2))
        }
        n <- n + 1
    }
    if (ngroups == 2){
        canvas$image <- addGrob(canvas$image, linesGrob(x = unit(rev(sstats), "native"),
                                                        y = unit(0.8, "npc"),
                                                        gp = gpar(lwd = 2, col = "red"),
                                                        arrow = arrow(length = unit(0.1, "inches")),
                                                        vp = canvas$graphPath("sample", 1),
                                                        name = "samplePlot.arrow.1"))
    }
    ## Filling sample databox
    names(cols) <- ylevels
    cols <- cols[sort(ylevels)]
    text <- as.character(levels[order(canvas$indexes[[i]])])
    canvas$image <- addGrob(canvas$image, coldatatextGrob
                            (data = text,
                             title = if (e$method == "permvar") "Random groups" else "Re-randomised",
                             cols = cols,
                             xpos = 0.5, max = 50,
                             name = "samplePlot.databox.text.2", vp = canvas$graphPath("databox", 2)))
    if (e$method == "sampvar"){
        if (!e$fade){
            canvas$image <- addGrob(canvas$image, getGrob(canvas$image, gPath("fadebox")))
            canvas$image <- addGrob(canvas$image, getGrob(canvas$image, gPath("sampvarlabels")))
            canvas$image <- addGrob(canvas$image, getGrob(canvas$image, gPath("dataAxis")))
        }
    }
}

addLinesKSamp <- function(canvas, e, fun = mean){
    x <- canvas$x
    expx <- fun(x)
    levels <- canvas$levels
    ylevels <- canvas$ylevels
    y <- old.stackPoints(x, levels, vp = canvas$graphPath("data"))
    n <- 1
    cols <- getColour(1:length(ylevels), length(ylevels))
    ## Plotting grand mean line.
    canvas$image <- addGrob(canvas$image,
                            segmentsGrob(x0 = expx, x1 = expx, y0 = 1,
                                         y1 = 3, default.units = "native",
                                         gp = gpar(col = "grey60", lty = "dashed"),
                                         vp = canvas$graphPath("animation.field"),
                                         name = "hline"))
    ## Plotting coloured data and individual sample mean lines.
    for (i in ylevels) {
        canvas$image <- addGrob(canvas$image, linesGrob
                                (x = unit(rep(fun(x[levels == i]), 2),
                                 "native"),
                                 y = unit(c(0.1, 0.55), "npc"),
                                 gp = gpar(lwd = 3, col = "purple3"),
                                 vp = canvas$graphPath("data", as.character(n)),
                                 name = paste("dataPlot", "line", n, sep = ".")))
        canvas$image <- addGrob(canvas$image, linesGrob
                                (x = unit(c(expx, fun(x[levels == i])), "native"),
                                 y = unit(rep(0.3, 2), "npc"),
                                 gp = gpar(lwd = 3, col = "red"),
                                 arrow = arrow(length = unit(0.1, "inches")),
                                 vp = canvas$graphPath("data", as.character(n)),
                                 name = paste("dataPlot", "arrow", n, sep = ".")))
        n <- n + 1
    }

}


calcF <- function(x, y, fun = mean){
    uy <- unique(y)
    gstat <- fun(x)
    sstats <- ns <- numeric(length(uy))
    j <- 1
    SSW <- 0
    for (i in uy){
        samp <- x[y == i]
        sstats[j] <- fun(samp)
        ns[j] <- length(samp)
        SSW <- SSW + sum((samp - sstats[j])^2)
        j <- j + 1
    }
    SSB <- sum(ns*((sstats - gstat)^2))
    dfB <- length(uy) - 1
    dfW <- length(x) - length(uy)
    (SSB/dfB)/(SSW/dfW)
}

animateKSample <- function(canvas, e, n.steps, mix = TRUE){
    e$clearPanel("sample")
    canvas$rmGrobs("samplePlot.databox.text.2")
    ## Drop samples down to middle plot
    x <- canvas$x
    levels <- canvas$levels
    ylevels <- canvas$ylevels
    nsamps <- length(ylevels)
    cols <- getColour(1:length(ylevels), length(ylevels))
    y <- old.stackPoints(canvas$x, canvas$levels, vp = canvas$graphPath("data"), y.min = 0.3,
                         y.max = 1)
    y.start <- y
    y.end <- y - nsamps
    y.step <- (y.start - y.end)/n.steps
    if (mix){
        ## Dropping samples
        for (i in 0:n.steps){
            plotPointGroups(canvas, x, y.start - i*y.step, levels, "data",
                            cols = cols, "samplePlot.temp")
            if (canvas$stopAnimation)
                return()
            canvas$drawImage()
        }
        if (canvas$stopAnimation)
            return()
        canvas$pauseImage(10)
        canvas$rmGrobs(paste("samplePlot.temp.points", 1:nsamps, sep = "."))

        ## Mixing samples
        y.end <- old.stackPoints(x, vp = canvas$graphPath("sample"), y.min = 0, y.max = 0.1*nsamps)
        y.end.samps <- list()
        y.start.samps <- list()
        y.step.samps <- list()
        for (i in 1:nsamps){
            y.end.samps[[i]] <- y.end[levels == ylevels[i]] + nsamps/2 - (i - 1)
            y.start.samps[[i]] <- y.start[levels == ylevels[i]]
            y.step.samps[[i]] <- (y.start.samps[[i]] - y.end.samps[[i]])/n.steps
        }
        for (i in 1:n.steps){
            for (j in 1:nsamps){
                plotPoints(canvas, x[levels == ylevels[j]],
                           y.start.samps[[j]] - i*y.step.samps[[j]],
                           vp = canvas$graphPath("sample", j), name = "samplePlot.tempjoin",
                           col = cols[j])
            }
            if (canvas$stopAnimation)
                return()
            canvas$drawImage()
        }
        canvas$rmGrobs(paste("samplePlot.tempjoin.points", 1:nsamps, sep = "."))
    }
    ## Separating samples - all relative to BOTTOM viewport.
    x.sample <- canvas$samples[[canvas$which.sample]]
    y.start <- old.stackPoints(x, vp = canvas$graphPath("sample", 1),
                               y.min = nsamps/2, y.max = nsamps/2 + 0.1*nsamps)
    y.start <- y.start[canvas$indexes[[canvas$which.sample]]]
    ## Turning points black.
    plotPoints(canvas, x.sample, y.start, vp = canvas$graphPath("sample", 1),
               name = "samplePlot.join", col = "black")
    if (canvas$stopAnimation)
        return()
    canvas$pauseImage(10)
    canvas$rmGrobs("samplePlot.join.points.1")
    ## Adding text to data boxes.
    text <-  as.character(levels[order(canvas$indexes[[canvas$which.sample]])])
    names(cols) <- ylevels
    dtcols <- cols[sort(ylevels)]
    canvas$image <- addGrob(canvas$image, coldatatextGrob
                            (data = text,
                             title = if (e$method == "permvar") "Random groups" else "Re-randomised",
                             cols = dtcols,
                             xpos = 0.5, max = 50,
                             name = "samplePlot.databox.text.2",
                             vp = canvas$graphPath("databox", 2)))
    y.sample <- old.stackPoints(x.sample, levels, vp = canvas$graphPath("sample"))
    y.end.samps <- list()
    y.start.samps <- list()
    y.step.samps <- list()
    for (i in 1:nsamps){
        y.start.samps[[i]] <- y.start[levels == ylevels[i]]
        y.end.samps[[i]] <- y.sample[levels == ylevels[i]] + (i - 1)
        y.step.samps[[i]] <- (y.start.samps[[i]] - y.end.samps[[i]])/n.steps
    }
    for (j in 1:nsamps){
        plotPoints(canvas, x.sample[levels == ylevels[j]],
                   y.start.samps[[j]],
                   vp = canvas$graphPath("sample", 1),
                   name = paste("samplePlot.tempjoin", j, sep = ""),
                   col = cols[j])
    }
    canvas$pauseImage(5)
    for (i in 1:n.steps){
        for (j in 1:nsamps){
            plotPoints(canvas, x.sample[levels == ylevels[j]],
                       y.start.samps[[j]] - i*y.step.samps[[j]],
                       vp = canvas$graphPath("sample", 1),
                       name = paste("samplePlot.tempjoin", j, sep = ""),
                       col = cols[j])
        }
        if (canvas$stopAnimation)
            return()
        canvas$drawImage()
    }
    rmgrobs <- paste(c(rep("samplePlot.tempjoin", each = nsamps), rep("samplePlot.join", each = nsamps)),
                     rep(1:nsamps, 2), sep = "")
    canvas$rmGrobs(paste(rmgrobs, "points.1", sep = "."))
}

plotPermStat <- function(canvas, e){
    canvas$plotted.stats <- c(canvas$plotted.stats, canvas$which.sample)
    stats <- c(canvas$stat.dist[canvas$plotted.stats], recursive = TRUE)
    y.max <- unit(1, "npc") - unit(2, "lines") - unit(0.5, "char")
    y <- stackPoints(stats, vp = canvas$graphPath("stat"), y.min = 0, y.max = y.max)
    plotPoints(canvas, stats, y, vp = canvas$graphPath("stat"), name = "statPlot")
}

showTailKSample <- function(canvas, e, fun = mean, fstat = TRUE){
    n.steps <- 10
    x <- canvas$x
    expx <- if (is.categorical(x)) length(x[x == canvas$loi]) / length(x)
            else fun(x)
    levels <- canvas$levels
    ylevels <- canvas$ylevels
    ngroups <- length(ylevels)
    stats <- c(canvas$stat.dist, recursive = TRUE)
    y.max <- unit(1, "npc") - unit(2, "lines") - unit(0.5, "char")
    y <- canvas$stat.ypos
    plotPoints(canvas, stats, y, vp = canvas$graphPath("stat"), name = "statPlot",
               alpha = 0.7)
    canvas$showLabels()
    groupstat <- numeric(ngroups)
    n <- 1
    for (i in ylevels) {
        if (is.categorical(x)) {
            levelx <- x[levels == i]
            groupstat[n] <- length(levelx[levelx == canvas$loi]) / length(levelx)
        } else {
            groupstat[n] <- fun(x[levels == i])
        }
        n <- n + 1
    }
    maxarrow <- max(abs(groupstat - expx))
    avearrow <- mean(abs(groupstat - expx))
    canvas$image <- addGrob(canvas$image, textGrob
                            (label = paste("Calculating ", e$perm.method, "...", sep = ""),
                             x = unit(0.5, "npc"), y = unit(0.6, "native") - unit(0.15, "inches"),
                             just = "top", gp = gpar(fontface = 2), name = "statPlot.calc.message",
                             vp = canvas$graphPath("animation.field")))
    sampstat <- canvas$calcAllStats(canvas$x, canvas$levels)
    y.start <- seq(from = 2 + 0.3/ngroups, by = 1/ngroups, length.out = ngroups)
    y.end <- rep(0.6, ngroups) + seq(-0.05, 0.05, length.out = ngroups)
    y.step <- (y.start - y.end)/n.steps
    x0.start <- unit(rep(expx, ngroups), "native")
    x1.start <- unit(groupstat, "native")
    x0.end <- unit(0.5, "npc") - 0.5*(x1.start - x0.start)
    x.step <- (x0.end - x0.start)*(1/n.steps)
    for (i in 0:n.steps){
        arrows <- segmentsGrob(x0 = x0.start + i*x.step, y0 = y.start - i*y.step,
                               x1 = x1.start + i*x.step, y1 = y.start - i*y.step,
                               default.units = "native",
                               arrow = arrow(length = unit(0.1, "inches")),
                               name = "statPlot.temp.arrows", gp = gpar(lwd = 3, col = "red"),
                               vp = canvas$graphPath("animation.field"))
        canvas$image <- addGrob(canvas$image, arrows)
        if (canvas$stopAnimation)
            return()
        canvas$drawImage()
    }
    if (canvas$stopAnimation)
        return()
    canvas$pauseImage(4)
    canvas$rmGrobs("statPlot.temp.arrows")
    canvas$image <- addGrob(canvas$image, roundrectGrob
                            (x = unit(0.5, "npc"), y = unit(0.6, "native"),
                             width = unit(maxarrow, "native"),
                             height = unit(0.2, "inches"), name = "statPlot.temp.rect",
                             gp = gpar(fill = "black"), r = unit(0.2, "snpc"),
                             vp = canvas$graphPath("animation.field")))
    if (canvas$stopAnimation)
        return()
    canvas$drawImage()
    canvas$rmGrobs("statPlot.calc.message")
    if (!fstat){
        arrwidth <- unit(avearrow, "native") - unit(0, "native")
        xs.start <- unit(0.5, "npc") + c(-0.5, 0.5)*arrwidth
        xs.end <- unit(c(0, avearrow), "native")
        xs.step <- (xs.end - xs.start)*(1/n.steps)
        y.start <- 0.6
        y.end <- 0
        y.step <- (y.end - y.start)/n.steps
        canvas$image <- addGrob(canvas$image, linesGrob
                                (x = xs.start, y = unit(y.start, "npc"),
                                 gp = gpar(lwd = 3, col = "red"),
                                 arrow = arrow(length = unit(0.1, "inches")),
                                 name = "statPlot.temp.arrows", vp = canvas$graphPath("stat")))
        if (canvas$stopAnimation)
            return()
        canvas$pauseImage(5)
        canvas$rmGrobs("statPlot.temp.rect")
        for (i in 1:n.steps){
            canvas$image <- addGrob(canvas$image, linesGrob
                                (x = xs.start + i*xs.step, y = unit(y.start + i*y.step, "npc"),
                                 gp = gpar(lwd = 3, col = "red"),
                                 arrow = arrow(length = unit(0.1, "inches")),
                                 name = "statPlot.temp.arrows", vp = canvas$graphPath("stat")))
            if (canvas$stopAnimation)
                return()
            canvas$drawImage()
        }
        if (canvas$stopAnimation)
            return()
        canvas$pauseImage(4)
        canvas$rmGrobs("statPlot.temp.arrows")
    } else {
        stats <- c(canvas$stat.dist, recursive = TRUE)
        minstat <- min(stats)
        maxstat <- max(stats)
        currstat <- canvas$calcAllStats(x, levels)
        sizemult <- min(1, (currstat - minstat)/(maxstat - minstat))
        halfheight <- 0.03 + 0.16*sizemult
        fontsize <- 12 + 64*sizemult
        if (is.categorical(canvas$x) & !fstat) dps <- 3 else dps <- canvas$dp
        stattext1 <- textGrob(label = format(round(currstat, dps), nsmall = dps),
                             x = unit(0.5, "npc"),
                             y = unit(0.6, "npc"), gp = gpar(col = "red", fontsize = fontsize),
                             name = "statPlot.temp.text")
        canvas$image <- addGrob(canvas$image, roundrectGrob
                                (x = unit(0.5, "npc"), y = unit(0.6, "native"),
                                 width = unit(halfheight/2, "npc"),
                                 height = unit(halfheight, "npc"), name = "statPlot.temp.rect",
                                 gp = gpar(fill = "black"), r = unit(0.2, "snpc"),
                                 vp = canvas$graphPath("animation.field")))
        canvas$drawImage()
        canvas$image <- addGrob(canvas$image, roundrectGrob
                                (x = unit(0.5, "npc"), y = unit(0.6, "native"),
                                 width = unit(halfheight, "npc"),
                                 height = unit(halfheight/2, "npc"), name = "statPlot.temp.rect",
                                 gp = gpar(fill = "black"), r = unit(0.2, "snpc"),
                                 vp = canvas$graphPath("animation.field")))
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
        pos <- convertX(unit(currstat, "native") - unit(0, "native") + 0.5*text.width,
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
        y.end <- 0.1
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
        if (canvas$stopAnimation)
            return()
        canvas$pauseImage(4)
    }
    y.max <- unit(1, "npc") - unit(2, "lines") - unit(0.5, "char")
    y <- stackPoints(stats, vp = canvas$graphPath("stat"), y.min = 0,
                         y.max = y.max)
    x.out <- stats[stats >= sampstat]
    x.in <- stats[stats < sampstat]
    y.out <- y[stats >= sampstat]
    y.in <- y[stats < sampstat]
    tot <- sum(stats >= sampstat)
    p <- mean(stats >= sampstat)
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
    arrow <- linesGrob(x = unit(c(0, sampstat), "native"),
                       y = unit(0, "native"),
                       gp = gpar(lwd = 2, col = "red"),
                       arrow = arrow(length = unit(0.1, "inches")),
                       name = "statPlot.arrow.1")
    canvas$showLabels()
    top.level <- downViewport(canvas$graphPath("stat"))
    text1 <- paste(tot, "/ 1000", sep = " ")
    text2 <- paste("= ",p)
    text.width <- max(stringWidth(text1), stringWidth(text2))
    arrow.width <- grobWidth(arrow)
    pos <- convertX(arrow.width + unit(5, "mm") + text.width, "npc", valueOnly = TRUE)
    if (pos > 1){
        xpos <- unit(1, "npc")
        hjust <- 1
    } else {
        xpos <- unit(rep(sampstat, 2), "native") + unit(5, "mm")
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
                            (x = unit(rep(sampstat, 2), "native"),
                             y = unit(c(0, 0.5), "npc") - unit(1, "mm"),
                             vp = canvas$graphPath("stat"), name = "statPlot.propline"))
    canvas$rmGrobs("statPlot.points.1")
    if (fstat){
        stattext <- getGrob(canvas$image, gPath("statPlot.temp.text"))
        canvas$image <- addGrob(canvas$image, stattext)
        if (canvas$stopAnimation)
            return()
        canvas$pauseImage(5)
        canvas$rmGrobs("statPlot.temp.text")
        canvas$image <- addGrob(canvas$image, pointsGrob
                                (x = unit(sampstat, "native"), y = unit(0, "npc"),
                                 pch = 19, gp = gpar(col = "red"), vp = canvas$graphPath("stat"),
                                 name = "statPlot.statpoint.1"))
    } else {
        canvas$image <- addGrob(canvas$image, arrow)
    }
    canvas$showLabels()
    canvas$drawImage()
    canvas$rmGrobs(c("statPlot.lightpoints", "statPlot.darkpoints", "statPlot.arrow.1",
                     "statPlot.proprect", "statPlot.proptext", "statPlot.propline",
                     "statPlot.statpoint.1"))
}

animateFStat <- function(canvas, e, n.steps, fun = mean, fstat = TRUE){
    x <- canvas$samples[[canvas$which.sample]]
    expx <- fun(x)
    levels <- canvas$levels
    ylevels <- canvas$ylevels
    ngroups <- length(ylevels)
    groupstat <- numeric(ngroups)
    n <- 1
    for (i in ylevels) {
        groupstat[n] <- fun(x[levels == i])
        n <- n + 1
    }
    maxarrow <- max(abs(groupstat - expx))
    avearrow <- mean(abs(groupstat - expx))
    canvas$image <- addGrob(canvas$image, textGrob
                            (label = paste("Calculating ", e$perm.method, "...", sep = ""),
                             x = unit(0.5, "npc"), y = unit(0.6, "native") - unit(0.15, "inches"),
                             just = "top", gp = gpar(fontface = 2), name = "statPlot.calc.message",
                             vp = canvas$graphPath("animation.field")))
    y.start <- seq(from = 1 + 0.3/ngroups, by = 1/ngroups, length.out = ngroups)
    y.end <- rep(0.6, ngroups) + seq(-0.05, 0.05, length.out = ngroups)
    y.step <- (y.start - y.end)/n.steps
    x0.start <- unit(rep(expx, ngroups), "native")
    x1.start <- unit(groupstat, "native")
    x0.end <- unit(0.5, "npc") - 0.5*(x1.start - x0.start)
    x.step <- (x0.end - x0.start)*(1/n.steps)
    for (i in 0:n.steps){
        arrows <- segmentsGrob(x0 = x0.start + i*x.step, y0 = y.start - i*y.step,
                               x1 = x1.start + i*x.step, y1 = y.start - i*y.step,
                               default.units = "native",
                               arrow = arrow(length = unit(0.1, "inches")),
                               name = "statPlot.temp.arrows", gp = gpar(lwd = 3, col = "red"),
                               vp = canvas$graphPath("animation.field"))
        canvas$image <- addGrob(canvas$image, arrows)
        if (canvas$stopAnimation)
            return()
        canvas$drawImage()
    }
    if (canvas$stopAnimation)
        return()
    canvas$pauseImage(4)
    canvas$rmGrobs("statPlot.temp.arrows")
    canvas$image <- addGrob(canvas$image, roundrectGrob
                            (x = unit(0.5, "npc"), y = unit(0.6, "native"),
                             width = unit(maxarrow, "native"),
                             height = unit(0.1, "native") + unit(0.2, "inches"),
                             name = "statPlot.temp.rect",
                             gp = gpar(fill = "black"), r = unit(0.2, "snpc"),
                             vp = canvas$graphPath("animation.field")))
    if (canvas$stopAnimation)
        return()
    canvas$drawImage()
    canvas$rmGrobs("statPlot.calc.message")
    if (!fstat){
        arrwidth <- unit(avearrow, "native") - unit(0, "native")
        xs.start <- unit(0.5, "npc") + c(-0.5, 0.5)*arrwidth
        xs.end <- unit(c(0, avearrow), "native")
        xs.step <- (xs.end - xs.start)*(1/n.steps)
        y.start <- 0.6
        y.end <- 0
        y.step <- (y.end - y.start)/n.steps
        canvas$image <- addGrob(canvas$image, linesGrob
                                (x = xs.start, y = unit(y.start, "npc"),
                                 gp = gpar(lwd = 3, col = "red"),
                                 arrow = arrow(length = unit(0.1, "inches")),
                                 name = "statPlot.temp.arrows", vp = canvas$graphPath("stat")))
        if (canvas$stopAnimation)
            return()
        canvas$pauseImage(5)
        canvas$rmGrobs("statPlot.temp.rect")
        for (i in 1:n.steps){
            canvas$image <- addGrob(canvas$image, linesGrob
                                (x = xs.start + i*xs.step, y = unit(y.start + i*y.step, "npc"),
                                 gp = gpar(lwd = 3, col = "red"),
                                 arrow = arrow(length = unit(0.1, "inches")),
                                 name = "statPlot.temp.arrows", vp = canvas$graphPath("stat")))
            if (canvas$stopAnimation)
                return()
            canvas$drawImage()
        }
        if (canvas$stopAnimation)
            return()
        canvas$pauseImage(4)
        canvas$rmGrobs("statPlot.temp.arrows")
    } else {
        stats <- c(canvas$stat.dist, recursive = TRUE)
        minstat <- min(stats)
        maxstat <- max(stats)
        currstat <- stats[canvas$which.sample]
        sizemult <- (currstat - minstat)/(maxstat - minstat)
        halfheight <- 0.03 + 0.16*sizemult
        fontsize <- 12 + 64*sizemult
        stattext <- textGrob(label = format(round(currstat, canvas$dp), nsmall = canvas$dp), x = unit(0.5, "npc"),
                             y = unit(0.6, "npc"), gp = gpar(col = "red", fontsize = fontsize),
                             name = "statPlot.temp.text")
        canvas$image <- addGrob(canvas$image, roundrectGrob
                                (x = unit(0.5, "npc"), y = unit(0.6, "native"),
                                 width = unit(halfheight/2, "npc"),
                                 height = unit(halfheight, "npc"), name = "statPlot.temp.rect",
                                 gp = gpar(fill = "black"), r = unit(0.2, "snpc"),
                                 vp = canvas$graphPath("animation.field")))
        canvas$drawImage()
        canvas$image <- addGrob(canvas$image, roundrectGrob
                                (x = unit(0.5, "npc"), y = unit(0.6, "native"),
                                 width = unit(halfheight, "npc"),
                                 height = unit(halfheight/2, "npc"), name = "statPlot.temp.rect",
                                 gp = gpar(fill = "black"), r = unit(0.2, "snpc"),
                                 vp = canvas$graphPath("animation.field")))
        canvas$drawImage()
        canvas$image <- addGrob(canvas$image, roundrectGrob
                                (x = unit(0.5, "npc"), y = unit(0.6, "native"),
                                 width = grobWidth(stattext) + unit(5, "mm"),
                                 height = unit(halfheight, "npc"), name = "statPlot.temp.rect",
                                 gp = gpar(fill = "black"), r = unit(0.2, "snpc"),
                                 vp = canvas$graphPath("animation.field")))
        stattext <- editGrob(stattext, vp = canvas$graphPath("stat"))
        canvas$image <- addGrob(canvas$image, stattext)
        if (canvas$stopAnimation)
            return()
        canvas$pauseImage(5)
        canvas$rmGrobs("statPlot.temp.rect")
        n.steps <- round(n.steps/2)
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
                                     vjust = 0.5*(1 - i/n.steps))
            if (canvas$stopAnimation)
                return()
            canvas$drawImage()
        }
        if (canvas$stopAnimation)
            return()
        canvas$pauseImage(4)
        canvas$rmGrobs("statPlot.temp.text")
    }
}
