canvasPermPropKSample <- setRefClass("canvasPermPropKSampleClass", contains = "canvasPlotClass",
                                     methods = list(
    calcStat = function(i = which.sample, ys = NULL, canvas = .self) {
         if (stat.method == "average deviation") {
             calcAveDevProp(samples[[i]], ys, canvas)
         } else if (stat.method == "chi-square statistic") {
             calcChiSq(samples[[i]], ys, canvas)
         }
    },

    calcAllStats = function(xs, ys = NULL, canvas = .self) {
         if (stat.method == "average deviation") {
             calcAveDevProp(xs, ys, canvas)
         } else if (stat.method == "chi-square statistic") {
             calcChiSq(xs, ys, canvas)
         }
    },

    displayResult = function(env, ...) {
         if (stat.method == "average deviation") {
             showTailKSample(.self, env, fstat = FALSE, ...)
         } else if (stat.method == "chi-square statistic") {
             showTailKSample(.self, env, fstat = TRUE, ...)
         }
    },

    plotSample = function(env, i = which.sample) {
        plotPermKProportions(.self, env, i)
    },

    showLabels = function() {
        permPropLabels(.self)
    },

    plotDataStat = function(env, ...) {
        addPropLinesKSamp(canvas = .self, e = env)
    },

    plotStatDist = function(env, ...) {
        plotPermStat(.self, env)
    },

    animateSample = function(...) {
        animateKSampleProp(.self, ...)
    },

    animateStat = function(env, n.steps) {
        animatePropStat(.self, env, n.steps, env$perm.method)
    },

    handle1000 = function(env, ...) {
        perm1000(.self, env, ...)
    }))

load_permutation_proportion_ksample <- function(e) {
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
    tmp.canvas <- canvasPermPropKSample$new()
    tmp.canvas$import(e$c1)
    e$c1 <- tmp.canvas
    e$c1$viewports <- tmp.vps
    e$c1$image <- tmp.image
    e$extra <- 0
}

animatePropStat <- function(canvas, e, n.steps, method = "average deviation") {
    x <- canvas$samples[[canvas$which.sample]]
    levels <- canvas$levels
    ylevels <- canvas$ylevels
    ngroups <- length(ylevels)
    groupstat <- numeric(ngroups)
    expx <- calcProportion(x, levels, canvas)
    groupstat <- calcPropDiff(x, levels, canvas)
    maxarrow <- max(abs(groupstat - expx))
    avearrow <- mean(abs(groupstat - expx))
    canvas$image <- addGrob(canvas$image, textGrob
                            (label = paste("Calculating ", e$perm.method, "...", sep = ""),
                             x = unit(0.5, "npc"), y = unit(0.6, "native") - unit(0.15, "inches"),
                             just = "top", gp = gpar(fontface = 2), name = "samplePlot.calc.message",
                             vp = canvas$graphPath("animation.field")))
    y.start <- seq(from = 1 + 0.35/ngroups, by = 1/ngroups, length.out = ngroups)
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
                               name = "samplePlot.temp.arrows", gp = gpar(lwd = 3, col = "red"),
                               vp = canvas$graphPath("animation.field"))
        canvas$image <- addGrob(canvas$image, arrows)
        if (canvas$stopAnimation)
            return()
        canvas$drawImage()
    }
    if (canvas$stopAnimation)
        return()
    canvas$pauseImage(4)
    canvas$rmGrobs("samplePlot.temp.arrows")
    canvas$image <- addGrob(canvas$image, roundrectGrob
                            (x = unit(0.5, "npc"), y = unit(0.6, "native"),
                             width = unit(maxarrow, "native"),
                             height = unit(0.1, "native") + unit(0.2, "inches"),
                             name = "samplePlot.temp.rect",
                             gp = gpar(fill = "black"), r = unit(0.2, "snpc"),
                             vp = canvas$graphPath("animation.field")))
    if (canvas$stopAnimation)
        return()
    canvas$drawImage()
    canvas$rmGrobs("samplePlot.calc.message")
    if (method == "average deviation") {
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
                                 name = "samplePlot.temp.arrows", vp = canvas$graphPath("stat")))
        if (canvas$stopAnimation)
            return()
        canvas$pauseImage(5)
        canvas$rmGrobs("samplePlot.temp.rect")
        for (i in 1:n.steps){
            canvas$image <- addGrob(canvas$image, linesGrob
                                (x = xs.start + i*xs.step, y = unit(y.start + i*y.step, "npc"),
                                 gp = gpar(lwd = 3, col = "red"),
                                 arrow = arrow(length = unit(0.1, "inches")),
                                 name = "samplePlot.temp.arrows", vp = canvas$graphPath("stat")))
            if (canvas$stopAnimation)
                return()
            canvas$drawImage()
        }
        if (canvas$stopAnimation)
            return()
        canvas$pauseImage(4)
        canvas$rmGrobs("samplePlot.temp.arrows")
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
                             name = "samplePlot.temp.text")
        canvas$image <- addGrob(canvas$image, roundrectGrob
                                (x = unit(0.5, "npc"), y = unit(0.6, "native"),
                                 width = unit(halfheight/2, "npc"),
                                 height = unit(halfheight, "npc"), name = "samplePlot.temp.rect",
                                 gp = gpar(fill = "black"), r = unit(0.2, "snpc"),
                                 vp = canvas$graphPath("animation.field")))
        canvas$drawImage()
        canvas$image <- addGrob(canvas$image, roundrectGrob
                                (x = unit(0.5, "npc"), y = unit(0.6, "native"),
                                 width = unit(halfheight, "npc"),
                                 height = unit(halfheight/2, "npc"), name = "samplePlot.temp.rect",
                                 gp = gpar(fill = "black"), r = unit(0.2, "snpc"),
                                 vp = canvas$graphPath("animation.field")))
        canvas$drawImage()
        canvas$image <- addGrob(canvas$image, roundrectGrob
                                (x = unit(0.5, "npc"), y = unit(0.6, "native"),
                                 width = grobWidth(stattext) + unit(5, "mm"),
                                 height = unit(halfheight, "npc"), name = "samplePlot.temp.rect",
                                 gp = gpar(fill = "black"), r = unit(0.2, "snpc"),
                                 vp = canvas$graphPath("animation.field")))
        stattext <- editGrob(stattext, vp = canvas$graphPath("stat"))
        canvas$image <- addGrob(canvas$image, stattext)
        if (canvas$stopAnimation)
            return()
        canvas$pauseImage(5)
        canvas$rmGrobs("samplePlot.temp.rect")
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
    }
}

animateKSampleProp <- function(canvas, e, n.steps, mix = TRUE) {
    e$clearPanel("sample")
    ## Drop samples down to middle plot
    x <- canvas$x
    levels <- canvas$levels
    ylevels <- canvas$ylevels
    depth <- downViewport("animation.field")
    max.str.width <- stringWidth(ylevels)
    max.str.width <- convertWidth(max(max.str.width), "npc", valueOnly = TRUE)
    upViewport(depth)
    ngroups <- length(ylevels)
    y.start <- seq(2, 2 + (ngroups - 1) / ngroups, length.out = ngroups)
    y.start <- y.start + (0.1 / ngroups)
    y.end <- y.start - 1
    y.step <- (y.start - y.end) / n.steps

    # Removing any existing grobs in the sample area
    canvas$rmGrobs(c("samplePlot.tempTotalPropbar",
                     paste("samplePlot.tempPropbar", 1:ngroups, sep = "."),
                     paste("samplePlot.propbar", 1:ngroups, sep = ".")))

    if (mix) {
        # Dropping samples
        for (i in 0:n.steps) {
            for (j in 1:ngroups) {
                canvas$image <- addGrob(canvas$image, kpropbarGrob
                                (x[levels == ylevels[j]],
                                 ylab = ylevels[j],
                                 ylab.pos = 1 - max.str.width,
                                 ylab.col = getColour(j, ngroups),
                                 draw.points = FALSE,
                                 lois = c(canvas$loi, canvas$loi.alt),
                                 y = y.start[j] - i*y.step[j],
                                 statLineHeight = 0.1 / ngroups,
                                 height = 0.6 / ngroups, vpcex = 1 / ngroups,
                                 name = paste("samplePlot.tempPropbar", j, sep="."),
                                 vp = canvas$graphPath("animation.field")))
            }
            canvas$showLabels()
            if (canvas$stopAnimation)
                return()
            canvas$drawImage()
        }

        # Merging samples
        y.start <- seq(1, 1 + (ngroups - 1) / ngroups, length.out = ngroups)
        y.start <- y.start + (0.1 / ngroups)
        y.end <- 1 + (1 - 0.6 / ngroups) / 2
        y.step <- (y.start - y.end) / n.steps
        for (i in 0:n.steps) {
            for (j in 1:ngroups) {
                canvas$image <- addGrob(canvas$image, kpropbarGrob
                                (x[levels == ylevels[j]],
                                 ylab = ylevels[j],
                                 ylab.pos = 1 - max.str.width,
                                 ylab.col = getColour(j, ngroups),
                                 draw.points = FALSE,
                                 lois = c(canvas$loi, canvas$loi.alt),
                                 y = y.start[j] - i*y.step[j],
                                 statLineHeight = 0.1 / ngroups,
                                 height = 0.6 / ngroups, vpcex = 1 / ngroups,
                                 name = paste("samplePlot.tempPropbar", j, sep="."),
                                 gp = gpar(alpha = 1 - (i / n.steps)),
                                 vp = canvas$graphPath("animation.field")))
            }
            canvas$image <- addGrob(canvas$image, kpropbarGrob
                            (x,
                             ylab = "",
                             draw.points = FALSE,
                             lois = c(canvas$loi, canvas$loi.alt),
                             y = y.end,
                             statLineHeight = 0.1 / ngroups,
                             height = 0.6 / ngroups, vpcex = 1 / ngroups,
                             name = "samplePlot.tempTotalPropbar",
                             gp = gpar(alpha = i / n.steps),
                             vp = canvas$graphPath("animation.field")))
            canvas$showLabels()
            if (canvas$stopAnimation)
                return()
            canvas$drawImage()
        }
        if (canvas$stopAnimation)
            return()
        canvas$pauseImage(5)
    }
    canvas$rmGrobs(paste("samplePlot.tempPropbar", 1:ngroups, sep = "."))

    # Drawing the total propbar which we will later break apart.
    x.sample <- canvas$samples[[canvas$which.sample]]
    y.end <- seq(1, 1 + (ngroups - 1) / ngroups, length.out = ngroups)
    y.end <- y.end + (0.1 / ngroups)
    y.start <- 1 + (1 - 0.6 / ngroups) / 2
    y.step <- (y.start - y.end) / n.steps
    canvas$image <- addGrob(canvas$image, kpropbarGrob
                    (x,
                     ylab = "",
                     draw.points = FALSE,
                     lois = c(canvas$loi, canvas$loi.alt),
                     y = y.start,
                     statLineHeight = 0.1 / ngroups,
                     height = 0.6 / ngroups, vpcex = 1 / ngroups,
                     name = "samplePlot.tempTotalPropbar",
                     vp = canvas$graphPath("animation.field")))

    canvas$pauseImage(5) # Pausing to show the merged samples clearly as
                         # we don't have many steps

    # Drawing the datatext grob, it should get overwritten anyway when
    # we call plotSample()
    cols <- getColour(1:length(ylevels), length(ylevels))
    names(cols) <- ylevels
    cols <- cols[order(ylevels)]
    text <- as.character(levels[order(canvas$indexes[[canvas$which.sample]])])
    canvas$image <- addGrob(canvas$image, coldatatextGrob
                            (data = text,
                             title = if (e$method == "permvar") "Random groups" else "Re-randomised",
                             cols = cols,
                             xpos = 0.5, max = 50,
                             name = "samplePlot.databox.text.2",
                             vp = canvas$graphPath("databox", 2)))

    canvas$pauseImage(5)

    # Splitting apart randomised samples
    for (i in 0:n.steps) {
        for (j in 1:ngroups) {
            canvas$image <- addGrob(canvas$image, kpropbarGrob
                            (x.sample[levels == ylevels[j]],
                             ylab = ylevels[j],
                             ylab.pos = 1 - max.str.width,
                             ylab.col = getColour(j, ngroups),
                             draw.points = FALSE,
                             lois = c(canvas$loi, canvas$loi.alt),
                             y = y.start - i*y.step[j],
                             statLineHeight = 0.1 / ngroups,
                             height = 0.6 / ngroups, vpcex = 1 / ngroups,
                             name = paste("samplePlot.tempPropbar", j, sep="."),
                             gp = gpar(alpha = i / n.steps),
                             vp = canvas$graphPath("animation.field")))
        }
        canvas$image <- addGrob(canvas$image, kpropbarGrob
                        (x,
                         ylab = "",
                         draw.points = FALSE,
                         lois = c(canvas$loi, canvas$loi.alt),
                         y = y.start,
                         statLineHeight = 0.1 / ngroups,
                         height = 0.6 / ngroups, vpcex = 1 / ngroups,
                         name = "samplePlot.tempTotalPropbar",
                         gp = gpar(alpha = 1 - (i / n.steps)),
                         vp = canvas$graphPath("animation.field")))
        canvas$showLabels()
        if (canvas$stopAnimation)
            return()
        canvas$drawImage()
    }
    canvas$rmGrobs(c("samplePlot.tempTotalPropbar", paste("samplePlot.tempPropbar", 1:ngroups, sep = ".")))
}

addPropLinesKSamp <- function(canvas, e) {
    xs <- canvas$x
    totalprop <- length(xs[xs == canvas$loi]) / length(xs)
    group.props <- numeric(length(canvas$ylevels))
    for (i in 1:length(canvas$ylevels)) {
        lxs <- xs[canvas$levels == canvas$ylevels[i]]
        group.props[i] <- length(lxs[lxs == canvas$loi]) / length(lxs)
    }

    canvas$image <- addGrob(canvas$image, linesGrob
                            (x = unit(rep(totalprop, 2), "native"),
                             y = unit(c(1, 3), "native"),
                             gp = gpar(lty = "dashed"),
                             name = "statLine",
                             vp = canvas$graphPath("animation.field")))
    for (i in 1:length(canvas$ylevels)) {
        canvas$image <- addGrob(canvas$image, linesGrob
                                (x = unit(c(totalprop, group.props[i]), "native"),
                                 y = unit(0.4, "native"),
                                 gp = gpar(col = "red", lwd = 2),
                                 arrow = arrow(length = unit(0.1, "inches")),
                                 name = paste("dataStatArrow", i, sep = "."),
                                 vp = canvas$graphPath("data", i)))
    }
}

calcAveDevProp <- function(xs, ys, canvas) {
    ylevels <- canvas$ylevels
    totalprop <- length(xs[xs == canvas$loi]) / length(xs)
    absdev <- 0
    for (i in 1:length(ylevels)) {
        lxs <- xs[ys == ylevels[i]]
        levelprop <- length(lxs[lxs == canvas$loi]) / length(lxs)
        leveldev <- abs(levelprop - totalprop)
        absdev <- absdev + leveldev
    }
    absdev / length(ylevels)
}

calcChiSq <- function(xs, ys, canvas) {
    # Transform the data into binary response for each level
    xs <- sapply(xs, function(x) {
        if (x == canvas$loi)
            x
        else
            canvas$loi.alt
    })
    observedTable <- table(xs, ys)

    # Building up table of expected values
    expectedTable <- observedTable
    oRowSums <- rowSums(expectedTable)
    oColSums <- colSums(expectedTable)
    totalSum <- sum(observedTable)

    for (i in 1:nrow(expectedTable))
        for (j in 1:ncol(expectedTable))
            expectedTable[i, j] <- oRowSums[i] * oColSums[j] / totalSum

    # Calculating the X^2 stat
    sum((observedTable - expectedTable)^2)
}

plotPermKProportions <- function(canvas, e, i) {
    xdata <- canvas$samples[[i]]
    ydata <- as.character(canvas$levels)
    ylevels <- canvas$ylevels
    depth <- downViewport(canvas$graphPath("sample"))
    max.str.width <- stringWidth(ylevels)
    max.str.width <- convertWidth(max(max.str.width), "native", valueOnly = TRUE)
    upViewport(depth)
    draw.points <- length(xdata) <= 100 & length(ylevels) == 2
    grand.mean <- length(canvas$x[canvas$x == canvas$loi]) / length(canvas$x)
    for (j in 1:length(ylevels)) {
        canvas$image <- addGrob(canvas$image, kpropbarGrob
                                (xdata[ydata == ylevels[j]],
                                 ylab = ylevels[j],
                                 ylab.pos = 1 - max.str.width,
                                 ylab.col = getColour(j, length(ylevels)),
                                 draw.points = draw.points,
                                 lois = c(canvas$loi, canvas$loi.alt),
                                 y = 0.1, height = 0.6, vpcex = 1 / length(ylevels),
                                 name = paste("samplePlot.propbar", j, sep="."),
                                 vp = canvas$graphPath("sample", j)))
        leveldata <- xdata[ydata == ylevels[j]]
        levelprop <- length(leveldata[leveldata == canvas$loi]) / length(leveldata)
        canvas$image <- addGrob(canvas$image, linesGrob
                                (x = unit(c(grand.mean, levelprop), "native"),
                                 y = unit(0.35, "native"),
                                 gp = gpar(lwd = 2, col = "red"),
                                 arrow = arrow(length = unit(0.1, "inches")),
                                 vp = canvas$graphPath("sample", j),
                                 name = paste("samplePlot.line", j, sep=".")))
    }
    ## Filling sample databox
    cols <- getColour(1:length(ylevels), length(ylevels))
    names(cols) <- ylevels
    cols <- cols[order(ylevels)]
    text <- as.character(ydata[order(canvas$indexes[[i]])])
    canvas$image <- addGrob(canvas$image, coldatatextGrob
                            (data = text,
                             title = if (e$method == "permvar") "Random groups" else "Re-randomised",
                             cols = cols,
                             xpos = 0.5, max = 50,
                             name = "samplePlot.databox.text.2", vp = canvas$graphPath("databox", 2)))
}
