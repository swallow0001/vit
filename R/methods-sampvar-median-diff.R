canvasSampvarMedianDiff <- setRefClass("canvasSampvarMedianDiffClass", contains = "canvasPlotClass",
                                       methods = list(
    calcStat = function(i = which.sample, ys = NULL, canvas = .self) {
        if (all(ylevels == sort(unique(ys)))) {
            calcDiff(samples[[i]], ys, fun = median)
        } else {
            rev(calcDiff(samples[[i]], ys, fun = median))
        }
    },

    calcAllStats = function(xs, ys = NULL, canvas = .self) {
        if (all(ylevels == sort(unique(ys)))) {
            calcDiff(xs, ys, fun = median)
        } else {
            rev(calcDiff(xs, ys, fun = median))
        }
    },

    animateStat = function(env, n.steps) {
        dropBootArrow(.self, env, n.steps)
    },

    plotSample = function(env, i = which.sample, ...) {
        plotSampvarMedianLevels(.self, i, env, ...)
    },

    fadePlots = function(env, ...) {
        fadeDataTwoSample(.self, env)
    },

    showLabels = function() {
        sampvarDiffLabels(.self)
    },

    plotDataStat = function(env, ...) {
        showStatDiffSampvarMedian(.self, env)
    },

    plotStatDist = function(env, ...) {
        plotBootDiffDist(.self, env)
    },

    animateSample = function(e, n.steps, n.slow, opts) {
        dropSampvarPointsDiff(.self, e, n.steps, n.slow)
    },

    handle1000 = function(env, ...) {
        sampvarDiff1000(.self, env, ...)
    }))

load_sampvar_median_diff <- function(e) {
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
    tmp.canvas <- canvasSampvarMedianDiff$new()
    tmp.canvas$import(e$c1)
    e$c1 <- tmp.canvas
    e$c1$viewports <- tmp.vps
    e$c1$image <- tmp.image
    e$difference <- TRUE
}

showStatDiffSampvarMedian <- function(canvas, e){
    dataDiffArrowMedian(canvas, e)
    x <- canvas$x
    levels <- canvas$levels
    ylevels <- canvas$ylevels
    mean1 <- median(x[levels == ylevels[1]])
    mean2 <- median(x[levels != ylevels[1]])
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

plotSampvarMedianLevels <- function(canvas, i, e, alpha = 0.25) {
    canvas$rmGrobs(c("samplePlot.temp.data.points."))
    index <- canvas$indexes[[i]]
    x <- canvas$x[index]
    levels <- canvas$levels[index]
    ylevels <- canvas$ylevels
    y <- old.stackPoints(x, levels, vp = canvas$graphPath("sample"))
    n <- 1
    cols <- character(canvas$n)
    cols[levels == canvas$ylevels[1]] <- getColour(1, 2, l = 35)
    cols[levels == canvas$ylevels[2]] <- getColour(2, 2, l = 35)
    ## Plotting samples, labels and ghosts
    for (j in 1:length(ylevels)) {
        plotPoints(canvas, x[levels == ylevels[j]],
                   y[levels == ylevels[j]],
                   col = getColour(1:length(ylevels), length(ylevels))[n],
                   vp = canvas$graphPath("sample", as.character(n)),
                   name = "samplePlot")
        canvas$image <- addGrob(canvas$image, linesGrob
                                (x = unit(rep(median(x[levels == ylevels[j]]), 2),
                                 "native"),
                                 y = unit(c(0.1, 0.55), "npc"),
                                 gp = gpar(lwd = 3, col = "blue"),
                                 vp = canvas$graphPath("sample", as.character(n)),
                                 name = paste("samplePlot", "line", n, sep = ".")))
        canvas$image <- addGrob(canvas$image, textGrob
                                (ylevels[j], x = 1, y = unit(1, "mm"), just = c("right", "bottom"),
                                 vp = canvas$graphPath("sample", as.character(n)),
                                 name = paste("samplePlot.text", n, sep = ".")))
        n <- n + 1
    }

    # Plotting data points
    data.y <- 2 + 0.5*canvas$y[index] + 0.5*(levels == canvas$ylevels[2])
    canvas$image <- addGrob(canvas$image,
                            pointsGrob(x = x, y = data.y, vp = canvas$graphPath("animation.field"),
                                       name = "samplePlot.temp.point",
                                       gp = gpar(col = cols), pch = 19))
    ## Plotting arrow difference
    canvas$image <- addGrob(canvas$image, linesGrob(x = unit(c(median(x[levels == ylevels[2]]),
                                                               median(x[levels == ylevels[1]])), "native"),
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
