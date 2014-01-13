canvasPermMedianDiff <- setRefClass("canvasPermMedianDiffClass", contains = "canvasPlotClass",
                                    methods = list(
    ## PlotKSample comes from methods-permutation-mean-ksample.R
    plotSample = function(env, i = which.sample) {
        plotKSample(.self, env, i, fun = median)
    },

    showLabels = function() {
        permLabels(.self)
    },

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

    plotDataStat = function(env, ...) {
        dataDiffArrowMedian(.self, env)
    },

    plotStatDist = function(env, ...) {
        plotDiffDist(.self, env)
    },

    animateSample = function(...) {
        permTwoSample(.self, ...)
    },

    animateStat = function(env, n.steps) {
        dropPermArrow(.self, env, n.steps)
    },

    displayResult = function(env, ...) {
        showTailPerm2Sample(.self, env, fun = median, ...)
    },

    handle1000 = function(env, ...) {
        perm1000(.self, env, ...)
    }))

load_permutation_median <- function(e) {
    e$c1$orderLevels(FUN = median)
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
    tmp.canvas <- canvasPermMedianDiff$new()
    tmp.canvas$import(e$c1)
    e$c1 <- tmp.canvas
    e$c1$viewports <- tmp.vps
    e$c1$image <- tmp.image
    ## Gives a value for the additional room given to each end of the x axis scale.
    e$extra <- 0
}

dataDiffArrowMedian <- function(canvas, e){
    x <- canvas$x
    levels <- canvas$levels
    ylevels <- canvas$ylevels
    median1 <- median(x[levels == ylevels[1]])
    median2 <- median(x[levels != ylevels[1]])
    y <- old.stackPoints(x, levels, vp = canvas$graphPath("data"))
    n <- 1
    for (i in ylevels) {
        plotBoxplot(canvas, x[levels == i], stat = median, stat.color = "purple",
                    show.w = length(x) < canvas$hist.cutoff,
                    vp = canvas$graphPath("data", as.character(n)),
                    name = "dataPlot")
        n <- n + 1
    }

    # Moving the diff arrows down in the case when we are using histograms
    # because histograms are level on y = 0
    y.locs <- c(0.8, 0.9)
    if (length(x) >= canvas$hist.cutoff)
        y.locs <- y.locs - 0.1

    canvas$image <- addGrob(canvas$image, linesGrob(x = unit(c(median2, median1), "native"),
                                                    y = unit(y.locs[1], "npc"),
                                                    gp = gpar(lwd = 2, col = "red"),
                                                    arrow = arrow(length = unit(0.1, "inches")),
                                                    vp = canvas$graphPath("data", 1),
                                                    name = "dataPlot.stat.2"))
    canvas$image <- addGrob(canvas$image, linesGrob(x = unit(rep(0, 2), "native"),
                                                    y = unit(c(0, 0.4), "npc"),
                                                    gp = gpar(lty = "dashed"),
                                                    vp = canvas$graphPath("stat"),
                                                    name = "zeroline.1"))
    canvas$image <- addGrob(canvas$image, textGrob
                            (label = format(round(diff(c(median2, median1)), canvas$dp), nsmall = canvas$dp),
                             x = unit(mean(c(median2, median1)), "native"),
                             y = unit(y.locs[2], "npc"),
                             just = c("centre", "bottom"),
                             gp = gpar(lwd = 2, col = "red", fontface = "bold"),
                             vp = canvas$graphPath("data", 1),
                             name = "dataPlot.stat.text"))
}
