canvasPermvarMedianDiff <- setRefClass("canvasPermvarMedianDiffClass", contains = "canvasPlotClass",
                                       methods = list(
    ## PlotKSample comes from methods-permutation-mean-ksample.R
    plotSample = function(env, i = which.sample) {
        plotKSample(.self, env, i, fun = median)
    },

    showLabels = function() {
        permvarLabels(.self)
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
    },

    plotStatDist = function(env, ...) {
        plotDiffDist(.self, env)
    },

    animateSample = function(...) {
        permvarTwoSample(.self, ...)
    },

    animateStat = function(env, n.steps) {
        dropPermArrow(.self, env, n.steps)
    },

    handle1000 = function(env, ...) {
        perm1000(.self, env, ...)
    }))

load_permvar_median_diff <- function(e) {
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
    tmp.canvas <- canvasPermvarMedianDiff$new()
    tmp.canvas$import(e$c1)
    e$c1 <- tmp.canvas
    e$c1$viewports <- tmp.vps
    e$c1$image <- tmp.image
    ## Gives a value for the additional room given to each end of the x axis scale.
    e$extra <- 0
}
