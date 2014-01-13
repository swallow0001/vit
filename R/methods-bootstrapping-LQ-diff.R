canvasBootLQDiff <- setRefClass("canvasBootLQDiffClass", contains = "canvasPlotClass",
                                methods = list(
    plotSample = function(env, i = which.sample, ...) {
        plotSampleQuartileLevels(.self, env, i, q = 2, ...)
    },

    showLabels = function() {
        bootLabelsTwoSample(.self)
    },

    calcStat = function(i = which.sample, ys = NULL, canvas = .self) {
        if (all(ylevels == sort(unique(ys)))) {
            calcDiff(samples[[i]], ys, fun = function(x) fivenum(x)[2])
        } else {
            rev(calcDiff(samples[[i]], ys, fun = function(x) fivenum(x)[2]))
        }
    },

    calcAllStats = function(xs, ys = NULL, canvas = .self) {
        if (all(ylevels == sort(unique(ys)))) {
            calcDiff(xs, ys, fun = function(x) fivenum(x)[2])
        } else {
            rev(calcDiff(xs, ys, fun = function(x) fivenum(x)[2]))
        }
    },

    plotDataStat = function(env, ...) {
        dataDiffArrowQuartile(.self, env, q = 2)
    },

    plotStatDist = function(env, ...) {
        plotBootDiffDist(.self, env)
    },

    animateSample = function(...) {
        dropPointsDiff(.self, ...)
    },

    trackSample = function(env) {
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

load_bootstrap_LQ_diff <- function(e) {
    e$c1$stat.in.use <- svalue(e$stat)
    e$pairedSamples <- svalue(e$paired.samples)
    e$difference <- TRUE
    # There is something messy going on with viewports and
    # gTrees, such that when attempting to import them, we
    # end up with "uninitializedField"s. Assign to a temp
    # var and reassign later.
    tmp.vps <- e$c1$viewports
    tmp.image <- e$c1$image
    e$c1$viewports <- NULL
    e$c1$image <- NULL
    tmp.canvas <- canvasBootLQDiff$new()
    tmp.canvas$import(e$c1)
    e$c1 <- tmp.canvas
    e$c1$viewports <- tmp.vps
    e$c1$image <- tmp.image
}
