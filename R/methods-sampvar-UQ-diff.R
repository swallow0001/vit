canvasSampvarUQDiff <- setRefClass("canvasSampvarUQDiffClass", contains = "canvasPlotClass",
                                   methods = list(
    calcStat = function(i = which.sample, ys = NULL, canvas = .self) {
        if (all(ylevels == sort(unique(ys)))) {
            calcDiff(samples[[i]], ys, fun = function(x) fivenum(x)[4])
        } else {
            rev(calcDiff(samples[[i]], ys, fun = function(x) fivenum(x)[4]))
        }
    },

    calcAllStats = function(xs, ys = NULL, canvas = .self) {
        if (all(ylevels == sort(unique(ys)))) {
            calcDiff(xs, ys, fun = function(x) fivenum(x)[4])
        } else {
            rev(calcDiff(xs, ys, fun = function(x) fivenum(x)[4]))
        }
    },

    animateStat = function(env, n.steps) {
        dropBootArrow(.self, env, n.steps)
    },

    plotSample = function(env, i = which.sample, ...) {
        plotSampvarQuartileLevels(.self, i, env, q = 4, ...)
    },

    fadePlots = function(env, ...) {
        fadeDataTwoSample(.self, env)
    },

    showLabels = function() {
        sampvarDiffLabels(.self)
    },

    plotDataStat = function(env, ...) {
        dataDiffArrowQuartile(.self, env, q = 4)
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

load_sampvar_UQ_diff <- function(e) {
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
    tmp.canvas <- canvasSampvarUQDiff$new()
    tmp.canvas$import(e$c1)
    e$c1 <- tmp.canvas
    e$c1$viewports <- tmp.vps
    e$c1$image <- tmp.image
    e$difference <- TRUE
}
