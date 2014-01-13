canvasSampvarMedian <- setRefClass("canvasSampvarMedianClass", contains = "canvasPlotClass",
                                   methods = list(    
    calcStat = function(i = which.sample, y = NULL, canvas = .self) {
        calcMedian(samples[[i]], y)
    },

    calcAllStats = function(a, b = NULL, canvas = .self) {
        calcMedian(a, b)
    },
    plotSample = function(env, i = which.sample) {
        plotSampvarBoxplotGhostMean(.self, i, env)
    },

    fadePlots = function(env, ...) {
        fadeData(.self, env)
    },

    showLabels = function() {
        sampvarLabels(.self)
    },

    plotDataStat = function(env, ...) {
        addStatLine(.self, env, fun = median)
    },

    plotStatDist = function(env, ...) {
        plotBootDist(.self, env)
    },

    animateSample = function(env, n.steps, n.slow, opts) {
        dropSampvarPoints1d(.self, env, n.steps, n.slow,
                            keep.plot = opts$keep.plot, move = opts$move)
    },

    animateStat = function(env, n.steps) {
        dropStat(.self, env, n.steps)
    },

    handle1000 = function(env, ...) {
        boot1000mean(.self, env, ...)
    }))

load_sampvar_median <- function(e) {
    e$c1$stat.in.use <- svalue(e$stat)
    # There is something messy going on with viewports and
    # gTrees, such that when attempting to import them, we
    # end up with "uninitializedField"s. Assign to a temp
    # var and reassign later.
    tmp.vps <- e$c1$viewports
    tmp.image <- e$c1$image
    e$c1$viewports <- NULL
    e$c1$image <- NULL
    tmp.canvas <- canvasSampvarMedian$new()
    tmp.canvas$import(e$c1)
    e$c1 <- tmp.canvas
    e$c1$viewports <- tmp.vps
    e$c1$image <- tmp.image
    e$difference <- FALSE
}
