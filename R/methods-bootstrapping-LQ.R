canvasBootLQ <- setRefClass("canvasBootLQClass", contains = "canvasPlotClass",
                            methods = list(
    plotSample = function(env, i = which.sample) {
        plotSamplePointsAndBoxplotGhostQuartile(.self, env, i, q = 2)
    },

    showLabels = function() {
        bootLabels(.self)
    },

    calcStat = function(i = which.sample, y = NULL, canvas = .self) {
        calcQuartile(samples[[i]], y, q = 2)
    },

    calcAllStats = function(a, b = NULL, canvas = .self) {
        calcQuartile(a, b, q = 2)
    },

    plotDataStat = function(env, ...) {
        lineOnBoxplotQuartile(.self, env, q = 2)
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
        boot1000Quartile(.self, env, q = 2, ...)
    },

    fadePlots = function(env, ...) {
        fadeSampleAndStat(.self, env, ...)
    }))

load_bootstrap_LQ <- function(e){
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
    tmp.canvas <- canvasBootLQ$new()
    tmp.canvas$import(e$c1)
    e$c1 <- tmp.canvas
    e$c1$viewports <- tmp.vps
    e$c1$image <- tmp.image
    e$difference <- FALSE
}
