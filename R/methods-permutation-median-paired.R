canvasPermMedianPaired <- setRefClass("canvasPermMedianPairedClass", contains = "canvasPlotClass",
                                      methods = list(
    plotSample = function(env, i = which.sample, ...) {
        plotSamplePointsPaired(.self, env, i, fun = median)
    },

    showLabels = function() {
        permPairedLabels(.self)
    },

    calcStat = function(i = which.sample, y = NULL, canvas = .self) {
        calcPermPairedMedian(samples[[i]], y)
    },

    calcAllStats = function(a, b = NULL, canvas = .self) {
        calcPermPairedMedian(a, b)
    },

    plotDataStat = function(env, ...) {
        zeroLineAndArrow(.self, env, fun = median)
    },

    plotStatDist = function(env, ...) {
        plotBootDist(.self, env)
    },

    animateSample = function(...) {
    },

    trackSample = function(env) {
    },

    animateStat = function(env, n.steps) {
        dropStat(.self, env, n.steps)
    },

    displayResult = function(env, ...) {
        showTailPairedPerm(.self, env, fun = median)
    },

    handle1000 = function(env, ...) {
        boot1000mean(.self, env, points = TRUE)
    },

    fadePlots = function(env, ...) {
    }))

load_permutation_median_paired <- function(e) {
    e$pairedSamples <- svalue(e$paired.samples)
    e$c1$stat.in.use <- svalue(e$stat)
    e$perm.method <- svalue(e$perm.choices)
    e$c1$stat.method <- e$perm.method
    ## Ensuring stat arrow always points to the right.
    if (eval(call(e$c1$stat.in.use, e$c1$x)) < 0){
        e$c1$x <- -e$c1$x
        e$c1$paired.data[, 1:2] <- e$c1$paired.data[, 2:1]
    }
    # There is something messy going on with viewports and
    # gTrees, such that when attempting to import them, we
    # end up with "uninitializedField"s. Assign to a temp
    # var and reassign later.
    tmp.vps <- e$c1$viewports
    tmp.image <- e$c1$image
    e$c1$viewports <- NULL
    e$c1$image <- NULL
    tmp.canvas <- canvasPermMedianPaired$new()
    tmp.canvas$import(e$c1)
    e$c1 <- tmp.canvas
    e$c1$viewports <- tmp.vps
    e$c1$image <- tmp.image
    e$extra <- 0
}

calcPermPairedMedian <- function(x, y = NULL){
    median(x)
}
