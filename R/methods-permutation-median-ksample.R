canvasPermMedianKSample <- setRefClass("canvasPermMedianKSampleClass", contains = "canvasPlotClass",
                                    methods = list(
    calcStat = function(i = which.sample, ys = NULL, canvas = .self) {
        if (stat.method == "pseudo F-statistic") {
            calcPseudoF(samples[[i]], ys, fun = median)
        } else if (stat.method == "average deviation") {
            calcAveDev(samples[[i]], ys, fun = median)
        }
    },

    calcAllStats = function(xs, ys = NULL, canvas = .self) {
        if (stat.method == "pseudo F-statistic") {
            calcPseudoF(xs, ys, fun = median)
        } else if (stat.method == "average deviation") {
            calcAveDev(xs, ys, fun = median)
        } 
    },

    ## plotKSample taken from methods-permutation-ksample.R
    plotSample = function(env, i = which.sample) {
        plotKSample(.self, env, i, median)
    },

    showLabels = function() {
        permPropLabels(.self)
    },

    plotDataStat = function(env, ...) {
        addLinesKSamp(canvas = .self, e = env, fun = median)
    },

    plotStatDist = function(env, ...) {
        plotPermStat(.self, env)
    },

    animateSample = function(...) {
        animateKSample(.self, ...)
    },

    animateStat = function(env, n.steps) {
        animateFStat(.self, env, n.steps, fun = median,
                     fstat = env$perm.method == "pseudo F-statistic")
    },

    displayResult = function(env, ...) {
        showTailKSample(.self, env, fun = median,
                        fstat = env$perm.method == "pseudo F-statistic")
    },
    ## perm1000 taken from methods-permutation-mean.R
    handle1000 = function(env, ...) {
        perm1000(.self, env, ...)
    }))

load_permutation_median_ksample <- function(e) {
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
    tmp.canvas <- canvasPermMedianKSample$new()
    tmp.canvas$import(e$c1)
    e$c1 <- tmp.canvas
    e$c1$viewports <- tmp.vps
    e$c1$image <- tmp.image
    e$extra <- 0
}

calcAveDev <- function(x, y, fun = mean){
    uy <- unique(y)
    gstat <- fun(x)
    devs <- numeric(length(uy))
    j = 1
    for (i in uy){
        samp <- x[y == i]
        devs[j] <- abs(fun(samp) - gstat)
        j <- j + 1
    }
    mean(devs)
}

calcPseudoF <- function(x, y, fun = median){
    uy <- unique(y)
    gstat <- fun(x)
    sstats <- ns <- numeric(length(uy))
    j <- 1
    SSW <- 0
    for (i in uy){
        samp <- x[y == i]
        sstats[j] <- fun(samp)
        ns[j] <- length(samp)
        quarts <- fivenum(samp)[c(2, 4)]
        SSW <- SSW + sqrt(length(samp) - 1)*diff(quarts)
        j <- j + 1
    }
    SSB <- sum(sqrt(ns)*abs(sstats - gstat))
    SSB/SSW
}
