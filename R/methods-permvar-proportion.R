canvasPermvarPropDiff <- setRefClass("canvasPermvarPropDiffClass", contains = "canvasPlotClass",
                                     methods = list(
    plotSample = function(env, i = which.sample) {
       plotKSampleProportions(.self, env, i)
    },

    showLabels = function() {
        permvarLabels(.self)
    },

    calcStat = function(i = which.sample, ys = NULL, canvas = .self) {
        props <- calcPropDiff(samples[[i]], levels, canvas)
        # Sorting in ylevel order but reversing so we end up
        # with correct differences
        rev(props[canvas$ylevels])
    },

    calcAllStats = function(xs, ys = NULL, canvas = .self) {
        props <- calcPropDiff(xs, ys, canvas)
        # Sorting in ylevel order but reversing so we end up
        # with correct differences
        rev(props[canvas$ylevels])
    },

    plotDataStat = function(env, ...) {
        permvarDiffArrowProp(.self, env)
    },

    plotStatDist = function(env, ...) {
        plotDiffDist(.self, env)
    },

    animateSample = function(...) {
        animatePermvarKSampleProp(.self, ...)
    },

    animateStat = function(env, n.steps) {
        dropPermPropArrow(.self, env, n.steps)
    },

    handle1000 = function(env, ...) {
        perm1000(.self, env, ...)
    }))

load_permvar_proportion_diff <- function(e) {
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
    tmp.canvas <- canvasPermvarPropDiff$new()
    tmp.canvas$import(e$c1)
    e$c1 <- tmp.canvas
    e$c1$viewports <- tmp.vps
    e$c1$image <- tmp.image
    ## Gives a value for the additional room given to each end of the x axis scale.
    e$extra <- 0
}

permvarDiffArrowProp <- function(canvas, e){
    x <- canvas$x
    n <- length(x)
    prop <- length(x[x == canvas$loi]) / n
    canvas$image <- addGrob(canvas$image, textGrob
                            (label = format(round(prop, canvas$dp), nsmall = canvas$dp),
                             x = unit(prop, "native"),
                             y = unit(0.65, "npc"),
                             just = c("centre", "bottom"),
                             gp = gpar(lwd = 2, col = "red", fontface = "bold"),
                             vp = canvas$graphPath("data", 1),
                             name = "dataPlot.stat.text"))
}
