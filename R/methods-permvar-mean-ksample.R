canvasPermvarMeanKSample <- setRefClass("canvasPermvarMeanKSampleClass", contains = "canvasPlotClass",
                                        methods = list(
    calcStat = function(i = which.sample, ys = NULL, canvas = .self) {
        if (stat.method == "F-statistic") {
            calcF(as.numeric(x), level.samples[[i]], fun = mean)
        } else if (stat.method == "average deviation") {
            calcAveDev(as.numeric(x), level.samples[[i]], fun = mean)
        }
    },

    calcAllStats = function(xs, ys = NULL, canvas = .self) {
        if (stat.method == "F-statistic") {
            calcF(as.numeric(xs), ys, fun = mean)
        } else if (stat.method == "average deviation") {
            calcAveDev(as.numeric(xs), ys, fun = mean)
        }
    },

    plotSample = function(env, i = which.sample) {
        plotKSample(.self, env, i, mean)
    },

    showLabels = function() {
        permvarLabels(.self)
    },

    plotDataStat = function(env, ...) {
        addPermvarLinesKSamp(canvas = .self, e = env, fun = mean)
    },

    plotStatDist = function(env, ...) {
        plotPermStat(.self, env)
    },

    animateSample = function(...) {
        animatePermvarKSample(.self, ...)
    },

    animateStat = function(env, n.steps) {
        animateFStat(.self, env, n.steps, fun = mean,
                     fstat = env$perm.method == "F-statistic")
    },

    ## perm1000 taken from methods-permutation-mean.R
    handle1000 = function(env, ...) {
        perm1000(.self, env, ...)
    }))

load_permvar_mean_ksample <- function(e) {
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
    tmp.canvas <- canvasPermvarMeanKSample$new()
    tmp.canvas$import(e$c1)
    e$c1 <- tmp.canvas
    e$c1$viewports <- tmp.vps
    e$c1$image <- tmp.image
    e$extra <- 0
}

animatePermvarKSample <- function(canvas, e, n.steps, mix = TRUE) {
    e$clearPanel("sample")
    canvas$rmGrobs("samplePlot.databox.text.2")
    ## Drop samples down to middle plot
    x <- canvas$x
    levels <- canvas$levels
    ylevels <- canvas$ylevels
    nsamps <- length(ylevels)
    cols <- getColour(1:length(ylevels), length(ylevels))
    y <- old.stackPoints(canvas$x, vp = canvas$graphPath("data"),
                         y.min = 0.3, y.max = 1) + 2
    y.start <- y
    vp.points.diff <- diff(c(nsamps/2, nsamps/2 + 0.1*nsamps)) / nsamps
    y.end <- old.stackPoints(canvas$x, y.min = 1.5, y.max = 1.6,
                             vp = canvas$graphPath("animation.field"))
    ##y.end <- ((y - 2.3) * ((vp.points.diff+0.2) / 0.7)) + 1.5
    if (! mix)
        n.steps <- 3
    y.step <- (y.start - y.end)/n.steps

    ## Dropping samples
    for (i in 0:n.steps) {
        plotPoints(canvas, x, y.start - i*y.step,
                   canvas$graphPath("animation.field"),
                   "samplePlot.temp", col = "black")
        if (canvas$stopAnimation)
            return()
        canvas$drawImage()
    }
    if (canvas$stopAnimation)
        return()
    canvas$pauseImage(10)
    canvas$rmGrobs("samplePlot.temp.points.")
    ## Separating samples - all relative to BOTTOM viewport.
    x.sample <- canvas$samples[[canvas$which.sample]]
    y.start <- old.stackPoints(x, vp = canvas$graphPath("sample", 1),
                               y.min = nsamps/2, y.max = nsamps/2 + 0.1*nsamps)
    y.start <- y.start[canvas$indexes[[canvas$which.sample]]]
    ## Adding text to data boxes.
    text <-  as.character(levels[order(canvas$indexes[[canvas$which.sample]])])
    names(cols) <- ylevels
    dtcols <- cols[sort(ylevels)]
    canvas$image <- addGrob(canvas$image, coldatatextGrob
                            (data = text, title = "Random groups", cols = dtcols,
                             xpos = 0.5, max = 50,
                             name = "samplePlot.databox.text.2",
                             vp = canvas$graphPath("databox", 2)))
    y.sample <- old.stackPoints(x.sample, levels, vp = canvas$graphPath("sample"))
    y.end.samps <- list()
    y.start.samps <- list()
    y.step.samps <- list()
    for (i in 1:nsamps){
        y.start.samps[[i]] <- y.start[levels == ylevels[i]]
        y.end.samps[[i]] <- y.sample[levels == ylevels[i]] + (i - 1)
        y.step.samps[[i]] <- (y.start.samps[[i]] - y.end.samps[[i]])/n.steps
    }
    for (j in 1:nsamps){
        plotPoints(canvas, x.sample[levels == ylevels[j]],
                   y.start.samps[[j]],
                   vp = canvas$graphPath("sample", 1),
                   name = paste("samplePlot.tempjoin", j, sep = ""),
                   col = cols[j])
    }
    canvas$pauseImage(5)
    for (i in 1:n.steps){
        for (j in 1:nsamps){
            plotPoints(canvas, x.sample[levels == ylevels[j]],
                       y.start.samps[[j]] - i*y.step.samps[[j]],
                       vp = canvas$graphPath("sample", 1),
                       name = paste("samplePlot.tempjoin", j, sep = ""),
                       col = cols[j])
        }
        if (canvas$stopAnimation)
            return()
        canvas$drawImage()
    }
    rmgrobs <- paste(c(rep("samplePlot.tempjoin", each = nsamps), rep("samplePlot.join", each = nsamps)),
                     rep(1:nsamps, 2), sep = "")
    canvas$rmGrobs(paste(rmgrobs, "points.1", sep = "."))
}

addPermvarLinesKSamp <- function(canvas, e, fun = mean){
    x <- canvas$x
    expx <- fun(x)
    canvas$image <- addGrob(canvas$image,
                            segmentsGrob(x0 = expx, x1 = expx, y0 = 1,
                                         y1 = 3, default.units = "native",
                                         gp = gpar(col = "grey60", lty = "dashed"),
                                         vp = canvas$graphPath("animation.field"),
                                         name = "hline"))
}
