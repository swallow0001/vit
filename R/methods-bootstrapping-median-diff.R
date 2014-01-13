canvasBootMedianDiff <- setRefClass("canvasBootMedianDiffClass", contains = "canvasPlotClass",
                                    methods = list(
    plotSample = function(env, i = which.sample, ...) {
        plotSampleLevelsMedian(.self, env, i, ...)
    },

    showLabels = function() {
        bootLabelsTwoSample(.self)
    },
    ## calcMedianDiff taken from methods-permutation-median.R
    calcStat = function(i = which.sample, ys = NULL, canvas = .self) {
        ys <- level.samples[[i]]
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
    ## dataDiffArrowMean taken from mehtods-permutation-mean.R
    plotDataStat = function(env, ...) {
        dataDiffArrowMedian(.self, env)
    },

    plotStatDist = function(env, ...) {
        plotBootDiffDist(.self, env)
    },

    animateSample = function(...) {
        dropPointsDiff(.self, ...)
    },

    trackSample = function(...) {
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

load_bootstrap_median_diff<- function(e) {
    e$c1$orderLevels(FUN = median)
    e$c1$stat.in.use <- svalue(e$stat)
    e$resampWithin <- enabled(e$resamp.within) & svalue(e$resamp.within)
    # There is something messy going on with viewports and
    # gTrees, such that when attempting to import them, we
    # end up with "uninitializedField"s. Assign to a temp
    # var and reassign later.
    tmp.vps <- e$c1$viewports
    tmp.image <- e$c1$image
    e$c1$viewports <- NULL
    e$c1$image <- NULL
    tmp.canvas <- canvasBootMedianDiff$new()
    tmp.canvas$import(e$c1)
    e$c1 <- tmp.canvas
    e$c1$viewports <- tmp.vps
    e$c1$image <- tmp.image
    e$extra <- 0
    e$difference <- TRUE
}

plotSampleLevelsMedian <- function(canvas, e, i, alpha = 0.25){
    x <- canvas$samples[[i]]
    levels <- canvas$level.samples[[i]]
    ylevels <- canvas$ylevels
    y <- old.stackPoints(x, levels, vp = canvas$graphPath("sample"))
    n <- 1
    canvas$sampled.stats <- c(canvas$sampled.stats, i)
    stats <- canvas$stat.dist[canvas$sampled.stats, , drop = FALSE]
    stats[, 1:2] <- stats[, 2:1]
    ## Plotting samples, labels and ghosts
    for (j in ylevels) {
        if (length(x) >= canvas$hist.cutoff) {
            plotHist(canvas, x[levels == j],
                     fill = getColour(n, length(ylevels), c = 50),
                     vp = canvas$graphPath("sample", n),
                     name = "samplePlot")
            # Using a rectGrob in the histogram case as we don't
            # want to see the red quartiles because they are difficult
            # to see above the histograms
            canvas$image <- addGrob(canvas$image, rectGrob
                                    (x = unit(stats[, n], "native"),
                                     y = unit(0.15, "npc"), height = unit(0.2, "npc"),
                                     width = 0, gp = gpar(alpha = alpha, col = "blue", lwd = 2),
                                     vp = canvas$graphPath("sample", n),
                                     name = paste("samplePlot.ghosts", n, sep = ".")))
        } else {
            plotPoints(canvas, x[levels == j], y[levels == j],
                       col = getColour(n, length(ylevels)),
                       vp = canvas$graphPath("sample", n),
                       name = "samplePlot")
            canvas$image <- addGrob(canvas$image, ghostsGrob
                                    (canvas$quartiles[canvas$sampled.stats, 1, j],
                                     canvas$quartiles[canvas$sampled.stats, 2, j],
                                     canvas$quartiles[canvas$sampled.stats, 3, j],
                                     alpha = alpha, box.color = "lightpink",
                                     vp = canvas$graphPath("sample", as.character(n)),
                                     name =  paste("samplePlot.ghosts", n, sep = ".")))
        }
        plotBoxplot(canvas, x[levels == j],
                    stat = median, stat.color = "blue",
                    vp = canvas$graphPath("sample", as.character(n)),
                    name = "samplePlot")
        canvas$image <- addGrob(canvas$image, boxplotGrob
                                (x[levels == j], box.color = "black",
                                 median.color = "black",
                                 show.w = FALSE, gp = gpar(lwd = 3),
                                 name = paste("samplePlot.boxplot", n, sep = "."),
                                 vp = canvas$graphPath("sample", as.character(n))))
        canvas$image <- addGrob(canvas$image, linesGrob
                                (x = unit(rep(median(x[levels == j]), 2),
                                 "native"),
                                 y = unit(c(0.1, 0.55), "npc"),
                                 gp = gpar(lwd = 3, col = "blue"),
                                 vp = canvas$graphPath("sample", as.character(n)),
                                 name = paste("samplePlot", "line", n, sep = ".")))
        canvas$image <- addGrob(canvas$image, roundrectGrob
                                (x = unit(1, "npc") + unit(1, "mm"),
                                 y = unit(0, "npc"), just = c("right", "bottom"),
                                 height = stringHeight(j) + unit(2, "mm"),
                                 width = stringWidth(j) + unit(2, "mm"),
                                 vp = canvas$graphPath("sample", n),
                                 gp = gpar(fill = "white", col = "white", alpha = 0.75),
                                 name = paste("samplePlot.blankrect", n, sep = ".")))
        canvas$image <- addGrob(canvas$image, textGrob
                                (j, x = 1, y = unit(1, "mm"), just = c("right", "bottom"),
                                 vp = canvas$graphPath("sample", as.character(n)),
                                 name = paste("samplePlot.text", n, sep = ".")))
        n <- n + 1
    }
    ## Plotting arrow difference
    canvas$image <- addGrob(canvas$image, linesGrob(x = unit(canvas$stat.dist[i, ], "native"),
                                                    y = unit(0.8, "npc"),
                                                    gp = gpar(lwd = 2, col = "red"),
                                                    arrow = arrow(length = unit(0.1, "inches")),
                                                    vp = canvas$graphPath("sample", 1),
                                                    name = "samplePlot.stat.2"))
    ## Filling sample databox
    cols <- getColour(1:length(ylevels), length(ylevels))
    names(cols) <- ylevels
    cols <- cols[order(ylevels)]
    text.sample1 <- datatextGrob(data = x, title = "", x = 0.25,
                                 max = 50, name = "text.sample1",
                                 vp = canvas$graphPath("databox", 2))
    text.sample2 <- coldatatextGrob(data = levels, title = "",
                                    cols = cols, xpos = 0.75,
                                    max = 50, name = "text.sample2",
                                    vp = canvas$graphPath("databox", 2))
    text.title <- datatextGrob(data = "", title = "Re-sample",
                               name = "text.sampletitle", vp = canvas$graphPath("databox", 2))
    text.sample <- grobTree(text.sample1, text.sample2, text.title, name = "samplePlot.databox.text.2")
    canvas$image <- addGrob(canvas$image, text.sample)
}
