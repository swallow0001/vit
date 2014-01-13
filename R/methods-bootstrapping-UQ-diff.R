canvasBootUQDiff <- setRefClass("canvasBootUQDiffClass", contains = "canvasPlotClass",
                                methods = list(
    plotSample = function(env, i = which.sample, ...) {
        plotSampleQuartileLevels(.self, env, i, q = 4, ...)
    },

    showLabels = function() {
        bootLabelsTwoSample(.self)
    },

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

    plotDataStat = function(env, ...) {
        dataDiffArrowQuartile(.self, env, q = 4)
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

load_bootstrap_UQ_diff <- function(e) {
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
    tmp.canvas <- canvasBootUQDiff$new()
    tmp.canvas$import(e$c1)
    e$c1 <- tmp.canvas
    e$c1$viewports <- tmp.vps
    e$c1$image <- tmp.image
}

plotSampleQuartileLevels <- function(canvas, e, i, alpha = 0.25, q = 2){
    x <- canvas$samples[[i]]
    levels <- canvas$level.samples[[i]]
    ylevels <- canvas$ylevels
    y <- old.stackPoints(x, levels, vp = canvas$graphPath("sample"))
    n <- 1
    canvas$sampled.stats <- c(canvas$sampled.stats, i)
    stats <- canvas$stat.dist[canvas$sampled.stats, , drop = FALSE]
    stats[, 1:2] <- stats[, 2:1]
    q.order <- if (q == 2) c(2, 1, 3) else c(1, 3, 2)
    ## Plotting samples, labels and ghosts
    for (j in ylevels) {
        canvas$image <- addGrob(canvas$image, ghostsGrob
                                (canvas$quartiles[canvas$sampled.stats, q.order[1], j],
                                 canvas$quartiles[canvas$sampled.stats, q.order[2], j],
                                 canvas$quartiles[canvas$sampled.stats, q.order[3], j],
                                 alpha = alpha, box.color = "lightpink",
                                 vp = canvas$graphPath("sample", as.character(n)),
                                 name =  paste("samplePlot.ghosts", n, sep = ".")))
        plotPoints(canvas, x[levels == j],
                   y[levels == j], col = getColour(1:length(ylevels), length(ylevels))[n],
                   vp = canvas$graphPath("sample", as.character(n)),
                   name = "samplePlot")
        plotBoxplot(canvas, x[levels == j],
                    stat = function(x) fivenum(x)[q], stat.color = "blue",
                    vp = canvas$graphPath("sample", as.character(n)),
                    name = "samplePlot")
        canvas$image <- addGrob(canvas$image, boxplotGrob
                                (x[levels == j], box.color = "black",
                                 median.color = "black",
                                 show.w = FALSE, gp = gpar(lwd = 3),
                                 name = paste("samplePlot.boxplot", n, sep = "."),
                                 vp = canvas$graphPath("sample", as.character(n))))
        canvas$image <- addGrob(canvas$image, linesGrob
                                (x = unit(rep(fivenum(x[levels == j])[q], 2),
                                 "native"),
                                 y = unit(c(0.1, 0.55), "npc"),
                                 gp = gpar(lwd = 3, col = "blue"),
                                 vp = canvas$graphPath("sample", as.character(n)),
                                 name = paste("samplePlot", "line", n, sep = ".")))
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
