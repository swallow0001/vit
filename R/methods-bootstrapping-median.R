canvasBootMedian <- setRefClass("canvasBootMedianClass", contains = "canvasPlotClass",
                             methods = list(
    plotSample = function(env, i = which.sample) {
        plotSamplePointsAndBoxplotGhostMedian(.self, env, i)
    },

    showLabels = function() {
        bootLabels(.self)
    },

    calcStat = function(i = which.sample, y = NULL, canvas = .self) {
        calcMedian(samples[[i]], y)
    },

    calcAllStats = function(a, b = NULL, canvas = .self) {
        calcMedian(a, b)
    },

    plotDataStat = function(env, ...) {
        lineOnBoxplotMedian(.self, env)
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
        boot1000median(.self, env, ...)
    },

    fadePlots = function(env, ...) {
        fadeSampleAndStat(.self, env, ...)
    }))

load_bootstrap_median <- function(e) {
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
    tmp.canvas <- canvasBootMedian$new()
    tmp.canvas$import(e$c1)
    e$c1 <- tmp.canvas
    e$c1$viewports <- tmp.vps
    e$c1$image <- tmp.image
    e$difference <- FALSE
}

calcMedian <- function(x, y = NULL)
    median(x)

plotSamplePointsAndBoxplotGhostMedian <- function(canvas, e, i){
    canvas$rmGrobs(c("dataPlot.ci.1", "samplePlot.rect.1"))
    alpha = 0.25
    canvas$sampled.stats <- c(canvas$sampled.stats, canvas$which.sample)
    x <- canvas$samples[[i]]
    if (length(x) >= canvas$hist.cutoff) {
        plotHist(canvas, canvas$samples[[i]], canvas$graphPath("sample"), "samplePlot")
    } else {
        y <- old.stackPoints(x, vp = canvas$graphPath("sample"))
        plotPoints(canvas, x, y, canvas$graphPath("sample"), "samplePlot", black = FALSE)
    }
    canvas$image <- addGrob(canvas$image, ghostsGrob(canvas$quartiles[canvas$sampled.stats, 1],
                                                     canvas$quartiles[canvas$sampled.stats, 2],
                                                     canvas$quartiles[canvas$sampled.stats, 3],
                                                     alpha = alpha, box.color = "lightpink",
                                                     vp = canvas$graphPath("sample"),
                                                     name = "samplePlot.ghosts.1"))
    canvas$image <- addGrob(canvas$image, boxplotGrob(x, box.color = "black",
                                                      median.color = "black",
                                                      show.w = FALSE, gp = gpar(lwd = 3),
                                                      name = "samplePlot.boxplot.1",
                                                      vp = canvas$graphPath("sample")))
    canvas$image <- addGrob(canvas$image, datatextGrob(data = x, title = "Re-sample",
                                                       name = "samplePlot.databox.text.2",
                                                       gp = gpar(col = "red"),
                                                       vp = canvas$graphPath("databox", 2),
                                                       max = 50))
    canvas$image <- addGrob(canvas$image, linesGrob
                            (x = unit(canvas$stat.dist[i], "native"),
                             y = unit(c(0.05, 0.5), "npc"), gp = gpar(lwd = 4, col = "blue"),
                             vp = canvas$graphPath("sample"), name = "samplePlot.lines.1"))
}

lineOnBoxplotMedian <- function(canvas, e){
   plotBoxplot(canvas, canvas$x, stat = median, stat.color = "purple3",
               canvas$graphPath("data"), "dataPlot")
    median.x <- median(canvas$x)
    canvas$image <- addGrob(canvas$image, textGrob
                            (label = format(round(median.x, canvas$dp), nsmall = canvas$dp),
                             x = unit(median.x, "native"),
                             y = unit(0.05, "npc"),
                             just = c("centre", "centre"),
                             gp = gpar(col = "red", fontface = "bold"),
                             name = "dataPlot.stat.text",
                             vp = canvas$graphPath("data")))
}

boot1000median <- function(canvas, e, points = FALSE){
    canvas$rmGrobs(c("samplePlot.databox.text.2", "dataPlot.ci.1"))
    allx <- c(canvas$stat.dist, recursive = TRUE)
    allinfo <- sapply(canvas$samples, function(x) fivenum(x)[2:4])
    y.max <- unit(1, "npc") - unit(2, "lines") - unit(0.5, "char")
    for (i in 20*(1:50)){
        if (points){
            x <- allx[1:i]
            y <- canvas$stat.ypos[1:i]
            plotPoints(canvas, x, y, canvas$graphPath("stat"), "statPlot",
                       black = FALSE, alpha = 0.7)
        }
        xsample <- canvas$samples[[i]]
        if (length(xsample) >= canvas$hist.cutoff) {
            plotHist(canvas, xsample, canvas$graphPath("sample"), "samplePlot")
        } else {
            ysample <- old.stackPoints(xsample, vp = canvas$graphPath("sample"), y.max = y.max)
            plotPoints(canvas, xsample, ysample, canvas$graphPath("sample"), "samplePlot",
                       black = FALSE)
        }
        canvas$image <- addGrob(canvas$image, ghostsGrob(allinfo[1, 1:i], allinfo[2, 1:i],
                                                         allinfo[3, 1:i], alpha = 0.05,
                                                         box.color = "lightpink",
                                                         vp = canvas$graphPath("sample"),
                                                         name = "samplePlot.ghosts.1"))
        canvas$image <- addGrob(canvas$image, boxplotGrob(data = xsample, box.color = "black",
                                                          median.color = "black", stat = median,
                                                          stat.color = "blue",
                                                          show.w = FALSE, gp = gpar(lwd = 3),
                                                          name = "samplePlot.boxplot.1",
                                                          vp = canvas$graphPath("sample")))
        canvas$image <- addGrob(canvas$image, datatextGrob(data = xsample, title = "Re-sample",
                                                           name = "samplePlot.databox.text.2",
                                                           gp = gpar(col = "red"),
                                                           vp = canvas$graphPath("databox", 2),
                                                           max = 50))

        if (canvas$stopAnimation)
            return()
        canvas$showLabels()
        canvas$drawImage()
    }

    canvas$rmGrobs(c("samplePlot.points.1", "samplePlot.boxplot.1",
                     "samplePlot.databox.text.2", "samplePlot.hist.1"))
    canvas$drawImage()
    ## Remove 1000 statistics next time something is plotted to avoid
    ## further statistics being plotted on top.
    #canvas$image <- removeGrob(canvas$image, gPath("statPlot.points.1"))
    ## Reset CI counter
    canvas$sampled.stats <- NULL
    canvas$plotted.stats <- NULL
}
