canvasBootUQ <- setRefClass("canvasBootUQClass", contains = "canvasPlotClass",
                            methods = list(
    plotSample = function(env, i = which.sample) {
        plotSamplePointsAndBoxplotGhostQuartile(.self, env, i, q = 4)
    },

    showLabels = function() {
        bootLabels(.self)
    },

    calcStat = function(i = which.sample, y = NULL, canvas = .self) {
        calcQuartile(samples[[i]], y, q = 4)
    },

    calcAllStats = function(a, b = NULL, canvas = .self) {
        calcQuartile(a, b, q = 4)
    },

    plotDataStat = function(env, ...) {
        lineOnBoxplotQuartile(.self, env, q = 4)
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
        boot1000Quartile(.self, env, q = 4, ...)
    },

    fadePlots = function(env, ...) {
        fadeSampleAndStat(.self, env, ...)
    }))

load_bootstrap_UQ <- function(e) {
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
    tmp.canvas <- canvasBootUQ$new()
    tmp.canvas$import(e$c1)
    e$c1 <- tmp.canvas
    e$c1$viewports <- tmp.vps
    e$c1$image <- tmp.image
    e$difference <- FALSE
}

calcQuartile <- function(x, y = NULL, q = 4){
    fivenum(x)[q]
}

plotSamplePointsAndBoxplotGhostQuartile <- function(canvas, e, i, q = 4){
    canvas$rmGrobs(c("dataPlot.ci.1", "samplePlot.rect.1"))
    alpha = 0.25
    canvas$sampled.stats <- c(canvas$sampled.stats, canvas$which.sample)
    x <- canvas$samples[[i]]
    if (length(x) >= canvas$hist.cutoff) {
        plotHist(canvas, x, canvas$graphPath("sample"), "samplePlot")
    } else {
        y <- old.stackPoints(x, vp = canvas$graphPath("sample"))
        plotPoints(canvas, x, y, canvas$graphPath("sample"), "samplePlot", black = FALSE)
    }
    allinfo <- c(canvas$stat.dist, recursive = TRUE)
    ## Need to change ghostsGrob function to work with quartiles.
    ## canvas$image <- addGrob(canvas$image, ghostsGrob(canvas$quartiles[canvas$sampled.stats, 1],
    ##                                                  canvas$quartiles[canvas$sampled.stats, 2],
    ##                                                  canvas$quartiles[canvas$sampled.stats, 3],
    ##                                                  alpha = alpha, box.color = "lightpink",
    ##                                                  vp = canvas$graphPath("sample"),
    ##                                                  name = "samplePlot.ghosts.1"))
    canvas$image <- addGrob(canvas$image, rectGrob
                            (x = unit(allinfo[canvas$sampled.stats], "native"),
                             y = unit(0.15, "npc"), height = unit(0.2, "npc"),
                             width = 0, gp = gpar(alpha = alpha, col = "blue", lwd = 2),
                             vp = canvas$graphPath("sample"), name = "samplePlot.ghosts.1"))
    canvas$image <- addGrob(canvas$image, boxplotGrob(x, box.color = "black",
                                                      median.color = "black",
                                                      show.w = FALSE, gp = gpar(lwd = 3),
                                                      name = "samplePlot.boxplot.1",
                                                      vp = canvas$graphPath("sample")))
    canvas$image <- addGrob(canvas$image, datatextGrob(data = x, title = "Re-sample",
                                                       name = "samplePlot.databox.text.2",
                                                       gp = gpar(col = "red"),
                                                       vp = canvas$graphPath("databox", 2)))
    canvas$image <- addGrob(canvas$image, linesGrob
                            (x = unit(canvas$stat.dist[i], "native"),
                             y = unit(c(0.05, 0.5), "npc"), gp = gpar(lwd = 4, col = "blue"),
                             vp = canvas$graphPath("sample"), name = "samplePlot.lines.1"))
}

lineOnBoxplotQuartile <- function(canvas, e, q = 4){
    plotBoxplot(canvas, canvas$x, stat = function(x) fivenum(x)[q], stat.color = "purple3",
                canvas$graphPath("data"), "dataPlot")
    box.q <- fivenum(canvas$x)[q]
    canvas$image <- addGrob(canvas$image, textGrob
                            (label = format(round(box.q, canvas$dp), nsmall = canvas$dp),
                             x = unit(box.q, "native"),
                             y = unit(0.025, "npc"),
                             just = c("centre", "bottom"),
                             gp = gpar(lwd = 2, col = "red", fontface = "bold"),
                             vp = canvas$graphPath("data", 1),
                             name = "dataPlot.stat.text"))
}

boot1000Quartile <- function(canvas, e, points = FALSE, q = 4){
    canvas$rmGrobs(c("samplePlot.databox.text.2", "dataPlot.ci.1"))
    allx <- c(canvas$stat.dist, recursive = TRUE)
    allinfo <- sapply(canvas$samples, function(x) fivenum(x)[2:4])
    y.max <- unit(1, "npc") - unit(2, "lines") - unit(0.5, "char")
    for (i in 20*(1:50)){
        x <- allx[1:i]
        xsample <- canvas$samples[[i]]
        if (length(xsample) >= canvas$hist.cutoff) {
            plotHist(canvas, xsample, canvas$graphPath("sample"), "samplePlot")
        } else {
            ysample <- old.stackPoints(xsample, vp = canvas$graphPath("sample"), y.max = y.max)
            plotPoints(canvas, xsample, ysample, canvas$graphPath("sample"), "samplePlot",
                       black = FALSE)
        }
        if (points) {
            y <- canvas$stat.ypos[1:i]
            plotPoints(canvas, x, y, canvas$graphPath("stat"), "statPlot",
                       black = FALSE, alpha = 0.7)
        }
        ## Need to change ghostsGrob function to work with quartiles.
        ## canvas$image <- addGrob(canvas$image, ghostsGrob(allinfo[1, 1:i], allinfo[2, 1:i],
        ##                                                  allinfo[3, 1:i], alpha = 0.05,
        ##                                                  box.color = "lightpink",
        ##                                                  vp = canvas$graphPath("sample"),
        ##                                                  name = "samplePlot.ghosts.1"))
        canvas$image <- addGrob(canvas$image, rectGrob
                                (x = unit(x, "native"),
                                 y = unit(0.15, "npc"), height = unit(0.2, "npc"),
                                 width = 0, gp = gpar(alpha = 0.05, col = "blue", lwd = 2),
                                 vp = canvas$graphPath("sample"), name = "samplePlot.ghosts.1"))
        canvas$image <- addGrob(canvas$image, boxplotGrob(data = xsample, box.color = "black",
                                                          median.color = "black",
                                                          stat = function(x) fivenum(x)[q],
                                                          stat.color = "blue",
                                                          show.w = FALSE, gp = gpar(lwd = 3),
                                                          name = "samplePlot.boxplot.1",
                                                          vp = canvas$graphPath("sample")))
        canvas$image <- addGrob(canvas$image, datatextGrob(data = xsample, title = "Re-sample",
                                                           name = "samplePlot.databox.text.2",
                                                           gp = gpar(col = "red"),
                                                           vp = canvas$graphPath("databox", 2)))

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
