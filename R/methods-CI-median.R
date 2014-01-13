canvasCIMedian <- setRefClass("canvasCIMedianClass", contains = "canvasPlotClass",
                            methods = list(
    calcStat = function(i = which.sample, y = NULL, canvas = .self) {
        if (stat.method == "bootstrap: percentile") {
            calcCIBootPercMedian(samples[[i]], y)
        } else if (stat.method == "bootstrap: +/- 2 s.e.") {
            calcCIBootSEMedian(samples[[i]], y)
        } else if (stat.method == "bootstrap: +/- t s.e.") {
            calcCIBootTSEMedian(samples[[i]], y)
        }
    },
    
    calcAllStats = function(x, y = NULL, canvas = .self) {
        if (stat.method == "bootstrap: percentile") {
            calcCIBootPercMedian(x, y)
        } else if (stat.method == "bootstrap: +/- 2 s.e.") {
            calcCIBootSEMedian(x, y)
        } else if (stat.method == "bootstrap: +/- t s.e.") {
            calcCIBootTSEMedian(x, y)
        }
    },

    plotSample = function(env, i = which.sample) {
        plotSamplePointsAndBoxplotMedian(.self, env, i)
    },

    showLabels = function() {
        ciLabels(.self)
    },

    plotDataStat = function(env, ...) {
        addMedianLine(.self, env)
    },

    plotSampleStat = function(env, i = which.sample, ...) {
        plotCI(.self, env, i, ...)
    },

    plotStatDist = function(env, ...) {
        plotCIDistMedian(.self, env)
    },

    animateSample = function(...) {
        dropPoints1d(.self, ...)
    },

    animateStat = function(env, n.steps) {
        dropCI(.self, env, n.steps)
    },

    displayResult = function(env, cov.message) {
        CIcounter(.self, env, cov.message, fun = median)
    },

    handle1000 = function(env, ...) {
        ci1000(.self, env, fun = median)
    }))

load_CI_median <- function(e) {
    confidence_check(e)
    e$c1$stat.in.use <- svalue(e$stat)
    e$cimethod <- svalue(e$cimeth)
    e$c1$stat.method <- e$cimethod
    # There is something messy going on with viewports and
    # gTrees, such that when attempting to import them, we
    # end up with "uninitializedField"s. Assign to a temp
    # var and reassign later.
    tmp.vps <- e$c1$viewports
    tmp.image <- e$c1$image
    e$c1$viewports <- NULL
    e$c1$image <- NULL
    tmp.canvas <- canvasCIMedian$new()
    tmp.canvas$import(e$c1)
    e$c1 <- tmp.canvas
    e$c1$viewports <- tmp.vps
    e$c1$image <- tmp.image
    e$results <- NULL
}

plotSamplePointsAndBoxplotMedian <- function(canvas, e, i) {
    canvas$rmGrobs("samplePlot.stat.1")
    x <- canvas$samples[[i]]
    if (length(x) >= 100)
        plotHist(canvas, x, canvas$graphPath("data"), "dataPlot")
    else {
        y <- old.stackPoints(x, vp = canvas$graphPath("sample"))
        plotPoints(canvas, x, y, canvas$graphPath("sample"), "samplePlot", black = TRUE)
        plotBoxplot(canvas, x, stat = median, stat.color = "blue", canvas$graphPath("sample"),
                    "samplePlot")
    }
}

calcCIBootPercMedian <- function(x, y = NULL){
    n <- length(x)
    nboots <- 999
    samps <- matrix(sample(x, size = nboots*n, replace = TRUE), nrow = nboots,
                    ncol = n)
    medians <- apply(samps, 1, median)
    quantile(medians, prob = c(0.025, 0.975), type = 1)
}

calcCIBootSEMedian <- function(x, y = NULL){
    n <- length(x)
    nboots <- 1000
    samps <- matrix(sample(x, size = nboots*n, replace = TRUE), nrow = nboots,
                 ncol = n)
    medians <- apply(samps, 1, median)
    se <- sd(medians)
    median(x) + c(-1, 1) * 2 * se
}

calcCIBootTSEMedian <- function(x, y = NULL){
    n <- length(x)
    nboots <- 1000
    samps <- matrix(sample(x, size = nboots*n, replace = TRUE), nrow = nboots,
                    ncol = n)
    medians <- apply(samps, 1, median)
    se <- sd(medians)
    median(x) + c(-1, 1) * qt(0.975, n - 1) * se
}

addMedianLine <- function(canvas, e) {
    x <- median(canvas$x)
    canvas$image <- addGrob(canvas$image,
                            segmentsGrob(x0 = x, x1 = x, y0 = 0,
                                         y1 = 3, default.units = "native",
                                         gp = gpar(col = "grey60", lty = "dashed"),
                                         vp = canvas$graphPath("animation.field"),
                                         name = "hline"))
        canvas$y <- old.stackPoints(canvas$x, vp = canvas$graphPath("data"))
    if (length(canvas$x) >= 1000)
        plotHist(canvas, canvas$x, canvas$graphPath("data"), "dataPlot") else {
        plotPoints(canvas, canvas$x, canvas$y, canvas$graphPath("data"), "dataPlot")
        plotBoxplot(canvas, canvas$x, stat = median, stat.color = "purple3",
                    canvas$graphPath("data"), "dataPlot")
    }
}

plotCIDistMedian <- function(canvas, e) {
    i <- canvas$which.sample
    bounds <- canvas$getStat(i)
    x <- mean(bounds)
    X <- median(canvas$x)
    if (X >= bounds[1] & X <= bounds[2]) color <- "green" else color <- "red"
    current <- data.frame(x = x, width = diff(c(bounds)), color = color)

    if ("statPlot.stat.dist" %in% childNames(canvas$image)) {
        dist.grob <- getGrob(canvas$image, gPath(c("statPlot.stat.dist")))
        dist.df <- dist.grob$data
        if (nrow(dist.df) >= 40) dist.df <- dist.df[-1,]
        dist.df <- rbind(dist.df[, -4], current)
    } else dist.df <- current

    dist.df$y <- 0.02 * 1:nrow(dist.df)
    green <- dist.df[dist.df$color == "green",]
    red <- dist.df[dist.df$color == "red",]

    if (nrow(green) > 0) {
        greenRects <- rectGrob(x = unit(green$x, "native"),
                               y = unit(green$y, "native"), width = unit(green$width, "native"),
                               height = unit(0.015, "native"), vp = canvas$graphPath("stat"),
                               gp = gpar(col = NA, fill = "green"))
    } else greenRects <- NULL

    if (nrow(red) > 0) {
        redRects <- rectGrob(x = unit(red$x, "native"),
                             y = unit(red$y, "native"), width = unit(red$width, "native"),
                             height = unit(0.015, "native"), vp = canvas$graphPath("stat"),
                             gp = gpar(col = NA, fill = "red"))
    } else redRects <- NULL

    new.dist <- gTree(data = dist.df, name = "statPlot.stat.dist",
                      childrenvp = canvas$viewports, children = gList(greenRects, redRects))

    canvas$image <- addGrob(canvas$image, new.dist)
}


