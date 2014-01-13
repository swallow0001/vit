canvasCIMean <- setRefClass("canvasCIMeanClass", contains = "canvasPlotClass",
                            methods = list(
    calcStat = function(i = which.sample, y = NULL, canvas = .self) {
        if (stat.method == "normal: +/- t s.e.") {
            calcCITWald(samples[[i]], y)
        } else if (stat.method == "normal: +/- 2 s.e.") {
            calcCI2Wald(samples[[i]], y)
        } else if (stat.method == "bootstrap: percentile") {
            calcCIBootPercMean(samples[[i]], y)
        } else if (stat.method == "bootstrap: +/- 2 s.e.") {
            calcCIBootSEMean(samples[[i]], y)
        } else if (stat.method == "bootstrap: +/- t s.e.") {
            calcCIBootTSEMean(samples[[i]], y)
        }
    },
    
    calcAllStats = function(x, y = NULL, canvas = .self) {
        if (stat.method == "normal: +/- t s.e.") {
            calcCITWald(x, y)
        } else if (stat.method == "normal: +/- 2 s.e.") {
            calcCI2Wald(x, y)
        } else if (stat.method == "bootstrap: percentile") {
            calcCIBootPercMean(x, y)
        } else if (stat.method == "bootstrap: +/- 2 s.e.") {
            calcCIBootSEMean(x, y)
        } else if (stat.method == "bootstrap: +/- t s.e.") {
            calcCIBootTSEMean(x, y)
        }
    },

    plotSample = function(env, i = which.sample) {
        plotSamplePointsAndBoxplotMean(.self, env, i)
    },

    showLabels = function() {
        ciLabels(.self)
    },

    plotDataStat = function(env, ...) {
        addMeanLine(.self, env)
    },

    plotSampleStat = function(env, i = which.sample, ...) {
        plotCI(.self, env, i, ...)
    },

    plotStatDist = function(env, ...) {
        plotCIDistMean(.self, env)
    },

    animateSample = function(...) {
        dropPoints1d(.self, ...)
    },

    animateStat = function(env, n.steps) {
        dropCI(.self, env, n.steps)
    },

    displayResult = function(env, cov.message) {
        CIcounter(.self, env, cov.message, fun = mean)
    },

    handle1000 = function(env, ...) {
        ci1000(.self, env, ...)
    }))

load_CI_mean <- function(e) {
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
    tmp.canvas <- canvasCIMean$new()
    tmp.canvas$import(e$c1)
    e$c1 <- tmp.canvas
    e$c1$viewports <- tmp.vps
    e$c1$image <- tmp.image
    e$results <- NULL
}

plotSamplePointsAndBoxplotMean <- function(canvas, e, i) {
    canvas$rmGrobs("samplePlot.stat.1")
    x <- canvas$samples[[i]]
    if (length(x) >= 5000)
        plotHist(canvas, x, canvas$graphPath("sample"), "samplePlot")
    else {
        y <- old.stackPoints(x, vp = canvas$graphPath("sample"))
        plotPoints(canvas, x, y, canvas$graphPath("sample"), "samplePlot", black = TRUE)
        plotBoxplot(canvas, x, stat = mean, stat.color = "blue", canvas$graphPath("sample"),
                    "samplePlot")
    }
}

ciLabels <- function(canvas){
    poplabel <- textGrob("Population",
                         x = unit(0, "npc") + unit(1, "mm"),
                         y = unit(1, "npc") - unit(1, "lines"),
                         just = "left",
                         name = "dataLabel",
                         vp = canvas$graphPath("data"),
                         gp = gpar(fontface = 2))
    methlabel <- textGrob("Module: CI Coverage",
                          x = unit(0, "npc"),
                          just = "left",
                          name = "methodLabel",
                          gp = gpar(fontsize = 10, fontface = "italic"),
                          vp = canvas$graphPath("canvas.header"))
    if (is.categorical(canvas$x)) {
        vlabels <- c("Variable: ", canvas$x.name, " (",
                     canvas$loi, " | ",
                     canvas$loi.alt, ")")
        vlabelXs <- unit(0, "npc")
        for (i in 1:(length(vlabels) - 1))
            vlabelXs <- unit.c(vlabelXs, vlabelXs[i] + stringWidth(vlabels[i]))
        varlabel <- textGrob(vlabels,
                             x = vlabelXs + stringWidth(methlabel$label) + unit(6, "mm"),
                             just = "left",
                             gp = gpar(col = c(rep("black", 3), "blue", "black", "red", "black"),
                                       fontsize = 10, fontface = "italic"),
                             name = "varLabel",
                             vp = canvas$graphPath("canvas.header"))
    } else {
        varlabel <- textGrob(paste("Variable:", canvas$x.name),
                             x = unit(0, "npc") + stringWidth(methlabel$label) + unit(6, "mm"),
                             just = "left",
                             gp = gpar(fontsize = 10, fontface = "italic"),
                             name = "varLabel",
                             vp = canvas$graphPath("canvas.header"))
    }
    quantitylabel <- textGrob(paste("Quantity:", canvas$stat.in.use),
                              x = varlabel$x[1] + stringWidth(paste(varlabel$label, collapse = "")) + unit(6, "mm"),
                              just = "left",
                              name = "quantityLabel",
                              gp = gpar(fontsize = 10, fontface = "italic"),
                              vp = canvas$graphPath("canvas.header"))
    cimethodlabel <- textGrob(paste("CI Method:", canvas$stat.method),
                              x = quantitylabel$x + stringWidth(quantitylabel$label) + unit(6, "mm"),
                              just = "left", name = "cimethodlabel", gp = gpar(fontsize = 10, fontface = "italic"),
                              vp = canvas$graphPath("canvas.header"))
    filelabel <- textGrob(paste("File:", canvas$data.file),
                          x = cimethodlabel$x + stringWidth(cimethodlabel$label) + unit(6, "mm"),
                          just = "left",
                          name = "fileLabel",
                          gp = gpar(fontsize = 10, fontface = "italic"),
                          vp = canvas$graphPath("canvas.header"))
    infosep <- linesGrob(x = unit(0:1, "npc"), y = unit(0, "npc"),
                         name = "infoSeparatorLine",
                         vp = canvas$graphPath("canvas.header"))
    samplabel <- textGrob("Sample",
                          x = unit(0, "npc") + unit(1, "mm"),
                          y = unit(0.8, "npc"),
                          just = c("left", "top"),
                          name = "sampleLabel",
                          vp = canvas$graphPath("sample"),
                          gp = gpar(fontface = 2))
    statlabel <- textGrob("CI history",
                          x = unit(0, "npc") + unit(1, "mm"),
                          y = unit(0.8, "npc"),
                          just = c("left", "top"),
                          name = "statLabel",
                          vp = canvas$graphPath("stat"),
                          gp = gpar(fontface = 2))
    cilabels <- grobTree(methlabel, varlabel, quantitylabel, cimethodlabel, filelabel,
                         infosep,
                         poplabel, samplabel, statlabel,
                         name = "cilabels")
    canvas$image <- addGrob(canvas$image, cilabels)
}


#' the various confidence coverage methods for CALC_STAT
#' note that the percentile bootstrap methods do not perform very well. Here's the speed of makeStatDistribution when using different version of them on an original data of size 80
# colMean(samps)
#   user  system elapsed
# 40.614   2.822  43.669

# apply(samps, 1, mean)
#   user  system elapsed
# 40.958   2.851  44.828

# apply(samps, 1, median)
#    user  system elapsed
#129.160   3.507 133.886
calcCITWald <- function(x, y = NULL){
    n <- length(x)
    se <- sd(x)/sqrt(n)
    mean(x) + c(-1, 1)*qt(0.975, n - 1)*se
}

calcCI2Wald <- function(x, y = NULL){
    n <- length(x)
    se <- sd(x)/sqrt(n)
    mean(x) + c(-2, 2)*se
}

calcCIBootPercMean <- function(x, y = NULL){
    n <- length(x)
    nboots <- 999
    samps <- matrix(sample(x, size = nboots*n, replace = TRUE),
                    nrow = nboots, ncol = n)
    means <- rowMeans(samps)
    quantile(means, prob = c(0.025, 0.975), type = 1)
}

calcCIBootSEMean <- function(x, y = NULL){
    n <- length(x)
    nboots <- 1000
    samps <- matrix(sample(x, size = nboots*n, replace = TRUE),
                    nrow = nboots, ncol = n)
    means <- rowMeans(samps)
    se <- sd(means)
    mean(x) + c(-1, 1) * 2 * se
}

calcCIBootTSEMean <- function(x, y = NULL){
    n <- length(x)
    nboots <- 1000
    samps <- matrix(sample(x, size = nboots*n, replace = TRUE), nrow = nboots,
                    ncol = n)
    means <- rowMeans(samps)
    se <- sd(means)
    mean(x) + c(-1, 1) * qt(0.975, n - 1) * se
}

addMeanLine <- function(canvas, e) {
    x <- mean(canvas$x)
    canvas$image <- addGrob(canvas$image,
                            segmentsGrob(x0 = x, x1 = x, y0 = 0,
                                         y1 = 3, default.units = "native",
                                         gp = gpar(col = "grey60", lty = "dashed"),
                                         vp = canvas$graphPath("animation.field"),
                                         name = "hline"))
    canvas$y <- old.stackPoints(canvas$x, vp = canvas$graphPath("data"))
    if (length(canvas$x) >= 5000)
        plotHist(canvas, canvas$x, canvas$graphPath("data"), "dataPlot")
    else {
        plotPoints(canvas, canvas$x, canvas$y, canvas$graphPath("data"), "dataPlot")
        plotBoxplot(canvas, canvas$x, stat = mean, stat.color = "purple3", canvas$graphPath("data"),
                    "dataPlot")
    }
}

plotCI <- function(canvas, e, i, pause = FALSE) {
    if (pause){
        method <- strsplit(svalue(e$cimeth), ":")[[1]][1]
        ciCalcLabel <- textGrob(paste("Calculating", method, "CI..."),
                                x = unit(0.5, "npc"), y = unit(0.6, "npc"),
                                just = c("centre", "top"), vp = canvas$graphPath("sample"),
                                gp = gpar(fontface = 2), name = "samplePlot.ciCalcLabel")
        canvas$image <- addGrob(canvas$image, ciCalcLabel)
        if (canvas$stopAnimation)
            return()
        canvas$pauseImage(15)
        canvas$rmGrobs("samplePlot.ciCalcLabel")
    }
    bounds <- canvas$stat.dist[[i]]
    x <- mean(bounds)
    canvas$image <- addGrob(canvas$image,
                            rectGrob(x = unit(x, "native"), y = unit(0.2, "native"),
                                     width = unit(diff(bounds), "native"),
                                     height = unit(0.015, "native"),
                                     gp = gpar(col = "#FF7F00",
                                     fill = "#FF7F00"), vp = canvas$graphPath("sample"),
                                     name = "samplePlot.stat.1"))
}

plotCIDistMean <- function(canvas, e) {
    i <- canvas$which.sample
    bounds <- canvas$getStat(i)
    x <- mean(bounds)
    X <- mean(canvas$x)
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

#' Animates a sample of points dropping down from the collection of points in the data window. The ANIMATE_SAMPLE method for numeric, one dimensional data.
dropPoints1d <- function(canvas, n.steps, n.slow, keep.plot, move = TRUE) {
    canvas$rmGrobs(c("samplePlot.points.1", "samplePlot.points"))
    if (!keep.plot){
        canvas$rmGrobs(c("samplePlot.boxplot.1", "samplePlot.boxplot", "samplePlot.stat.1"))
    }
    index <- canvas$indexes[[canvas$which.sample]]
    x <- canvas$x[index]
    y.start <- y.pos <- canvas$y[index] + 2 # to place in data vp
    y.end <- old.stackPoints(x, vp = canvas$graphPath("sample")) + 1
    y.step <- (y.start - y.end)/n.steps
    n.slow <- min(n.slow, length(x))
    ## Lighting up of sampled points.
    if (move){
        sampSelectLabel <- textGrob("Selecting sample...", x = unit(0.5, "npc"), y = unit(0.6, "npc"),
                                just = c("centre", "top"), vp = canvas$graphPath("sample"),
                                gp = gpar(fontface = 2), name = "samplePlot.sampSelectLabel")
        canvas$image <- addGrob(canvas$image, sampSelectLabel)
        for (i in 1:length(x)){
            canvas$image <- addGrob(canvas$image,
                                    pointsGrob(x[1:i], y = (canvas$y[index])[1:i],
                                               vp = canvas$graphPath("data"),
                                               pch = 19,
                                               name = "samplePlot.data.samp"))
            if (i <= n.slow) speed = 10 else speed = 1
            if (canvas$stopAnimation)
                return()
            canvas$pauseImage(speed)
        }
        ## Force pause before points drop.
        if (canvas$stopAnimation)
            return()
        canvas$pauseImage(20)
    }
    canvas$image <- addGrob(canvas$image,
                            pointsGrob(x, y = canvas$y[index], vp = canvas$graphPath("data"),
                                       pch = 19,
                                       name = "samplePlot.data.samp"))
    ## Dropping of points.
    if (move){
        for (i in 1:n.steps){
            y.pos <- y.pos - y.step
            canvas$image <- addGrob(canvas$image,
                                    pointsGrob(x, y.pos, vp = canvas$graphPath("animation.field"),
                                               pch = 19, name = "samplePlot.temp"))
            if (canvas$stopAnimation)
                return()
            canvas$drawImage()
        }
        canvas$rmGrobs(c("samplePlot.sampSelectLabel", "samplePlot.temp"))
    }
}


#' confidence coverage method for ANIMATE_STAT
dropCI <- function(canvas, e, n.steps) {
    canvas$drawImage()
    stat.grob <- getGrob(canvas$image, gPath(c("samplePlot.stat.1")))
    grob.width <- stat.grob$width
    grob.x <- stat.grob$x

    y.start <- if (is.categorical(canvas$x)) 1 else 1.2
    y.end <- .02 * min(length(canvas$sampled.stats) + 1, 41)

    step <- (y.start - y.end)/n.steps

    for (i in 1:n.steps) {
        canvas$image <- addGrob(canvas$image,
                                rectGrob(x = grob.x,
                                         y = unit(y.start - i * step, "native"),
                                         width = grob.width,
                                         height = unit(0.015, "native"),
                                         gp = gpar(col = "#FF7F00",
                                         fill = "#FF7F00"),
                                         vp = canvas$graphPath("animation.field"),
                                         name = "statPlot.moving.stat"))
        if (canvas$stopAnimation)
            return()
        canvas$drawImage()
    }
    canvas$pauseImage(10)
    canvas$rmGrobs("statPlot.moving.stat")
}

##' confidence coverage method for DISPLAY_RESULT
CIcounter <- function(canvas, env, cov.message = TRUE, fun = mean) {
    if (is.null(env$results)) {
        bounds <- do.call("rbind", canvas$stat.dist)
        #X <- mean(canvas$calcAllStats(canvas$x))
        if (is.categorical(canvas$x)){
            X <- mean(canvas$x == canvas$loi)
        } else {
             X <- fun(canvas$x)
        }
        env$results <- X >= bounds[,1] & X <= bounds[,2]
    }
    canvas$sampled.stats <- c(canvas$sampled.stats, canvas$which.sample)
    total <- length(canvas$sampled.stats)
    success <- sum(env$results[canvas$sampled.stats])
    xunit <- unit(0, "npc") + 0.5*stringWidth("1000 of 1000") + unit(2, "mm")
    ## Using the above xunit doesn't work for countertext3, as this
    ## does not have a cex of 1. Conversion prevents this from being
    ## an issue.
    xunit <- convertX(xunit, "cm")
    countertext1 <- textGrob("Coverage:", x = xunit, y = unit(0.5, "npc"),
                             vp = canvas$graphPath("stat"), gp = gpar(fontface = 2),
                             name = "countertext1")
    countertext2 <- textGrob(paste(success, "of", total), x = xunit,
                             y = unit(0.5, "npc") - unit(1, "lines"),
                             vp = canvas$graphPath("stat"), name = "countertext2")
    countertext3 <- textGrob(paste(round(success/total*100, 1), "%"),
                             x = xunit, y = unit(0.5, "npc") - unit(2/1.3, "lines"),
                             vp = canvas$graphPath("stat"), gp = gpar(fontface = 2, cex = 1.3),
                             name = "countertext3")
    counterborder <- rectGrob(x = xunit, y = unit(0.5, "npc") + unit(0.5, "lines"),
                              width = stringWidth("1000 of 1000"),
                              height = unit(2, "mm") + unit(3, "lines"),
                              gp = gpar(fill = "white"),
                              just = c("centre", "top"), vp = canvas$graphPath("stat"),
                              name = "counterborder")
    countertext <- grobTree(counterborder, countertext1, countertext2, countertext3,
                            name = "statPlot.countertext")
    canvas$image <- addGrob(canvas$image, countertext)
    if (cov.message){
        bounds <- canvas$getStat(canvas$which.sample)
        x <- mean(bounds)
        if (is.categorical(canvas$x)){
            X <- mean(canvas$x == canvas$loi)
        } else {
            X <- fun(canvas$x)
        }
        if (X >= bounds[1] & X <= bounds[2]) color <- "green" else color <- "red"
        text <- if (color == "green") "Covered" else "Missed"
        xunit <- unit(1, "npc") - 0.5*stringWidth("true value") - unit(5, "mm")
        messagetext1 <- textGrob(text, x = xunit, y = unit(0.5, "npc") - unit(0.5, "lines"),
                                 vp = canvas$graphPath("stat"), name = "messagetext1",
                                 gp = gpar(col = color))
        messagetext2 <- textGrob("true value", x = xunit,
                                 y = unit(0.5, "npc") - unit(1.5, "lines"),
                                 vp = canvas$graphPath("stat"), name = "messagetext2")
        messageborder <- rectGrob(x = xunit, y = unit(0.5, "npc"),
                                  width = unit(2, "mm") + stringWidth("true value"),
                                  height = unit(2, "mm") + unit(2, "lines"),
                                  gp = gpar(fill = "white"),
                                  just = c("centre", "top"), vp = canvas$graphPath("stat"),
                                  name = "messageborder")
        countertext <- grobTree(messageborder, messagetext1, messagetext2,
                                name = "statPlot.messagetext")
        canvas$image <- addGrob(canvas$image, countertext)
        if (canvas$stopAnimation)
            return()
        canvas$pauseImage(15)
        canvas$rmGrobs("statPlot.messagetext")
    }
}


#' confidence coverage method for HANDLE_1000: how to display the results of 1000 bootstrap samples
ci1000 <- function(canvas, e, fun = mean){
    canvas$rmGrobs(c("samplePlot.points.1", "samplePlot.boxplot.1", "samplePlot.data.samp"))
    if (canvas$which.sample >= 900) canvas$which.sample <- round(runif(1, 0, 100))
    bounds <- do.call("rbind", canvas$stat.dist)
    #X <- mean(canvas$calcAllStats(canvas$x))
    X <- if (! is.null(fun)) fun(canvas$x)
         else canvas$calcAllStats(canvas$x, NULL, canvas)
    e$results <- X >= bounds[,1] & X <= bounds[,2]
    ## Overall coverage percentage
    totperc <- mean(!e$results)
    ## Required 'red' CIs for final display of 40 CIs.
    noreq <- ceiling(40*totperc)
    if (noreq > 0){
        plotted.index <- (canvas$which.sample + 61):(canvas$which.sample + 100)
        plotted.samples <- canvas$samples[plotted.index]
        diff <- sum(!e$results[plotted.index]) - noreq
        if (diff != 0) {
            if (diff > 0) {
                index.changeout <- sample(which(!e$results[plotted.index]),
                                          size = diff) + canvas$which.sample + 60
                index.changein <- sample(which(e$results), size = diff)
            } else {
                index.changeout <- sample(which(e$results[plotted.index]),
                                          size = abs(diff)) + canvas$which.sample + 60
                index.changein <- sample(which(!e$results), size = abs(diff))
            }
            for (i in 1:abs(diff)){
                samples.changeout <- canvas$samples[[index.changeout[i]]]
                canvas$samples[[index.changeout[i]]] <- canvas$samples[[index.changein[i]]]
                canvas$samples[[index.changein[i]]] <- samples.changeout
                statdist.changeout <- canvas$stat.dist[[index.changeout[i]]]
                canvas$stat.dist[[index.changeout[i]]] <- canvas$stat.dist[[index.changein[i]]]
                canvas$stat.dist[[index.changein[i]]] <- statdist.changeout
            }
        }
    }
    ## If running out of samples, select a random starting point in first 100.
    for (j in c(seq(1 , 1000, by = 10), 1000)) {
        canvas$plotSample(e)
        canvas$plotSampleStat(e)
        if (! is.categorical(canvas$x)) {
            index <- canvas$indexes[[canvas$which.sample]]
            canvas$image <- addGrob(canvas$image,
                            pointsGrob(canvas$x[index],
                                       y = canvas$y[index],
                                       vp = canvas$graphPath("data"),
                                       pch = 19,
                                       name = "samplePlot.data.samp"))
        }
        canvas$plotStatDist(e)
        canvas$advanceWhichSample()
        success <- sum(e$results[1:j])
        xunit <- unit(0, "npc") + 0.5*stringWidth("1000 of 1000") + unit(2, "mm")
        ## Using the above xunit doesn't work for countertext3, as this
        ## does not have a cex of 1. Conversion prevents this from being
        ## an issue.
        xunit <- convertX(xunit, "cm")
        countertext1 <- textGrob("Coverage:", x = xunit, y = unit(0.5, "npc"),
                                 vp = canvas$graphPath("stat"), gp = gpar(fontface = 2),
                                 name = "countertext1")
        countertext2 <- textGrob(paste(success, "of", j), x = xunit,
                                 y = unit(0.5, "npc") - unit(1, "lines"),
                                 vp = canvas$graphPath("stat"), name = "countertext2")
        countertext3 <- textGrob(paste(round(success/j*100, 1), "%"),
                                 x = xunit, y = unit(0.5, "npc") - unit(2/1.3, "lines"),
                                 vp = canvas$graphPath("stat"), gp = gpar(fontface = 2, cex = 1.3),
                                 name = "countertext3")
        counterborder <- rectGrob(x = xunit, y = unit(0.5, "npc") + unit(0.5, "lines"),
                                  width = stringWidth("1000 of 1000"),
                                  height = unit(2, "mm") + unit(3, "lines"),
                                  gp = gpar(fill = "white"),
                                  just = c("centre", "top"), vp = canvas$graphPath("stat"),
                                  name = "counterborder")
        countertext <- grobTree(counterborder, countertext1, countertext2, countertext3,
                                name = "statPlot.countertext")
        canvas$image <- addGrob(canvas$image, countertext)
        canvas$showLabels()
        if (canvas$stopAnimation)
            return()
        canvas$drawImage()
    }
    ## Move 1000 CIs next time something is plotted to avoid further CIs getting plotted on top.
    canvas$rmGrobs(c("statPlot.stat.dist", "statPlot.countertext"))
    ## Reset CI counter
    canvas$sampled.stats <- NULL
}



