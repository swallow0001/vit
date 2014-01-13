canvasSampvarMean <- setRefClass("canvasSampvarMeanClass", contains = "canvasPlotClass",
                                 methods = list(
    calcStat = function(i = which.sample, y = NULL, canvas = .self) {
        calcMean(samples[[i]], y)
    },

    calcAllStats = function(a, b = NULL, canvas = .self) {
        calcMean(a, b)
    },
    plotSample = function(env, i = which.sample) {
        plotSampvarBoxplotGhostMean(.self, i, env)
    },

    showLabels = function() {
        sampvarLabels(.self)
    },

    fadePlots = function(env, ...) {
        fadeData(.self, env)
    },

    plotDataStat = function(env, ...) {
        addStatLine(.self, env)
    },

    plotStatDist = function(env, ...) {
        plotBootDist(.self, env)
    },

    animateSample = function(env, n.steps, n.slow, opts) {
        dropSampvarPoints1d(.self, env, n.steps, n.slow,
                            keep.plot = opts$keep.plot, move = opts$move)
    },

    animateStat = function(env, n.steps) {
        dropStat(.self, env, n.steps)
    },

    handle1000 = function(env, ...) {
        boot1000mean(.self, env, ...)
    },

    displayResult = function(env, ...) {
        plotTheoDist(env)
    }))

load_sampvar_mean <- function(e) {
    e$c1$stat.in.use <- svalue(e$stat)
    e$sampvar.method <- ""
    # There is something messy going on with viewports and
    # gTrees, such that when attempting to import them, we
    # end up with "uninitializedField"s. Assign to a temp
    # var and reassign later.
    tmp.vps <- e$c1$viewports
    tmp.image <- e$c1$image
    e$c1$viewports <- NULL
    e$c1$image <- NULL
    tmp.canvas <- canvasSampvarMean$new()
    tmp.canvas$import(e$c1)
    e$c1 <- tmp.canvas
    e$c1$viewports <- tmp.vps
    e$c1$image <- tmp.image
    e$difference <- FALSE
}

addStatLine <- function(canvas, e, fun = mean) {
    x <- fun(canvas$x)
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
        plotBoxplot(canvas, canvas$x, stat = fun, stat.color = "purple3", canvas$graphPath("data"),
                    "dataPlot")
        canvas$image <- addGrob(canvas$image, textGrob
                                (label = format(round(x, canvas$dp), nsmall = canvas$dp),
                                 x = unit(x, "native"),
                                 y = unit(0.05, "npc"),
                                 gp = gpar(fontface = "bold", col = "red"),
                                 vp = canvas$graphPath("data"),
                                 name = "dataPlot.stat.text"))
    }
}

dropSampvarPoints1d <- function(canvas, e, n.steps, n.slow, keep.plot, move = TRUE, pause = 10) {
    canvas$rmGrobs(c("samplePlot.datapoints.points.1", "samplePlot.data.samp",
                     "samplePlot.points.1", "statPlot.theodist.1",
                     "samplePlot.boxplot.1", "samplePlot.lines.1"))
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
            if (!e$fade){
                canvas$image <- addGrob(canvas$image, getGrob(canvas$image, gPath("fadebox")))
                canvas$image <- addGrob(canvas$image, getGrob(canvas$image, gPath("sampvarlabels")))
                canvas$image <- addGrob(canvas$image, getGrob(canvas$image, gPath("dataAxis")))
            }
            canvas$pauseImage(speed)
        }
        ## Force pause before points drop.
        if (canvas$stopAnimation)
            return()
        if (!e$fade){
            canvas$image <- addGrob(canvas$image, getGrob(canvas$image, gPath("fadebox")))
            canvas$image <- addGrob(canvas$image, getGrob(canvas$image, gPath("sampvarlabels")))
            canvas$image <- addGrob(canvas$image, getGrob(canvas$image, gPath("dataAxis")))
        }
        canvas$pauseImage(20)
    }
    canvas$image <- addGrob(canvas$image,
                            pointsGrob(x, y = canvas$y[index], vp = canvas$graphPath("data"),
                                       pch = 19,
                                       name = "samplePlot.data.samp"))
    ## Dropping of points.
    if (move){
        canvas$rmGrobs(c("samplePlot.points.1", "samplePlot.points", "samplePlot.lines.1"))
        for (i in 1:n.steps){
            y.pos <- y.pos - y.step
            canvas$image <- addGrob(canvas$image,
                                    pointsGrob(x, y.pos, vp = canvas$graphPath("animation.field"),
                                               pch = 19, name = "samplePlot.temp"))
            if (canvas$stopAnimation)
                return()
            if (!e$fade){
                canvas$image <- addGrob(canvas$image, getGrob(canvas$image, gPath("fadebox")))
                canvas$image <- addGrob(canvas$image, getGrob(canvas$image, gPath("sampvarlabels")))
                canvas$image <- addGrob(canvas$image, getGrob(canvas$image, gPath("dataAxis")))
            }
            canvas$drawImage()
        }
        canvas$rmGrobs(c("samplePlot.sampSelectLabel", "samplePlot.temp"))
        canvas$plotSample(e, canvas$which.sample)
        canvas$pauseImage(pause)
    }
}

plotSampvarBoxplotGhostMean <- function(canvas, i, e) {
    canvas$rmGrobs(c("dataPlot.ci.1", "samplePlot.rect.1", "statPlot.theodist.1",
                     "samplePlot.datapoints.points.1", "samplePlot.data.samp"))
    alpha = 0.25
    canvas$sampled.stats <- c(canvas$sampled.stats, i)
    x <- canvas$samples[[i]]
    y <- old.stackPoints(x, vp = canvas$graphPath("sample"))
    plotPoints(canvas, x, y, canvas$graphPath("sample"), "samplePlot", black = TRUE)
    index <- canvas$indexes[[i]]
    data.x <- canvas$x[index]
    data.y <- canvas$y[index]
    plotPoints(canvas, data.x, data.y, canvas$graphPath("data"), "samplePlot.datapoints", black = TRUE)
    allinfo <- c(canvas$stat.dist, recursive = TRUE)
    canvas$image <- addGrob(canvas$image, rectGrob
                            (x = unit(allinfo[canvas$sampled.stats], "native"),
                             y = unit(0.15, "npc"), height = unit(0.2, "npc"),
                             width = 0, gp = gpar(alpha = 0.25, col = "blue", lwd = 2),
                             vp = canvas$graphPath("sample"), name = "samplePlot.ghosts.1"))
    canvas$image <- addGrob(canvas$image, linesGrob
                            (x = unit(canvas$stat.dist[i], "native"),
                             y = unit(c(0.05, 0.5), "npc"), gp = gpar(lwd = 4, col = "blue"),
                             vp = canvas$graphPath("sample"), name = "samplePlot.lines.1"))
    if (!e$fade){
        canvas$image <- addGrob(canvas$image, getGrob(canvas$image, gPath("fadebox")))
        canvas$image <- addGrob(canvas$image, getGrob(canvas$image, gPath("sampvarlabels")))
        canvas$image <- addGrob(canvas$image, getGrob(canvas$image, gPath("dataAxis")))
    }
}

sampvarLabels <- function(canvas) {
    poplabel <- textGrob("Population",
                         x = unit(0, "npc") + unit(1, "mm"),
                         y = unit(1, "npc") - unit(1, "lines"),
                         just = "left",
                         name = "dataLabel",
                         vp = canvas$graphPath("data"),
                         gp = gpar(fontface = 2))
    methlabel <- textGrob("Module: Sampling Variation",
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
                             x = stringWidth(methlabel$label) + unit(6, "mm"),
                             just = "left",
                             name = "varLabel",
                             gp = gpar(fontsize = 10, fontface = "italic"),
                             vp = canvas$graphPath("canvas.header"))
    }
    quantitylabel <- textGrob(paste("Quantity:", canvas$stat.in.use),
                              x = varlabel$x[1] + stringWidth(paste(varlabel$label, collapse = "")) + unit(6, "mm"),
                              just = "left",
                              name = "quantityLabel",
                              gp = gpar(fontsize = 10, fontface = "italic"),
                              vp = canvas$graphPath("canvas.header"))
    filelabel <- textGrob(paste("File:", canvas$data.file),
                          x = quantitylabel$x + stringWidth(quantitylabel$label) + unit(6, "mm"),
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
    statlabel <- textGrob("Sampling Distribution",
                          x = unit(0, "npc") + unit(1, "mm"),
                          y = unit(0.8, "npc"),
                          just = c("left", "top"),
                          name = "statLabel",
                          vp = canvas$graphPath("stat"),
                          gp = gpar(fontface = 2))
    sampvarlabels <- grobTree(methlabel, varlabel, quantitylabel, filelabel,
                              infosep,
                              poplabel, samplabel, statlabel,
                              name = "sampvarlabels")
    canvas$image <- addGrob(canvas$image, sampvarlabels)
}

plotTheoDist <- function(e){
    canvas <- e$c1
    ## Replotting statistic distribution
    x <- c(canvas$stat.dist, recursive = TRUE)
    y <- canvas$stat.ypos
    plotPoints(canvas, x, y, canvas$graphPath("stat"),
               "statPlot", black = FALSE, alpha = 0.7)
    mean <- mean(canvas$x)
    sd <- pop.sd(canvas$x)/sqrt(canvas$n)
    ## Getting statistic panel x-scale.
    top.level <- downViewport(canvas$graphPath("stat"))
    stat.scale <- current.viewport()$xscale
    upViewport(top.level)
    ## Calculating normal density under the CLT.
    xs <- seq(stat.scale[1], stat.scale[2], length.out = 300)
    ys <- dnorm(xs, mean, sd)
    ## We need a sense of "density scale" for the y-axis. Fitting a
    ## kernel density estimator can provide this. We calculate the
    ## KDE, find the maximum point, map this to meet up with the top
    ## of the topmost point in the statistic panel, and scale the
    ## normal density curve accordingly. This ensures that the normal
    ## density curve has about the same area below it as the area
    ## taken up by the points; the normal density will have the same
    ## area as the KDE, which, in turn, will have a similar area to
    ## the points.
    dens <- density(x, from = stat.scale[1], to = stat.scale[2])
    ys <- ys/max(dens$y)
    y.max <- unit(1, "npc") - unit(2, "lines") - unit(0.5, "char")
    canvas$image <- addGrob(canvas$image, linesGrob
                            (x = unit(xs, "native"), y = (y.max*ys) + ys*unit(0.5, "char"),
                             gp = gpar(lwd = 2, col = "red"),
                             name = "statPlot.theodist.1",
                             vp = canvas$graphPath("stat")))
    canvas$showLabels()
    canvas$drawImage()
    canvas$rmGrobs(c("samplePlot.points.1", "samplePlot.lines.1",
                     "samplePlot.datapoints.points.1", "samplePlot.databox.text.2",
                     "samplePlot.stat.1", "samplePlot.hist.1"))
}

fadeData <- function(canvas, e){
    if (e$fade){
        canvas$image <- addGrob(canvas$image, rectGrob
                                (y = 0, just = "bottom", vp = canvas$graphPath("data"),
                                 width = unit(1, "npc") + unit(1, "char"),
                                 height = unit(1, "npc") + unit(0.5, "char"),
                                 gp = gpar(col = "white", fill = "white", alpha = 0.9),
                                 name = "fadebox"))
        canvas$image <- addGrob(canvas$image, getGrob(canvas$image, gPath("sampvarlabels")))
        canvas$image <- addGrob(canvas$image, getGrob(canvas$image, gPath("dataAxis")))
        e$fade <- FALSE
    } else {
        canvas$rmGrobs("fadebox")
        e$fade <- TRUE
    }
    canvas$drawImage()
}

pop.sd <- function(x){
    sqrt(var(x)*(length(x)-1)/length(x))
}
