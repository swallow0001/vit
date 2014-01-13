canvasPermMeanPaired <- setRefClass("canvasPermMeanPairedClass", contains = "canvasPlotClass",
                                    methods = list(
    plotSample = function(env, i = which.sample, ...) {
       plotSamplePointsPaired(.self, env, i, fun = mean)
    },

    showLabels = function() {
        permPairedLabels(.self)
    },

    calcStat = function(i = which.sample, y = NULL, canvas = .self) {
        calcPermPairedMean(samples[[i]], y)
    },

    calcAllStats = function(a, b = NULL, canvas = .self) {
        calcPermPairedMean(a, b)
    },

    plotDataStat = function(env, ...) {
        zeroLineAndArrow(.self, env, fun = mean)
    },

    plotStatDist = function(env, ...) {
        plotBootDist(.self, env)
    },

    animateSample = function(env, n.steps, mix) {
        animatePermPairedSamples(.self, env, n.steps, mix)
    },

    trackSample = function(env) {
    },

    animateStat = function(env, n.steps) {
        dropStat(.self, env, n.steps)
    },

    displayResult = function(env, ...) {
        showTailPairedPerm(.self, env, fun = mean)
    },

    handle1000 = function(env, ...) {
        boot1000mean(.self, env, points = TRUE)
    },

    fadePlots = function(env, ...) {
    }))

load_permutation_mean_paired <- function(e) {
    e$pairedSamples <- svalue(e$paired.samples)
    e$c1$stat.in.use <- svalue(e$stat)
    e$perm.method <- svalue(e$perm.choices)
    e$c1$stat.method <- e$perm.method
    ## Ensuring stat arrow always points to the right.
    if (eval(call(e$c1$stat.in.use, e$c1$x)) < 0) {
        e$c1$x <- -e$c1$x
        e$c1$paired.data[, 1:2] <- e$c1$paired.data[, 2:1]
    }
    # There is something messy going on with viewports and
    # gTrees, such that when attempting to import them, we
    # end up with "uninitializedField"s. Assign to a temp
    # var and reassign later.
    tmp.vps <- e$c1$viewports
    tmp.image <- e$c1$image
    e$c1$viewports <- NULL
    e$c1$image <- NULL
    tmp.canvas <- canvasPermMeanPaired$new()
    tmp.canvas$import(e$c1)
    e$c1 <- tmp.canvas
    e$c1$viewports <- tmp.vps
    e$c1$image <- tmp.image
    e$extra <- 0
}

calcPermPairedMean <- function(x, y = NULL){
    mean(x)
}

permPairedLabels <- function(canvas) {
    samplabel <- textGrob("Sample",
                          x = unit(0, "npc") + unit(1, "mm"),
                          y = unit(1, "npc") - unit(1, "mm"),
                          just = c("left", "top"),
                          name = "dataLabel",
                          vp = canvas$graphPath("data"),
                          gp = gpar(fontface = 2))
    methlabel <- textGrob("Module: Paired samples randomisation test",
                          x = unit(0, "npc"),
                          just = "left",
                          gp = gpar(fontsize = 10, fontface = "italic"),
                          name = "methodLabel",
                          vp = canvas$graphPath("canvas.header"))
    varlabel <- textGrob(paste("Variable:", canvas$x.name, "-", canvas$y.name),
                         x = unit(0, "npc") + stringWidth(methlabel$label) + unit(6, "mm"),
                         just = "left",
                         gp = gpar(fontsize = 10, fontface = "italic"),
                         name = "varLabel",
                         vp = canvas$graphPath("canvas.header"))
    quantitylabel <- textGrob(paste("Quantity:", canvas$stat.in.use),
                              x = varlabel$x[1] + stringWidth(paste(varlabel$label, collapse = "")) + unit(6, "mm"),
                              just = "left",
                              name = "quantityLabel",
                              gp = gpar(fontsize = 10, fontface = "italic"),
                              vp = canvas$graphPath("canvas.header")) 
    quantmethlabel <- textGrob(paste("Statistic:", canvas$stat.method),
                               x = quantitylabel$x + stringWidth(quantitylabel$label) + unit(6, "mm"),
                               just = "left",
                               name = "quantmethLabel",
                               gp = gpar(fontsize = 10, fontface = "italic"),
                               vp = canvas$graphPath("canvas.header")) 
    filelabel <- textGrob(paste("File:", canvas$data.file),
                          x = quantmethlabel$x + stringWidth(quantmethlabel$label) + unit(6, "mm"),
                          just = "left",
                          name = "fileLabel",
                          gp = gpar(fontsize = 10, fontface = "italic"),
                          vp = canvas$graphPath("canvas.header"))
    infosep <- linesGrob(x = unit(0:1, "npc"), y = unit(0, "npc"),
                         name = "infoSeparatorLine",
                         vp = canvas$graphPath("canvas.header"))
    dataLabelBackground <- rectGrob(x = unit(0, "npc"),
                                    y = unit(1, "npc") - unit(1, "lines") - unit(1, "mm"),
                                    width = stringWidth(samplabel$label) + unit(2, "mm"),
                                    height = stringHeight(samplabel$label) + unit(2, "mm"),
                                    just = c("left", "bottom"),
                                    gp = gpar(col = "white", fill = "white", alpha = 0.7),
                                    name = "dataLabelBackground",
                                    vp = canvas$graphPath("data"))
    resamplabel <- textGrob("Re-randomised data",
                            x = unit(0, "npc") + unit(1, "mm"),
                            y = unit(0.8, "npc"),
                            just = c("left", "top"),
                            name = "sampleLabel",
                            vp = canvas$graphPath("sample"),
                            gp = gpar(fontface = 2))
    resampLabelBackground <- rectGrob(x = unit(0, "npc"),
                                      y = unit(0.8, "npc") + unit(1, "mm"),
                                      width = stringWidth(resamplabel$label) + unit(4, "mm"),
                                      height = stringHeight(resamplabel$label) + unit(2, "mm"),
                                      just = c("left", "top"),
                                      gp = gpar(col = "white", fill = "white", alpha = 0.7),
                                      name = "resampLabelBackground",
                                      vp = canvas$graphPath("sample"))
    statlabel <- textGrob("Re-randomisation distribution",
                          x = unit(0, "npc") + unit(1, "mm"),
                          y = unit(0.8, "npc"),
                          just = c("left", "top"),
                          name = "statLabel",
                          vp = canvas$graphPath("stat"),
                          gp = gpar(fontface = 2))
    bootlabels <- grobTree(methlabel, varlabel, quantitylabel, quantmethlabel, filelabel,
                           infosep,
                           dataLabelBackground,
                           samplabel,
                           resampLabelBackground,
                           resamplabel, statlabel,
                           name = "bootlabels")
    canvas$image <- addGrob(canvas$image, bootlabels)
}

plotSamplePointsPaired <- function(canvas, e, i, fun = mean){
    alpha = 0.25
    canvas$sampled.stats <- c(canvas$sampled.stats, i)
    x <- canvas$samples[[i]]
    y <- old.stackPoints(x, vp = canvas$graphPath("sample"))
    plotPoints(canvas, x, y, canvas$graphPath("sample"), "samplePlot", black = FALSE)
    allinfo <- c(canvas$stat.dist, recursive = TRUE)
    canvas$image <- addGrob(canvas$image, rectGrob
                            (x = unit(allinfo[canvas$sampled.stats], "native"),
                             y = unit(0.15, "npc"), height = unit(0.2, "npc"),
                             width = 0, gp = gpar(alpha = alpha, col = "blue", lwd = 2),
                             vp = canvas$graphPath("sample"), name = "samplePlot.ghosts.1"))
    canvas$image <- addGrob(canvas$image, linesGrob
                            (x = unit(canvas$stat.dist[i], "native"),
                             y = unit(c(0.05, 0.5), "npc"), gp = gpar(lwd = 4, col = "blue"),
                             vp = canvas$graphPath("sample"), name = "samplePlot.lines.1"))
    canvas$image <- addGrob(canvas$image, linesGrob
                            (x = unit(c(0, fun(x)), "native"),
                             y = unit(0.2, "npc"),
                             gp = gpar(lwd = 2, col = "red"),
                             arrow = arrow(length = unit(0.1, "inches")),
                             vp = canvas$graphPath("sample"),
                             name = "samplePlot.stat.1"))

    if (nchar(canvas$x.name) > 5){
        title1 <- paste(substr(canvas$x.name, 1, 5), "...", sep = "")
    } else {
        title1 <- canvas$x.name
    }
    if (nchar(canvas$y.name) > 5){
        title2 <- paste(substr(canvas$y.name, 1, 5), "...", sep = "")
    } else {
        title2 <- canvas$y.name
    }
    ifswap <- canvas$if.permswap[[i]]
    sample1 <- numeric(length(canvas$n))
    sample2 <- numeric(length(canvas$n))
    sample1[ifswap] <- (canvas$paired.data[, 1])[ifswap]
    sample1[!ifswap] <- (canvas$paired.data[, 2])[!ifswap]
    sample2[ifswap] <- (canvas$paired.data[, 2])[ifswap]
    sample2[!ifswap] <- (canvas$paired.data[, 1])[!ifswap]
    xsw <- max(stringWidth(c(title1, format(round(sample1, canvas$dp), nsmall = canvas$dp))))
    ysw <- max(stringWidth(c(title2, format(round(sample2, canvas$dp), nsmall = canvas$dp))))
    dsw <- max(stringWidth(c("diff", format(-abs(round(x, canvas$dp)), nsmall = canvas$dp))))
    maxw <- max(xsw, ysw, dsw)
    sparespace <- (unit(1, "npc") - 3*maxw - unit(2, "mm"))*(1/6)
    textcols <- character(canvas$n)
    textcols[ifswap] <- "red"
    textcols[!ifswap] <- "blue"
    textcols <- c("black", textcols[1:(min(50, canvas$n))])
    text.sample1 <- datatextGrob(data = sample2, title = title1,
                                 x = unit(1, "mm") + sparespace + 0.5*maxw,
                                 max = 50, name = "samplePlot.text1.2",
                                 gp = gpar(col = textcols),
                                 vp = canvas$graphPath("databox", 2))
    text.sample2 <- datatextGrob(data = sample1, title = title2,
                                 x = unit(0.5, "npc"),
                                 max = 50, name = "samplePlot.text2.2",
                                 gp = gpar(col = textcols),
                                 vp = canvas$graphPath("databox", 2))
    text.sample3 <- datatextGrob(data = x, title = "diff",
                                 x = unit(1, "npc") - unit(1, "mm")
                                 - sparespace - 0.5*maxw,
                                 max = 50, name = "samplePlot.text3.2",
                                 vp = canvas$graphPath("databox", 2))
    canvas$image <- addGrob(canvas$image, text.sample1)
    canvas$image <- addGrob(canvas$image, text.sample2)
    canvas$image <- addGrob(canvas$image, text.sample3)
}

zeroLineAndArrow <- function(canvas, e, fun = mean){
    plotBoxplot(canvas, canvas$x, stat = fun, stat.color = "purple3",
                canvas$graphPath("data"), "dataPlot")
    stat.x <- fun(canvas$x)
    arrow <- linesGrob(x = unit(c(0, stat.x), "native"),
                       y = unit(0.2, "npc"),
                       gp = gpar(lwd = 2, col = "red"),
                       arrow = arrow(length = unit(0.1, "inches")),
                       vp = canvas$graphPath("data"),
                       name = "dataPlot.stat.1")
    canvas$image <- addGrob(canvas$image, arrow)
    canvas$image <- addGrob(canvas$image, textGrob
                            (label = format(round(stat.x, canvas$dp), nsmall = canvas$dp),
                             x = unit(stat.x, "native"),
                             y = unit(0.1, "npc"),
                             just = c("centre", "bottom"),
                             gp = gpar(lwd = 2, col = "red", fontface = "bold"),
                             vp = canvas$graphPath("data", 1),
                             name = "dataPlot.stat.text"))
}

showTailPairedPerm <- function(canvas, e, fun = mean){
    n.steps <- 10
    x <- canvas$x
    levels <- canvas$levels
    ylevels <- canvas$ylevels
    stat1 <- fun(x)
    ## Replotting statistic distribution
    stats <- canvas$stat.dist
    y.stats <- canvas$stat.ypos
    plotPoints(canvas, stats, y.stats, canvas$graphPath("stat"),
               "statPlot", black = FALSE, alpha = 0.7)
    y.start <- 2.2
    y.end <- 0
    y.step <- (y.start - y.end)/n.steps
    x.start <- stat1
    x.end <- stat1
    x.step <- (x.start - x.end)/n.steps
    for (i in 0:10){
        canvas$image <- addGrob(canvas$image, linesGrob
                                (x = unit(c(0, stat1) - i*x.step, "native"),
                                 y = unit(y.start - i*y.step, "native"),
                                 gp = gpar(lwd = 2, col = "red"),
                                 arrow = arrow(length = unit(0.1, "inches")),
                                 vp = canvas$graphPath("animation.field"),
                                 name = "statPlot.temp.arrow"))
        if (canvas$stopAnimation)
            return()
        canvas$drawImage()
    }
    canvas$rmGrobs("statPlot.temp.arrow")
    diff <- stat1
    ## We only consider one-tailed p-values - the statistics
    ## considered "more extreme" that what is observed therefore
    ## depends on the original direction of the observed effect.
    if (diff > 0){
        x.in <- stats[stats < diff]
        x.out <- stats[stats >= diff]
        y.in <- y.stats[stats < diff]
        y.out <- y.stats[stats >= diff]
        tot <- sum(stats >= diff)
        p <- mean(stats >= diff)
    } else {
        x.in <- stats[stats > diff]
        x.out <- stats[stats <= diff]
        y.in <- y.stats[stats > diff]
        y.out <- y.stats[stats <= diff]
        tot <- sum(stats <= diff)
        p <- mean(stats <= diff)
    }
    if (length(x.in) > 0){
        canvas$image <- addGrob(canvas$image, pointsGrob
                                (x.in, y.in, gp = gpar(col = "lightgrey", lwd = 2, alpha = 0.7),
                                 vp = canvas$graphPath("stat"), name = "statPlot.lightpoints"))
    }
    if (length(x.out) > 0){
        canvas$image <- addGrob(canvas$image, pointsGrob
                                (x.out, y.out, gp = gpar(col = "grey60", lwd = 2, alpha = 0.7),
                                         vp = canvas$graphPath("stat"), name = "statPlot.darkpoints"))
    }
    ## Calculating whether or not p-value text is printed off screen.
    arrow <- linesGrob(x = unit(c(0, diff), "native"),
                       y = unit(y.end, "native"),
                       gp = gpar(lwd = 2, col = "red"),
                       arrow = arrow(length = unit(0.1, "inches")),
                       name = "statPlot.arrow.1")
    top.level <- downViewport(canvas$graphPath("stat"))
    text1 <- paste(tot, "/ 1000", sep = " ")
    text2 <- paste("= ",p)
    text.width <- max(stringWidth(text1), stringWidth(text2))
    arrow.width <- grobWidth(arrow)
    pos <- convertX(arrow.width + unit(5, "mm") + text.width, "npc", valueOnly = TRUE)
    if (pos > 0.5){
        xpos <- unit(1, "npc")
        hjust <- 1
    } else {
        xpos <- unit(rep(diff, 2), "native") + unit(5, "mm")
        hjust <- 0
    }
    upViewport(top.level)
    arrow <- editGrob(arrow, vp = canvas$graphPath("stat"))
    canvas$image <- addGrob(canvas$image, rectGrob
                            (x = xpos, y = unit(0.5, "npc"), width = text.width,
                             height = unit(2, "lines"), hjust = hjust, vjust = 0,
                             gp = gpar(col = "white", fill = "white"),
                             vp = canvas$graphPath("stat"), name = "statPlot.proprect"))
    canvas$image <- addGrob(canvas$image, textGrob
                            (c(text1, text2),
                             x = xpos,
                             y = unit.c(unit(1, "lines") + unit(0.5, "npc") + unit(1, "mm"),
                             unit(0.5, "npc") + unit(1, "mm")),
                             hjust = hjust, vjust = 0, vp = canvas$graphPath("stat"),
                             name = "statPlot.proptext"))
    canvas$image <- addGrob(canvas$image, linesGrob
                            (x = unit(rep(diff, 2), "native"),
                             y = unit(c(0, 0.5), "npc") - unit(1, "mm"),
                             vp = canvas$graphPath("stat"), name = "statPlot.propline"))
    canvas$image <- addGrob(canvas$image, arrow)
    canvas$showLabels()
    if (canvas$stopAnimation)
        return()
    canvas$drawImage()
    canvas$rmGrobs(c("statPlot.proptext", "statPlot.proprect", "statPlot.propline",
                     "statPlot.arrow.1", "statPlot.points.1", "statPlot.lightpoints", "statPlot.darkpoints"))
}

animatePermPairedSamples <- function(canvas, e, n.steps = 10, mix = FALSE){
    canvas$rmGrobs(c("samplePlot.points.1", "samplePlot.lines.1", "samplePlot.stat.1",
                     "samplePlot.text1.2", "samplePlot.text2.2", "samplePlot.text3.2"))
    n.steps = 10
    n.slow = 5
    n <- canvas$n
    max <- 50
    paired.data <- canvas$paired.data
    ifswap <- canvas$if.permswap[[canvas$which.sample]]
    sample1 <- numeric(length(canvas$n))
    sample2 <- numeric(length(canvas$n))
    sample1[ifswap] <- (canvas$paired.data[, 1])[ifswap]
    sample1[!ifswap] <- (canvas$paired.data[, 2])[!ifswap]
    sample2[ifswap] <- (canvas$paired.data[, 2])[ifswap]
    sample2[!ifswap] <- (canvas$paired.data[, 1])[!ifswap]
    x <- canvas$x
    x[ifswap] <- -x[ifswap]
    n.slow <- min(n.slow, n)
    ntext <- min(n, max)
    nnos <- if (n > max) max - 1 else n
    npcs <- (ntext:0)/ntext
    ## Gives the y positions of text in data text grob ("top" justified).
    yunit <- (unit(npcs, "npc") - unit(4*(npcs - 0.5), "mm") + unit(1 - npcs, "lines"))
    x.name <- canvas$x.name
    y.name <- canvas$y.name
    if (nchar(x.name) > 5){
        title1 <- paste(substr(x.name, 1, 5), "...", sep = "")
    } else {
        title1 <- x.name
    }
    if (nchar(y.name) > 5){
        title2 <- paste(substr(y.name, 1, 5), "...", sep = "")
    } else {
        title2 <- y.name
    }
    xsw <- max(stringWidth(c(title1, format(round(canvas$paired.data[, 1], canvas$dp),
                                            nsmall = canvas$dp))))
    ysw <- max(stringWidth(c(title2, format(round(canvas$paired.data[, 2], canvas$dp),
                                            nsmall = canvas$dp))))
    dsw <- max(stringWidth(c("diff", format(round(canvas$x, canvas$dp),
                                            nsmall = canvas$dp))))
    maxw <- max(xsw, ysw, dsw)
    sparespace <- (unit(1, "npc") - 3*maxw - unit(2, "mm"))*(1/6)
    sparespace2 <- (unit(0.5, "npc") - 3*maxw - unit(2, "mm"))*(1/6)
    xs.start <- unit.c(unit(1, "mm") + sparespace2 + 0.5*maxw, unit(0.25, "npc"))
    xs.ends <- xs.start + unit(0.5, "npc")
    textcols <- character(canvas$n)
    textcols[ifswap] <- "red"
    textcols[!ifswap] <- "blue"
    textcols <- c("black", textcols[1:(min(50, canvas$n))])
    for (i in seq(from = 1, by = 1, length.out = nnos)){
        if (ifswap[i]){
            xs.end <- rev(xs.ends)
            col <- "red"
        } else {
            xs.end <- xs.ends
            col <- "blue"
        }
        fface <- rep(1, ntext + 1)
        fface[i + 1] <- 2
        cols <- c("black", rep("blue", ntext))
        cols[i + 1] <- col
        text.sample1 <- datatextGrob(data = paired.data[, 1], title = title1,
                                     x = unit(1, "mm") + sparespace + 0.5*maxw,
                                     max = 50, name = "text.sample1",
                                     gp = gpar(col = cols, fontface = fface),
                                     vp = canvas$graphPath("databox", 1))
        text.sample2 <- datatextGrob(data = paired.data[, 2], title = title2,
                                     x = unit(0.5, "npc"),
                                     max = 50, name = "text.sample2",
                                     gp = gpar(col = cols, fontface = fface),
                                     vp = canvas$graphPath("databox", 1))
        canvas$image <- addGrob(canvas$image, text.sample1)
        canvas$image <- addGrob(canvas$image, text.sample2)
        canvas$pauseImage(5)
        if (mix & i <= 10){
            for (j in 0:(n.steps - 1)){
                text <- format(round(paired.data[i, ], canvas$dp), nsmall = canvas$dp)
                xs.step <- (xs.end - xs.start)*(1/n.steps)
                canvas$image <- addGrob(canvas$image, textGrob
                                        (text, y = yunit[i + 1], x = xs.start + j*xs.step,
                                         name = "samplePlot.text.temp",
                                         gp = gpar(col = textcols[i + 1], fontface = 2),
                                         just = "top", vp = vpPath("canvas.all", "canvas.boxes")))
                if (canvas$stopAnimation){
                    return()
                }
                canvas$drawImage()
            }
        }
        sample.text1 <- datatextGrob(data = sample2, title = title1,
                                     x = unit(1, "mm") + sparespace + 0.5*maxw,
                                     max = 50, name = "samplePlot.text1.2",
                                     gp = gpar(fontface = fface,
                                     col = textcols,
                                     alpha = c(rep(1, i + 1), rep(0, ntext - i))),
                                     vp = canvas$graphPath("databox", 2))
        sample.text2 <- datatextGrob(data = sample1, title = title2,
                                     x = unit(0.5, "npc"),
                                     max = 50, name = "samplePlot.text2.2",
                                     gp = gpar(fontface = fface,
                                     col = textcols,
                                     alpha = c(rep(1, i + 1), rep(0, ntext - i))),
                                     vp = canvas$graphPath("databox", 2))
        sample.text3 <- datatextGrob(data = x, title = "diff",
                                     x = unit(1, "npc") - unit(1, "mm")
                                     - sparespace - 0.5*maxw,
                                     max = 50, name = "samplePlot.text3.2",
                                     gp = gpar(alpha = c(rep(1, i + 1),
                                               rep(0, ntext - i))),
                                     vp = canvas$graphPath("databox", 2))
        canvas$image <- addGrob(canvas$image, sample.text1)
        canvas$image <- addGrob(canvas$image, sample.text2)
        canvas$image <- addGrob(canvas$image, sample.text3)
        canvas$rmGrobs("samplePlot.text.temp")
        if (canvas$stopAnimation){
            return()
        }
        canvas$pauseImage(5)
        canvas$image <- editGrob(canvas$image, gPath("samplePlot.text1.2"),
                                 gp = gpar(fontface = 1))
        canvas$image <- editGrob(canvas$image, gPath("samplePlot.text2.2"),
                                         gp = gpar(fontface = 1))
    }
    canvas$image <- editGrob(canvas$image, gPath("text.sample1"),
                             gp = gpar(fontface = 1))
    canvas$image <- editGrob(canvas$image, gPath("text.sample2"),
                             gp = gpar(fontface = 1))
    canvas$drawImage()
}
