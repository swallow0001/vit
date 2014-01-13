canvasPermPropDiff <- setRefClass("canvasPermPropDiffClass", contains = "canvasPlotClass",
                                  methods = list(
    plotSample = function(env, i = which.sample) {
        plotKSampleProportions(.self, env, i)
    },

    showLabels = function() {
        permPropLabels(.self)
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
        dataDiffArrowProp(.self, env)
    },

    plotStatDist = function(env, ...) {
        plotDiffDist(.self, env)
    },

    animateSample = function(...) {
        permPropTwoSample(.self, ...)
    },

    animateStat = function(env, n.steps) {
        dropPermPropArrow(.self, env, n.steps)
    },

    displayResult = function(env, ...) {
        showTailPropDiff(.self, env, ...)
    },

    handle1000 = function(env, ...) {
        perm1000(.self, env, ...)
    }))

load_permutation_proportion <- function(e) {
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
    tmp.canvas <- canvasPermPropDiff$new()
    tmp.canvas$import(e$c1)
    e$c1 <- tmp.canvas
    e$c1$viewports <- tmp.vps
    e$c1$image <- tmp.image
    ## Gives a value for the additional room given to each end of the x axis scale.
    e$extra <- 0
}

calcPropDiff <- function(x, y = NULL, canvas) {
    # Just using the first level for now
    xdata <- as.character(x)
    ydata <- as.character(y)
    ylevels <- canvas$ylevels
    props <- numeric(length(ylevels))
    for (i in 1:length(ylevels)) {
        leveldata <- xdata[ydata == ylevels[i]]
        props[i] <- length(leveldata[leveldata == canvas$loi]) / length(leveldata)
    }
    names(props) <- ylevels
    props
}

permPropLabels <- function(canvas) {
    n <- length(canvas$ylevels)
    samplabel <- textGrob("Data",
                          x = unit(0, "npc") + unit(1, "mm"),
                          y = unit(1, "npc") - unit(1, "mm"),
                          just = c("left", "top"),
                          vp = canvas$graphPath("data", n),
                          name = "dataLabel",
                          gp = gpar(fontface = 2))
    methlabel <- textGrob("Module: Randomisation test",
                          x = unit(0, "npc"),
                          just = "left",
                          gp = gpar(fontsize = 10, fontface = "italic"),
                          name = "methodLabel",
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
                             name = "varLabel",
                             gp = gpar(col = c(rep("black", 3), "blue", "black", "red", "black"),
                                       fontsize = 10, fontface = "italic"),
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
                                    vp = canvas$graphPath("data", n))
    rerandomlabel <- textGrob("Re-randomised data",
                            x = unit(0, "npc") + unit(1, "mm"),
                            y = unit(0.6, "npc"),
                            just = c("left", "top"),
                            vp = canvas$graphPath("sample", n),
                            name = "sampleLabel",
                            gp = gpar(fontface = 2))
    rerandomLabelBackground <- rectGrob(x = unit(0, "npc"),
                                      y = unit(0.6, "npc") + unit(0.5, "lines") - unit(1, "mm"),
                                      width = stringWidth(rerandomlabel$label) + unit(4, "mm"),
                                      height = stringHeight(rerandomlabel$label) + unit(2, "mm"),
                                      just = c("left", "top"),
                                      gp = gpar(col = "white", fill = "white", alpha = 0.7),
                                      name = "rerandomLabelBackground",
                                      vp = canvas$graphPath("sample", n))
    statlabel <- textGrob("Re-randomisation distribution",
                          x = unit(0, "npc") + unit(1, "mm"),
                          y = unit(0.8, "npc"),
                          just = c("left", "top"),
                          vp = canvas$graphPath("stat"),
                          name = "statLabel",
                          gp = gpar(fontface = 2))
    permlabels <- grobTree(methlabel, varlabel, quantitylabel, quantmethlabel, filelabel,
                           infosep,
                           dataLabelBackground,
                           samplabel,
                           rerandomLabelBackground,
                           rerandomlabel,
                           statlabel,
                           name = "permlabels")
    canvas$image <- addGrob(canvas$image, permlabels)
}

plotKSampleProportions <- function(canvas, e, i) {
    x <- canvas$samples[[i]]
    x[x != canvas$loi] <- canvas$loi.alt
    levels <- canvas$levels
    ylevels <- canvas$ylevels
    props <- rev(calcPropDiff(x, levels, canvas)[ylevels])
    seekViewport(canvas$graphPath("sample", 1))
    max.str.width <- stringWidth(ylevels)
    max.str.width <- convertWidth(max(max.str.width), "native", valueOnly = TRUE)
    upViewport(0)
    for (j in 1:length(ylevels)) {
        canvas$image <- addGrob(canvas$image, kpropbarGrob
                                (x[levels == ylevels[j]],
                                 ylab = ylevels[j],
                                 ylab.pos = 1 - max.str.width,
                                 ylab.col = getColour(j, length(ylevels)),
                                 draw.points = FALSE,
                                 lois = c(canvas$loi, canvas$loi.alt),
                                 y = 0, height = 0.5, vpcex = 1 / length(ylevels),
                                 name = paste("samplePlot.propbar", j, sep="."),
                                 vp = canvas$graphPath("sample", j)))
    }

    # If we're not doing ANOVA, show the difference
    if (length(ylevels) == 2) {
        canvas$image <- addGrob(canvas$image, linesGrob
                                (x = unit(props, "native"),
                                 y = unit(0.75, "native"),
                                 gp = gpar(lwd = 2, col = "red"),
                                 arrow = arrow(length = unit(0.1, "inches")),
                                 vp = canvas$graphPath("sample", 1),
                                 name = "samplePlot.line.1"))
        ## Filling sample databox
        cols <- getColour(1:length(ylevels), length(ylevels))
        names(cols) <- ylevels
        cols <- cols[order(ylevels)]
        text <- as.character(levels[order(canvas$indexes[[i]])])
        canvas$image <- addGrob(canvas$image, coldatatextGrob
                                (data = text,
                                 title = if (e$method == "permvar") "Random groups" else "Re-randomised",
                                 cols = cols,
                                 xpos = 0.5, max = 50,
                                 name = "samplePlot.databox.text.2", vp = canvas$graphPath("databox", 2)))
    }
}

dataDiffArrowProp <- function(canvas, e){
    x <- canvas$x
    levels <- canvas$levels
    # ylevels goes from largest to smallest, we want the reverse
    ylevels <- rev(canvas$ylevels)
    n <- length(x)
    firstY <- x[levels == ylevels[1]]
    prop1 <- length(firstY[firstY == canvas$loi]) / length(firstY)
    secondY <- x[levels != ylevels[1]]
    prop2 <- length(secondY[secondY == canvas$loi]) / length(secondY)
    props <- c(prop1, prop2)
    canvas$image <- addGrob(canvas$image, linesGrob(x = unit(props, "native"),
                                                    y = unit(0.9, "npc"),
                                                    gp = gpar(lwd = 2, col = "red"),
                                                    arrow = arrow(length = unit(0.1, "inches")),
                                                    vp = canvas$graphPath("data", 1),
                                                    name = "dataPlot.stat.1"))
    canvas$image <- addGrob(canvas$image, linesGrob(x = unit(rep(0, 2), "native"),
                                                    y = unit(c(0, 0.4), "npc"),
                                                    gp = gpar(lty = "dashed"),
                                                    vp = canvas$graphPath("stat"),
                                                    name = "zeroline.1"))
    canvas$image <- addGrob(canvas$image, textGrob
                            (label = format(round(diff(props), 3), nsmall = 3),
                             x = unit(mean(props), "native"),
                             y = unit(0.95, "npc"),
                             just = c("centre", "bottom"),
                             gp = gpar(lwd = 2, col = "red", fontface = "bold"),
                             vp = canvas$graphPath("data", 1),
                             name = "dataPlot.stat.text"))
}

dropPermPropArrow <- function(canvas, e, n.steps){
    arrowbounds <- canvas$stat.dist[c(canvas$plotted.stats, canvas$which.sample)]
    stats <- sapply(arrowbounds, diff)
    curr.arrow <- arrowbounds[[length(stats)]]
    curr.stat <- stats[length(stats)]
    y.max <- unit(1, "npc") - unit(2, "lines") - unit(0.5, "char")
    y <- stackPoints(stats, vp = canvas$graphPath("stat"), y.min = 0, y.max = y.max)

    # Need to find out the location of the value of native
    # units relative to the animation.field viewport
    depth <- downViewport(canvas$graphPath("stat"))
    x.zero <- convertX(unit(0, "native"), "npc", valueOnly = TRUE)
    upViewport(depth)
    depth <- downViewport(canvas$graphPath("sample"))
    curr.arrow <- convertX(unit(curr.arrow, "native"), "npc", valueOnly = TRUE)
    upViewport(depth)

    y.start <- 1 + (0.75 / 2)
    y.end <- y[length(stats)]
    y.step <- (y.start - y.end)/n.steps
    xs.start <- curr.arrow
    xs.end <- c(x.zero, x.zero + diff(curr.arrow))
    xs.step <- (xs.start - xs.end)/n.steps
    for (i in 0:n.steps){
        temp.arrow <- linesGrob(x = unit(xs.start - i*xs.step, "npc"),
                                y = unit(rep(y.start - i*y.step, 2), "native"),
                                gp = gpar(lwd = 2, col = "red"),
                                arrow = arrow(length = unit(0.1, "inches")),
                                vp = canvas$graphPath("animation.field"),
                                name = "samplePlot.temp.arrow")
        canvas$image <- addGrob(canvas$image, temp.arrow)
        if (canvas$stopAnimation)
            return()
        canvas$drawImage()
    }
    canvas$image <- removeGrob(canvas$image, gPath("samplePlot.temp.arrow"))
}

permPropTwoSample <- function(canvas, e, n.steps, mix = TRUE) {
    e$clearPanel("sample")
    canvas$rmGrobs(c("samplePlot.propbar.1", "samplePlot.propbar.2"))
    ## Drop samples down to middle plot.
    x <- canvas$x
    levels <- canvas$levels
    ylevels <- canvas$ylevels
    seekViewport(canvas$graphPath("sample", 1))
    max.str.width <- stringWidth(ylevels)
    max.str.width <- convertWidth(max(max.str.width), "native", valueOnly = TRUE)
    upViewport(0)
    y.start <- c(2, 2.5)
    y.end <- y.start - 1
    y.step <- 1 / n.steps
    height.start <- 0.8*0.5
    height.end <- 0.5*0.5
    height.step <- (height.start - height.end) / n.steps
    bar.start <- 0.1
    bar.end <- 0.05
    bar.step <- (bar.start - bar.end) / n.steps
    if (mix) {
        # Dropping samples
        for (i in 1:n.steps) {
            curr.step <- y.start - i*y.step
            currbar.step <- bar.start - i*bar.step
            canvas$image <- addGrob(canvas$image, kpropbarGrob
                                    (x[levels == ylevels[1]],
                                     ylab = ylevels[1],
                                     ylab.pos = 1 - max.str.width,
                                     ylab.col = getColour(1, 2),
                                     statLineHeight = currbar.step,
                                     height = height.start - i*height.step,
                                     draw.points = FALSE,
                                     lois = c(canvas$loi, canvas$loi.alt),
                                     y = curr.step[1], vpcex = 1 / length(ylevels),
                                     name = paste("samplePlot.tempPropbar", 1, sep = "."),
                                     vp = canvas$graphPath("animation.field")))
            canvas$image <- addGrob(canvas$image, kpropbarGrob
                                    (x[levels == ylevels[2]],
                                     ylab = ylevels[2],
                                     ylab.pos = 1 - max.str.width,
                                     ylab.col = getColour(2, 2),
                                     statLineHeight = currbar.step,
                                     height = height.start - i*height.step,
                                     draw.points = FALSE,
                                     lois = c(canvas$loi, canvas$loi.alt),
                                     y = curr.step[2], vpcex = 1 / length(ylevels),
                                     name = paste("samplePlot.tempPropbar", 2, sep = "."),
                                     vp = canvas$graphPath("animation.field")))
            # Want labels to appear over the top
            permPropLabels(canvas)
            if (canvas$stopAnimation)
                return()
            canvas$drawImage()
        }
        if (canvas$stopAnimation)
            return()
        canvas$pauseImage(10)
        fadeIn.start <- 0
        fadeIn.step <- 1 / n.steps
        fadeOut.start <- 1
        fadeOut.step <- -fadeIn.step
        # Mixing sample levels
        y.start <- c(1, 1.5)
        y.end <- c(1, 1)
        y.step <- (y.start - y.end) / n.steps
        height.start <- 0.5*0.5
        height.end <- 0.75*2*0.5
        height.step <- (height.start - height.end) / n.steps
        bar.start <- 0.05
        bar.end <- 0.1
        bar.step <- (bar.start - bar.end) / n.steps
        for (i in 1:n.steps) {
            curr.step <- y.start - i*y.step
            currbar.step <- bar.start - i*bar.step
            currfadeIn.step <- fadeIn.start + i*fadeIn.step
            currfadeOut.step <- fadeOut.start + i*fadeOut.step
            canvas$image <- addGrob(canvas$image, kpropbarGrob
                                    (x[levels == ylevels[1]],
                                     ylab = ylevels[1],
                                     ylab.pos = 1 - max.str.width,
                                     ylab.col = getColour(1, 2),
                                     statLineHeight = bar.start,
                                     height = height.start - i*height.step,
                                     draw.points = FALSE,
                                     lois = c(canvas$loi, canvas$loi.alt),
                                     y = curr.step[1], vpcex = 1 / length(ylevels),
                                     name = paste("samplePlot.tempPropbar", 1, sep = "."),
                                     gp = gpar(alpha = currfadeOut.step),
                                     vp = canvas$graphPath("animation.field")))
            canvas$image <- addGrob(canvas$image, kpropbarGrob
                                    (x[levels == ylevels[2]],
                                     ylab = ylevels[2],
                                     ylab.pos = 1 - max.str.width,
                                     ylab.col = getColour(2, 2),
                                     statLineHeight = bar.start,
                                     height = height.start - i*height.step,
                                     draw.points = FALSE,
                                     lois = c(canvas$loi, canvas$loi.alt),
                                     y = curr.step[2], vpcex = 1 / length(ylevels),
                                     name = paste("samplePlot.tempPropbar", 2, sep = "."),
                                     gp = gpar(alpha = currfadeOut.step),
                                     vp = canvas$graphPath("animation.field")))
            canvas$image <- addGrob(canvas$image, kpropbarGrob
                                    (x,
                                     ylab = "",
                                     ylab.pos = 1 - max.str.width,
                                     statLineHeight = bar.start,
                                     height = height.end,
                                     draw.points = FALSE,
                                     lois = c(canvas$loi, canvas$loi.alt),
                                     y = 1, vpcex = 1,
                                     name = paste("samplePlot.tempPropbar", 3, sep = "."),
                                     gp = gpar(alpha = currfadeIn.step),
                                     vp = canvas$graphPath("animation.field")))
            permPropLabels(canvas)
            if (canvas$stopAnimation)
                return()
            canvas$drawImage()
        }
        canvas$rmGrobs(c("samplePlot.tempPropbar.1", "samplePlot.tempPropbar.2"))
    }

    canvas$pauseImage(5) # Pausing to show the merged samples clearly as
                         # we don't have many steps

    # Drawing the datatext grob, it should get overwritten anyway when
    # we call plotSample()
    cols <- getColour(1:length(ylevels), length(ylevels))
    names(cols) <- ylevels
    cols <- cols[order(ylevels)]
    text <- as.character(levels[order(canvas$indexes[[canvas$which.sample]])])
    canvas$image <- addGrob(canvas$image, coldatatextGrob
                            (data = text,
                             title = if (e$method == "permvar") "Random groups" else "Re-randomised",
                             cols = cols,
                             xpos = 0.5, max = 50,
                             name = "samplePlot.databox.text.2",
                             vp = canvas$graphPath("databox", 2)))

    canvas$pauseImage(5)

    # Separating samples
    x.sample <- canvas$samples[[canvas$which.sample]]
    y.start <- c(1, 1)
    y.end <- c(1, 1.5)
    y.step <- (y.start - y.end) / n.steps
    height.end <- 0.5*0.5
    height.start <- 0.75*2*0.5
    height.step <- (height.start - height.end) / n.steps
    bar.start <- 0.05
    bar.end <- 0.1
    bar.step <- (bar.start - bar.end) / n.steps
    fadeIn.start <- 0
    fadeIn.step <- 1 / n.steps
    fadeOut.start <- 1
    fadeOut.step <- -fadeIn.step
    for (i in 1:n.steps) {
        curr.step <- y.start - i*y.step
        currbar.step <- bar.start - i*bar.step
        currfadeIn.step <- fadeIn.start + i*fadeIn.step
        currfadeOut.step <- fadeOut.start + i*fadeOut.step
        canvas$image <- addGrob(canvas$image, kpropbarGrob
                         (x.sample[levels == ylevels[1]],
                          ylab = ylevels[1],
                          ylab.pos = 1 - max.str.width,
                          ylab.col = getColour(1, 2),
                          statLineHeight = bar.start,
                          height = height.start - i*height.step,
                          draw.points = FALSE,
                          lois = c(canvas$loi, canvas$loi.alt),
                          y = curr.step[1], vpcex = 1 / length(ylevels),
                          name = paste("samplePlot.tempPropbar", 1, sep = "."),
                          gp = gpar(alpha = currfadeIn.step),
                          vp = canvas$graphPath("animation.field")))
        canvas$image <- addGrob(canvas$image, kpropbarGrob
                                (x.sample[levels == ylevels[2]],
                                 ylab = ylevels[2],
                                 ylab.pos = 1 - max.str.width,
                                 ylab.col = getColour(2, 2),
                                 statLineHeight = bar.start,
                                 height = height.start - i*height.step,
                                 draw.points = FALSE,
                                 lois = c(canvas$loi, canvas$loi.alt),
                                 y = curr.step[2], vpcex = 1 / length(ylevels),
                                 name = paste("samplePlot.tempPropbar", 2, sep = "."),
                                 gp = gpar(alpha = currfadeIn.step),
                                 vp = canvas$graphPath("animation.field")))
        canvas$image <- addGrob(canvas$image, kpropbarGrob
                                (x,
                                 ylab = "",
                                 ylab.pos = 1 - max.str.width,
                                 statLineHeight = bar.start,
                                 height = height.start,
                                 draw.points = FALSE,
                                 lois = c(canvas$loi, canvas$loi.alt),
                                 y = 1, vpcex = 1,
                                 name = paste("samplePlot.tempPropbar", 3, sep = "."),
                                 gp = gpar(alpha = currfadeOut.step),
                                 vp = canvas$graphPath("animation.field")))
        permPropLabels(canvas)
        if (canvas$stopAnimation)
            return()
        canvas$drawImage()
    }
    canvas$rmGrobs(c(paste("samplePlot.tempPropbar", 1:3, sep = ".")))
}

showTailPropDiff <- function(canvas, e) {
    n.steps <- 10
    x <- canvas$x
    levels <- canvas$levels
    # ylevels goes from largest to smallest, we want the reverse
    ylevels <- rev(canvas$ylevels)
    n <- length(x)
    firstY <- x[levels == ylevels[1]]
    prop1 <- length(firstY[firstY == canvas$loi]) / length(firstY)
    secondY <- x[levels != ylevels[1]]
    prop2 <- length(secondY[secondY == canvas$loi]) / length(secondY)
    props <- c(prop1, prop2)

    # Need to find out the location of the value of native
    # units relative to the animation.field viewport
    depth <- downViewport(canvas$graphPath("stat"))
    x.zero <- convertX(unit(0, "native"), "npc", valueOnly = TRUE)
    upViewport(depth)
    depth <- downViewport(canvas$graphPath("data"))
    props <- convertX(unit(props, "native"), "npc", valueOnly = TRUE)
    upViewport(depth)

    # Replotting statistic distribution
    stats <- sapply(canvas$stat.dist, diff)
    y.max <- unit(1, "npc") - unit(2, "lines") - unit(0.5, "char")
    y.stats <- canvas$stat.ypos
    plotPoints(canvas, stats, y.stats, canvas$graphPath("stat"),
               name = "statPlot.points.1", black = FALSE, alpha = 0.7)
    y.start <- 2 + (0.75 / 2)
    y.end <- 0
    y.step <- (y.start - y.end)/n.steps
    x.start <- props
    x.end <- c(x.zero, x.zero + diff(props))
    x.step <- (x.start - x.end) / n.steps
    for (i in 0:10) {
        canvas$image <- addGrob(canvas$image, linesGrob
                                (x = unit(x.start - i*x.step, "npc"),
                                 y = unit(y.start - i*y.step, "native"),
                                 gp = gpar(lwd = 2, col = "red"),
                                 arrow = arrow(length = unit(0.1, "inches")),
                                 vp = canvas$graphPath("animation.field"),
                                 name = "statPlot.temp.arrow"))
        if (canvas$stopAnimation)
            return()
        canvas$drawImage()
    }
    canvas$image <- removeGrob(canvas$image, gPath("statPlot.temp.arrow"))
    prop.diff <- diff(props)
    y.max <- unit(1, "npc") - unit(2, "lines") - unit(0.5, "char")
    y <- canvas$stat.ypos
    # We only consider one-tailed p-values - the statistics
    # considered "more extreme" that what is observed therefore
    # depends on the original direction of the observed effect.
    if (prop.diff > 0){
        x.in <- stats[stats < prop.diff]
        x.out <- stats[stats >= prop.diff]
        y.in <- y[stats < prop.diff]
        y.out <- y[stats >= prop.diff]
        tot <- sum(stats >= prop.diff)
        p <- mean(stats >= prop.diff)
    } else {
        x.in <- stats[stats > prop.diff]
        x.out <- stats[stats <= prop.diff]
        y.in <- y[stats > prop.diff]
        y.out <- y[stats <= prop.diff]
        tot <- sum(stats <= prop.diff)
        p <- mean(stats <= prop.diff)
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
    arrow <- linesGrob(x = unit(x.end, "npc"),
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
        xpos <- unit(rep(prop.diff, 2), "native") + unit(5, "mm")
        hjust <- 0
    }
    upViewport(top.level)
    arrow <- editGrob(arrow, vp = canvas$graphPath("animation.field"))
    canvas$image <- addGrob(canvas$image, rectGrob
                            (x = xpos, y = unit(0.5, "npc"), width = text.width,
                             height = unit(2, "lines"), hjust = hjust, vjust = 0,
                             gp = gpar(col = "white", fill = "white"),
                             vp = canvas$graphPath("stat"), name = "statPlot.proprect"))
    canvas$image <- addGrob(canvas$image, textGrob
                            (c(text1, text2),
                             x = xpos,
                             y = unit.c(unit(1, "lines") + unit(0.5, "npc"), unit(0.5, "npc")),
                             hjust = hjust, vjust = 0, vp = canvas$graphPath("stat"),
                             name = "statPlot.proptext"))
    canvas$image <- addGrob(canvas$image, linesGrob
                            (x = unit(rep(x.end[2], 2), "npc"),
                             y = unit(c(0, 0.5), "npc") - unit(1, "mm"),
                             vp = canvas$graphPath("stat"), name = "statPlot.propline"))
    canvas$image <- addGrob(canvas$image, arrow)
    canvas$showLabels()
    if (canvas$stopAnimation)
        return()
    canvas$drawImage()
    canvas$rmGrobs(c("statPlot.proprect", "statPlot.proptext", "statPlot.propline",
                     "statPlot.arrow.1", "statPlot.points.1"))
    if (length(x.in) > 0)
        canvas$image <- removeGrob(canvas$image, gPath("statPlot.lightpoints"))
    if (length(x.out) > 0)
        canvas$image <- removeGrob(canvas$image, gPath("statPlot.darkpoints"))
}
