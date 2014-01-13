canvasSampvarDefault <- setRefClass("canvasSampvarDefaultClass", contains = "canvasPlotClass",
                                    methods = list(
    calcStat = function(i = which.sample, y = NULL, canvas = .self) {
        calcMedian(samples[[i]], y)
    },

    calcAllStats = function(a, b = NULL, canvas = .self) {
        calcMedian(a, b)
    },

    plotSample = function(env, i = which.sample) {
        plotSampvarBoxplotGhostDefault(.self, .self$which.sample, env, alpha = 0.25)
    },

    showLabels = function() {
        sampvarDefaultLabels(.self)
    },

    fadePlots = function(env, ...) {
        fadeData(.self, env)
    },

    plotDataStat = function(env, ...) {
    },

    plotStatDist = function(env, ...) {
    },

    animateSample = function(env, n.steps, n.slow, opts) {
        dropSampvarPoints1d(.self, env, n.steps, n.slow,
                            keep.plot = opts$keep.plot, move = opts$move,
                            pause = 20)
    },

    animateStat = function(env, n.steps) {
    },

    handle1000 = function(env, ...) {
        sampvar1000default(.self, env)
    }))

load_sampvar_default <- function(e) {
    e$c1$stat.in.use <- svalue(e$stat)
    # There is something messy going on with viewports and
    # gTrees, such that when attempting to import them, we
    # end up with "uninitializedField"s. Assign to a temp
    # var and reassign later.
    tmp.vps <- e$c1$viewports
    tmp.image <- e$c1$image
    e$c1$viewports <- NULL
    e$c1$image <- NULL
    tmp.canvas <- canvasSampvarDefault$new()
    tmp.canvas$import(e$c1)
    e$c1 <- tmp.canvas
    e$c1$viewports <- tmp.vps
    e$c1$image <- tmp.image
    e$difference <- FALSE
}

plotSampvarBoxplotGhostDefault <- function(canvas, i, e, alpha = 0.25) {
    canvas$rmGrobs(c("samplePlot.datapoints.points.1", "samplePlot.data.samp",
                     "samplePlot.boxplot.1", "samplePlot.lines.1"))
    x <- canvas$samples[[i]]
    y <- old.stackPoints(x, vp = canvas$graphPath("sample"))
    plotPoints(canvas, x, y, canvas$graphPath("sample"), "samplePlot", black = TRUE)
    index <- canvas$indexes[[i]]
    data.x <- canvas$x[index]
    data.y <- canvas$y[index]
    plotPoints(canvas, data.x, data.y, canvas$graphPath("data"), "samplePlot.datapoints", black = TRUE)
    canvas$image <- addGrob(canvas$image, ghostsGrob(canvas$quartiles[1:i, 1],
                                                     canvas$quartiles[1:i, 2],
                                                     canvas$quartiles[1:i, 3],
                                                     alpha = alpha, box.color = "red",
                                                     median.color = "blue",
                                                     vp = canvas$graphPath("sample"),
                                                     name = "samplePlot.boxplot.ghosts.1"))
    canvas$image <- addGrob(canvas$image, boxplotGrob(x,
                                                      box.color = "black",
                                                      median.color = "black",
                                                      stat.color = "black",
                                                      show.w = FALSE,
                                                      vp = canvas$graphPath("sample"),
                                                      gp = gpar(lwd = 3),
                                                      name = "samplePlot.boxplot.1"))
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

sampvar1000default <- function(canvas, e) {
    canvas$rmGrobs(c("samplePlot.boxplot.1", "samplePlot.datapoints.points.1", "samplePlot.data.samp"))
    for (i in 20*(1:50)) {
        canvas$rmGrobs(c("samplePlot.datapoints.points.1", "samplePlot.data.samp"))
        plotSampvarBoxplotGhostDefault(canvas, i, e, alpha = 0.04)
        if (canvas$stopAnimation) {
            return()
        }
        canvas$showLabels()
        canvas$drawImage()
    }
    canvas$rmGrobs(c("samplePlot.points.1", "samplePlot.lines.1", "samplePlot.boxplot.1",
                     "samplePlot.datapoints.points.1"))
    canvas$drawImage()
    canvas$sampled.stats <- NULL
    canvas$plotted.stats <- NULL
}

sampvarDefaultLabels <- function(canvas) {
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
                             x = unit(0, "npc") + stringWidth(methlabel$label) + unit(6, "mm"),
                             just = "left",
                             name = "varLabel",
                             gp = gpar(fontsize = 10, fontface = "italic"),
                             vp = canvas$graphPath("canvas.header"))
    }
    quantitylabel <- textGrob("Quantity: default",
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
    sampvarlabels <- grobTree(methlabel, varlabel, quantitylabel, filelabel,
                              infosep,
                              poplabel, samplabel,
                              name = "sampvarlabels")
    canvas$image <- addGrob(canvas$image, sampvarlabels)
}

