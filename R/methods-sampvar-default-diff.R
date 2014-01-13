canvasSampvarDefaultDiff <- setRefClass("canvasSampvarDefaultDiffClass", contains = "canvasPlotClass",
                                        methods = list(
    calcStat = function(i = which.sample, ys = NULL, canvas = .self) {
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

    plotSample = function(env, i = which.sample) {
        plotSampvarBoxplotGhostDefaultDiff(.self,
                                           i, env, alpha = 0.25)
    },

    showLabels = function() {
        sampvarDefaultDiffLabels(.self)
    },

    fadePlots = function(env, ...) {
        fadeDataTwoSample(.self, env)
    },

    plotDataStat = function(env, ...) {
    },

    plotStatDist = function(env, ...) {
    },

    animateSample = function(e, n.steps, n.slow, opts) {
        dropSampvarPointsDiff(.self, e, n.steps, n.slow,
                              pause = 20)
    },

    animateStat = function(env, n.steps) {
    },

    handle1000 = function(env, ...) {
        sampvar1000DefaultDiff(.self, env)
    }))

load_sampvar_default_diff <- function(e) {
    e$c1$stat.in.use <- svalue(e$stat)
    # There is something messy going on with viewports and
    # gTrees, such that when attempting to import them, we
    # end up with "uninitializedField"s. Assign to a temp
    # var and reassign later.
    tmp.vps <- e$c1$viewports
    tmp.image <- e$c1$image
    e$c1$viewports <- NULL
    e$c1$image <- NULL
    tmp.canvas <- canvasSampvarDefaultDiff$new()
    tmp.canvas$import(e$c1)
    e$c1 <- tmp.canvas
    e$c1$viewports <- tmp.vps
    e$c1$image <- tmp.image
    e$difference <- TRUE
}

plotSampvarBoxplotGhostDefaultDiff <- function(canvas, i, e, alpha = 0.25) {
    canvas$rmGrobs(c("samplePlot.datapoints.points.1", "samplePlot.data.samp",
                     "samplePlot.boxplot.1", "samplePlot.lines.1"))
    index <- canvas$indexes[[i]]
    x <- canvas$x[index]
    levels <- canvas$levels[index]
    ylevels <- canvas$ylevels
    data.x <- canvas$x[index]
    data.y <- 2 + 0.5*canvas$y[index] + 0.5*(levels == canvas$ylevels[2])
    for (j in 1:length(ylevels)) {
        canvas$image <- addGrob(canvas$image, textGrob
                                (ylevels[j], x = 1, y = unit(0.5, "npc"), just = c("right", "bottom"),
                                 vp = canvas$graphPath("sample", j),
                                 name = paste("samplePlot.text", j, sep = ".")))
        current.level <- levels == ylevels[j]
        y <- old.stackPoints(x[current.level], vp = canvas$graphPath("sample"))
        plotPoints(canvas, x[current.level], y,
                   canvas$graphPath("sample", j),
                   col = getColour(j, length(ylevels)),
                   "samplePlot")
        canvas$image <- addGrob(canvas$image, ghostsGrob(canvas$quartiles[1:i, 1, j],
                                                         canvas$quartiles[1:i, 2, j],
                                                         canvas$quartiles[1:i, 3, j],
                                                         alpha = alpha, box.color = "red",
                                                         median.color = "blue",
                                                         vp = canvas$graphPath("sample", j),
                                                         name = paste("samplePlot.boxplot.ghosts", j, sep = ".")))
        canvas$image <- addGrob(canvas$image, boxplotGrob(x[current.level],
                                                          box.color = "black",
                                                          median.color = "black",
                                                          stat.color = "black",
                                                          show.w = FALSE,
                                                          vp = canvas$graphPath("sample", j),
                                                          gp = gpar(lwd = 3),
                                                          name = paste("samplePlot.boxplot", j, sep = ".")))
        canvas$image <- addGrob(canvas$image, linesGrob
                                (x = unit(canvas$quartiles[i, 2, j], "native"),
                                 y = unit(c(0.05, 0.5), "npc"), gp = gpar(lwd = 4, col = "blue"),
                                 vp = canvas$graphPath("sample", j), name = paste("samplePlot.lines", j, sep = ".")))
    }

    plotPoints(canvas, x,
               data.y, black = TRUE,
               vp = canvas$graphPath("animation.field"),
               name ="samplePlot.temp.data")
    if (!e$fade){
        canvas$image <- addGrob(canvas$image, getGrob(canvas$image, gPath("fadebox1")))
        canvas$image <- addGrob(canvas$image, getGrob(canvas$image, gPath("fadebox2")))
        canvas$image <- addGrob(canvas$image, getGrob(canvas$image, gPath("sampvarlabels")))
        canvas$image <- addGrob(canvas$image, getGrob(canvas$image, gPath("dataAxis")))
    }
}

sampvar1000DefaultDiff <- function(canvas, e) {
    canvas$rmGrobs(c("samplePlot.boxplot.1", "samplePlot.datapoints.points.1", "samplePlot.data.samp"))
    for (i in 20*(1:50)) {
        canvas$rmGrobs(c("samplePlot.datapoints.points.1", "samplePlot.data.samp"))
        plotSampvarBoxplotGhostDefaultDiff(canvas, i, e, alpha = 0.04)
        if (canvas$stopAnimation) {
            return()
        }
        canvas$showLabels()
        canvas$drawImage()
    }
    canvas$rmGrobs(c(paste(c("samplePlot.points", "samplePlot.lines", "samplePlot.boxplot"),
                     rep(1:2, each = 3), sep = "."),
                     "samplePlot.temp.data.points.",
                     "samplePlot.datapoints.points.1"))
    canvas$drawImage()
    canvas$sampled.stats <- NULL
    canvas$plotted.stats <- NULL
}

sampvarDefaultDiffLabels <- function(canvas) {
    poplabel <- textGrob("Population",
                         x = unit(0, "npc") + unit(1, "mm"),
                         y = unit(1, "npc") - unit(1, "mm"),
                         just = c("left", "top"),
                         name = "dataLabel",
                         vp = canvas$graphPath("data", 2),
                         gp = gpar(fontface = 2))
    methlabel <- textGrob("Module: 2 Sample Sampling Variation",
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
    filelabel <- textGrob(paste("File:", canvas$data.file),
                          x = quantitylabel$x + stringWidth(quantitylabel$label) + unit(6, "mm"),
                          just = "left",
                          name = "fileLabel",
                          gp = gpar(fontsize = 10, fontface = "italic"),
                          vp = canvas$graphPath("canvas.header"))
    infosep <- linesGrob(x = unit(0:1, "npc"), y = unit(0, "npc"),
                         name = "infoSeparatorLine",
                         vp = canvas$graphPath("canvas.header"))
    dataLabelBackground <- rectGrob(x = unit(0, "npc"),
                                    y = unit(1, "npc") - unit(1, "lines") - unit(1, "mm"),
                                    width = stringWidth(poplabel$label) + unit(2, "mm"),
                                    height = stringHeight(poplabel$label) + unit(2, "mm"),
                                    just = c("left", "bottom"),
                                    gp = gpar(col = "white", fill = "white", alpha = 0.7),
                                    name = "dataLabelBackground",
                                    vp = canvas$graphPath("data", 2))
    samplabel <- textGrob("Sample",
                            x = unit(0, "npc") + unit(1, "mm"),
                            y = unit(0.6, "npc"),
                            just = c("left", "top"),
                            name = "sampleLabel",
                            vp = canvas$graphPath("sample", 2),
                            gp = gpar(fontface = 2))
    sampLabelBackground <- rectGrob(x = unit(0, "npc"),
                                      y = unit(0.6, "npc") + unit(0.5, "lines") - unit(1, "mm"),
                                      width = stringWidth(samplabel$label) + unit(4, "mm"),
                                      height = stringHeight(samplabel$label) + unit(2, "mm"),
                                      just = c("left", "top"),
                                      gp = gpar(col = "white", fill = "white", alpha = 0.7),
                                      name = "resampLabelBackground",
                                      vp = canvas$graphPath("sample", 2))
    sampvarlabels <- grobTree(methlabel, varlabel, quantitylabel, filelabel,
                              infosep,
                              dataLabelBackground,
                              poplabel,
                              sampLabelBackground,
                              samplabel,
                              name = "sampvarlabels")
    canvas$image <- addGrob(canvas$image, sampvarlabels)
}
