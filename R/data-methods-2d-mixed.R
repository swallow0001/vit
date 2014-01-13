# plotting methods for displaying two dimensional data that has one numeric dimension and one categorical dimension. These methods rely on the methods for 1d numeric data.

# plotPoints and plotBoxplot are defined in the data-methods-numeric-1d.R file
plotPointGroupsAndBoxplot <- function(canvas) {
    canvas$orderLevels()
    canvas$y <- old.stackPoints(canvas$x, canvas$levels, vp = canvas$graphPath("data"),
                                y.min = 0.3, y.max = 1)
    n <- 1
    ylevels <- canvas$ylevels
    cols <- getColour(1:length(ylevels), length(levels))
    for (i in ylevels) {
        plotPoints(canvas, canvas$x[canvas$levels == i],
                   canvas$y[canvas$levels == i], col = cols[n],
                   vp = canvas$graphPath("data", as.character(n)),
                   name = "dataPlot")
        plotBoxplot(canvas, canvas$x[canvas$levels == i],
                    stat = NULL, stat.color = NULL,
                    vp = canvas$graphPath("data", as.character(n)),
                    name = "dataPlot")
        canvas$image <- addGrob(canvas$image, textGrob
                                (i, x = 1, y = unit(1, "mm"), just = c("right", "bottom"),
                                 vp = canvas$graphPath("data", as.character(n)),
                                 name = paste("dataPlot.text", n, sep = ".")))
        n <- n + 1
    }
}

plotPointGroupsNoBoxplot <- function(canvas) {
    canvas$orderLevels()
    n <- 1
    ylevels <- canvas$ylevels
    for (i in ylevels) {
        if (length(canvas$x) >= canvas$hist.cutoff) {
            col <- getColour(n, length(ylevels), c = 50)
            plotHist(canvas, canvas$x[canvas$levels == i],
                     vp = canvas$graphPath("data", n), name = "dataPlot", 
                     fill = col)
        } else {
            col <- getColour(n, length(ylevels))
            canvas$y <- old.stackPoints(canvas$x, canvas$levels, vp = canvas$graphPath("data"),
                                        y.min = 0.3, y.max = 1)
            plotPoints(canvas, canvas$x[canvas$levels == i],
                       canvas$y[canvas$levels == i], col = col,
                       vp = canvas$graphPath("data", as.character(n)),
                       name = "dataPlot")
        }
        canvas$image <- addGrob(canvas$image, roundrectGrob
                                (x = unit(1, "npc") + unit(1, "mm"),
                                 y = unit(0, "npc"), just = c("right", "bottom"),
                                 height = stringHeight(i) + unit(2, "mm"),
                                 width = stringWidth(i) + unit(2, "mm"),
                                 vp = canvas$graphPath("data", n),
                                 gp = gpar(fill = "white", col = "white", alpha = 0.75),
                                 name = paste("dataPlot.blankrect", n, sep = ".")))
        canvas$image <- addGrob(canvas$image, textGrob
                                (i, x = 1, y = unit(1, "mm"), just = c("right", "bottom"),
                                 vp = canvas$graphPath("data", as.character(n)),
                                 name = paste("dataPlot.text", n, sep = ".")))
        n <- n + 1
    }
    if (length(ylevels) > 2){
        canvas$image <- editGrob(canvas$image, gPath("dataAxis"), label = FALSE)
    }
}


plotPointGroups <- function(canvas, x, y, levels, graph, cols = NULL, name){
    if (is.null(cols)) cols = "grey60"
    cols <- rep(cols, length = length(unique(levels)))
    n <- 1
    for (i in canvas$ylevels) {
        plotPoints(canvas, x[levels == i],
                   y[canvas$levels == i], col = cols[n],
                   vp = canvas$graphPath(graph, as.character(n)),
                   name = name)
        n <- n + 1
    }
}
