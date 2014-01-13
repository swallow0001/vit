# Plotting methods for displaying two dimensional data that has two categorical dimensions.

plotKProportions <- function(canvas) {
    # Normally, in the continuous case we use the canvas method
    # orderLevels in order to correctly order the levels of our data.
    # Begin orderLevels
    if (!is.null(canvas$loi)) {
        xdata <- as.character(canvas$x)
        xlevels <- sort(unique(xdata))
        xdata[xdata != canvas$loi] <- if (length(xlevels) > 2) "All Else" else canvas$loi.alt
    } else {
        xdata <- as.character(canvas$x)
        xlevels <- sort(unique(xdata))
        xdata[xdata != xlevels[1]] <- if (length(xlevels) > 2) "All Else" else xlevels[2]
    }
    ydata <- as.character(canvas$levels)

    ylevels <- sort(unique(ydata))
    props <- numeric(length(ylevels))
    for (i in 1:length(ylevels)) {
        ldata <- xdata[ydata == ylevels[i]]
        loi <- if (! is.null(canvas$loi)) canvas$loi else xlevels[1]
        props[i] <- length(ldata[ldata == loi]) / length(ldata)
    }
    names(props) <- ylevels
    ylevels <- names(sort(props, decreasing = TRUE))
    canvas$ylevels <- ylevels
    # End orderLevels

    if (is.null(canvas$loi.data)) {
        # Need to set loi vars if we haven't set them via the 'Analyse' tab yet
        if (length(xlevels) > 2) {
            canvas$loi.alt <- "All Else"
            canvas$loi <- xlevels[1]
            canvas$loi.data <- xdata
            canvas$loi.data[canvas$loi.data != canvas$loi] <- canvas$loi.alt
        } else {
            canvas$loi <- xlevels[1]
            canvas$loi.alt <- xlevels[2]
            canvas$loi.data <- xdata
        }
    }

    max.str.width <- stringWidth(ylevels)
    max.str.width <- convertWidth(max(max.str.width), "npc", valueOnly = TRUE)
    draw.points <- length(xdata) <= 500 & length(ylevels) == 2
    for (i in 1:length(ylevels)) {
        canvas$image <- addGrob(canvas$image, kpropbarGrob
                                (xdata[ydata == ylevels[i]],
                                 ylab = ylevels[i],
                                 ylab.pos = 1 - max.str.width,
                                 ylab.col = getColour(i, length(ylevels)),
                                 draw.points = draw.points,
                                 lois = c(canvas$loi, canvas$loi.alt),
                                 y = 0.1, height = 0.6, vpcex = 1 / length(ylevels),
                                 name = paste("dataPlot.propbar", i, sep="."),
                                 vp = canvas$graphPath("data", i)))
    }

    # In the case when we're doing k-sample methods, we don't need
    # to see the axis labels for the data *and* the sample, thus
    # remove them for the data.
    if (length(ylevels) > 2)
        canvas$image <- editGrob(canvas$image, gPath("dataAxis"), label = FALSE)
}
