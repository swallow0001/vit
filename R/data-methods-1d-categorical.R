# plotting methods for displaying one dimensional, categorical data

setLevelOfInterest <- function(canvas, loi) {
    xdata <- canvas$x
    xlevels <- sort(unique(xdata))
    canvas$loi <- if (is.null(loi)) xlevels[1] else loi
    canvas$loi.alt <- if (length(xlevels) > 2) "All Else"
                      else xlevels[xlevels != canvas$loi]
    xdata[xdata != canvas$loi] <- canvas$loi.alt
    canvas$loi.data <- xdata
    canvas
}

plotPropbar <- function(canvas) {
    # Don't plot more than 1000 points because it's slow and we can't
    # end up seeing anything useful anyway.
    pbg <- propbarGrob(data = canvas$loi.data,
                       y = 0.9 / 8,
                       height = 0.45,
                       lois = c(canvas$loi, canvas$loi.alt),
                       draw.points = length(canvas$x) <= 500,
                       name = "dataPlot",
                       vp = canvas$graphPath("data"))
    canvas$image <- addGrob(canvas$image, pbg)
}
