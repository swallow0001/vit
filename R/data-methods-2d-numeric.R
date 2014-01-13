plotScatterPlot <- function(canvas, x, y, vp, col) {
    plotPoints(canvas, x, y, canvas$graphPath(vp), name = paste(vp, "Plot", sep = ""))
    coeffs <- coefficients(lm(as.numeric(y) ~ as.numeric(x)))
    ## Ensuring line of best fit does not leave viewport.
    ## WHY are we sitting in animation.field2 viewport here?!
    seekViewport(canvas$graphPath(vp))
    x.scale <- current.viewport()$xscale
    y.scale <- current.viewport()$yscale
    seekViewport("animation.field2")
    preds <- coeffs[1] + x.scale*coeffs[2]
    invpreds <- (y.scale - coeffs[1])/coeffs[2]
    xs <- c(x.scale[preds >= y.scale[1] & preds <= y.scale[2]],
            invpreds[invpreds > x.scale[1] & invpreds < x.scale[2]])
    ys <- c(preds[preds >= y.scale[1] & preds <= y.scale[2]],
            y.scale[invpreds >= x.scale[1] & invpreds <= x.scale[2]])
    lobf <- linesGrob(x = xs, y = ys, default.units = "native",
                      gp = gpar(lwd = 2, col = col), name = paste(vp, "Plot.line.1", sep = ""),
                      vp = canvas$graphPath(vp))
    canvas$image <- addGrob(canvas$image, lobf)
}

plotFreqScatterPlot <- function(canvas, x, y, freq, vp, col){
    any.text <- any(freq > 1)
    any.points <- any(freq == 1)
    if (any.text){
        text <- textGrob(label = freq[freq > 1], x = x[freq > 1], y = y[freq > 1],
                         default.units = "native", gp = gpar(lwd = 2, col = "grey60"),
                         vp = canvas$graphPath(vp))
    } else {
        text <- NULL
    }
    if (any.points){
        points <- pointsGrob(x = x[freq == 1], y = y[freq == 1], default.units = "native",
                             gp = gpar(lwd = 2, col = "grey60"),
                             vp = canvas$graphPath(vp))
    } else {
        points <- NULL
    }
    textpoints <- grobTree(gList(text, points),
                           name = paste(vp, "Plot.points.1", sep = ""))
    canvas$image <- addGrob(canvas$image, textpoints)
    x <- rep(x, freq)
    y <- rep(y, freq)
    coeffs <- coefficients(lm(as.numeric(y) ~ as.numeric(x)))
    ## Ensuring line of best fit does not leave viewport.
    seekViewport(canvas$graphPath(vp))
    x.scale <- current.viewport()$xscale
    y.scale <- current.viewport()$yscale
    seekViewport("animation.field2")
    preds <- coeffs[1] + x.scale*coeffs[2]
    invpreds <- (y.scale - coeffs[1])/coeffs[2]
    xs <- c(x.scale[preds >= y.scale[1] & preds <= y.scale[2]],
            invpreds[invpreds > x.scale[1] & invpreds < x.scale[2]])
    ys <- c(preds[preds >= y.scale[1] & preds <= y.scale[2]],
            y.scale[invpreds > x.scale[1] & invpreds < x.scale[2]])
    lobf <- linesGrob(x = xs, y = ys, default.units = "native",
                      gp = gpar(lwd = 2, col = col), name = paste(vp, "Plot.line.1", sep = ""),
                      vp = canvas$graphPath(vp))
    canvas$image <- addGrob(canvas$image, lobf)
}


coordsCalc <- function(coeffs, x, y){
    if (diff(range(y)) == 0){
        y.scale <- range(y) + c(-1, 1)
    } else {
        y.scale <- range(y) + c(-0.04, 0.04)*diff(range(y))
    }
    if (diff(range(x)) == 0){
        x.scale <- range(x) + c(-1, 1)
    } else {
        x.scale <- range(x) + c(-0.04, 0.04)*diff(range(x))
    }
    preds <- coeffs[1] + x.scale*coeffs[2]
    invpreds <- (y.scale - coeffs[1])/coeffs[2]
    xs <- c(x.scale[preds >= y.scale[1] & preds <= y.scale[2]],
            invpreds[invpreds > x.scale[1] & invpreds < x.scale[2]])
    ys <- c(preds[preds > y.scale[1] & preds < y.scale[2]],
            y.scale[invpreds >= x.scale[1] & invpreds <= x.scale[2]])
    list(xs = xs, ys = ys)
}
