
grid.propbar <- function(...)
    grid.draw(propbarGrob(...))

# propbarGrob constructs a proportion bar of a vector of categorical data.
propbarGrob <- function(data, y = 0, height = 0.2, cols = c("blue", "#FFCFCF"),
                        lois = NULL, draw.points = TRUE,
                        name = NULL, gp = NULL, vp = NULL) {
    # The propbarGrob needs to be filled with points that are ideally 
    # equidistant from one another. The general process is to first identify
    # the size of the propbar on the device. Once that has been accomplished,
    # determine the dimensions of the grid of points that are to occupy the
    # propbar.
    # Currently we calculate the points grid for the entire grob, then work
    # it out for each group by adding or removing columns of points if necessary.
    # This is not ideal as we may end up with columns that are closer together
    # than the rows.

    # For some reason (not established yet) we cannot obtain correct unit
    # conversion calculations when calculating point coordinates.
    # This is broken when in the sample.data.1 VP but works in the data.data.1
    # VP, as a result, we switch to the data.data.1 VP, do our calculations
    # and place the grob in the correct VP.
    seekViewport("data.data.1")

    levels <- lois
    ns <- sum(data == levels[1])
    ns <- c(ns, length(data) - ns)
    one.group <- any(ns == 0)
    empty.group <- if (ns[1] == 0) "left" else "right"
    p <- mean(data == levels[1])
    sdP <- sqrt(p*(1 - p) / sum(ns))
    range <- p + c(-1, 1)*5*sdP
    if (range[1] < 0) range[1] = 0
    if (range[2] > 1) range[2] = 1

    statBar <- linesGrob(x = rep(unit(p, "npc"), 2),
                         y = unit(c(0, height + y), "npc"),
                         gp = gpar(col = "black", lwd = 3),
                         name = "statBar")
    leftBar <- rectGrob(x = 0, y = y, width = unit(p, "npc"),
                        height = unit(height, "npc"),
                        just = c("left", "bottom"), name = "leftBar",
                        gp = gpar(fill = cols[1], col = cols[1]))
    rightBar <- rectGrob(x = p, y = y, width = unit(1 - p, "npc"),
                         height = unit(height, "npc"),
                         just = c("left", "bottom"), name = "rightBar",
                         gp = gpar(fill = cols[2], col = cols[2]))
    leftPropText <- textGrob(ns[1], x = unit(0.5 * p, "npc"),
                             y = unit(y + 0.5 * height, "npc"),
                             name = "leftPropText",
                             gp = gpar(fontsize = 80, alpha = 0.75,
                                       col = "white", cex = height / 0.45))
    rightPropText <- textGrob(ns[2], x = unit(p + 0.5 * (1 - p), "npc"),
                              y = unit(y + 0.5 * height, "npc"),
                              name = "rightPropText",
                              gp = gpar(fontsize = 80, alpha = 0.75,
                                        col = "white", cex = height / 0.45))
    text1 <- textGrob(levels[1], x = 0, y = unit(height + 2 * y, "npc"),
                      just = c("left", "bottom"), name = "text1")
    text2 <- textGrob(levels[2], x = 1, y = unit(height + 2 * y, "npc"),
                      just = c("right", "bottom"), name = "text2")

    # Flag if the count labels are too large to fit within their boundaries
    showBlankRects <- FALSE

    # If we have a small proportion on either end we cannot fit the text
    # in the proportion anymore, as a result, shift it to being above it
    leftPropTextWidth <- convertWidth(grobWidth(leftPropText), "npc", valueOnly = TRUE)
    rightPropTextWidth <- convertWidth(grobWidth(rightPropText), "npc", valueOnly = TRUE)
    if (min(c(leftPropTextWidth, rightPropTextWidth)) > min(c(p, 1 - p))) {
        showBlankRects <- TRUE

        if (one.group) {
            llabel <- if (empty.group == "right") ns[1] else 0
            rlabel <- if (empty.group == "left") ns[2] else 0
        } else {
            llabel <- ns[1]
            rlabel <- ns[2]
        }
        leftPropText <- textGrob(llabel,
                                 x = unit(0, "npc"),
                                 y = unit(height + 2 * y, "npc"),
                                 just = c("left", "bottom"),
                                 name = "leftPropText",
                                 gp = gpar(fontsize = 50, alpha = 0.75,
                                           col = "black", cex = height / 0.45))
        rightPropText <- textGrob(rlabel,
                                  x = unit(1, "npc"),
                                  y = unit(height + 2 * y, "npc"),
                                  just = c("right", "bottom"),
                                  name = "rightPropText",
                                  gp = gpar(fontsize = 50, alpha = 0.75,
                                            col = "black", cex = height / 0.45)) 
        leftBlankRect <- roundrectGrob(x = unit(0, "npc") - unit(1, "mm"),
                                       y = unit(height + 2 * y, "npc") - unit(1, "mm"),
                                       height = grobHeight(text1) + unit(2, "mm"),
                                       width = grobWidth(text1) + unit(2, "mm"),
                                       just = c("left", "bottom"),
                                       name = "leftBlankRect",
                                       gp = gpar(col = "blue", fill = "white", alpha = 0.8))
        rightBlankRect <- roundrectGrob(x = unit(1, "npc") + unit(1, "mm"),
                                        y = unit(height + 2 * y, "npc") - unit(1, "mm"),
                                        height = grobHeight(text2) + unit(2, "mm"),
                                        width = grobWidth(text2) + unit(2, "mm"),
                                        just = c("right", "bottom"),
                                        name = "rightBlankRect",
                                        gp = gpar(col = "red", fill = "white", alpha = 0.8))
    }

    if (one.group)
        ns <- max(ns)

    # If we have a large amount of observations, point calculation is too slow.
    # Moreover, the points become less useful visually, thus just draw a propbar
    # with points removed.
    if (sum(ns) > 500 | !draw.points) {
        # Returning to our previous VP, the reasoning being explained earlier.
        upViewport(0)
        return(gTree(data = data,
                     x = unit(0, "npc"), y = unit(0, "npc"),
                     height = height, cols = cols, p = p,
                     range = range, name = name, gp = gp, vp = vp,
                     children = gList(leftBar,
                                      leftPropText,
                                      rightBar,
                                      rightPropText,
                                      statBar,
                                      if (showBlankRects) leftBlankRect else NULL,
                                      if (showBlankRects) rightBlankRect else NULL,
                                      text1, text2),
                     points.present = FALSE, cl = "propbar"))
    }

    halfStatBarWidth <- 0.5 * convertX(grobWidth(statBar),
                                       "char", valueOnly = TRUE)

    # This calculates the distance that the sequence of point coordinates
    # should span
    calculatePointSpan <- function(distance, dim = "width") {
        if (dim == "width")
            span <- convertWidth(unit(distance, "npc") -
                      unit(2, "char") - unit(halfStatBarWidth, "char"),
                    "npc", valueOnly = TRUE)
        if (dim == "height")
            span <- convertHeight(unit(distance, "npc") -
                      unit(2, "char"),
                    "npc", valueOnly = TRUE)
        span
    }

    # Calculate dimensions for each box.
    vpW <- c(convertWidth(unit(p, "npc"), "inches", valueOnly = TRUE),
             convertWidth(unit(1 - p, "npc"), "inches", valueOnly = TRUE))
    vpH <- convertHeight(unit(height, "npc"), "inches", valueOnly = TRUE)
    
    if (one.group)
        vpW <- max(vpW)

    # Using absolute values because sometimes the values can be negative
    aspectRatio <- abs(vpW) / abs(vpH)
    # Could use rows but we don't really need it for now
    nCols <- ceiling(sqrt(aspectRatio * ns))
    # nRows <- ceiling(sqrt(ns / aspectRatio))

    # Now that we know the number of columns in total, calculate the number
    # of rows for each group.
    nRows <- ceiling(ns / nCols)

    if (one.group) {
        # Adjusting the number of cols and rows so they best match
        # the size of our dataset
        nCols <- nCols[1]
        nRows <- nRows[1]
        if (nCols * nRows >= ns + nRows)
            nCols <- ceiling(ns / nRows)

        xs <- unit(1, "char") +
                if (empty.group == "right")
                    unit(rep(seq(0, calculatePointSpan(1, "width"),
                                 length.out = nCols), each = nRows), "npc")
                else
                    unit(rep(seq(calculatePointSpan(1, "width"), 0,
                                 length.out = nCols), each = nRows), "npc") 
        xs <- xs[1:ns]
        ys <- unit(1, "char") + unit(y, "npc") +
                 unit(rep(seq(0, calculatePointSpan(height, "height"),
                              length.out = nRows), times = nCols), "npc")
        ys <- ys[1:ns]
        # Returning to our previous VP, the reasoning being explained earlier.
        upViewport(0)

        if (empty.group == "right") {
            leftPoints <- pointsGrob(x = xs,
                                     y = ys,
                                     gp = gpar(col = "#7F7FFF"),
                                     name = "leftPoints")
            leftPropText <- textGrob(ns, x = unit(0.5 * p, "npc"),
                                     y = unit(y + 0.5 * height, "npc"),
                                     name = "leftPropText",
                                     gp = gpar(fontsize = 80, alpha = 0.75,
                                               col = "white", cex = height / 0.45))
            return(gTree(data = data,
                         x = xs, y = ys,
                         height = height, cols = cols, p = p,
                         range = range, name = name, gp = gp, vp = vp,
                         children = gList(leftBar, leftPoints, leftPropText,
                                          rightBar,
                                          statBar,
                                          text1, text2),
                         points.present = TRUE, cl = "propbar"))
        } else {
            rightPoints <- pointsGrob(x = xs,
                                      y = ys,
                                      gp = gpar(col = "#FF3434"),
                                      name = "rightPoints")
            rightPropText <- textGrob(ns, x = unit(p + 0.5 * (1 - p), "npc"),
                                      y = unit(y + 0.5 * height, "npc"),
                                      name = "rightPropText",
                                      gp = gpar(fontsize = 80, alpha = 0.75,
                                                col = "white", cex = height / 0.45))
            return(gTree(data = data,
                         x = xs, y = ys,
                         height = height, cols = cols, p = p,
                         range = range, name = name, gp = gp, vp = vp,
                         children = gList(leftBar,
                                          rightBar, rightPoints, rightPropText,
                                          statBar,
                                          text1, text2),
                          points.present = TRUE, cl = "propbar"))
        }
    }
        
    # We need to trim the number of columns in case the ceiling overestimates
    # the size of each group.
    if (nCols[1] * nRows[1] >= ns[1] + nRows[1]) 
        nCols[1] <- ceiling(ns[1] / nRows[1])
    if (nCols[2] * nRows[2] >= ns[2] + nRows[2]) 
        nCols[2] <- ceiling(ns[2] / nRows[2])

    # Here we calculate point coordinates assuming they occupy the entire
    # grid allocated to them. We simply trim the resulting output to get
    # the desired number of points.

    # Left group
    lxs <- unit(1, "char") +
             unit(rep(seq(0, calculatePointSpan(p, "width"),
                          length.out = nCols[1]), each = nRows[1]), "npc")
    lxs <- lxs[1:ns[1]]
    lys <- unit(1, "char") + unit(y, "npc") +
             unit(rep(seq(0, calculatePointSpan(height, "height"),
                          length.out = nRows[1]), times = nCols[1]), "npc")
    lys <- lys[1:ns[1]]

    # Right group
    rxs <- unit(p, "npc") + unit(1, "char") + unit(halfStatBarWidth, "char") +
             unit(rep(seq(calculatePointSpan(1 - p, "width"), 0,
                          length.out = nCols[2]), each = nRows[2]), "npc")
    rxs <- rxs[1:ns[2]]
    rys <- unit(1, "char") + unit(y, "npc") +
             unit(rep(seq(0, calculatePointSpan(height, "height"),
                          length.out = nRows[2]), times = nCols[2]), "npc")
    rys <- rys[1:ns[2]]

    # Returning to our previous VP, the reasoning being explained earlier.
    upViewport(0)

    leftPoints <- pointsGrob(x = lxs,
                             y = lys,
                             gp = gpar(col = "#7F7FFF"),
                             name = "leftPoints")
    rightPoints <- pointsGrob(x = rxs,
                              y = rys,
                              gp = gpar(col = "#FF3434"),
                              name = "rightPoints")

    # Assigning each observation in the data an x and y unit.
    lindex <- 1
    rindex <- 1
    if (data[1] == levels[1]) {
        xunit <- lxs[1]
        yunit <- lys[1]
        lindex <- 2
    } else {
        xunit <- rxs[1]
        yunit <- rys[1]
        rindex <- 2
    }
    # Ideally we wouldn't need to iteratively build up a unit vector
    # because it is slow.
    for (i in 2:length(data)) {
        if (data[i] == levels[1]) {
            xunit <- unit.c(xunit, lxs[lindex])
            yunit <- unit.c(yunit, lys[lindex])
            lindex <- lindex + 1
        } else {
            xunit <- unit.c(xunit, rxs[rindex])
            yunit <- unit.c(yunit, rys[rindex])
            rindex <- rindex + 1
        }
    }

    gTree(data = data,
          x = xunit, y = yunit,
          height = height, cols = cols, p = p,
          range = range, name = name, gp = gp, vp = vp,
          children = gList(leftBar, leftPoints,
                           leftPropText,
                           rightBar, rightPoints,
                           rightPropText,
                           statBar,
                           if (showBlankRects) leftBlankRect else NULL,
                           if (showBlankRects) rightBlankRect else NULL,
                           text1, text2),
          points.present = TRUE, cl = "propbar")
}

drawDetails.propbar <- function(x, recording){
    for (i in childNames(x)) grid.draw(getGrob(x, i))
}

validDetails.propbar <- function(x) x

editDetails.propbar <- function(x, specs){
    propbarGrob(data = x$data, y = x$y,
                height = x$height, cols = x$cols,
                name = x$name, gp = x$gp, vp = x$vp)
}

grid.propbar.example <- function(data = sample(1:2, size = 50, replace = TRUE, prob = c(0.6, 0.4)),
                                 y = 0, height = 0.2, cols = c("blue", "red"), name = "propbarExample"){
    grid.newpage()
    pbg <- propbarGrob(data = data, y = y, height = height, cols = cols, name = name)
    pushViewport(viewport(height = 0.8, width = 0.8))
    grid.draw(pbg)
    grid.xaxis()
}
