
grid.kpropbar <- function(...)
    grid.draw(propbarGrob(...))

# kpropbarGrob constructs a proportion bar of vectors of categorical data.
# It differs from a propbarGrob by coping with 2 dimensions of categorical
# data.
kpropbarGrob <- function(data, ylab = "", ylab.pos = 1, y = 0,
                         statLineHeight = 0.1, height = 0.2,
                         cols = c("blue", "red"), ylab.col = "black",
                         draw.points = TRUE,
                         vpcex = 0.5, lois = NULL, name = NULL,
                         gp = NULL, vp = NULL) {
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
    # The same is the case with stringHeights and the animation.field VP.
    # Thus, we calculate stringHeight in the animation.field viewport when
    # animating the kpropbarGrob
    if (vp$name == "animation.field") {
        seekViewport(vp$name)
        ylabHeight <- convertHeight(stringHeight(ylab), "native", valueOnly = TRUE)
        upViewport(0)
        seekViewport("data.data.1")
    } else {
        seekViewport("data.data.1")
        ylabHeight <- convertHeight(stringHeight(ylab), "native", valueOnly = TRUE)
    }

    levels <- lois
    data <- as.character(data)
    ns <- length(data[data == levels[1]])
    ns <- c(ns, length(data) - ns)
    one.group <- any(ns == 0)
    empty.group <- if (ns[1] == 0) "left" else "right"
    alpha.anim <- if (! is.null(gp)) gp$alpha != 1
                  else FALSE
    p <- ns[1] / length(data)
    sdP <- sqrt(p*(1 - p) / sum(ns))
    range <- p + c(-1, 1)*5*sdP
    if (range[1] < 0) range[1] = 0
    if (range[2] > 1) range[2] = 1

    ytext <- textGrob(ylab, x = unit(0, "native") + unit(2, "mm"),
                      y = unit(y + 0.5, "native"), name = "ytext")
    statBar <- linesGrob(x = rep(unit(p, "native"), 2),
                         y = unit(c(y - statLineHeight, height + y + statLineHeight), "native"),
                         gp = gpar(col = "black", lwd = 3),
                         name = "statBar")
    lbgp <- if (alpha.anim)
                gpar(fill = cols[1], col = cols[1], alpha = 0.8)
            else
                gpar(fill = cols[1], col = cols[1])
    leftBar <- rectGrob(x = unit(0, "native"), y = unit(y, "native"),
                        width = unit(p, "native"), height = unit(height, "native"),
                        just = c("left", "bottom"), name = "leftBar",
                        gp = lbgp)
    leftPropText <- textGrob(ns[1], x = unit(0.5 * p, "native"),
                             y = unit(y + 0.5 * height, "native"),
                             name = "leftPropText",
                             gp = gpar(fontsize = 80, alpha = 0.75,
                                       col = "white", cex = vpcex))
    rbgp <- if (alpha.anim)
                gpar(fill = cols[2], col = cols[2],
                     alpha = if (draw.points) 0.1 else 0.2) # Dirty hack to ensure colours remain consistent
                                                             # when points are present or not
            else
                gpar(fill = "#FFCFCF", col = "#FFCFCF")
    rightBar <- rectGrob(x = unit(p, "native"), y = unit(y, "native"),
                         width = unit(1 - p, "native"), height = unit(height, "native"),
                         just = c("left", "bottom"), name = "rightBar",
                         gp = rbgp)
    rightPropText <- textGrob(ns[2], x = unit(p + 0.5 * (1 - p), "native"),
                              y = unit(y + 0.5 * height, "native"),
                              name = "rightPropText",
                              gp = gpar(fontsize = 80, alpha = 0.75,
                                        col = "white", cex = vpcex))
    ylabText <- textGrob(ylab,
                         x = unit(ylab.pos, "native") - unit(2, "mm"),
                         y = unit(y, "native") + unit(1, "mm"),
                         gp = gpar(col = ylab.col),
                         just = c("left", "bottom"), name = "ylabText")
    ylabText$y <- unit((height - ylabHeight) / 2, "native") + unit(y, "native")
    ylabBackground <- roundrectGrob(x = unit(ylab.pos, "native") - unit(3, "mm"),
                                    y = ylabText$y - unit(1, "mm"),
                                    width = grobWidth(ylabText) + unit(2, "mm"),
                                    height = grobHeight(ylabText) + unit(2, "mm"),
                                    just = c("left", "bottom"),
                                    gp = gpar(col = "white", fill = "white", alpha = 0.5),
                                    name = "ylabBackground")
    ylevelText <- textGrob(ylab, x = 1, y = unit(height + y + ylabHeight, "native"),
                           gp = gpar(col = ylab.col),
                           just = c("right", "bottom"), name = "ylevelText")

    if (one.group)
        ns <- max(ns)

    # When we have the case where the boundary rectangle is smaller than
    # the area needed to hold text, don't bother drawing text.
    leftBoxWidth <- convertWidth(grobWidth(leftBar), "native", valueOnly = TRUE)
    leftTextWidth <- convertWidth(stringWidth(leftPropText$label), "native", valueOnly = TRUE)
    rightBoxWidth <- convertWidth(grobWidth(rightBar), "native", valueOnly = TRUE)
    rightTextWidth <- convertWidth(stringWidth(rightPropText$label), "native", valueOnly = TRUE)
    hide.lefttext <- leftTextWidth > leftBoxWidth
    hide.righttext <- rightTextWidth > rightBoxWidth

    # If we have a large amount of observations, point calculation is too slow.
    # Moreover, the points become less useful visually, thus just draw a propbar
    # with points removed.
    if (sum(ns) > 500 | !draw.points) {
        # Returning to our previous VP, the reasoning being explained earlier.
        upViewport(0)

        # Here we define the child elements we wish to include subject to
        # the following constraints:
        # - Only draw label text if we have actual characters to draw
        # - Draw proportion text only when there are non-zero counts in each level
        if (nchar(ylab) > 0) {
            if (one.group) {
                if (empty.group == "right") {
                    gchild <- gList(leftBar, leftPropText,
                                    rightBar,
                                    statBar,
                                    ylabBackground, ylabText)
                } else {
                    rightPropText <- textGrob(ns, x = unit(p + 0.5 * (1 - p), "native"),
                                              y = unit(y + 0.5 * height, "native"),
                                              name = "rightPropText",
                                              gp = gpar(fontsize = 80, alpha = 0.75,
                                                        col = "white", cex = vpcex))
                    gchild <- gList(leftBar,
                                    rightBar, rightPropText,
                                    statBar,
                                    ylabBackground, ylabText)
                }
            } else
                gchild <- gList(leftBar, if (hide.lefttext) NULL else leftPropText,
                                rightBar, if (hide.righttext) NULL else rightPropText,
                                statBar,
                                ylabBackground, ylabText)
        } else {
            if (one.group) {
                if (empty.group == "right") {
                    gchild <- gList(leftBar, leftPropText,
                                    rightBar,
                                    statBar)
                } else {
                    rightPropText <- textGrob(ns, x = unit(p + 0.5 * (1 - p), "native"),
                                              y = unit(y + 0.5 * height, "native"),
                                              name = "rightPropText",
                                              gp = gpar(fontsize = 80, alpha = 0.75,
                                                        col = "white", cex = vpcex))
                    gchild <- gList(leftBar,
                                    rightBar, rightPropText,
                                    statBar)
                }
            } else
                gchild <- gList(leftBar, if (hide.lefttext) NULL else leftPropText,
                                rightBar, if (hide.righttext) NULL else rightPropText,
                                statBar)
        }

        # Adding the required children and returning the tree
        return(gTree(data = data,
                     x = unit(0, "native"), y = unit(0, "native"),
                     height = height, cols = cols, p = p,
                     range = range, name = name, gp = gp, vp = vp,
                     children = gchild,
                     points.present = FALSE, class = "propbar"))
    }

    halfStatBarWidth <- 0.5 * convertX(grobWidth(statBar),
                                       "char", valueOnly = TRUE)

    # This calculates the distance that the sequence of point coordinates
    # should span
    calculatePointSpan <- function(distance, dim = "width") {
        if (dim == "width")
            span <- convertWidth(unit(distance, "native") -
                      unit(2, "char") - unit(halfStatBarWidth, "char"),
                    "native", valueOnly = TRUE)
        if (dim == "height")
            span <- convertHeight(unit(distance, "native") -
                      unit(2, "char"),
                    "native", valueOnly = TRUE)
        span
    }

    # Calculate dimensions for each box.
    vpW <- c(convertWidth(unit(p, "native"), "inches", valueOnly = TRUE),
             convertWidth(unit(1 - p, "native"), "inches", valueOnly = TRUE))
    vpH <- convertHeight(unit(height, "native"), "inches", valueOnly = TRUE)
    
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

    # Creating points gps as they are used in a couple of places
    lpgp <- if (alpha.anim) gpar(col = "white", alpha = 0.5)
            else gpar(col = "#7F7FFF")
    rpgp <- if (alpha.anim) gpar(col = cols[2], alpha = 0.5)
            else gpar(col = "#FF3434")

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
                                 length.out = nCols), each = nRows), "native")
                else
                    unit(rep(seq(calculatePointSpan(1, "width"), 0,
                                 length.out = nCols), each = nRows), "native") 
        xs <- xs[1:ns]
        ys <- unit(1, "char") + unit(y, "native") +
                 unit(rep(seq(0, calculatePointSpan(height, "height"),
                              length.out = nRows), times = nCols), "native")
        ys <- ys[1:ns]
        # Returning to our previous VP, the reasoning being explained earlier.
        upViewport(0)

        if (empty.group == "right") {
            leftPoints <- pointsGrob(x = xs,
                                     y = ys,
                                     gp = lpgp,
                                     name = "leftPoints")
            leftPropText <- textGrob(ns, x = unit(0.5 * p, "native"),
                                     y = unit(y + 0.5 * height, "native"),
                                     name = "leftPropText",
                                     gp = gpar(fontsize = 80, alpha = 0.75,
                                               col = "white", cex = vpcex))
            return(gTree(data = data,
                         x = xs, y = ys,
                         height = height, cols = cols, p = p,
                         range = range, name = name, gp = gp, vp = vp,
                         children = gList(leftBar, leftPoints, leftPropText,
                                          rightBar,
                                          statBar,
                                          ylevelText),
                         points.present = TRUE, cl = "propbar"))
        } else {
            rightPoints <- pointsGrob(x = xs,
                                      y = ys,
                                      gp = rpgp,
                                      name = "rightPoints")
            rightPropText <- textGrob(ns, x = unit(p + 0.5 * (1 - p), "native"),
                                      y = unit(y + 0.5 * height, "native"),
                                      name = "rightPropText",
                                      gp = gpar(fontsize = 80, alpha = 0.75,
                                                col = "white", cex = vpcex))
            return(gTree(data = data,
                         x = xs, y = ys,
                         height = height, cols = cols, p = p,
                         range = range, name = name, gp = gp, vp = vp,
                         children = gList(leftBar,
                                          rightBar, rightPoints, rightPropText,
                                          statBar,
                                          ylevelText),
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
                          length.out = nCols[1]), each = nRows[1]), "native")
    lxs <- lxs[1:ns[1]]
    lys <- unit(1, "char") + unit(y, "native") +
             unit(rep(seq(0, calculatePointSpan(height, "height"),
                          length.out = nRows[1]), times = nCols[1]), "native")
    lys <- lys[1:ns[1]]

    # Right group
    rxs <- unit(1, "char") + unit(halfStatBarWidth, "char") +
             unit(p + rep(seq(calculatePointSpan(1 - p, "width"), 0,
                          length.out = nCols[2]), each = nRows[2]), "native")
    rxs <- rxs[1:ns[2]]
    rys <- unit(1, "char") + unit(y, "native") +
             unit(rep(seq(0, calculatePointSpan(height, "height"),
                          length.out = nRows[2]), times = nCols[2]), "native")
    rys <- rys[1:ns[2]]

    # Returning to our previous VP, the reasoning being explained earlier.
    upViewport(0)

    leftPoints <- pointsGrob(x = lxs,
                             y = lys,
                             gp = lpgp,
                             name = "leftPoints")
    rightPoints <- pointsGrob(x = rxs,
                              y = rys,
                              gp = rpgp,
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

    # We can fit everything we want into our propbar, return it.
    gTree(data = data,
          x = xunit, y = yunit,
          height = height, cols = cols, p = p,
          range = range, name = name, gp = gp, vp = vp,
          children = gList(leftBar, leftPoints,
                           rightBar, rightPoints,
                           statBar,
                           if (hide.lefttext) NULL else leftPropText,
                           if (hide.righttext) NULL else rightPropText,
                           ylevelText),
          points.present = TRUE, cl = "propbar")
}

drawDetails.kpropbar <- function(x, recording){
    for (i in childNames(x)) grid.draw(getGrob(x, i))
}

validDetails.kpropbar <- function(x) x

editDetails.kpropbar <- function(x, specs) {
    kpropbarGrob(data = x$data,
                 ylab = x$ylab, ylab.pos = x$ylab.pos,
                 y = x$y, height = x$height, cols = x$cols,
                 name = x$name, gp = x$gp, vp = x$vp,
                 draw.points = x$draw.points)
}

grid.kpropbar.example <- function(data = sample(1:2, size = 50, replace = TRUE, prob = c(0.6, 0.4)),
                                 y = 0, height = 0.2, cols = c("blue", "red"), name = "propbarExample"){
    grid.newpage()
    pbg <- kpropbarGrob(data = data, y = y, height = height, cols = cols, name = name)
    pushViewport(viewport(height = 0.8, width = 0.8))
    grid.draw(pbg)
    grid.xaxis()
}
