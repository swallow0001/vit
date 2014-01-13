canvasSampvarProp <- setRefClass("canvasSampvarPropClass", contains = "canvasPlotClass",
                                 methods = list(
    calcStat = function(i = which.sample, y = NULL, canvas = .self) {
        calcProportion(samples[[i]], y, canvas)
    },

    calcAllStats = function(a, b = NULL, canvas = .self) {
        calcProportion(a, b, canvas)
    },

    plotSample = function(env, i = which.sample) {
        plotSampvarProportions(.self, env, i)
    },

    showLabels = function() {
        sampvarLabels(.self)
    },

    plotDataStat = function(env, ...) {
        addPropLine(.self, env)
    },

    plotStatDist = function(env, ...) {
        plotBootDist(.self, env)
    },

    animateSample = function(e, n.steps, n.slow, opts) {
        dropPointsProp(.self, n.steps, n.slow,
                       keep.plot = opts$keep.plot, move = opts$move)
    },

    animateStat = function(env, n.steps) {
        dropStat(.self, env, n.steps, linelength = 0.5)
    },

    handle1000 = function(env, ...) {
        sampvar1000prop(.self, env, ...)
    },

    displayResult = function(env, ...) {
        plotSampvarPropTheoDist(.self, env, ...)
    }))

load_sampvar_proportion <- function(e) {
    e$c1$stat.in.use <- svalue(e$stat)
    # There is something messy going on with viewports and
    # gTrees, such that when attempting to import them, we
    # end up with "uninitializedField"s. Assign to a temp
    # var and reassign later.
    tmp.vps <- e$c1$viewports
    tmp.image <- e$c1$image
    e$c1$viewports <- NULL
    e$c1$image <- NULL
    tmp.canvas <- canvasSampvarProp$new()
    tmp.canvas$import(e$c1)
    e$c1 <- tmp.canvas
    e$c1$viewports <- tmp.vps
    e$c1$image <- tmp.image
    e$difference <- FALSE
}

plotSampvarProportions <- function(canvas, e, i) {
    canvas$rmGrobs(c("dataPlot.ci.1", "samplePlot.propbar.1",
                     "samplePlot.stat.1", "samplePlot.ghosts.1", "samplePlot.blank.1"))
    x <- canvas$samples[[i]]
    x[x != canvas$loi] <- canvas$loi.alt

    canvas$sampled.stats <- c(canvas$sampled.stats, i)
    allinfo <- c(canvas$stat.dist, recursive = TRUE)
    alpha <- 0.2

    canvas$image <- addGrob(canvas$image,
                            rectGrob(x = unit(allinfo[canvas$sampled.stats], "npc"),
                                     y = unit(0, "npc"), height = unit(0.3, "npc"),
                                     width = unit(0, "npc"), just = c("left", "bottom"),
                                     gp = gpar(alpha = alpha, col = "blue", lwd = 2),
                                     vp = canvas$graphPath("sample"),
                                     name = "samplePlot.ghosts.1"))
    # We add a blank white rect to obscure most of the ghosting lines
    # so that they are not visible through the propbar.
    canvas$image <- addGrob(canvas$image,
                            rectGrob(x = unit(0, "npc"), y = unit(0.1, "npc"),
                                     width = unit(1, "npc"), height = unit(0.4, "npc"),
                                     just = c("left", "bottom"),
                                     gp = gpar(fill = "white", col = "white"),
                                     vp = canvas$graphPath("sample"),
                                     name = "samplePlot.blank.1"))

    canvas$image <- addGrob(canvas$image,
                            propbarGrob(data = x,
                                        y = 0.1,
                                        height = 0.4,
                                        draw.points = length(canvas$x) < 100,
                                        lois = c(canvas$loi, canvas$loi.alt),
                                        name = "samplePlot.propbar.1",
                                        vp = canvas$graphPath("sample")))

    dp <- getGrob(canvas$image, "dataPlot")
    index <- canvas$indexes[[i]]
    lcol <- "deepskyblue"
    rcol <- "red3"
    cols <- ifelse(x == canvas$loi, lcol, rcol)
    if (dp$points.present) {
        dpx <- dp$x[index]
        dpy <- dp$y[index]
        canvas$image <- addGrob(canvas$image, pointsGrob
                                (x = dpx, y = dpy,
                                 pch = 19, gp = gpar(col = cols),
                                 vp = canvas$graphPath("data"),
                                 name = "samplePlot.datapoints.1"))
    } else {
        # In the case where we don't have access to points,
        # just make up random locations.
        dataProp <- dp$p
        depth <- downViewport("data.data.1")
        point.width <- convertWidth(unit(1, "char"), "npc", valueOnly = TRUE)
        point.height <- convertHeight(unit(1, "char"), "npc", valueOnly = TRUE)
        upViewport(depth)

        # Work out where the points are to be randomly
        # allocated within each "bar"
        leftBar.left <- point.width
        leftBar.right <- dataProp - point.width
        rightBar.left <- dataProp + point.width
        rightBar.right <- 1 - point.width
        bar.top <- 0.45 + 0.1 - point.height
        bar.bottom <- 0.1 + point.height

        # Show no more than 100 points
        n.points <- min(100, canvas$n)
        left.group <- x == canvas$loi
        n.left <- if (canvas$n > 100) round(dataProp * n.points)
                  else length(x[left.group])
        n.right <- if (canvas$n > 100) round((1 - dataProp) * n.points)
                   else length(x[!left.group])

        # Generate left locations
        lpx <- runif(n.left, leftBar.left, leftBar.right)
        lpy <- runif(n.left, bar.bottom, bar.top)
        # Generate right locations
        rpx <- runif(n.right, rightBar.left, rightBar.right)
        rpy <- runif(n.right, bar.bottom, bar.top)

        # Vector of indices to select points from
        point.inds <- sample(n.points)
        data.py <- c(lpy, rpy)[point.inds]
        data.px <- c(lpx, rpx)[point.inds]
        cols <- ifelse(data.px < dataProp, lcol, rcol)

        # Overlaying the sampled data points
        canvas$image <- addGrob(canvas$image, pointsGrob
                                (x = unit(data.px, "npc"), y = unit(data.py, "npc"),
                                 pch = 19, gp = gpar(col = cols),
                                 vp = canvas$graphPath("data"),
                                 name = "samplePlot.datapoints.1"))
    }

    canvas$rmGrobs("samplePlot.blank.1")
}

sampvar1000prop <- function(canvas, e, points = FALSE) {
    # Starting from scratch with ghosting & points
    canvas$sampled.stats <- NULL
    canvas$plotted.stats <- NULL
    canvas$rmGrobs(c("dataPlot.ci.1", "samplePlot.blank.1", "samplePlot.ghosts.1"))
    allx <- c(canvas$stat.dist, recursive = TRUE)
    y.max <- unit(1, "npc") - unit(2, "lines") - unit(0.5, "char")

    for (i in 20*(1:50)) {
        x <- canvas$samples[[i]]
        xs <- allx[1:i] # All of the proportions so far
        y <- canvas$stat.ypos[1:i]
        if (points)
            plotPoints(canvas, xs, y, canvas$graphPath("stat"),
                       "statPlot", black = FALSE, alpha = 0.7)
        canvas$rmGrobs("samplePlot.propbar.1")
        alpha <- 0.02
        canvas$image <- addGrob(canvas$image,
                                rectGrob(x = unit(xs, "npc"),
                                         y = unit(0, "npc"), height = unit(0.3, "npc"),
                                         width = unit(0, "npc"), just = c("left", "bottom"),
                                         gp = gpar(alpha = alpha, col = "blue", lwd = 2),
                                         vp = canvas$graphPath("sample"),
                                         name = "samplePlot.ghosts.1"))
        # We add a blank white rect to obscure most of the ghosting lines
        # so that they are not visible through the propbar.
        canvas$image <- addGrob(canvas$image,
                                rectGrob(x = unit(0, "npc"), y = unit(0.1, "npc"),
                                         width = unit(1, "npc"), height = unit(0.4, "npc"),
                                         just = c("left", "bottom"),
                                         gp = gpar(fill = "white", col = "white"),
                                         vp = canvas$graphPath("sample"),
                                         name = "samplePlot.blank.1"))
        canvas$plotSample(e, i)
        canvas$showLabels()
        if (canvas$stopAnimation)
            return()
        canvas$drawImage()
    }

    canvas$rmGrobs(c("samplePlot.blank.1", "samplePlot.propbar.1", "samplePlot.datapoints.1"))
    canvas$drawImage()
    ## Remove 1000 statistics next time something is plotted to avoid
    ## further statistics being plotted on top.
    #canvas$image <- removeGrob(canvas$image, gPath("statPlot.points.1"))
    ## Reset CI counter
    canvas$sampled.stats <- NULL
    canvas$plotted.stats <- NULL
}

plotSampvarPropTheoDist <- function(canvas, e) {
    ## Replotting statistic distribution
    x <- c(canvas$stat.dist, recursive = TRUE)
    y <- canvas$stat.ypos
    plotPoints(canvas, x, y, canvas$graphPath("stat"),
               "statPlot", black = FALSE, alpha = 0.7)
    stat.mean <- mean(x)
    stat.sd <- sd(x)
    mean <- mean(canvas$x == canvas$loi)
    sd <- sqrt(mean * (1 - mean) / canvas$n) *
          sqrt((length(canvas$x) - canvas$n) / length(canvas$x)) # FPC
    ## Getting statistic panel x-scale.
    top.level <- downViewport(canvas$graphPath("stat"))
    stat.scale <- current.viewport()$xscale
    upViewport(top.level)
    ## Calculating normal density under the CLT.
    xs <- seq(stat.scale[1], stat.scale[2], length.out = 300)
    ys <- dnorm(xs, mean, sd)
    ## We need a sense of "density scale" for the y-axis. Fitting a
    ## kernel density estimator can provide this. We calculate the
    ## KDE, find the maximum point, map this to meet up with the top
    ## of the topmost point in the statistic panel, and scale the
    ## normal density curve accordingly. This ensures that the normal
    ## density curve has about the same area below it as the area
    ## taken up by the points; the normal density will have the same
    ## area as the KDE, which, in turn, will have a similar area to
    ## the points.
    dens <- density(x, from = stat.scale[1], to = stat.scale[2])
    ys <- ys/max(dens$y)
    y.max <- unit(1, "npc") - unit(2, "lines") - unit(0.5, "char")
    canvas$image <- addGrob(canvas$image, linesGrob
                            (x = unit(xs, "native"), y = (y.max*ys) + ys*unit(0.5, "char"),
                             gp = gpar(lwd = 2, col = "red"),
                             name = "statPlot.theodist.1",
                             vp = canvas$graphPath("stat")))
    canvas$showLabels()
    canvas$drawImage()
    canvas$rmGrobs(c("samplePlot.points.1", "samplePlot.lines.1",
                     "samplePlot.datapoints.points.1", "samplePlot.databox.text.2",
                     "samplePlot.stat.1", "samplePlot.hist.1"))
}
