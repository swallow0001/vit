canvasCIProp <- setRefClass("canvasCIPropClass", contains = "canvasPlotClass",
                            methods = list(
    calcStat = function(i = which.sample, y = NULL, canvas = .self) {
        if (stat.method == "normal: +/- t s.e.") {
            calcPropTCIWald(samples[[i]], y, canvas)
        } else if (stat.method == "normal: +/- 2 s.e.") {
            calcPropCI2Wald(samples[[i]], y, canvas)
        } else if (stat.method == "normal: +/- 1/sqrt(n)") {
            calcPropCIRootN(samples[[i]], y, canvas)
        } else if (stat.method == "bootstrap: percentile") {
            calcCIBootPercProp(samples[[i]], y, canvas)
        } else if (stat.method == "bootstrap: +/- 2 s.e.") {
            calcCIBootSEMeanProp(samples[[i]], y, canvas)
        } else if (stat.method == "bootstrap: +/- t s.e.") {
            calcCIBootTSEMeanProp(samples[[i]], y, canvas)
        }
    },

    calcAllStats = function(x, y = NULL, canvas = .self) {
        if (stat.method == "normal: +/- t s.e.") {
            calcPropTCIWald(x, y, canvas)
        } else if (stat.method == "normal: +/- 2 s.e.") {
            calcPropCI2Wald(x, y, canvas)
        } else if (stat.method == "normal: +/- 1/sqrt(n)") {
            calcPropCIRootN(x, y, canvas)
        } else if (stat.method == "bootstrap: percentile") {
            calcCIBootPercProp(x, y, canvas)
        } else if (stat.method == "bootstrap: +/- 2 s.e.") {
            calcCIBootSEMeanProp(x, y, canvas)
        } else if (stat.method == "bootstrap: +/- t s.e.") {
            calcCIBootTSEMeanProp(x, y, canvas)
        }
    },

    plotSample = function(env, i = which.sample) {
        plotCISampleProportions(.self, env, i)
    },

    showLabels = function() {
        ciLabels(.self)
    },

    plotDataStat = function(env, ...) {
        addPropLine(.self, env)
    },

    plotSampleStat = function(env, i = which.sample, ...) {
        plotPropCI(.self, env, i, ...)
    },

    plotStatDist = function(env, ...) {
        plotCIDistPropMean(.self, env)
    },

    animateSample = function(...) {
        dropPointsProp(.self, ...)
    },

    animateStat = function(env, n.steps) {
        dropCI(.self, env, n.steps)
    },

    displayResult = function(env, cov.message) {
        CIcounter(.self, env, cov.message, fun = mean)
    },

    handle1000 = function(env, ...) {
        ci1000(.self, env, fun = NULL)
    }))

load_CI_proportion <- function(e) {
    confidence_check(e)
    e$c1$stat.in.use <- svalue(e$stat)
    e$cimethod <- svalue(e$cimeth)
    e$c1$stat.method <- e$cimethod
    # There is something messy going on with viewports and
    # gTrees, such that when attempting to import them, we
    # end up with "uninitializedField"s. Assign to a temp
    # var and reassign later.
    tmp.vps <- e$c1$viewports
    tmp.image <- e$c1$image
    e$c1$viewports <- NULL
    e$c1$image <- NULL
    tmp.canvas <- canvasCIProp$new()
    tmp.canvas$import(e$c1)
    e$c1 <- tmp.canvas
    e$c1$viewports <- tmp.vps
    e$c1$image <- tmp.image
    e$results <- NULL
}

plotCISampleProportions <- function(canvas, e, i) {
    canvas$rmGrobs(c("dataPlot.ci.1", "samplePlot.propbar.1", "samplePlot.stat.1"))
    x <- canvas$samples[[i]]
    x[x != canvas$loi] <- canvas$loi.alt
    canvas$image <- addGrob(canvas$image,
                            propbarGrob(data = x,
                                        y = 0.1,
                                        height = 0.4,
                                        draw.points = length(canvas$x) < 100,
                                        lois = c(canvas$loi, canvas$loi.alt),
                                        name = "samplePlot.propbar.1",
                                        vp = canvas$graphPath("sample")))
}

calcPropTCIWald <- function(x, y = NULL, canvas) {
    n <- length(x)
    x <- as.character(x)
    loiN <- length(x[x == canvas$loi])
    prop <- loiN / n
    se <- sqrt((prop * (1 - prop)) / n)
    prop + c(-1, 1) * qt(0.975, n - 1) * se
}

calcPropCI2Wald <- function(x, y = NULL, canvas) {
    n <- length(x)
    x <- as.character(x)
    loiN <- length(x[x == canvas$loi])
    prop <- loiN / n
    se <- sqrt((prop * (1 - prop)) / n)
    prop + c(-2, 2)*se
}

calcPropCIRootN <- function(x, y = NULL, canvas) {
    n <- length(x)
    x <- as.character(x)
    prop <- mean(x == canvas$loi)
    prop + c(-1, 1) * (1 / sqrt(n))
}

calcCIBootPercProp <- function(x, y = NULL, canvas) {
    n <- length(x)
    nboots <- 999
    samps <- matrix(sample(x, size = nboots*n, replace = TRUE),
                    nrow = nboots, ncol = n)
    props <- apply(samps, 1, function(obs) {
        obsx <- as.character(obs)
        loiN <- length(obsx[obsx == canvas$loi])
        prop <- loiN / length(obs)
        prop
    })
    quantile(props, prob = c(0.025, 0.975), type = 1)
}

calcCIBootSEMeanProp <- function(x, y = NULL, canvas) {
    n <- length(x)
    nboots <- 1000
    samps <- matrix(sample(x, size = nboots*n, replace = TRUE),
                    nrow = nboots, ncol = n)
    props <- apply(samps, 1, function(obs) {
        obsx <- as.character(obs)
        loiN <- length(obsx[obsx == canvas$loi])
        prop <- loiN / length(obs)
        prop
    })
    se <- sd(props)
    mean(props) + c(-2, 2) * se
}

calcCIBootTSEMeanProp <- function(x, y = NULL, canvas) {
    n <- length(x)
    nboots <- 1000
    samps <- matrix(sample(x, size = nboots*n, replace = TRUE), nrow = nboots,
                    ncol = n)
    props <- apply(samps, 1, function(obs) {
        obsx <- as.character(obs)
        loiN <- length(obsx[obsx == canvas$loi])
        prop <- loiN / length(obs)
        prop
    })
    se <- sd(props)
    mean(props) + c(-1, 1) * qt(0.975, n - 1) * se
}

addPropLine <- function(canvas, e) {
    x <- canvas$loi.data
    n <- length(x)
    loiN <- length(x[x == canvas$loi])
    prop <- loiN / n
    canvas$image <- addGrob(canvas$image,
                            segmentsGrob(x0 = prop, x1 = prop, y0 = 0,
                                         y1 = 3, default.units = "native",
                                         gp = gpar(col = "grey60", lty = "dashed"),
                                         vp = canvas$graphPath("animation.field"),
                                         name = "hline"))
}

plotPropCI <- function(canvas, e, i, pause = FALSE) {
    if (pause){
        method <- strsplit(svalue(e$cimeth), ":")[[1]][1]
        ciCalcLabel <- textGrob(paste("Calculating", method, "CI..."),
                                x = unit(0.5, "npc"), y = unit(0.6, "npc"),
                                just = c("centre", "top"), vp = canvas$graphPath("sample"),
                                gp = gpar(fontface = 2), name = "samplePlot.ciCalcLabel")
        canvas$image <- addGrob(canvas$image, ciCalcLabel)
        if (canvas$stopAnimation)
            return()
        canvas$pauseImage(15)
        canvas$rmGrobs("samplePlot.ciCalcLabel")
    }
    bounds <- canvas$stat.dist[[i]]
    x <- mean(bounds)
    if (x == 0 | x == 1) {
        canvas$image <- addGrob(canvas$image,
                                rectGrob(x = unit(x, "native"), y = unit(0.2, "native"),
                                         width = unit(0.005, "native"),
                                         height = unit(0.03, "native"),
                                         gp = gpar(col = "red", fill = "red"),
                                         vp = canvas$graphPath("sample"),
                                         name = "samplePlot.stat.1"))
    } else {
        canvas$image <- addGrob(canvas$image,
                                rectGrob(x = unit(x, "native"), y = unit(0.2, "native"),
                                         width = unit(diff(bounds), "native"),
                                         height = unit(0.015, "native"),
                                         gp = gpar(col = "#FF7F00",
                                         fill = "#FF7F00"), vp = canvas$graphPath("sample"),
                                         name = "samplePlot.stat.1"))
    }
}

plotCIDistPropMean <- function(canvas, e) {
    i <- canvas$which.sample
    bounds <- canvas$getStat(i)
    x <- mean(bounds)
    data <- as.character(canvas$loi.data)
    n <- length(data)
    loiN <- length(data[data == canvas$loi])
    X <- loiN / n

    if (X >= bounds[1] & X <= bounds[2]) color <- "green" else color <- "red"
    current <- data.frame(x = x, width = diff(c(bounds)), color = color)

    if ("statPlot.stat.dist" %in% childNames(canvas$image)) {
        dist.grob <- getGrob(canvas$image, gPath(c("statPlot.stat.dist")))
        dist.df <- dist.grob$data
        if (nrow(dist.df) >= 40) dist.df <- dist.df[-1,]
        dist.df <- rbind(dist.df[, -4], current)
    } else dist.df <- current

    dist.df$y <- 0.02 * 1:nrow(dist.df)
    green <- dist.df[dist.df$color == "green",]
    red <- dist.df[dist.df$color == "red",]

    if (nrow(green) > 0) {
        greenRects <- rectGrob(x = unit(green$x, "native"),
                               y = unit(green$y, "native"), width = unit(green$width, "native"),
                               height = unit(0.015, "native"), vp = canvas$graphPath("stat"),
                               gp = gpar(col = NA, fill = "green"))
    } else greenRects <- NULL

    if (nrow(red) > 0) {
        red$width <- ifelse(red$width == 0 | red$width == 1, 0.005, red$width)
        redRects <- rectGrob(x = unit(red$x, "native"),
                             y = unit(red$y, "native"), width = unit(red$width, "native"),
                             height = unit(0.015, "native"), vp = canvas$graphPath("stat"),
                             gp = gpar(col = NA, fill = "red"))
    } else redRects <- NULL

    new.dist <- gTree(data = dist.df, name = "statPlot.stat.dist",
                      childrenvp = canvas$viewports, children = gList(greenRects, redRects))

    canvas$image <- addGrob(canvas$image, new.dist)
}

dropPointsProp <- function(canvas, n.steps, n.slow, keep.plot, move = TRUE) {
    canvas$rmGrobs(c("samplePlot.points.1", "samplePlot.points",
                     "samplePlot.propbar.1", "samplePlot.stat.1", "samplePlot.datapoints.1"))
    index <- canvas$indexes[[canvas$which.sample]]
    x <- canvas$x[index]

    if (move) {
        sampSelectLabel <- textGrob("Selecting sample...", x = unit(0.5, "npc"), y = unit(0.6, "npc"),
                                    just = c("centre", "top"), vp = canvas$graphPath("sample"),
                                    gp = gpar(fontface = 2), name = "samplePlot.sampSelectLabel")
        canvas$image <- addGrob(canvas$image, sampSelectLabel)
    }

    # Grabbing point positions from data prop grob
    dp <- getGrob(canvas$image, gPath("dataPlot"))

    # When we have points available to us, attempt to accurately
    # animate between the data and the sample.
    if (dp$points.present) {
        dpx <- dp$x[index]
        dpy <- dp$y[index]
        seekViewport("data.data.1")
        dpx <- convertX(dpx, "native", valueOnly = TRUE)
        dpy <- convertY(dpy, "native", valueOnly = TRUE)
        upViewport(0)

        # Creating sample prop grob to get point positions from
        sp <- propbarGrob(data = x,
                          y = 0.1,
                          height = 0.4,
                          lois = c(canvas$loi, canvas$loi.alt),
                          name = "samplePlot",
                          vp = canvas$graphPath("sample"))
        spx <- sp$x
        spy <- sp$y
        seekViewport("sample.data.1")
        spx <- convertX(spx, "native", valueOnly = TRUE)
        spy <- convertY(spy, "native", valueOnly = TRUE)
        upViewport(0)

        y.start <- dpy + 2
        y.end <- spy + 1
        y.step <- (y.end - y.start)/n.steps
        x.step <- (spx - dpx) / n.steps
        n.slow <- min(n.slow, length(x))

        # Lighting up of sampled points.
        cols <- ifelse(x == canvas$loi, "deepskyblue", "red3")
        if (move) {
            for (i in 1:length(x)) {
                canvas$image <- addGrob(canvas$image,
                                        pointsGrob(dpx[1:i], y = dpy[1:i],
                                                   vp = canvas$graphPath("data"),
                                                   pch = 19,
                                                   gp = gpar(col = cols),
                                                   name = "samplePlot.data.samp"))
                speed <- if (i <= n.slow) 10 else 1
                if (canvas$stopAnimation)
                    return()
                canvas$pauseImage(speed)
            }
            if (canvas$stopAnimation)
                return()
            canvas$pauseImage(20)
        }
        canvas$image <- addGrob(canvas$image,
                                pointsGrob(dpx, y = dpy, vp = canvas$graphPath("data"),
                                           pch = 19,
                                           gp = gpar(col = cols),
                                           name = "samplePlot.data.samp"))

        # Dropping of points.
        if (move){
            for (i in 1:n.steps){
                y.pos <- y.start + (y.step * i)
                x.pos <- dpx + (x.step * i)
                canvas$image <- addGrob(canvas$image,
                                        pointsGrob(x.pos, y.pos, vp = canvas$graphPath("animation.field"),
                                                   gp = gpar(col = cols),
                                                   pch = 19, name = "samplePlot.temp"))
                if (canvas$stopAnimation)
                    return()
                canvas$drawImage()
            }
            canvas$pauseImage(10)
            canvas$rmGrobs(c("samplePlot.sampSelectLabel", "samplePlot.temp"))
            plotCISampleProportions(canvas, NULL, canvas$which.sample)
            canvas$pauseImage(10)
        }
        canvas$image <- removeGrob(canvas$image, gPath("samplePlot.data.samp"))
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

        # Creating sample prop grob to get prop from
        sp <- propbarGrob(data = x,
                          y = 0.1,
                          height = 0.4,
                          draw.points = FALSE,
                          lois = c(canvas$loi, canvas$loi.alt),
                          name = "samplePlot",
                          vp = canvas$graphPath("sample"))
        sampleProp <- sp$p

        # Show no more than 100 points
        n.points <- min(100, length(x))
        n.left <- round(sampleProp * n.points)
        n.right <- round((1 - sampleProp) * n.points)

        # Generate left locations
        lpx <- runif(n.left, leftBar.left, leftBar.right)
        lpy <- runif(n.left, bar.bottom, bar.top)
        # Generate right locations
        rpx <- runif(n.right, rightBar.left, rightBar.right)
        rpy <- runif(n.right, bar.bottom, bar.top)

        # Will need to scale points to that they are able to
        # fit within the sample propbar nicely.
        # Will either be squeezing them closer together or spreading them apart
        # horizontally, but vertically they will always be closer together.
        # Scale factor from data -> sample
        v.scale.factor <- (0.4 - 2*point.height) / (0.45 - 2*point.height)
        left.scale.factor <- (sampleProp - 2*point.width) / (dataProp - 2*point.width)
        right.scale.factor <- ((1 - sampleProp) - 2*point.width) / ((1 - dataProp) - 2*point.width)

        # Turning into animation.field coordinates
        lpy <- lpy + 2
        rpy <- rpy + 2
        # Vector of indices to select points from
        point.inds <- sample(n.points)
        data.py <- c(lpy, rpy)[point.inds]
        data.px <- c(lpx, rpx)[point.inds]
        sample.py <- ((data.py - 2 - point.height) * v.scale.factor) + 1 + point.height
        sample.px <- sapply(data.px, function(x) {
            if (x > dataProp) {
                # Right group
                ((x - dataProp - point.width) * right.scale.factor) + point.width + sampleProp
            } else {
                # Left group
                ((x - point.width) * left.scale.factor) + point.width
            }
        })

        # Lighting up of sampled points.
        cols <- ifelse(point.inds > n.left, "red3", "deepskyblue")
        if (move) {
            for (i in 1:n.points) {
                canvas$image <- addGrob(canvas$image,
                                        pointsGrob(data.px[1:i], y = data.py[1:i],
                                                   vp = canvas$graphPath("animation.field"),
                                                   pch = 19,
                                                   gp = gpar(col = cols),
                                                   name = "samplePlot.data.samp"))
                speed <- if (i <= n.slow) 10 else 1
                if (canvas$stopAnimation)
                    return()
                canvas$pauseImage(speed)
            }
            # Force pause before points drop.
            if (canvas$stopAnimation)
                return()
            canvas$pauseImage(20)
        }
        canvas$image <- addGrob(canvas$image,
                                pointsGrob(x = data.px, y = data.py,
                                           vp = canvas$graphPath("animation.field"),
                                           pch = 19,
                                           gp = gpar(col = cols),
                                           name = "samplePlot.data.samp"))

        x.step <- (sample.px - data.px) / n.steps
        y.step <- (sample.py - data.py) / n.steps

        # Dropping of points.
        if (move){
            for (i in 1:n.steps) {
                y.pos <- data.py + (y.step * i)
                x.pos <- data.px + (x.step * i)
                dropped.points <- pointsGrob(x.pos, y.pos,
                                             vp = canvas$graphPath("animation.field"),
                                             gp = gpar(col = cols),
                                             pch = 19, name = "samplePlot.temp")
                canvas$image <- addGrob(canvas$image, dropped.points)
                if (canvas$stopAnimation)
                    return()
                canvas$drawImage()
            }
            canvas$pauseImage(10)
            plotCISampleProportions(canvas, NULL, canvas$which.sample)
            canvas$image <- addGrob(canvas$image, dropped.points)
            if (canvas$stopAnimation)
                return()
            canvas$pauseImage(20)
            canvas$rmGrobs(c("samplePlot.sampSelectLabel", "samplePlot.temp"))
        }
        canvas$rmGrobs("samplePlot.data.samp")
    }
}

