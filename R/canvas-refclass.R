# The canvasClass reference class provides a canvas generator object
# for making canvas instances.
canvas <- setRefClass("canvasClass",
                      fields = c("x", "y", "x.name", "y.name", "levels",
                                 "level.samples", "ylevels", "progressType", "stopAnimation",
                                 "animationPaused", "if.permswap", "stat.in.use", "data.file",
                                 "loi", "loi.alt", "loi.data", "stat.ypos", "ngroups", "stat.method",
                                 "n", "samples", "sampled.stats", "paired.data", "max.hist.height",
                                 "plotted.stats", "indexes", "x.scale.ys", "dp", "hist.cutoff",
                                 "which.sample", "stat.dist", "quartiles", "viewports", "image"),
                      methods = list(
    initialize = function(x = NULL, y = NULL, levels = NULL, x.name = NULL, y.name = NULL, paired.data = NULL,
                          samples = NULL, level.samples = NULL, ylevels = NULL, loi = NULL, loi.alt = NULL, loi.data = NULL,
                          stat.in.use = NULL, stat.method = NULL, ngroups = NULL, max.hist.height = NULL, viewports = NULL,
                          image = NULL, n = 0, progressType = "text", data.file = NULL, which.sample = 0, if.permswap = NULL,
                          indexes = NULL, animationPaused = FALSE, stopAnimation = FALSE, stat.dist = NULL, quartiles = NULL,
                          stat.ypos = NULL, x.scale.ys = NULL, sampled.stats = NULL, plotted.stats = NULL,
                          ...) {
        x <<- x
        y <<- y
        x.name <<- x.name
        y.name <<- y.name
        samples <<- samples
        levels <<- levels
        level.samples <<- level.samples
        ylevels <<- ylevels
        image <<- image
        loi <<- loi
        loi.alt <<- loi.alt
        loi.data <<- loi.data
        stat.in.use <<- stat.in.use
        stat.method <<- stat.method
        ngroups <<- ngroups
        hist.cutoff <<- 1000 # Sample size where we start drawing hists
        max.hist.height <<- max.hist.height
        n <<- length(x)
        viewports <<- viewports
        paired.data <<- paired.data
        progressType <<- progressType
        data.file <<- data.file
        indexes <<- indexes
        which.sample <<- which.sample
        if.permswap <<- if.permswap
        animationPaused <<- animationPaused
        stopAnimation <<- stopAnimation
        dp <<- 2
        stat.dist <<- stat.dist
        quartiles <<- quartiles
        stat.ypos <<- stat.ypos
        x.scale.ys <<- x.scale.ys
        sampled.stats <<- sampled.stats
        plotted.stats <<- plotted.stats
        invisible(.self)
    },
    advanceWhichSample = function() {
        # Advances which.sample.
        if (which.sample >= 1000) which.sample <<- 0
        which.sample <<- which.sample + 1
    },
    # The buildBoxes method generates databoxes for methods that
    # require them, currently this is all methods except ci and
    # sampvar.
    buildBoxes = function(paired.samples = FALSE) {
        databox1 <- roundrectGrob(name = "databox1",
                                  x = unit(1, "mm"), y = unit(1, "mm"),
                                  width = unit(1, "npc") - unit(2, "mm"),
                                  height = unit(1, "npc") - unit(2, "mm"),
                                  just = c("left", "bottom"),
                                  gp = gpar(col = "gray40", fill = "gray95", lwd = 2),
                                  vp = graphPath("databox", 1))
        databox2 <- roundrectGrob(name = "databox2",
                                  x = unit(1, "mm"), y = unit(1, "mm"),
                                  width = unit(1, "npc") - unit(2, "mm"),
                                  height = unit(1, "npc") - unit(2, "mm"),
                                  just = c("left", "bottom"),
                                  gp = gpar(col = "gray40", fill = "gray95", lwd = 2),
                                  vp = graphPath("databox", 2))
        image <<- addGrob(image, databox1) # Left databox
        image <<- addGrob(image, databox2) # Right databox
        if (paired.samples) {
            # Because of space restrictions, trim the variable names
            # if necessary
            if (nchar(x.name) > 5)
                title1 <- paste(substr(x.name, 1, 5), "...", sep = "")
            else
                title1 <- x.name
            if (nchar(y.name) > 5)
                title2 <- paste(substr(y.name, 1, 5), "...", sep = "")
            else
                title2 <- y.name
            # We need to work out the maximum width of the text that
            # appears in the databoxes so that we can rearrange the
            # placement of text in the first and third columns of the
            # first databox.
            xsw <- max(stringWidth(c(title1, format(round(paired.data[, 1], .self$dp), nsmall = .self$dp))))
            ysw <- max(stringWidth(c(title2, format(round(paired.data[, 2], .self$dp), nsmall = .self$dp))))
            dsw <- max(stringWidth(c("diff", format(round(x, .self$dp), nsmall = .self$dp))))
            maxw <- max(xsw, ysw, dsw)
            sparespace <- (unit(1, "npc") - 3*maxw - unit(2, "mm"))*(1/6)
            cols <- c("black", rep("blue", min(n, 50)))
            # Adding paired sample datatext grobs
            text.sample1 <- datatextGrob(data = paired.data[, 1], title = title1,
                                         x = unit(1, "mm") + sparespace + 0.5*maxw,
                                         max = 50, name = "text.sample1",
                                         gp = gpar(col = cols),
                                         vp = graphPath("databox", 1))
            text.sample2 <- datatextGrob(data = paired.data[, 2], title = title2,
                                         x = unit(0.5, "npc"),
                                         max = 50, name = "text.sample2",
                                         gp = gpar(col = cols),
                                         vp = graphPath("databox", 1))
            text.sample3 <- datatextGrob(data = x, title = "diff",
                                         x = unit(1, "npc") - unit(1, "mm")
                                         - sparespace - 0.5*maxw,
                                         max = 50, name = "text.sample3",
                                         vp = graphPath("databox", 1))
            image <<- addGrob(image, text.sample1)
            image <<- addGrob(image, text.sample2)
            image <<- addGrob(image, text.sample3)
        } else if (is.null(levels) | ! is.null(ngroups)) {
            # In the case where we only have a single variable to
            # consider and we're also not in the permvar module,
            # simply list the contents of one variable.
            if (nchar(x.name) > 20)
                title1 <- paste(substr(x.name, 1, 19), "...", sep = "")
            else
                title1 <- x.name
            text.sample <- datatextGrob(data = x, title = title1, max = 50,
                                        name = "text.sample",
                                        vp = graphPath("databox", 1))
            image <<- addGrob(image, text.sample)
        } else {
            # The alternative case is where we have two variables that
            # we need to list.  Trim variable names if they're too
            # long and colour levels of a factor if they're
            # categorical.
            if (nchar(x.name) > 12)
                title1 <- paste(substr(x.name, 1, 11), "...", sep = "")
            else
                title1 <- x.name
            if (nchar(y.name) > 12)
                title2 <- paste(substr(y.name, 1, 11), "...", sep = "")
            else
                title2 <- y.name
            if (is.categorical(levels)) {
                cols <- getColour(1:length(ylevels), length(ylevels))
                cols <- cols[order(ylevels)]
                text.sample2 <- coldatatextGrob(data = as.character(levels),
                                                title = title2,
                                                cols = cols, x = 0.75,
                                                max = 50, name = "text.sample2",
                                                vp = graphPath("databox", 1))
            } else {
                text.sample2 <- datatextGrob(data = levels, title = title2,
                                                 x = 0.75, max = 50, name = "text.sample2",
                                                vp = graphPath("databox", 1))
            }
            text.sample1 <- datatextGrob(data = x, title = title1, x = 0.25,
                                         max = 50, name = "text.sample1",
                                         vp = graphPath("databox", 1))
            image <<- addGrob(image, text.sample1)
            image <<- addGrob(image, text.sample2)
        }
    },
    # The drawImage method is mostly a convenience function to save us
    # from clearing and redrawing an image. It can handle the case
    # when we have paused the playback of an animation by throwing
    # itself into an infinite loop. This is a bit of a hack but at
    # least to the user it works well enough. Pay no attention to the
    # man behind the curtain.
    drawImage = function() {
        while (.self$animationPaused) {}

        if ("Acinonyx" %in% rownames(installed.packages()))
            plot.new()

        # Draws current image in device.
        grid.newpage()
        grid.draw(image)

        # On some devices (notably on Mac) we end up being unable to
        # see anything besides a single frame due to buffering.
        # dev.flush() will force the device to show what it has
        # currently buffered.
        if (exists("dev.flush"))
            dev.flush()
    },
    # A convenience method that simply redraws the image several
    # times, this is a bit of a hack but (at least to our knowledge)
    # we don't have much alternative as calling sleep() will render
    # the GUI unusable for the sleeping period.
    #
    # This method takes a single parameter, pause, the amount of times
    # that we are going to redraw the image.  Calling the pauseImage
    # method with pause = 1 is equivalent to simply calling
    # drawImage().
    pauseImage = function(pause = 1) {
        for (i in 1:pause)
            .self$drawImage()
    },
    # Given an index i, returns the ith sample from our generated
    # samples. The default is to return the current sample that we're
    # observing.
    getSample = function(i = which.sample) {
        samples[[i]]
    },
    # Given an index i, returns the ith statistic from our
    # generated distribution of statistics.
    getStat = function(i = which.sample) {
        if (is.null(stat.dist))
            makeStatDistribution()
        stat.dist[[i]]
    },
    # Returns the truncated list of statistics that contains all of
    # the statistics we have observed so far.
    getStatDist = function() {
        stat.dist[1:which.sample]
    },
    # getStatYPos is a method that pre-calculates the positions that
    # each of the points in our stat panel are going to fall into. The
    # rationale behind this is that stackPoints is too slow to be able
    # to do calculate points for each iteration where we build up our
    # distribution of points.
    #
    # Because the statistic distribution is known in advance, we can
    # save ourselves the hassle of continually recalculating where
    # points are supposed to fall. This method returns us the same
    # output as if we were calling stackPoints so we can simply subset
    # the result to plot up to the current sample.
    getStatYPos = function() {
        # If we're using a list to store our stat.dist (most of the
        # time this is the case)
        if (is.list(stat.dist)) {
            if (length(stat.dist[[1]]) == 1) {
                # Because the stats are single values, collapse the
                # list into a single vector.
                stats <- c(stat.dist, recursive = TRUE)
            } else if (!is.null(stat.in.use) && stat.in.use == "slope") {
                # Because our list is storing the coefficients of our
                # linear models, we're only interested in the slope
                # components, not the intercepts. Thus we subset to
                # filter out the intercepts.
                stats <- c(stat.dist, recursive = TRUE)[c(FALSE, TRUE)]
            } else {
                # If we're dealing with a statistic distribution of
                # differences, get the differences for each element in
                # the list and return the results as a single vector.
                stats <- sapply(stat.dist, diff)
            }
        } else if (is.matrix(stat.dist)) {
            # Sometimes we are dealing with difference methods where
            # two values need to be retained.
            if (! is.null(stat.in.use)) {
                if (stat.in.use == "IQR ratio") {
                    # The IQR method is a bit of a special case. Store
                    # as a log10'd ratio of the "top" statistic over
                    # the "bottom" statistic.
                    stats <- log10(stat.dist[, 2] / stat.dist[, 1])
                } else if (stat.in.use == "slope") {
                    # The slope is stored in the second element from
                    # the output of coef(). Grab only the slope
                    # coefficients.
                    stats <- stat.dist[, 2]
                } else {
                    # Default to grabbing the difference.
                    stats <- apply(stat.dist, 1, diff)
                }
            } else {
                # We don't know what statistic is being used, but
                # assume that a simple diff will work.
                stats <- apply(stat.dist, 1, diff)
            }
        } else if (is.vector(stat.dist)) {
            stats <- stat.dist
        }
        # Determining the max height of the statistic distribution.
        if (!is.null(stat.in.use) && stat.in.use == "slope") {
            y.max <- unit(1, "npc") - unit(0.5, "char")
        } else {
            y.max <- unit(1, "npc") - unit(2, "lines") - unit(0.5, "char")
        }
        stat.ypos <<- stackPoints(stats, vp = graphPath("stat"),
                                  y.min = 0, y.max = y.max)
    },
    # This method is determines how samples and their statistics are
    # to be generated for use within VIT.
    #
    # Because of the many ways in which we can generate samples and
    # their statistics, we are required to have a bunch of optional
    # parameters to be able to handle this.
    #
    # Parameters:
    #   replace: Whether we are sampling with replacement or not.
    #   sample.both: Whether we are sampling both variables, x and
    #                levels or not.
    #   resamp.within: Whether we are sampling within groups or not.
    #   perm.paired.samples: If we are in the permutation module,
    #                        whether we are permuting paired sample
    #                        data.
    makeSamples = function(replace, sample.both = FALSE, resamp.within = FALSE, perm.paired.samples = FALSE) {
        if (perm.paired.samples) {
            ## Permuting paired samples here. When we permute, we
            ## randomly sample (p = 0.5) to see if we swap the order
            ## of the differencing.
            ifswap <- sample(c(TRUE, FALSE), size = 1000*n, replace = TRUE)
            xvec <- rep(paired.data[, 1], 1000)
            yvec <- rep(paired.data[, 2], 1000)
            swapvec <- xvec[ifswap]
            xvec[ifswap] <- yvec[ifswap]
            yvec[ifswap] <- swapvec
            samples <<- split(xvec - yvec, rep(1:1000, each = n))
            if.permswap <<- split(ifswap, rep(1:1000, each = n))
        } else {
            # We are not in the permutation module and not using
            # paired sample data. This is the case most of the time.
            if (resamp.within) {
                ## Sampling within groups here.
                ## Reordering databoxes by groups.
                ord <- order(levels)
                levels <<- as.character(levels)[ord]
                x <<- x[ord]
                index.vec <- numeric(1000*n)
                ng <- length(ylevels)
                group.indices <- list()
                for (i in 1:ng)
                    group.indices[[i]] <- which(levels == ylevels[i])
                for (i in 1:1000) {
                    sample.indices <- numeric(n)
                    for (j in 1:ng)
                        sample.indices[group.indices[[j]]] <- sample(group.indices[[j]],
                                                                     replace = TRUE)
                    index.vec[(n*(i - 1) + 1):(n*i)] <- sample.indices
                }
                levels.vec <- rep(levels, 1000)
                level.samples <<- split(levels.vec, rep(1:1000, each = n))
            } else {
                ## Normal sampling.
                if (replace) {
                    ## With replacement.
                    index.vec <- sample(1:length(x), n * 1000, replace = TRUE)
                } else {
                    ## Without replacement.
                    index.vec <- list()
                    for (i in 1:1000) {
                        index.vec[[i]] <- sample(1:length(x), size = n)
                    }
                    index.vec <- unlist(index.vec)
                }
                if (sample.both) {
                    ## Sampling both x and y together.
                    prob <- TRUE
                    ## If y is a factor, we want to prevent the whole
                    ## sample being taken from just one group. If y is
                    ## numeric, we want at least two unique x values
                    ## so that we have variance in our explanatory
                    ## variable.
                    while (any(prob)) {
                        levels.vec <- as.character(levels[index.vec])
                        level.samples <<- split(levels.vec, rep(1:1000, each = n))
                        sample.vec <- x[index.vec]
                        samples <<- split(sample.vec, rep(1:1000, each = n))
                        if (stat.in.use == "slope") {
                            prob <- sapply(samples, function(x) all (x == x[1]))
                        } else {
                            prob <- sapply(level.samples, function(x) all(x == x[1]))
                        }
                        for (i in (1:1000)[prob])
                            index.vec[(n*(i - 1) + 1):(n*i)] <- sample(1:length(x), size = n,
                                                                       replace = replace)
                    }
                }
            }
            sample.vec <- x[index.vec]
            samples <<- split(sample.vec, rep(1:1000, each = n))
            indexes <<- split(index.vec, rep(1:1000, each = n))

            # When we're using histograms, we need to scale the heights
            # so that they are consistent across all samples and data
            if (stat.in.use != "slope" & ! is.categorical(x)) {
                if (length(x) >= hist.cutoff) {
                    max.hist.height <<- 0 # Initialising to zero
                    if (is.null(level.samples) & ! is.null(x)) {
                        # We need to grab a histogram of all of the
                        # data so we can set appropriate breakpoints
                        # for each of the bars and use the same
                        # breakpoints for each of the histograms from
                        # our data.
                        whole.hist <- hist(x, plot = FALSE)
                        # Generate a histogram for each of our samples
                        # and see if the height of any of the bars
                        # exceeds our maximum known histogram height.
                        for (samp in 1:length(samples)) {
                            curdat <- samples[[samp]]
                            boxes <- length(whole.hist$mids)
                            all.histvals <- hist(x = curdat, breaks = whole.hist$breaks,
                                                 include.lowest = TRUE, right = TRUE, plot = FALSE)
                            max.height <- max(all.histvals$density)
                            if (max.height > max.hist.height)
                                max.hist.height <<- max.height
                        }
                    } else {
                        # Don't have a main histogram to compare
                        # against because we have several levels. Run
                        # through each of the levels for each sample
                        # (and level sample) and base the histograms
                        # on the number of bars present when all
                        # levels are aggregated (for a given
                        # ylevel). If we encounter a large enough
                        # histogram, store that in max.hist.height.
                        for (samp in 1:length(samples)) {
                            curr.xs <- samples[[samp]]
                            curr.ys <- level.samples[[samp]]
                            for (yl in 1:length(ylevels)) {
                                level.curr.xs <- curr.xs[curr.ys == ylevels[yl]]
                                boxes <- length(hist(level.curr.xs, plot = FALSE)$mids)
                                all.histvals <- hist(x = level.curr.xs,
                                                     breaks = seq(min(level.curr.xs), max(level.curr.xs),
                                                                  length.out = boxes),
                                                     include.lowest = TRUE, right = TRUE, plot = FALSE)
                                max.height <- max(all.histvals$density)
                                if (max.height > max.hist.height)
                                    max.hist.height <<- max.height
                            }
                        }
                    }
                }
            }
        }
        which.sample <<- 1
    },
    # This is the workhorse that calculates the statistics for all of
    # our samples. This is where we spend a lot of time after hitting
    # 'Record My Choices'.
    #
    # Most of the time this happens fairly quickly, but on some
    # methods (like CI medians), the amount of time it takes can be
    # quite large. As a result we show a console progress bar all of
    # the time except when we know that at least 30s will go by before
    # we see a result. In that case we show a graphical progress bar.
    #
    # Mostly this is just using plyr to call our calcStat methods.
    makeStatistics = function() {
        if (!is.null(level.samples)) {
            xmat <- matrix(c(samples, recursive = TRUE), ncol = n, byrow = TRUE)
            levelsmat <- matrix(c(level.samples, recursive = TRUE), ncol = n, byrow = TRUE)
            allmat <- cbind(xmat, levelsmat)
            stat.dist <<- plyr::aaply(allmat, 1,
                                      function(x) .self$calcAllStats(x[1:n], x[-(1:n)], .self),
                                      .progress = .self$progressType)
        } else {
            stat.dist <<- plyr::llply(samples, function(x) .self$calcAllStats(x, .self$levels, .self),
                                      .progress = .self$progressType)
        }
    },
    # Calculates the upper and lower quartiles and stores this in the
    # canvas field 'quartiles'.
    makeQuartiles = function() {
        if (!is.null(level.samples)) {
            q <- array(0, dim = c(1000, 3, length(ylevels)))
            dimnames(q) <- list(NULL, NULL, ylevels)
            for (i in 1:1000) {
                xs <- samples[[i]]
                ys <- level.samples[[i]]
                for (j in ylevels)
                    q[i, , j] <- fivenum(xs[ys == j])[2:4]
            }
        } else {
            q <- t(sapply(samples, function(x) fivenum(x)[2:4]))
        }
        quartiles <<- q
    },
    # orderLevels determines the order in which we place the levels in
    # the viewports (when there is more than one group of course).
    #
    # Usually we end up with the higher statistic having the lower VP
    # number (and thus appearing lower in the data and sample panels).
    # What this means is that if we're comparing means across a bunch
    # of groups, the means appear to go down and to the right.
    #
    # The result of calling orderLevels is that we store in ylevels
    # the order in which we wish to place each of the levels.
    orderLevels = function(FUN = NULL) {
        if (is.null(FUN) & ! is.null(stat.in.use)) {
            if (stat.in.use == "IQR ratio")
                FUN <- function (x) -diff(fivenum(x)[c(2, 4)])
            else if (stat.in.use == "median")
                FUN <- median
            else
                FUN <- mean
        } else if (is.null(FUN)) {
            FUN <- mean
        }

        samps <- split(x, levels)
        ylevels <<- names(sort(sapply(samps, FUN), decreasing = TRUE))
    },
    # Convenience method that allows us to throw in a bunch of grob
    # names to remove and it will only attempt to remove them if they
    # are present in our image.
    #
    # This allows us to vectorise the removal of grobs from our image
    # gTree while also avoiding a potential source of error if we
    # attempt to remove a grob that does not exist. removeGrob()
    # complains if this occurs.
    rmGrobs = function(grobs = NULL) {
        for (i in grobs) {
            if (i %in% childNames(image)) {
                image <<- removeGrob(image, gPath(i))
            }
        }
    },
    # Convenience method to reset the canvas variables to their
    # initial values (i.e. after we have hit 'Record My Choices').
    reset = function() {
        # Resetting canvas values to defaults
        .self$which.sample <- 1
        .self$animationPaused <- FALSE
        .self$sampled.stats <- NULL
        .self$plotted.stats <- NULL
    },
    # Convenience method that resets what has been plotted to what we
    # would usually see after hitting 'Record My Choices'.
    resetPlots = function(e) {
        # Drawing only what is necessary
        e$clearPanel("sample")
        e$clearPanel("stat")
        rmGrobs(c("samplePlot.databox.text.2", "dataPlot.ci.1"))
        if (e$method %in% c("bootstrap", "permutation")) {
            # We can only have paired samples in the above
            # methods. Handle this case if we do have paired samples.
            ps <- if (! is.null(e$pairedSamples)) e$pairedSamples
                  else FALSE
            .self$buildBoxes(ps)
        }
        .self$plotData()
        .self$showLabels()
        .self$plotDataStat(e)
    },
    # If we have two variables and we wish to turn them into paired
    # sample data, call this function to perform the task.
    switchToPaired = function() {
        x <<- levels - x
        levels <<- NULL
    },
    # Assigning NULL functions that we're going to be overriding later
    graphPath = function(...) {},
    plotData = function(...) {},
    plotSample = function(...) {},
    calcStat = function(...) {},
    calcAllStats = function(...) {},
    showLabels = function(...) {},
    plotDataStat = function(...) {},
    plotStatDist = function(...) {},
    displayResult = function(...) {},
    animateSample = function(...) {},
    animateStat = function(...) {},
    handle1000 = function(...) {},
    plotSampleStat = function(...) {},
    trackSample = function(...) {},
    fadePlots = function(...) {}
    ))
