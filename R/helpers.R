is.categorical <- function(x) {
    inherits(x, c("character", "factor"))
}

getColour <- function(index, n, c = 100, l = 65) {
    distance.table <- matrix(
        c(1, 0, 0, 0, 0, 0, 0, 0, 0,
          1, 2, 0, 0, 0, 0, 0, 0, 0,
          1, 2, 3, 0, 0, 0, 0, 0, 0,
          1, 3, 2, 4, 0, 0, 0, 0, 0,
          1, 3, 5, 2, 4, 0, 0, 0, 0,
          1, 4, 2, 5, 3, 6, 0, 0, 0,
          1, 4, 7, 3, 6, 2, 5, 0, 0,
          1, 5, 8, 4, 7, 2, 6, 3, 0,
          1, 5, 9, 4, 8, 3, 7, 2, 6),
        nrow = 9, ncol = 9, byrow = TRUE)

    # Remove one slice so that hue bounds don't touch
    h <- c(0, 360 - (360 / n)) + 15 # Could expose this later but not necessary
    rotate <- function(x) x %% 360
    hues <- rotate(seq(h[1], h[2], length.out = n))
    inds <- distance.table[n, 1:n]
    inds <- inds[index]
    hcl(hues, c, l)[inds]
}

vitBugReport <- function() {
    pkgname <- "vit"
    desc <- packageDescription(pkgname)
    ind <- paste(rep(" ", floor(0.05 * getOption("width"))), collapse = "")
    parwrap <- function(txt, indent = "") {
        cat(paste(strwrap(txt, prefix = ind), collapse = "\n"), "\n")
    }
    cat("\n")
    tmp <- paste("To submit a bug report for the package ",
                 sQuote(pkgname),
                 ", please send an email with the following information:",
                 sep = "")
    parwrap(tmp, ind)
    cat("\n")
    cat(ind, "To: ", desc$Maintainer, "\n", sep = "")
    cat(ind, "Subject: [BUG REPORT] ", desc$Package, " ", desc$Version, "\n", sep = "")
    cat("\n")
    tmp <- paste("For the body of the email, please paste the error message produced by ",
                 sQuote(pkgname),
                 " and describe any steps necessary to reproduce the bug.",
                 sep = "")
    parwrap(tmp, ind)
    cat("\n")
    tmp <- paste("The output from the commands ",
                 sQuote("sessionInfo()"), " and ", sQuote("Sys.info()"),
                 " would also be helpful.",
                 sep = "")
    parwrap(tmp, ind)
    cat("\n")
    tmp <- paste("Optionally, if the bug only occurs with a specific dataset, attaching it to the email would be helpful.")
    parwrap(tmp, ind)
    cat("\n")
    cat(ind, "Thank you for your bug report.\n\n", sep = "")
}

getScreenResolution <- function() {
    if (.Platform$OS.type != "windows")
        return(NA)
    screen.width <- tryCatch(system("wmic desktopmonitor get screenwidth", intern = TRUE),
                             error = function(e) NA)
    screen.height <- tryCatch(system("wmic desktopmonitor get screenheight", intern = TRUE),
                              error = function(e) NA)
    if (any(is.na(c(screen.width, screen.height))))
        return(NA)
    as.numeric(c(screen.width[-c(1, length(screen.width))],
                 screen.height[-c(1, length(screen.height))]
    ))
}

getAvailableScreenDims <- function() {
    screen.res <- getScreenResolution()
    if (any(is.na(screen.res))) return(NA)
    screen.res[2] <- screen.res[2] - 40 # Adjusting for taskbar height
    screen.res
}

getMainWindowHeight <- function() {
    screen.res <- getAvailableScreenDims()
    if (length(screen.res) == 1)
        600 # Because we cannot find a suitable res, assume 600 is OK
    else
        min(c(600, screen.res[2]))
}

# This function is used for text formatting purposes that we get
# readable text labels on things like confintGrobs
ciLabelDigits <- function(x) {
    if (length(x) > 1)
        x <- abs(diff(x))
    ndigits <- 0
    if (is.categorical(x))
        ndigits <- 2
    else if (max(x) >= 1000)
        ndigits <- 3
    else
        ndigits <- 2
}

updateVit <- function() {
    oldr <- r <- getOption("repos")
    on.exit(options(repos = oldr))
    r["CRAN"] <- "http://cran.stat.auckland.ac.nz"
    options(repos = r)

    # Update CRAN packages
    update.packages(ask = FALSE)

    # Update VIT from ~wild
    if (.Platform$OS.type != "unix") {
        tmploc <- file.path(tempdir(), "vit.zip")
        download.file("http://www.stat.auckland.ac.nz/~wild/downloads/vit.zip",
                      destfile = tmploc)
        install.packages(tmploc, type = "win.binary")
        file.remove(tmploc)
    } else {
        tmploc <- file.path(tempdir(), "vit.tar.gz")
        download.file("http://www.stat.auckland.ac.nz/~wild/downloads/vit.tar.gz",
                      destfile = tmploc)
        install.packages(tmploc, type = "source")
        file.remove(tmploc)
    }

    # Update iNZight too if we can
    if ("iNZight" %in% rownames(installed.packages())) {
        iNZight::updateiNZight()
    }
}

notYetImplemented <- function(name = "This method", ...) {
	cat(paste(name, "has not yet been implemented.\n"))
}
