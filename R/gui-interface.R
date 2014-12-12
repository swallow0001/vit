###  File          :  gui-interface.R
###  Modified by   :  Chris Park <cpar137@aucklanduni.ac.nz>
###  Description   :  Modified the "newdevice" function.
###  Date modified :  December 12, 2014.

###  Providing a helpful message for users when loading the package
.onAttach <- function(libname, pkgname) {
    lwd <- getOption("width")
    ind <- paste(rep(" ", floor(0.05 * lwd)), collapse = "")
    header <- paste(rep("=", lwd), collapse = "")
    parwrap <- function(txt, indent = "") {
        paste(strwrap(txt, prefix = ind), collapse = "\n")
    }

    ## Ensure we're using RGtk2
    options("guiToolkit" = "RGtk2")

    packageStartupMessage(header)
    packageStartupMessage("")
    packageStartupMessage(parwrap(paste("You have successfully loaded the",
                                        "Visual Inference Tools package!")))
    packageStartupMessage("")
    packageStartupMessage(parwrap(paste("To get started with VIT simply",
                                        "run the following command:")))
    packageStartupMessage("")
    packageStartupMessage(parwrap("iNZightVIT()"))
    packageStartupMessage("")
    
    ## If using Mac, need to inform them that it is going to be SLOW!
    if (.Platform$OS.type != "windows" && Sys.info()["sysname"] == "Darwin") {
        packageStartupMessage(parwrap(paste("Please be patient while",
                                            "the software loads. You can also ignore",
                                            "any error/warning messages that show up",
                                            "below.")))
        packageStartupMessage("")
        packageStartupMessage(parwrap(paste("Check behind this window to make sure",
                                            "iNZightVIT hasn't loaded behind it!")))
        packageStartupMessage("")
    }
    
    packageStartupMessage(header)
}


newdevice <-
    function(width, height, ...) {
        ##  Check if "shiny" is currently loaded.
        if ("package:shiny" %in% search())
            ##  Let shiny set default graphics device, i.e. height and width.
            return()    
        ##  Check if "Acinonyx" is installed. This is a logical vector that returns
        ##  TRUE if installed and FALSE otherwise.
        checkAcinonyx <- "Acinonyx" %in% rownames(installed.packages())        
        if (checkAcinonyx) {
            ##  If "Acinonyx" is installed, check if it can be successfully loaded.
            ##  If so, use the "idev" device provided by the package.
            tryAcinonyx <- suppressWarnings(try(library(Acinonyx), silent = TRUE))
            if (!inherits(ac, "try-error")) {             
                ##  Acinonyx uses pixels rather than inches, convert inches to
                ##  pixels to determine dims. Assume 90 dpi.        
                width.in <- round(width * 90)
                height.in <- round(height * 90)
                Acinonyx::idev(width = width.in, height = height.in)
                ##  If we cannot load "Acinonyx" successfully, issue a helpful warning
                ##  message for users.
            } else {
                gmessage(paste("Unfortunately, the package used for animation",
                               "in iNZightVIT is incompatible with your system.",
                               "While you can still use VIT, you may experience",
                               "some animation issues. We suggest you download",
                               "our iNZightVIT module for older Mac OS: \n\n", ,
                               "https://www.stat.auckland.ac.nz/~wild/iNZight/mac.html"),
                         title = "VIT Animation Compatiblity Issue")            
            }
            ##  If Acinonyx is NOT installed, we handle the windows and non-windows
            ##  Operating Systems separately.
        } else {
            ##  If OS is non-windows...
            if (.Platform$OS.type != "windows") {
                ##  Note that there exist three variants of the cairo-based device.
                ##  See ?X11() for more details.
                ## 
                ##  If the "cairoDevice" package is loaded, we use the Cairo() graphics
                ##  device as it supports anti-aliasing and produces visually pleasing
                ##  plots across all platforms.
                if ("package:cairoDevice" %in% search()) { # For consistency    
                    cairoDevice::Cairo(width = width, height = height, ...)
                } else {
                    ##  If "cairoDevice" is NOT intalled, issue a helpful warning message.
                    if (!("cairoDevice" %in% rownames(installed.packages()))) {
                        warning("We suggest you install the `cairoDevice`
                                package for better animations")
                        ##  We use a buffered device as otherwise repainting when the
                        ##  window is exposed will be slow.
                        X11(width = width, height = height, type = "cairo", ...)
                        ##  If the "cairoDevice" package is installed but not loaded,
                        ##  load the package and use the cross-platform "Cairo" device.
                    } else { 
                        library(cairoDevice)
                        cairoDevice::Cairo(width = width, height = height, ...)
                    }
                }
                ##  If OS is "windows", then we use the default windows device, which
                ##  supports double buffering by default.
            } else { 
                dev.new(width = width, height = height, ...)
            }
        }
    }

## Brings up homepage which allows the user to either run iNZight or VIT.

iNZightVIT <- function(disposeR = FALSE) {
    ## With `disposeR`, we only want to set this to TRUE if they user
    ## opens via the icon in the standalone version, OR if they explicitly
    ## say so when they open the software from within R manually.
    
    e <- new.vit.env()
    e$disposeR <- disposeR
    e$homefun <- iNZightVIT

    permvar.handler <- function(){
        dispose(home)
        graphics.off()
        newdevice(height = 7.5, width = 11.25)
        plot.new()
        e$title <- "Randomisation variation"
        setupGUI(e)
        permvarGUIHandler(e)
        dataGUI(e)
    }
    permutation.handler <- function(){
        dispose(home)
        graphics.off()
        newdevice(height = 7.5, width = 11.25)
        plot.new()
        e$title <- "Randomisation tests"
        setupGUI(e)
        permGUIHandler(e)
        dataGUI(e)
    }
    sampvar.handler <- function() {
        dispose(home)
        graphics.off()
        newdevice(height = 7.5, width = 7.5)
        plot.new()
        e$title <- "Sampling variation"
        setupGUI(e)
        sampvarGUIHandler(e)
        dataGUI(e)
    }
    bootstrap.handler <- function(){
        dispose(home)
        graphics.off()
        newdevice(height = 7.5, width = 11.25)
        plot.new()
        e$title <- "Bootstrap confidence intervals"
        setupGUI(e)
        bootstrapGUIHandler(e)
        dataGUI(e)
    }
    confint.handler <- function(){
        dispose(home)
        graphics.off()
        newdevice(height = 7.5, width = 7.5)
        e$title <- "Confidence interval coverage"
        setupGUI(e)
        CIGUIHandler(e)
        dataGUI(e)
    }
    ## Generates home page
    home <- gwindow("The iNZight and Visual Inference Tool Systems", visible = TRUE)

    ## We want to just quit R any time the home screen is closed (but
    ## only when iNZight hasn't been loaded)
    addHandlerUnrealize(home, handler = function(h, ...) {
        if (e$disposeR) {
            e$confirmDialog("Do you wish to quit iNZightVIT?",
                            handler = function(...) {
                                q(save = "no")
                            })
        } else {
            dispose(home)
            dev.off()
        }
        return(TRUE)
    })

    group <- ggroup(horizontal = FALSE, container = home)
    title <- glabel("The iNZight and VIT Systems", container = group)
    addSpace(group, 10, horizontal = FALSE)

    inzight.group <- ggroup(horizontal = FALSE, container = group)

    have.inzight <- "iNZight" %in% rownames(installed.packages())
    if (have.inzight) {
        ## datestamp <- strsplit(packageDescription("iNZight")$Version, "-")[[1]][2]
        datestamp <- packageDescription("iNZight")$Version
        inz.frame.title <- paste("iNZight", " (v", datestamp, ")", sep = "")
    } else {
        inz.frame.title <- "iNZight (not installed)"
    }

    inzight.frame <-
        gframe(inz.frame.title, container = inzight.group, horizontal = FALSE)
    inzight.banner <-
        gimage(system.file("images/inzight-banner.png", package = "vit"),
               handler = function(h, ...)
               browseURL("http://www.stat.auckland.ac.nz/~wild/iNZight/"))
    add(inzight.frame, inzight.banner)
    addSpace(inzight.frame, 5, horizontal = FALSE)
    inzight.title <- glabel("iNZight is a program for analysing data")
    add(inzight.frame, inzight.title)
    home.inzight <-
        gbutton("Run iNZight", expand = TRUE,
                handler = function(h, ...) {
                    ## We first check presence of the iNZight package. The idea is so that we have
                    ## a reasonable idea of whether we should provide a splash screen when loading
                    ## iNZight (as it takes a while...)
                    if ("iNZight" %in% rownames(installed.packages())) {
                        lib.png <- suppressWarnings(require(png))
                        if (lib.png) {
                            iNZight.splash <-
                                png::readPNG(system.file("images", "inzight-splash.png",
                                                         package = "vit"),
                                             exists("rasterImage"))
                            dev.new(width = 3.5, height = 2)
                            grid.newpage()
                            grid.raster(iNZight.splash, width = unit(3.5, "inches"),
                                        height = unit(2, "inches"))
                        }
                        
                        suppressPackageStartupMessages({
                            suppressWarnings(library(iNZight))
                        })
                        
                        if (lib.png)
                            dev.off()

                        e$disposeR <- FALSE
                        detach("package:gWidgetsRGtk2")
                        detach("package:gWidgets")
                        suppressPackageStartupMessages({
                            suppressWarnings(iNZight(disposeR = disposeR))
                        })
                        dispose(home)
                    } else {
                        gmessage("You must install the iNZight package to run iNZight.",
                                 title = "Error - iNZight required", icon = "error")
                    }
                })
    enabled(home.inzight) <- have.inzight
    add(inzight.frame, home.inzight)
    addSpace(group, 10, horizontal = FALSE)

    vit.group <- ggroup(horizontal = FALSE, container = group)

    ##  datestamp <- strsplit(packageDescription("vit")$Version, "-")[[1]][2]
    datestamp <- packageDescription("vit")$Version
    vit.frame.title <-
        paste("Visual Inference Tools", " (v", datestamp, ")", sep = "")
    vit.frame <-
        gframe(vit.frame.title, container = vit.group, horizontal = FALSE)
    vit.banner <-
        gimage(system.file("images/vit-banner.png", package = "vit"),
               handler = function(h, ...)
               browseURL("http://www.stat.auckland.ac.nz/~wild/VIT/"))
    add(vit.frame, vit.banner)
    addSpace(vit.frame, 5, horizontal = FALSE)
    vit.title <-
        glabel("Visual Inference Tools (VIT) contains programs for developing concepts")
    add(vit.frame, vit.title)
    vit.functions <- list(permvar.handler,
                          permutation.handler,
                          sampvar.handler,
                          bootstrap.handler,
                          confint.handler)
    vit.modules <- c("Randomisation variation",
                     "Randomisation tests",
                     "Sampling variation",
                     "Bootstrap confidence interval construction",
                     "Confidence interval coverage")
    vit.radio <- gradio(vit.modules, horizontal = FALSE)
    add(vit.frame, vit.radio)
    vit.run <- gbutton("Run selected VIT module", expand = TRUE,
                       handler = function(h, ...){
                           vit.functions[[which(vit.modules == svalue(vit.radio))]]()
                       })
    add(vit.frame, vit.run)
    addSpace(vit.group, 10, horizontal = FALSE)
    logo <-
        gimage(system.file("images/uoa-logo.png", package = "vit"),
               handler = function(h, ...) browseURL("http://www.stat.auckland.ac.nz/"))
    add(group, logo)
}

## Brings up homepage which allows user to choose vit module to use.
vit <- function() {
    e <- new.vit.env()
    e$disposeR <- FALSE
    e$homefun <- vit
    home <- gwindow("Visual Inference Tools", visible = TRUE)
    home.buttons <- ggroup(horizontal = FALSE, container = home)
    title <- glabel("Visual Inference Tools", container = home.buttons)
    ## Generates home page
    home.permvar <-
        gbutton("Randomisation variation", container = home.buttons, expand = TRUE,
                handler = function(h, ...) {
                    dispose(home)
                    graphics.off()
                    newdevice(height = 7.5, width = 11.25)
                    plot.new()
                    e$title <- "Randomisation variation"
                    setupGUI(e)
                    permvarGUIHandler(e)
                    dataGUI(e)
                })
    home.permutation <-
        gbutton("Randomisation tests", container = home.buttons, expand = TRUE,
                handler = function(h, ...){
                    dispose(home)
                    graphics.off()
                    newdevice(height = 7.5, width = 11.25)
                    plot.new()
                    e$title <- "Randomisation tests"
                    setupGUI(e)
                    permGUIHandler(e)
                    dataGUI(e)
                })
    home.sampvar <-
        gbutton("Sampling variation", container = home.buttons, expand = TRUE,
                handler = function(h, ...) {
                    dispose(home)
                    graphics.off()
                    newdevice(height = 7.5, width = 7.5)
                    plot.new()
                    e$title <- "Sampling variation"
                    setupGUI(e)
                    sampvarGUIHandler(e)
                    dataGUI(e)
                })
    home.bootstrap <-
        gbutton("Bootstrap confidence interval construction",
                container = home.buttons, expand = TRUE,
                handler = function(h, ...){
                    dispose(home)
                    graphics.off()
                    newdevice(height = 7.5, width = 11.25)
                    plot.new()
                    e$title <- "Bootstrap confidence intervals"
                    setupGUI(e)
                    bootstrapGUIHandler(e)
                    dataGUI(e)
                })
    home.confint <-
        gbutton("Confidence interval coverage", container = home.buttons, expand = TRUE,
                handler = function(h, ...){
                    dispose(home)
                    graphics.off()
                    newdevice(height = 7.5, width = 7.5)
                    e$title <- "Confidence interval coverage"
                    setupGUI(e)
                    CIGUIHandler(e)
                    dataGUI(e)
                })
}
