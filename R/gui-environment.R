new.vit.env <- function() {
    e <- new.env()

    # methods specifically for loading data
    e$fileReader <- function() {
        enabled(e$importData) <- FALSE
        e$specifyFileForImport()
    }

    e$specifyFileForSaving <- function(...) {
        e1 <- new.env()
        saveFileWin <- gwindow("File Browser", container = TRUE, parent = e$win)

        fileMainGp <- ggroup(container = saveFileWin, horizontal = FALSE)
        filetbl <- glayout(container = fileMainGp)

        l <- list()
        l[[gettext("Bitmap Image (BMP)")]] <- "bmp"
        l[[gettext("JPEG Image (JPG)")]] <- "jpg"
        l[[gettext("Portable Document Format (PDF)")]] <- "pdf"
        l[[gettext("PNG Image (PNG)")]] <- "png"
        l[[gettext("TIFF Image (TIFF)")]] <- "tiff"

        fileExtensions <- l
        pop <- function(x) x[-length(x)]
        popchar <- function(str) paste(pop(unlist(strsplit(str, ""))),
                                       collapse = "")

        filterList <- lapply(fileExtensions, function(i)
            list(patterns = paste("*.", i, sep = "")))

        ll = list()
        ll$"All files " <- list(patterns = "*")
        filterList <- c(ll, filterList)

        filetbl[2, 2] <- glabel("Local file")
        filetbl[2, 3] <- (filebrowse <- gfilebrowse(text = "Specify a file",
            action = invisible, type = "save", container = filetbl,
            filter = filterList, quote = FALSE))
        filetbl[3, 2:3] <- gseparator(container = filetbl)
        filetbl[4, 2] = gettext("File type is")
        filetbl[4, 3] <- (filetype = gdroplist(
            c("<use file extension to determine>",
              names(filterList[!filterList %in% ll])),
            container = filetbl))
        visible(filetbl) <- TRUE

        buttonGp <- ggroup(container = fileMainGp)
        addSpring(buttonGp)
        okButton <- gbutton("OK",
            handler = function(h,...) e1$okButtonHandler())
        cancelButton <- gbutton("Cancel",
            handler = function(h,...) e1$cancelButtonHandler())
        add(buttonGp, okButton)
        add(buttonGp, cancelButton)

        e1$cancelButtonHandler <- function(h,...) {
            dispose(saveFileWin)
        }

        e1$okButtonHandler <- function(h,...) {
            theFile <- svalue(filebrowse)
            ext <- NULL ## the extension, figure out

            if (theFile != "Specify a file") {
                fileType <- svalue(filetype)
                if (fileType != "<use file extension to determine>") {
                    ext <- fileExtensions[[fileType]][1]
                } else if (is.null(ext)) {
                    tmp <- unlist(strsplit(basename(theFile), split="\\."))
                    ext <- tolower(tmp[length(tmp)])
                    # In the case where we aren't able to assign a usable
                    # file extension, assume we want PNG
                    if (! ext %in% c("pdf", "png", "jpg", "tiff", "bmp"))
                        ext <- "png"
                }
                e1$saveFile(theFile, ext)
                dispose(saveFileWin)
            }
        }

        e1$saveFile <- function(theFile, ext) {
            # Determine whether a user has specified a file just by name
            # rather than using the file browser
            dirsep <- paste("[", .Platform$file.sep,
                            if (.Platform$OS.type == "windows") "\\" else NULL,
                            "]", sep = "")
            # In the case that a user has given a filename rather than
            # a file path, set the save location to the current working dir
            if (length(strsplit(theFile, dirsep)[[1]]) == 1)
                theFile <- paste(getwd(), theFile, sep = .Platform$file.sep)

            file.split <- paste("[", .Platform$file.sep,
                                if (.Platform$OS.type == "windows") "\\" else NULL,
                                ".]", sep = "")
            tmp <- unlist(strsplit(basename(theFile), split = file.split))
            ext.tmp <- tail(tmp, 1)
            # Modifying the width to take into account the aspect ratio
            # of the plots in different methods
            devwidth <- if (e$method %in% c("ci", "sampvar")) 640 else 960
            devheight <- 640

            if (length(ext) == 0) {
                gmessage(title = "Error", message = "Check file type",
                         icon = "error", container = TRUE, parent = saveFileWin)
            } else if (ext.tmp != ext) {
                theFile <- paste(theFile, ext, sep = ".")
            }

            if (ext == "pdf") {
                dev.copy2pdf(file = theFile)
            } else if (ext == "png") {
                dev.copy(png, file = theFile, width = devwidth, height = devheight)
                tmp <- dev.off()
            } else if (ext == "bmp") {
                dev.copy(bmp, file = theFile, width = devwidth, height = devheight)
                tmp <- dev.off()
            } else if (ext == "tiff") {
                dev.copy(tiff, file = theFile, width = devwidth, height = devheight)
                tmp <- dev.off()
            } else if (ext == "jpg" | ext == "jpeg") {
                dev.copy(jpeg, file = theFile, width = devwidth, height = devheight, quality = 100)
                tmp <- dev.off()
            } else {
                gmessage(title = "Error", message = "Unable to save file",
                         icon = "error", container = TRUE, parent = saveFileWin)
            }
        }
    }

    e$specifyFileForImport <- function(...) {
        e1 <- new.env()
        importFileWin <- gwindow("File Browser", container = TRUE, parent = e$win)

        # When we close the window, we want the effect to be the same as if
        # we were to click the cancel button.
        addHandlerDestroy(importFileWin, handler = function(h, ...) {
            if (! enabled(e$importData))
                enabled(e$importData) <- TRUE
        }, ...)

        fileMainGp <- ggroup(container = importFileWin, horizontal = FALSE)

        filetbl <- glayout(container = fileMainGp)

        l <- list()
        l[[gettext("Tab Delimited Text files")]] <- "txt"
        l[[gettext("CSV files")]] <- "csv"
        l[[gettext("2007 Excel files")]] <- "xlsx"
        l[[gettext("97-2003 Excel files")]] <- "xls"

        fileExtensions <- l
        pop <- function(x) x[-length(x)]
        popchar <- function(str) {
            paste(pop(unlist(strsplit(str, ""))),
                  collapse = "")
        }

        filterList <- lapply(fileExtensions, function(i) {
            list(patterns = paste("*.", i, sep = ""))
        })

        ll <- list()
        ll$"All files " <- list(patterns = "*")
        filterList <- c(ll, filterList)


        filetbl[2, 2] <- glabel("Local file")
        filetbl[2, 3] <- (filebrowse <-
            gfilebrowse(text = "Specify a file", action = invisible,
                        container = filetbl, filter = filterList,
                        quote = FALSE))
        filetbl[3, 2:3] <- gseparator(container = filetbl)
        filetbl[4, 2] <- gettext("File type is")
        filetbl[4, 3] <- (filetype = gdroplist(
            c("<use file extension to determine>",
              sapply(names(filterList[!filterList %in% ll]), popchar)),
            container = filetbl))
        visible(filetbl) <- TRUE

        buttonGp <- ggroup(container = fileMainGp)
        addSpring(buttonGp)
        okButton <- gbutton("OK",
            handler = function(h,...) e1$okButtonHandler())
        cancelButton <- gbutton("Cancel",
            handler = function(h,...) e1$cancelButtonHandler())
        add(buttonGp, okButton)
        add(buttonGp, cancelButton)

        add(fileMainGp, glabel("Space for extra options: define NA string, header presence etc."))

        e1$cancelButtonHandler <- function(h,...) {
            dispose(importFileWin)
        }

        e1$okButtonHandler <- function(h,...) {
            theFile <- svalue(filebrowse)
            ext <- NULL ## the extension, figure out

            if (theFile == "Specify a file" || ! file.exists(theFile)) {
                # Don't do anything if no file has been chosen
            } else {
                fileType <- svalue(filetype)
                if (fileType != "<use file extension to determine>") {
                    ## use filterList to get
                    fileType <- paste(fileType, "s", sep = "", collapse = "")
                    ## append s back
                    ext <- fileExtensions[[fileType]][1]
                } else if (is.null(ext)) {
                    file.split <- paste("[", .Platform$file.sep,
                                        if (.Platform$OS.type == "windows") "\\" else NULL,
                                        ".]", sep = "")
                    tmp <- unlist(strsplit(basename(theFile), split = file.split))
                    ext <- tail(tmp, 1)
                }
            e1$importFile(theFile, ext)
            }
        }

        e1$importFile <- function(theFile, ext) {
            file.split <- paste("[", .Platform$file.sep,
                                if (.Platform$OS.type == "windows") "\\" else NULL,
                                ".]", sep = "")
            tmp <- unlist(strsplit(basename(theFile), split = file.split))
            ext.tmp <- tail(tmp, 1)

            if (length(ext) == 0) {
                gmessage(title = "Error", message = "Check file type",
                         icon = "error", container = TRUE, parent = importFileWin)
            } else if (ext.tmp != ext) {
                gmessage(title = "Error", message =
                         "Chosen file is different than the selected file type",
                         icon = "error", container = TRUE, parent = importFileWin)
            } else if (ext %in% c("txt", "csv")) {
                if (ext == "txt")
                    out <- try(read.table(theFile, header = TRUE, sep = "\t",
                                          na.strings = c("NULL", "NA", "N/A", "#N/A", "", "<NA>"),
                                          check.names = TRUE))
                else
                    out <- try(read.csv(theFile, header = TRUE,
                                        na.strings = c("NULL", "NA", "N/A", "#N/A", "", "<NA>"),
                                        check.names = TRUE))
                if (inherits(out, "try-error")) {
                    gmessage(sprintf("Error loading file: %s\n", basename(theFile)),
                             "Error loading data file", icon = "error")
                    enabled(okButton) = TRUE
                    return(TRUE)
                } else {
                    enabled(okButton) <- FALSE
                    num.rows <- nrow(out)
                    out.inc.rows <- data.frame(ROW_NAME = 1:num.rows, out,
                                               check.names = TRUE)
                    tag(e$obj,"dataSet") <- out
                    tag(e$obj,"rowDataSet") <- out.inc.rows
                    tag(e$obj, "originalDataSet") <- out
                    e$inDataView <- num.rows * ncol(out) <= 200000
                    if (e$inDataView) {
                        enabled(e$dataView) <- FALSE
                        enabled(e$listView) <- TRUE
                        e$updateData()
                    } else {
                        enabled(e$dataView) <- FALSE
                        enabled(e$listView) <- FALSE
                        e$updateList()
                    }
                    enabled(okButton) <- TRUE
                    e$clearAllSlots()
                    enabled(e$importData) <- TRUE
                    dispose(importFileWin)
                    e$loaded <- FALSE
                    # Storing the filename of the data in a gui element
                    # that we will always have access to
                    tag(e$obj, "data.file") <- basename(theFile)
                }
          } else if (ext %in% c("xls", "xlsx")) {
              if (ext == "xls") {
                  channel <- if (exists("odbcConnectExcel"))
                                 try(RODBC::odbcConnectExcel(theFile, readOnly = TRUE,
                                                             readOnlyOptimize = TRUE))
                             else NULL
                  excelString <- "Excel"
              } else {
                  channel <- if (exists("odbcConnectExcel2007"))
                                 try(RODBC::odbcConnectExcel2007(theFile, readOnly = TRUE,
                                                                 readOnlyOptimize = TRUE))
                             else NULL
                  excelString <- "Excel (>= 2007)"
              }
              if (is.null(channel) || inherits(channel, "try-error")) {
                  gmessage(paste(sprintf("Error loading file: %s\n", basename(theFile)),
                                 paste("Is", excelString, "present on this system?"),
                                 sep = "\n"),
                           "Error loading Excel file", icon = "error")
                  enabled(okButton) <- TRUE
                  odbcCloseAll()
                  return(TRUE)
              } else {
                  enabled(okButton) <- FALSE
                  #no na.omit()
                  out <- try(sqlFetch(channel, sqtable = "Sheet1", as.is = TRUE,
                                      na.strings = c("NULL", "NA", "N/A", "#N/A", "", "<NA>")))
                  if (inherits(out,"try-error")) {
                      gmessage("Please ensure that the Excel worksheet containing the data is named as Sheet1\n\nIf the error persists, please save the dataset as a CSV (comma separated) file", parent = importFileWin)
                      enabled(okButton) <- TRUE
                  } else {
                      for (i in 1:length(names(out))) {
                          x <- as.numeric(out[, i])
                          if (all(is.na(x)))
                              out[, i] <- factor(as.character(out[, i]))
                          else out[, i] <- x
                      }

                      num.rows <- nrow(out)
                      out.inc.rows <- data.frame(ROW_NAME = 1:num.rows, out)
                      names(out.inc.rows) <- make.names(names(out.inc.rows), unique = TRUE)
                      tag(e$obj,"dataSet") <-  out
                      tag(e$obj,"rowDataSet") <- out.inc.rows
                      tag(e$obj, "originalDataSet") <- out
                      e$inDataView <- num.rows * ncol(out) <= 200000
                      if (e$inDataView) {
                          enabled(e$dataView) <- FALSE
                          enabled(e$listView) <- TRUE
                          e$updateData()
                      } else {
                          enabled(e$dataView) <- FALSE
                          enabled(e$listView) <- FALSE
                          e$updateList()
                      }
                      enabled(okButton) <- TRUE
                      e$clearAllSlots()
                      enabled(e$importData) <- TRUE
                      dispose(importFileWin)
                      e$loaded <- FALSE
                      odbcCloseAll()
                      # Storing the filename of the data in a gui element
                      # that we will always have access to
                      tag(e$obj, "data.file") <- basename(theFile)
                  }
              }
            }
        }
    }

    e$updateData <- function() {
        names(tag(e$obj,"dataSet")) <- make.names(names(tag(e$obj,"dataSet")),
                                                  unique = TRUE)
        tag(e$obj,"rowDataSet") <- data.frame(ROW_NAME = tag(e$obj, "rowDataSet")[, 1],
                                              tag(e$obj, "dataSet"))
        names(tag(e$obj,"rowDataSet")) <- make.names(names(tag(e$obj, "rowDataSet")), unique = TRUE)

        if(!is.null(e$dataList))
            delete(e$dataGp, e$dataList, expand = TRUE)
        if(!is.null(e$dataList1))
            delete(e$dataGp, e$dataList1, expand = TRUE)
        if(!is.null(e$dataList2))
            delete(e$dataGp, e$dataList2, expand = TRUE)
        if(!is.null(e$dataSt))
            delete(e$dataGp, e$dataSt, expand = TRUE)

        e$dataSt <- gdf(tag(e$obj,"dataSet"), expand = TRUE)
        add(e$dataGp, e$dataSt, expand = TRUE)
        dataset.names <- names(tag(e$obj, "dataSet"))
        empty.name <- "-- Select variable name --"
        e$xVar[] <- c(empty.name, dataset.names)
        svalue(e$xVar) <- empty.name
        e$xName <- NULL
        e$yVar[] <- c(empty.name, dataset.names)
        svalue(e$yVar) <- empty.name
        e$yName <- NULL
        addHandlerChanged(e$dataSt, handler = function(h,...) {
            tag(e$obj,"dataSet") <- e$dataSt[]
            dataset.names <- names(tag(e$obj, "dataSet"))
            empty.name <- "-- Select variable name --"
            e$xVar[] <- c(empty.name, dataset.names)
            svalue(e$xVar) <- empty.name
            e$xName <- NULL
            e$yVar[] <- c(empty.name, dataset.names)
            svalue(e$yVar) <- empty.name
            e$yName <- NULL
        })
        e$inDataView <- TRUE
    }

    e$updateList <- function() {
        names(tag(e$obj,"dataSet")) <- make.names(names(tag(e$obj,"dataSet")), unique = TRUE)
        tag(e$obj,"rowDataSet") <- data.frame(ROW_NAME = tag(e$obj, "rowDataSet")[, 1], tag(e$obj, "dataSet"))
        names(tag(e$obj,"rowDataSet")) <- make.names(names(tag(e$obj, "rowDataSet")), unique = TRUE)

        if (!is.null(e$dataList))
            delete(e$dataGp, e$dataList, expand = TRUE)
        if (!is.null(e$dataList1))
            delete(e$dataGp, e$dataList1, expand = TRUE)
        if (!is.null(e$dataList2))
            delete(e$dataGp, e$dataList2, expand = TRUE)
        if (!is.null(e$dataSt))
            delete(e$dataGp, e$dataSt, expand = TRUE)

        dataset.names <- names(tag(e$obj, "dataSet"))
        total.names <- length(dataset.names)
        N <- min(20, total.names)

        if (N > 0 && (length(names(tag(e$obj,"dataSet"))) < 80)) {
            d1 <- dataset.names[1:N]
            d2 <- if (total.names > N) dataset.names[(N+1):total.names]
                  else ""
            e$dataList1 <- gtable(d1, expand = TRUE)
            names(e$dataList1) <- "VARIABLES"
            e$dataList2 <- gtable(d2, expand = TRUE)
            names(e$dataList2) <- "...CONTINUED"
            adddropsource(e$dataList1)
            adddropsource(e$dataList2)
            add(e$dataGp, e$dataList1, expand = TRUE)
            add(e$dataGp, e$dataList2, expand = TRUE)
        } else {
            d <- names(tag(e$obj,"dataSet"))
            e$dataList <- gtable(d,expand = TRUE)
            names(e$dataList) <- "VARIABLES"
            adddropsource(e$dataList)
            add(e$dataGp, e$dataList, expand = TRUE)
        }

        e$inDataView <- FALSE
    }

    e$viewData <- function(h, ...) {
        if (is.null(tag(e$obj, "dataSet"))) {
            gmessage("Please load a new data set (with named columns)",
                     parent = e$win)
        } else if ((names(tag(e$obj, "dataSet"))[1] == "empty")) {
            gmessage("Please load a new data set", parent = e$win)
        } else {
            enabled(h$obj) = FALSE
            e$updateData()
            enabled(e$listView) = TRUE
            e$inDataView = TRUE
        }
    }

    e$viewList <- function(h, ...) {
        if (is.null(tag(e$obj, "dataSet"))) {
            gmessage("Please load a new data set (with named columns)",
                     parent = e$win)
        } else if (names(tag(e$obj, "dataSet"))[1] == "empty") {
            gmessage("Please load a new data set", parent = e$win)
        } else {
            enabled(h$obj) <- FALSE
            e$updateList()
            enabled(e$dataView) <- TRUE
            e$inDataView <- FALSE
        }
    }

    # buildCanvas creates a canvas object from the R5 reference class canvas. This canvas object is saved in the GUI environment and handles all of the graphical displays in the vit tool. It may help to keep GUI methods (functions that begin with e$ ) separate in your mind from the canvas methods (functions that begin with e$c1$    ). They behave a little differently. In general GUI methods affect the gui environment and canvas methods affect the canvas object. Handler functions that work with both are saved to the top level whenever possible. Indexes and samples can be given so that a new canvas object cointains the same samples as the previous one.
    e$buildCanvas <- function(paired.samples = FALSE) {
        if (e$method == "permvar") {
            # Saving info as it will get wiped when we create new a new canvas.
            # We need to do this for the permvar methods in particular.
            tmp.ngroups <- e$c1$ngroups
            tmp.levels <- e$c1$levels
            tmp.ylevels <- e$c1$ylevels
            retain.vars <- TRUE
        } else {
            retain.vars <- FALSE
        }

        if (paired.samples) {
            e$c1 <- canvasPlot$new(data.boxes = e$data.boxes,
                                   plot.load.function = "load_numeric_1d",
                                   x = e$xData - e$yData, levels = NULL,
                                   paired.data = cbind(e$xData, e$yData),
                                   x.name = e$xName, y.name = e$yName)
        } else {
            e$c1 <- canvasPlot$new(data.boxes = e$data.boxes,
                                   plot.load.function =
                                       if (is.numeric(e$xData) & is.numeric(e$yData))
                                           "load_numeric_2d"
                                       else
                                           NULL,
                                   x = e$xData, levels = e$yData,
                                   x.name = e$xName, y.name = e$yName)
        }

        ## loads the data dependent details that allow the canvas to perform
        ## its basic actions. NOTE: should actions be stored in e?
        if (is.numeric(e$stat.scale)) {
            stat.scale <- e$stat.scale
        } else if (is.categorical(e$xData)) {
            stat.scale <- c(0, 1)
        } else if (e$stat.scale) {
            stat.scale <- range(e$xData)
        } else {
            stat.scale <- range(e$xData) - mean(range(e$xData))
        }

        # When we have set a value for our LoI try using that for plotData
        # otherwise just use the first level
        if (is.categorical(e$xData)) {
            currLevel <- svalue(e$loi.choices)
            if (currLevel != "")
                e$c1 <- setLevelOfInterest(e$c1, currLevel)
            else
                e$c1 <- setLevelOfInterest(e$c1, NULL)
        }

        if (retain.vars) {
            # Because we end up wiping all of the info needed for permvar
            # methods, restore them back into the canvas so we end up with
            # correct viewports and plotting methods.
            e$c1$ngroups <- tmp.ngroups
            e$c1$levels <- tmp.levels
            e$c1$ylevels <- tmp.ylevels
        }

        buildViewports(e$c1, e$c1$x, e$c1$levels, e$c1$data.boxes, stat.scale = stat.scale)
        e$c1$buildImage(!is.null(e$c1$levels) && !is.categorical(e$c1$levels))
        pushViewport(e$c1$viewports)
        e$c1$plotData()
    }

    e$clearAllSlots <- function() {
        empty.name <- "-- Select variable name --"
        svalue(e$xVar) <- empty.name
        e$xData <- NULL
        tag(e$obj,"e$xVarData") <- NULL
        svalue(e$yVar) <- empty.name
        e$yData <- NULL
        tag(e$obj,"e$yVarData") <- NULL
    }

    e$reverseVariables <- function() {
        temp <- e$xData
        e$xData <- e$yData
        e$yData <- temp

        temp <- svalue(e$xVar)
        svalue(e$xVar) <- svalue(e$yVar)
        svalue(e$yVar) <- temp
    }

    # Arranges all the details for calculating statistics by making samples and
    # picking a correct sampling method.
    e$sample_check <- function() {
        # check for potential trouble
        if (!is.null(e$xData)) {
            if (e$replace == FALSE & as.numeric(svalue(e$ssize)) >
                length(e$xData)) {
                grid.newpage()
                grid.text("Sample size can not exceed data size when sampling without replacement.")
                svalue(e$ssize) <- length(e$xData)
                enabled(e$upper) <- TRUE
                enabled(e$lower) <- FALSE
                return(FALSE)
            }

            if (as.numeric(svalue(e$ssize)) < 2) {
                grid.newpage()
                grid.text("Sample size must be > 1.")
                svalue(e$ssize) <- 2
                enabled(e$upper) <- TRUE
                enabled(e$lower) <- FALSE
                return(FALSE)
            }
        }
        return(TRUE)
    }

    e$variable_check <- function() {
        e$data.loaded <- FALSE
        if (is.null(e$xData)) {
            grid.newpage()
            grid.text("Please select Variable 1")
            enabled(e$obj) <- TRUE
            return()
        }

        if (is.categorical(e$xData) & !is.categorical(e$yData) &
            !is.null(e$yData)) {
            e$reverseVariables()
        }

        if (!is.categorical(e$xData) & !is.categorical(e$yData) &
            !is.null(e$yData)) {
            grid.newpage()
            return()
        }

        e$data.loaded <- TRUE
    }

    e$na_check <- function(for.x = TRUE) {
        if (for.x) {
            e$xNA <- e$xData
            if (is.null(e$yData)) {
                e$xData <- e$xNA[!is.na(e$xNA)]
            } else {
                subset <- is.na(e$xNA) | is.na(e$yNA)
                e$xData <- e$xNA[!subset]
                e$yData <- e$yNA[!subset]
            }
        } else {
            e$yNA <- e$yData
            if (is.null(e$xData)) {
                e$yData <- e$yNA[!is.na(e$yNA)]
            } else {
                subset <- is.na(e$xNA) | is.na(e$yNA)
                e$xData <- e$xNA[!subset]
                e$yData <- e$yNA[!subset]
            }
        }
    }

    e$confirmDialog <- function(message, handler = NULL) {
        e$window <- gwindow("Confirm", width = 20, height = 20)
        group <- ggroup(container = e$window)
        gimage("info", dirname = "stock", size = "dialog", container = group)

        ## A group for the message and buttons
        inner.group <- ggroup(horizontal = FALSE, container = group)
        glabel(message, container = inner.group, expand = TRUE)

        ## A group to organize the buttons
        button.group <- ggroup(container = inner.group)
        ## Push buttons to right
        addSpring(button.group)
        gbutton("OK", handler = handler, container = button.group)
        gbutton("Cancel", handler = function(h,...) {
                    dispose(e$window)
                }, container = button.group)

        return()
    }

    ## Clears bottom two panels of canvas.
    e$resetCanvas <- function() {
        e$buildCanvas()
        if (e$data.boxes) e$c1$buildBoxes()
        e$c1$drawImage()
    }

    ## Clears bottom two panels of canvas but holds onto current sample.
    e$resetCanvasKeepSample <- function(old.canvas) {
        old.samples <- old.canvas$samples
        old.indexes <- old.canvas$indexes
        e$resetCanvas()
        e$c1$samples <- old.samples
        e$c1$indexes <- old.indexes
        e$c1$which.sample <- 1
    }

    ## Clears bottom panel of canvas
    e$clearPanel <- function(panel = "stat") {
        clear.panel <- paste(panel, "Plot", sep = "")
        grobs <- childNames(e$c1$image)
        grobs.to.clear <- grobs[substr(grobs, 1, nchar(panel) + 4) == clear.panel]
        for (i in grobs.to.clear)
            e$c1$image <- removeGrob(e$c1$image, i)
    }

    e
}
