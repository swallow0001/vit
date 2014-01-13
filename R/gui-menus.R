getModuleMenu <- function(e) {
    tmp.g.menu <- e$g.menu[]

    # Setting GUI menu entries for when we want to change modules
    permvar.menu <- gaction("Randomisation variation", handler = function(...) {
                               e$confirmDialog("Do you wish to change modules?",
                                               handler = function(...) {
                                   dispose(e$win)
                                   dispose(e$window)
                                   dev.off()
                                   newdevice(height = 7.5, width = 11.25)
                                   e$title <- "Randomisation variation"
                                   setupGUI(e)
                                   permvarGUIHandler(e)
                                   dataGUI(e)
                               })
                           })
    perm.menu <- gaction("Randomisation tests", handler = function(...) {
                            e$confirmDialog("Do you wish to change modules?",
                                            handler = function(...) {
                                dispose(e$win)
                                dispose(e$window)
                                dev.off()
                                newdevice(height = 7.5, width = 11.25)
                                e$title <- "Randomisation tests"
                                setupGUI(e)
                                permGUIHandler(e)
                                dataGUI(e)
                            })
                        })
    sampvar.menu <- gaction("Sampling variation", handler = function(...) {
                               e$confirmDialog("Do you wish to change modules?",
                                               handler = function(...) {
                                   dispose(e$win)
                                   dispose(e$window)
                                   dev.off()
                                   newdevice(height = 7.5, width = 7.5)
                                   e$title <- "Sampling variation"
                                   setupGUI(e)
                                   sampvarGUIHandler(e)
                                   dataGUI(e)
                               })
                           })
    bootstrap.menu <- gaction("Bootstrap confidence intervals", handler = function(...) {
                                 e$confirmDialog("Do you wish to change modules?",
                                                 handler = function(...) {
                                     dispose(e$win)
                                     dispose(e$window)
                                     dev.off()
                                     newdevice(height = 7.5, width = 11.25)
                                     e$title <- "Bootstrap confidence intervals"
                                     setupGUI(e)
                                     bootstrapGUIHandler(e)
                                     dataGUI(e)
                                 })
                             })
    ci.menu <- gaction("Confidence interval coverage", handler = function(...) {
                          e$confirmDialog("Do you wish to change modules?",
                                          handler = function(...) {
                              dispose(e$win)
                              dispose(e$window)
                              dev.off()
                              newdevice(height = 7.5, width = 7.5)
                              e$title <- "Confidence interval coverage"
                              setupGUI(e)
                              CIGUIHandler(e)
                              dataGUI(e)
                          })
                      })

    all.menus <- list(permvar.menu, perm.menu, sampvar.menu,
                      bootstrap.menu, ci.menu)
    menu.names <- c("Randomisation variation",
                    "Randomisation tests",
                    "Sampling variation",
                    "Bootstrap confidence intervals",
                    "Confidence interval coverage")
    method.names <- c("permvar", "permutation", "sampvar", "bootstrap", "ci")
    ind <- which(e$method == method.names)
    tmp.g.menu$File$"VIT Modules" <- all.menus[-ind]
    names(tmp.g.menu$File$"VIT Modules") <- menu.names[-ind]

    tmp.g.menu
}
