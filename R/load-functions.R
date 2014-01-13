# calls the load function for the selected statistic method. The load
# functions are stored together with the details they load in the
# methods file for each method
loadStatDetails <- function(e) {
    ## is.diff <- if (! e$loaded) "diff" else NULL
    ## This screws up single sample bootstrapping (tries to invoke
    ## bootstrapping of two samples). Why is it required?
    is.diff <- NULL
    if (!is.null(e$c1$levels)){
        if (is.categorical(e$c1$levels)){
            is.diff <- if (length(e$c1$ylevels) == 2) "diff" else "anova"
        } else {
            is.diff <- "regression"
        }
    } else if (e$method == "permutation"){
        if (svalue(e$paired.samples)){
            is.diff <- "paired"
        }
    }
    stat.method <- paste(c(e$method, svalue(e$stat), is.diff), collapse = ".")
    loadfun <- list("ci.mean" = load_CI_mean,
                    "ci.median" = load_CI_median,
                    "ci.proportion" = load_CI_proportion,
                    "bootstrap.mean" = load_bootstrap_mean,
                    "bootstrap.mean.diff" = load_bootstrap_mean_diff,
                    "bootstrap.median" = load_bootstrap_median,
                    "bootstrap.median.diff" = load_bootstrap_median_diff,
                    "bootstrap.proportion" = load_bootstrap_proportion,
                    "bootstrap.proportion.diff" = load_bootstrap_proportion_diff,
                    "bootstrap.lower quartile" = load_bootstrap_LQ,
                    "bootstrap.lower quartile.diff" = load_bootstrap_LQ_diff,
                    "bootstrap.upper quartile" = load_bootstrap_UQ,
                    "bootstrap.upper quartile.diff" = load_bootstrap_UQ_diff,
                    "bootstrap.IQR ratio.diff" = load_bootstrap_iqr_ratio,
                    "bootstrap.slope.regression" = load_bootstrap_slope,
                    "permutation.mean.diff" = load_permutation_mean,
                    "permutation.mean.paired" = load_permutation_mean_paired,
                    "permutation.median.diff" = load_permutation_median,
                    "permutation.median.paired" = load_permutation_median_paired,
                    "permutation.proportion.diff" = load_permutation_proportion,
                    "permutation.proportion.anova" = load_permutation_proportion_ksample,
                    "permutation.mean.anova" = load_permutation_mean_ksample,
                    "permutation.median.anova" = load_permutation_median_ksample,
                    "permutation.slope.regression" = load_permutation_slope,
                    "permvar.mean.diff" = load_permvar_mean_diff,
                    "permvar.mean.anova" = load_permvar_mean_ksample,
                    "permvar.median.diff" = load_permvar_median_diff,
                    "permvar.median.anova" = load_permvar_median_ksample,
                    "permvar.proportion.diff" = load_permvar_proportion_diff,
                    "permvar.proportion.anova" = load_permvar_proportion_ksample,
                    "sampvar.default" = load_sampvar_default,
                    "sampvar.default.diff" = load_sampvar_default_diff,
                    "sampvar.mean" = load_sampvar_mean,
                    "sampvar.mean.diff" = load_sampvar_mean_diff,
                    "sampvar.median" = load_sampvar_median,
                    "sampvar.median.diff" = load_sampvar_median_diff,
                    "sampvar.lower quartile" = load_sampvar_LQ,
                    "sampvar.upper quartile" = load_sampvar_UQ,
                    "sampvar.proportion" = load_sampvar_proportion,
                    "sampvar.proportion.diff" = load_sampvar_proportion_diff,
                    "sampvar.lower quartile.diff" = load_sampvar_LQ_diff,
                    "sampvar.upper quartile.diff" = load_sampvar_UQ_diff,
                    "sampvar.IQR ratio.diff" = load_sampvar_iqr_ratio,
                    "sampvar.slope.regression" = load_sampvar_slope)[[stat.method]]
    if (is.null(loadfun) | (e$method == "permvar" & ! is.null(e$yData))) {
        plot.new()
        grid.text("Methods do not yet exist for this data type in this module.")
        e$loaded <- FALSE
    } else {
        loadfun(e)
        e$loaded <- TRUE
    }
}
