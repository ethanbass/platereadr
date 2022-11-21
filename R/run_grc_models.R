#' Run growthcurver models
#' @import growthcurver
#' @import dplyr
#' @import rlang
#' @importFrom purrr map
#' @importFrom tidyr unnest
#' @param x tidy growth curve data
#' @param lambda Character vector specifying wavelength (lambda) to use.
#' @param what what to return. Either \code{models}, \code{predictions}, or
#' \code{both}.
#' @param plot_it Whether to plot the growth curves
#' @author Ethan Bass
#' @note: This function is based on code by Daniel Padfield (https://github.com/sprouffske/growthcurver/issues/12)
#' @export
run_grc_model <- function(x, lambda, what = c("models","predictions","both"), plot_it = TRUE){
  what <- match.arg(what, c("models","predictions","both"))

  output <- tibble(well = unique(x$name), model=list(NA), vals=list(NA), data=list(NA))
  for (well in unique(x$name)){
    cond <- which(x$lambda == lambda & x$name == well & x$Time > 5)
    m <- SummarizeGrowth(x[cond,"Time"], x[cond,"value"], bg_correct = "min")
    idx <- which(output$well == well)
    output$model[[idx]] <- m$model
    output$vals[[idx]] <- m$vals %>% unlist() %>% bind_rows() %>%
      mutate(across(.data$k:.data$auc_e, as.numeric))
    output$data[[idx]] <- m$data
  }
  rm <- which(sapply(output$model,is.character))
  output[rm,2] <- NA


  d_preds <- mutate(output, preds = map(.data$model, broom::augment)) %>%
    unnest(.data$preds) %>%
    select(-c(2:4))

  d_preds$well <- factor(d_preds$well, levels = unique(d_preds$well))
  d_preds <- mutate(d_preds, row = substr(.data$well,1,1),
                    column=substr(.data$well,2,3))
  d_preds$column <- factor(d_preds$column, c(1:12))

  if (plot_it){
    p <- plot_growth(d_preds)
    print(p)
  }

  switch(what,
         "models" = output,
         "predictions" = d_preds,
         "both" = list(output, d_preds))
}

#' Run growthcurver models
#' @import ggplot2
#' @param x A data.frame containing predictions from \code{run_grc_models}
#' @param line_col color of fitted lines
#' @param pt_col A factor in predictions data.frame to be used to determine colors
#' of data points.
#' @param plot_curves Logical. Whether to plot growth curves or not.
#' @param facets Logical. Whether to plot facets or not.
#' @author Ethan Bass
#' @export
plot_growth <- function(x, line_col = "black", pt_col, plot_curves = TRUE,
                        facets = TRUE){
  p <- ggplot(x) +
    geom_point(aes(t, n, color={{pt_col}}))
  if (plot_curves)
    p <- p + geom_line(aes(.data$t, .data$.fitted), col = line_col)
  if (facets){
    p <- p + facet_grid(rows=vars(.data$row), cols=vars(.data$column))
  }
  p
}
