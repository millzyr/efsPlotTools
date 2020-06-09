#' @param df The dataset to access data from
#' @param x The variable to group the bars with
#' @param y The y value of each group
#' @param y_points The number of ticks to place on the y axis
#' @param y_label Allows you to place a prefix on the y-axis (e.g. $)
#' @param y_scale Handles the rounding of the y axis. Set to 0.05 (or 5\%) by default. Use 1 for whole numbers.
#' @return A ggplot2 object
#' @export
#'
construct_bargraph <- function(df, x, y, y_points, y_label,  y_scale = 1){
  x_ <- sym(x)
  y_ <- sym(y)

  # Setup the limits for the y scale
  y_upper_limit <- df %>% summarise(`maximum` = max(!!y)) %>% pull()
  # Scale it to the nearest 0.5
  y_upper_limit <- ceiling(y_upper_limit / y_scale) * y_scale

  y_labels <- paste("$",seq(0,y_upper_limit, y_scale), sep="")

  p <- df %>%
    ggplot(aes(x = !!x_, y = !!y_, text = paste(x,': ', !!x,'</br></br>',y,': ',y_label,round(!!y_,2)))) +
    theme_minimal() +
    geom_bar(aes(y = !!y_), fill = "#7ac143", width = 0.5, stat = "identity") +
    scale_y_continuous(labels =  y_labels, breaks = seq(0,y_upper_limit, y_scale), accuracy = decimal_places(y_scale),  limits =
                         c(0,y_upper_limit))

  return(p)
}

