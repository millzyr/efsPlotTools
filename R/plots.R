#' Plot a bargraph of frequencies
#'
#' This function provides a template for producing bargraphs in the Economic Farm Survey that
#' can be used to display histogram data on a bargraph (expressed as a percentage). Also
#' includes custom tooltip text that allows this percentage to be shown in HTML output
#'
#' @param df The dataset to access data from
#' @param x The variable to display the distribution of
#' @param y_points The number of ticks to place on the y axis
#' @param x_scale Handles the rounding of the bin intervals. For example, 0.1 would set the bin groupings so they occur between two one decimal place numbers
#' @param y_scale Handles the rounding of the y axis. Set to 0.05 (or 5%) by default
#' @return p A ggplot2 object
#' @export
construct_bargraph_frequency <- function(df, x, y_points, x_scale = 0.1, y_scale = 0.05){
  x <- sym(x)
  payout_interval <- df %>%
    select(!!x) %>%
    summarise(low = floor(min(!!x) / x_scale) * x_scale,
              high = ceiling(max(!!x) / x_scale) * x_scale)
  payout_breaks <- seq(from = payout_interval %>%
                         select(`low`) %>% pull(),
                       to = payout_interval %>%
                         select(`high`) %>% pull(),
                       by = (payout_interval %>%
                               select(`high`) %>% pull() - payout_interval %>%
                               select(`low`) %>% pull()) / y_points)
  #Create the tag labels
  payout_tags <- concatenate_numbers(payout_breaks)

  #Summarise removes the total number of rows.
  obs_num <- nrow(df)

  #Create the binned data
  df <- df %>% mutate(bin = cut(`Milk Payout Received ($ per kg MS)`, breaks = payout_breaks,
                                labels = payout_tags))

  #Convert to percentages
  df <- df %>% group_by(`bin`) %>% summarise(freq = n() / obs_num) %>%
    mutate(`Frequency` = `freq` * 100)


  # Setup the limits for the y scale
  figure_5_1_y_upper_limit <- df %>% summarise(`maximum` = max(`freq`)) %>% pull()
  # Scale it to the nearest 0.5
  figure_5_1_y_upper_limit <- ceiling(figure_5_1_y_upper_limit / y_scale) * y_scale

  p <- df %>% ggplot(aes(x = `bin`, text = paste('Bin: ', `bin`,'</br></br>Frequency: ',
                                                 round(`freq` * 100,2), '%', sep=""))) +
    theme_minimal() +
    geom_bar(aes(y = freq), fill = "#7ac143", width = 0.5, stat = "identity") +
    scale_y_continuous(labels = percent, breaks = seq(0,figure_5_1_y_upper_limit,0.05), limits =
                         c(0,figure_5_1_y_upper_limit))
  return(p)
}
