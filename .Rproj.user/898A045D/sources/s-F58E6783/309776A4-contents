#' Boxplot
#'
#'This function produces a boxplot of \code{vy}
#'grouped by DRG codes
#'
#' @param vy a string name for the variable y in the dataframe d, either
#' Average.Medicare.Payments, Average.Total.Payments, Average.Covered.Charges
#'
#' @return A boxplot of DRG codes versus \code{vy} grouped by DRG Codes
#' @export
#'
#'@importFrom stringr str_to_title
#'@import dplyr
#'@import tidyverse
#'@import ggplot2
#'
#' @examples
#' box_plot_inpatientdf("Average.Medicare.Payments")
#'
box_plot_inpatientdf <- function(vy = c("Average.Medicare.Payments", "Average.Total.Payments", "Average.Covered.Charges")){
  y <- vy #variable y assigned to variable

  #Load data into function from data folder
  data("data_inpatient")
  delayedAssign("d", data_inpatient)
  summary(d)


  #Group the data from the dataframe above by DRG code
  df <- d %>%
    group_by(DRG.Definition)

  #Have the codes only in the column
  df$DRG.Definition <- substr(df$DRG.Definition, 1, 4)

  #Remove . from y labels for plot
  lab_y <- gsub(".", " ", y, fixed = TRUE)

  #  Create plot by x and y variables
  ggplot(df, aes_string(x = "DRG.Definition", y = vy)) +
    geom_boxplot() + #Boxplot

    # Labels for x and y axis
    xlab("DRG Codes") +
    ylab(str_to_title(lab_y)) +

    # Change position of x-axis labels
    theme(axis.text.x = element_text(angle = 90)) +

    #Title for the plot
    ggtitle(paste0("Boxplot of ", lab_y))

}




