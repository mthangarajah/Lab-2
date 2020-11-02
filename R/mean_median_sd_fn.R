#' Statistics of Average Medicare Payments
#'
#'This function produces a table of either mean, median or standard deviation
#'of the average medicare payments by DRG code
#'
#' @param x a string, either mean, median or sd (standard deviation)
#'
#' @return Table of of either mean, median or standard deviation
#'of the average medicare payments by DRG code
#'
#'@import dplyr
#'@import tidyverse
#'@import knitr
#'
#' @export
#'
#' @examples
#' mean_median_sd("sd")
#'
#'
mean_median_sd <- function(x){

  #Reads in data from data folder
  data("data_inpatient")
  delayedAssign("df_inpatient", data_inpatient)
  summary(df_inpatient)

  #Assigns input variable x to a new variable n
  n <- x

  #check if n is equal to mean
  if (n == "mean"){


    df_inpatient %>%

      group_by(DRG.Definition) %>% #Group data by DRG code

      #summarise mean of data by DRG code
      summarise(Mean = mean(Average.Medicare.Payments)) %>%

      #Create nice table
      knitr::kable()


    #check if n is equal to median
  } else if (n == "median"){
    df_inpatient %>%

      group_by(DRG.Definition) %>% #Group data by DRG code

      #summarise median of data by DRG code
      summarise(Median = median(Average.Medicare.Payments)) %>%

      #Create nice table
      knitr::kable()

    # if n is not equal to either mean or median
  } else {

    df_inpatient %>%

        group_by(DRG.Definition) %>% #Group data by DRG code

      #summarise standard deviation of data by DRG code
      summarise(SD = sd(Average.Medicare.Payments)) %>%

      #Create nice table
      knitr::kable()

  }}


