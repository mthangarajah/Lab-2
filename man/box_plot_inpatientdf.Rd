% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/boxplot_drg.R
\name{box_plot_inpatientdf}
\alias{box_plot_inpatientdf}
\title{Boxplot}
\usage{
box_plot_inpatientdf(
  vy = c("Average.Medicare.Payments", "Average.Total.Payments",
    "Average.Covered.Charges")
)
}
\arguments{
\item{vy}{a string name for the variable y in the dataframe d, either
Average.Medicare.Payments, Average.Total.Payments, Average.Covered.Charges}
}
\value{
A boxplot of DRG codes versus \code{vy} grouped by DRG Codes
}
\description{
This function produces a boxplot of \code{vy}
grouped by DRG codes
}
\examples{
box_plot_inpatientdf("Average.Medicare.Payments")

}
