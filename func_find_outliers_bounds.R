


### remove large outliers in the data ----------------------------------------------------

### - find out the outliers
###   https://statsandr.com/blog/outliers-detection-in-r/

## 1. 
# out <- boxplot.stats(x)$out %>% sort()
# out_min <- min(out, na.rm = T); out_min



#' Here we implement the inter-quantile-range based method as originally formulated by John Tukey. [1] [2]. 
#' Note: This is one of the most common definitions for "whiskers" on a box-and-whiskers plot
find_outlier_bounds_iqr <- function(x, k = 1.5){
  Q <- quantile(x, probs=c(.25, .75), na.rm = T)
  iqr <- IQR(x, na.rm = T)
  upper_bound <-  Q[2] + k*iqr  # Upper Range  
  lower_bound <- Q[1]  - k*iqr  # Lower Range
  bounds <- c(lower_bound, upper_bound)
  names(bounds) <- c('lower', 'upper')
  return(bounds)
}


## 2. Hampel filter ----------------------------------------------------------------------
#' Hampel filter, consists of considering as outliers the values outside the interval (I) 
#'    formed by the median, plus or minus 3 median absolute deviations (MAD)
#'    
find_outlier_bounds_Hampel <- function(x, k = 3){
  lower_bound <- median(x, na.rm = T) - k * mad(x, constant = 1, na.rm = T)
  upper_bound <- median(x, na.rm = T) + k * mad(x, constant = 1, na.rm = T)
  # outlier_index <- which(x < lower_bound | x > upper_bound) %>% sort()
  
  bounds <- c(lower_bound, upper_bound)
  names(bounds) <- c('lower', 'upper')
  return(bounds)
}





## 3. Rosner's test ----------------------------------------------------------------------
library(EnvStats)

#' Title
#'
#' @param x numeric vector of observations. Missing (NA), undefined (NaN), 
#'    and infinite (Inf, -Inf) values are allowed but will be removed. 
#'    There must be at least 10 non-missing, finite observations in x.
#' @param k positive integer indicating the number of suspected outliers. 
#'    The argument k must be between 1 and where denotes the number of non-missing, 
#'    finite values in the arguemnt x. The default value is k=3.
#'
#' @return
#' @export
#'
#' @examples
find_outlier_bounds <- function(x, k = 3){
  x <- x[!is.na(x) & !is.infinite(x)]
  test <- EnvStats::rosnerTest(x, k)
  test_df <- test$all.stats %>%
    dplyr::filter(Outlier == FALSE)
  upper <- max(test_df$Value, na.rm = T)
  lower <- min(test_df$Value, na.rm = T)
  
  bounds <- c(lower, upper)
  names(bounds) <- c('lower', 'upper')
  return(bounds)
}
