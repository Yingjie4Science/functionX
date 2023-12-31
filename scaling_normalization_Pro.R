



##' deal with extreme values --------------------------------------------------------- #
##'   before using *Min-Max Scaling* method to scale the data to a specific range (0-100), 
##'   we first use *Robust Scaling* to reduce the impact of extreme values on the scaling process.
##' [Data Normalization With R](https://medium.com/swlh/data-normalisation-with-r-6ef1d1947970)

library(dplyr)
library(scales)

## Custom Robust Scaling function with NA handling ---------------------------------------
robust_scale <- function(x) {
  median_val <- median(x, na.rm = TRUE)
  iqr_val <- IQR(x, na.rm = TRUE)
  scaled_val <- (x - median_val) / iqr_val
  scaled_val
}


## Functions

func_robust_scaling <- function(data, var){
  d <- data %>%
    dplyr::mutate_at(vars(var), ~ robust_scale(.))
  return(d)
}


func_z_scaling <- function(data, var){
  d <- data %>%
    dplyr::mutate_at(vars(var), ~ base::scale(., center = TRUE, scale = TRUE))
  return(d)
}


###' If we want to scale between some bounded arbitrary set of values [a, b]
###'  Mean Normalisation
func_norm_minmax <- function(x, a=0, b=1){
  a + (x- mean(x, na.rm = TRUE)) * (b-a) / (max(x, na.rm = TRUE) - min(x, na.rm = TRUE))
}

