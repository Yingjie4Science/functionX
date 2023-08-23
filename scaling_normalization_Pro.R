



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
  data <- data %>%
    dplyr::mutate_at(vars(var), ~ robust_scale(.))
}


func_z_scaling <- function(data, var){
  data <- data %>%
    dplyr::mutate_at(vars(var), ~ base::scale(., center = TRUE, scale = TRUE))
}