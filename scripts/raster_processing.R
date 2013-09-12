library(spatial.tools)

tahoe_highrez <- brick(system.file("external/tahoe_highrez.tif", package="spatial.tools"))

# Pixel-based processing:
ndvi_function <- function(x,...) {
  # Note that x is received by the function as a 3-d array:
  red_band <- x[,,2]
  nir_band <- x[,,3]
  ndvi <- (nir_band - red_band)/(nir_band + red_band)
  # The output of the function should also be a 3-d array,
  # even if it is a single band:
  ndvi <- array(ndvi,dim=c(dim(x)[1],dim(x)[2],1))
  return(ndvi)
}

sfQuickInit(cpus=1)
system.time(tahoe_ndvi <- focal_hpc(x=tahoe_highrez, fun=ndvi_function))
sfQuickStop()

# Focal-based processing:
local_smoother <- function(x,...) {
  # Assumes a 3-d array representing
  # a single local window, and return
  # a single value or a vector of values.
  smoothed <- apply(x,3,mean)
  return(smoothed)
}

# Apply the function to a 3x3 window:
sfQuickInit(cpus=2)
tahoe_3x3_smoothed <- focal_hpc(x=tahoe_highrez,fun=local_smoother,window_dims=c(3,3))
sfQuickStop()
# Example with 7 x 7 window in full parallel mode:
sfQuickInit()
tahoe_7x7_smoothed <- focal_hpc(x=tahoe_highrez,fun=local_smoother,window_dims=c(7,7))
sfQuickStop(