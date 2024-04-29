# FluoroProbe heatmap
# Author: Mary Lofton
# Date: 26APR24

# Purpose: visualize FP data to compare to GLM-AED calibration

# interpolation function borrowed from glmtools
# https://rdrr.io/github/USGS-R/glmtools/src/R/timeseries_plots.R
.interpolate2grid <- function(xyzData, xcol = 1, ycol = 2, zcol = 3) {
  # Interpolate field or modeled data to grid 
  # xcol, ycol, and zcol and column numbers from data.frame
  # The spreads of x and y must be within four orders of magnitude of each other for interp to work
  # Therefore must scale data to be within similar magnitude to numeric dates (1e6)
  gridData <-interp2xyz(interp(
    x = as.numeric(xyzData[,xcol]), y=xyzData[,ycol]*1e6, z=xyzData[,zcol], 
    duplicate="mean", linear = T,
    xo = as.numeric(seq(min(xyzData[,xcol]), max(xyzData[,xcol]), by = 'day')),
    yo = 1e6*seq(min(xyzData[,ycol]), max(xyzData[,ycol]), by = 0.1)), 
    data.frame=TRUE) %>%
    dplyr::mutate(x = as.POSIXct(.data$x, origin = '1970-01-01', 
                                 tz = Sys.timezone())) %>%
    dplyr::mutate(y = .data$y/1e6) %>%
    dplyr::arrange(.data$x, .data$y)
  
  return(gridData)
}

flora_heatmap <- function(fp_data, reservoir, years, z){
  
  #subset to relevant data
  fp <- fp_data %>%
    select(DateTime, Depth_m, {{z}}) 
  fp <- data.frame(fp)
  
  
  observed_df <- .interpolate2grid(fp, xcol = 1, ycol = 2, zcol = 3) %>% 
    rename(DateTime = .data$x, Depth = .data$y, var = .data$z)
  
  legend.title = "FP biomass w/o cyanos (ug/L)" 
  text.size = 12
  color.palette = 'RdYlBu' 
  color.direction = -1 
  obs.color = 'white' 
  obs.alpha = 0.6 
  obs.shape = 16 
  obs.size = 1
  shiftPalette = NULL 
  zlim = c(0,45)
  
  h1 = ggplot(data = observed_df, aes(x = .data$DateTime, y = .data$Depth)) +
    geom_raster(aes(fill = .data$var), interpolate = F) +
    scale_y_reverse(expand = c(0.01,0.01)) +
    scale_x_datetime(expand = c(0.01,0.01), limits = c(min(observed_df$DateTime), max(observed_df$DateTime))) +
    scale_fill_distiller(palette = color.palette, direction = color.direction, na.value = "grey90", limits = zlim) +
    ylab('Depth (m)') + xlab('Date') +
    labs(fill = legend.title) +
    theme_bw(base_size = text.size)
  
  return(h1)
  
}
