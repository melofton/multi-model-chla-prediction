#Plot observations
#Author: Mary Lofton
#Date last updated: 15APR24

# Purpose: plot chlorophyll-a observations with shading for training/
# calibration periods and lines for example prediction dates

library(RColorBrewer)

#'Function to plot chl-a observations
#'@param observations data frame with columns:
#'Date: yyyy-mm-dd
#'Chla_ugL: observed daily mean of chlorophyll-a from EXO in ug/L
#'@param pred_only TRUE/FALSE if you want to plot only the prediction period
#'@param focal_dates list of dates where you want to insert vlines to indicate example prediction dates
#'@param forecast_horizon numeric; use only if you are plotting a rectangle to indicate example prediction periods
#'@param plotly TRUE/FALSE should output be plotly figure? doesn't work well with some of the other aesthetics tho
#'@param train_test_box TRUE/FALSE plot rectangles to indicate train/test periods?
#'@param training_dates character vector of start and end dates for training period (yyyy-mm-dd) if train_test_box == TRUE
#'@param testing_dates character vector of start and end dates for testing period (yyyy-mm-dd) if train_test_box == TRUE
#'@param focal_dates_geom either "RECT" or "LINE"


PlotObservations <- function(observations, pred_only, focal_dates, forecast_horizon,
                             plotly, train_test_box, training_dates, testing_dates,
                             focal_dates_geom){
  
  if(pred_only == TRUE){
    observations <- observations %>%
      filter(lubridate::year(datetime) %in% c(2022,2023))
  }
  
  p <- ggplot()+
    xlab("")+
    ylab(expression(paste("Chlorophyll-a (",mu,g,~L^-1,")")))+
    theme_bw()+
    ylim(NA,max(observations$Chla_ugL_mean)+3)
  
  if(pred_only == TRUE){
    p <- p + ggtitle("Blooms during prediction period: 2022/2023") + scale_x_date(date_labels = "%b")

  } 
  
  if(train_test_box == TRUE){
    p <- p +
      geom_rect(aes(xmin = as.Date(training_dates[1]),
                xmax = as.Date(training_dates[2]) + 1,
                ymin = -Inf, ymax = Inf, fill = "training period", alpha = "training period")) +
      geom_rect(aes(xmin = as.Date(testing_dates[1]),
                xmax = as.Date(testing_dates[2]) + 1,
                ymin = -Inf, ymax = Inf, fill = "prediction period", alpha = "prediction period")) +
      scale_fill_manual(values = c("training period" = "lightyellow",
                                   "prediction period" = "lightblue"),
                        name = "")+
      scale_alpha_manual(values = c("training period" = 0.7,
                                    "prediction period" = 0.2),
                         name = "")
  } 
  
  p <- p +
    geom_point(data = observations, aes(x = datetime, y = Chla_ugL_mean, shape = "observations"), size = 1)+
    scale_shape_manual(values = c("observations" = 16), name = "")
  
  if(!is.null(focal_dates)){
    
    if(focal_dates_geom == "RECT"){
    
    for(f in 1:length(focal_dates)){
      
      panels <- letters
      num_panels <- panels[1:length(focal_dates)]
      focal_df <- data.frame(start_dates = as.Date(focal_dates),
                             names = paste0("Fig. 2",num_panels),
                             finish_dates = as.Date(focal_dates) + (forecast_horizon + 1))
    p <- p +
      geom_rect(data = focal_df,
                aes(xmin = start_dates,
                xmax = finish_dates, color = names),
                ymin = 0, ymax = max(observations$Chla_ugL_mean, na.rm = TRUE) + 1,
                fill = NA)+
      scale_color_viridis_d(option = "turbo", name = "Example prediction \ndates")

    }
      
    } else {
      
      panels <- letters
      num_panels <- panels[1:length(focal_dates)]
      focal_df <- data.frame(dates = as.Date(focal_dates),
                             names = paste0("Fig. 2",num_panels))

        p <- p +
          geom_vline(data = focal_df, aes(xintercept = dates, color = names))+
          scale_color_viridis_d(option = "turbo", name = "Example prediction \ndates")
        
      
    }
    
  }
  
  p <- p +
    guides(shape = guide_legend(order = 1),
           fill = guide_legend(order = 2),
           alpha = guide_legend(order = 2),
           colour = guide_legend(order = 3, ncol = 2)) +
    theme(legend.spacing.y = unit(0, 'cm'))
  
  if(plotly == TRUE){
    p <- ggplotly(p + ylab("Chlorophyll-a (ug/L)"))
  }

  return(p)
}
