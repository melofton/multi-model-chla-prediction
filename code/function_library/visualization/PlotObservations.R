#Plot observations
#Author: Mary Lofton
#Date last updated: 15APR24


PlotObservations <- function(observations, pred_only, focal_dates, forecast_horizon,
                             plotly){
  
  if(pred_only == TRUE){
    observations <- observations %>%
      filter(lubridate::year(datetime) %in% c(2022,2023))
  }
  
  p <- ggplot(data = observations, aes(x = datetime, y = Chla_ugL_mean))+
    geom_point(size = 1) + 
    xlab("")+
    ylab(expression(paste("Chlorophyll-a (",mu,g,~L^-1,")")))+
    theme_bw()+
    theme(axis.title.y = element_text(size = 16),
          axis.text.x = element_text(size = 14),
          plot.title = element_text(hjust = 0.05, vjust = -8, face = "bold",
                                    size = 16))+
    ylim(NA,max(observations$Chla_ugL_mean)+3)
  
  if(pred_only == TRUE){
    p <- p + ggtitle("2022/2023") + scale_x_date(date_labels = "%b")

  } 
  
  if(!is.null(focal_dates)){
    
    for(f in 1:length(focal_dates))
    p <- p +
      geom_rect(xmin = as.Date(focal_dates[f]) - (forecast_horizon + 1),
                xmax = as.Date(focal_dates[f]) + 1,
                ymin = 5, ymax = max(observations$Chla_ugL_mean) + 1,
                fill = NA, color = "#B85233",
                linetype = 8)
  }
  
  if(plotly == TRUE){
    p <- ggplotly(p + ylab("Chlorophyll-a (ug/L)"))
  }

  return(p)
}
