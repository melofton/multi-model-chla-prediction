#Plot model fits
#Author: Mary Lofton
#Date last updated: 15APR24


PlotModelFits <- function(observations, predictions, model_ids){
  
    #limit to year of prediction
    observations <- observations %>%
      filter(!lubridate::year(datetime) %in% c(2022,2023)) %>%
      add_column(variable = "observed")
    
    #limit to models you want
    predictions <- predictions %>%
      filter(model_id %in% model_ids) %>%
      mutate(model_id = factor(model_id, levels = model_ids))
    
  p <- ggplot()+
    geom_point(data = observations, aes(x = datetime, y = Chla_ugL_mean,
                                        group = variable, fill = variable), size = 1) + 
    geom_line(data = predictions, aes(x = datetime, y = prediction, group = model_id, color = model_type, linetype = model_id))+
    xlab("")+
    ylab(expression(paste("Chlorophyll-a (",mu,g,~L^-1,")")))+
    theme_bw()+
    theme(axis.title.y = element_text(size = 16),
          axis.text.x = element_text(size = 14),
          plot.title = element_text(hjust = 0.05, vjust = -8, face = "bold",
                                    size = 16),
          legend.title = element_text(face = "bold"))+
    scale_linetype_discrete(name = "Model ID")+
    # if want to group models by type, can do that with colors in line below
    scale_color_manual(name = "Model type", values = c("null" = "#948E0A", "process-based" = "#B85233","data-driven" = "#71BFB9"))+ #"#71BFB9","#B85233","#E69F00","#0072B2"
    scale_fill_discrete(name = "")+
    guides(fill = guide_legend(order = 3), colour = guide_legend(order = 1), linetype = guide_legend(order = 2))
  
    

  return(p)
}
