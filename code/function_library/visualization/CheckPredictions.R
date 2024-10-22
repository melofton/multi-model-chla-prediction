# Visualization to check a set of predictions by a model
# Author: Mary Lofton
# Date: 10OCT24

# Purpose: generate visualizations of all predictions from a model to be checked over
dat <- read_csv("./model_output/validation_output.csv") %>%
  filter(model_id == "MARS")
model_output <- dat
pred_dates <- seq.Date(from = as.Date("2022-01-01"), to = as.Date("2023-11-26"), by = "day")
plot_title = "MARS predictions at FCR"
plot_file = "./figures/MARS_trials"
obs <- read_csv("./data/data_processed/chla_obs.csv")

CheckPredictions <- function(model_output, pred_dates, plot_title, plot_file, obs){

  years = unique(year(model_output$datetime))
  months = unique(month(model_output$datetime))
  
  for(i in 1:length(years)){
    
    for(j in 1:length(months)){
      
      plotdata <- dat %>%
        filter(year(reference_datetime) == years[i] & month(reference_datetime) == months[j])
      
      plot_pred <- plotdata %>%
        mutate(reference_datetime = as.Date(reference_datetime),
               ic = ifelse(datetime == reference_datetime, prediction, NA),
               prediction = ifelse(datetime == reference_datetime, NA, prediction)) %>%
        left_join(obs, by = "datetime") %>%
        rename(obs = Chla_ugL_mean) 
      
      p1 <- ggplot(data = plot_pred)+
        geom_line(aes(x = datetime, y = prediction, 
                      group = as.factor(reference_datetime),
                      color = as.factor(reference_datetime)))+
        geom_point(aes(x = datetime, y = obs), color = "black")+
        geom_point(aes(x = datetime, y = ic,
                       group = as.factor(reference_datetime),
                       color = as.factor(reference_datetime)))+
        labs(color = "Reference datetime")+
        ylab("Chlorophyll-a (ug/L)")+
        xlab("")+
        ggtitle(plot_title)+
        theme_bw()
      
      plotfile = paste0(plot_file,"/trials_",years[i],"-",months[j],".png")
      
      ggsave(filename = plotfile, plot = p1, device = "png",
             height = 6, width = 10, units = "in")
      
      
      
    }
  }

}



years = c(2022,2023)
months = c(1:12)

for(i in 1:length(years)){
  
  for(j in 1:length(months)){
    
    plotdata <- dat %>%
      filter(year(reference_datetime) == years[i] & month(reference_datetime) == months[j])
    
    plot_pred_GLMAED <- plotdata %>%
      mutate(reference_datetime = as.Date(reference_datetime),
             ic = ifelse(datetime == reference_datetime, prediction, NA),
             prediction = ifelse(datetime == reference_datetime, NA, prediction)) %>%
      left_join(dat_GLMAED, by = "datetime") %>%
      rename(obs = Chla_ugL_mean) %>%
      select(-Flag_Chla_ugL_mean)
    
    p1 <- ggplot(data = plot_pred_GLMAED)+
      geom_line(aes(x = datetime, y = prediction, 
                    group = as.factor(reference_datetime),
                    color = as.factor(reference_datetime)))+
      geom_point(aes(x = datetime, y = obs), color = "black")+
      geom_point(aes(x = datetime, y = ic,
                     group = as.factor(reference_datetime),
                     color = as.factor(reference_datetime)))+
      facet_wrap(facets = vars(trial))+
      labs(color = "Reference datetime")+
      ylab("Chlorophyll-a (ug/L)")+
      xlab("")+
      ggtitle("GLM-AED predictions at FCR")+
      theme_bw()
    plotfile = paste0("./figures/GLMAED_trials/trials_",years[i],"-",months[j],".png")
    ggsave(filename = plotfile, plot = p1, device = "png",
           height = 6, width = 10, units = "in")
    
    
    
  }
}


plotdata <- dat %>%
  filter(year(reference_datetime) == 2022 & month(reference_datetime) == 1)

plot_pred_GLMAED <- plotdata %>%
  mutate(reference_datetime = as.Date(reference_datetime),
         ic = ifelse(datetime == reference_datetime, prediction, NA),
         prediction = ifelse(datetime == reference_datetime, NA, prediction)) %>%
  left_join(dat_GLMAED, by = "datetime") %>%
  rename(obs = Chla_ugL_mean) %>%
  select(-Flag_Chla_ugL_mean)

p1 <- ggplot(data = plot_pred_GLMAED)+
  geom_line(aes(x = datetime, y = prediction, 
                group = as.factor(reference_datetime),
                color = as.factor(reference_datetime)))+
  geom_point(aes(x = datetime, y = obs), color = "black")+
  geom_point(aes(x = datetime, y = ic,
                 group = as.factor(reference_datetime),
                 color = as.factor(reference_datetime)))+
  facet_wrap(facets = vars(trial))+
  labs(color = "Reference datetime")+
  ylab("Chlorophyll-a (ug/L)")+
  xlab("")+
  ggtitle("GLM-AED predictions at FCR")+
  theme_bw()
p1
