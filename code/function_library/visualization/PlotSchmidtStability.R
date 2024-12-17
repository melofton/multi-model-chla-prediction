# Plot Schmidty stability
# Author: Mary Lofton
# Date: 17DEC24

# Purpose: visualize Schmidt stability

# load packages
library(viridis)

# define function
PlotSchmidtStability <- function(ss_data, testing_dates){
  
ss_data <- ss_data %>%
  mutate(strat_bin = factor(strat_bin, levels = c("mixed","onset","stratified","decline")))

plot_cols <- viridis(6, option = "turbo")

ggplot()+
  geom_point(data = ss_data, aes(x = datetime, y = schmidt.stability, color = strat_bin))+
  theme_bw()+
    geom_rect(aes(xmin = as.Date(testing_dates[1]),
                  xmax = as.Date(testing_dates[2]) + 1,
                  ymin = -Inf, ymax = Inf, fill = "prediction period", alpha = "prediction period")) +
    scale_fill_manual(values = c("training period" = "lightyellow",
                                 "prediction period" = "lightblue"),
                      name = "")+
    scale_alpha_manual(values = c("training period" = 0.7,
                                  "prediction period" = 0.2),
                       name = "")+
    scale_color_manual(values = plot_cols[1:4], name = "Stratification period")+
    ylab(expression(paste("Schmidt stability (",J,~m^-2,")")))+
    xlab("")

}