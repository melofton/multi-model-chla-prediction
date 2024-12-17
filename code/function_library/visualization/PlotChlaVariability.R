# Plot chl-a variability
# Author: Mary Lofton
# Date: 17DEC24

# Purpose: plot histogram of day-to-day change in chl-a at FCR to assess
# variability

obs <- read_csv("./data/data_processed/chla_obs.csv")

PlotChlaVariability <- function(obs){
  
  obs <- obs %>%
    mutate(delta = c(NA,abs(diff(Chla_ugL_mean, na.rm = TRUE))))
  
  dens <- density(obs$delta, na.rm = TRUE)
  q90 <- quantile(obs$delta, 0.90, na.rm = TRUE)
  dd <- with(dens,data.frame(x,y)) %>%
    mutate(var_bin = ifelse(x > q90, "high","low"))
  
  plot_cols <- viridis(6, option = "turbo")
  
  p <- ggplot(data = dd, aes(x = x, y = y))+
    geom_line()+
    geom_ribbon(data=subset(dd,x<q90),aes(ymax=y,fill=var_bin),ymin=0,
                colour=NA)+
    geom_ribbon(data=subset(dd,x>q90),aes(ymax=y,fill=var_bin),ymin=0,
                colour=NA)+
    geom_vline(xintercept = q90, linetype = "dashed")+
    xlab(expression(paste("Absolute value of daily change in chl-a (",mu,g,~L^-1,")")))+
    ylab("Density")+
    scale_fill_manual(values = c("low" = plot_cols[5],
                                 "high" = plot_cols[6]),
                      name = "Chl-a variability",
                      labels = c(expression(paste("high (> 3.16 ",mu,g,~L^-1,")")),
                                 expression(paste("low (< 3.16 ",mu,g,~L^-1,")"))))+
    theme_bw()
  
  return(list(p = p, q90 = q90))
  
}