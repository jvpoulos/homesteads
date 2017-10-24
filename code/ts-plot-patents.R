TsPlotPatents <- function(df, analysis, main = "") {
  library(ggplot2)
  library(zoo)
  library(scales)
  library(wesanderson)
  
  gg.xts <- ggplot(df, aes(x = year)) +
    
    # panel layout
    facet_grid(series~., scales = "free_y", space = "fixed", shrink = TRUE, drop = TRUE, labeller = label_value) + # label_value is default
    
    theme(strip.text= element_text(size = 12, family = "serif", face='bold')) +
    
    # line colours
    geom_line(data = subset(df, variable == "Observed homesteads.pc"), aes(y = value, colour = "Observed homesteads.pc", linetype="Observed homesteads.pc"), show.legend = TRUE, size=0.5) +
    
    geom_line(data = subset(df, variable == "Predicted homesteads.pc"), aes(y = value, colour = "Predicted homesteads.pc", linetype="Predicted homesteads.pc"), show.legend = FALSE, size=0.5) +
    
    geom_line(data = subset(df, variable == "Pointwise homesteads.pc"), aes(y = value, colour = "Predicted homesteads.pc", linetype="Predicted homesteads.pc"), show.legend = FALSE, size=0.5) +
    
    # geom_line(data = subset(df, variable == "Cumulative homesteads.pc"), aes(y = value ,colour = "Predicted homesteads.pc", linetype="Predicted homesteads.pc"), show.legend = FALSE, size=1) +
    
    geom_line(data = subset(df, variable == "Observed sales.pc"), aes(y = value, colour = "Observed sales.pc", linetype="Observed sales.pc"), show.legend = TRUE, size=0.5) +
    
    geom_line(data = subset(df, variable == "Predicted sales.pc"), aes(y = value, colour = "Predicted sales.pc", linetype="Predicted sales.pc"), show.legend = FALSE, size=0.5) +
    
    geom_line(data = subset(df, variable == "Pointwise sales.pc"), aes(y = value, colour = "Predicted sales.pc", linetype="Predicted sales.pc"), show.legend = FALSE, size=0.5) +
    
    # geom_line(data = subset(df, variable == "Cumulative sales.pc"), aes(y = value ,colour = "Predicted sales.pc", linetype="Predicted sales.pc"), show.legend = FALSE, size=1) +
    
    # intervals
    
    geom_ribbon(data = subset(df, variable == "Pointwise homesteads.pc"), aes(ymin = pointwise.homesteads.pc.min, ymax=pointwise.homesteads.pc.max, colour="Predicted homesteads.pc"), alpha=.2, size=1, show.legend = FALSE) +
    
    geom_ribbon(data = subset(df, variable == "Pointwise sales.pc"), aes(ymin = pointwise.sales.pc.min, ymax=pointwise.sales.pc.max, colour="Predicted sales.pc"), alpha=.2, size=1, show.legend = FALSE) +
    
    # geom_ribbon(data = subset(df, variable == "Cumulative homesteads.pc"), aes(ymin = cumulative.homesteads.pc.min, ymax=cumulative.homesteads.pc.max, colour="Predicted homesteads.pc"), alpha=.2, size=1, show.legend = FALSE) +   
    # 
    # geom_ribbon(data = subset(df, variable == "Cumulative sales.pc"), aes(ymin = cumulative.sales.pc.min, ymax=cumulative.sales.pc.max, colour="Predicted sales.pc"), alpha=.2, size=1, show.legend = FALSE) +   
    
    # horizontal line to indicate zero values
    geom_hline(yintercept = 0, size = 0.5, colour = "black") +
    
    # main y-axis title
    ylab("") +
    
    # main x-axis title
    xlab("") +
    
    # main chart title
    ggtitle(main)
  
  # vertical line to indicate intervention
  
  if(analysis=='analysis-01'){
    intervention <- geom_vline(xintercept=c(as.numeric(as.POSIXct("1862-01-01 00:00:00",tz="UTC"))), linetype=2)
  } 
  
  if(analysis=='analysis-12'){
    intervention <- geom_vline(xintercept=c(as.numeric(as.POSIXct("1866-01-01 00:00:00",tz="UTC"))), linetype=2)
  } 
  
  if(analysis=='analysis-34'){
    intervention <- geom_vline(xintercept= c(as.numeric(as.POSIXct("1889-01-01 00:00:00",tz="UTC"))), linetype=2)
  }
  
  if(analysis=='analysis-41'){
    intervention <- geom_vline(xintercept= c(as.numeric(as.POSIXct("1889-01-01 00:00:00",tz="UTC"))), linetype=2)
  }
  
  # horizontal ticks
  
  if(analysis=='analysis-01'){
    ticks <- scale_x_datetime(date_breaks="12 years",labels=date_format("%Y"), 
                              time_trans(tz="UTC"),
                              limits = c(as.POSIXct("1804-12-31 19:03:58"), as.POSIXct("1976-12-31 19:00:00")))
  }  
  
  if(analysis=='analysis-12'){
    ticks <- scale_x_datetime(date_breaks="8 years",labels=date_format("%Y"), 
                              time_trans(tz="UTC"),
                              limits = c(as.POSIXct("1802-12-31 19:03:58"), as.POSIXct("1976-12-31 19:00:00")))
  }  
  if(analysis=='analysis-34'){
    ticks <- scale_x_datetime(date_breaks="6 years",labels=date_format("%Y"),
                              time_trans(tz="UTC"),
                              limits = c(as.POSIXct("1818-12-31 19:03:58"), as.POSIXct("1976-12-31 19:00:00")))
  } 
  
  if(analysis=='analysis-41'){
    ticks <- scale_x_datetime(date_breaks="6 years",labels=date_format("%Y"),
                              time_trans(tz="UTC"),
                              limits = c(as.POSIXct("1818-12-31 19:03:58"), as.POSIXct("1976-12-31 19:00:00")))
  } 
  
  # annotation text
  
  if(analysis=='analysis-01'){
    ann_text <- data.frame(year = c(as.POSIXlt("1847-01-01 EST"), as.POSIXlt("1877-01-01 EST")), value=-30, 
                           series = factor("Sales time-series", levels = c("Sales time-series","Pointwise impact","Cumulative impact")),
                           lab = c("pre-intervention \n (train/validation)",  "post-intervention \n (test)"))
  }
  
  if(analysis=='analysis-12'){
    ann_text <- data.frame(year = c(as.POSIXlt("1851-01-01 EST"), as.POSIXlt("1881-01-01 EST")), value=-5, 
                           series = factor("Sales time-series", levels = c("Sales time-series","Pointwise impact","Cumulative impact")),
                           lab = c("pre-intervention \n (train/validation)",  "post-intervention \n (test)"))
  }
  if(analysis=='analysis-34'){
    ann_text <- data.frame(year = c(as.POSIXlt("1874-01-01 EST"), as.POSIXlt("1904-01-01 EST")), value=-5, 
                           series = factor("Sales time-series", levels = c("Sales time-series","Pointwise impact","Cumulative impact")),
                           lab = c("pre-intervention \n (train/validation)",  "post-intervention \n (test)"))
  }
  if(analysis=='analysis-41'){
    ann_text <- data.frame(year = c(as.POSIXlt("1874-01-01 EST"),as.POSIXlt("1904-01-01 EST")), value=-5, 
                           series = factor("Sales time-series", levels = c("Sales time-series","Pointwise impact","Cumulative impact")),
                           lab = c("pre-intervention \n (train/validation)", "post-intervention \n (test)"))
  }
  
  
  # legend 
  
  gg.xts <- gg.xts +
    intervention +
    ticks +
    theme( legend.title = element_blank()
           #    , legend.position = c(0.90,0.8)
           #    , legend.justification = c(1,0)
           , legend.position = "top"
           , legend.background = element_rect()
           , axis.text=element_text(size=12)
           , axis.title.x=element_blank()
           , axis.ticks.x=element_blank()
           , axis.ticks.y=element_blank()
           , legend.text=element_text(size=12, family = "serif")
           , legend.box = "horizontal" # not working?)
    ) + geom_text(data = ann_text,aes(y = value, label =lab), family="serif", fontface="italic",  size=5) +
    scale_y_continuous(name="Log per-capita homesteads or sales") +
    scale_colour_manual(name="", values = c("Observed sales.pc" = wes_palette("Darjeeling")[1],"Predicted sales.pc" = wes_palette("Darjeeling")[1], "Observed homesteads.pc" = wes_palette("Darjeeling")[2], "Predicted homesteads.pc" = wes_palette("Darjeeling")[2]),
                        labels=c("Observed homesteads","Observed sales", "Predicted homesteads", "Predicted sales")) +
    scale_linetype_manual(name="", values = c("Predicted sales.pc" = "dashed","Predicted homesteads.pc" = "dashed", "Observed sales.pc" = "solid", "Observed homesteads.pc" = "solid"),
                          labels=c("Observed homesteads","Observed sales", "Predicted homesteads", "Predicted sales"))  + 
    theme(legend.key.width=unit(3,"line")) 
  return(gg.xts)
}