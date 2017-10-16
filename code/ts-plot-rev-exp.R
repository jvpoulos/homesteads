TsPlotRevExp <- function(df, analysis, main = "") {
  library(ggplot2)
  library(zoo)
  library(scales)
  library(wesanderson)
  
  gg.xts <- ggplot(df, aes(x = year)) +
  
  # panel layout
  facet_grid(series~., scales = "free_y", space = "fixed", shrink = TRUE, drop = TRUE, labeller = label_value) + # label_value is default
    
  theme(strip.text= element_text(size = 12, family = "serif", face='bold')) +
  
  # line colours
    geom_line(data = subset(df, variable == "Observed rev.pc"), aes(y = value, colour = "Observed rev.pc", linetype="Observed rev.pc"), show.legend = TRUE, size=0.5) +
   
    geom_line(data = subset(df, variable == "Predicted rev.pc"), aes(y = value, colour = "Predicted rev.pc", linetype="Predicted rev.pc"), show.legend = FALSE, size=0.5) +
   
    geom_line(data = subset(df, variable == "Pointwise rev.pc"), aes(y = value, colour = "Predicted rev.pc", linetype="Predicted rev.pc"), show.legend = FALSE, size=0.5) +
   
    # geom_line(data = subset(df, variable == "Cumulative rev.pc"), aes(y = value ,colour = "Predicted rev.pc", linetype="Predicted rev.pc"), show.legend = FALSE, size=1) +
    
    geom_line(data = subset(df, variable == "Observed exp.pc"), aes(y = value, colour = "Observed exp.pc", linetype="Observed exp.pc"), show.legend = TRUE, size=0.5) +
    
    geom_line(data = subset(df, variable == "Predicted exp.pc"), aes(y = value, colour = "Predicted exp.pc", linetype="Predicted exp.pc"), show.legend = FALSE, size=0.5) +
    
    geom_line(data = subset(df, variable == "Pointwise exp.pc"), aes(y = value, colour = "Predicted exp.pc", linetype="Predicted exp.pc"), show.legend = FALSE, size=0.5) +
    
    # geom_line(data = subset(df, variable == "Cumulative exp.pc"), aes(y = value ,colour = "Predicted exp.pc", linetype="Predicted exp.pc"), show.legend = FALSE, size=1) +
    
  # intervals
   
    geom_ribbon(data = subset(df, variable == "Pointwise rev.pc"), aes(ymin = pointwise.rev.pc.min, ymax=pointwise.rev.pc.max, colour="Predicted rev.pc"), alpha=.2, size=1, show.legend = FALSE) +
    
    geom_ribbon(data = subset(df, variable == "Pointwise exp.pc"), aes(ymin = pointwise.exp.pc.min, ymax=pointwise.exp.pc.max, colour="Predicted exp.pc"), alpha=.2, size=1, show.legend = FALSE) +
    
    # geom_ribbon(data = subset(df, variable == "Cumulative rev.pc"), aes(ymin = cumulative.rev.pc.min, ymax=cumulative.rev.pc.max, colour="Predicted rev.pc"), alpha=.2, size=1, show.legend = FALSE) +   
    # 
    # geom_ribbon(data = subset(df, variable == "Cumulative exp.pc"), aes(ymin = cumulative.exp.pc.min, ymax=cumulative.exp.pc.max, colour="Predicted exp.pc"), alpha=.2, size=1, show.legend = FALSE) +   
  
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
    intervention <- geom_vline(xintercept=c(as.numeric(as.POSIXct("1862-05-01 06:00:00",tz="UTC"))), linetype=2)
  } 
  
  if(analysis=='analysis-12'){
    intervention <- geom_vline(xintercept=c(as.numeric(as.POSIXct("1866-06-01 06:00:00",tz="UTC"))), linetype=2)
  } 
  
  if(analysis=='analysis-34'){
    intervention <- geom_vline(xintercept= c(as.numeric(as.POSIXct("1889-03-01 06:00:00",tz="UTC"))), linetype=2)
  }
  
  if(analysis=='analysis-41'){
    intervention <- geom_vline(xintercept= c(as.numeric(as.POSIXct("1889-03-01 06:00:00",tz="UTC"))), linetype=2)
  }
  
  # horizontal ticks
  
  if(analysis=='analysis-01'){
    ticks <- scale_x_datetime(date_breaks="10 years",labels=date_format("%Y"), 
                              time_trans(tz="UTC"),
                              limits = c(as.POSIXct("1823-12-31 19:03:58"), as.POSIXct("1981-12-31 19:00:00")))
  }  
  
  if(analysis=='analysis-12'){
    ticks <- scale_x_datetime(date_breaks="10 years",labels=date_format("%Y"), 
                     time_trans(tz="UTC"),
                     limits = c(as.POSIXct("1823-12-31 19:03:58"), as.POSIXct("1981-12-31 19:00:00")))
  }  
  if(analysis=='analysis-34'){
    ticks <- scale_x_datetime(date_breaks="10 years",labels=date_format("%Y"),
                              time_trans(tz="UTC"),
                              limits = c(as.POSIXct("1826-12-31 19:03:58"), as.POSIXct("1981-12-31 19:00:00")))
  } 
  
  if(analysis=='analysis-41'){
    ticks <- scale_x_datetime(date_breaks="10 years",labels=date_format("%Y"),
                              time_trans(tz="UTC"),
                              limits = c(as.POSIXct("1826-12-31 19:03:58"), as.POSIXct("1981-12-31 19:00:00")))
  } 
    
# annotation text
  
  if(analysis=='analysis-01'){
    ann_text <- data.frame(year = c(as.POSIXlt("1850-01-01 EST"), as.POSIXlt("1871-01-01 EST"), as.POSIXlt("1895-01-01 EST")), value=3, 
                           series = factor("Revenues time-series", levels = c("Revenues time-series","Pointwise impact","Cumulative impact")),
                           lab = c("pre-intervention \n (train/validation)", "", "post-intervention \n (test)"))
  }
  
  if(analysis=='analysis-12'){
    ann_text <- data.frame(year = c(as.POSIXlt("1850-01-01 EST"), as.POSIXlt("1871-01-01 EST"), as.POSIXlt("1895-01-01 EST")), value=2.5, 
                           series = factor("Revenues time-series", levels = c("Revenues time-series","Pointwise impact","Cumulative impact")),
                           lab = c("pre-intervention \n (train/validation)", "", "post-intervention \n (test)"))
  }
  if(analysis=='analysis-34'){
    ann_text <- data.frame(year = c(as.POSIXlt("1870-01-01 EST"), as.POSIXlt("1894-01-01 EST"),as.POSIXlt("1915-01-01 EST")), value=2.5, 
                           series = factor("Revenues time-series", levels = c("Revenues time-series","Pointwise impact","Cumulative impact")),
                           lab = c("pre-intervention \n (train/validation)", "", "post-intervention \n (test)"))
  }
  if(analysis=='analysis-41'){
    ann_text <- data.frame(year = c(as.POSIXlt("1870-01-01 EST"), as.POSIXlt("1894-01-01 EST"),as.POSIXlt("1915-01-01 EST")), value=3, 
                           series = factor("Revenues time-series", levels = c("Revenues time-series","Pointwise impact","Cumulative impact")),
                           lab = c("pre-intervention \n (train/validation)", "", "post-intervention \n (test)"))
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
    scale_y_continuous(name="Log per-capita revenues or expenditures") +
    scale_colour_manual(name="", values = c("Observed exp.pc" = wes_palette("Darjeeling")[3],"Predicted exp.pc" = wes_palette("Darjeeling")[3], "Observed rev.pc" = wes_palette("Darjeeling")[5], "Predicted rev.pc" = wes_palette("Darjeeling")[5]),
                        labels=c("Observed expenditures", "Observed revenues", "Predicted expenditures", "Predicted revenues")) +
    scale_linetype_manual(name="", values = c("Predicted exp.pc" = "dashed","Predicted rev.pc" = "dashed", "Observed exp.pc" = "solid", "Observed rev.pc" = "solid"),
                          labels=c("Observed expenditures", "Observed revenues", "Predicted expenditures", "Predicted revenues"))  + 
    theme(legend.key.width=unit(3,"line")) 
return(gg.xts)
}