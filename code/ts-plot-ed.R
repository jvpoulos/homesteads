TsPlotEd <- function(df, analysis, main = "") {
  library(ggplot2)
  library(zoo)
  library(scales)
  library(wesanderson)
  
  gg.xts <- ggplot(df, aes(x = year)) +
    
    # panel layout
    facet_grid(series~., scales = "free_y", space = "fixed", shrink = TRUE, drop = TRUE, labeller = label_value) + # label_value is default
    
    theme(strip.text= element_text(size = 12, family = "serif", face='bold')) +
    
    # line colours

    geom_line(data = subset(df, variable == "Observed educ.pc"), aes(y = value, colour = "Observed educ.pc", linetype="Observed educ.pc"), show.legend = TRUE, size=0.5) +
    
    geom_line(data = subset(df, variable == "Predicted educ.pc"), aes(y = value, colour = "Predicted educ.pc", linetype="Predicted educ.pc"), show.legend = FALSE, size=0.5) +
    
    geom_line(data = subset(df, variable == "Pointwise educ.pc"), aes(y = value, colour = "Predicted educ.pc", linetype="Predicted educ.pc"), show.legend = FALSE, size=0.5) +
    
    # geom_line(data = subset(df, variable == "Cumulative educ.pc"), aes(y = value ,colour = "Predicted educ.pc", linetype="Predicted educ.pc"), show.legend = FALSE, size=1) +
    
    # intervals
    geom_ribbon(data = subset(df, variable == "Pointwise educ.pc"), aes(ymin = pointwise.educ.pc.min, ymax=pointwise.educ.pc.max, colour="Predicted educ.pc"), alpha=.2, size=1, show.legend = FALSE) +
        # 
    # geom_ribbon(data = subset(df, variable == "Cumulative educ.pc"), aes(ymin = cumulative.educ.pc.min, ymax=cumulative.educ.pc.max, colour="Predicted educ.pc"), alpha=.2, size=1, show.legend = FALSE) +   
    
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
                              limits = c(as.POSIXct("1824-12-31 19:03:58"), as.POSIXct("1941-12-31 19:00:00")))
  }  
  
  if(analysis=='analysis-12'){
    ticks <- scale_x_datetime(date_breaks="10 years",labels=date_format("%Y"), 
                              time_trans(tz="UTC"),
                              limits = c(as.POSIXct("1824-12-31 19:03:58"), as.POSIXct("1941-12-31 19:00:00")))
  }  
  if(analysis=='analysis-34'){
    ticks <- scale_x_datetime(date_breaks="10 years",labels=date_format("%Y"),
                              time_trans(tz="UTC"),
                              limits = c(as.POSIXct("1824-12-31 19:03:58"), as.POSIXct("1941-12-31 19:00:00")))
  } 
  if(analysis=='analysis-41'){
    ticks <- scale_x_datetime(date_breaks="10 years",labels=date_format("%Y"),
                              time_trans(tz="UTC"),
                              limits = c(as.POSIXct("1824-12-31 19:03:58"), as.POSIXct("1941-12-31 19:00:00")))
  } 
  
  # annotation text
  
  if(analysis=='analysis-01'){
    ann_text <- data.frame(year = c(as.POSIXlt("1850-01-01 EST"), as.POSIXlt("1871-01-01 EST"), as.POSIXlt("1895-01-01 EST")), value=1.5, 
                           series = factor("Time-series", levels = c("Time-series","Pointwise impact","Cumulative impact")),
                           lab = c("pre-intervention \n (train/validation)", "", "post-intervention \n (test)"))
  }
  
  if(analysis=='analysis-12'){
    ann_text <- data.frame(year = c(as.POSIXlt("1850-01-01 EST"), as.POSIXlt("1871-01-01 EST"), as.POSIXlt("1895-01-01 EST")), value=1, 
                           series = factor("Time-series", levels = c("Time-series","Pointwise impact","Cumulative impact")),
                           lab = c("pre-intervention \n (train/validation)", "", "post-intervention \n (test)"))
  }
  if(analysis=='analysis-34'){
    ann_text <- data.frame(year = c(as.POSIXlt("1870-01-01 EST"), as.POSIXlt("1894-01-01 EST"),as.POSIXlt("1915-01-01 EST")), value=-5, 
                           series = factor("Time-series", levels = c("Time-series","Pointwise impact","Cumulative impact")),
                           lab = c("pre-intervention \n (train/validation)", "", "post-intervention \n (test)"))
  }
  if(analysis=='analysis-41'){
    ann_text <- data.frame(year = c(as.POSIXlt("1870-01-01 EST"), as.POSIXlt("1894-01-01 EST"),as.POSIXlt("1915-01-01 EST")), value=-6, 
                           series = factor("Time-series", levels = c("Time-series","Pointwise impact","Cumulative impact")),
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
    scale_y_continuous(name="Log per-capita education spending") +
    scale_colour_manual(name="", values = c("Observed educ.pc" = wes_palette("Darjeeling2")[2],"Predicted educ.pc" = wes_palette("Darjeeling2")[2]),
                        labels=c("Observed education spending", "Predicted education spending")) +
    scale_linetype_manual(name="", values = c("Predicted educ.pc" = "dashed","Observed educ.pc" = "solid"),
                          labels=c("Observed education spending", "Predicted education spending")) +
    theme(legend.key.width=unit(2,"line")) 
  return(gg.xts)
}