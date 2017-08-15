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
    geom_line(data = subset(df, variable == "Observed ed.pc"), aes(y = value, colour = "Observed ed.pc", linetype="Observed ed.pc"), show.legend = TRUE, size=0.3) +
   
    geom_line(data = subset(df, variable == "Predicted ed.pc"), aes(y = value, colour = "Predicted ed.pc", linetype="Predicted ed.pc"), show.legend = TRUE, size=0.3) +
   
    geom_line(data = subset(df, variable == "Pointwise ed.pc"), aes(y = value, colour = "Predicted ed.pc", linetype="Predicted ed.pc"), show.legend = FALSE, size=1) +
   
    geom_line(data = subset(df, variable == "Cumulative ed.pc"), aes(y = value ,colour = "Predicted ed.pc", linetype="Predicted ed.pc"), show.legend = FALSE, size=1) +
    

  # intervals
   
    geom_ribbon(data = subset(df, variable == "Pointwise ed.pc"), aes(ymin = pointwise.ed.pc.min, ymax=pointwise.ed.pc.max, colour="Predicted ed.pc", colour="Predicted ed.pc"), alpha=.2, size=1, show.legend = FALSE) +
    
    geom_ribbon(data = subset(df, variable == "Cumulative ed.pc"), aes(ymin = cumulative.ed.pc.min, ymax=cumulative.ed.pc.max, colour="Predicted ed.pc"), alpha=.2, size=1, show.legend = FALSE) +   
    
    # horizontal line to indicate zero values
    geom_hline(yintercept = 0, size = 0.5, colour = "black") +
    
    # main y-axis title
    ylab("") +
    
    # main x-axis title
    xlab("") +
    
    # main chart title
    ggtitle(main)
  
    # vertical line to indicate intervention
    
    if(analysis=='analysis-12'){
      intervention <- geom_vline(xintercept=c(as.numeric(as.POSIXct("1866-06-01 06:00:00",tz="UTC")),
                                              as.numeric(as.POSIXct("1876-06-01 06:00:00",tz="UTC"))), linetype=2)
    } 
  
  if(analysis=='analysis-34'){
    intervention <- geom_vline(xintercept= as.numeric(as.POSIXct("1889-03-01 06:00:00",tz="UTC")), linetype=5)
  }
  
  # horizontal ticks
  if(analysis=='analysis-12'){
    ticks <- scale_x_datetime(date_breaks="10 years",labels=date_format("%Y"),
                              time_trans(tz="UTC")) 
  } 
  if(analysis=='analysis-34'){
    ticks <- scale_x_datetime(date_breaks="10 years",labels=date_format("%Y"),
                              time_trans(tz="UTC")) 
  } 
  
  # annotation text
  if(analysis=='analysis-12'){
    ann_text <- data.frame(year = c(as.POSIXlt("1850-01-01 EST"), as.POSIXlt("1871-06-01 EST"), as.POSIXlt("1890-01-01 EST")), value=2, 
                           series = factor("Time-series", levels = c("Time-series","Pointwise impact","Cumulative impact")),
                           lab = c("pre-intervention \n (training)", "intervention \n (test)", "post-intervention \n (test)"))
  }
  if(analysis=='analysis-34'){
    ann_text <- data.frame(year = c(as.POSIXlt("1870-01-01 EST"), as.POSIXlt("1910-01-01 EST")), value=10, 
                           series = factor("Time-series", levels = c("Time-series","Pointwise impact","Cumulative impact")),
                           lab = c("pre-intervention \n (training)", "post-intervention \n (test)"))
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
    scale_y_continuous(name="Per-capita education spending") +
    scale_colour_manual(name="", values = c("Observed ed.pc" = wes_palette("Darjeeling2")[2],"Predicted ed.pc" = wes_palette("Darjeeling2")[2]),
                        labels=c("Observed", "Predicted")) +
    scale_linetype_manual(name="", values = c("Predicted ed.pc" = "dashed","Observed ed.pc" = "solid"),
                          labels=c("Observed", "Predicted"))  + 
    theme(legend.key.width=unit(3,"line")) 
  return(gg.xts)
}