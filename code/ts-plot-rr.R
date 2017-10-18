TsPlotRR <- function(df, analysis, main = "") {
  library(ggplot2)
  library(zoo)
  library(scales)
  library(wesanderson)
  
  gg.xts <- ggplot(df, aes(x = year)) +
    
    # panel layout
    facet_grid(series~., scales = "free_y", space = "fixed", shrink = TRUE, drop = TRUE, labeller = label_value) + # label_value is default
    
    theme(strip.text= element_text(size = 12, family = "serif", face='bold')) +
    
    # line colours
    geom_line(data = subset(df, variable == "Observed access"), aes(y = value, colour = "Observed access", linetype="Observed access"), show.legend = TRUE, size=0.5) +
    
    geom_line(data = subset(df, variable == "Predicted access"), aes(y = value, colour = "Predicted access", linetype="Predicted access"), show.legend = FALSE, size=0.5) +
    
    geom_line(data = subset(df, variable == "Pointwise access"), aes(y = value, colour = "Predicted access", linetype="Predicted access"), show.legend = FALSE, size=0.5) +
    
    # geom_line(data = subset(df, variable == "Cumulative access"), aes(y = value ,colour = "Predicted access", linetype="Predicted access"), show.legend = FALSE, size=1) +
    
    # intervals
    
    geom_ribbon(data = subset(df, variable == "Pointwise access"), aes(ymin = pointwise.access.min, ymax=pointwise.access.max, colour="Predicted access"), alpha=.2, size=1, show.legend = FALSE) +
    
    # geom_ribbon(data = subset(df, variable == "Cumulative access"), aes(ymin = cumulative.access.min, ymax=cumulative.access.max, colour="Predicted access"), alpha=.2, size=1, show.legend = FALSE) +   

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
    ticks <- scale_x_datetime(date_breaks="6 years",labels=date_format("%Y"), 
                              time_trans(tz="UTC"),
                              limits = c(as.POSIXct("1830-12-31 19:03:58"), as.POSIXct("1910-12-31 19:00:00")))
  }  
  
  if(analysis=='analysis-12'){
    ticks <- scale_x_datetime(date_breaks="8 years",labels=date_format("%Y"), 
                              time_trans(tz="UTC"),
                              limits = c(as.POSIXct("1830-12-31 19:03:58"), as.POSIXct("1910-12-31 19:00:00")))
  }  
  if(analysis=='analysis-34'){
    ticks <- scale_x_datetime(date_breaks="8 years",labels=date_format("%Y"),
                              time_trans(tz="UTC"),
                              limits = c(as.POSIXct("1829-12-31 19:03:58"), as.POSIXct("1910-12-31 19:00:00")))
  } 
  
  if(analysis=='analysis-41'){
    ticks <- scale_x_datetime(date_breaks="8 years",labels=date_format("%Y"),
                              time_trans(tz="UTC"),
                              limits = c(as.POSIXct("1829-12-31 19:03:58"), as.POSIXct("1910-12-31 19:00:00")))
  } 
  
  # annotation text
  
  if(analysis=='analysis-01'){
    ann_text <- data.frame(year = c(as.POSIXlt("1850-01-01 EST"), as.POSIXlt("1871-01-01 EST"), as.POSIXlt("1895-01-01 EST")), value=0.5, 
                           series = factor("Time-series", levels = c("Time-series","Pointwise impact","Cumulative impact")),
                           lab = c("pre-intervention \n (train/validation)", "", "post-intervention \n (test)"))
  }
  
  if(analysis=='analysis-12'){
    ann_text <- data.frame(year = c(as.POSIXlt("1850-01-01 EST"), as.POSIXlt("1871-01-01 EST"), as.POSIXlt("1895-01-01 EST")), value=0.5, 
                           series = factor("Time-series", levels = c("Time-series","Pointwise impact","Cumulative impact")),
                           lab = c("pre-intervention \n (train/validation)", "", "post-intervention \n (test)"))
  }
  if(analysis=='analysis-34'){
    ann_text <- data.frame(year = c(as.POSIXlt("1850-01-01 EST"), as.POSIXlt("1871-01-01 EST"), as.POSIXlt("1900-01-01 EST")), value=0.3, 
                           series = factor("Time-series", levels = c("Time-series","Pointwise impact","Cumulative impact")),
                           lab = c("pre-intervention \n (train/validation)", "", "post-intervention \n (test)"))
  }
  if(analysis=='analysis-41'){
    ann_text <- data.frame(year = c(as.POSIXlt("1850-01-01 EST"), as.POSIXlt("1871-01-01 EST"), as.POSIXlt("1900-01-01 EST")), value=0.5, 
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
    scale_y_continuous(name="Log per-capita revenues or expenditures") +
    scale_colour_manual(name="", values = c("Observed access" = wes_palette("Darjeeling")[4], "Predicted access" = wes_palette("Darjeeling")[4]),
                        labels=c("Observed access", "Predicted access")) +
    scale_linetype_manual(name="", values = c("Predicted access" = "dashed", "Observed access" = "solid"),
                          labels=c("Observed access", "Predicted access"))  + 
    theme(legend.key.width=unit(3,"line")) 
  return(gg.xts)
}