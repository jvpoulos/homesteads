TsPlot <- function(df, main = "") {
  library(ggplot2)
  library(zoo)
  library(scales)
  library(wesanderson)
  
  gg.xts <- ggplot(df, aes(x = year)) +
    
    # panel layout
    facet_grid(series~., scales = "free_y", space = "fixed", shrink = TRUE, drop = TRUE, labeller = label_value) + # label_value is default
    
    theme(strip.text= element_text(size = 12, family = "serif", face='bold')) +
    
    # line colours
    geom_line(data = subset(df, variable == "Observed impact"), aes(y = value, colour = "Observed impact", linetype="Observed impact"), show.legend = TRUE, size=0.5) +
    
    geom_line(data = subset(df, variable == "Predicted impact"), aes(y = value, colour = "Predicted impact", linetype="Predicted impact"), show.legend = FALSE, size=0.5) +
    
    geom_line(data = subset(df, variable == "Pointwise impact"), aes(y = value, colour = "Predicted impact", linetype="Predicted impact"), show.legend = FALSE, size=0.5) +
    
    geom_line(data = subset(df, variable == "Cumulative impact"), aes(y = value ,colour = "Predicted impact", linetype="Predicted impact"), show.legend = FALSE, size=1) +
    
    # intervals
    geom_ribbon(data = subset(df, variable == "Predicted impact"), aes(ymin = pred.impact.min, ymax=pred.impact.max, colour="Predicted impact"), alpha=.2, size=1, show.legend = FALSE) +
    
    geom_ribbon(data = subset(df, variable == "Pointwise impact"), aes(ymin = pointwise.impact.min, ymax=pointwise.impact.max, colour="Predicted impact"), alpha=.2, size=1, show.legend = FALSE) +
    
    geom_ribbon(data = subset(df, variable == "Cumulative impact"), aes(ymin = cumulative.impact.min, ymax=cumulative.impact.max, colour="Predicted impact"), alpha=.2, size=1, show.legend = FALSE) +   
    
    # horizontal line to indicate zero values
    geom_hline(yintercept = 0, size = 0.5, colour = "black") +
    
    # main y-axis title
    ylab("") +
    
    # main x-axis title
    xlab("") +
    
    # main chart title
    ggtitle(main)
  
  # vertical line to indicate intervention
  
  intervention <- geom_vline(xintercept=c(as.numeric(as.POSIXct("2005-12-31 00:00:00",tz="UTC")),
                                          as.numeric(as.POSIXct("2006-12-31 00:00:00",tz="UTC"))), linetype=2)
  
  # horizontal ticks
  
  ticks <- scale_x_datetime(date_breaks="10 years",labels=date_format("%Y"), 
                            time_trans(tz="UTC"),
                            limits = c(as.POSIXct("1948-12-30 19:00:00"), as.POSIXct("2007-12-30 19:00:00")))
  
  # annotation text
  
  ann_text <- data.frame(year = c(as.POSIXlt("1980-01-01 EST"), as.POSIXlt("2006-12-31 EST")), value=80, 
                         series = factor("Observed time-series", levels = c("Observed time-series","Pointwise impact","Cumulative impact")),
                         lab = c("pre-period", "post-period"))
  
  # legend 
  
  gg.xts <- gg.xts +
    intervention +
    ticks +
    theme( legend.title = element_blank()
           , legend.position = c(0.35,0.93)
           , legend.justification = c(1,0)
           #  , legend.position = "top"
           , legend.background = element_rect()
           , axis.text=element_text(size=12)
           , axis.title.x=element_blank()
           , axis.ticks.x=element_blank()
           , axis.ticks.y=element_blank()
           , legend.text=element_text(size=12, family = "serif")
           , legend.box = "horizontal" # not working?)
    ) + geom_text(data = ann_text,aes(y = value, label =lab), family="serif", fontface="italic",  size=5) +
    scale_y_continuous(name="Log per-capita state government total expenditure or revenue (1982$)") +
    scale_colour_manual(name="", values = c("Observed impact" = wes_palette("Darjeeling")[5], "Predicted impact" = wes_palette("Darjeeling")[5]),
                        labels=c("Observed time-series", "Predicted time-series")) +
    scale_linetype_manual(name="", values = c("Predicted impact" = "dashed", "Observed impact" = "solid"),
                          labels=c("Observed time-series", "Predicted time-series"))  + 
    theme(legend.key.width=unit(3,"line")) 
  return(gg.xts)
}