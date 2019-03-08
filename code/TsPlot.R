TsPlot <- function(df, main = "",y.title="",limits,breaks) {
  library(ggplot2)
  library(zoo)
  library(scales)
  library(wesanderson)
  
  gg.xts <- ggplot(df, aes(x = year)) +
    
    # panel layout
    facet_grid(series~., scales = "free_y", space = "fixed", shrink = TRUE, drop = TRUE, labeller = label_value) + # label_value is default
    
    theme(strip.text= element_text(size = 12, family = "serif", face='bold')) +
    
    # line colours
    geom_line(data = subset(df, variable == "observed.pls"), aes(y = value, colour = "observed.pls", linetype="observed.pls"), show.legend = TRUE, size=1) +
    geom_line(data = subset(df, variable == "predicted.pls"), aes(y = value, colour = "predicted.pls", linetype="predicted.pls"), show.legend = FALSE, size=1) +
    geom_line(data = subset(df, variable == "pointwise.pls"), aes(y = value, colour = "observed.pls", linetype="observed.pls"), show.legend = FALSE, size=1) +
   # geom_line(data = subset(df, variable == "cumulative.pls"), aes(y = value ,colour = "observed.pls", linetype="observed.pls"), show.legend = FALSE, size=1) +
    
    geom_line(data = subset(df, variable == "observed.sls"), aes(y = value, colour = "observed.sls", linetype="observed.sls"), show.legend = TRUE, size=1) +

    # intervals
    geom_ribbon(data = subset(df, variable == "pointwise.pls"), aes(ymin = lower, ymax=upper, colour="predicted.pls"), alpha=.1, size=0.5, show.legend = FALSE) +
    #geom_ribbon(data = subset(df, variable == "cumulative.pls"), aes(ymin = lower, ymax=upper, colour="predicted.pls"), alpha=.1, size=0.5, show.legend = FALSE) +   
    
    # horizontal line to indicate zero values
    geom_hline(yintercept = 0, size = 0.5, colour = "black") +
    
    # main y-axis title
    ylab("") +
    
    # main x-axis title
    xlab("") +
    
    # main chart title
    ggtitle(main)
  
  # vertical line to indicate intervention
  
  intervention <- geom_vline(xintercept=c(as.numeric(as.POSIXct("1869-1-31 00:00:00",tz="UTC"))), linetype=2)
  
  # horizontal ticks
  
  ticks <- scale_x_datetime(breaks=breaks,
                            labels=date_format("%Y"), 
                              time_trans(tz="UTC"),
                              limits=limits)
  

  # annotation text
  
  ann_text <- data.frame(year = c(as.POSIXlt("1849-01-01 UTC"), as.POSIXlt("1889-01-01 UTC")), value=1,
                         series = factor("Time-series", levels = c("Time-series", "Per-period impact")),
                         lab = c("pre-period","post-period"))
  
  # legend 
  
  gg.xts <- gg.xts +
    intervention +
    ticks +
    theme( legend.title = element_blank()
           , plot.title = element_text(hjust = 0.5)
           , legend.position = c(0.18,0.85)
           , legend.justification = c(1,0)
           #  , legend.position = "top"
           , legend.background = element_rect()
           , axis.text=element_text(size=12)
           , axis.title.x=element_blank()
           , axis.ticks.x=element_blank()
           , axis.ticks.y=element_blank()
           , legend.text=element_text(size=12, family = "serif")
           , legend.box = "horizontal" # not working?)
    ) + geom_text(data = ann_text,aes(y = value, label =lab), family="serif", fontface="italic",  size=6) +
    scale_y_continuous(name=y.title) +
    scale_colour_manual(name="", values = c(  "observed.pls" = wes_palette("Darjeeling1")[5], 
                                              "observed.sls" = wes_palette("Darjeeling1")[1], 
                                              "predicted.pls" = wes_palette("Darjeeling1")[5]),
                        labels=c("Observed PLS", "Observed SLS", 
                                 "Predicted PLS")) +
    scale_linetype_manual(name="", values = c("observed.pls" = "solid", 
                                              "observed.sls" = "longdash", 
                                              "predicted.pls" = "dotted"),
                          labels=c("Observed PLS", "Observed SLS", 
                                   "Predicted PLS")) +
    theme(legend.key.width=unit(3,"line")) 
  return(gg.xts)
}