Tslsot <- function(df, main = "",y.title="") {
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
    geom_line(data = subset(df, variable == "cumulative.pls"), aes(y = value ,colour = "observed.pls", linetype="observed.pls"), show.legend = FALSE, size=1) +
    
    geom_line(data = subset(df, variable == "observed.sls"), aes(y = value, colour = "observed.sls", linetype="observed.sls"), show.legend = TRUE, size=1) +
  #  geom_line(data = subset(df, variable == "predicted.sls"), aes(y = value, colour = "predicted.sls", linetype="predicted.sls"), show.legend = FALSE, size=1) +
  #  geom_line(data = subset(df, variable == "pointwise.sls"), aes(y = value, colour = "observed.sls", linetype="observed.sls"), show.legend = FALSE, size=1) +
  #  geom_line(data = subset(df, variable == "cumulative.sls"), aes(y = value ,colour = "observed.sls", linetype="observed.sls"), show.legend = FALSE, size=1) +
    
    # intervals
    geom_ribbon(data = subset(df, variable == "pointwise.pls"), aes(ymin = pointwise.lo, ymax=pointwise.hi, colour="predicted.pls"), alpha=.1, size=0.5, show.legend = FALSE) +
    geom_ribbon(data = subset(df, variable == "cumulative.pls"), aes(ymin = cumulative.lo, ymax=cumulative.hi, colour="predicted.pls"), alpha=.1, size=0.5, show.legend = FALSE) +   
    
   # geom_ribbon(data = subset(df, variable == "pointwise.sls"), aes(ymin = pointwise.lo, ymax=pointwise.hi, colour="predicted.sls"), alpha=.1, size=0.5, show.legend = FALSE) +
  #  geom_ribbon(data = subset(df, variable == "cumulative.sls"), aes(ymin = cumulative.lo, ymax=cumulative.hi, colour="predicted.sls"), alpha=.1, size=0.5, show.legend = FALSE) +   
    
    # horizontal line to indicate zero values
    geom_hline(yintercept = 0, size = 0.5, colour = "black") +
    
    # main y-axis title
    ylab("") +
    
    # main x-axis title
    xlab("") +
    
    # main chart title
    ggtitle(main)
  
  # vertical line to indicate intervention
  
  intervention <- geom_vline(xintercept=c(as.numeric(as.POSIXct("1869-12-31 00:00:00",tz="UTC")),
                                          as.numeric(as.POSIXct("1872-12-31 00:00:00",tz="UTC"))), linetype=2)
  
  # horizontal ticks
  
  ticks <- scale_x_datetime(date_breaks="19 years",labels=date_format("%Y"), 
                            time_trans(tz="UTC"),
                            limits = c(as.POSIXct("1783-12-30 19:00:00"), as.POSIXct("1982-12-30 19:00:00")))
  
  # annotation text
  
  # ann_text <- data.frame(year = c(as.POSIXlt("1980-01-01 EST"), as.POSIXlt("2006-12-31 EST")), value=8, 
  #                        series = factor("Observed time-series", levels = c("Observed time-series","pointwise.pls","cumulative.pls")),
  #                        lab = c("pre-period", "post-period"))
  
  # legend 
  
  gg.xts <- gg.xts +
    intervention +
    ticks +
    theme( legend.title = element_blank()
           , legend.position = c(0.2,0.9)
           , legend.justification = c(1,0)
           #  , legend.position = "top"
           , legend.background = element_rect()
           , axis.text=element_text(size=12)
           , axis.title.x=element_blank()
           , axis.ticks.x=element_blank()
           , axis.ticks.y=element_blank()
           , legend.text=element_text(size=12, family = "serif")
           , legend.box = "horizontal" # not working?)
    ) + #geom_text(data = ann_text,aes(y = value, label =lab), family="serif", fontface="italic",  size=5) +
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