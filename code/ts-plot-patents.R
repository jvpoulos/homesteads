TsPlotPatents <- function(df, main = "") {
  library(ggplot2)
  library(scales)
  library(wesanderson)
  
  gg.xts <- ggplot(df, aes(x = date)) +
  
  # panel layout
  facet_grid(series~., scales = "free_y", space = "fixed", shrink = TRUE, drop = TRUE, labeller = label_value) + # label_value is default
    
  theme(strip.text= element_text(size = 12, family = "serif", face='bold')) +
  
  # line colours
    geom_line(data = subset(df, variable == "Observed sales"), aes(y = value, colour = "Observed sales", linetype="Observed sales"), show.legend = TRUE, size=0.3) +
   
    geom_line(data = subset(df, variable == "Predicted sales"), aes(y = value, colour = "Predicted sales", linetype="Predicted sales"), show.legend = TRUE, size=0.3) +
   
    geom_line(data = subset(df, variable == "Pointwise sales"), aes(y = value, colour = "Predicted sales", linetype="Predicted sales"), show.legend = FALSE, size=1) +
   
    geom_line(data = subset(df, variable == "Cumulative sales"), aes(y = value ,colour = "Predicted sales", linetype="Predicted sales"), show.legend = FALSE, size=1) +
    
    geom_line(data = subset(df, variable == "Observed homesteads"), aes(y = value, colour = "Observed homesteads", linetype="Observed homesteads"), show.legend = FALSE, size=0.3) +
    
    geom_line(data = subset(df, variable == "Predicted homesteads"), aes(y = value, colour = "Predicted homesteads", linetype="Predicted homesteads"), show.legend = FALSE, size=0.3) +
    
    geom_line(data = subset(df, variable == "Pointwise homesteads"), aes(y = value, colour = "Predicted homesteads", linetype="Predicted homesteads"), show.legend = FALSE, size=1) +
    
    geom_line(data = subset(df, variable == "Cumulative homesteads"), aes(y = value ,colour = "Predicted homesteads", linetype="Predicted homesteads"), show.legend = FALSE, size=1) +
    
  # intervals
   
    geom_ribbon(data = subset(df, variable == "Pointwise sales"), aes(ymin = pointwise.sales.min, ymax=pointwise.sales.max, colour="Predicted sales", colour="Predicted sales"), alpha=.2, size=1, show.legend = FALSE) +
    
    geom_ribbon(data = subset(df, variable == "Pointwise homesteads"), aes(ymin = pointwise.homesteads.min, ymax=pointwise.homesteads.max, colour="Predicted homesteads"), alpha=.2, size=1, show.legend = FALSE) +
    
    geom_ribbon(data = subset(df, variable == "Cumulative sales"), aes(ymin = cumulative.sales.min, ymax=cumulative.sales.max, colour="Predicted sales"), alpha=.2, size=1, show.legend = FALSE) +   
    
    geom_ribbon(data = subset(df, variable == "Cumulative homesteads"), aes(ymin = cumulative.homesteads.min, ymax=cumulative.homesteads.max, colour="Predicted homesteads"), alpha=.2, size=1, show.legend = FALSE) +   
    
    # vertical line to indicate intervention
  
  geom_vline(xintercept=c(as.numeric(as.POSIXct("1876-06-01 06:00:00",tz="UTC"))), linetype=4) + 
    
  # horizontal line to indicate zero values
  geom_hline(yintercept = 0, size = 0.5, colour = "black") +
  
 # horizontal ticks
  # scale_x_datetime(limits=c(as.POSIXct("1892-08-01 06:00:00",tz="UTC"), as.POSIXct("1940-08-01 18:00:00",tz="UTC")),
  #                   date_breaks="10 years",labels=date_format("%Y"),
  #                   time_trans(tz="UTC"))+

  # main y-axis title
  ylab("") +
  
  # main x-axis title
  xlab("") +
  
  # main chart title
  ggtitle(main)

# annotation text
  ann_text <- data.frame(date = c(as.POSIXlt("1875-01-01 EST"), as.POSIXlt("1885-01-01 EST")), value=400, 
                         series = factor("Time-series", levels = c("Time-series","Pointwise impact","Cumulative impact")),
                         lab = c("pre-intervention \n (training)", "post-intervention \n (test)"))

# legend 

  gg.xts <- gg.xts +
  
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
    scale_colour_manual(name="", values = c("Observed homesteads" = wes_palette("Darjeeling")[1],"Predicted homesteads" = wes_palette("Darjeeling")[1], "Observed sales" = wes_palette("Darjeeling")[2], "Predicted sales" = wes_palette("Darjeeling")[2]),
                        labels=c("Observed homesteads", "Observed sales", "Predicted homesteads", "Predicted sales")) +
    scale_linetype_manual(name="", values = c("Predicted homesteads" = "dashed","Predicted sales" = "dashed", "Observed homesteads" = "solid", "Observed sales" = "solid"),
                          labels=c("Observed homesteads", "Observed sales", "Predicted homesteads", "Predicted sales"))  + 
  theme(legend.key.width=unit(3,"line")) 
return(gg.xts)
}