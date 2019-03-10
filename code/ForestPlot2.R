ForestPlot2 <- function(d, xlab, ylab, title="", leglab, ylim=NULL){
  p <- ggplot(d, aes(x=variable, y = y, ymin=y.lo, ymax=y.hi,colour=region, shape=region)) +
    geom_pointrange(size=1, alpha=0.6) +
    #  coord_flip() +
    geom_hline(data=data.frame(x=0, y = 1), aes(x=x, yintercept=0), colour="black", lty=2) +
    #     scale_y_continuous(labels = scales::percent) +
    #   labs(colour = leglab, shape=region) +
    ggtitle(title) +
    theme(plot.title = element_text(hjust = 0.5)) +
    coord_cartesian(ylim=ylim) +
    ylab(ylab) +
    xlab(xlab) #switch because of the coord_flip() above
  return(p)
}