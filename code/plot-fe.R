## Plot all estimates

# Data for plot

plot.data.fe <- data.frame(region=rep(c("West","South"),each=5),
                            variable= rep(c("Tax1","Tax2","Education","Revenue", "Expenditures"),times=2),
                            y = c(tax1.west.fe.delta,
                                  tax2.west.fe.delta,
                                  educ.west.state.delta,
                                  rev.west.state.delta,
                                  exp.west.state.delta,
                                  tax1.south.fe.delta,
                                  tax2.south.fe.delta,
                                  educ.south.state.delta,
                                  rev.south.state.delta,
                                  exp.south.state.delta),
                            y.lo = c(tax1.west.fe.CI[1],
                                     tax2.west.fe.CI[1],
                                     educ.west.state.CI[1],
                                     rev.west.state.CI[1],
                                     exp.west.state.CI[1],
                                     tax1.south.fe.CI[1],
                                     tax2.south.fe.CI[1],
                                     educ.south.state.CI[1],
                                     rev.south.state.CI[1],
                                     exp.south.state.CI[1]),
                            y.hi = c(tax1.west.fe.CI[2],
                                     tax2.west.fe.CI[2],
                                     educ.west.state.CI[2],
                                     rev.west.state.CI[2],
                                     exp.west.state.CI[2],
                                     tax1.south.fe.CI[2],
                                     tax2.south.fe.CI[2],
                                     educ.south.state.CI[2],
                                     rev.south.state.CI[2],
                                     exp.south.state.CI[2]))


# Plot forest plots

ForestPlot2 <- function(d, xlab, ylab, title="", leglab, ylim=NULL){
  p <- ggplot(d, aes(x=variable, y = y, ymin=y.lo, ymax=y.hi,colour=region)) +
    geom_pointrange(size=1, alpha=0.6) +
    #  coord_flip() +
    geom_hline(data=data.frame(x=0, y = 1), aes(x=x, yintercept=0), colour="black", lty=2) +
    scale_y_continuous(labels = scales::percent) +
    labs(colour = leglab) +
    ggtitle(title) +
    theme(plot.title = element_text(hjust = 0.5)) +
    coord_cartesian(ylim=ylim) +
    ylab(ylab) +
    xlab(xlab) #switch because of the coord_flip() above
  return(p)
}

plot.data.fe$variable <- as.factor(plot.data.fe$variable)
summary.plot <- ForestPlot2(plot.data.fe,ylab="Estimated effect of log per-capita homesteads",xlab="",title="Lagged fixed effects estimates on county- and state-level measures",leglab="Region")

ggsave(paste0(results.directory,"plots/fe-state-tax.png"), summary.plot, width=11, height=8.5)
