
# Get high and low MSPEs (no time dimension)

CollectMSPE <-function(n.pre, forecast, true, x){
  return(mean((as.matrix(true[(n.pre+1):nrow(true),])[,x]-as.matrix(forecast)[,x])**2))
}


# west
west.revpc.lstm.mses <- sapply(1:west.revpc.n.placebo, CollectMSPE, 
                                      n.pre=west.revpc.n.pre, 
                                      forecast=as.matrix(west.revpc.lstm.pred.control),
                                      true=west.revpc.x)

sd(west.revpc.lstm.mses)

west.exppc.lstm.mses <- sapply(1:west.exppc.n.placebo, CollectMSPE, 
                                          n.pre=west.exppc.n.pre, 
                                          forecast=as.matrix(west.exppc.lstm.pred.control),
                                          true=west.exppc.x)

sd(west.exppc.lstm.mses)

west.educpc.lstm.mses <- sapply(1:west.educpc.n.placebo, CollectMSPE, 
                                          n.pre=west.educpc.n.pre, 
                                          forecast=as.matrix(west.educpc.lstm.pred.control),
                                          true=west.educpc.x)

sd(west.educpc.lstm.mses)

# south

south.revpc.lstm.mses <- sapply(1:south.revpc.n.placebo, CollectMSPE, 
                                          n.pre=south.revpc.n.pre, 
                                          forecast=as.matrix(south.revpc.lstm.pred.control),
                                          true=south.revpc.x)

sd(south.revpc.lstm.mses)

south.exppc.lstm.mses <- sapply(1:south.exppc.n.placebo, CollectMSPE, 
                                          n.pre=south.exppc.n.pre, 
                                          forecast=as.matrix(south.exppc.lstm.pred.control),
                                          true=south.exppc.x)

sd(south.exppc.lstm.mses)

south.educpc.lstm.mses <- sapply(1:south.educpc.n.placebo, CollectMSPE, 
                                           n.pre=south.educpc.n.pre, 
                                           forecast=as.matrix(south.educpc.lstm.pred.control),
                                           true=south.educpc.x)

sd(south.educpc.lstm.mses)
