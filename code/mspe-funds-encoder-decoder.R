
# Get high and low MSPEs (no time dimension)

CollectMSPE <-function(n.pre, forecast, true, x){
  return(mean((as.matrix(true[(n.pre+1):nrow(true),])[,x]-as.matrix(forecast)[,x])**2))
}


# west
west.revpc.encoder.decoder.mses <- sapply(1:west.revpc.n.placebo, CollectMSPE, 
                                      n.pre=west.revpc.n.pre, 
                                      forecast=as.matrix(west.revpc.encoder.decoder.pred.control),
                                      true=west.revpc.x)

sd(west.revpc.encoder.decoder.mses)

west.exppc.encoder.decoder.mses <- sapply(1:west.exppc.n.placebo, CollectMSPE, 
                                          n.pre=west.exppc.n.pre, 
                                          forecast=as.matrix(west.exppc.encoder.decoder.pred.control),
                                          true=west.exppc.x)

sd(west.exppc.encoder.decoder.mses)

west.educpc.encoder.decoder.mses <- sapply(1:west.educpc.n.placebo, CollectMSPE, 
                                          n.pre=west.educpc.n.pre, 
                                          forecast=as.matrix(west.educpc.encoder.decoder.pred.control),
                                          true=west.educpc.x)

sd(west.educpc.encoder.decoder.mses)

# south

south.revpc.encoder.decoder.mses <- sapply(1:south.revpc.n.placebo, CollectMSPE, 
                                          n.pre=south.revpc.n.pre, 
                                          forecast=as.matrix(south.revpc.encoder.decoder.pred.control),
                                          true=south.revpc.x)

sd(south.revpc.encoder.decoder.mses)

south.exppc.encoder.decoder.mses <- sapply(1:south.exppc.n.placebo, CollectMSPE, 
                                          n.pre=south.exppc.n.pre, 
                                          forecast=as.matrix(south.exppc.encoder.decoder.pred.control),
                                          true=south.exppc.x)

sd(south.exppc.encoder.decoder.mses)

south.educpc.encoder.decoder.mses <- sapply(1:south.educpc.n.placebo, CollectMSPE, 
                                           n.pre=south.educpc.n.pre, 
                                           forecast=as.matrix(south.educpc.encoder.decoder.pred.control),
                                           true=south.educpc.x)

sd(south.educpc.encoder.decoder.mses)
