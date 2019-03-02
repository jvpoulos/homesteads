require(softImpute)

# impute missing
if(outcomes){
  y.fits <- softImpute(d[1:which(rownames(d)=="1868"),], rank.max=3, lambda=0, maxit=200, trace.it=TRUE, type="svd") # fit on pre-treatment
} else{
  y.fits <- softImpute(d[1:which(rownames(d)=="1870"),], rank.max=2, lambda=1, maxit=200, trace.it=TRUE, type="svd") 
}

d.imp <- softImpute::complete(d, y.fits) # complete missing entries in full matrix

d.imp <- log(d.imp+.Machine
             $double.eps) # take log