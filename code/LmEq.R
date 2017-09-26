LmEq <- function(m) {
  
  l <- list(a = format(coef(m)[1], digits = 2),
            b = format(abs(coef(m)[2]), digits = 2),
            p.val = format(summary(m)[[4]][2], digits = 1));
  
  if (coef(m)[2] >= 0)  {
    eq <- substitute(italic(y) == a + b %.% italic(x)*","~~italic(p)~"="~p.val,l)
  } else {
    eq <- substitute(italic(y) == a - b %.% italic(x)*","~~italic(p)~"="~p.val,l)    
  }
  
  as.character(as.expression(eq));                 
}