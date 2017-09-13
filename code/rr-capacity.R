############################################
# State capacity vs. railroad access plots #
############################################

library(ggplot2)
library(reshape2)
library(dplyr)

ggplotRegression <- function (fit) {
  require(ggplot2)
  ggplot(fit$model, aes_string(x = names(fit$model)[2], y = names(fit$model)[1])) + 
    geom_point() +
    stat_smooth(method = "lm", col = "red") +
    labs(title = paste0("y = ",signif(fit$coef[[1]],2 ), " + ",
                        signif(fit$coef[[2]], 2), "x, ",
                        " p-value = ",signif(summary(fit)$coef[2,4], 2), ", ",
                        " R^2 = ",signif(summary(fit)$r.squared, 2), ", ",
                        " N = ",summary(fit)$df[2]+summary(fit)$df[1]))
}

# Reshape funds

funds.m <- melt(funds[c("state","year","rev.pc","exp.pc")], id.vars = c("state","year"),
                              variable.name = "category",
                              value.name = "value")

# summarize RR data by state

rr.inter.m.state <- rr.inter.m %>% 
  group_by(state,InOpBy) %>% 
  summarise_each(funs(sum(., na.rm = TRUE)),cumulative.track,AREA_SQMI) %>% # state/year sums
  select(state,InOpBy,AREA_SQMI,cumulative.track)

rr.inter.m.state$track2 <- rr.inter.m.state$cumulative.track/rr.inter.m.state$AREA_SQMI # state ratio

# merge funds with rr data

rr.funds <- merge(rr.inter.m.state[rr.inter.m.state$InOpBy>=1865,],  # post-CW obs
                  funds.m[funds.m$year >= 1865,], 
                  by.x =c("state","InOpBy"),
                  by.y = c("state","year"))

# Scatter plot (each point state/year observation) # add lines for each category

ggplot(rr.funds, aes(track2, value)) + 
  geom_point(aes(colour = factor(category)))