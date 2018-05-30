## Plot rev/exp time-series

funds.plot <- funds

funds.plot$cat <- NA
funds.plot$cat[funds.plot$state %in% southern.pub] <- "Southern public land"
funds.plot$cat[funds.plot$state %in% southern.state] <- "Southern state land"
funds.plot$cat[funds.plot$state %in% setdiff(pub.states,southern.pub)] <- "Western public land"
funds.plot$cat[funds.plot$state %in% setdiff(state.land.states,southern.state)] <- "Northeastern state land"

cats.funds.plot <- funds.plot %>% 
  filter(!is.na(cat)) %>%  # rm non-southern state land states
  group_by(year,cat) %>% 
  summarise_each(funs(mean(., na.rm = TRUE)),rev.pc,exp.pc,educ.pc) 

# By time x region

rev.pc.state.time <- ggplot(cats.funds.plot, aes( year, rev.pc ,color=cat )) + 
  geom_smooth(span=0.1, se = FALSE) +
  coord_cartesian(xlim=c(1800,1975)) +
  geom_vline(xintercept=1862, linetype=1) +  
  geom_vline(xintercept=1866, linetype=2) + # Southern HSA signed
 # geom_vline(xintercept=1876, linetype=2) + # Southern HSA repealed
#  geom_vline(xintercept=1889, linetype=5) +  
  scale_y_continuous(name="State government total revenue, log per-capita (1982$)") +
  xlab("") +
  scale_color_discrete("State type")

ggsave(paste0(results.directory,"plots/rev-pc-state-time.png"), rev.pc.state.time, width=11, height=8.5)

exp.pc.state.time <- ggplot(cats.funds.plot, aes( year, exp.pc ,color=cat )) + 
  geom_smooth(span=0.1, se = FALSE) +
  coord_cartesian(xlim=c(1800,1975)) +
  geom_vline(xintercept=1862, linetype=1) +  
  geom_vline(xintercept=1866, linetype=2) + # Southern HSA signed
#  geom_vline(xintercept=1876, linetype=2) + # Southern HSA repealed
#  geom_vline(xintercept=1889, linetype=5) +  
  scale_y_continuous(name="State government total expenditure, log per-capita (1982$)") +
  xlab("") +
  scale_color_discrete("State type")

ggsave(paste0(results.directory,"plots/exp-pc-state-time.png"), exp.pc.state.time, width=11, height=8.5)

educ.pc.state.time <- ggplot(cats.funds.plot, aes( year, educ.pc ,color=cat )) + 
  geom_smooth(span=0.1, se = FALSE) +
  coord_cartesian(xlim=c(1800,1942)) +
  geom_vline(xintercept=1862, linetype=1) +  
  geom_vline(xintercept=1866, linetype=2) + # Southern HSA signed
#  geom_vline(xintercept=1876, linetype=2) + # Southern HSA repealed
 # geom_vline(xintercept=1889, linetype=5) +  
  scale_y_continuous(name="State government education spending, log per-capita (1982$)") +
  xlab("") +
  scale_color_discrete("State type")

ggsave(paste0(results.directory,"plots/educ-pc-state-time.png"), educ.pc.state.time, width=11, height=8.5)