library(scales)
library(ggplot2)
library(reshape)
library(reshape2)
library(tidyr)
library(wesanderson)
library(plm)
library(stringr)
library(dplyr)

## Heatmap of homestead entries: state x year

homesteads.state <- patents.sum %>%
  group_by(state_code,year) %>% #group counties by state and year
  dplyr::mutate(homesteads.sum=sum(homesteads)) %>%
  distinct(state_code,year,.keep_all = TRUE) %>%
  arrange(state_code,year) %>%
  select(state_code,year, homesteads.sum, ns.pop)
  
homesteads.state <- homesteads.state  %>%
  group_by(state_code) %>%
  dplyr::mutate(homesteads.pc = homesteads.sum/ns.pop) %>%
  filter(year %in% c(1869:1922))  %>%
  arrange(state_code,year)

homesteads.state$state_code <- as.factor(homesteads.state$state_code)
levels(homesteads.state$state_code) <- rev(levels(homesteads.state$state_code))

homesteads.state <- make.pbalanced(homesteads.state, balance.type = "fill")
homesteads.state$homesteads.pc[is.na(homesteads.state$homesteads.pc)] <- 0

homesteads.state.m <- melt(data.frame(homesteads.state)[c("state_code","year","homesteads.pc")], id.vars = c("state_code","year"))

pal <- wes_palette("Zissou1",3, type = "continuous")

p <- ggplot(homesteads.state.m, aes(year,state_code)) + geom_tile(aes(fill = value),
                                                                  colour = "white") + scale_fill_gradientn(colours = pal) + 
  theme_grey(base_size=9) + labs(x="Year", y="Public Land State") + 
  theme_bw() + 
  scale_x_continuous(expand = c(0, 0), breaks= seq(1870, 1940, 10), labels = seq(1870, 1940, 10)) +
  scale_y_discrete(expand = c(0, 0)) +
  labs(fill=str_wrap('Per-Capita Homestead Entries', 10)) +
  theme(panel.border = element_blank(), panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"),
        axis.ticks=element_blank()) 

ggsave(paste0(results.directory,"plots/homestead-heatmap.png"), p, width=11, height=8.5)
