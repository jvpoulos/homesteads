###################################
# Summarize non-response and treatment status #
###################################

library(ggplot2)
library(wesanderson)
library(reshape)
library(reshape2)
library(stringr)
library(grid)

# Read data
capacity.outcomes <- readRDS(paste0("data/capacity-outcomes-none.rds"))

# Prepare outcomes data
exp <- capacity.outcomes$exp.pc
treat <- exp$mask # NxT masked matrix 
Y.missing <- exp$M.missing # NxT # 1=observed, NA=missing/imputed
Y <- exp$M # NxT

pub.states2 <- pub.states[pub.states%in%rownames(Y)]
state.land.states2 <- state.land.states[state.land.states%in%rownames(Y)]

# summary stats for paper
print(round(sum(Y.missing)/(dim(Y.missing)[1]*dim(Y.missing)[2]),3)) # % missing overall

print(round(sum(Y.missing[rownames(Y.missing)%in%pub.states2,][,colnames(Y.missing) >= 1869])/(dim(Y.missing)[1]*dim(Y.missing)[2]),3)) # % missing in test set

print(round(sum(Y.missing[rownames(Y.missing)%in%pub.states2,])/(dim(Y.missing)[1]*dim(Y.missing)[2]),3)) # % missing in treated
print(round(sum(Y.missing[rownames(Y.missing)%in%state.land.states2,])/(dim(Y.missing)[1]*dim(Y.missing)[2]),3)) # % missing in control

# N x T Heatmap: Nonresponse

Y.missing[is.na(Y.missing)] <- 2 # 1=observed, 2=missing
Y.missing <- Y.missing-1  # 1=missing, 0=observed

Y.missing <- Y.missing[match(c(rev(state.land.states2),rev(pub.states2)), row.names(Y.missing)),]

Y.missing.m <- melt(Y.missing)
colnames(Y.missing.m) <- c("State","Year","value")

Y.missing.m$Year <- factor(Y.missing.m$Year)

Y.missing.m$State <- factor(Y.missing.m$State, levels=c(rev(state.land.states2),rev(pub.states2)))

year.labels <- c(colnames(Y.missing)[seq(1,ncol(Y.missing),15)])

text_high <- textGrob("Treated", gp=gpar(fontsize=9, fontface="bold"), rot=90)
text_low <- textGrob("Control", gp=gpar(fontsize=9, fontface="bold"),rot=90)

nonre.heat <- ggplot(Y.missing.m, aes(Year,State)) + geom_tile(aes(fill = value),
                                                                  colour = "white") + 
  scale_fill_gradient(low="lightblue", high="red",
                      breaks=c(0, 1),
                      limits=c(0, 1),
                      labels=rev(c("Missing","Present"))) + 
  theme_grey(base_size=9) + labs(x="Year", y=" ") + 
  theme_bw() + 
  geom_hline(yintercept = 19.5) +
  annotation_custom(text_high,xmin=-12,xmax=-17,ymin=28,ymax=28) + 
  annotation_custom(text_low,xmin=-12,xmax=-17,ymin=10,ymax=10) +
  coord_cartesian(clip = "off") +
  scale_x_discrete(expand = c(0, 0), breaks= year.labels, labels = year.labels) +
  scale_y_discrete(expand = c(0, 0)) +
  labs(fill=str_wrap('Observations', 15)) +
  theme(panel.border = element_blank(), panel.grid.major = element_blank(), legend.title = element_text(size = 9, face="bold"), 
        axis.text.x = element_text(size=9),
        panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"),
        axis.ticks=element_blank())

ggsave(paste0(plots.directory,"/missing-heatmap.png"), nonre.heat, scale=1.25)

# N x T Heatmap: Treatment

treat[rownames(treat)%in%pub.states2,] <- treat[rownames(treat)%in%pub.states2,] +1

states.sort <- c(rev(state.land.states2), names(sort(rowSums(treat[rownames(treat)%in%pub.states2,])))) # sort by initial entry

treat <- treat[match(states.sort, row.names(treat)),]

treat.m <- melt(treat)
colnames(treat.m) <- c("State","Year","value")

treat.m$Year <- factor(treat.m$Year)

treat.m$State <- factor(treat.m$State, levels=c(rev(state.land.states2),rev(pub.states2)))

year.labels <- c(colnames(treat)[seq(1,ncol(treat),15)])

pal <- wes_palette("Zissou1",5, type = "discrete")

treat.heat <- ggplot(treat.m, aes(Year,State)) + geom_tile(aes(fill = value),
                                                               colour = "white") + 
  scale_fill_gradientn(colours=pal[c(2,3,5)],
                      breaks=c(0,1,2),
                      limits=c(0,2),
                      labels=rev(c("Treated (post)", "Treated (pre)","Control"))) + 
  theme_grey(base_size=9) + labs(x="Year", y=" ") + 
  theme_bw() + 
  geom_hline(yintercept = 19.5) +
  annotation_custom(text_high,xmin=-12,xmax=-17,ymin=28,ymax=28) + 
  annotation_custom(text_low,xmin=-12,xmax=-17,ymin=10,ymax=10) +
  coord_cartesian(clip = "off") +
  scale_x_discrete(expand = c(0, 0), breaks= year.labels, labels = year.labels) +
  scale_y_discrete(expand = c(0, 0)) +
  labs(fill=str_wrap('Treatment status', 25)) +
  theme(panel.border = element_blank(), panel.grid.major = element_blank(), legend.title = element_text(size = 9, face="bold"), 
        axis.text.x = element_text(size=9),
        panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"),
        axis.ticks=element_blank()) 

ggsave(paste0(plots.directory, "/treat-heatmap.png"), treat.heat, scale=1.25)