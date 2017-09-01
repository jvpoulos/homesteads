library(plotly)
library(dplyr)
library(reshape2)

## Analysis 1

# revpc

revpc.names <- colnames(rev.pc)[-1]

revpc <- read_csv("~/Dropbox/github/drnns-prediction/results/revpc/analysis-12/treated/attention.csv", col_names = FALSE)

revpc.attn <- cbind(revpc.names,revpc)

revpc.attn$revpc.names <- sapply(revpc.attn$revpc.names, as.character)

revpc.attn <- cbind("Attention"=revpc.attn$X1,colsplit(revpc.names,"[.]", c("Category","State")))

# clean Category and State 

revpc.attn$Category <- gsub("X", "",revpc.attn$Category)

revpc.attn$State<- gsub("pc.", "",revpc.attn$State)

revpc.attn$Category <- as.numeric(revpc.attn$Category) # drops "cat","type","ed","exp","rev"

# Catgories
rev2 <- c(10:29) 
exp2 <- c(31:40)

rev3 <- c(100,120:123,130:132,140:148,150,160:163,170,180,181,190:193,200,210,220:225,227,230,235,236,240:242,250:253,259:276,280,290:299)
exp3 <- c(310:317,320:325,330:337,340:347,350:357,360:370,380,390:392,395,398,400:417)

revpc.attn <- subset(revpc.attn, ! Category %in% drop.cats)
revpc.attn.s <- subset(revpc.attn, Category %in% c(rev3,exp3) )  # plot 3-digit categories

# Plot

revpc.attn.plot <- plot_ly(
  x = revpc.attn.s$Category, y = factor(revpc.attn.s$State, levels=rev(revpc.attn.s$State)),
  z = revpc.attn.s$Attention, type = "heatmap", name="Attention"
) %>%
  layout(title = 'Per-capita revenue model',
         xaxis = list(title = '3-digit revenue (100-200) and expenditure (300+) categories'),
         yaxis = list(title = 'State')) %>% 
  colorbar(title = "Attention")
htmlwidgets::saveWidget(revpc.attn.plot, file = paste0(results.directory, "plots/revpc-attn-south.html"))

# exppc

exppc.names <- colnames(exp.pc)[-1]

exppc <- read_csv("~/Dropbox/github/drnns-prediction/results/exppc/analysis-12/treated/attention.csv", col_names = FALSE)

exppc.attn <- cbind(exppc.names,exppc)

exppc.attn$exppc.names <- sapply(exppc.attn$exppc.names, as.character)

exppc.attn <- cbind("Attention"=exppc.attn$X1,colsplit(exppc.names,"[.]", c("Category","State")))

# clean Category and State 

exppc.attn$Category <- gsub("X", "",exppc.attn$Category)

exppc.attn$State<- gsub("pc.", "",exppc.attn$State)

exppc.attn$Category <- as.numeric(exppc.attn$Category)

exppc.attn <- subset(exppc.attn, ! is.na(Category))
exppc.attn.s <- subset(exppc.attn, Category %in% c(rev3,exp3) )  # plot 3-digit categories

# Plot

exppc.attn.plot <- plot_ly(
  x = exppc.attn.s$Category, y = factor(exppc.attn.s$State, levels=rev(exppc.attn.s$State)),
  z = exppc.attn.s$Attention, type = "heatmap", name="Attention"
) %>%
  layout(title = 'Per-capita expenditure model',
         xaxis = list(title = '3-digit revenue (100-200) and expenditure (300+) categories'),
         yaxis = list(title = 'State')) %>% 
  colorbar(title = "Attention")
htmlwidgets::saveWidget(exppc.attn.plot, file = paste0(results.directory, "plots/exppc-attn-south.html"))

#edpc

edpc.names <- colnames(ed.pc)[-1]

edpc <- read_csv("~/Dropbox/github/drnns-prediction/results/edpc/analysis-12/treated/attention.csv", col_names = FALSE)

edpc.attn <- cbind(edpc.names,edpc)

edpc.attn$edpc.names <- sapply(edpc.attn$edpc.names, as.character)

edpc.attn <- cbind("Attention"=edpc.attn$X1,colsplit(edpc.names,"[.]", c("Category","State")))

# clean Category and State 

edpc.attn$Category <- gsub("X", "",edpc.attn$Category)

edpc.attn$State<- gsub("pc.", "",edpc.attn$State)

edpc.attn$Category <- as.numeric(edpc.attn$Category)

edpc.attn <- subset(edpc.attn, ! is.na(Category))
edpc.attn.s <- subset(edpc.attn, Category %in% c(rev3,exp3) )  # plot 3-digit categories

# Plot

edpc.attn.plot <- plot_ly(
  x = edpc.attn.s$Category, y = factor(edpc.attn.s$State, levels=rev(edpc.attn.s$State)),
  z = edpc.attn.s$Attention, type = "heatmap", name="Attention"
) %>%
  layout(title = 'Per-capita education model',
         xaxis = list(title = '3-digit revenue (100-200) and expenditure (300+) categories'),
         yaxis = list(title = 'State')) %>% 
  colorbar(title = "Attention")
htmlwidgets::saveWidget(edpc.attn.plot, file = paste0(results.directory, "plots/edpc-attn-south.html"))

## Analysis 3

# revpc

revpc.names <- colnames(rev.pc)[-1]

revpc <- read_csv("~/Dropbox/github/drnns-prediction/results/revpc/analysis-34/treated/attention.csv", col_names = FALSE)

revpc.attn <- cbind(revpc.names,revpc)

revpc.attn$revpc.names <- sapply(revpc.attn$revpc.names, as.character)

revpc.attn <- cbind("Attention"=revpc.attn$X1,colsplit(revpc.names,"[.]", c("Category","State")))

# clean Category and State 

revpc.attn$Category <- gsub("X", "",revpc.attn$Category)

revpc.attn$State<- gsub("pc.", "",revpc.attn$State)

revpc.attn$Category <- as.numeric(revpc.attn$Category) # drops "cat","type","ed","exp","rev"

# Catgories
rev2 <- c(10:29) 
exp2 <- c(31:40)

rev3 <- c(100,120:123,130:132,140:148,150,160:163,170,180,181,190:193,200,210,220:225,227,230,235,236,240:242,250:253,259:276,280,290:299)
exp3 <- c(310:317,320:325,330:337,340:347,350:357,360:370,380,390:392,395,398,400:417)

revpc.attn <- subset(revpc.attn, ! Category %in% drop.cats)
revpc.attn.s <- subset(revpc.attn, Category %in% c(rev3,exp3) )  # plot 3-digit categories

# Plot

revpc.attn.plot <- plot_ly(
  x = revpc.attn.s$Category, y = factor(revpc.attn.s$State, levels=rev(revpc.attn.s$State)),
  z = revpc.attn.s$Attention, type = "heatmap", name="Attention"
) %>%
  layout(title = 'Per-capita revenue model',
         xaxis = list(title = '3-digit revenue (100-200) and expenditure (300+) categories'),
         yaxis = list(title = 'State')) %>% 
  colorbar(title = "Attention")
htmlwidgets::saveWidget(revpc.attn.plot, file = paste0(results.directory, "plots/revpc-attn-public.html"))

# Report category sums
revpc.sum <- revpc.attn.s  %>% 
  group_by(Category) %>% 
  summarise_each(funs(mean(., na.rm = TRUE)))

# exppc

exppc.names <- colnames(exp.pc)[-1]

exppc <- read_csv("~/Dropbox/github/drnns-prediction/results/exppc/analysis-34/treated/attention.csv", col_names = FALSE)

exppc.attn <- cbind(exppc.names,exppc)

exppc.attn$exppc.names <- sapply(exppc.attn$exppc.names, as.character)

exppc.attn <- cbind("Attention"=exppc.attn$X1,colsplit(exppc.names,"[.]", c("Category","State")))

# clean Category and State 

exppc.attn$Category <- gsub("X", "",exppc.attn$Category)

exppc.attn$State<- gsub("pc.", "",exppc.attn$State)

exppc.attn$Category <- as.numeric(exppc.attn$Category)

exppc.attn <- subset(exppc.attn, ! is.na(Category))
exppc.attn.s <- subset(exppc.attn, Category %in% c(rev3,exp3) )  # plot 3-digit categories

# Plot

exppc.attn.plot <- plot_ly(
  x = exppc.attn.s$Category, y = factor(exppc.attn.s$State, levels=rev(exppc.attn.s$State)),
  z = exppc.attn.s$Attention, type = "heatmap", name="Attention"
) %>%
  layout(title = 'Per-capita expenditure model',
         xaxis = list(title = '3-digit revenue (100-200) and expenditure (300+) categories'),
         yaxis = list(title = 'State')) %>% 
  colorbar(title = "Attention")
htmlwidgets::saveWidget(exppc.attn.plot, file = paste0(results.directory, "plots/exppc-attn-public.html"))

# Report category sums
exppc.sum <- exppc.attn.s  %>% 
  group_by(Category) %>% 
  summarise_each(funs(mean(., na.rm = TRUE)))

#edpc

edpc.names <- colnames(ed.pc)[-1]

edpc <- read_csv("~/Dropbox/github/drnns-prediction/results/edpc/analysis-34/treated/attention.csv", col_names = FALSE)

edpc.attn <- cbind(edpc.names,edpc)

edpc.attn$edpc.names <- sapply(edpc.attn$edpc.names, as.character)

edpc.attn <- cbind("Attention"=edpc.attn$X1,colsplit(edpc.names,"[.]", c("Category","State")))

# clean Category and State 

edpc.attn$Category <- gsub("X", "",edpc.attn$Category)

edpc.attn$State<- gsub("pc.", "",edpc.attn$State)

edpc.attn$Category <- as.numeric(edpc.attn$Category)

edpc.attn <- subset(edpc.attn, ! is.na(Category))
edpc.attn.s <- subset(edpc.attn, Category %in% c(rev3,exp3) )  # plot 3-digit categories

# Plot

edpc.attn.plot <- plot_ly(
  x = edpc.attn.s$Category, y = factor(edpc.attn.s$State, levels=rev(edpc.attn.s$State)),
  z = edpc.attn.s$Attention, type = "heatmap", name="Attention"
) %>%
  layout(title = 'Per-capita education model',
         xaxis = list(title = '3-digit revenue (100-200) and expenditure (300+) categories'),
         yaxis = list(title = 'State')) %>% 
  colorbar(title = "Attention")
htmlwidgets::saveWidget(edpc.attn.plot, file = paste0(results.directory, "plots/edpc-attn-public.html"))
