library(plotly)
library(dplyr)
library(reshape2)

Range01 <- function(x, ...){(x - min(x, ...)) / (max(x, ...) - min(x, ...))} # fn to normalize Attn 0 to 1

## Analysis 1

# revpc

revpc.names <- colnames(rev.pc)[-1]

revpc <- read_csv(paste0(results.directory, "predictions/revpc/analysis-12/treated/attention.csv"), col_names = FALSE)

revpc.attn <- cbind(revpc.names,revpc)

revpc.attn$revpc.names <- sapply(revpc.attn$revpc.names, as.character)

revpc.attn <- cbind("Attention"=revpc.attn$X1,colsplit(revpc.names,"pc[.]", c("Category","State")))

# clean Category

levels(revpc.attn$Category) <- c("Expenditures","Revenues")

# Plot

revpc.attn.plot <- plot_ly(
  x = revpc.attn$Category, y = factor(revpc.attn$State, levels=rev(levels(revpc.attn$State))),
  z = Range01(revpc.attn$Attention), type = "heatmap", name="Attention"
) %>%
  layout(title = 'Outcome: revenues',
         xaxis = list(title = 'Predictor'),
         yaxis = list(title = 'State')) %>% 
  colorbar(title = "Attention")
htmlwidgets::saveWidget(revpc.attn.plot, file = paste0(results.directory, "plots/revpc-attn-south.html"))

# exppc

exppc.names <- colnames(exp.pc)[-1]

exppc <- read_csv(paste0(results.directory, "predictions/exppc/analysis-12/treated/attention.csv"), col_names = FALSE)

exppc.attn <- cbind(exppc.names,exppc)

exppc.attn$exppc.names <- sapply(exppc.attn$exppc.names, as.character)

exppc.attn <- cbind("Attention"=exppc.attn$X1,colsplit(exppc.names,"pc[.]", c("Category","State")))

# clean Category

levels(exppc.attn$Category) <- c("Expenditures","Revenues")

# Plot

Range01 <- function(x, ...){(x - min(x, ...)) / (max(x, ...) - min(x, ...))} # normalize Attn 0 to 1

exppc.attn.plot <- plot_ly(
  x = exppc.attn$Category, y = factor(exppc.attn$State, levels=rev(levels(exppc.attn$State))),
  z = Range01(exppc.attn$Attention), type = "heatmap", name="Attention"
) %>%
  layout(title = 'Outcome: expenditures',
         xaxis = list(title = 'Predictor'),
         yaxis = list(title = 'State')) %>% 
  colorbar(title = "Attention")
htmlwidgets::saveWidget(exppc.attn.plot, file = paste0(results.directory, "plots/exppc-attn-south.html"))


## Analysis 3

# revpc

revpc.names <- colnames(rev.pc)[-1]

revpc <- read_csv(paste0(results.directory, "predictions/revpc/analysis-34/treated/attention.csv"), col_names = FALSE)

revpc.attn <- cbind(revpc.names,revpc)

revpc.attn$revpc.names <- sapply(revpc.attn$revpc.names, as.character)

revpc.attn <- cbind("Attention"=revpc.attn$X1,colsplit(revpc.names,"pc[.]", c("Category","State")))

# clean Category

levels(revpc.attn$Category) <- c("Expenditures","Revenues")

# Plot

revpc.attn.plot <- plot_ly(
  x = revpc.attn$Category, y = factor(revpc.attn$State, levels=rev(levels(revpc.attn$State))),
  z = Range01(revpc.attn$Attention), type = "heatmap", name="Attention"
) %>%
  layout(title = 'Outcome: revenues',
         xaxis = list(title = 'Predictor'),
         yaxis = list(title = 'State')) %>% 
  colorbar(title = "Attention")
htmlwidgets::saveWidget(revpc.attn.plot, file = paste0(results.directory, "plots/revpc-attn-public.html"))

# exppc

exppc.names <- colnames(exp.pc)[-1]

exppc <- read_csv(paste0(results.directory, "predictions/exppc/analysis-34/treated/attention.csv"), col_names = FALSE)

exppc.attn <- cbind(exppc.names,exppc)

exppc.attn$exppc.names <- sapply(exppc.attn$exppc.names, as.character)

exppc.attn <- cbind("Attention"=exppc.attn$X1,colsplit(exppc.names,"pc[.]", c("Category","State")))

# clean Category

levels(exppc.attn$Category) <- c("Expenditures","Revenues")

# Plot

Range01 <- function(x, ...){(x - min(x, ...)) / (max(x, ...) - min(x, ...))} # normalize Attn 0 to 1

exppc.attn.plot <- plot_ly(
  x = exppc.attn$Category, y = factor(exppc.attn$State, levels=rev(levels(exppc.attn$State))),
  z = Range01(exppc.attn$Attention), type = "heatmap", name="Attention"
) %>%
  layout(title = 'Outcome: expenditures',
         xaxis = list(title = 'Predictor'),
         yaxis = list(title = 'State')) %>% 
  colorbar(title = "Attention")
htmlwidgets::saveWidget(exppc.attn.plot, file = paste0(results.directory, "plots/exppc-attn-public.html"))
