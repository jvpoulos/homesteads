library(plotly)
library(dplyr)
library(reshape2)

Range01 <- function(x, ...){(x - min(x, ...)) / (max(x, ...) - min(x, ...))} # fn to normalize Attn 0 to 1

analysis <-0

if(analysis==0){
## Analysis 0

# revpc

revpc.names <- colnames(rev.pc)[-1]

revpc <- read_csv(paste0(results.directory, "predictions/revpc/analysis-01/treated/attention.csv"), col_names = FALSE)

revpc.attn <- cbind(revpc.names,revpc)

revpc.attn$revpc.names <- sapply(revpc.attn$revpc.names, as.character)

revpc.attn <- cbind("Attention"=revpc.attn$X1,colsplit(revpc.names,"pc[.]", c("Category","State")))

# clean Category

levels(revpc.attn$Category) <- c("Expenditures","Revenues","Education spending")

# Plot

revpc.attn.plot <- plot_ly(
  x = revpc.attn$Category, y = factor(revpc.attn$State, levels=rev(levels(revpc.attn$State))),
  z = Range01(revpc.attn$Attention), type = "heatmap", name="Attention"
) %>%
  layout(title = 'Outcome: revenues',
         xaxis = list(title = 'Predictor'),
         yaxis = list(title = 'State')) %>% 
  colorbar(title = "Attention")
htmlwidgets::saveWidget(revpc.attn.plot, file = paste0(results.directory, "plots/revpc-attn-hsa.html"))

# exppc

exppc.names <- colnames(exp.pc)[-1]

exppc <- read_csv(paste0(results.directory, "predictions/exppc/analysis-01/treated/attention.csv"), col_names = FALSE)

exppc.attn <- cbind(exppc.names,exppc)

exppc.attn$exppc.names <- sapply(exppc.attn$exppc.names, as.character)

exppc.attn <- cbind("Attention"=exppc.attn$X1,colsplit(exppc.names,"pc[.]", c("Category","State")))

# clean Category

levels(exppc.attn$Category) <- c("Expenditures","Revenues","Education spending")

# Plot

exppc.attn.plot <- plot_ly(
  x = exppc.attn$Category, y = factor(exppc.attn$State, levels=rev(levels(exppc.attn$State))),
  z = Range01(exppc.attn$Attention), type = "heatmap", name="Attention"
) %>%
  layout(title = 'Outcome: expenditures',
         xaxis = list(title = 'Predictor'),
         yaxis = list(title = 'State')) %>% 
  colorbar(title = "Attention")
htmlwidgets::saveWidget(exppc.attn.plot, file = paste0(results.directory, "plots/exppc-attn-hsa.html"))

# educpc

educpc.names <- colnames(exp.pc)[-1]

educpc <- read_csv(paste0(results.directory, "predictions/educpc/analysis-01/treated/attention.csv"), col_names = FALSE)

educpc.attn <- cbind(educpc.names,educpc)

educpc.attn$educpc.names <- sapply(educpc.attn$educpc.names, as.character)

educpc.attn <- cbind("Attention"=educpc.attn$X1,colsplit(educpc.names,"pc[.]", c("Category","State")))

# clean Category

levels(educpc.attn$Category) <- c("Expenditures","Revenues","Education spending")

# Plot

educpc.attn.plot <- plot_ly(
  x = educpc.attn$Category, y = factor(educpc.attn$State, levels=rev(levels(educpc.attn$State))),
  z = Range01(educpc.attn$Attention), type = "heatmap", name="Attention"
) %>%
  layout(title = 'Outcome: education spending',
         xaxis = list(title = 'Predictor'),
         yaxis = list(title = 'State')) %>% 
  colorbar(title = "Attention")
htmlwidgets::saveWidget(educpc.attn.plot, file = paste0(results.directory, "plots/educpc-attn-hsa.html"))
}

if (analysis==1){
## Analysis 1

# revpc

revpc.names <- colnames(rev.pc)[-1]

revpc <- read_csv(paste0(results.directory, "predictions/revpc/analysis-12/treated/attention.csv"), col_names = FALSE)

revpc.attn <- cbind(revpc.names,revpc)

revpc.attn$revpc.names <- sapply(revpc.attn$revpc.names, as.character)

revpc.attn <- cbind("Attention"=revpc.attn$X1,colsplit(revpc.names,"pc[.]", c("Category","State")))

# clean Category

levels(revpc.attn$Category) <- c("Expenditures","Revenues","Education spending")

# Plot

revpc.attn.plot <- plot_ly(
  x = revpc.attn$Category, y = factor(revpc.attn$State, levels=rev(levels(revpc.attn$State))),
  z = Range01(revpc.attn$Attention), type = "heatmap", name="Attention"
) %>%
  layout(title = 'Outcome: revenues',
         xaxis = list(title = 'Predictor'),
         yaxis = list(title = 'State')) %>% 
  colorbar(title = "Attention")
htmlwidgets::saveWidget(revpc.attn.plot, file = paste0(results.directory, "plots/revpc-attn-sha.html"))

# exppc

exppc.names <- colnames(exp.pc)[-1]

exppc <- read_csv(paste0(results.directory, "predictions/exppc/analysis-12/treated/attention.csv"), col_names = FALSE)

exppc.attn <- cbind(exppc.names,exppc)

exppc.attn$exppc.names <- sapply(exppc.attn$exppc.names, as.character)

exppc.attn <- cbind("Attention"=exppc.attn$X1,colsplit(exppc.names,"pc[.]", c("Category","State")))

# clean Category

levels(exppc.attn$Category) <- c("Expenditures","Revenues","Education spending")

# Plot

exppc.attn.plot <- plot_ly(
  x = exppc.attn$Category, y = factor(exppc.attn$State, levels=rev(levels(exppc.attn$State))),
  z = Range01(exppc.attn$Attention), type = "heatmap", name="Attention"
) %>%
  layout(title = 'Outcome: expenditures',
         xaxis = list(title = 'Predictor'),
         yaxis = list(title = 'State')) %>% 
  colorbar(title = "Attention")
htmlwidgets::saveWidget(exppc.attn.plot, file = paste0(results.directory, "plots/exppc-attn-sha.html"))

# educpc

educpc.names <- colnames(educ.pc)[-1]

educpc <- read_csv(paste0(results.directory, "predictions/educpc/analysis-12/treated/attention.csv"), col_names = FALSE)

educpc.attn <- cbind(educpc.names,educpc)

educpc.attn$educpc.names <- sapply(educpc.attn$educpc.names, as.character)

educpc.attn <- cbind("Attention"=educpc.attn$X1,colsplit(educpc.names,"pc[.]", c("Category","State")))

# clean Category

levels(educpc.attn$Category) <- c("Expenditures","Revenues","Education spending")

# Plot

educpc.attn.plot <- plot_ly(
  x = educpc.attn$Category, y = factor(educpc.attn$State, levels=rev(levels(educpc.attn$State))),
  z = Range01(educpc.attn$Attention), type = "heatmap", name="Attention"
) %>%
  layout(title = 'Outcome: expenditures',
         xaxis = list(title = 'Predictor'),
         yaxis = list(title = 'State')) %>% 
  colorbar(title = "Attention")
htmlwidgets::saveWidget(educpc.attn.plot, file = paste0(results.directory, "plots/educpc-attn-sha.html"))
}

if (analysis==3){
## Analysis 3

# revpc

revpc.names <- colnames(rev.pc)[-1]

revpc <- read_csv(paste0(results.directory, "predictions/revpc/analysis-34/treated/attention.csv"), col_names = FALSE)

revpc.attn <- cbind(revpc.names,revpc)

revpc.attn$revpc.names <- sapply(revpc.attn$revpc.names, as.character)

revpc.attn <- cbind("Attention"=revpc.attn$X1,colsplit(revpc.names,"pc[.]", c("Category","State")))

# clean Category

levels(revpc.attn$Category) <- c("Expenditures","Revenues","Education spending")

# Plot

revpc.attn.plot <- plot_ly(
  x = revpc.attn$Category, y = factor(revpc.attn$State, levels=rev(levels(revpc.attn$State))),
  z = Range01(revpc.attn$Attention), type = "heatmap", name="Attention"
) %>%
  layout(title = 'Outcome: revenues',
         xaxis = list(title = 'Predictor'),
         yaxis = list(title = 'State')) %>% 
  colorbar(title = "Attention")
htmlwidgets::saveWidget(revpc.attn.plot, file = paste0(results.directory, "plots/revpc-attn-89.html"))

# exppc

exppc.names <- colnames(exp.pc)[-1]

exppc <- read_csv(paste0(results.directory, "predictions/exppc/analysis-34/treated/attention.csv"), col_names = FALSE)

exppc.attn <- cbind(exppc.names,exppc)

exppc.attn$exppc.names <- sapply(exppc.attn$exppc.names, as.character)

exppc.attn <- cbind("Attention"=exppc.attn$X1,colsplit(exppc.names,"pc[.]", c("Category","State")))

# clean Category

levels(exppc.attn$Category) <- c("Expenditures","Revenues","Education spending")

# Plot

exppc.attn.plot <- plot_ly(
  x = exppc.attn$Category, y = factor(exppc.attn$State, levels=rev(levels(exppc.attn$State))),
  z = Range01(exppc.attn$Attention), type = "heatmap", name="Attention"
) %>%
  layout(title = 'Outcome: expenditures',
         xaxis = list(title = 'Predictor'),
         yaxis = list(title = 'State')) %>% 
  colorbar(title = "Attention")
htmlwidgets::saveWidget(exppc.attn.plot, file = paste0(results.directory, "plots/exppc-attn-89.html"))

# educpc

educpc.names <- colnames(educ.pc)[-1]

educpc <- read_csv(paste0(results.directory, "predictions/educpc/analysis-34/treated/attention.csv"), col_names = FALSE)

educpc.attn <- cbind(educpc.names,educpc)

educpc.attn$educpc.names <- sapply(educpc.attn$educpc.names, as.character)

educpc.attn <- cbind("Attention"=educpc.attn$X1,colsplit(educpc.names,"pc[.]", c("Category","State")))

# clean Category

levels(educpc.attn$Category) <- c("Expenditures","Revenues","Education spending")

# Plot

educpc.attn.plot <- plot_ly(
  x = educpc.attn$Category, y = factor(educpc.attn$State, levels=rev(levels(educpc.attn$State))),
  z = Range01(educpc.attn$Attention), type = "heatmap", name="Attention"
) %>%
  layout(title = 'Outcome: education spending',
         xaxis = list(title = 'Predictor'),
         yaxis = list(title = 'State')) %>% 
  colorbar(title = "Attention")
htmlwidgets::saveWidget(educpc.attn.plot, file = paste0(results.directory, "plots/educpc-attn-89.html"))
}

if (analysis==4){
  ## Analysis 4
  
  # revpc
  
  revpc.names <- colnames(rev.pc)[-1]
  
  revpc <- read_csv(paste0(results.directory, "predictions/revpc/analysis-41/treated/attention.csv"), col_names = FALSE)
  
  revpc.attn <- cbind(revpc.names,revpc)
  
  revpc.attn$revpc.names <- sapply(revpc.attn$revpc.names, as.character)
  
  revpc.attn <- cbind("Attention"=revpc.attn$X1,colsplit(revpc.names,"pc[.]", c("Category","State")))
  
  # clean Category
  
  levels(revpc.attn$Category) <- c("Expenditures","Revenues","Education spending")
  
  # Plot
  
  revpc.attn.plot <- plot_ly(
    x = revpc.attn$Category, y = factor(revpc.attn$State, levels=rev(levels(revpc.attn$State))),
    z = Range01(revpc.attn$Attention), type = "heatmap", name="Attention"
  ) %>%
    layout(title = 'Outcome: revenues',
           xaxis = list(title = 'Predictor'),
           yaxis = list(title = 'State')) %>% 
    colorbar(title = "Attention")
  htmlwidgets::saveWidget(revpc.attn.plot, file = paste0(results.directory, "plots/revpc-attn-89-west.html"))
  
  # exppc
  
  exppc.names <- colnames(exp.pc)[-1]
  
  exppc <- read_csv(paste0(results.directory, "predictions/exppc/analysis-41/treated/attention.csv"), col_names = FALSE)
  
  exppc.attn <- cbind(exppc.names,exppc)
  
  exppc.attn$exppc.names <- sapply(exppc.attn$exppc.names, as.character)
  
  exppc.attn <- cbind("Attention"=exppc.attn$X1,colsplit(exppc.names,"pc[.]", c("Category","State")))
  
  # clean Category
  
  levels(exppc.attn$Category) <- c("Expenditures","Revenues","Education spending")
  
  # Plot
  
  exppc.attn.plot <- plot_ly(
    x = exppc.attn$Category, y = factor(exppc.attn$State, levels=rev(levels(exppc.attn$State))),
    z = Range01(exppc.attn$Attention), type = "heatmap", name="Attention"
  ) %>%
    layout(title = 'Outcome: expenditures',
           xaxis = list(title = 'Predictor'),
           yaxis = list(title = 'State')) %>% 
    colorbar(title = "Attention")
  htmlwidgets::saveWidget(exppc.attn.plot, file = paste0(results.directory, "plots/exppc-attn-89-west.html"))
  
  # educpc
  
  educpc.names <- colnames(educ.pc)[-1]
  
  educpc <- read_csv(paste0(results.directory, "predictions/educpc/analysis-41/treated/attention.csv"), col_names = FALSE)
  
  educpc.attn <- cbind(educpc.names,educpc)
  
  educpc.attn$educpc.names <- sapply(educpc.attn$educpc.names, as.character)
  
  educpc.attn <- cbind("Attention"=educpc.attn$X1,colsplit(educpc.names,"pc[.]", c("Category","State")))
  
  # clean Category
  
  levels(educpc.attn$Category) <- c("Expenditures","Revenues","Education spending")
  
  # Plot
  
  educpc.attn.plot <- plot_ly(
    x = educpc.attn$Category, y = factor(educpc.attn$State, levels=rev(levels(educpc.attn$State))),
    z = Range01(educpc.attn$Attention), type = "heatmap", name="Attention"
  ) %>%
    layout(title = 'Outcome: education spending',
           xaxis = list(title = 'Predictor'),
           yaxis = list(title = 'State')) %>% 
    colorbar(title = "Attention")
  htmlwidgets::saveWidget(educpc.attn.plot, file = paste0(results.directory, "plots/educpc-attn-89-west.html"))
}
