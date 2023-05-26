if (!require(ggplot2))
{
  install.packages("ggplot2")
  library(ggplot2)
}

#plot Kaplan-Meier curves for each level of factor variable
plot_kaplan_factorized <- function(dataset, col, legend_title, scale_lvls=NULL)
{
  dataset[,col] <- as.factor(dataset[,col])
  model_fit <- survfit(formula = as.formula(paste("Surv(time, death_status) ~ ",
                                                  col)),
                       data =  dataset)
  
  if (is.null(scale_lvls))
    scale_lvls = levels(dataset[,col])
  
  autoplot(model_fit) +
    labs(x = "\n Years", y = "Survival Percent \n",
         fill = legend_title, colour = legend_title, title = "") +
    scale_color_discrete(labels = scale_lvls) +
    scale_fill_discrete(labels = scale_lvls) +
    theme(plot.title = element_text(hjust = 0.5), 
          axis.title.x = element_text(face="bold", colour="#FF7A33", size = 14),
          axis.title.y = element_text(face="bold", colour="#FF7A33", size = 14),
          legend.title = element_text(face="bold", size = 14),
          legend.position = c(0.15, 0.2),
          legend.text = element_text(size=14))
}

plot_density_stratified <- function(dataset, col, col_pgs, legend_title)
{
  dataset[, col] = as.factor(dataset[, col]) 
  scale_lvls = levels(dataset[,col])
  
  ggplot(dataset[,c(col, col_pgs)],
         aes_string(x=col_pgs, color = col)) +
    #geom_histogram( color='#e9ecef', alpha=0.6, position='identity')
    geom_density(adjust=0.5, alpha=.0)  +
    labs(title = "", 
         x = "polygenic risk score", 
         y = "density") +
    scale_color_discrete(name = "Has depression?") +
    theme(plot.title = element_text(hjust = 0.5, size = 14), 
          axis.title.x = element_text(face="bold", colour="#FF7A33", size = 14),
          axis.title.y = element_text(face="bold", colour="#FF7A33", size = 14))
  
}

plot_hist_stratified <- function(dataset, col, col_pgs, legend_title)
{
  dataset[, col] = as.factor(dataset[, col]) 
  scale_lvls = levels(dataset[,col])
  
  ggplot(dataset[,c(col, col_pgs)],
         aes_string(x=col_pgs, fill = col)) +
    geom_histogram( color='#e9ecef', alpha=0.6, position='identity') +
    labs(title = "", 
         x = "polygenic risk score", 
         y = "density") +
    scale_color_discrete(name = "Has depression?") +
    theme(plot.title = element_text(hjust = 0.5, size = 14), 
          axis.title.x = element_text(face="bold", colour="#FF7A33", size = 14),
          axis.title.y = element_text(face="bold", colour="#FF7A33", size = 14))
}

plot_roc_curve <- function(data)
{
  #scale_color_manual()
  ggplot(data = data, aes(x = x, y = y)) +
    geom_line(data = subset(data, lt=="roc"), aes(colour="ROC")) +
    geom_line(data = subset(data, lt=="lower"), linetype="dashed", aes(colour="CI")) +
    geom_line(data = subset(data, lt=="upper"), linetype="dashed", aes(colour="CI")) +
    scale_color_manual(name="Legend", values=c("red", "blue", "blue")) +
    labs(title = "ROC Curve", 
         x = "False positive rate", 
         y = "True positive rate") +
    theme(plot.title = element_text(hjust = 0.5, size = 14), 
          axis.title.x = element_text(face="bold", colour="#FF7A33", size = 14),
          axis.title.y = element_text(face="bold", colour="#FF7A33", size = 14),
          legend.position = c(0.8,0.2)) #+
    #xlim(-0.05, 1.05) +
    #ylim(-0.05, 1.05)
}

plot_roc_curve_compared <- function(data1, data1_pgs)
{
  data = data.frame(
    x = c(data1$ROC$x[1:201], data1_pgs$ROC$x[1:201]),
    y = c(data1$ROC$y[1:201], data1_pgs$ROC$y[1:201]),
    lt = c(rep("Clinical", 201), rep("PGS", 201))
  )
  
  ggplot(data = data, aes(x = x, y = y)) +
    geom_line(data = subset(data, lt=="Clinical"), aes(colour="Clinical")) +
    geom_line(data = subset(data, lt=="PGS"),  aes(colour="Clinical + PGS")) +
    scale_color_manual(name="Legend", values=c("red", "blue")) +
    labs(title = "", 
         x = "False positive rate", 
         y = "True positive rate") +
    theme(plot.title = element_text(hjust = 0.5, size = 14), 
          axis.title.x = element_text(face="bold", colour="#FF7A33", size = 14),
          axis.title.y = element_text(face="bold", colour="#FF7A33", size = 14))
}
