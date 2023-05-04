
#plot Kaplan-Meier curves for each level of factor variable
plot_kaplan_factorized <- function(dataset, col)
{
  model_fit <- survfit(formula = as.formula(paste("Surv(surv_years, death_status) ~ ",
                                                  col)),
                       data =  dataset)
  
  legened_title <- ""
  autoplot(model_fit) +
    labs(x = "\n Survival Time (Years) ", y = "Survival Probabilities \n",
         fill = legened_title, colour = legened_title, title = "Survival Time of Graduates") +
    #scale_color_discrete(labels = c("Male", "Female")) +
    #scale_fill_discrete(labels = c("Male", "Female")) +
    theme(plot.title = element_text(hjust = 0.5), 
          axis.title.x = element_text(face="bold", colour="#FF7A33", size = 14),
          axis.title.y = element_text(face="bold", colour="#FF7A33", size = 14),
          legend.title = element_text(face="bold", size = 14),
          legend.position = c(0.15, 0.2),
          legend.text = element_text(size=14))
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
          axis.title.y = element_text(face="bold", colour="#FF7A33", size = 14))
}