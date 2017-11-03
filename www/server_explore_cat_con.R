#Creates a scatter plot of a categorial vs a continuous variable
plot_cat_con <- function(data, input){
  
  #Subset to relevant data
  df <- subset(data$total, select = c("Microorganism_ID", 
                                      input$cat_con_x, 
                                      input$cat_con_y))
  names(df) <- c("Microorganism_ID", "X", input$cat_con_y)
  df.long <- melt(data = df, id.vars = c("Microorganism_ID", "X"))
  
  #Define colors
  color <- c(color_blue[3],
             color_orange[3],
             color_tan[3],
             color_blue[1],
             color_orange[1],
             color_tan[1],
             color_blue[5],
             color_orange[5],
             color_tan[5])
  
  #Create factors to sort colors by default
  df.long$X <- as.factor(df.long$X)
  df.long$variable <- factor(x = df.long$variable, 
                             levels = c("Pe",
                                        "Bo",
                                        "Mo",
                                        "Geo",
                                        "Fu",
                                        "Pse",
                                        "Xa",
                                        "Agr"),
                             ordered = T)
  
  #Set seed to prevent random movements
  set.seed(1)
  
  #Generate the desired plottype
  if (input$cat_con_type == "Box"){
    p <- ggplot(df.long, aes(x = X, y = value, fill = variable)) +
      geom_boxplot() +
      scale_fill_manual(values = color)
  }else{
    p <- ggplot(df.long, aes(x = X, y = value, group = variable, colour = variable)) +
      geom_jitter(height = 0, width = 0.05, size = 3, alpha = 0.7) +
      scale_colour_manual(values = color)
  }
  
  #Generate the plot
  p <- p + scale_y_continuous(expand = c(0, 0)) + 
    coord_flip() +
    plot_theme +
    theme(
      legend.title=element_blank(),
      panel.grid.major.y = element_blank(),
      panel.grid.major.x = element_line(colour = adjustcolor('grey', 0.2)),
      axis.line.y = element_blank(),
      axis.title.y=element_blank(),
      axis.ticks.y=element_blank(),
      axis.title.x=element_blank(),
      axis.text.x = element_text(angle = 45, hjust = 1)
    )
  return(p)
}

server_explore_cat_con <- function(data, input){
  eventReactive(c(data(), input$cat_con_button), {
    #Prevent errors from showing up
    if(is.null(data())){
      return(NULL)
    }
    #Render the plot
    p <- plot_cat_con(data(), input)
    return(p)
  })
}