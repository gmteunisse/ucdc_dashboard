#Generates a histogram of the distribution of biocontrol activities
plot_assay_hist <- function(data, cols, type){
  
  #Subset the dataframe
  df <- subset(x = data$total, select = c("Microorganism_ID", "Amplificiation", cols))
  
  #Select only organisms of the right type
  if (type == "bacteria"){
    pattern <- "16S"
  }else{
    pattern <- "ITS"
  }
  index <- grep(pattern = pattern, x = df$Amplificiation)
  df <- df[index,]
  if(nrow(df) == 0){
    return(NULL)
  }
  
  #Remove the amplification column
  df <- subset(x = df, select = c("Microorganism_ID", cols))
  
  #Determine whether to output % (bacteria vs. fungi; fungi vs. fungi) or mm (bacteria vs. bacteria)
  fungi <- c("Pe", "Bo", "Mo", "Geo", "Fu")
  if (sum(cols %in% fungi) > 0){
    ymax <- 100
  }else{
    ymax <- 20
  }
  df.long <- melt(df, id.vars = c("Microorganism_ID"))
  p <- ggplot(df.long, aes(value, group = variable)) +
    geom_histogram(bins = 50, position = "dodge", center = 1, fill = color_blue[3]) +
    scale_x_continuous(limits = c(-2, ymax), labels = function(x){
      if(ymax == 100){return(paste(x, "%", sep = ""))}
      else{return(paste(x,"mm", sep = " "))}}) +
    scale_y_continuous(expand = c(0, 0)) + 
    facet_wrap(~variable, scales = "free_y", ncol = 5) +
    plot_theme +
    theme(
      axis.line.y = element_blank(),
      axis.title.y=element_blank(),
      axis.ticks.y=element_blank(),
      axis.title.x=element_blank(),
      legend.position="none"
    )
  
  return(p)
}

server_overview_histograms <- function(input, output, sessions, data){
  #Plot: bacteria (biocontroller) vs bacteria (pathogens)
  output$hist_bact_bact <- renderPlot({
    
    #Prevent errors from showing up
    if(is.null(data())){
      return(NULL)
    }
    
    #Generate the plot
    p <- plot_assay_hist(data(), c("Pse", "Xa", "Agr"), "bacteria")
    p
    
  })
  
  #Plot: bacteria (biocontroller) vs fungi (pathogens)
  output$hist_bact_fungi <- renderPlot({
    
    #Prevent errors from showing up
    if(is.null(data())){
      return(NULL)
    }
    
    #Generate the plot
    p <- plot_assay_hist(data(), c("Pe", "Bo", "Mo", "Geo", "Fu"), "bacteria")
    p
    
  })
  
  #Plot: fungi (biocontroller) vs bacteria (pathogens)
  output$hist_fungi_bact <- renderPlot({
    
    #Prevent errors from showing up
    if(is.null(data())){
      return(NULL)
    }
    
    #Generate the plot
    p <- plot_assay_hist(data(), c("Pse", "Xa", "Agr"), "fungi")
    p
    
  })
  
  #Plot: fungi (biocontroller) vs fungi (pathogens)
  output$hist_fungi_fungi <- renderPlot({
    
    #Prevent errors from showing up
    if(is.null(data())){
      return(NULL)
    }
    
    #Generate the plot
    p <- plot_assay_hist(data(), c("Pe", "Bo", "Mo", "Geo", "Fu"), "fungi")
    p
    
  })
  
}