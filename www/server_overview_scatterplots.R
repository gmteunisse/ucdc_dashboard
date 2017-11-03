#Creates a scatterplot of functional assays
plot_assay_dot <- function(data, raw_tables, cols, type){
  
  #Determine whether to display mm or %
  fungi <- c("Pe", "Bo", "Mo", "Geo", "Fu")
  if (sum(cols %in% fungi) > 0){
    ymax <- 100
  }else{
    ymax <- 20
  }
  
  #Define the data
  df <- raw_tables$total
  
  #Define colors: selected or filtered out
  df$Status <- "Filtered"
  index <- which(df$Microorganism_ID %in% data$total$Microorganism_ID)
  df$Status[index] <- "Selected"
  df$Status <- factor(x = df$Status, levels = c("Selected", "Filtered"), ordered = T)
  
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
  
  #Subset the dataframe and make ready for plotting
  df <- subset(x = df, select = c("Microorganism_ID", "Status", cols))
  df.long <- melt(df, id.vars = c("Microorganism_ID", "Status"))
  df.long$value <- as.numeric(df.long$value)
  df.long <- df.long[order(x = df.long$value, decreasing = T),]
  
  #Pick colors
  colors <- c(color_blue[3], color_grey[3])
  if (sum(df.long$Status == "Filtered") == nrow(df.long)){
    colors <- color_grey[3]
  }
  
  #loop through all pathogens and generate plots
  plots <- list()
  for (org in cols){
    #Subset to pathogen
    df.tmp <- df.long[df.long$variable == org,]
    df.tmp$Microorganism_ID <- factor(x = df.tmp$Microorganism_ID,
                                      levels = unique(df.tmp$Microorganism_ID),
                                      ordered = T)
    print(df.tmp)
    #Generate plot
    p <- ggplot(df.tmp, aes(x = Microorganism_ID,
                             y = value, 
                             color = Status, 
                             group = variable,
                             label = Microorganism_ID)) +
      geom_point() +
      scale_y_continuous(limits = c(0, ymax),
                         expand = c(0, 0), 
                         labels = function(x){
                           if(ymax == 100){return(paste(x, "%", sep = ""))}
                           else{return(paste(x,"mm", sep = " "))}}) + 
      scale_color_manual(values = colors) +
      facet_wrap(~variable, scales = "free_y", ncol = 5) +
      plot_theme +
      theme(
        axis.line.y = element_blank(),
        axis.title.y=element_blank(),
        axis.ticks.y=element_blank(),
        axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        legend.position="none"
      )
    plots[[org]] <- p
  }
  #Combine all plots to imitate a facet_wrap
  p <- do.call("grid.arrange", c(plots, nrow = 1))
  
  return(p)
}

server_overview_scatterplots <- function(input, output, server, data, raw_tables){
  #Plot: bacteria (biocontroller) vs bacteria (pathogens)
  output$dot_bact_bact <- renderPlot({
    #Prevent errors from showing up
    if(is.null(data())){
      return(NULL)
    }
    
    #Render the plot
    p <- plot_assay_dot(data(), raw_tables(), c("Pse", "Xa", "Agr"), "bacteria")
    p
  })
  
  #Plot: bacteria (biocontroller) vs fungi (pathogens)
  output$dot_bact_fungi <- renderPlot({
    #Prevent errors from showing up
    if(is.null(data())){
      return(NULL)
    }
    
    #Render the plot
    p <- plot_assay_dot(data(), raw_tables(), c("Pe", "Bo", "Mo", "Geo", "Fu"), "bacteria")
    p
  })
  
  #Plot: fungi (biocontroller) vs bacteria (pathogens)
  output$dot_fungi_bact <- renderPlot({
    #Prevent errors from showing up
    if(is.null(data())){
      return(NULL)
    }
    
    #Render the plot
    p <- plot_assay_dot(data(), raw_tables(), c("Pse", "Xa", "Agr"), "fungi")
    p
  })
  
  #Plot: fungi (biocontroller) vs fungi (pathogens)
  output$dot_fungi_fungi<- renderPlot({
    #Prevent errors from showing up
    if(is.null(data())){
      return(NULL)
    }
    
    #Render the plot
    p <- plot_assay_dot(data(), raw_tables(), c("Pe", "Bo", "Mo", "Geo", "Fu"), "fungi")
    p
  })
}