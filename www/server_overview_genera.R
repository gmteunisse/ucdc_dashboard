#Generates a donut plot for the distribution of identified genera.
plot_genera <- function(data, region){
  
  #Subset to 16S data
  df <- data$total[grep(pattern = region, x = data$total$Amplificiation),]
  if(nrow(df) < 1){
    p <- NULL
    return(p)
  }
  tbl <- as.data.frame(table(df$Genus, useNA = "ifany"), stringsAsFactors = F)
  
  #Make ready for plotting: Sort, move NAs to end and define colors
  if (nrow(tbl) < 15){n <- 15}else{n <- nrow(tbl)}
  color <- rep(c(color_blue, color_tan, color_orange), n/15)[1:nrow(tbl)]
  tbl <- tbl[order(tbl$Freq, decreasing = T),]
  if (sum(is.na(tbl$Var1)) > 0){
    na_index <- which(is.na(tbl$Var1))
    tbl[nrow(tbl) + 1,] <- c("Not Available", tbl[na_index,]$Freq)
    tbl <- tbl[-na_index,]
    color[nrow(tbl)] <- color_grey[3]
  }
  tbl$Var1 <- factor(x = tbl$Var1, levels = tbl$Var1, ordered = T)
  
  #Create plot
  p <- ggplot(tbl, aes(x = "", y = Freq, fill = Var1, group = Var1)) +
    geom_bar(stat = "identity", position = "stack", width = 0.3) +
    coord_polar("y") +
    scale_fill_manual(values = color) +
    geom_text(aes(label = Freq), position = position_stack(vjust = 0.5), size = 3) +
    plot_theme +
    theme(legend.title=element_blank(),
          axis.line.x = element_blank(),
          axis.line.y = element_blank(),
          axis.title.x=element_blank(),
          axis.text.x=element_blank(),
          axis.ticks.x=element_blank(),
          axis.title.y=element_blank(),
          axis.text.y=element_blank(),
          axis.ticks.y=element_blank(),
          strip.background = element_blank(),
          panel.grid.major.y = element_blank(),
          panel.spacing.x = unit(-0.5,"cm"),
          panel.spacing.y = unit(-0.5,"cm"),
          text = element_text(family = "Helvetica", size = 12))
  
  return(p)
}

server_overview_genera <- function(input, output, session, data){
  #Plot: bacterial genera of selected samples
  output$donut_bact <- renderPlot({
    
    #Prevent errors from showing up
    if(is.null(data())){
      return(NULL)
    }
    
    #Generate the plot
    p <- plot_genera(data(), "16S")
    if(is.null(p)){
      return(NULL)
    }
    p
    
  })
  
  #Plot: fungal genera of selected samples
  output$donut_fungi <- renderPlot({
    
    #Prevent errors from showing up
    if(is.null(data())){
      return(NULL)
    }
    
    #Generate the plot
    p <- plot_genera(data(), "ITS")
    if(is.null(p)){
      return(NULL)
    }
    p
    
  })
}