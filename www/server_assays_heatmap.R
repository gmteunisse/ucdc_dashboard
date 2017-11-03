#Generate a heatmap of the data, showing the perfomance of each microorganism on every assay
plot_heatmap <- function(data, type){
  
  #Define the different assays
  pgp_assays <- c("PA", "PS", "SF", "FN")
  fungi_assays <- c("Pe", "Bo", "Mo", "Geo", "Fu")
  bact_assays <- c("Pse", "Xa", "Agr")
  
  #Subset the data to relevant data
  df <- subset(data$total, select = c("Microorganism_ID",  
                                      "Amplificiation", 
                                      pgp_assays, 
                                      fungi_assays, 
                                      bact_assays))
  
  #Define the type of organism
  df$organism <- NA
  df$organism[grep(pattern= "16S", x = df$Amplificiation)] <- "Bacteria"
  df$organism[grep(pattern= "ITS", x = df$Amplificiation)] <- "Fungi"
  
  #Subset to only the selected type
  df <- df[df$organism == type,]
  
  #Do nothing with empty dataframes
  if (nrow(df) == 0){
    return(NULL)
  }
  
  #Remove the amplification column
  df <- subset(df, select = c("Microorganism_ID", 
                                      "organism",
                                      pgp_assays, 
                                      fungi_assays, 
                                      bact_assays))
  
  #Change to long data type
  df <- melt(df, id.vars = c("Microorganism_ID", "organism"))
  
  #Add labels
  df$group <- NA
  df$group[which(df$variable %in% pgp_assays)] <- "PGP"
  df$group[which(df$variable %in% fungi_assays)] <- "Biocontrol: fungi"
  df$group[which(df$variable %in% bact_assays)] <- "Biocontrol: bacteria"
  
  #Replace yes and no with 1 and 0
  df$value[which(df$value == "yes")] <- 1
  df$value[which(df$value == "no")] <- 0
  df$variable <- as.factor(df$variable)
  df$value <- as.numeric(df$value)
  
  #Create a new column with values expressed as a percentage of the maximum for each assay
  max_values <- aggregate(value ~ organism + variable, df, max, na.rm = T)
  values <- left_join(x = df, y = max_values, by = c("organism" = "organism", "variable" = "variable"))
  #CONTINUE HERE
  df$adj_value <- df$value / values$value.y
  
  #Sum the score of each microorganism over all assays and sort from high to low
  score <- aggregate(adj_value ~ Microorganism_ID, df, sum, na.rm = T)
  score <- score[order(score$adj_value, decreasing = F),]
  df$Microorganism_ID <- factor(x = df$Microorganism_ID, levels = score$Microorganism_ID, ordered = T)
  
  #Remove NAs
  df <- df[!is.na(df$Microorganism_ID),]
  
  #Generate the plot
  p <- ggplot(df, aes(x = variable,
                      y = Microorganism_ID,
                      fill = adj_value,
                      label = value)) +
    geom_tile() +
    facet_grid(organism~group, scales = "free", space = "free") +
    scale_fill_continuous(low = "#eff4fd",
                          high = color_blue[3],
                          na.value = "#FFFFFF") +
    scale_x_discrete(position = "top") +
    geom_text(color = "#2e2e2e" , size = 3) +
    plot_theme +
    theme(
      axis.title.y=element_blank(),
      axis.title.x=element_blank(),
      panel.grid.major = element_blank(),
      legend.position="none"
    )
  return(p)
}

server_assays_heatmap <- function(input, output, server, data){
  
  #Generate the bacterial assays plotheight based on the number of samples available
  plotheight_bact <- reactive({
    if(is.null(data())){
      return(100L)
    }
    n <- length(grep(pattern = "16S", data()$total$Amplificiation))
    height <- 10 + 15 * n
    return(height)
  })
  
  #Generate the fungal assays plotheight based on the number of samples available
  plotheight_fungi <- reactive({
    if(is.null(data())){
      return(100L)
    }
    n <- length(grep(pattern = "ITS", data()$total$Amplificiation))
    print(n)
    height <- 10 + 15 * n
    print(height)
    return(height)
  })
  
  #Render the heatmap for bacteria
  output$heatmap_bact <- renderPlot({
    #Prevent errors from showing up
    if(is.null(data())){
      return(NULL)
    }
    
    #Render the plot
    p <- plot_heatmap(data(), "Bacteria")
    p
  })
  
  #Render the heatmap for fungi
  output$heatmap_fungi <- renderPlot({
    #Prevent errors from showing up
    if(is.null(data())){
      return(NULL)
    }
    
    #Render the plot
    p <- plot_heatmap(data(), "Fungi")
    p
  })
  
  #Render the correct plotheight for the heatmap
  output$heatmap_bact.ui <- renderUI({
    return(plotOutput("heatmap_bact", height = plotheight_bact()))
  })
  output$heatmap_fungi.ui <- renderUI({
    return(plotOutput("heatmap_fungi", height = plotheight_fungi()))
  })
}