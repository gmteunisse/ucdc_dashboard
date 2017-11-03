#Generates a donut plot for the distribution of "yes", "no" and "not available"
plot_freq <- function(data, variable){
  
  #Count occurrences and make ready for plotting. Not using table() function because not suitable.
  df <- data$total
  tbl <- data.frame(Var1 = factor(x = c("Yes", "No", "Not available"),
                                  levels = c("Yes", "No", "Not available"),
                                  ordered = T),
                    Freq = c(0, 0, 0))
  tbl[1,2] <- sum(tolower(df[[variable]]) == "yes", na.rm = T)
  tbl[2,2] <- sum(tolower(df[[variable]]) == "no", na.rm = T)
  tbl[3,2] <- sum(is.na(df[[variable]]))
  tbl$Freq[tbl$Freq == 0] <- NA
  colors <- c(Yes = color_orange[3], No = color_blue[3], "Not available" = color_grey[3])
  
  #Create plot
  p <- ggplot(tbl, aes(x = "", y = Freq, fill = Var1, group = Var1)) +
    geom_bar(stat = "identity", width = 0.3) +
    geom_text(aes(label = Freq), position = position_stack(vjust = 0.5), size = 3) +
    coord_polar("y") +
    scale_fill_manual(values = colors) +
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
          text = element_text(family = "Helvetica", size = 12),
          legend.position = "top",
          legend.direction = "vertical")
  
  return(p)
}

#Generates a donut plot for the distribution of human pathogenicity levels
plot_path <- function(data){
  
  #Count occurrences and make ready for plotting. Not using table() function because not suitable.
  df <- data$total
  tbl <- data.frame(Var1 = factor(x = c(1, 2, 3, 4, "Not available"),
                                  levels = c(1, 2, 3, 4, "Not available"),
                                  ordered = T),
                    Freq = c(0, 0, 0, 0, 0))
  tbl[1,2] <- sum(df[["Pathogen_class"]] == "1", na.rm = T)
  tbl[2,2] <- sum(df[["Pathogen_class"]] == "2", na.rm = T)
  tbl[3,2] <- sum(df[["Pathogen_class"]] == "3", na.rm = T)
  tbl[4,2] <- sum(df[["Pathogen_class"]] == "4", na.rm = T)
  tbl[5,2] <- sum(is.na(df[["Pathogen_class"]]))
  tbl$Freq[tbl$Freq == 0] <- NA
  colors <- c("1" = color_blue[3], 
              "2" = color_orange[3],
              "3" = color_orange[2],
              "4" = color_orange[1],
              "Not available" = color_grey[3])
  
  #Create plot
  p <- ggplot(tbl, aes(x = "", y = Freq, fill = Var1, group = Var1)) +
    geom_bar(stat = "identity", width = 0.3) +
    geom_text(aes(label = Freq), position = position_stack(vjust = 0.5), size = 3) +
    coord_polar("y") +
    scale_fill_manual(values = colors) +
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
          text = element_text(family = "Helvetica", size = 12),
          legend.position = "top",
          legend.direction = "vertical")
  
  return(p)
}

server_overview_donuts <- function(input, output, session, data){
  #Plot: plant pathogenicity
  output$donut_plant_path <- renderPlot({
    
    #Prevent errors from showing uo
    if(is.null(data())){
      return(NULL)
    }
    
    #Generate the plot
    p <- plot_freq(data(), "Pathogenicity_plant")
    p
    
  })
  
  #Plot: human pathogenicity
  output$donut_human_path <- renderPlot({
    
    #Prevent errors from showing uo
    if(is.null(data())){
      return(NULL)
    }
    
    #Generate the plot
    p <- plot_path(data())
    p
    
  })
  
  #Plot: PA pgp activity
  output$donut_PA <- renderPlot({
    
    #Prevent errors from showing uo
    if(is.null(data())){
      return(NULL)
    }
    
    #Generate the plot
    p <- plot_freq(data(), "PA")
    p
    
  })
  
  #Plot: PS pgp activity
  output$donut_PS <- renderPlot({
    
    #Prevent errors from showing uo
    if(is.null(data())){
      return(NULL)
    }
    
    #Generate the plot
    p <- plot_freq(data(), "PS")
    p
    
  })
  
  #Plot: SF pgp activity
  output$donut_SF <- renderPlot({
    
    #Prevent errors from showing uo
    if(is.null(data())){
      return(NULL)
    }
    
    #Generate the plot
    p <- plot_freq(data(), "SF")
    p
    
  })
  
  #Plot: FN pgp activity
  output$donut_FN <- renderPlot({
    
    #Prevent errors from showing uo
    if(is.null(data())){
      return(NULL)
    }
    
    #Generate the plot
    p <- plot_freq(data(), "FN")
    p
    
  })
}