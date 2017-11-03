#Creates a bar-plot of a category vs a category
plot_cat_cat <- function(data, input){
  
  #Subset to relevant data
  df <- subset(data$total, select = c("Microorganism_ID", 
                                      input$cat_cat_x, 
                                      input$cat_cat_y))
  df <- df[!is.na(df[[input$cat_cat_x]]),]
  
  #Make ready for plotting: define colors
  if (length(unique(df[[input$cat_cat_y]])) < 15){n <- 15}else{n <- length(unique(df[[input$cat_cat_y]]))}
  color <- rep(c(color_blue, color_tan, color_orange), n/15)[1:length(unique(df[[input$cat_cat_y]]))]
  
  #If NAs exist, replace them with text and assign color grey
  if (sum(is.na(df[[input$cat_cat_y]])) > 0){
    na_index <- which(is.na(df[[input$cat_cat_y]]))
    plot_levels <- c(unique(df[[input$cat_cat_y]][!is.na(df[[input$cat_cat_y]])]),
                     "Not available")
    df[[input$cat_cat_y]][na_index] <- "Not available"
    df[[input$cat_cat_y]] <- factor(x = df[[input$cat_cat_y]],
                                    levels = plot_levels,
                                    ordered = T)
    color[length(color)] <- color_grey[3]
  }
  
  #Generate the plot
  plot_code <- sprintf("ggplot(df, aes(%s, group = %s, fill = %s))",
                       input$cat_cat_x,
                       input$cat_cat_y,
                       input$cat_cat_y)
  p <- eval(parse(text = plot_code))
  p <- p + geom_bar(position = "stack") +
    scale_fill_manual(values = color) +
    scale_y_continuous(expand = c(0, 0)) + 
    plot_theme +
    coord_flip() +
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

server_explore_cat_cat <- function(data, input){
  
  #Subset to relevant data
  #df <- eventReactive(c(data(), input$cat_cat_button), {
  #  
  #  #Prevent errors from showing
  #  if(is.null(data())){
  #    return(NULL)
  #  }
  #  
  #  #Subset to relevant data
  #  df <- subset(data()$total, select = c("Microorganism_ID", 
  #                                      input$cat_cat_x, 
  #                                      input$cat_cat_y))
  #  df <- df[!is.na(df[[input$cat_cat_x]]),]
  #  return(df)
  #})
  
  
  #Generate the cat_cat plotheight based on the number of samples available
  #plotheight <- reactive({
  #  if(is.null(df())){
  #    return(100L)
  #  }
  #  m <- length(unique(df()[[input$cat_cat_y]]))
  #  n <- length(unique(df()[[input$cat_cat_x]]))
  #  height <- 10 + 15 * n * m
  #  return(height)
  #})
  
  eventReactive(c(data(), input$cat_cat_button), {
    #Prevent errors from showing up
    if(is.null(data())){
      return(NULL)
    }
    #Render the plot
    p <- plot_cat_cat(data(), input)
    p
  })
}