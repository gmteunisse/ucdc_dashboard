#Generates valueboxes for the number of samples selected and their taxonomy
server_overview_boxes <- function(input, output, server, data){
  #ValueBox: number of samples selected
  output$samples_selected <- renderValueBox({
    
    if(is.null(data())){
      value <- 0
    }else{
      value <- nrow(data()$total)
    }
    valueBox(value = value, 
             subtitle = "Samples selected", 
             color = "blue", 
             icon = icon("hashtag")
    )
  })
  
  #ValueBox: number of bacteria selected
  output$bact_selected <- renderValueBox({
    
    if(is.null(data())){
      value <- 0
    }else{
      value <- length(grep(pattern = "16S", x = data()$total$Amplificiation))
    }
    valueBox(value = value, 
             subtitle = "16S amplified samples", 
             color = "blue", 
             icon = icon("hashtag")
    )
  })
  
  #ValueBox: number of fungi selected
  output$fungi_selected <- renderValueBox({
    
    if(is.null(data())){
      value <- 0
    }else{
      value <- length(grep(pattern = "ITS", x = data()$total$Amplificiation))
    }
    valueBox(value = value, 
             subtitle = "ITS amplified samples", 
             color = "blue", 
             icon = icon("hashtag")
    )
  })
}