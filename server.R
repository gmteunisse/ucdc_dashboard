## Title:   Server for shiny dashboard for collection analysis
## Date:    19-oct-2017
## Author:  Guus Martijn Teunisse (gmteunisse@gmail.com)
##########################################################################

##### Shiny server #####

#Define the shiny server
server <- function(input, output) {
  
  #Obtain and read input data from the user
  raw_tables <- reactive({
    inFile <- input$file1
    if(is.null(inFile)){
      return(NULL)
    }
    tables <- list()
    tables$samples <- as.data.frame(read_excel(path = inFile$datapath,
                                               sheet = 1,
                                               trim_ws = T,
                                               skip = 1))
    tables$isolation <- as.data.frame(read_excel(path = inFile$datapath,
                                                 sheet = 2,
                                                 trim_ws = T,
                                                 skip = 1))
    tables$taxonomy <- as.data.frame(read_excel(path = inFile$datapath,
                                                sheet = 3,
                                                trim_ws = T,
                                                skip = 1))
    tables$literature <- as.data.frame(read_excel(path = inFile$datapath,
                                                  sheet = 4,
                                                  trim_ws = T,
                                                  skip = 1))
    tables$assays <- as.data.frame(read_excel(path = inFile$datapath,
                                              sheet = 5,
                                              trim_ws = T,
                                              skip = 1))
    tables$literature$Microorganism_scientific_name <- NULL
    tables$assays$Microorganism_scientific_name <- NULL
    tables$total <-full_join(tables$assays, tables$literature)
    tables$total <- full_join(tables$total, tables$taxonomy)
    tables$total <- left_join(tables$total, tables$isolation)
    tables$total <- left_join(tables$total, tables$samples)
    return(tables)
  })
  
  ##### Analyse data and create dataframes for display #####
  
  #Subset the data according to the filters
  data <- eventReactive(c(raw_tables(), input$filter_button), {
    
    #Prevent errors from showing and create empty data structures
    if (is.null(raw_tables())){
      return(NULL)
    }
    data <- list()
    remove_ids <- vector()
    
    #Plant pathogen filtering
    if (input$radio_plant == "No"){
      remove_ids <- subset(raw_tables()$literature, tolower(Pathogenicity_plant) == "yes")$Microorganism_ID
    }
    
    #Human pathogen filtering
    remove_ids <- c(remove_ids,
                    subset(raw_tables()$literature, Pathogen_class > input$slider_patho)$Microorganism_ID)
    
    #PGP function filtering
    for (pgp in input$checkbox_pgp){
      remove_ids <- c(remove_ids, 
                      raw_tables()$assays[tolower(raw_tables()$assays[[pgp]]) == "no",]$Microorganism_ID)
    }
    
    #Fungal biocontrol filtering
    fungi <- c("Pe", "Bo", "Mo", "Geo", "Fu")
    for (fung in fungi){
      filter_id <- paste("slider_", fung, sep = "")
      remove_ids <- c(remove_ids, 
                      raw_tables()$assays[raw_tables()$assays[[fung]] < input[[filter_id]],]$Microorganism_ID)
    }
    
    #Bacterial biocontrol filtering
    bacts <- c("Pse", "Xa", "Agr")
    for (bact in bacts){
      filter_id <- paste("slider_", bact, sep = "")
      remove_ids <- c(remove_ids, 
                      raw_tables()$assays[raw_tables()$assays[[bact]] < input[[filter_id]],]$Microorganism_ID)
    }
    
    #Subset data tables based on the IDs that have been selected for removal
    data$samples <- raw_tables()$samples
    data$isolation <- raw_tables()$isolation[!raw_tables()$isolation$Microorganism_ID %in% remove_ids,]
    data$taxonomy <- raw_tables()$taxonomy[!raw_tables()$taxonomy$Microorganism_ID %in% remove_ids,]
    data$literature <- raw_tables()$literature[!raw_tables()$literature$Microorganism_ID %in% remove_ids,]
    data$assays <- raw_tables()$assays[!raw_tables()$assays$Microorganism_ID %in% remove_ids,]
    
    #Generate a joined table of all relevant information
    data$total <- full_join(data$assays, data$literature, by = c("Microorganism_ID"))
    data$total <- full_join(data$total, data$taxonomy, by = c("Microorganism_ID"))
    data$total <- left_join(data$total, data$isolation, by = c("Microorganism_ID"))
    data$total <- left_join(data$total, data$samples, by = c("Sample_ID"))
    return(data)
  })
  
  plotheight <- reactive({
    if(is.null(data())){
      return(100L)
    }
    height <- 10 + 15 * nrow(data()$total[!is.na(data()$total$Microorganism_ID),])
    return(height)
  })
  
  ##### Render graphs and tables #####
  
  #### Overview Tab ####
  
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
  
  #Plot: Anti-Pse activity
  output$hist_Pse <- renderPlot({
    
    #Prevent errors from showing up
    if(is.null(data())){
      return(NULL)
    }
    
    #Generate the plot
    p <- plot_assay_hist(data(), c("Pse"))
    p
    
  })
  
  #Plot: Anti-Xa activity
  output$hist_Xa <- renderPlot({
    
    #Prevent errors from showing up
    if(is.null(data())){
      return(NULL)
    }
    
    #Generate the plot
    p <- plot_assay_hist(data(), c("Xa"))
    p
    
  })
  
  #Plot: Anti-Agr activity
  output$hist_Agr <- renderPlot({
    
    #Prevent errors from showing up
    if(is.null(data())){
      return(NULL)
    }
    
    #Generate the plot
    p <- plot_assay_hist(data(), c("Agr"))
    p
    
  })
  
  #Plot: Anti-Pe activity
  output$hist_Pe <- renderPlot({
    
    #Prevent errors from showing up
    if(is.null(data())){
      return(NULL)
    }
    
    #Generate the plot
    p <- plot_assay_hist(data(), c("Pe"))
    p
    
  })
  
  #Plot: Anti-Bo activity
  output$hist_Bo <- renderPlot({
    
    #Prevent errors from showing up
    if(is.null(data())){
      return(NULL)
    }
    
    #Generate the plot
    p <- plot_assay_hist(data(), c("Bo"))
    p
    
  })
  
  #Plot: Anti-Mo activity
  output$hist_Mo <- renderPlot({
    
    #Prevent errors from showing up
    if(is.null(data())){
      return(NULL)
    }
    
    #Generate the plot
    p <- plot_assay_hist(data(), c("Mo"))
    p
    
  })
  
  #Plot: Anti-Geo activity
  output$hist_Geo <- renderPlot({
    
    #Prevent errors from showing up
    if(is.null(data())){
      return(NULL)
    }
    
    #Generate the plot
    p <- plot_assay_hist(data(), c("Geo"))
    p
    
  })
  
  #Plot: Anti-Fu activity
  output$hist_Fu <- renderPlot({
    
    #Prevent errors from showing up
    if(is.null(data())){
      return(NULL)
    }
    
    #Generate the plot
    p <- plot_assay_hist(data(), c("Fu"))
    p
    
  })
  
  #### Assays Tab ####
  output$dot_Pse <- renderPlotly({
    #Prevent errors from showing up
    if(is.null(data())){
      return(NULL)
    }
    
    #Render the plot
    p <- plot_assay_dot(data(), raw_tables(), c("Pse"))
    p
    
  })
  
  output$dot_Xa <- renderPlotly({
    #Prevent errors from showing up
    if(is.null(data())){
      return(NULL)
    }
    
    #Render the plot
    p <- plot_assay_dot(data(), raw_tables(), c("Xa"))
    p
    
  })
  
  output$dot_Agr <- renderPlotly({
    #Prevent errors from showing up
    if(is.null(data())){
      return(NULL)
    }
    
    #Render the plot
    p <- plot_assay_dot(data(), raw_tables(), c("Agr"))
    p
    
  })
  
  output$dot_Pe <- renderPlotly({
    #Prevent errors from showing up
    if(is.null(data())){
      return(NULL)
    }
    
    #Render the plot
    p <- plot_assay_dot(data(), raw_tables(), c("Pe"))
    p
    
  })
  
  output$dot_Bo <- renderPlotly({
    #Prevent errors from showing up
    if(is.null(data())){
      return(NULL)
    }
    
    #Render the plot
    p <- plot_assay_dot(data(), raw_tables(), c("Bo"))
    p
    
  })
  
  output$dot_Mo <- renderPlotly({
    #Prevent errors from showing up
    if(is.null(data())){
      return(NULL)
    }
    
    #Render the plot
    p <- plot_assay_dot(data(), raw_tables(), c("Mo"))
    p
    
  })
  
  output$dot_Geo <- renderPlotly({
    #Prevent errors from showing up
    if(is.null(data())){
      return(NULL)
    }
    
    #Render the plot
    p <- plot_assay_dot(data(), raw_tables(), c("Geo"))
    p
    
  })
  
  output$dot_Fu <- renderPlotly({
    #Prevent errors from showing up
    if(is.null(data())){
      return(NULL)
    }
    
    #Render the plot
    p <- plot_assay_dot(data(), raw_tables(), c("Fu"))
    p
    
  })
  
  output$heatmap <- renderPlot({
    #Prevent errors from showing up
    if(is.null(data())){
      return(NULL)
    }
    
    #Render the plot
    p <- plot_heatmap(data())
    p
  })
  
  #Render the plotheight
  output$heatmap.ui <- renderUI({
    return(plotOutput("heatmap", height = plotheight()))
  })
  #### Tables tab ####
  #Render the data tables
  output$table_samples <- renderDataTable(
    {as.data.frame(data()$samples)}
  )
  output$table_isolation <- renderDataTable(
    {as.data.frame(data()$isolation)}
  )
  output$table_taxonomy <- renderDataTable(
    {as.data.frame(data()$taxonomy)}
  )
  output$table_literature <- renderDataTable(
    {as.data.frame(data()$literature)}
  )
  output$table_assays <- renderDataTable(
    {as.data.frame(data()$assays)}
  )
}