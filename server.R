## Title:   Server for shiny dashboard for collection analysis
## Date:    19-oct-2017
## Author:  Guus Martijn Teunisse (gmteunisse@gmail.com)
##########################################################################

#Define the shiny server
server <- function(input, output, session) {
  
  #Obtain and read input data from the user
  raw_tables <- read_raw_tables(input, output, session)
  
  ##### Analyse data and create dataframes for display #####
  #Subset the data according to the filters
  data <- subset_tables(input, output, session, raw_tables)
  
  ##### Render graphs and tables #####
  #### Overview Tab ####
  server_overview_boxes(input, output, server, data)
  server_overview_genera(input, output, server, data)
  server_overview_donuts(input, output, server, data)
  server_overview_histograms(input, output, server, data)
  server_overview_scatterplots(input, output, server, data, raw_tables)
  
  #### Assays Tab ####
  server_assays_heatmap(input, output, server, data)
  
  #### Explore tab ####
  cat_cat_plot <- server_explore_cat_cat(data, input)
  output$cat_cat_plot <- renderPlot({
    cat_cat_plot()
  })
  
  cat_con_plot <- server_explore_cat_con(data, input)
  output$cat_con_plot <- renderPlot({
    cat_con_plot()
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
  
  #Stop session after exiting
  session$onSessionEnded(function() {
    stopApp()
  })
}