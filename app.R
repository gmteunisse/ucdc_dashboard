## Title:   shiny dashboard for collection analysis
## Date:    19-oct-2017
## Author:  Guus Martijn Teunisse (gmteunisse@gmail.com)
##########################################################################

###### Functions #####

#Loads your packages and installs those that are not yet installed
awesome_package_loader <- function(package_vector){
  for(package_name in package_vector){
    require_code <- paste("require(", package_name, ")", sep="")
    if (!eval(parse(text = require_code))){
      install_code <- paste("install.packages('", package_name, "')", sep="")
      eval(parse(text = install_code))
      eval(parse(text = require_code))
    }
  }
}

#Generates a donut plot for the distribution of "yes", "no" and "not available"
plot_freq <- function(data, variable){
  
  #Count occurrences and make ready for plotting
  df <- data$total
  tbl <- as.data.frame(table(df[[variable]], useNA = "always"), stringsAsFactors = F)
  to_plot <- c("No", "Yes", "Not available")
  if (nrow(tbl) != 3){ #find out which data is missing and add a 0
    
  }
  tbl$Var1 <- factor(x = tbl$Var1,
                     levels = c("Yes", "No", "Not available"),
                     ordered = T)
  
  #Create plot
  p <- ggplot(tbl, aes(x = "", y = Freq, fill = Var1, group = Var1)) +
    geom_bar(stat = "identity", width = 0.3) +
    geom_text(aes(label = Freq), position = position_stack(vjust = 0.5), size = 2) +
    coord_polar("y") +
    scale_fill_manual(values = c(color_theme[1], color_blue[3], color_grey[3]) ) +
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
          legend.position = "top")
  
  return(p)
  
}

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
    geom_text(aes(label = Freq), position = position_stack(vjust = 0.5), size = 2) +
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

##### Session basic #####

#Load packages
awesome_package_loader(c("shiny", "shinydashboard", "readxl", "dplyr", "ggplot2", "plotly"))

#Define plot styling
#Set colors
color_theme <- c("#ff7256", "#c1c1c1", "#b3ecec")
color_blue <- c("#3c598e", "#5077bd", "#6495ed", "#83aaf0", "#a2bff4")
color_orange <- c("#994433", "#cc5b44", "#ff7256", "#ff8e77", "#ffaa99")
color_tan <- c("#8e703c", "#bd9650", "#edbc64", "#f0c983", "#f4d6a2")
color_grey <- c("#737373", "#9a9a9a", "#c1c1c1", "#cdcdcd", "#d9d9d9")

#Set plotting aesthetics
plot_theme <- theme(panel.background = element_rect(fill = 'transparent', colour = NA),
                    plot.background = element_rect(fill = 'transparent', colour = NA),
                    legend.background = element_rect(fill = 'transparent', colour = NA),
                    panel.grid.minor = element_line(colour = NA),
                    panel.grid.major = element_line(colour = adjustcolor('grey', 0.2)),
                    legend.key = element_rect(fill = "white"),
                    legend.key.size = unit(0.4, "cm"),
                    text = element_text(family = "Helvetica", size = 12),
                    panel.grid.major.x = element_blank(),
                    plot.title = element_text(hjust = 0.5),
                    axis.line.x = element_line(colour = 'grey'),
                    axis.line.y = element_line(colour = 'grey'),
                    axis.ticks = element_line(colour = 'grey'))

##### Shiny UI #####

#Define the shiny UI
ui <- dashboardPage(
  dashboardHeader(title = "UCDC collection"),
  
  ##### Sidebar #####
  
  dashboardSidebar(
    
    #Field to obtain input of data table
    fileInput(inputId = "file1",
              label = "Upload your .xlsx table",
              buttonLabel = "Select file",
              placeholder = "No file selected",
              accept = c()),
    
    #Menus
    sidebarMenu(
      menuItem("Overview", tabName = "overview", icon = icon("pie-chart")),
      menuItem("Assays", tabName = "assays", icon = icon("bar-chart")),
      menuItem("Explore", tabName = "explore", icon = icon("area-chart")),
      menuItem("Tables", tabName = "tables", icon = icon("table")),
      
      #Special menu: filtering options to subset data
      menuItem("Filters", tabName = "filters", icon = icon("filter"),
               menuItem("Pathogenicity", tabName = "filter_patho", 
                        sliderInput(inputId = "slider_patho",
                           label = "Human pathogenicity treshold:",
                           value = 4,
                           min = 1,
                           max = 4,
                           ticks = T),
                        radioButtons(inputId = "radio_plant",
                                      label = "Include plant pathogens:",
                                      choices = c("Yes", "No"),
                                      selected = "Yes")
                        ),
               menuItem("Plant growth functions", tabName = "filter_pgp",
                        checkboxGroupInput(inputId = "checkbox_pgp",
                                           label = "PGP functions:",
                                           choices = c("PA", "PS", "SF", "FN"))
                        ),
               menuItem("Fungal biocontrol", tabName= "filter_fungi",
                        sliderInput(inputId = "slider_Pe",
                                    label = "Min. Pe inhibition (%)",
                                    value = 0,
                                    min = 0,
                                    max = 100),
                        sliderInput(inputId = "slider_Bo",
                                    label = "Min. Bo inhibition (%)",
                                    value = 0,
                                    min = 0,
                                    max = 100),
                        sliderInput(inputId = "slider_Mo",
                                    label = "Min. Mo inhibition (%)",
                                    value = 0,
                                    min = 0,
                                    max = 100),
                        sliderInput(inputId = "slider_Geo",
                                    label = "Min. Geo inhibition (%)",
                                    value = 0,
                                    min = 0,
                                    max = 100),
                        sliderInput(inputId = "slider_Fu",
                                    label = "Min. Fu inhibition (%)",
                                    value = 0,
                                    min = 0,
                                    max = 100)
                        ),
               menuItem("Bacterial biocontrol", tabName= "filter_bact",
                        sliderInput(inputId = "slider_Pse",
                                    label = "Min. Pse inhibition (%)",
                                    value = 0,
                                    min = 0,
                                    max = 100),
                        sliderInput(inputId = "slider_Xa",
                                    label = "Min. Xa inhibition (%)",
                                    value = 0,
                                    min = 0,
                                    max = 100),
                        sliderInput(inputId = "slider_Agr",
                                    label = "Min. Agr inhibition (%)",
                                    value = 0,
                                    min = 0,
                                    max = 100)
               ),
               actionButton(inputId = "filter_button", label = "Filter")
      )
    )
  ),
  
  ##### Body #####
  
  dashboardBody(
    tabItems(
      #Display each data table in its own tab.
      tabItem(tabName = "overview",
              fluidRow(
                valueBoxOutput(outputId = "samples_selected"),
                valueBoxOutput(outputId = "bact_selected"),
                valueBoxOutput(outputId = "fungi_selected")
              ),
              
              fluidRow(
                box(
                  title = "16S amplified genera",
                  solidHeader = T,
                  collapsible = T,
                  plotOutput("donut_bact"), 
                  height = 500
                ),
                box(
                  title = "ITS amplified genera",
                  solidHeader = T,
                  collapsible = T,
                  plotOutput("donut_fungi"),
                  height = 500
                ),
                fluidRow(
                  box(
                    title = "Plant pathogen",
                    solidHeader = T,
                    collapsible = T,
                    plotOutput("donut_plant_path"),
                    height = 500
                  )
                )
              )
      ),
      tabItem(tabName = "assays"),
      tabItem(tabName = "exlore"),
      tabItem(tabName = "tables",
              fluidRow(
                tabBox(title = NULL,
                       id = "data_table",
                       width = NULL,
                       tabPanel("Sample information",
                                div(style = "overflow-x: scroll",
                                    dataTableOutput("table_samples")
                                )
                       ),
                       tabPanel("Microorganism isolation and cultivation",
                                div(style = "overflow-x: scroll",
                                    dataTableOutput("table_isolation")
                                )
                       ),
                       tabPanel("Microorganism characterization",
                                div(style = "overflow-x: scroll",
                                    dataTableOutput("table_taxonomy")
                                )
                       ),
                       tabPanel("Literature",
                                div(style = "overflow-x: scroll",
                                    dataTableOutput("table_literature")
                                )
                       ),
                       tabPanel("Functional assays",
                                div(style = "overflow-x: scroll",
                                    dataTableOutput("table_assays")
                                )
                       )
                )
                )
              )
    )
  )
)

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
    data$total <- full_join(data$assays, data$literature)
    data$total <- full_join(data$total, data$taxonomy)
    data$total <- left_join(data$total, data$isolation)
    data$total <- left_join(data$total, data$samples)
    return(data)
  })
  
  ##### Render graphs and tables #####
  
  ### Overview Tab
  
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
  
  ### Tables tab
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

#Run the app
shinyApp(ui, server)