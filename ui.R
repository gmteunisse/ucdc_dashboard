## Title:   UI for shiny dashboard for collection analysis
## Date:    19-oct-2017
## Author:  Guus Martijn Teunisse (gmteunisse@gmail.com)
##########################################################################

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
      #### UI: Overview ####
      tabItem(tabName = "overview",
              #Boxes with values
              fluidRow(
                valueBoxOutput(outputId = "samples_selected"),
                valueBoxOutput(outputId = "bact_selected"),
                valueBoxOutput(outputId = "fungi_selected")
              ),
              #Genera distributions
              fluidRow(
                box(
                  title = "16S amplified genera",
                  solidHeader = T,
                  collapsible = T,
                  plotOutput("donut_bact", height = 300L), 
                  width = 6
                ),
                box(
                  title = "ITS amplified genera",
                  solidHeader = T,
                  collapsible = T,
                  plotOutput("donut_fungi", height = 300L),
                  width = 6
                )
              ),
              #Distributions of pathogenicity and PGP
              fluidRow(
                box(
                  title = "Plant pathogen",
                  solidHeader = T,
                  collapsible = T,
                  plotOutput("donut_plant_path", height = 200L),
                  width = 2
                ),
                box(
                  title = "Human pathogen",
                  solidHeader = T,
                  collapsible = T,
                  plotOutput("donut_human_path", height = 200L), 
                  width = 2
                ),
                box(
                  title = "PGP activity: PA",
                  solidHeader = T,
                  collapsible = T,
                  plotOutput("donut_PA", height = 200L), 
                  width = 2
                ),
                box(
                  title = "PGP activity: PS",
                  solidHeader = T,
                  collapsible = T,
                  plotOutput("donut_PS", height = 200L), 
                  width = 2
                ),
                box(
                  title = "PGP activity: SF",
                  solidHeader = T,
                  collapsible = T,
                  plotOutput("donut_SF", height = 200L), 
                  width = 2
                ),
                box(
                  title = "PGP activity: FN",
                  solidHeader = T,
                  collapsible = T,
                  plotOutput("donut_FN", height = 200L), 
                  width = 2
                )
              ),
              
              #Distribution of inhibition assays
              fluidRow(
                tabBox(title = "Inhibition: Pse",
                  id = "inhibition_pse",
                  width = 3,
                  tabPanel("Histogram",
                           plotOutput("hist_Pse", height = 150)),
                  tabPanel("Samples",
                           plotlyOutput("dot_Pse", height = 150))
                ),
                tabBox(title = "Inhibition: Xa",
                       id = "inhibition_xa",
                       width = 3,
                       tabPanel("Histogram",
                                plotOutput("hist_Xa", height = 150)),
                       tabPanel("Samples",
                                plotlyOutput("dot_Xa", height = 150))
                ),
                tabBox(title = "Inhibition: Agr",
                       id = "inhibition_agr",
                       width = 3,
                       tabPanel("Histogram",
                                plotOutput("hist_Agr", height = 150)),
                       tabPanel("Samples",
                                plotlyOutput("dot_Agr", height = 150))
                ),
                tabBox(title = "Inhibition: Pe",
                       id = "inhibition_pe",
                       width = 3,
                       tabPanel("Histogram",
                                plotOutput("hist_Pe", height = 150)),
                       tabPanel("Samples",
                                plotlyOutput("dot_Pe", height = 150))
                ),
                tabBox(title = "Inhibition: Bo",
                       id = "inhibition_bo",
                       width = 3,
                       tabPanel("Histogram",
                                plotOutput("hist_Bo", height = 150)),
                       tabPanel("Samples",
                                plotlyOutput("dot_Bo", height = 150))
                ),
                tabBox(title = "Inhibition: Mo",
                       id = "inhibition_mo",
                       width = 3,
                       tabPanel("Histogram",
                                plotOutput("hist_Mo", height = 150)),
                       tabPanel("Samples",
                                plotlyOutput("dot_Mo", height = 150))
                ),
                tabBox(title = "Inhibition: Geo",
                       id = "inhibition_geo",
                       width = 3,
                       tabPanel("Histogram",
                                plotOutput("hist_Geo", height = 150)),
                       tabPanel("Samples",
                                plotlyOutput("dot_Geo", height = 150))
                ),
                tabBox(title = "Inhibition: Fu",
                       id = "inhibition_fu",
                       width = 3,
                       tabPanel("Histogram",
                                plotOutput("hist_Fu", height = 150)),
                       tabPanel("Samples",
                                plotlyOutput("dot_Fu", height = 150))
                )
              )
      ),
      #### UI: Assays ####
      tabItem(tabName = "assays",
              fluidRow(
                       box(
                         title = "Assay performance per organism",
                         solidHeader = T,
                         collapsible = T,
                         uiOutput("heatmap.ui"),
                         width = 12
                         )
                       )
      ),
      #### UI: Explore ####
      tabItem(tabName = "exlore"),
      #### UI: Tables ####
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