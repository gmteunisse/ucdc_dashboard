#Sidebar
ui_sidebar <- function(){
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
                                    label = "Min. Pse inhibition (mm)",
                                    value = 0,
                                    min = 0,
                                    max = 20),
                        sliderInput(inputId = "slider_Xa",
                                    label = "Min. Xa inhibition (mm)",
                                    value = 0,
                                    min = 0,
                                    max = 20),
                        sliderInput(inputId = "slider_Agr",
                                    label = "Min. Agr inhibition (mm)",
                                    value = 0,
                                    min = 0,
                                    max = 20)
               ),
               menuItem("Organism type", tabName = "filter_type",
                        checkboxGroupInput(inputId = "checkbox_type",
                                           label = "Microorganism type",
                                           choices = c("Prokaryotes (16S)", "Fungi (ITS)"),
                                           selected = c("Prokaryotes (16S)", "Fungi (ITS)")),
                        checkboxGroupInput(inputId = "checkbox_xphyte",
                                           label = "Microorganism growth",
                                           choices = c("Epiphytic", "Endophytic"),
                                           selected = c("Epiphytic", "Endophytic"))
               ),
               actionButton(inputId = "filter_button", label = "Filter")
      )
    )
  )
}