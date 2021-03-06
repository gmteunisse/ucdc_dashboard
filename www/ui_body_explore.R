#Explore
ui_body_explore <- function(){
  tabItem(tabName = "explore",
          fluidRow(
            box(title = "Categorical vs. categorical",
                collapsible = T,
                solidHeader = T,
                width = 4,
                selectInput(inputId = "cat_cat_x",
                            label = "X-category",
                            choices = c("Sample_site",
                                        "Producer",
                                        "Sample_scientific_name",
                                        "Sample_type_primary",
                                        "Microorganism_type",
                                        "Family",
                                        "Genus",
                                        "Pathogenicity_plant",
                                        "Pathogen_class",
                                        "PA",
                                        "PS",
                                        "SF",
                                        "FN"),
                            selected = "Sample_site",
                            multiple = F),
                selectInput(inputId = "cat_cat_y",
                            label = "Grouping",
                            choices = c("Sample_site",
                                        "Producer",
                                        "Sample_scientific_name",
                                        "Sample_type_primary",
                                        "Microorganism_type",
                                        "Family",
                                        "Genus",
                                        "Pathogenicity_plant",
                                        "Pathogen_class",
                                        "PA",
                                        "PS",
                                        "SF",
                                        "FN"),
                            selected = "Genus",
                            multiple = F),
                actionButton(inputId = "cat_cat_button", label = "Plot")
            ),
            box(title = NULL,
                width = 8,
                collapsible = T,
                solidHeader = T,
                plotOutput(outputId = "cat_cat_plot"))
          ),
          fluidRow(
            box(title = "Categorical vs. biocontrol assays",
                collapsible = T,
                solidHeader = T,
                width = 4,
                selectInput(inputId = "cat_con_x",
                            label = "X-category",
                            choices = c("Sample_site",
                                        "Producer",
                                        "Sample_scientific_name",
                                        "Sample_type_primary",
                                        "Microorganism_type",
                                        "Family",
                                        "Genus",
                                        "Pathogenicity_plant",
                                        "Pathogen_class",
                                        "PA",
                                        "PS",
                                        "SF",
                                        "FN"),
                            selected = "Sample_site",
                            multiple = F),
                selectInput(inputId = "cat_con_y",
                            label = "Assay (multiple possible)",
                            choices = c("Pe",
                                        "Bo",
                                        "Mo",
                                        "Geo",
                                        "Fu",
                                        "Pse",
                                        "Xa",
                                        "Agr"),
                            selected = "Pe",
                            multiple = T),
                selectInput(inputId = "cat_con_type",
                            label = "Plot type",
                            choices = c("Box", "Scatter"),
                            selected = "Scatter",
                            multiple = F),
                actionButton(inputId = "cat_con_button", label = "Plot")
            ),
            box(title = NULL,
                width = 8,
                collapsible = T,
                solidHeader = T,
                plotOutput("cat_con_plot"))
          )
  )
}