#Overview
ui_body_overview <- function(){
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
          #Donutplots of pathogenicity and PGP
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
            tabBox(title = "Bacteria vs. bacterial pathogens",
                   id = "inhibition_bact_bact",
                   width = 12,
                   tabPanel("Histogram",
                            plotOutput("hist_bact_bact", height = 150)),
                   tabPanel("Samples",
                            plotOutput("dot_bact_bact", height = 150))
            ),
            tabBox(title = "Bacteria vs. fungal pathogens",
                   id = "inhibition_bact_fungi",
                   width = 12,
                   tabPanel("Histogram",
                            plotOutput("hist_bact_fungi", height = 150)),
                   tabPanel("Samples",
                            plotOutput("dot_bact_fungi", height = 150))
            )
          ),
          fluidRow(
            tabBox(title = "Fungi vs. bacterial pathogens",
                   id = "inhibition_fungi_bact",
                   width = 12,
                   tabPanel("Histogram",
                            plotOutput("hist_fungi_bact", height = 150)),
                   tabPanel("Samples",
                            plotOutput("dot_fungi_bact", height = 150)
                            )
            ),
            tabBox(title = "Fungi vs. fungal pathogens",
                   id = "inhibition_fungi_fungi",
                   width = 12,
                   tabPanel("Histogram",
                            plotOutput("hist_fungi_fungi", height = 150)),
                   tabPanel("Samples",
                            plotOutput("dot_fungi_fungi", height = 150)
                            )
            )
            )
  )
}