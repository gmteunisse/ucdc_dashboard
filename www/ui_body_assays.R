#Assays
ui_body_assays <- function(){
  tabItem(tabName = "assays",
          fluidRow(
            box(
              title = "Bacterial samples",
              solidHeader = T,
              collapsible = T,
              uiOutput("heatmap_bact.ui"),
              width = 6
            ),
            box(
              title = "Fungal samples",
              solidHeader = T,
              collapsible = T,
              uiOutput("heatmap_fungi.ui"),
              width = 6
            )
          )
  )
}