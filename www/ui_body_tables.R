#Tables
ui_body_tables <- function(){
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
}