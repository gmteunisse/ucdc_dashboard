#Body
ui_body <- function(){
  dashboardBody(
    tabItems(
      #### UI: Overview ####
      ui_body_overview(),
      #### UI: Assays ####
      ui_body_assays(),
      #### UI: Explore ####
      ui_body_explore(),
      #### UI: Tables ####
      ui_body_tables()
    )
  )
}