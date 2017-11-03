## Title:   UI for shiny dashboard for collection analysis
## Date:    19-oct-2017
## Author:  Guus Martijn Teunisse (gmteunisse@gmail.com)
##########################################################################

##### Shiny UI ####

#Define the shiny UI
ui <- dashboardPage(
  dashboardHeader(title = "UCDC collection"),
  ui_sidebar(),
  ui_body()
)