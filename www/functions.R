## Title:   Functions for shiny dashboard for collection analysis
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
