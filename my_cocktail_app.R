packages <- "shiny"
install.packages(setdiff(packages, rownames(installed.packages())))
library(shiny)
run <- function(){
  runApp(temp)}

temp <- tempfile(fileext = ".R")
download.file("https://raw.githubusercontent.com/aimeokoko/cocktail/main/cocktail_app.R", 
              temp)

run()
