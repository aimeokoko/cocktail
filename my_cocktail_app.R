ifelse(require(shiny), library(shiny), install.packages(shiny))

run <- function(){
runApp("cocktail_shiny.R")}

temp <- tempfile(fileext = ".R")
download.file("https://raw.githubusercontent.com/aimeokoko/cocktail/main/cocktail_app.R", 
              temp)

run()
