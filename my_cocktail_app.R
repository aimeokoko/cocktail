ifelse(require(shiny), library(shiny), install.packages(shiny))

run <- function(){
runApp("cocktail_shiny.R")}

download.file("https://raw.githubusercontent.com/aimeokoko/cocktail/main/cocktail_app.R", "cocktail_shiny.R")

run()
