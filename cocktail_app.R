#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# This app uses data from the website "https://www.1001cocktails.com/"



# Required libraries Mon Apr 19 12:58:15 2021----------

packages <- c("shiny", "dplyr", "rvest", "stringr", "purrr", "wordcloud")
install.packages(setdiff(packages, rownames(installed.packages())))

library(shiny)
library(rvest)
library(dplyr)
library(stringr)
library(purrr)
library(wordcloud)

#Data Mon Apr 19 12:59:14 2021----------
## Can be found on my github "https://github.com/aimeokoko/cocktail

final_cock <- read.csv("https://raw.githubusercontent.com/aimeokoko/cocktail/main/cocktails2.csv")
d <- read.csv("https://raw.githubusercontent.com/aimeokoko/cocktail/main/words.csv")


# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Les bons Cocktails"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            selectInput("group",
                        "Cocktail Category",
                        choices = unique(final_cock$Groupe),
                        selected = "96-grands-classiques"),
            
            selectInput("cock", 
                        "Cocktail", 
                        choices = "")
        ),
        # Show a plot of the generated distribution
        mainPanel(
            
            h4("Have a good drink"),
            uiOutput("img"),
            
            h3("Ingrédients"),
            htmlOutput("ingre"),
           
           h3("Recette"),
           htmlOutput("rec"),
           
           h3("Nuage de mots"),
           plotOutput("word")
           
           
        )
    )
)

# Define server logic required to draw a histogram
server <- function(session, input, output) {
    
    grp <- reactive({final_cock %>% 
            filter(Groupe == input$group) %>% 
            select(Cocktail)})
    
    observeEvent(input$group, {
        choi <- final_cock %>% 
            filter(Groupe == input$group) %>% 
            select(Cocktail)
        updateSelectInput(session, "cock", "Cocktail",
                          choices = choi)
    })

    output$ingre <- renderUI({
        # generate bins based on input$bins from ui.R
        ingred <- final_cock %>% filter(Cocktail == input$cock) %>% 
            select(Ingrédients) %>% reduce(paste)
            HTML(paste(ingred, collapse = "<br/>"))
    })
    output$rec <- renderUI({
        recet <- final_cock %>% filter(Cocktail == input$cock) %>% 
            select(Recette) %>% slice(1) %>% as.character() %>% 
            str_remove_all("\t") %>% str_sub(2,-1) %>% 
            str_replace_all("\n", "<br/> <br/>")
        HTML(recet)
    })
    
    output$word <- renderPlot({
        set.seed(1234)
        wordcloud(words = d$word, freq = d$freq, min.freq = 1,
          max.words=209, random.order=FALSE, rot.per=0.35, 
          colors=brewer.pal(8, "Dark2"))
        }, width = 600, height = 400)
    
    
    output$img <- renderUI({
        link <- final_cock %>% 
            filter(Cocktail == input$cock) %>% 
            select(Images) %>% slice(1) %>% as.character()
        tags$img(src = link)
    })
    
}



# Run the application 
shinyApp(ui = ui, server = server)
