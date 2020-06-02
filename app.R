# Author: Iliass Benali
# Date: 2/06/2020
# Description: This is a project capstone for the following course:
#       https://www.coursera.org/learn/data-science-project
#

library(shiny)
library(shinythemes)
library(markdown)
library(dplyr)
library(tm)

# UI
ui <- shinyUI(
    navbarPage("The Next Word Predictor",
               theme = shinytheme("united"),
               tabPanel("Application",
                        fluidPage(
                            titlePanel("App:"),
                            sidebarLayout(
                                sidebarPanel(
                                    textInput("userInput",
                                              "Enter  a phrase (multiple words):",
                                              value =  "",
                                              placeholder = "Enter your phrase here"),
                                    br(),
                                    sliderInput("numPredictions", "Number of Predictions:",
                                                value = 1.0, min = 1.0, max = 3.0, step = 1.0)
                                ),
                                mainPanel(
                                    h4("Your Input"),
                                    verbatimTextOutput("userSentence"),
                                    br(),
                                    h4("Predicted words"),
                                    verbatimTextOutput("prediction1"),
                                    verbatimTextOutput("prediction2"),
                                    verbatimTextOutput("prediction3")
                                )
                            )
                        )
               ),
               tabPanel("Info",
                        h3("About This App"),
                        br(),
                        div("The Next Word Predictor is a Shiny app that uses a text
                            prediction algorithm to predict the next word or words
                            based on phrase entered by the user.",
                            br(),
                            br(),
                            " The App will detect that you have finished typing then
                            it display the predicted word(s). Please wait a few
                            seconds for the output to appear.",
                            br(),
                            br(),
                            "Use the slider bar to select up to three next
                            word predictions. The top prediction will be
                            shown first followed by the second and third likely
                            next words.",
                            
                            br(),
                            br(),
                            
                            a(target = "_blank", href = "https://github.com/Bnilss/data-science-coursera-capstone",
                              "The Source code")),
                        br(),
                        
                        
                        
               )
    )
)

# server logic
server <- function(input, output) {

    
    # load the n-gram frequencies generated through "ngram-frequencies.R"
    initialPrediction <- readRDS("./data/start-word-prediction.RData")
    freq2ngram <- readRDS("./data/bigram.RData")
    freq3ngram <- readRDS("./data/trigram.RData")
    freq4ngram <- readRDS("./data/quadgram.RData")
    
    
    # load bad words file
    badWordsFile <- "./data/full-list-of-bad-words_text-file_2018_07_30.txt"
    con <- file(badWordsFile, open = "r")
    badwds <- readLines(con, encoding = "UTF-8", skipNul = TRUE)
    badwds <- iconv(badwds, "latin1", "ASCII", sub = "")
    close(con)
    
    predictionMatch <- function(userInput, ngrams) {
        
        # >= quadgram
        if (ngrams > 3) {
            userInput3 <- paste(userInput[length(userInput) - 2],
                                userInput[length(userInput) - 1],
                                userInput[length(userInput)])
            dataTokens <- freq4ngram %>% filter(variable == userInput3)
            if (nrow(dataTokens) >= 1) {
                return(dataTokens$outcome[1:3])
            }
            
            # backoff to trigram
            return(predictionMatch(userInput, ngrams - 1))
        }
        
        # trigram
        if (ngrams == 3) {
            userInput1 <- paste(userInput[length(userInput)-1], userInput[length(userInput)])
            dataTokens <- freq3ngram %>% filter(variable == userInput1)
            if (nrow(dataTokens) >= 1) {
                return(dataTokens$outcome[1:3])
            }
            # backoff to bigram
            return(predictionMatch(userInput, ngrams - 1))
        }
        
        # <= bigram
        if (ngrams < 3) {
            userInput1 <- userInput[length(userInput)]
            dataTokens <- freq2ngram %>% filter(variable == userInput1)
            return(dataTokens$outcome[1:3])
        }
        
        # for performance reasons the unigram was not implemented
        return(NA)
    }
    
    cleanInput <- function(input) {
        
        
        if (input == "" | is.na(input)) {
            return("")
        }
        
        input <- tolower(input)
        
        # remove URL, email addresses, Twitter handles and hash tags
        input <- gsub("(f|ht)tp(s?)://(.*)[.][a-z]+", "", input, ignore.case = FALSE, perl = TRUE)
        input <- gsub("\\S+[@]\\S+", "", input, ignore.case = FALSE, perl = TRUE)
        input <- gsub("@[^\\s]+", "", input, ignore.case = FALSE, perl = TRUE)
        input <- gsub("#[^\\s]+", "", input, ignore.case = FALSE, perl = TRUE)
        
        # remove ordinal numbers
        input <- gsub("[0-9](?:st|nd|rd|th)", "", input, ignore.case = FALSE, perl = TRUE)
        
        # remove bad words
        input <- removeWords(input, badwds)
        
        # remove punctuation
        input <- gsub("[^\\p{L}'\\s]+", "", input, ignore.case = FALSE, perl = TRUE)
        
        # remove punctuation (leaving ')
        input <- gsub("[.\\-!]", " ", input, ignore.case = FALSE, perl = TRUE)
        
        # trim leading and trailing whitespace
        input <- gsub("^\\s+|\\s+$", "", input)
        input <- stripWhitespace(input)
        
        
        
        if (input == "" | is.na(input)) {
            return("")
        }
        
        input <- unlist(strsplit(input, " "))
        
        return(input)
        
    }
    
    predictNextWord <- function(input, word = 0) {
        
        input <- cleanInput(input)
        
        if (input[1] == "") {
            output <- initialPrediction
        } else if (length(input) == 1) {
            output <- predictionMatch(input, ngrams = 2)
        } else if (length(input) == 2) {
            output <- predictionMatch(input, ngrams = 3)
        } else if (length(input) > 2) {
            output <- predictionMatch(input, ngrams = 4)
        }
        
        if (word == 0) {
            return(output)
        } else if (word == 1) {
            return(output[1])
        } else if (word == 2) {
            return(output[2])
        } else if (word == 3) {
            return(output[3])
        }
        
    }
    
    
        
        # original sentence
        output$userSentence <- renderText({input$userInput});
        
        # reactive controls
        observe({
            numPredictions <- input$numPredictions
            if (numPredictions == 1) {
                output$prediction1 <- reactive({predictNextWord(input$userInput, 1)})
                output$prediction2 <- NULL
                output$prediction3 <- NULL
            } else if (numPredictions == 2) {
                output$prediction1 <- reactive({predictNextWord(input$userInput, 1)})
                output$prediction2 <- reactive({predictNextWord(input$userInput, 2)})
                output$prediction3 <- NULL
            } else if (numPredictions == 3) {
                output$prediction1 <- reactive({predictNextWord(input$userInput, 1)})
                output$prediction2 <- reactive({predictNextWord(input$userInput, 2)})
                output$prediction3 <- reactive({predictNextWord(input$userInput, 3)})
            }
        })
        
}

# Run the application 
shinyApp(ui = ui, server = server)
