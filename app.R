###########################################################################
# R Regex Builder
# Author: Nellie Ponarul
# Last Updated 11/22/2019
# This file runs the Shiny app regex builder
# Dependencies: stringr
###########################################################################

# I. Program Setup --------------------------------------------------------
require(shiny)
source("regexfunction.R")


# II. UI definition -------------------------------------------------------

ui <- fluidPage(
  # CSS link
  includeCSS("www/style.css"),
  
  # Title row
    fluidRow(
      h1("Regex Builder")
    ),
  
  # Input row
    fluidRow(
      h4("Regular expressions (RegEx) are used to identify patterns in strings. Explore how to create RegEx patterns using the following options."),
      column(4, 
      # Character classes well panel
      wellPanel( h5("Character Classes", class = "inputLabel")
        ,p("Valid quantities include any number and '0+' or '1+'")
        ,textInput("quantity", "Quantity", value="1")
        ,selectInput("type", "Type", c("number", "alpha", "alphanumeric", "punctuation","upper", "lower", "word", "nonword", "space", "notspace", "anything"),"number")
        ,actionButton("addType", "Add")
        )
      ),
    
    column(4, 
      # Literal well panel
      wellPanel( h5("Literal", class = "inputLabel")
                 , p("Use this to add any literal strings")
                 , br()
                 , br()
                 ,textInput("literal", "Literal", value="literal string")
                 , br()
                 , br()
                 , br()
                 ,actionButton("addLiteral", "Add")
              )
    ),
    
    column(4,
      # Lookaround well panel
      wellPanel(h5("Lookarounds", class = "inputLabel")
        , p("Use this to denote patterns that on either end of a string")
        ,selectInput("lookaround", "Lookaround", c("followed by", "not followed by", "preceded by", "not preceded by"), "followed by")
        ,textInput("literalla", "Expression", value="")
        ,actionButton("addLookaround", "Add")
      )
    )
  )
    
  
  # Main panel
  , fluidRow(
    column(10, 
        # input
        wellPanel(
        p("Input:")
        ,textOutput("input")  
      ),

      # output
      wellPanel(
        p("Output:")
        ,textOutput("output")  
        
      )
      

    ),
    
    column(2,
           # remove and clear buttoms
           actionButton("remove", "Remove"),
           actionButton("clear", "Clear")
           )
  
  ))



# III. Server Definition --------------------------------------------------

server <- function(input, output) {
  regexVals <- reactiveValues(elements=c()) #list of inputs
  
  observeEvent(input$addType, {
    regexVals$elements <- c(regexVals$elements, paste0(paste(input$quantity, input$type, sep=","),";"))
  })
  observeEvent(input$addLiteral, {
    regexVals$elements <- c(regexVals$elements, paste0(paste(input$literal, "literal", sep=","),";"))
  })
  
  observeEvent(input$addLookaround, {
    regexVals$elements <- c(regexVals$elements, paste0(paste(input$literalla, input$lookaround, sep=","),";"))
  })
  
  observeEvent(input$remove, {
    regexVals$elements <- head(regexVals$elements, -1)
    
  })
  
  # Render Text will re-render every time the regex values list changes
  output$input <- renderText({
    paste0(regexVals$elements,collapse="")
  })
  
  # converts elements into regex values
  output$output <- renderText({
    regex_dict(paste0(regexVals$elements,collapse=""))
  })
  
  observeEvent(input$clear, {
    regexVals$elements <- c()
  })
}

# Run Shiny app
shinyApp(ui=ui,server=server)
