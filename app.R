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
  # CSS
  tags$head(
    tags$style(HTML('#addType{background-color:#6baed6'))
    ,tags$style(HTML('#addLookaround{background-color:#6baed6'))
    ,tags$style(HTML('#addLiteral{background-color:#6baed6'))
    ,tags$style(HTML('#remove{background-color:#6baed6'))
    ,tags$style(HTML('#clear{background-color:#3182bd'))
    
  )
  
  # Elements in the side bar
  ,sidebarLayout(
    sidebarPanel(
      # Descriptors well panel
    wellPanel( h5("Descriptors")
      ,p("Valid Values include any number and '0+' or '1+'")
      ,textInput("quantity", "Quantity", value="1")
      ,selectInput("type", "Type", c("number", "alpha", "alphanumeric", "punctuation","upper", "lower", "word", "nonword", "space", "notspace", "anything"),"number")
      ,actionButton("addType", "Add")
      ,style="background-color: #f7f7f7"
      )
      # Literal well panel
    ,wellPanel( h5("Literal")
               ,textInput("literal", "Literal", value="1")
               ,actionButton("addLiteral", "Add")
               ,style="background-color: #f7f7f7"
    )
      # Lookaround well panel
    , wellPanel(h5("Lookarounds")
      ,selectInput("lookaround", "Lookaround", c("followed by", "not followed by", "preceded by", "not preceded by"), "followed by")
      ,textInput("literalla", "Expression", value="")
      ,actionButton("addLookaround", "Add")
      ,style="border-width:5px;border-color:9ecae1"
    )
    ,style = "background-color: #bdbdbd"
  )
  
  # Main panel
  , mainPanel(
      titlePanel("Regex Builder")
      ,wellPanel(
        p("Input:")
        ,textOutput("input")  
        ,style="background-color: #f7f7f7"
      )
      ,actionButton("remove", "Remove")
      ,wellPanel(
        style="border:5px;border-color:9ecae1"
        ,p("Output:")
        ,textOutput("output")  
        
      )
      ,actionButton("clear", "Clear")
  )
  )
)


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
