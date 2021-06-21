library(shiny)
library(shinyWidgets)
library(epiDisplay)
library(summarytools)
library(ggplot2)
library(shinyjs)


mode1 <- function(dat){
  freq= table(dat)
  if(max(freq)==1){
    return("No Mode")
  }else{
  return(as.numeric(names(freq)[which.max(freq)]))
  }
}

ui <- fluidPage(
  
  br(),

  titlePanel(h1("Psy Stat 101")),
  br(),
  br(),
  titlePanel(h3("Central Tendency")),
  br(),
  
  sidebarLayout(
    sidebarPanel(
     helpText( " Please enter your data"),
     textInput("breaks", "", 
               placeholder = "Enter values separated by a comma (,)"),
     selectInput("variable", "Type:",
                 c("Population" = "pp",
                   "Sample" = "ss")),
     ),

      mainPanel(
        br(),
        br(),
      textOutput("breakresult"), 
      br(),
#############################################################################################################################   
      br(),
      tabsetPanel(type = "tabs",
                  tabPanel("Mean", br(),
                           helpText("Type in your work "),
                           wellPanel(
                             br(),
                             uiOutput("formula"),
                             br(),
                             numericInput("myInput3", label = "Sum of Data", value = NULL, width = 800),
                             numericInput("myInput4", label = "Data Size", value = NULL, width = 160),
                             uiOutput("ui3", width = 800),
                             br(), 
                             
                             
                             br()),br(),br(),br(),
                              uiOutput("results")),

                  
 #############################################################################################################################  
               

                  tabPanel("Median", br(), 
                           helpText("Step 1"), 
                           br(),
                           textOutput("order_data"),
                           br(),
                           br(),
                           helpText("Step 2"),
                           textOutput("middle_point"),
                           helpText("Note: middle position depends on wether data size is an even number or an odd number."),
                           br(),
                           br(),
                           helpText("Step 3"),
                           textOutput("median_result")),
                  
                  
                  tabPanel("Mode", br(),
                           helpText("Create a frequency table for your dataset"), 
                           br(),
                           tableOutput("freq"),
                           br(),
                           textOutput("mode_result"),
                           br(),
                           
                           helpText("Note: if frequencies are equal to 1 then there is no mode for this dataset."))
                           
#############################################################################################################################  

)
)
)
)


server <- function(input, output, session) {
  
  data <- reactive({
    nums <- extract(input$breaks)
    data <- data.frame(x = c(nums),
                       sx = sum(nums),
                       size=length(nums),
                       y1= mean(nums),
                       y2= median(nums),
                       y3= mode1(nums))
    data
  })
  
  extract <- function(text) {
    text <- gsub(" ", "", text)
    split <- strsplit(text, ",", fixed = FALSE)[[1]]
    as.numeric(split)
  }
  
 
  v <- reactiveValues(data = NULL)
  observeEvent(input$ans_box, {
    v$data <- extract(input$breaks)
  })  
  
  observeEvent(input$ans_box, {
    shinyjs::toggle(output$results)
  })
  
  #############################################################################################################################  
   output$results <- renderUI({
     numbers <- extract(input$breaks)
     me=mean(numbers)
     
     if (anyNA(input$obs2)) {
       "No data or work entered yet, please enter data or work." 
     } else if( input$variable=="ss"& (!is.na(input$myInput3))){ 
         paste(c("Sample Mean directly from the dataset is", paste(me, collapse = ", ")), collapse = " ")
       } else if ( input$variable!="ss" & (!is.na(input$myInput3))){
       paste(c("Population Mean directly from the dataset is", paste(me, collapse = ", ")), collapse = " ")
       } else if ( input$variable!="ss" & (is.na(input$myInput3))){
         "No work entered yet, please enter your work." }
     
   })

    output$formula <- renderUI({
    if( input$variable=="ss"){
      withMathJax(
        helpText('Sample mean formula $$\\overline{x} = \\frac{\\Sigma x_i}{n}$$'))}
    else{withMathJax(
      helpText('Population mean formula $$\\mu = \\frac{\\Sigma x}{N}$$'))}
  })

  
  output$ui3 <- renderUI( {
    C <- input$myInput3
    D <- input$myInput4
    if(input$variable=="ss"){numericInput("obs2", "Sample mean", value = C/D)}
    else{numericInput("obs2", "Population mean", value = C/D)}
  })

  #############################################################################################################################  
  
  
  output$value2 <- renderText({    
    numbers <- extract(input$breaks)
    me=mean(numbers)
    if (anyNA(numbers)) {
      "No data yet, please enter data"
    } else {
      paste(summary(numbers))
    }
  })
  
  #############################################################################################################################  
  #############################################################################################################################  
  ##################################################################################################################################### 
  
  output$mode_result <- renderText({
    
    nums <- extract(input$breaks)
    mmode <- mode1(nums)
    
    if (anyNA(mmode)) {
      "No data yet, please enter data"
    } else {
      paste(c("Mode is", paste(mmode, collapse = ", ")), collapse = " ")
      
      
    }
  })
    output$freq = renderTable({
    numbers <- extract(input$breaks)
    
    ftable(numbers)
  })
    
    #####################################################################################################################################  
  output$order_data <- renderText({
    
    nums <- extract(input$breaks)
    or <- nums[order(nums)]
    paste(c("Order data acendingly", paste(or, collapse = ", ")), collapse = " ")

  })
  
  output$middle_point <- renderText({
    
    nums <- extract(input$breaks)
    n <- length(nums)
    if(n%%2!=0){
      if ( n<0 ) {
        "No data yet, please enter data"
      } else {
        paste(c("Middle position is", paste(0.5*(n+1), collapse = ", ")), collapse = " ")}
      }else{
        if (n<0) {
          "No data yet, please enter data"
        } else {
          paste(c("Middle position is",paste( c(0.5*n, 0.5*n+1), collapse = ", ")), collapse = " ")
    }
    
      }
    
  })
  
  output$median_result <- renderText({
    
    nums <- extract(input$breaks)
    mmedian <- median(nums)
    
    if (anyNA(mmedian)) {
      "No data yet, please enter data"
    } else {
      paste(c("Median is", paste(mmedian, collapse = ", ")), collapse = " ")
      
      
    }
  })
  
  #####################################################################################################################################   
  

  output$ui3 <- renderUI( {
    C <- input$myInput3
    D <- input$myInput4
    if(input$variable=="ss"){numericInput("obs2", "Sample mean from your work", value = C/D)}
    else{numericInput("obs2", "Population mean from your work", value = C/D)}
  })

  
  #####################################################################################################################################  
  
  output$breakresult <- renderText({
    
    nums <- extract(input$breaks)
    
    if (anyNA(nums)) {
      "Invalid input"
    } else {
      paste(c("Dataset Display:", paste(nums, collapse = ", ")), collapse = " ")
      
      
    }
  })
  

  #####################################################################################################################################   

}

shinyApp(ui = ui, server = server)
