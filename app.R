library(shiny)
library(shinyWidgets)
library(epiDisplay)
library(summarytools)
library(ggplot2)
library(shinyjs)
library(ggplot2)
library(dplyr)
library(purrr)
library(RColorBrewer)
library(scales)
library(rvg)       # For interactive / tooltips
library(ggiraph)   # For interactive / tooltips



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
      
##########################################################################################################################   
      br(),
      tabsetPanel(type = "tabs",
                  tabPanel("Mean", br(),
                           uiOutput("formula"),
                           

                
                           uiOutput("formula2")
                           #uiOutput("results")
                           ),
 #############################################################################################################################  
               

                  tabPanel("Median", br(), 
                           helpText("Step 1"), 
                           br(),
                           textOutput("order_data"),
                           br(),
                           br(),
                           helpText("Step 2"),
                           helpText("In this step we calculate the middle positions of the dataset"),
                           uiOutput("middle_point"),
                           helpText("Note: middle positions depend on wether data size is an even number or an odd number."),
                           br(),
                           br(),
                           helpText("Step 3"),
                           textOutput("median_result"),
                           textOutput("median_result2"),
                           
                           br(),
                           br(),
                           
                           br(),
                           ),
                  
                  
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
  output$formula <- renderUI({
    numbers <- extract(input$breaks)
    s=sum(numbers)
    
    if( input$variable=="ss"){
      
      withMathJax(
        helpText(h3('Sample mean formula $$\\overline{x} = \\frac{\\Sigma x_i}{n}$$')))
      
    }else{
      
      withMathJax(
        helpText(h3('Population mean formula $$\\mu = \\frac{\\Sigma x}{N}$$')))}
    
  })
  
  output$formula2 <- renderUI({
    numbers <- extract(input$breaks)
    s=sum(numbers)
    n=length(numbers)
    me=round(mean(numbers),2)
    if(n==0){print("No Data yet")}
    else if(input$variable!="ss"){wellPanel(
      
    h2(  withMathJax(sprintf("$$ \\mu= \\frac{%.2f}{%.2f}$$", 
                          s, n )) ),
    h2(  withMathJax(sprintf("$$ \\mu= %.2f$$", 
                            me)) )

    )}else{
      wellPanel(
        h2(  withMathJax(sprintf("$$ \\overline{x} = \\frac{%.2f}{%.2f}$$", 
                                s, n )) ),   
        h2(  withMathJax(sprintf("$$ \\overline{x} = %.2f $$", 
                                                  me )) ))   }
    })
  
  
  
  output$sum <- renderUI({
    numbers <- extract(input$breaks)
    me=sum(numbers)
    if (anyNA(numbers)){}else{numericInput("obs2", "Sum", value = me)}
  })
  
  output$sizee <- renderUI({
    numbers <- extract(input$breaks)
    n=length(numbers)
    if (anyNA(numbers)){}else{numericInput("obs3", "Size", value = n)}
  })

  output$results <- renderUI({
    numbers <- extract(input$breaks)
    me=round(mean(numbers),2)
    if (anyNA(numbers)){}else{numericInput("obs4", "Mean", value = me)}
  })
  
  


#############################################################################################################################  
  
  
  #############################################################################################################################  
  ##################################################################################################################################### 
  
  output$mode_result <- renderText({
    
    nums <- extract(input$breaks)
    mmode <- mode1(nums)
    
    if (anyNA(mmode)) {
      "No Data"
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

    
output$middle_point <- renderUI({
  
  nums <- extract(input$breaks)
  n <- length(nums)
  odn=0.5*(n+1)
  ed=c(0.5*n,0.5*n+1)
  
  if(n==0){print("  ")} else
  if(input$variable!="ss" & n%%2!=0 ){
        wellPanel(
          helpText(withMathJax("middle position formula$$\\frac{N+1}{2}$$ ")),
          withMathJax(sprintf("$$\\frac{%.2f+1}{2}=%.2f$$ ", n, odn)),
          paste(c("The Middle Position is", paste(odn, collapse = ", ")), collapse = " "))
          
  }else if( input$variable=="ss" & n%%2!=0){
    wellPanel(
      helpText(withMathJax("middle position formula$$\\frac{n+1}{2}$$ ")),
      withMathJax(sprintf("$$\\frac{%.2f+1}{2}=%.2f$$ ", n, odn)),
      paste(c("The Middle Position is", paste(odn, collapse = ", ")), collapse = " "))
      }
    else if( input$variable=="ss" & n%%2==0 ){
      wellPanel(
     
          helpText(withMathJax("middle position formula$$\\frac{n+1}{2}$$ ")),
          withMathJax(sprintf("$$\\frac{%.2f+1}{2}=%.2f$$ ", n, odn)),
          paste(c("Middle positions are (", ed[1],",",ed[2],")"), collapse = " "))
        
    }
    else if(input$variable!="ss" & n%%2==0){
    wellPanel(
      helpText(withMathJax("middle position formula$$\\frac{N+1}{2}$$ ")),
      withMathJax(sprintf("$$\\frac{%.2f+1}{2}=%.2f$$ ", n, odn)),
      paste(c("Middle positions are (", ed[1],",",ed[2],")"), collapse = " "))}
  
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
  output$median_result2 <- renderText({
    
    nums <- extract(input$breaks)
    n <- length(nums)
    p1 <- n/2
    if(n==0){
      " "
    }
    })
  
  #####################################################################################################################################   

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