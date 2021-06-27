library(shiny)
library(DT)
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

library(plyr)
library(lubridate)
options(shiny.transcode.json = FALSE)


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
  titlePanel(h3("Range and Variability")),
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
                  tabPanel("Range", br(),
                           uiOutput("formula2")
                 ),
                  #############################################################################################################################  
                  
                  
                  tabPanel("Variance and Standard Deviation", br(), 
                           helpText("Step 1"), 
                           br(),
                           helpText("Compute the mean"),
                           uiOutput("formula"),
                           br(),
                           br(),
                           helpText("Step 2"),
                           textOutput("sp3"),
                           tableOutput("t1"),
                           br(),
                           helpText("Step 3"),
                           textOutput("sp4"),
                           tableOutput("t2"),
                           br(),
                           helpText("Step 4"),
                           textOutput("sp"),
                           uiOutput("formula4"),
                           tableOutput("t3"),
                           
                           br(),
                           helpText("Step 5"),
                           textOutput("sp2"),
                           uiOutput("formula3")
                            ),
                 
                 tabPanel(" ", br(), 
                          tableOutput("table"),
                          withMathJax()
                 )
                  #############################################################################################################################  
                  
      )
    )
  )
)


server <- function(input, output, session) {

  

  
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
  
  datos=reactive({
    x <- extract(input$breaks)
    mean_deviation_scores <- x-mean(x)
    
    cbind( x, mean_deviation_scores)
  })  
  
  output$t1 <- renderTable({
    datos()
    
  })
  
  datos2=reactive({
    x <- extract(input$breaks)
    mean_deviation_scores <- x-mean(x)
    squared_mean_deviations <- mean_deviation_scores^2
    cbind(x, mean_deviation_scores, squared_mean_deviations)
  })  
  
  output$t2 <- renderTable({
    datos2() },colnames = T)
  
  output$t3 <- renderTable({
    datos2() },colnames = T)
  
  ###########################################################################################################################
  output$sp <- renderText({
    if(input$variable!="ss"){ 
      print("Compute the Population variance $$\\sigma^2=\\frac{\\Sigma(x-\\mu)^2}{N}$$")}
    else{
      print("Compute the Sample variance $$s^2=\\frac{\\Sigma(x-\\overline{x})^2}{n-1}$$")}
  })
  
  output$sp2 <- renderText({
    if(input$variable!="ss"){
      print("Compute the Population standard deviation $$\\sigma=\\sqrt(\\sigma^2)=\\sqrt(\\frac{\\Sigma(x-\\mu)^2}{N})$$")}
    else{
      print("Compute the Sample standard deviation $$s=\\sqrt(s^2)=\\sqrt(\\frac{\\Sigma(x-\\overline{x})^2}{n-1})$$")}
  })
  
  output$sp3 <- renderText({
    if(input$variable!="ss"){
      print("Compute the mean deviation scores $$(X-\\mu)$$")}
    else{ print("Compute the mean deviation scores $$(X-\\overline{x})$$")}
  })
  
  output$sp4 <- renderText({
    if(input$variable!="ss"){
     print("Compute squared mean deviations $$(X-\\mu)^2$$")}
    else{
      print("Compute squared mean deviations $$(X-\\overline{x})^2$$")}
  })
  
  
  #############################################################################################################################  
  
  output$formula2 <- renderUI({
    numbers <- extract(input$breaks)
    ma=max(numbers)
    mi=min(numbers)
    
    if(input$variable!="ss"){
      wellPanel(
      div(style="display:inline-block;vertical-align:top; width: 340px;",
         h3("Population Range = Max - Min")),
      div(style="display:inline-block;vertical-align:top; width:30;",withMathJax(h3("$$=$$"))),
      div(style="display: inline-block;vertical-align:top; width: 100px;",
      numericInput("a","",ma)),
      div(style="display:inline-block;vertical-align:top; width: 30;",h3("$$-$$")),
      div(style="display: inline-block;vertical-align:top; width: 100px;",
          numericInput("b","",mi)),
      div(style="display:inline-block;vertical-align:top; width: 30;",h3("$$=$$")),
      div(style="display: inline-block;vertical-align:top; width: 100px;",
          numericInput("c","",ma-mi))
      )
    }else{
      wellPanel(
        div(style="display:inline-block;vertical-align:top; width: 320px;",
            h3('Sample Range = Max - Min')),
        div(style="display:inline-block;vertical-align:top; width:30;", withMathJax(h3("$$=$$"))),
        div(style="display: inline-block;vertical-align:top; width: 100px;",
            numericInput("a","",ma)),
        div(style="display:inline-block;vertical-align:top; width: 30;",h3("$$-$$")),
        div(style="display: inline-block;vertical-align:top; width: 100px;",
            numericInput("b","",mi)),
        div(style="display:inline-block;vertical-align:top; width: 30;",h3("$$=$$")),
        div(style="display: inline-block;vertical-align:top; width: 100px;",
            numericInput("c","",ma-mi))
      )
      
    }
  })
  
  output$formula <- renderUI({
    if(input$variable!="ss"){wellPanel(
      div(style="display:inline-block;vertical-align:top; width: 60px;",withMathJax(h3("$$\\mu=$$"))), 
      
      div(style="display: inline-block;vertical-align:top; width: 150px;",
          uiOutput("sum")),
      
      div(style="display:inline-block;vertical-align:top; width: 60;",h3("$$\\div$$")),
      
      div(style="display: inline-block;vertical-align:top; width: 150px;",
          uiOutput("sizee")),
      div(style="display:inline-block;vertical-align:top; width: 60px;",h3("=")),
      div(style="display:inline-block;vertical-align:top; width: 150px;",uiOutput("results"))
    )}else{
      wellPanel(
        div(style="display:inline-block;vertical-align:top; width: 60px;",withMathJax(h3("$$\\overline{x}=$$"))), 
        
        div(style="display: inline-block;vertical-align:top; width: 150px;",
            uiOutput("sum")),
        
        div(style="display:inline-block;vertical-align:top; width: 60;",h3("$$\\div$$")),
        
        div(style="display: inline-block;vertical-align:top; width: 150px;",
            uiOutput("sizee")),
        div(style="display:inline-block;vertical-align:top; width: 60px;",h3("=")),
        div(style="display:inline-block;vertical-align:top; width: 150px;",uiOutput("results")))
      
    }
  })
  
  output$formula4 <- renderUI({
    numbers <- extract(input$breaks)
    me=mean(numbers)
    tp=sum((numbers-me)^2)
    bt=length(numbers)
    bt2=bt-1
    spk=round(tp/bt,2)
    
    if(input$variable!="ss"){
      wellPanel(
        div(style="display:inline-block;vertical-align:top; width: 60px;",withMathJax(h3("$$=$$"))),
      div(style="display: inline-block;vertical-align:top; width: 150px;",
          numericInput("e","",tp)),
      
      div(style="display:inline-block;vertical-align:top; width: 60;",h3("$$\\div$$")),
      
      div(style="display: inline-block;vertical-align:top; width: 150px;",
          numericInput("f","",bt)),
      div(style="display:inline-block;vertical-align:top; width: 60px;",h3("$$=$$")),
      div(style="display:inline-block;vertical-align:top; width: 150px;",numericInput("g","",spk))
    )
      }else{
      wellPanel(
        div(style="display:inline-block;vertical-align:top; width: 60px;",withMathJax(h3("$$=$$"))),
        div(style="display: inline-block;vertical-align:top; width: 150px;",
            numericInput("e","",tp)),
        
        div(style="display:inline-block;vertical-align:top; width: 60;",h3("$$\\div$$")),
        
        div(style="display: inline-block;vertical-align:top; width: 150px;",
            numericInput("f1","",bt2)),
        div(style="display:inline-block;vertical-align:top; width: 60px;",h3("$$=$$")),
        div(style="display:inline-block;vertical-align:top; width: 150px;",numericInput("g1","",round(var(numbers),2))))
      
    }
  })

  output$formula3 <- renderUI({
    numbers <- extract(input$breaks)
    me=mean(numbers)
    tp=sum((numbers-me)^2)
    bt=length(numbers)
    spk=round(tp/bt,2)
    s=round(sqrt(spk),2)
    if(input$variable!="ss"){
    wellPanel(
        div(style="display:inline-block;vertical-align:top; width: 60px;",withMathJax(h3("$$=$$"))),
        div(style="display:inline-block;vertical-align:top; width: 60px;",withMathJax(h3("$$\\sqrt{}$$"))),
        div(style="display:inline-block;vertical-align:top; width: 150px;",numericInput("g3","",spk)),
        div(style="display:inline-block;vertical-align:top; width: 60px;",h3("$$=$$")),
        div(style="display:inline-block;vertical-align:top; width: 150px;",numericInput("g2","",s))
    )}else{
      wellPanel(
      div(style="display:inline-block;vertical-align:top; width: 60px;",withMathJax(h3("$$=$$"))),
      div(style="display:inline-block;vertical-align:top; width: 60px;",withMathJax(h3("$$\\sqrt{}$$"))),
      div(style="display:inline-block;vertical-align:top; width: 150px;",numericInput("g4","",round(var(numbers),2))),
          div(style="display:inline-block;vertical-align:top; width: 60px;",h3("$$=$$")),
          div(style="display:inline-block;vertical-align:top; width: 150px;",numericInput("g5","",round(sd(numbers),2)))
      ) 
      }
   
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
    
    if(input$variable!="ss" & n%%2!=0 ){
      wellPanel(
        helpText(withMathJax("middle position formula$$\\frac{N+1}{2}$$ ")),
        div(style="display:inline-block;vertical-align:top; width: 160px;", numericInput("od", "N+1",value = n+1 )),
        div(style="display:inline-block;vertical-align:top; width: 60;",h3("$$\\div2=$$")),
        div(style="display:inline-block;vertical-align:top; width: 160px;", numericInput("od", "middle position", value=odn )))
    }else if( input$variable=="ss" & n%%2!=0){
      wellPanel(
        helpText(withMathJax("middle position formula$$\\frac{n+1}{2}$$ ")),
        div(style="display:inline-block;vertical-align:top; width: 160px;", numericInput("od", "n+1",value = n+1 )),
        div(style="display:inline-block;vertical-align:top; width: 60;",h3("$$\\div2=$$")),
        div(style="display:inline-block;vertical-align:top; width: 160px;",numericInput("od", "middle position",value = odn )))
    }
    else if( input$variable=="ss" & n%%2==0){
      wellPanel(
        div(style="display:inline-block;vertical-align:top; width: 160px;", numericInput("even2", 
                                                                                         withMathJax("$$\\frac{n}{2}$$"), 
                                                                                         value =ed[1] )),
        div(style="display:inline-block;vertical-align:top; width: 160px;", numericInput("even3", 
                                                                                         withMathJax("$$\\frac{n}{2}+1$$"), 
                                                                                         value =ed[2] )))
      
    }
    else if(input$variable!="ss" & n%%2==0){
      wellPanel(
        
        div(style="display:inline-block;vertical-align:top; width: 160px;",  numericInput("even2", 
                                                                                          withMathJax("$$\\frac{N}{2}$$"), 
                                                                                          value =ed[1] )),
        div(style="display:inline-block;vertical-align:top; width: 160px;",numericInput("even3", 
                                                                                        withMathJax("$$\\frac{N}{2}+1$$"), 
                                                                                        value =ed[2] )))
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
