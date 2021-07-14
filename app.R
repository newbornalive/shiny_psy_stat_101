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
                  
                  
                  tabPanel("Variance and Standard Deviation",
                           
                           br(), 
                           helpText("Step 1"), 
                           br(),
                           helpText("Compute the mean"),
                           uiOutput("formula"),
                           br(),
                           br(),
                           helpText("Step 2"),
                           textOutput("sp3"),
                          
                           
                           DT::dataTableOutput('table1'),
                           
                           
                           br(),
                           
                           br(),
                           helpText("Step 3"),
                           textOutput("sp4"),
                           DT::dataTableOutput('table2'),
                           br(),
                           
                           br(),
                           helpText("Step 4"),
                           textOutput("sp"),
                           DT::dataTableOutput('table3'),
                           
                           uiOutput("formula4"),
                          
                           
                           br(),
                           br(),
                           helpText("Step 5"),
                           textOutput("sp2"),
                           uiOutput("formula3")
                            ),
                 
                 br(),
                 
                 br(),
                 
                 tabPanel(" ", br()
                       
                 )
                  #############################################################################################################################  
                  
      )
    )
  )
)


server <- function(input, output, session) {
 
  ###############################################################

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
  
 # output$t1 <- renderTable({
 #   datos()
    
 # })

#######################################################################################################################
  proxy1 = dataTableProxy('table1')
  
  observe({
    replaceData(proxy1, df(), rownames = FALSE)
  })
  
  df <- reactive({
    
    numbers <- as.numeric(extract(input$breaks))
    ms= numbers-mean(numbers)
    df = data.frame(cbind(numbers,ms))
    
    df['$$x$$'] <- df[,1]
    df['$$x-\\mu$$'] <- df[,2]
    
    # workaround to get rid of first column
    df[ , names(df)[-c(1,2)]]
  })
  
  output$table1 <- DT::renderDataTable(rownames = FALSE, {
    isolate(df())
  })
  ##########################
  proxy2 = dataTableProxy('table2')
  
  observe({
    replaceData(proxy2, df2(), rownames = FALSE)
  })
  
  df2 <- reactive({
    
    numbers <- as.numeric(extract(input$breaks))
    ms= numbers-mean(numbers)
    ms2=round(ms^2,2)
    df2 = data.frame(cbind(numbers,ms, ms2))
    
    df2['$$x$$'] <- df2[,1]
    df2['$$x-\\mu$$'] <- df2[,2]
    df2['$$(x-\\mu)^2$$'] <- df2[,3]
    # workaround to get rid of first column
    df2[ , names(df2)[-c(1,2,3)]]
  })
  
  output$table2 <- DT::renderDataTable(rownames = FALSE, {
    isolate(df2())
  })
  
  
  proxy3 = dataTableProxy('table3')
  
  observe({
    replaceData(proxy3, df3(), rownames = FALSE)
  })
  df3 <- reactive({
    
    numbers <- as.numeric(extract(input$breaks))
    ms= numbers-mean(numbers)
    ms2=round(ms^2,2)
    
    df3 = data.frame(cbind(numbers,ms, ms2))
    
    df3['$$x$$'] <- df3[,1]
    df3['$$x-\\mu$$'] <- df3[,2]
    df3['$$(x-\\mu)^2$$'] <- df3[,3]
    # workaround to get rid of first column
    df3[ , names(df3)[-c(1,2,3)]]
  })
  
  output$table3 <- DT::renderDataTable(rownames = FALSE, {
    isolate(df3())
  })
  
  
  ##########################
  #######################################################################################################################
  datos2=reactive({
    x <- extract(input$breaks)
    mean_deviation_scores <- x-mean(x)
    squared_mean_deviations <- mean_deviation_scores^2
    
    data.frame(cbind(x, mean_deviation_scores, squared_mean_deviations))
  
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
      print("Compute the Sample variance $$ s^2=\\frac{\\Sigma(x-\\overline{x})^2}{n-1}$$")}
  })
  
  output$sp2 <- renderText({
     if(input$variable!="ss"){
      print("Compute the Population standard deviation $$\\sigma=\\sqrt{\\sigma^2}=\\sqrt{\\frac{\\Sigma(x-\\mu)^2}{N}}$$")}
    else{
      print("Compute the Sample standard deviation $$s=\\sqrt{s^2}=\\sqrt{\\frac{\\Sigma(x-\\overline{x})^2}{n-1}}$$")}
  })
  
  output$sp3 <- renderText({
    if(input$variable!="ss"){
      print("Compute the mean deviation scores $$(X-\\mu)$$")
      }
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
    numbers <- extract(input$breaks)
    s=sum(numbers)
    n=length(numbers)
    me=round(mean(numbers),2)
    if(n==0) {h2("No Data yet")}
    else if(input$variable!="ss"){wellPanel(
      
      h3(  withMathJax(sprintf("$$ \\mu= \\frac{%.2f}{%.2f}$$", 
                               s, n )) ),
      h3(  withMathJax(sprintf("$$ \\mu= %.2f$$", 
                               me)) )
      
    )}else{
      wellPanel(
        h3(  withMathJax(sprintf("$$ \\overline{x} = \\frac{%.2f}{%.2f}$$", 
                                 s, n )) ),   
        h3(  withMathJax(sprintf("$$ \\overline{x} = %.2f $$", 
                                 me )) ))   }
  })
  
  

  output$formula4 <- renderUI({
    numbers <- extract(input$breaks)
    me=mean(numbers)
    tp=sum((numbers-me)^2)
    bt=length(numbers)
    bt2=bt-1
    spk=round(tp/bt,2)
    if(tp==0){h2("No Data yet")}
    else if(input$variable!="ss"){
      wellPanel(
        h4(withMathJax(sprintf("$$ \\sigma^2 = \\frac{%.2f}{%.2f}=%.2f$$", tp, bt, spk)) ),
       )
      }else{
      wellPanel(
        h4(withMathJax(sprintf("$$ s^2 = \\frac{%.2f}{%.2f}=%.2f$$", tp, bt2, round(var(numbers),2))) ))
    }
  })

  output$formula3 <- renderUI({
    numbers <- extract(input$breaks)
    me=mean(numbers)
    tp=sum((numbers-me)^2)
    bt=length(numbers)
    bt2=bt-1
    spk=round(tp/bt,2)
    s=round(sqrt(spk),2)
    if(tp==0){h2("No Data yet")} else if(input$variable!="ss"){
    wellPanel(
        h4(withMathJax(sprintf("$$ \\sigma =\\sqrt{\\frac{%.2f}{%.2f}}=%.2f$$", tp, bt, s)) ),
   
    )}else{
      wellPanel(
        h4(withMathJax(sprintf("$$ s =\\sqrt{\\frac{%.2f}{%.2f}}=%.2f$$", round(var(numbers),2), bt2, round(sd(numbers),2))) ),

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