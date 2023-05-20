#' Make the framework off a shiny App
#' @description
#' `makeApp()` write the framework of your shiny application.
#' You precise the different component you need, looking at
#' shiny cheat sheet. Once `makeApp()` have produced the 
#' framework, You'll copy and paste the recurrent codes, and adapt the 
#' names of lists components.
#'
#' @param input a vector of strings describing the different
#' types of inputs you want in the user interface. Are implemented : 
#'  'numericInput', 'sliderInput', 'checkboxInput',  'checkboxGroupInput', 'fileInput', 'downloadButton', 'radioButtons', 
#' 'dateInput', 'dateRangeInput', 'selectInput', 'textInput'.
#' @param render a vector of render*() the server will produce, 
#' usually and automatically paired with output arguments.
#' Are implemented : 'renderImage', 'renderText', 'renderTable', 
#' 'renderDataTable', 'renderPlot'.
#' @param output a vector of strings describing the different
#' types of outputs you want in the user interface and to the 
#' server. Are implemented : 'plotOutput', 'htmlOutput', 'imageOutput', 
#' 'tableOutput', 'dataTableOutput'. 
#' @param  action 
#' @param layout a string among 'sidebarPanel' or 'tabsetPanel'.
#' Are implemented :  'sidebarLayout', 'tabsetPanel'.
#' @param dirname the name of the new directory that will
#' contains our shiny App if mode=2, or the name of a file is mode=1.
#' @param filename name of the file if mode=1.
#' @param mode 0, 1 or 2, to write the code of shiny app in
#' * 0 : the console,  
#' * 1 : one file, or 
#' * 2 : in 2 files ui.R and server.R in the dirname directory
#' in different files.
#' @param reac boolean, TRUE or FALSE to convert on purpose every input in
#' reactive object, or not.
#' @ return Argument mode= fix the number of file to write
#' your shiny App. If mode=0, `makeApp()` will return a character 
#' string in console. If mode=1 or mode=2, `makeApp()` will return nothing
#' but write the lines in 1 or 2 files.
#'
#' @examples
#' makeApp(output='plotOutput', mode=0, reac=TRUE)
#' makeApp(
#'   output=c('plotOutput', 'textOutput'), 
#'   input=c('sliderInput','numericInput'), mode=0, reac=FALSE)
#' @author Julien Bousquet (2023)
#' @export
makeApp <- function(
  dirname = NULL,
  input   =NULL,
  render = NULL,
  output  = NULL, 
  action = NULL,
  layout = NULL, 
  mode = 0,
  filename=dirname,
  reac=FALSE
){

  input   <- match.arg(input,    choices=c( NA, 'numericInput', 'sliderInput', 'checkboxInput',  'checkboxGroupInput', 'fileInput', 'downloadButton', 'radioButtons', 'dateInput', 'dateRangeInput', 'selectInput', 'textInput', 'submitButton', 'actionButton'), several.ok=TRUE) 
  output <- match.arg(output,  choices=c(NA, 'plotOutput', 'htmlOutput', 'imageOutput', 'tableOutput', 'dataTableOutput'), several.ok=TRUE) 
  render <- match.arg(render,  choices=c(NA, 'renderImage', 'renderText', 'renderTable', 'renderDataTable', 'renderPlot' ), several.ok=TRUE) 
  action <- match.arg(action, choices=c(NA, 'actionButton', 'submitButton', 'isolate', 'observeEvent', 'eventReactive'  ), several.ok=TRUE) 
  layout <- match.arg(layout,   choices=c(NA, 'sidebarLayout', 'tabsetPanel'))

  if(!is.logical(reac))stop('Argument reac= must be TRUE or FALSE.')
  if(!is.character(dirname) & !is.character(filename) & mode>0)stop('Argument dirname or filename must be a string like "myshinydir".')

  if('actionButton' %in% input) action <- c(action, 'actionButton')
  if('submitButton' %in% input){
    action <- c(action, 'submitButton')
    if( length(input)==1 | is.na(input)) input <- c(input ,'numericInput')
  }

  if(mode==2){
   if(is.null(dirname)){
     stop('Give a directory name with dirname to write the files.')
   }else{
     if(dir.exists(dirname)) {
      stop(paste('Directory',dirname,'already exists. Propose an other name.'))
     }else{
      dir.create(dirname)
      message(paste0('Directory \n',getwd(),'/',dirname,
              ' \nis created and ready to host shiny Application'))
      message('  Use :\ninstall.packages("shiny")\nlibrary(shiny)')
     }
   }
  }

  uiStr <- paste0(
    ifelse(mode<=1,'library(shiny)\n\n',''),
    'ui <- fluidPage( \n',

    #layouts part
    ifelse( 'sidebarLayout' %in% layout, 
      ' titlePanel("Title of your page"),
 sidebarLayout(
  sidebarPanel(\n',''), 

    ifelse( 'tabsetPanel' %in% layout,
      ' titlePanel("Title of your page"),
  tabsetPanel(id="tabset",
  tabPanel("Tab one", \n',''),

    #inputs part
    ifelse('numericInput' %in% input,
      '   numericInput(inputId="numeric",
     label="Numeric Input",
     value= 20,
     min  =  0,
     max  = 50,
     step =  1,
   ),\n',''),
    ifelse('sliderInput' %in% input,
      '   sliderInput(inputId="slider",
     label="Set it",
     value= 10,
     step =  5,
     min  =-10,
     max  =100
   ),\n',''),
    ifelse('textInput' %in% input | 'textOutput' %in% output | 'renderText' %in% render,
      '   textInput(inputId="text", 
     label = "Text", 
     value = "Default Text",
     placeholder = "Enter some text"
   ),\n',''),
    ifelse('checkboxInput' %in% input,
      '   checkboxInput(inputId="checkbox", 
     label = "Checkbox", 
     value = FALSE
   ),\n',''),
    ifelse('selectInput' %in% input,
      '   selectInput(inputId="select", 
     label   = "Select your choice",
     choices = c("Choice 1"="Value 1", "Choice 2"="Value 2", "Choice 3"="Value 3") 
   ),\n',''),
    ifelse('checkboxGroupInput' %in% input,
      '   checkboxGroupInput(
     inputId = "checkboxGroup",
     label   = "Group of checkboxes",
     choices = c("case 1"="1", "case 2" = "2" ,"case 3" = "3"),
     selected= "case 2",
     inline  = FALSE, 
    ),\n',''), 
 
    ifelse('fileInput' %in% input | 'downloadButton' %in% input,
      '    fileInput(
      inputId = "upload",
      label   = "Choose file",
      accept  = c(".csv", ".CSV"),
      buttonLabel="Choose",
      placeholder="Default or example",
      multiple= FALSE
   ),\n',''),

    ifelse('radioButtons' %in% input,
      '   radioButtons(
     inputId = "radio",
     label   = "Select option",
     selected= 2,
     inline  = FALSE,
     choiceNames = c("choice 1", "choice 2", "choice 3"),
     choiceValues= 1:3
   ),\n',''),

    ifelse('dateInput' %in% input,
      '   dateInput(
     inputId = "date",
     label   = "Choose a date :",
     daysofweekdisabled = c(1,2),
     datesdisabled = c("2023-05-26", "2012-05-27")
     ),\n',''),

    ifelse('dateRangeInput' %in% input,
      '   dateRangeInput(
     inputId = "dateRange",
     label   = "Choose start and end dates :"
     ),\n',''),

    ifelse('actionButton' %in% action | 'isolate' %in% action, 
      '   sliderInput("obs", "Number of observations", 0, 1000, 500),
   actionButton("goButton", "Go!"),\n', ''),

    ifelse('observeEvent' %in% action | 'eventReactive' %in% action, 
      '         numericInput("x", "Value", 5),
     br(),
     actionButton("button", "Show"), \n', ''),

    ifelse('submitButton' %in% action,
      '    submitButton("Update View", icon("refresh")),
   helpText("When you click the button above, you should see",
      "the output  updates to reflect the value you",
      "entered above."), \n', ''),
    ifelse( 'sidebarLayout' %in% layout, 
      ' ), #end of sidebarPanel
 mainPanel(\n',''),

    #display part
    ifelse('numericInput' %in% input,
      '   textOutput("numeric"), br(), \n',''),
    ifelse('sliderInput' %in% input,
      '   textOutput("slider"), br(),  \n',''),
    ifelse('fileInput' %in% input | 'downloadButton' %in% input,
      '   downloadButton("download","Download the same file you upload"), br(), \n',''),
    ifelse('textInput' %in% input | 'textOutput' %in% output | 'renderText' %in% render, 
      '   textOutput("text"), br(), \n',''),
    ifelse('checkboxInput' %in% input, 
      '   textOutput("chkbx"), br(), \n',''),
    ifelse('selectInput' %in% input, 
      '   textOutput("select"), br(), \n',''),
    ifelse('checkboxGroupInput' %in% input, 
      '   textOutput("chkbxgrp"), br(), \n',''),
    ifelse('radioButtons' %in% input, 
      '   textOutput("radio"), br(), \n',''),
    ifelse('dateInput' %in% input, 
      '   textOutput("date"), br(), \n',''),
    ifelse('dateRangeInput' %in% input, 
      '   textOutput("dateRange"), br(), \n',''),


    #output part
    ifelse('htmlOutput' %in% output,
      '    htmlOutput("html"), br(), \n',''),
    ifelse('tableOutput' %in% output | 'renderTable' %in% render,
      '    tableOutput("tab"), br(), \n',''),
    ifelse('dataTableOutput' %in% output | 'renderDataTable' %in% render,
      '    dataTableOutput("tab"), br(), \n',''),

    ifelse('plotOutput' %in% output | 'renderPlot' %in% render,
      '   sliderInput(inputId="sliderPlot",
     label="Number of obs.",
     value= 20,
     step =  5,
     min  =10,
     max  =1000
    ),br(),

   plotOutput("plot"), br(), \n',''),


    #render part
    ifelse('renderImage'  %in% render | 'imageOutput' %in% output, 
      '   sliderInput("n", "Number of observations", 2, 1000, 500),
   imageOutput("plot1"), br(), \n',''),

    #action part
    ifelse('actionButton' %in% action | 'isolate' %in% action, 
      '   plotOutput("distPlot"), br(), \n',''),
    ifelse('observeEvent' %in% action | 'eventReactive' %in% action, 
      '   tableOutput("table"), br(), \n', ''),
 
  
    ifelse( 'sidebarLayout' %in% layout, 
      ' ) #end of mainPanel 
) #end of sidebarLayout
\n',''),


    ifelse('tabsetPanel' %in% layout,
      '   ), #end of tabPanel one
  tabPanel("Tab two",
    br(), "Add outputs here" ),
  tabPanel("Tab three",
    br(), "Add outputs here" )
 ) #end of tabsetPanel  \n',''),

  ')\n' # end of fluidPage()
  ) # end of paste

#write server with implicit reactive context
if( !reac ){
serverStr <- 
  paste0(
  'server <- function ( input, output, session){ \n',
  ifelse('numericInput' %in% input,
    '  output$numeric <- renderPrint({ paste("Numeric :",input$numeric) })\n',''),
  ifelse('sliderInput' %in% input,
    '  output$slider  <- renderPrint({ paste("Slider :" ,input$slider) })\n',''),
  ifelse('textInput' %in% input | 'textOutput' %in% output | 'renderText' %in% render ,
    '  output$text <- renderText({return(paste("Text Input :", input$text)) })\n',''),
  ifelse('checkboxInput' %in% input,
    ' #  input$checkbox is a boolean
    output$chkbx <- renderText({return(paste("Checkbox Input :", input$checkbox)) })\n',''),
  ifelse('selectInput' %in% input,
    '  output$select <- renderText({return(paste("Select Input :", input$select)) })\n',''),
  ifelse('checkboxGroupInput' %in% input,
    ' #  input$checkboxGroup is a vector
  output$chkbxgrp <- renderText({return(paste("input$checkboxGroup is a vector : ",paste(input$checkboxGroup, collapse=" ")))})\n','') ,
  ifelse('fileInput' %in% input | 'downloadButton' %in% input,
    '   data <- reactive({
     req(input$upload) 
     read.csv(input$upload$datapath, dec=".", sep=",") 
    })\n
    output$download <- downloadHandler(
      filename=function(){paste0("data-",Sys.Date(),".csv")},
      content = function(file){write.csv(data(), file)}
    )\n',''),
  ifelse('radioButtons' %in% input, 
    '  output$radio <- renderText({ return(paste("RadioButtons :",input$radio)) })\n',''),
  ifelse('dateInput' %in% input, 
    '  output$date <- renderText({ return(paste("Date :",input$date)) })\n',''),
  ifelse('dateRangeInput' %in% input, 
    '  output$dateRange <- renderText({ return(paste("Date range :",paste(input$dateRange, collapse="to"))) })\n',''),
  ifelse('plotOutput' %in% output | 'renderPlot' %in% render,
    '  output$plot <- renderPlot({ 
    x <- rnorm(input$sliderPlot)
    hist( x )
  })\n',''),
    ifelse('actionButton' %in% action | 'isolate' %in% action, 
      '   output$distPlot <- renderPlot({
    # Take a dependency on input$goButton. This will run once initially,
    # because the value changes from NULL to 0.
    input$goButton

    # Use isolate() to avoid dependency on input$obs
    dist <- isolate(rnorm(input$obs))
    hist(dist)
  })\n',''),     

    ifelse('observeEvent' %in% action | 'eventReactive' %in% action, 
      '  # Take an action every time button is pressed;
    # here, we just print a message to the console
    observeEvent(input$button, {
      cat("Showing", input$x, "rows\n")
    })
    # Take a reactive dependency on input$button, but
    # not on any of the stuff inside the function
    df <- eventReactive(input$button, {
      head(cars, input$x)
    }) 
 output$table <- renderTable({
      df()
    })\n', ''),

  '') # fin du paste0
} # end of if(!reac)


#write server with explicit reactive context
if( reac ){
serverStr <- 
  paste0(
  'server <- function ( input, output, session){ \n',
#define reactives objects
  ifelse('numericInput' %in% input,
    '  numeric <- reactive({ return(input$numeric) })\n',''),
  ifelse('sliderInput' %in% input,
    '  slider  <- reactive({ return(input$slider) })\n',''),
  ifelse('textInput' %in% input | 'textOutput' %in% output | 'renderText' %in% render,
    ' text <- reactive({ return(input$text) })\n',''),
  ifelse('checkboxInput' %in% input,
    ' checkbox <- reactive({ return(input$checkbox) })\n',''),
  ifelse('selectInput' %in% input,
    '  select <- reactive({ return(input$select) })\n',''),
  ifelse('checkboxGroupInput' %in% input,
    ' checkboxGroup <- reactive({ return(input$checkboxGroup) })\n','') ,
  ifelse('radioButtons' %in% input, 
    '  radio <- reactive({ return(input$radio) })\n',''),
  ifelse('dateInput' %in% input, 
    '  reactDate <- reactive({ return(input$date) })\n',''),
  ifelse('dateRangeInput' %in% input, 
    '  dateRange <- reactive({ return(input$dateRange) })\n',''),
  ifelse('plotOutput' %in% output | 'renderPlot' %in% render,
    '  slider <- reactive({ return(input$sliderPlot) })\n',''),
  ifelse('actionButton' %in% action | 'isolate' %in% action, 
    '  obs <- reactive({ return(input$obs) })\n', ''),
    ifelse('observeEvent' %in% action | 'eventReactive' %in% action, 
    '  x <- reactive({ return(input$x) })\n', ''),

#Create outputs
  ifelse('numericInput' %in% input,
    '  output$numeric <- renderText({ paste("Numeric :",numeric() ) })\n',''),
  ifelse('sliderInput' %in% input,
    '  output$slider  <- renderText({ paste("Slider :" ,slider()) })\n',''),
  ifelse('textInput' %in% input | 'textOutput' %in% output | 'renderText' %in% render,
    '  output$text <- renderText({return(paste("Text Input :", text() )) })\n',''),
  ifelse('checkboxInput' %in% input,
    ' #  input$Checkbox is a boolean
    output$chkbx <- renderText({paste("Checkbox Input :", checkbox() ) })\n',''),
  ifelse('selectInput' %in% input,
    '  output$select <- renderText({return(paste("Select Input :", select() )) })\n',''),
  ifelse('checkboxGroupInput' %in% input,
    ' #  input$checkboxGroup is a vector
  output$chkbxgrp <- renderText({return(paste("input$checkboxGroup is a vector : ",paste(checkboxGroup(), collapse=" ")))})\n','') ,
  ifelse('fileInput' %in% input | 'downloadButton' %in% input,
    '   data <- reactive({
     req(input$upload) 
     read.csv(input$upload$datapath, dec=".", sep=",") 
    })\n
    output$download <- downloadHandler(
      filename=function(){paste0("data-",Sys.Date(),".csv")},
      content = function(file){write.csv(data(), file)}
    )\n',''),
  ifelse('radioButtons' %in% input, 
    '  output$radio <- renderText({ return(paste("RadioButtons :",radio() )) })\n',''),
  ifelse('dateInput' %in% input, 
    '  output$date <- renderText({ return(paste("Date :",reactDate())) })\n',''),
  ifelse('dateRangeInput' %in% input, 
    '  output$dateRange <- renderText({ return(paste("Date range :",paste(dateRange(), collapse=" to "))) })\n',''),

  ifelse('plotOutput' %in% output | 'renderPlot' %in% render,
    '  output$plot <- renderPlot({ 
    x <- rnorm(slider())
    hist( x )
  })\n',''),

    ifelse('actionButton' %in% action | 'isolate' %in% action, 
      '  output$distPlot <- renderPlot({
    # Take a dependency on input$goButton. This will run once initially,
    # because the value changes from NULL to 0.
    input$goButton

    # Use isolate() to avoid dependency on input$obs
    dist <- isolate(rnorm(obs()))
    hist(dist)
  })\n',''),

    ifelse('observeEvent' %in% action | 'eventReactive' %in% action, 
      '  # Take an action every time button is pressed;
    # here, we just print a message to the console
    observeEvent(input$button, {
      cat("Showing", x(), "rows\n")
    })
    # Take a reactive dependency on input$button, but
    # not on any of the stuff inside the function
    df <- eventReactive(input$button, {
      head(cars, x())
    })
    output$table <- renderTable({
      df()
    })\n', ''),

    # end of server function later
  '') # end of paste0

} # end of if(reac)

#serverStr no reactive implication
serverStr <- paste0(serverStr,
  #layouts part 
  ifelse('tabsetPanel' %in% layout, 
      '  output$tabname  <- renderText({ paste("You are in panel",  input$tabset)})\n ', ''),

  #output part
  ifelse('htmlOutput' %in% output,
    '  output$html <- renderText("<b>Hello</b> <i>World</i> ")\n',''),

  ifelse('tableOutput' %in% output | 'renderTable' %in% render,
    '  data(iris)
  output$tab <- renderTable({ iris })\n',''),
  ifelse('dataTableOutput' %in% output | 'renderDataTable' %in% render,
    '  data(iris)
  output$tab <- renderDataTable({ iris })\n',''),

 #render part
  ifelse('renderImage' %in% render | 'imageOutput' %in% output,
    '  output$plot1 <- renderImage({
     #Generate a temporary file to save the image. 
     #It will be deleted after renderImage
     #sends it, because deleteFile=TRUE.
     outfile <- tempfile(fileext=".png")

     # Generate a png image
     png(outfile, width=400, height=400)
     hist(rnorm(input$n))
     dev.off() #write the hist into tmp outfile

     # Return a list
     list(src = outfile,
         alt = "This is alternate text")
   }, 
   deleteFile = TRUE) #second argument of renderImage
  \n', ''), #end of renderImage ifelse

  '}\n'  # end of server function
) #end of paste0


if(mode==0){
  cat(uiStr)
  cat('\n\n')
  cat(serverStr)
  cat('\n\nshinyApp(ui, server)\n')  
}
if(mode==1){
  if( file.exists(filename))stop(paste0('The file ',dirname,'already exists. Please give an other name in dirname.'))
  message(
  paste0('  Open ',dirname,'.R file in R or RStudio
  and run all. The last command of the file will launch 
  the app : 
shinyApp(ui, server)'))
  cat(uiStr, file=paste0(dirname,'.R'))
  cat('\n\n', file=paste0(dirname,'.R'), append=TRUE)
  cat(serverStr, file=paste0(dirname,'.R'), append=TRUE)
  cat('\n\nshinyApp(ui, server)\n', file=paste0(dirname,'.R'), append=TRUE)
  #try to open the file automatically in R
}
  if(mode==2){ message(
    paste0('  Connect to the app directory with : 
getwd("',getwd(),'/',dirname,'") 
  and run the app :\nrunApp()'))
  cat(uiStr, file=paste0(dirname,'/ui.R'))
  cat(serverStr, file=paste0(dirname,'/server.R'))
  }
}

#tests
mode=2
input <- c('checkboxInput') # 'checkboxGroupInput' )
input <- c('numericInput','sliderInput' )
input <- c('fileInput')
dirname <- 'test'

#tests in implicit reactive mode
#makeApp(dirname='test', input=c('numericInput','sliderInput' ), mode=1)
#makeApp(dirname='test', input='fileInput', mode=0)
#makeApp(dirname='test', input='checkboxInput', mode=0) 
#makeApp(dirname='test', input='checkboxGroupInput', mode=0)
#makeApp(dirname='test', input='radioButtons', mode=0)
#makeApp(dirname='test', input='dateInput', mode=0)
#makeApp(dirname='test', input='dateRangeInput', mode=0)
#makeApp(dirname='test', input='selectInput', mode=0)
#makeApp(dirname='test', input='textInput', mode=0)
#makeApp(dirname='test', output='htmlOutput', mode=0
#makeApp(dirname='test', output='textOutput', mode=0)
#makeApp(dirname='test', output='imageOutput', mode=0)
#makeApp(dirname='test', render='renderImage', mode=0)
#makeApp(dirname='test', render='renderText', mode=0)
#makeApp(dirname='test', render='renderDataTable', mode=0)

# test in explicit reactive mode
#makeApp(dirname='test', input=c('numericInput' ), mode=0, reac=TRUE)
#makeApp(dirname='test', input=c('numericInput','sliderInput' ), mode=0, reac=TRUE)
#makeApp(dirname='test', input='fileInput', mode=0, reac=TRUE)
#makeApp(dirname='test', input='checkboxInput', mode=0, reac=TRUE)
#makeApp(dirname='test', input='checkboxGroupInput', mode=0, reac=TRUE)
#makeApp(dirname='test', input='radioButtons', mode=0, reac=TRUE)
#makeApp(dirname='test', input='dateInput', mode=0, reac=TRUE)
#makeApp(dirname='test', input='dateRangeInput', mode=0, reac=TRUE)
#makeApp(dirname='test', input='selectInput', mode=0, reac=TRUE)
#makeApp(dirname='test', input='textInput', mode=0, reac=TRUE)






