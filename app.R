# aesthetics -------------------------------------------------------------------
aes_list = list(
  area = c("x*","y*","alpha","color","fill","group","linetype","size"),
  bar = c("x*","y*","alpha","color","fill","group","linetype","size"),
  bin2d = c("x*","y*","fill","group"),
  density = c("x*","y*","alpha","color","fill","linetype","size","weight"),
  dotplot = c("x*","y*","alpha","color","fill"),
  freqpoly = c("x*","y*","color","linetype",'size'),
  histogram = c('x*','y*','color','fill','linetype','size','weight'),
  jitter = c('x*','y*','alpha','color','fill','shape','size'),
  point = c('x*','y*','alpha','color','fill','shape','size'),
  quantile =c("x*","y*","alpha","color","linetype","size","weight"),
  rug = c('x*','y',"alpha","color","linetype","size"),
  smooth =  c("x*","y*","alpha","color","fill","linetype","size","weight"),
  text = c("x*",'y*','label*','alpha','angle','color','family','fontface','hjust','lineheight','size','vjust'),
  density2d = c('x*','y*','alpha','color','linetype','size'),
  hex = c('x*','y*','alpha','color','fill','size'),
  line = c('x*','y*','alpha','color','linetype','size'),
  step = c('x*','y*','alpha','color','linetype','size'),
  boxplot = c('lower*','middle*','upper*','x*','ymax*','ymin*','alpha','color','fill','linetype','shape','size','weight'),
  violin = c('x*','y*','alpha','color','fill','linetype','size','weight'),
  crossbar = c('x*','y*','ymax*','ymin*','alpha','color','fill','linetype','size'),
  errorbar = c('x*','ymax*','ymin*','alpha*','color','linetype','size','width'),
  errorbarh = c('x*','ymax*','ymin*','alpha*','color','linetype','size','width'),
  pointrange = c('x*','y*','ymin*','ymax*','alpha','color','fill','linetype','shape','size'),
  map = c('map_id*','alpha','color','fill','linetype','size'),
  contour = c('x*','y*','z*','alpha','color'),
  raster = c('x*','y*','alpha','fill'),
  tile = c('x*','y*','alpha','color','fill','linetype','size'),
  polygon = c('x*','y*','group*','alpha','color','fill','linetype','size'),
  path = c('x*','y*','alpha','color','linetype','size'),
  ribbon = c('x*','ymax*','ymin*','alpha','color','fill','linetype','size'),
  segment = c('x*','xend*','y*','yend*','alpha','color','linetype','size'),
  rect = c('xmax*','xmin*','ymax*','ymin*','alpha','color','linetype','size','fill'),
  spoke = c('x*','y*','angle*','radius*','alpha','color','group','linetype','size')
)

#####


library(shiny)
library(ggplot2)

# Define UI for application that draws a histogram
ui <- fluidPage(
   
   # Application title
   titlePanel("GGUI"),
   
   # Sidebar with a slider input for number of bins 
   sidebarLayout(
      sidebarPanel(style = "overflow-y:scroll; max-height: 1000px",
        tabsetPanel(
          tabPanel("Create Layer",
             br(),
             selectizeInput(inputId = "plotTypeSelect",label = "Select Plot Type",choices = names(aes_list)),
             splitLayout(
               fileInput(inputId = 'importData',label = "Import CSV",accept = c("text/csv",
                                                                                "text/comma-separated-values,text/plain",
                                                                                ".csv")),
               textInput(inputId = 'dataName', label = "Dataset Name",value = "Data")
             ),
             br(),
             br(),
             splitLayout(
               actionButton("saveLayer","Save Layer"),
               textInput(inputId = 'layerName', label = "Layer Name Name",value = "")
             ),
             br(),
             br(),
             uiOutput("selectData"),
             uiOutput("controls")
          ),
         tabPanel("Saved Layers",
             uiOutput("savedLayers")
          )
        )
      ),
      
      # Show a plot of the generated distribution
      mainPanel(
         plotOutput("plot"),
         plotOutput("combinedPlot")
      )
   )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  controls = list()

  RV <- reactiveValues(
    updateUI = 0,
    dataList = list(),
    layerList = list(),
    layerNames = vector()
  )
  
  #### Create Layer Tab ####
  observeEvent(input$importData,{
    df <- read.csv(input$importData$datapath)
    RV$dataList[[input$dataName]] <<- df
  })
  
  observeEvent(input$saveLayer,{
    if(input$layerName == ""){
      showModal(modalDialog(
        title = "Blank Layer Name",
        "Please enter a name for the layer.",
        easyClose = TRUE,
        footer = NULL
      ))
      return()
    }
    RV$layerList[[input$layerName]] <<- list(g)
    RV$layerNames <<- c(RV$layerNames,input$layerName)
  })
  
  observe({
    if(is.null(input$selectedDF)){
      return()
    }
    controls <<- list()
    aes = aes_list[[input$plotTypeSelect]]
    for(i in aes){
      aesName = sub(pattern = '\\*',replacement = '',x = i)
      required = grepl('\\*',x = i)
      controls[[aesName]] <<- splitLayout(cellWidths = c('95%','5%'),
                                selectInput(inputId = aesName,label = aesName,choices = colnames(RV$dataList[[input$selectedDF]])),
                                checkboxInput(inputId = paste0(aesName,"_Checkbox"),label = "",value = ifelse(required,yes = T,no = F))
      )
    }
    RV$updateUI = rnorm(1)
  })
  
  output$selectData <- renderUI({
    selectInput(inputId = 'selectedDF','Select Data for Plot',choices = names(RV$dataList))
  })
  
  output$controls <- renderUI({
    RV$updateUI
    controls
  })
   
  output$plot <- renderPlot({
     input$updatePlot
     
     tempList = list()
     aes = isolate(aes_list[[input$plotTypeSelect]])
     for(i in aes){
       aesName = sub(pattern = '\\*',replacement = '',x = i)
       input[[aesName]]
       input[[paste0(aesName,"_Checkbox")]]
       if(isolate(input[[paste0(aesName,"_Checkbox")]])){
        tempList[[aesName]] = isolate(input[[aesName]])
       }
     }
     g <<- ggplot(data = RV$dataList[[input$selectedDF]]) 
     g <<- g + do.call(paste0("geom_",input$plotTypeSelect),args = list(mapping = do.call("aes_string", tempList)))
     g
  })
   
  output$combinedPlot <- renderPlot({
    cp = NULL
    for(i in RV$layerList){
      if(is.null(cp)){
        cp = i
      }else{
        cp = cp + i
      }
    }
    if(isolate(length(RV$layerList)) == 0){
      cp = geom_blank() + theme_void()
    }
    cp
  })

  #### Save Layer Tab ####
  output$savedLayers <- renderUI({
    layerOutput = list()
    for(i in RV$layerNames){
      layerOutput[[i]]  = splitLayout(
        h3(i),
        actionButton(paste0('deleteLayer_',i),'Delete Layer')
      )
    }
    layerOutput
  })
}

# Run the application 
shinyApp(ui = ui, server = server)
