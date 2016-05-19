library(ggplot2)
library(shinydashboard)
library(DT)
library(AnomalyDetection)
library(lubridate)
ui <- dashboardPage(
  
  dashboardHeader(title = "Anomaly Detection"),
  dashboardSidebar(
    selectInput("direction", "choose the direction for test",
                c("pos",'neg','both'), selected = "both",
                multiple = FALSE, selectize = TRUE, width = NULL, size = NULL),
    
    numericInput("max_anoma", "maximum percent of anomalies",
                 0.05, min = 0, max = 1, step = 0.01, width = NULL)
    ,numericInput("period", "period of series",
                  7, min = 1, max = 1000, step = 1, width = NULL)
    ,checkboxInput("plotting", "plot the anomalies", value = TRUE, width = NULL)
    ,fileInput("data_file", "upload your data file (xlsx,csv)",
               multiple = FALSE, accept = NULL, width = NULL)
    
  ),
  dashboardBody(
    
    withTags(
      div(p("Anomaly detection require the dataframe you upload to be one of : "),
          ol(
            li("numerical vector "),
            li("dataframe with first column as timestamp and second column as value, further columns will be simply ignored")
          )
      )
    ),
    h2(textOutput("illustration")),
    fluidRow(
      plotOutput("plot"),
      br(),
      dataTableOutput('mytable')
      
    )
  )
)

server <- function(input, output) {
  value=reactiveValues(anoms=NULL,plot=NULL)
  observe({
    value$anoms=NULL
    value$plot=NULL
    inFile <- input$data_file
    if (is.null(inFile))
      return(NULL)
    withProgress(message = "processing your data, be patient",value=0.1,{
    d<-tryCatch(read.csv(inFile$datapath),
                warning=function(e) 
                {
                  output$illustration<-renderText({"Cannot read the csv file,
                    please check and upload again"}
                  )
                  return(NULL)
                },
                error = function(e) 
                {
                  output$illustration<-renderText({"Cannot read the csv file,
                    please check and upload again"}
                  )
                  return(NULL)
                }
    )
    incProgress(0.2)
    if (is.null(d))
      return(NULL)
    if(dim(d)[1]<=7){
      output$illustration<-renderText({"your time series too short,
                    please check and upload again"}
      )
      return(NULL)
    }
    if(dim(d)[2]==1&&is.numeric(d[,1])){
      result=AnomalyDetectionVec(d[,1],max_anoms = input$max_anoma
                                 ,period=input$period
                                 ,direction=input$direction
                                 ,plot=input$plotting)
      incProgress(0.4)
    }
    
    else if(!any(is.na(parse_date_time(d[,1],
                                       c('%m/%d/%Y %H:%M:%S','%m/%d/%Y %H%M','%Y-%m-%d %I:%M:%S',
                                         '%Y-%m-%d %H:%M','%Y%m%d','%Y-%m-%d','%Y%m','Y-%m')
                                       ,exact = TRUE)))){
      d[,1]=parse_date_time(d[,1],
                            c('%m/%d/%Y %H:%M:%S','%m/%d/%Y %H%M','%Y-%m-%d %I:%M:%S',
                              '%Y-%m-%d %H:%M','%Y%m%d','%Y-%m-%d','%Y%m','Y-%m')
                            ,exact = TRUE)
      result=AnomalyDetectionTs(d[,c(1,2)],max_anoms = input$max_anoma
                                ,direction=input$direction,plot=input$plotting)
      incProgress(0.4)
    }
    else{
      output$illustration<-renderText({"Wrong format data uploaded,please retry "})
      return(NULL)
      
    }
    output$illustration<-renderText({"Anomalies Found!"})
    rs=result$anom
    rs[,1]=as.character(rs[,1])
    value$anoms=rs
    
    incProgress(0.2)
    if(!is.null(result$plot)){
      value$plot=result$plot
    }
    })
  })
  output$mytable<-renderDataTable({
    if(is.null(value$anoms)){
      return ()
    }
    else{
      value$anoms
    }
  })
  output$plot<-renderPlot({
    if(is.null(value$plot)){
      return ()
    }
    else{
      value$plot
    }
    })
  }

shinyApp(ui, server)
