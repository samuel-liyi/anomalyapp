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
  
  observe({
    output$plot=renderPlot({})
    output$mytable=renderTable({})
    inFile <- input$data_file
    if (is.null(inFile))
      return(NULL)
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
    if (is.null(d))
      return(NULL)
    if(dim(d)[2]==1&&is.numeric(d[,2])){
      result=AnomalyDetectionVec(d[,1],max_anoms = input$max_anoma
                                 ,period=input$period
                                 ,direction=input$direction)
    }
    
    else if(!any(is.na(as.POSIXlt(d[,1])))){
      d[,1]=as.POSIXct(d[,1],origin='1970-01-01')
      result=AnomalyDetectionTs(d[,c(1,2)],max_anoms = input$max_anoma
                                ,direction=input$direction,plot=TRUE)
    }
    else{
      output$illustration<-renderText({"Wrong format data uploaded,please retry "})
      return(NULL)
      
    }
    output$illustration<-renderText({"Anomalies Found!"})
    
    rs=result$anom
    rs[,1]=as.character(rs[,1])
    output$mytable<-renderDataTable({
      rs
    })
    if(!is.null(result$plot)){
      output$plot<-renderPlot({result$plot})
    }
                })
  }

shinyApp(ui, server)
