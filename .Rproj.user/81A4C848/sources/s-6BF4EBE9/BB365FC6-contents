

library(shiny)
source("functions.R")
source("sort.R")
library(DT)
library(ggplot2)
library(RColorBrewer)
library(stringr)


require(reshape2)
require(Hmisc)


# Define UI for application that draws a histogram
ui <- fluidPage(
    
    sidebarLayout(
        sidebarPanel(
            fluidRow(column(12, fileInput("csv_file_input", "Select all csv files to sort", multiple = TRUE, accept = '.csv'))),    
            fluidRow(column(12,textAreaInput("group_labels", "Enter categories", value = "", width = "100%", height = NULL,cols = NULL, rows = NULL, placeholder = "placeholder", resize = "vertical"))),
            fluidRow(column(4,actionButton("group_files", "Group")),
                     column(4, downloadButton("download_csvs", "Download Sorted Csvs")),
                     column(4, actionButton("generate_graph", "Generate Graph")))
    
    
    ),
    mainPanel( 
        fluidRow(column(12, plotOutput("pm_vac_graph"))), 
        fluidRow(column(12, dataTableOutput("csv_files"))),        
        fluidRow(column(12, dataTableOutput("temp"))) )

    )
)


# Define server logic required to draw a histogram
server <- function(input, output) 
{
    
  
    output$csv_files <- renderDataTable({
        
        if(is.null(input$csv_file_input))
            return(NULL)
        files <- get_table(input$csv_file_input)
        
     
        
        
    })
    
    observeEvent(input$group_files, {
      if(is.null(input$csv_file_input) || is.null(input$group_labels)) 
          return(NULL)
        output$csv_files <- sort_table(input$csv_file_input, input$group_labels)
        
    })
    
    observeEvent(input$generate_graph,
     {
         output$pm_vac_graph <- renderPlot({
             if(is.null(input$csv_file_input) || is.null(input$group_labels)) 
                 return(NULL)
             get_graph(input$csv_file_input, input$group_labels, "Temporary Title")
             
         })
     })
    

    output$download_csvs <- downloadHandler(
        filename = function() {
            paste("Results", "zip", sep=".")
        },
        content = function(fname) 
        {
            fs <- c()
            tmpdir <- tempdir()
            setwd(tempdir())
            
            res<-  sort_data(input$csv_file_input, input$group_labels)
            
            fs<-c(fs, "PM_mpi_all.csv")
            write.csv(res$pm_mpi, "PM_mpi_all.csv",  row.names = FALSE, na = "")
            
            fs<-c(fs, "Vac_mpi_all.csv")
            write.csv(res$vac_mpi, "Vac_mpi_all.csv",  row.names = FALSE, na ="")
            
            fs<-c(fs, "PM_vac_ratio_all.csv")
            write.csv(res$pm_vac, "PM_vac_ratio_all.csv",  row.names = FALSE,  na ="")
            
            fs<-c(fs, "PM_mpi_grouped.csv")
            write.csv(res$grouped_pm_mpi, "PM_mpi_grouped.csv", row.names = FALSE, na ="")
            
            fs<-c(fs, "Vac_mpi_grouped.csv")
            write.csv(res$grouped_vac_mpi, "Vac_mpi_grouped.csv",  row.names = FALSE, na ="")
            
            fs<-c(fs, "PM_vac_ratio_grouped.csv")
            write.csv(res$grouped_ratio, "PM_vac_ratio_grouped.csv",  row.names = FALSE, na= "")
            
            
            zip(zipfile=fname, files=fs)
        },
        contentType = "application/zip"
    )
    
    

}

# Run the application 
shinyApp(ui = ui, server = server)
