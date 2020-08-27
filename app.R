

library(shiny)
source("functions.R")
source("sort.R")
library(DT)
library(ggplot2)
library(RColorBrewer)
library(stringr)

library(shinyBS)


require(reshape2)
require(Hmisc)


# Define UI for application that draws a histogram
ui <- fluidPage(
    
    sidebarLayout(
        sidebarPanel(
            fluidRow(column(12, fileInput("csv_file_input", "Select all csv files to sort", multiple = TRUE, accept = '.csv'))),    
            fluidRow(column(12,textAreaInput("group_labels", "Enter categories", value = "", width = "100%", height = NULL,cols = NULL, rows = NULL, placeholder = "Enter group names separated by a comma", resize = "vertical"))),
            fluidRow(column(12, textInput("title", "Dataset Title", value = "My Dataset",))),
            fluidRow(actionButton("group_files", "Group"),
                      downloadButton("download_csvs", "Download Sorted Csvs"),
                      actionButton("generate_graph", "Generate Graph"), align = "center")
    
    
    ),
    mainPanel( 
        fluidRow(column(12, dataTableOutput("csv_files"))),
        bsModal("show_graph", "PM/vac Ratio", "generate_graph", size = "large",plotOutput("pm_vac_graph"),downloadButton('download_graph', 'Download'))
    )        

    )

)


# Define server logic required to draw a histogram
server <- function(input, output) 
{

  
    output$csv_files <- renderDataTable({
        
        if(is.null(input$csv_file_input))
            return(NULL)
        files <- get_table(input$csv_file_input)
        
     
        
        
    }, options = list(dom = 'tpl', pageLength = 15))
    
    observeEvent(input$group_files, {
      if(is.null(input$csv_file_input) || is.null(input$group_labels) || input$group_labels == "") 
          return(NULL)
        output$csv_files <- sort_table(input$csv_file_input, input$group_labels)
        
    })
    
    observeEvent(input$generate_graph,
     {
         output$pm_vac_graph <- renderPlot({
             if(is.null(input$csv_file_input) || is.null(input$group_labels) || input$group_labels == "") 
                 return(NULL)
              get_graph(input$csv_file_input, input$group_labels, input$title)
             
         })
     })
    
    get_download_plot <- function(){
        if(is.null(input$csv_file_input) || is.null(input$group_labels) || input$group_labels == "") 
            return(NULL)
        get_graph(input$csv_file_input, input$group_labels, input$title)
    }

    output$download_csvs <- downloadHandler(
        filename = function() {
            paste(paste0(input$title," Results"), "zip", sep=".")
        },
        content = function(fname) 
        {
            fs <- c()
            tmpdir <- tempdir()
            setwd(tempdir())
            
            res<-  sort_data(input$csv_file_input, input$group_labels)
            
            fs<-c(fs, paste0(input$title," PM-mpi-all.csv"))
            write.csv(res$pm_mpi, paste0(input$title," PM-mpi-all.csv"),  row.names = FALSE, na = "")
            
            fs<-c(fs, paste0(input$title, " vac-mpi-all.csv"))
            write.csv(res$vac_mpi, paste0(input$title, " vac-mpi-all.csv"),  row.names = FALSE, na ="")
            
            fs<-c(fs, paste0(input$title," PM-vac-ratio-all.csv"))
            write.csv(res$pm_vac, paste0(input$title," PM-vac-ratio-all.csv"),  row.names = FALSE,  na ="")
            
            fs<-c(fs, paste0(input$title, " PM-mpi-grouped.csv"))
            write.csv(res$grouped_pm_mpi, paste0(input$title, " PM-mpi-grouped.csv"), row.names = FALSE, na ="")
            
            fs<-c(fs, paste0(input$title, " vac-mpi-grouped.csv"))
            write.csv(res$grouped_vac_mpi, paste0(input$title, " vac-mpi-grouped.csv"),  row.names = FALSE, na ="")
            
            fs<-c(fs, paste0(input$title, " PM-vac-ratio-grouped.csv"))
            write.csv(res$grouped_ratio, paste0(input$title, " PM-vac-ratio-grouped.csv"),  row.names = FALSE, na= "")
            
            
            zip(zipfile=fname, files=fs)
        },
        contentType = "application/zip"
    )
    
    output$download_graph <- downloadHandler(
        filename = paste0(input$title, " pm_vac_graph.png"),
        content = function(file) {
            device <- function(..., width, height) grDevices::png(..., width = width, height = height, res = 300, units = "in")
            ggsave(file, plot = get_download_plot(), device = device, limitsize = FALSE)
        }) 
    

}

# Run the application 
shinyApp(ui = ui, server = server)
