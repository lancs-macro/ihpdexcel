

library(shiny)
library(openxlsx)

# source("functions.R")

ui <- fluidPage(
    
    titlePanel("Internationa House Price Database Excel"),
    
    sidebarLayout(
        sidebarPanel(
            
            
            downloadButton(
                "download_excel", 
                "Download Data to Excel"
            )
            
        ),
        
        # Show a plot of the generated distribution
        mainPanel(
          imageOutput("rhpi_lag1"),
          imageOutput("rhpi_lag4"),
          imageOutput("ratio_lag1"),
          imageOutput("ratio_lag4")
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
      
    output$rhpi_lag1 <- renderImage({
      list(src = here::here("comparisons", "hpta2104", "ratio_lag1.png"),
           contentType = 'image/png',
           width = 400,
           height = 300,
           alt = "This is alternate text")
    }, deleteFile = FALSE)
    output$download_excel <- downloadHandler(
        filename = function() {
            "data1.xlsx"
        },
        content = function(file) {
          # source("create-excel.R")
          wb <- loadWorkbook(here::here("versions", "hpta2104.xlsx"))
          saveWorkbook(wb, file)
        }
    )
}

# Run the application 
shinyApp(ui = ui, server = server)
