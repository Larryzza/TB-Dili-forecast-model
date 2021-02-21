library(shiny)

# Define UI for data upload app ----
ui <- fluidPage(
    
    # App title ----
    titlePanel(""),
    
    # Sidebar layout with input and output definitions ----
    sidebarLayout(
        
        # Sidebar panel for inputs ----
        sidebarPanel(
            
            # Input: Select a file ----
            fileInput("file1", "上传要预测的患者数据",
                      multiple = FALSE,
                      accept = c("text/csv",
                                 "text/comma-separated-values,text/plain",
                                 ".csv")),
            
            # Horizontal line ----
            tags$hr(),
            
            # Input: Checkbox if file has header ----
            checkboxInput("header", "Header", TRUE),
            
            # Input: Select separator ----
            radioButtons("sep", "Separator",
                         choices = c(Comma = ",",
                                     Semicolon = ";",
                                     Tab = "\t"),
                         selected = ","),
            
            # Input: Select quotes ----
            radioButtons("quote", "Quote",
                         choices = c(None = "",
                                     "Double Quote" = '"',
                                     "Single Quote" = "'"),
                         selected = '"'),
            
            
            numericInput("obsr", "查看多少行数据？", 5),
            numericInput("obsl", "查看多少列数据？", 5),
            
            checkboxInput("summary", "查看数据概要", FALSE),
            
            # Horizontal line ----
            tags$hr(),
            numericInput("obsfr", "查看多少行预测？", 5),
            actionButton("forec", "查看预测"),
            downloadButton("downloadData", "下载预测结果")
            
        ),
        
        # Main panel for displaying outputs ----
        mainPanel(
            # Output: Data file ----
            
            tableOutput("contents"),
            verbatimTextOutput("summary"),
            verbatimTextOutput("forecast")
            
        )
    )
)

