library(shiny)


#read model 
load("./data/model.rds")


server <- function(input, output) {
    
    df  <- reactive({
        # input$file1 will be NULL initially. After the user selects
        # and uploads a file, head of that data file by default,
        # or all rows if selected, will be shown.
        
        req(input$file1)
        
        # when reading semicolon separated files,
        # having a comma separator causes `read.csv` to error
        tryCatch(
            {
                df <- read.csv(input$file1$datapath,
                               header = input$header,
                               sep = input$sep,
                               quote = input$quote)
            },
            error = function(e) {
                # return a safeError if a parsing error occurs
                stop(safeError(e))
            }
        )
    })
    
    output$contents <- renderTable({
        return(head(df(), n = c(input$obsr,input$obsl)))
    })
    
    # 数据概要信息
    output[['summary']] <- renderPrint({
        if (input$summary) return(summary(df()))
    })
    
    output_data <- eventReactive(input$forec, {
        data_input <- df()
        data_input$date <- as.Date(data_input$date, tryFormats = "%m/%d/%Y")
        data_input <- data_input %>% 
            select(-c(edu,result)) %>% 
            cbind(model.matrix(~edu-1, model.frame(~edu-1, 
                                                   com.dat, 
                                                   na.action=na.pass)))
        dtest <- xgb.DMatrix(data = data_input %>%
                                 select(c(model[["feature_names"]])) %>%
                                 as.matrix())
        pred <- predict(model, dtest)
        xgbpred <- ifelse (pred >= 0.5,1,0)
        output <- data.frame(id=data_input$id,forecast=xgbpred)
        output$forecast <- ifelse(output$forecast==1,"YES","NO")
        output_data <- output
        return(output_data)
    })
    
    output[['forecast']] <- renderPrint({ 
        if (input$forec) return(head(output_data(), n = input$obsfr))
    })
    
    output$downloadData <- downloadHandler(
        filename = function() {
            paste(Sys.time(), "_forecast.csv", sep = "")
        },
        content = function(file) {
            write.csv(output_data(), file, row.names = FALSE)
        }
    )
}

