library(DBI)
library(RSQLite)
library(shiny)

server <- function(input, output, session) {
    observeEvent(input$predict_button, {
        # Open a new database connection for this session
        db_connection <- dbConnect(RSQLite::SQLite(), "SQLPrediction.db")
        
        user_input <- input$user_input
        
        # Split the user input into words
        words <- unlist(strsplit(user_input, "\\s+"))
        
        # Initialize predictions as an empty list
        top_prediction <- NULL
        top_predictions <- NULL
        
        if (length(words) == 1) {
            # Handle single-word input
            n_minus_1 <- words[1]
            
            sql_statement <- paste0(
                "SELECT DISTINCT prediction FROM combined_data ",
                "WHERE n_minus_1 = '", n_minus_1, "' ",
                "ORDER BY total_freq DESC LIMIT 1"
            )
            
            result <- dbGetQuery(db_connection, sql_statement)
            
            if (nrow(result) > 0) {
                top_prediction <- result$prediction[1]
            }
        } else {
            # Handle multi-word phrases
            for (i in length(words):1) {
                n_minus_1 <- paste(words[(i + 1):length(words)], collapse = " ")
                
                sql_statement <- paste0(
                    "SELECT DISTINCT prediction FROM combined_data ",
                    "WHERE n_minus_1 = '", n_minus_1, "' ",
                    "ORDER BY total_freq DESC LIMIT 3"
                )
                
                result <- dbGetQuery(db_connection, sql_statement)
                
                if (nrow(result) > 0) {
                    top_prediction <- result$prediction[1]
                    top_predictions <- result$prediction[-1]
                    break  # Exit the loop if predictions are found
                }
            }
        }
        
        if (!is.null(top_prediction)) {
            output$prediction_output <- renderText(paste(" ", top_prediction))
        } else {
            output$prediction_output <- renderText("No prediction available.")
        }
        
        if (!is.null(top_predictions)) {
            output$top_predictions <- renderText(paste(" ", paste(top_predictions, collapse = ", ")))
        } else {
            output$top_predictions <- renderText(NULL)  # Clear the top predictions
        }
        
        # Close the connection when the session ends
        onSessionEnded(function() {
            dbDisconnect(db_connection)
        }, session)
    })
}