server <- function(input, output) {
    observeEvent(input$predict_button, {
        user_input <- input$user_input
        
        # Check if user input contains profanity
        if (any(grepl(paste(profanity_list, collapse = "|"), tolower(user_input)))) {
            output$predictions <- renderText("Profanity detected. Please enter a clean input.")
        } else {
            # Your code to make predictions based on user_input
            predictions <- kneser_ney(user_input)
            
            if (anyNA(predictions$Prob)) {
                output$predictions <- renderText("No predictions available.")
            } else {
                predictions$Prob <- round(predictions$Prob, 2)  # Round probabilities to two decimal places
                output$predictions <- renderText({
                    paste("", paste(predictions$Word, " (Prob:", predictions$Prob, ")", collapse = ", "))
                })
            }
        }
    })
}
