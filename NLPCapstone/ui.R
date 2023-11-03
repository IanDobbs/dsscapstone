ui <- fluidPage(
    titlePanel("DSS Capstone Project - Next Word Predictor"),
    tabsetPanel(
        tabPanel("Predictor", 
                 sidebarLayout(
                     sidebarPanel(
                         textAreaInput("user_input", "Enter text here:"),
                         actionButton("predict_button", "Get Predictions")  # Add the action button
                     ),
                     mainPanel(
                         h3("Predictions:"),
                         verbatimTextOutput("predictions")
                     )
                 )
        ),
        tabPanel("Instructions", 
                 p("1. Enter the text in the 'Enter text here' box."),
                 p("2. There is a profanity filter on input text that will generate a message."),
                 p("2. Click the 'Get Predictions' button to see predictions."),
                 p("3. Up to 5 predictions and associated probabilities will be displayed."),
                 p("4. The predictions will be displayed on the 'Predictions' tab.")
        )
    )
)
