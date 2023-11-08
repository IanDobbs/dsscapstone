# Define the UI
ui <- fluidPage(
    titlePanel("Next Word Prediction App"),
    tabsetPanel(
        # Add an "Instructions" tab
        tabPanel("Instructions",
                 HTML("<p>Welcome to the Next Word Prediction App!</p>"),
                 HTML("<p>This app helps you predict the next word in a sequence.</p>"),
                 HTML("<p>To use the app, follow these steps:</p>"),
                 HTML("<ol>
                    <li>Enter a phrase in the 'Enter a phrase' input box.</li>
                    <li>Click the 'Predict' button to get the prediction.</li>
                    <li>The predicted word will be displayed in the main panel.</li>
                    <li>The next most frequent predicted words will be displayed on the 'Top Predictions' tab.</li>
                </ol>")
        ),
        # Add the main tab for predictions
        tabPanel("Prediction",
                 sidebarLayout(
                     sidebarPanel(
                         textInput("user_input", "Enter a phrase:"),
                         actionButton("predict_button", "Predict")
                     ),
                     mainPanel(
                         h3("Predicted Next Word:"),
                         textOutput("prediction_output")
                     )
                 )
        ),
        # Add a new tab for "Top Predictions"
        tabPanel("Top Predictions",
                 mainPanel(
                     h3("Next Most Frequent Predictions:"),
                     textOutput("top_predictions")
                 )
        )
    )
)