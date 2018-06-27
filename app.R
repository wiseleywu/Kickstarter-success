library(shiny)

# load script
source('./scripts/model_serve.R')

ui <- shinyUI(navbarPage("Kickstarter Success",
                         tabPanel(
                           "Predictor",
                           fluidRow(
                             column(4),
                             column(4,
                                    img(src="kickstarter_logo.jpg", width=600),
                                    h2("Would a particular Kickstarter project meet its funding goal?", align = "center"),
                                    br(),
                                    p("This model could make a prediction without relying on current funding data
                                       (e.g. number of backers/pledges). Simply put, it would work if you were to submit
                                       a project that has just started its campaign."),
                                    br(),
                                    textInput("link", label = h4("Paste the project website URL here"), width = "100%"),
                                    actionButton("submit", "Submit Link", style="align:center", width = "100%"),
                                    hr(),
                                    h4(uiOutput("prediction")),
                                    p(uiOutput("note"))
                             ),
                             column(4)
                           )
                           ),tabPanel("Paper",
                                      tags$iframe(style="height:800px; width:100%; scrolling=yes",
                                                  src="./final_report.pdf")),
                         navbarMenu("Nitty Gritty",
                                    tabPanel("Data Processing Script", includeHTML("./script.html")),
                                    tabPanel("Early Data Exploration", includeHTML("./R_notebook/eda_final.nb.html")),
                                    tabPanel("Logistics LDA Model", includeHTML("./R_notebook/logistics_LDA.nb.html")),
                                    tabPanel("CART Model", includeHTML("./R_notebook/cart.nb.html")),
                                    tabPanel("Random Forest Model", includeHTML("./R_notebook/random_forest.nb.html")),
                                    tabPanel("xgBoost Model", includeHTML("./R_notebook/xgboost_tuning.nb.html")),
                                    tabPanel("Final Evaluation", includeHTML("./R_notebook/final_evaluation.nb.html"))
                                    )
#                         tags$script(src = "rainbow-custom.min.js"),
#                         theme = "github.css"
))

# Define server logic ----
server <- function(input, output) {
  #  output$value <- renderPrint(input$link)
  linkInput <- eventReactive(input$submit, {
    getProjectPrediction(input$link)
  }, ignoreNULL = TRUE)
  output$prediction <- renderText({
    pred <- linkInput()
    print(pred)
    if(is.double(pred)) {
      if (linkInput() > 0.5) {
        answer<-" "
      } else {
        answer<-" <em>not</em> "
      }
      HTML(paste("The model predicts that this particular project <em>will</em>", answer ,"meet its funding goal"))
    } else if (is.logical(pred)) {
      paste("Something went wrong. Please doublecheck your input.")
    }
    

  })
  output$note <- renderText({
    if (linkInput()) {
      HTML(paste("Want to know more about how this is done? Check out the 
                  <b>paper</b> and the <b>nitty gritty</b> from the navbar 
                  above! <br><br>

                  How do you know if the prediction is correct? Check back when
                  the project deadline has passed! Alternatively,
                  you could submit past projects instead. <br><br>

                  If the prediction is correct, good! If not, do realize no model
                  is perfect and this model has a 75 % accuracy in out-of-sample data. <br><br>

                  Note: this model is trained using US based projects only, 
                  therefore results from other countries' projects will be erroneous.
                  Furthermore, the model described in the paper is slightly
                  different from the one implemented here (popularity data from Google
                 Trends and S&P 500 index data were not used here"))
    }
  })
}

# Run the app ----
shinyApp(ui = ui, server = server)