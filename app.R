library(rpart)
library(shiny)
library(rpart.plot)
library(shinythemes)
library(tidyverse)
library(shinyWidgets)
library(plotly)

OFFER <- read.csv("Offer.csv", row.names = 1)

# Define UI for application that draws a histogram
ui <- fluidPage(
  tags$head(tags$link(rel = "shortcut icon",href="1F332.svg"),
            HTML('<!-- Primary Meta Tags -->
                <title>Visualize Offer Trees 🌲</title>
                <meta name="title" content="Visualize Offer Trees 🌲">
                <meta name="description" content="Understand Trees">
                
                <!-- Open Graph / Facebook -->
                <meta property="og:type" content="website">
                <meta property="og:url" content="https://aholmes24.shinyapps.io/VisualizeTree/">
                <meta property="og:title" content="Visualize Offer Trees 🌲">
                <meta property="og:description" content="Understand Trees">
                <meta property="og:image" content="https://www.nps.gov/common/uploads/cropped_image/primary/1BF87320-E487-28A4-8E0F241A813FA447.jpg?width=1600&quality=90&mode=crop">
                
                <!-- Twitter -->
                <meta property="twitter:card" content="summary_large_image">
                <meta property="twitter:url" content="https://aholmes24.shinyapps.io/VisualizeTree/">
                <meta property="twitter:title" content="Visualize Offer Trees 🌲">
                <meta property="twitter:description" content="Understand Trees">
                <meta property="twitter:image" content="https://www.nps.gov/common/uploads/cropped_image/primary/1BF87320-E487-28A4-8E0F241A813FA447.jpg?width=1600&quality=90&mode=crop">')),
  theme = shinytheme(theme = "united"),
  # Application title
  titlePanel("Visualize Offer Trees 🌲"),

  # Sidebar with a slider input for number of bins
  sidebarLayout(
    sidebarPanel(
      width = 2,
      numericInput("cp",
        "Complexity Parameter",
        value = .01,
        step = .01
      ),
      sliderInput("minbucket",
        "Minimum Obs in Node",
        min = 0,
        max = 150,
        value = 30
      ),
      hr(),
      radioButtons(
        inputId = "train",
        "Train or Test",
        c("Train", "Test"),
        selected = "Train"
      ),
      sliderInput("mix",
        "Train / Test Percent",
        min = 0.01,
        max = 0.99,
        value = .7
      ),
      hr(),
      h4("Accuracy"),
      progressBar("progaccur",
        value = 0,
        display_pct = T,
        status = "warning"
      ),
      hr(),
      h4("Frequency Table"),
      tableOutput("frqtab")
    ),


    # Show a plot of the generated distribution
    mainPanel(
      column(plotOutput("rpartoutput"),
             hr(),
        plotOutput("varimp"),
        width = 12
      )
    )
  )
)

# Define server logic required to draw a histogram
server <- function(input, output, session) {
  train.rows <- reactive({
    sample(1:nrow(OFFER), input$mix * nrow(OFFER))
  })

  train <- reactive({
    OFFER[train.rows(), ]
  })

  holdout <- reactive({
    OFFER[-(train.rows()), ]
  })

  TREE <- reactive({
    rpart(Response ~ .,
      data = train(),
      cp = input$cp,
      minbucket = input$minbucket
    )
    
  })

  output$rpartoutput <- renderPlot({
    rpart.plot(TREE(),
      shadow.col = "gray",
      main = "Recent Offer Success Rate"
    )
  })

  output$varimp <- renderPlot({
    if (nrow(data.frame(TREE()$variable.importance)) > 0) {
      df <- data.frame(imp = TREE()$variable.importance)
      df2 <- df %>%
        rownames_to_column() %>%
        rename("variable" = rowname) %>%
        arrange(imp) %>%
        mutate(variable = fct_inorder(variable))

      ggplot(df2) +
        geom_segment(aes(x = variable, y = 0, xend = variable, yend = imp),
          size = 1.5, alpha = 0.7) +
        geom_point(aes(x = variable, y = imp, col = variable),
          size = 4, show.legend = F) +
        coord_flip() +
        labs(
          y = "Importance",
          x = "Variable",
          title = "Variable Importance") +
        theme_bw() +
        theme(
          axis.title = element_text(size = 20),
          axis.text = element_text(size = 15),
          plot.title = element_text(size=25, hjust = .5))
    }
  })
  output$frqtab <- renderTable({
      
     if (input$train == "Train") {
         prediction <- predict(TREE(), newdata = train(), type = "class")
         table_mat <- table(train()$Response, prediction)
     }else {
         prediction <- predict(TREE(), newdata = holdout(), type = "class")
         table_mat <- table(holdout()$Response, prediction)
     }
      accur <- sum(diag(table_mat)) / sum(table_mat)
      
      prog <- accur * 100
      
      if (prog < 33) {
          status <- "danger"
      } else if (prog >= 33 & prog < 67) {
          status <- "warning"
      } else {
          status <- "success"
      }
      updateProgressBar(session = session, id = "progaccur", value = prog, status = status)
      table <- data.frame(table_mat)
      colnames(table) <- c("Truth","Prediction","Count")
      table
  })
}

# Run the application
shinyApp(ui = ui, server = server)
