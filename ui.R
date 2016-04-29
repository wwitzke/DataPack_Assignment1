# If you want to run multiple lifespan estimations, just refresh the page.

shinyUI(fluidPage(
    titlePanel("Death Prediction Evaluation Service"),

    sidebarLayout(
        sidebarPanel(
            p("Enter the number of years you think you have left to live in the text box below and press", strong("Evaluate"), "only when you think you are ready. Please make an effort to be accurate. We cannot evaluate your estiamte if you aren't going to take this seriously."),
            p("Your prediction evaluation will appear to the right, along with some nice statistical information comparing your prediction to the predictions others made about their own lifespans, for your enjoyment."),
            numericInput('estimate', "Lifespan Estimation", "0", min=0, max=1000, step=1),
            actionButton("evaluate", "Evaluate")
        ),
        mainPanel(
            h4("Your evaluation:"),
            textOutput('evaluation'),
            h4("Your lifespan estimate compared to other people:"),
            plotOutput('lifespanHist'),
            h4("How correct your lifespan estimate was compared to other people:"),
            plotOutput('wrongnessHist'),
            h4("Final comments:"),
            textOutput("comments")
        ),
    )
))
