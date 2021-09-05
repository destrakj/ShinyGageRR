library(shiny)

#ui
shinyUI(tagList(
  br(),
  img(
    src = "logo.png",
    height = 75,
    width = 250
  ),
  br(),
  br(),
  navbarPage(
    "Statistical Tools",
    tabPanel(
      "Gage Repeatability and Reproduceability",
      sidebarPanel(
        p(
          "Perform Gage Repeatability & Reproduceability using Two Way ANOVA With or Without Interaction."
        ),
        br(),
        textInput("partDesc", "Part Description (Part Number): "),
        textInput("measurementTool", "Measurement Tool (Tool Id): "),
        br(),
        dateInput("date", "Date: "),
        textInput("operator1", "Operator 1: "),
        textInput("operator2", "Operator 2: "),
        textInput("operator3", "Operator 3: "),
        br(),
        br(),
        p("Gage Repeatability and Reproduceability Study "),
        numericInput(
          "sigma",
          "Sigma: ",
          min = 1,
          max = 6,
          value = 5.15
        ),
        numericInput("usl", "Upper Specification Limit: ", value = 0),
        numericInput("lsl", "Lower Specification Limit: ", value = 0),
        br(),
        br(),
        p("Input Measurement "),
        tags$a(href = "https://shorturl.at/fqCJ0", "Download!"),
        br(),
        br(),
        fileInput(
          inputId = "csvFile",
          label = "Upload Measurement",
          accept = c("text/csv", "text/comma-separated-values,text/plain", ".csv")
        ),
        br(),
        br(),
        actionButton("downloadData", "Print")
      ),
      mainPanel(tabsetPanel(
        tabPanel(title = "Input Table",
                 tableOutput("table")),
        tabPanel(
          title = "Result Without Interaction",
          verbatimTextOutput("without2"),
          p("Two-way ANOVA Without Interaction :"),
          verbatimTextOutput("without3"),
          p("Variance Components :"),
          verbatimTextOutput("without4"),
          p("Gage Evaluation :"),
          verbatimTextOutput("without5"),
        ),
        tabPanel(
          title = "Result With Interaction",
          verbatimTextOutput("with2"),
          p("Two-way ANOVA With Interaction :"),
          verbatimTextOutput("with3"),
          p("Variance Components :"),
          verbatimTextOutput("with4"),
          p("Gage Evaluation :"),
          verbatimTextOutput("with5"),
        )
      ))
    ),
    tabPanel("Process Capability", div(
      img(
        src = "UnderConstruction.PNG",
        height = 600,
        width = 600
      ),
      style = "text-align: center;"
    ))
  )
))