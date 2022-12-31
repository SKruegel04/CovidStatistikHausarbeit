#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
covidData <- read.csv("./RKI_COVID19_Berlin.csv")

datenfarben <- c(
  "AnzahlGenesen" = "green",
  "AnzahlFall" = "blue",
  "AnzahlTodesfall" = "red"
)
datennamen <- c(
  "AnzahlGenesen" = "Genesen",
  "AnzahlFall" = "Fall",
  "AnzahlTodesfall" = "Todesfall"
)

bezirke <- unique(covidData$Landkreis)
geschlechter <- unique(covidData$Geschlecht)
altersgruppen <- unique(covidData$Altersgruppe)

# Define UI for application that draws a histogram
ui <- fluidPage(
  # Application title
  titlePanel("Bezirke"),
  sidebarLayout(
    # Panel on the left side of the plot
    sidebarPanel(
      selectInput(
        inputId = "typ",
        label = "Typ:",
        choices = c(
          "Bezirke" = "Landkreis",
          "Geschlechter" = "Geschlecht",
          "Altersgruppen" = "Altersgruppe"
        ),
        selected = "Bezirke"
      ),
      selectizeInput(
        inputId = "fallTypen",
        label = "Falltypen:",
        choices = c(
          "Fälle" = "AnzahlFall",
          "Todesfälle" = "AnzahlTodesfall",
          "Genesen" = "AnzahlGenesen"
        ),
        selected = c("AnzahlFall", "AnzahlTodesfall"),
        multiple = TRUE,
        options = list(plugins = list("remove_button"))
      ),
      checkboxInput(
        "horizontal",
        "Balkendiagramm"
      ),
      # Wird angezeigt wenn "Bezirke" ausgewählt
      conditionalPanel(
        condition = "input.typ == 'Landkreis'",
        checkboxGroupInput(
          inputId = "bezirk",
          label = "Bezirk",
          choices = bezirke,
          selected = bezirke
        )
      ),
      # Wird angezeigt wenn "Geschlechter" ausgewählt
      conditionalPanel(
        condition = "input.typ == 'Geschlecht'",
        checkboxGroupInput(
          inputId = "geschlecht",
          label = "Geschlecht",
          choices = geschlechter,
          selected = geschlechter
        )
      ),
      # Wird angezeigt wenn "Altersgruppen" ausgewählt
      conditionalPanel(
        condition = "input.typ == 'Altersgruppe'",
        checkboxGroupInput(
          inputId = "altersgruppe",
          label = "Altersgruppe",
          choices = altersgruppen,
          selected = altersgruppen
        )
      )
    ),
    # Main panel with plot
    mainPanel(
      plotOutput("distPlot")
    )
  )
)

# Define server logic required to draw a histogram
server <- function(input, output) {

  output$distPlot <- renderPlot({
    
    basisDaten <- covidData[, input$typ]

    daten <- NULL
    farben <- c()
    namen <- c()
    for (fallTyp in input$fallTypen) {
      neuerDatensatz <- table(basisDaten[covidData[, fallTyp] != "0"])
      if (is.null(daten)) {
        daten <- neuerDatensatz
      } else {
        daten <- rbind(daten, neuerDatensatz)
      }
      farben <- c(farben, datenfarben[fallTyp])
      namen <- c(namen, datennamen[fallTyp])
    }
    row.names(daten) <- namen
    
    if (input$typ == "Landkreis") {
      daten <- subset(daten, select = input$bezirk)
    } else if (input$typ == "Geschlecht") {
      daten <- subset(daten, select = input$geschlecht)
    } else if (input$typ == "Altersgruppe") {
      daten <- subset(daten, select = input$altersgruppe)
    }
  
    barplot(
      daten,
      beside = TRUE,
      col = farben,
      xlab = "",
      ylab = "",
      las = 2,
      cex.names = 0.7,
      horiz = input$horizontal
    )
    legend("right", y = -30, legend = namen, fill = farben)
  })
}

# Run the application 
shinyApp(ui = ui, server = server)
