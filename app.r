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
bezirk <- covidData$Landkreis
fall <- covidData$AnzahlFall != "0"
todesfall <- covidData$AnzahlTodesfall != "0"
BezirkTode <- table(bezirk[todesfall])
bezirkNamen <- c("Charlottenburg-Wilmersdorf", "Friedrichshain-Kreuzberg", "Lichtenberg", "Marzahn-Hellersdorf", "Mitte", "Neukölln", "Pankow", "Reinickendorf", "Spandau",
                 "Steglitz-Zelendorf", "Tempelhof-Schöneberg", "Treptow-Köpenick")
geschlecht <- unique(covidData$Geschlecht)
GeschlechtFaelle <- table(covidData$Geschlecht[fall])
GeschlechtTode <- table(covidData$Geschlecht[todesfall])

GeschlechtData <- rbind(GeschlechtFaelle[geschlecht], GeschlechtTode[geschlecht])

choices <- 

# Define UI for application that draws a histogram
ui <- fluidPage(
  # Application title
  titlePanel("Bezirke"),
  sidebarLayout(
    # Panel on the left side of the plot
    sidebarPanel(
      varSelectInput(
        inputId = "plotType",
        label = "Typ:",
        choices = c(
          "Bezirke" = "Bezirke",
          "Geschlechter" = "Geschlecht",
          "Todesfälle" = "Todesfälle"
        )
      ),
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
     
    BezirkFälle <- table(bezirk)
    if (input$plotType == "Bezirke"){
      barplot(cbind(BezirkFälle, BezirkTode) ~ bezirkNamen, main = "Anzahl der Covid-19-Fälle einschließlich Todesfälle nach Bezirken in Berlin", 
              legend.text = TRUE, args.legend = list(x = 15, y = 10000), xlab = "Bezirke")
      
     # modell <- lm(covidData$AnzahlFall ~ covidData$Landkreis)
    
    } else if (input$plotType == "Todesfälle") {
      barplot(BezirkTode, main = "Anzahl der Todesfälle an Covid-19 nach Bezirken in Berlin")
      
    } else if (input$plotType == "Geschlechter") {
      barplot(
        GeschlechtData,
        beside = TRUE,
        col = c("blue", "red"),
        xlab = "Geschlechter",
        ylab = "Fälle und Todesfälle"
      )
      
    }
     
  })
}

# Run the application 
shinyApp(ui = ui, server = server)
