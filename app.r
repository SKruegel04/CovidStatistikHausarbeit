#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(ggplot2)
library(dplyr)
library (readxl)

covidData <- read.csv("./RKI_COVID19_Berlin.csv")
covidData$Meldedatum <- as.Date(covidData$Meldedatum)

bevölkerungsData <- read_excel ("SB_A01-05-00_2022h01_BE.xlsx", sheet = "T6", na = "NA")
bevölkerung_bezirke <- as.numeric(bevölkerungsData$...2[129:140])

fallTypen <- c(
  "Fälle" = "AnzahlFall",
  "Todesfälle" = "AnzahlTodesfall",
  "Genesen" = "AnzahlGenesen"
)

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

meldedaten <- unique(covidData$Meldedatum)
meldedaten <- meldedaten[order(meldedaten)]
startDatum <- meldedaten[1]
endDatum <- meldedaten[length(meldedaten)]
bezirke <- unique(covidData$Landkreis)
geschlechter <- unique(covidData$Geschlecht)
altersgruppen <- unique(covidData$Altersgruppe)

# Define UI for application that draws a histogram
ui <- fluidPage(
  # Application title
  titlePanel("SARS-CoV-2-Pandemie"),
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
        choices = fallTypen,
        selected = c("AnzahlTodesfall", "AnzahlGenesen"),
        multiple = TRUE,
        options = list(
          plugins = list("remove_button"),
          minItems = 1
        )
      ),
      
      selectInput(
        inputId = "chartTyp",
        label = "Darstellung:",
        choices = c(
          "Barplot" = "Barplot",
          "Barplot (Horizontal)" = "BarplotHorizontal",
          "Barplot (relative Häufigkeit nach Bezirksbevölkerungsgröße)" = "BarplotProportional",
          "Mosaikplot" = "Mosaikplot",
          "Zeitreihe" = "Zeitreihe",
          "Trend" = "Trend"
        ),
        selected = "Barplot"
      ),
      
      dateInput(
        inputId = "zeitraumVon",
        label = "Von:",
        value = startDatum,
        min = startDatum,
        max = endDatum,
        format = "yyyy-mm-dd",
        startview = "month",
        weekstart = 1,
        language = "de"
      ),
      
      dateInput(
        inputId = "zeitraumBis",
        label = "Bis:",
        value = endDatum,
        min = startDatum,
        max = endDatum,
        format = "yyyy-mm-dd",
        startview = "month",
        weekstart = 1,
        language = "de"
      ),
      
      # Wird angezeigt wenn "Bezirke" ausgewählt
      conditionalPanel(
        condition = "input.typ == 'Landkreis'",
        
        checkboxGroupInput(
          inputId = "bezirk",
          label = "Bezirk:",
          choices = bezirke,
          selected = bezirke
        )
      ),
      # Wird angezeigt wenn "Geschlechter" ausgewählt
      conditionalPanel(
        condition = "input.typ == 'Geschlecht'",

        checkboxGroupInput(
          inputId = "geschlecht",
          label = "Geschlecht:",
          choices = geschlechter,
          selected = geschlechter
        )
      ),
      # Wird angezeigt wenn "Altersgruppen" ausgewählt
      conditionalPanel(
        condition = "input.typ == 'Altersgruppe'",

        checkboxGroupInput(
          inputId = "altersgruppe",
          label = "Altersgruppe:",
          choices = altersgruppen,
          selected = altersgruppen
        )
      )
    ),
    # Main panel mit unserem fertigen Plot
    mainPanel(
      plotOutput("ausgabePlot")
    )
  )
)

# Define server logic required to draw a histogram
server <- function(input, output) {

  output$ausgabePlot <- renderPlot({

    limitierteDaten = subset(
      covidData,
      Meldedatum > input$zeitraumVon & Meldedatum < input$zeitraumBis
    )
    
    
    # Basis-Datensatz basierend auf "Typ" auswahl
    basisDaten <- limitierteDaten[, input$typ]
    
    # Wenn keine Falltypen ausgewählt, wähle alle aus
    gewaehlteFallTypen <- input$fallTypen
    if (length(gewaehlteFallTypen) < 1) {
      gewaehlteFallTypen <- fallTypen
    }
    
    # Fixiere FallTypen für "Zeitreihe" auf "AnzahlFall"
    if (input$chartTyp == "Zeitreihe") {
      gewaehlteFallTypen <- c("AnzahlFall")
    }

    # Erstelle Zeilen im Modell für jeden Falltypen
    daten <- NULL
    farben <- c()
    namen <- c()
    for (fallTyp in gewaehlteFallTypen) {
      neuerDatensatz <- table(basisDaten[limitierteDaten[, fallTyp] != "0"])
      if (is.null(daten)) {
        daten <- rbind(neuerDatensatz)
      } else {
        daten <- rbind(daten, neuerDatensatz)
      }
      farben <- c(farben, datenfarben[fallTyp])
      namen <- c(namen, datennamen[fallTyp])
    }
    row.names(daten) <- namen
    
    
    
    # Datenfilter auf Basis von Bezirk
    if (input$typ == "Landkreis") {
      daten <- subset(daten, select = input$bezirk)
    } else if (input$typ == "Geschlecht") {
      daten <- subset(daten, select = input$geschlecht)
    } else if (input$typ == "Altersgruppe") {
      daten <- subset(daten, select = input$altersgruppe)
    }
    
    # Plot auf Basis von chartType eingabe
    if (input$chartTyp %in% c("Barplot", "BarplotHorizontal")) {
      barplot(
        daten,
        beside = TRUE,
        col = farben,
        xlab = "",
        ylab = "",
        cex.names = 0.5,
        las = 2,
        horiz = input$chartTyp == "BarplotHorizontal"
      )
      # Legende für den Plot
      legend("right", y = -30, legend = namen, fill = farben)
    } else if (input$chartTyp == "BarplotProportional") {
      
      barplot(
        daten / bevölkerung_bezirke,
        beside = TRUE,
        col = farben,
        xlab = "",
        ylab = "",
        cex.names = 0.5,
        las = 2,
        horiz = input$chartTyp == "BarplotHorizontal"
      )
      # Legende für den Plot
      legend("right", y = -30, legend = namen, fill = farben)
    } else if (input$chartTyp == "Mosaikplot") {
      mosaicplot(
        t(daten),
        dir = c("h", "v"),
        main = "Mosaikplot",
        color = "skyblue2", 
        xlab = input$typ,
        cex.names = 1,
        las = 1
      )
    } else if (input$chartTyp == "Zeitreihe") {
    
      ggplot(data=covidData, aes(x=covidData$Meldedatum, y=covidData$AnzahlFall)) + 
        geom_line() + geom_smooth() + 
        labs(x = 'Meldedatum',
             y = 'Anzahl Covid-19 Fälle') + 
        ggtitle('Covid-19 Fälle')
      
      } else if (input$chartTyp == "Trend") {
      
      ggplot(data=covidData, aes(x=covidData$Meldedatum, y=covidData$AnzahlFall)) + 
        geom_smooth() + labs(x = 'Meldedatum', y = 'Anzahl Covid-19 Fälle') +
        ggtitle('Trend der Covid-19 Fälle')
      
     }
  })
}

# Run the application 
shinyApp(ui = ui, server = server)
