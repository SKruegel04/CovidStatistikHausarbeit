#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(clock)
library(shiny)
library(ggplot2)
library(dplyr)
library (readxl)

covidData <- read.csv("./RKI_COVID19_Berlin.csv")
covidData$Meldedatum <- as.Date(covidData$Meldedatum)

bevölkerungsData <- read_excel ("SB_A01-05-00_2022h01_BE.xlsx", sheet = "T6", na = "NA")
bevölkerung_bezirke <- as.numeric(bevölkerungsData$...2[129:140])
bevölkerung_insgesamt <- as.numeric(bevölkerungsData$...2[141])

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
endDatum <- meldedaten[length(meldedaten)]
startDatum <- meldedaten[1]
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
      
      selectInput(
        inputId = "chartTyp",
        label = "Darstellung:",
        choices = c(
          "Barplot" = "Barplot",
          "Barplot (Horizontal)" = "BarplotHorizontal",
          "Barplot (relative Häufigkeit der Fälle nach Bezirksbevölkerungsgröße)" = "BarplotProportionalBezirkeFall",
          "Barplot (relative Häufigkeit der Todesfälle nach Bezirksbevölkerungsgröße)" = "BarplotProportionalBezirkeTod",
          "Barplot (relative Häufigkeit nach Bevölkerungsgröße)" = "BarplotProportionalGesamt",
          "Mosaikplot" = "Mosaikplot",
          "Zeitreihe" = "Zeitreihe",
          "Trend" = "Trend"
        ),
        selected = "Barplot"
      ),
      
      conditionalPanel(
        condition = "input.chartTyp != 'Zeitreihe' & input.chartTyp != 'Trend' & 
        input.chartTyp != 'BarplotProportionalBezirkeFall' &
        input.chartTyp != 'BarplotProportionalBezirkeTod'",
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
    
    datenAusschnitt = subset(
      covidData,
      Meldedatum > input$zeitraumVon & Meldedatum < input$zeitraumBis
    )
    
    basisDaten <- datenAusschnitt[, input$typ]
    
    # Datenfilter auf Basis von gewähltem Typ und spezifischer Eingabe
    if (input$typ == "Landkreis") {
      datenAusschnitt <- subset(datenAusschnitt, Landkreis %in% input$bezirk)
    } else if (input$typ == "Geschlecht") {
      datenAusschnitt <- subset(datenAusschnitt, Geschlecht %in% input$geschlecht)
    } else if (input$typ == "Altersgruppe") {
      datenAusschnitt <- subset(datenAusschnitt, Altersgruppe %in% input$altersgruppe)
    }
    
    # Wenn keine Falltypen ausgewählt, wähle alle aus
    gewaehlteFallTypen <- input$fallTypen
    if (length(gewaehlteFallTypen) < 1) {
      gewaehlteFallTypen <- c("AnzahlTodesfall", "AnzahlGenesen")
    } else if(input$chartTyp == 'Zeitreihe' | input$chartTyp == 'Trend'){
      gewaehlteFallTypen <- c("AnzahlTodesfall", "AnzahlFall")
    } else if(input$chartTyp == 'BarplotProportionalBezirkeFall'){
      gewaehlteFallTypen <- c("AnzahlFall")
    } else if(input$chartTyp == 'BarplotProportionalBezirkeTod'){
      gewaehlteFallTypen <- c("AnzahlTodesfall")
    }
    
    # Erstelle Zeilen im Modell für jeden Falltypen
    daten <- NULL
    farben <- c()
    namen <- c()
    
    
    for (fallTyp in gewaehlteFallTypen) {
      aggregierteDatenFürFalltyp <- table(basisDaten[datenAusschnitt[, fallTyp] != "0"])
      if (is.null(daten)) {
        daten <- rbind(aggregierteDatenFürFalltyp)
      } else {
        daten <- rbind(daten, aggregierteDatenFürFalltyp)
      }
      farben <- c(farben, datenfarben[fallTyp])
      namen <- c(namen, datennamen[fallTyp])
    }
    row.names(daten) <- namen
    
    # Plot auf Basis von chartType eingabe
    if (input$chartTyp %in% c("Barplot", "BarplotHorizontal")) {
      barplot(
        daten,
        beside = TRUE,
        col = farben,
        xlab = "",
        ylab = "",
        cex.names = 0.3,
        las = 2,
        horiz = input$chartTyp == "BarplotHorizontal"
      )
      # Legende für den Plot
      legend("right", y = -30, legend = namen, fill = farben)
    } else if (input$chartTyp == "BarplotProportionalBezirkeFall") {
      
      barplot(
        daten/bevölkerung_bezirke,
        beside = TRUE,
        col = farben,
        xlab = "",
        ylab = "",
        cex.names = 0.3,
        las = 2,
        horiz = input$chartTyp == "BarplotHorizontal",
        main = "Relative Häufigkeit der SARS-CoV-2 Fälle nach Bezirken"
      )
      # Legende für den Plot
      legend("right", y = -30, legend = namen, fill = farben)
    } else if (input$chartTyp == "BarplotProportionalBezirkeTod") {
      
      barplot(
        daten/bevölkerung_bezirke,
        beside = TRUE,
        col = farben,
        xlab = "",
        ylab = "",
        cex.names = 0.3,
        las = 2,
        horiz = input$chartTyp == "BarplotHorizontal",
        main = "Relative Häufigkeit der Todesfälle an SARS-CoV-2 nach Bezirken"
      )
      # Legende für den Plot
      legend("right", y = -30, legend = namen, fill = farben)
    } else if (input$chartTyp == "BarplotProportionalGesamt") {
      
      barplot(
        
        daten/bevölkerung_insgesamt,
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
        cex.names = 1,
        las = 1
      )
    } else if (input$chartTyp == "Zeitreihe") {
      
      ggplot(
        data = datenAusschnitt,
        aes(x = Meldedatum)
      ) +
        geom_line(aes(y = AnzahlTodesfall), col = "red") +
        geom_line(aes(y = AnzahlFall), col = "blue") +
        labs(x = 'Meldedatum', y = 'Anzahl der Todesfälle/Fälle') +
        ggtitle('Zeitreihe der COVID-19 Fällen (blau) und Todesfällen (rot)')
      
    } else if (input$chartTyp == "Trend") {
      
      ggplot(
        data = datenAusschnitt,
        aes(x = Meldedatum)
      ) + geom_smooth(aes(y = AnzahlTodesfall), col = "red") +
        geom_smooth(aes(y = AnzahlFall), col = "blue") +
        labs(x = 'Meldedatum', y = "") +
        ggtitle('Trends bei COVID-19 Fällen (blau) und Todesfällen (rot)')
    }
  })
}

# Run the application 
shinyApp(ui = ui, server = server)
