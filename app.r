#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)

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
bezirkNamen <- c("Mitte", "Friedrichshain-\nKreuzberg", "Pankow", "Charlottenburg-\nWilmersdorf","Spandau", "Steglitz-\nZelendorf", "Tempelhof-\nSchöneberg", "Neukölln", 
                 "Treptow-\nKöpenick", "Marzahn-\nHellersdorf", "Lichtenberg", "Reinickendorf")
genesene <- covidData$AnzahlGenesen > "0"
Genesene_geschlecht <- sort(table(covidData$Geschlecht[genesene]))
todesfall <- covidData$AnzahlTodesfall != "0"
Verstorbene_geschlecht <- sort(table(covidData$Geschlecht[todesfall]))
matr_geschlechter <- rbind(Verstorbene_geschlecht, Genesene_geschlecht)
Genesene_alter <- table(covidData$Altersgruppe[genesene])
Verstorbene_alter <- table(covidData$Altersgruppe[todesfall])
matr_alter <- cbind(Verstorbene_alter, Genesene_alter)
Genesene_bezirk <- table(covidData$Landkreis[genesene])
Verstorbene_bezirk <- table(covidData$Landkreis[todesfall])
matr_bezirke <- cbind(Verstorbene_bezirk, Genesene_bezirk)
bezirke_prop <- table(covidData$Landkreis)/bevölkerung_bezirke



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
        selected = c("AnzahlFall", "AnzahlTodesfall"),
        multiple = TRUE,
        options = list(
          plugins = list("remove_button"),
          minItems = 1
        )
      ),
      checkboxInput(
        "horizontal",
        "Horizontal"
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
        checkboxInput(
          "mosaicplot_bezirk",
          "Mosaikplot"
        ),
        checkboxInput(
          "proportional_bezirke",
          "Proportional"
        ),
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
        checkboxInput(
          "mosaicplot_geschlecht",
          "Mosaikplot"
        ),
        
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
        checkboxInput(
          "mosaicplot_alter",
          "Mosaikplot"
        ),
        
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
    
    # Datenfilter auf Basis von bezirk, geschlecht und altersgruppe
    # Plot
    if (input$typ == "Landkreis") {
      daten <- subset(daten, select = input$bezirk)
      barplot(
        daten,
        names.arg = bezirkNamen,
        beside = TRUE,
        col = farben,
        xlab = "",
        ylab = "",
        las = 2,
        cex.names = 0.7,
        horiz = input$horizontal
      )
      # Legende für den Plot
      legend("right", y = -30, legend = namen, fill = farben)
      
      if(input$mosaicplot_bezirk){
        mosaicplot(matr_bezirke, 
                   main = "Mosaikplot", 
                   las = 4,
                   xlab = "Bezirke",
                   color = "skyblue")
        
      }
      if(input$proportional_bezirke){
        barplot(sort(bezirke_prop),
                beside = TRUE,
                col = "blue",
                xlab = "",
                ylab = "",
                las = 2,
                cex.names = 0.4,
                horiz = input$horizontal)
      }
      
      
    } else if (input$typ == "Geschlecht") {
      daten <- subset(daten, select = input$geschlecht)
      barplot(
        daten,
        beside = TRUE,
        col = farben,
        xlab = "",
        ylab = "",
        cex.names = 0.7,
        horiz = input$horizontal
      )
      # Legende für den Plot
      legend("right", y = -30, legend = namen, fill = farben)
      
      if(input$mosaicplot_geschlecht){
       
        mosaicplot(matr_geschlechter, dir = c("h", "v"),
                   main = "Mosaikplot",
                   color = "skyblue2", 
                   xlab = "Geschlecht",
                   cex.names = 1,
                   las = 1)
        
      }
      
    } else if (input$typ == "Altersgruppe") {
      daten <- subset(daten, select = input$altersgruppe)
      barplot(
        daten,
        beside = TRUE,
        col = farben,
        xlab = "",
        ylab = "",
        cex.names = 0.7,
        horiz = input$horizontal
      )
      # Legende für den Plot
      legend("right", y = -30, legend = namen, fill = farben)
      
      if(input$mosaicplot_alter){
        mosaicplot(matr_alter, 
                   main = "Mosaikplot", 
                   las = 4,
                   xlab = "Altersgruppe",
                   color = "skyblue")
        
      }
    }
    
  })
}

# Run the application 
shinyApp(ui = ui, server = server)
