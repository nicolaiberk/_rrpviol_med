# ______________________________________________
# Media rections to RR violence
# Goal: Write GUI for sample coding
# Procedure: draw sample, annotate, save
# ______________________________________________
# Date:  Wed Apr 28 17:26:26 2021
# Author: Nicolai Berk
#  R version 4.0.3 (2020-10-10)
# ______________________________________________


# Setup ####
library(tidyverse)
library(here)
library(data.table)
library(shiny)


# setup ui

ui <- fluidPage(
  
  # Application title
  titlePanel(paste0("Handkodierung Nachrichtenartikel")),

  conditionalPanel(
    condition = "input.start != 1",
    actionButton("start", "Start")),
  
  # Text output and questions
  conditionalPanel(
    condition = "input.start == 1", 
    fluidPage(
      
      br(),
      'Bitte lies den Titel unten (falls notwendig auch den Rest des Texts) und beantworte die Fragen. Wenn Du fertig bist, klicke bitte auf "Weiter".',
      br(),
      
      h3(textOutput("Title")),
      checkboxInput("more", "Zeige Volltext.", value = F),
      conditionalPanel(
        condition = 'input.more == 1',
        fluidPage(textOutput("Text"))
      ),
      
      br(),
      br(),
      
      column(
        4,
             
        ## migrationsfrage
        radioButtons("migration", 
                     label = "Nimmt der Artikel Bezug auf die Themen Migration und Integration?",
                     choices = c("Ja", "Nein", "Unklar"), ),
        
        ### extended definition
        checkboxInput("mig_def", "Zeige erweiterte Definition.", value = F),
        conditionalPanel(
          condition = 'input.mig_def == 1',
          fluidPage("Hierzu zählen bspw. Flüchtlingszahlen, Diskussionen um Zuwanderungs- und Einbürgerungsrechte, xenophobe Angriffe/Anschläge, migrationskritische/-feindliche Demonstrationen. ")
        ),
        
        ### specific migration category
        conditionalPanel(
          condition = 'input.migration == "Ja"',
          radioButtons("mig_fine", 
                       label = "Welcher thematischen Subkategorie kann der Artikel zugeordnet werden?",
                       choices = c("Immigration (inkl. Grenzsicherung, Rückführungsdebatten, Asylrecht)", 
                                   "Integration (inkl. Einbürgerungsdebatten)", 
                                   "Xenophobe Angriffe/Anschläge",
                                   "Migrationsfeindliche/-kritische Demonstrationen/Proteste",
                                   "Sonstiges...")),
        ),
        
        ### open category
        conditionalPanel(
          condition = 'input.mig_fine == "Sonstiges..."',
          textInput("mig_oth", 
                    label = "Welcher thematischen Subkategorie kann der Artikel zugeordnet werden (Einordnung selbständig vornehmen)?")
        ),
        
        ### specific migration region
        conditionalPanel(
          condition = 'input.migration == "Ja"',
          radioButtons("mig_geo", 
                       label = "Auf welche geographische Region (als Ziel der Migration) bezieht sich der Artikel?",
                       choices = c("Deutschland", "EU", "Rest der Welt (bspw. Mexiko-USA)")),
        ),
        
      ),
      
      column(
        8,
        h3("Nimmt der Artikel Bezug auf das Thema..."),
        column(
          4,
          radioButtons("laworder", 
                       label = "Kriminalität, Law and Order, öffentliche Sicherheit?",
                       choices = c("Ja", "Nein", "Unklar")),
          checkboxInput("law_def", "Zeige erweiterte Definition.", value = F),
          conditionalPanel(
            condition = 'input.law_def == 1',
            fluidPage("Hinweis: Hierzu zählen bspw. Kriminalitätsstatistiken, Berichte über die Polizei und andere Sicherheitsorgane, sowie allgemeine Berichte über Kriminalität.")
          ),
          
          br(),
          radioButtons("climate", 
                       label = "Klimapolitik/Umweltpolitik?",
                       choices = c("Ja", "Nein", "Unklar")),
          
        ),
        
        column(
          4,
          
          br(),
          radioButtons("social", 
                       label = "Deutsches Sozialsystem?",
                       choices = c("Ja", "Nein", "Unklar")),
          
          
          br(),
          radioButtons("afd", 
                       label = "AfD?",
                       choices = c("Ja", "Nein", "Unklar"))
      ),
        
          )
      
    ),
      
      br(),
      
      actionButton("nextpage", "Weiter", icon = icon("check")),
      
      br(),
      
      # add "save button"
      actionButton("save", "Daten speichern.", icon = icon("download")),
  )
)


# Define server logic required to draw a histogram
server <- function(input, output, session) {
  
  observeEvent(input$start, {
    
    showModal(modalDialog(
      title = "Wie heisst Du?  (schreib den Namen jedes Mal gleich!)",
      textInput("name", "Name", placeholder = "Name"),
      footer = tagList(actionButton("newstart", "Starte neu."),
              actionButton("loadfile", "Lade existierendes file..."))
    ))
  })
  
  observeEvent(input$loadfile, {
    
    removeModal()
    
    # import sample
    dta <<- fread(here(paste0("_dt/handcoding_", input$name, ".csv")), encoding = "UTF-8")
    
    # define initial text
    i <<- (1 + nrow(dta) - sum(dta$mig == "")) # starting point  = 1 + number of rows - number of uncoded rows 
    output$Title <- renderText(as.character(dta$title[i]))
    output$Text <- renderText(as.character(dta$text[i]))
    
  })
  
  
  observeEvent(input$newstart, {
    
    # import sample
    dta <<- fread(here("_dt/sample_handcoding.csv"), encoding = "UTF-8")[, c("title", "url", "text")]
    
    # define initial text
    i <<- 1
    output$Title <- renderText(as.character(dta$title[i]))
    output$Text <- renderText(as.character(dta$text[i]))
    
    dta$mig        <<- ""
    dta$mig_cat    <<- ""
    dta$mig_cat_oe <<- ""
    dta$mig_geo    <<- ""
    dta$laworder   <<- ""
    dta$climate    <<- ""
    dta$social     <<- ""
    dta$afd        <<- ""
    
    removeModal()
  })
  
  
  
  observeEvent(input$save, {
    
    showModal(modalDialog(
      title="Sicher, dass Du speichern und beenden willst?",
      tagList(actionButton("save_fin", "Speichern", icon = icon("check")),
              modalButton("Cancel", icon = icon("remove"))
      )
    ))
  })
  
  observeEvent(input$save_fin, {
    
    fwrite(dta, file = here(paste0("_dt/handcoding_", input$name, ".csv")))
    
    showModal(modalDialog(
      title="Daten gespeichert, Du kannst den Browser jetzt schliessen",
    ))
    
    Sys.sleep(5)
    
    stopApp()
    
  })

  observeEvent(input$nextpage, {
    
      
      # hide text
      updateCheckboxInput(session, inputId = "more", value = F)
      
      # write changes
      dta[i, "mig"]        <<- input$migration
      dta[i, "mig_cat"]    <<- input$mig_fine
      dta[i, "mig_cat_oe"] <<- input$mig_oth
      dta[i, "mig_geo"]    <<- input$mig_geo
      dta[i, "laworder"]   <<- input$laworder
      dta[i, "climate"]    <<- input$climate
      dta[i, "social"]     <<- input$social
      dta[i, "afd"]        <<- input$afd
      
    if (i < nrow(dta)){
      
      # call next row
      i <<- i + 1
      output$Title <- renderText(as.character(dta$title[i]))
      output$Text <- renderText(as.character(dta$text[i]))
      
    }else{
      
      showModal(modalDialog(
        title="Kodierung abgeschlossen!",
        tagList(
          actionButton("save_fin", "Speichern und schliessen.", icon = icon("download"))
        )
      ))
      
    }
    
    
  })
  
}

# Run the application 
shinyApp(ui = ui, server = server)

