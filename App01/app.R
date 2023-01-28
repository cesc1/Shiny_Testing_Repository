library(shiny)

source("helper-functions.R")

# Swap fluidPage() to bootstrapPage() for more control with bootstrap.
ui <- bootstrapPage(
  # UI preamble ---------------------
  
  # Afegir theme per carregar bootstrap5 i variables SASS
  theme = bslib::bs_theme(version = 5, primary = "#FF6622"),
  
  # HTML head, metadata que el viewer no veu:
  tags$head(
    tags$title("Pet guesser"),
    tags$meta(name = "description", content = "GUess if the name is for a cat or dog"),
    
    # Afegir Favicon desde Real Favicon Generator
    HTML('<link rel="apple-touch-icon" sizes="180x180" href="/apple-touch-icon.png">
<link rel="icon" type="image/png" sizes="32x32" href="/favicon-32x32.png">
<link rel="icon" type="image/png" sizes="16x16" href="/favicon-16x16.png">'),
    
    # Afegir font desde Google Fonts
    HTML(glue('
<link rel="preconnect" href="https://fonts.googleapis.com">
<link rel="preconnect" href="https://fonts.gstatic.com" crossorigin>
<link href="https://fonts.googleapis.com/css2?family=',
              'Inter:wght@100;200;300;400;500;600;700;800;900&display=swap"
         rel="stylesheet">'))
  ),

  # Afegir custom css
  tags$head(
    tags$link(rel = "stylesheet", 
              type = "text/css", 
              href = "site.css")),
  
  # Seccio abans de l'app perque es vegi be ---------------
  # section es per una seccio de la pagina
  tags$section(class = "py-5",
               
               # Container afegeix espais a esquerra i dreta
               div(class = "container",
                   
                   # Fem una fila amb una sola columna per centrar contingut
                   div(class = "row",
                     div(class = "col-10 offset-1 col-md-6 offset-md-3 text-center",
                         
                         # SaturnCloud Logo
                         # img-fluid resizes amb la pantalla
                         img(height = "48",
                             class = "py-2 img-fluid",
                             src = "saturncloud-logo-1.svg"
                         ),
                         
                         # El gat i el gos amb custom css
                         div(class = "row",
                             div(class = "col-6 offset-3",
                                 div(class = "overlapping-images",
                                     img(src = "dog.jpg", class = "img-circle img-left"),
                                     img(src = "cat.jpg", class = "img-circle img-right")
                                 )
                             )
                         ),
                         
                         # Titol
                         h1("Pet name species guesser"),
                         p(glue("Try and guess if a pet name is more popular ",
                                "with cats or dogs in the Seattle pet license data."))
                     )
                   )
               )
  ),
  
  
  # Input/output section ---------------------------------------
 
  tags$section(class = "bg-light py-3",
      div(class = "container",
          div(class = "row",
              # 2 cols
              div(class = "col-lg-6",
                  div(class = "bg-white rounded border shadow m-1 p-2 h-100",
                      #textInput en shiny no es pot editar per tant:
                      # nolint start
                      HTML('<div class="form-group shiny-input-container w-100">
            <label class="control-label" id="name-label" for="name">Pet name</label>
            <input id="name" type="text" class="form-control shiny-bound-input" value="">
          </div>'),
                      # nolint end
                      div(class = "d-grid gap-2",
                          actionButton("guess_cat", "Guess Cats", class = "btn-primary text-white"),
                          actionButton("guess_dog", "Guess Dogs", class = "btn-primary text-white")
                      )
                  )
              ),
              div(class = "col-lg-6 text-center",
                  # fixar 200px height per fer el plot mes petit
                  plotOutput("plot", height = "200px"),
                  uiOutput("text")
              )
          )
      )
  )
)


# Define server logic required to draw a histogram
server <- function(input, output) {
  results <- reactive({
    input$guess_cat
    input$guess_dog
    isolate({
      selected_name <- fix_name(input$name)
      if (nchar(selected_name) == 0) {
        NULL
      } else {
        pet_type(pet_data, selected_name)
      }
    })
  })
  
  guess <- reactiveVal(label = "guess")
  
  observe({
    input$guess_cat
    isolate({
      guess("cats")
    })
  })
  
  observe({
    input$guess_dog
    isolate({
      guess("dogs")
    })
  })
  
  output$plot <- renderPlot({
    plot_value(results())
  })
  
  output$text <- renderUI({
    if(is.null(results())) {
      h3("Please select a valid name")
    } else if(!is.finite(results()$p)){
      h3("No pets in the data with that name")
    } else {
      if(results()$type == guess()) {
        value <- "correct!"
        value_style <- "correct-value"
      } else {
        value <- "incorrect."
        value_style <- "incorrect-value"
      }
      HTML(glue("<h3><strong class={value_style}>{guess()} is {value}</strong>",
                "({results()$cats} cats, {results()$dogs} dogs).</h3>"))
    }
  })
  
}

# Run the application 
shinyApp(ui = ui, server = server)
