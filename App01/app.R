library(shiny)

source("R/utils_ui.R")
source("helper-functions.R")


# Swap fluidPage() to bootstrapPage() for more control with bootstrap.
ui <- bootstrapPage(
  # UI preamble ---------------------------------------
  
  # Add bs5 and sass variable
  theme = ui_bsTheme(),
  
  # HTML head (metadata
  ui_head(),

  # Logo, images and title
  ui_pretty(),
  
  # Input/output section ---------------------------------------
 
  tags$section(class = "bg-light py-3",
      div(class = "container",
          div(class = "row",
              # 2 cols
              div(class = "col-lg-6",
                  div(class = "bg-white rounded border shadow m-1 p-2 h-100",
                      #textInput en shiny no es pot editar per tant:
                      # nolint start (id "name")
                      HTML(glue(
                        '<div class="form-group shiny-input-container w-100">',
                        '<label class="control-label" id="name-label" for="name">Pet name</label>',
                        '<input id="name" type="text" class="form-control shiny-bound-input" value=""></div>'
                      )),
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
