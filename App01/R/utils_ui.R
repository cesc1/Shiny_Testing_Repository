require(shiny)


# -------------------------------------------
# Theme -------------------------------------
# -------------------------------------------

ui_bsTheme <- function() {
  bslib::bs_theme(version = 5, primary = "#FF6622")
}


# -------------------------------------------
# Head functions-----------------------------
# -------------------------------------------

## Title/Description

ui_meta <- function(
    title = "Pet guesser",
    description = "Guess if the name is for a cat or a dog.") {
  
  tags$head(
    tags$title(title),
    tags$meta(name = "description", content = description)
  )
}


## Favicon
ui_favicon <- function() {
  HTML(glue(
    '<link rel="apple-touch-icon" sizes="180x180" href="/apple-touch-icon.png">',
    '<link rel="icon" type="image/png" sizes="32x32" href="/favicon-32x32.png">',
    '<link rel="icon" type="image/png" sizes="16x16" href="/favicon-16x16.png">'
  ))
}

## Google font
ui_font <- function() {
  HTML(glue(
    '<link rel="preconnect" href="https://fonts.googleapis.com">',
    '<link rel="preconnect" href="https://fonts.gstatic.com" crossorigin>',
    '<link href="https://fonts.googleapis.com/css2?family=',
    'Inter:wght@100;200;300;400;500;600;700;800;900&display=swap"
         rel="stylesheet">'
  ))
}

## Add css
ui_cssAdd <- function() {
  tags$link(
    rel = "stylesheet",
    type = "text/css",
    href = "site.css"
  )
}

## Add previous functions inside head
ui_head <- function() {
  tags$head(
    ui_meta(),
    ui_favicon(),
    ui_font(),
    ui_cssAdd()
  )
}


# -------------------------------------------
# Styling the app ---------------------------
# -------------------------------------------

## Logo

ui_img_logo <- function() {
  img(height = "48",
      class = "py-2 img-fluid",
      src = "saturncloud-logo-1.svg"
  )
}

## Image of cat and dog with css

ui_img_cute <- function() {
  div(class = "row",
      div(class = "col-6 offset-3",
          div(class = "overlapping-images",
              img(src = "dog.jpg", class = "img-circle img-left"),
              img(src = "cat.jpg", class = "img-circle img-right")
          )
      )
  )
}

## Title and explanation

ui_title <- function(title = "Pet name species guesser") {
  div(
    h1(title),
    p(glue(
      "Try and guess if a pet name is more popular ",
      "with cats or dogs in the Seattle pet license data."
    ))
  )
}

## Add previous functions inside layout

ui_pretty <- function() {
  tags$section(class = "py-5",
    div(class = "container",
      div(class = "row",
        div(class = "col-10 offset-1 col-md-6 offset-md-3 text-center",
          
          ui_img_logo(),
          ui_img_cute(),
          ui_title()
        )
      )
    )
  )
}




