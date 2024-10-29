library(shiny)
library(bslib)
library(bsicons)
library(htmltools)
library(httr2)
library(purrr)
library(glue)



# Source utilities --------------------------------------------------------

source("utilitise.R")


# Define UI ---------------------------------------------------------------

ui <- bslib::page_fillable(
  title = "My App",
  theme = bslib::bs_theme(bootswatch = "minty"),

  # Sidebar configuration
  sidebar = sidebar(open = "closed"),  # Initialize the sidebar as closed

  type_effect(),
  press_enter_to_chat(),

  # Main content with a chat card
  bslib::card(
    bslib::card_header(
      div(
        "Chatbot",
        class = "flex-grow-1"
      ),
      bslib::popover(
        bsicons::bs_icon("gear", class = "ms-auto"),
        # passwordInput("api_key", "API Key",
        #               placeholder = "Enter your OpenAI API key"),
        # shiny::p(
        #   shiny::tags$small(
        #     "Get your API key from the OpenAI website: ",
        #     shiny::tags$a(href = "https://platform.openai.com/api-keys", "https://platform.openai.com/api-keys")
        #   ),
        #   style = "margin-top: -18px;"
        # ),
        selectInput("model", "Model", choices = c("gpt-3.5-turbo", "gpt-4-turbo-preview")),
        shiny::checkboxInput("speech", "Enable speech", value = FALSE),
        shiny::p(
          shiny::tags$small(
            style = "font-size: 0.8em;",
            "Enable speech to text for a more interactive experience"
          ),
          style = "margin-top: -18px;"),
        title = "Settings",
      ),
      class = "d-flex align-items-center justify-content-between"  # Improve alignment
    ),
    # Chat history display
    uiOutput("chat_history"),

    # Input and button section
    div(
      class = "mt-auto",  # Auto margin top for better spacing
      style = css(
        "margin-left" = "10%",  # Less margin for smaller screens
        "margin-right" = "10%"
      ),

      # Input row
      fluidRow(
        column(
          10,  # Adjust width for better input field and button alignment
          textAreaInput(
            "prompt", NULL,
            width = "100%", rows = 2,  # Make input a bit larger for better typing experience
            placeholder = "Ask a question ..."
          )
        ),
        column(
          2,
          actionButton(
            inputId = "chat",
            label = icon("fas fa-paper-plane"),  # Paper plane icon for send
            class = "btn btn-primary btn-block",  # Button block style for responsiveness
            width = "100%"
          )
        )
      )
    )
  )
)


server <- function(input, output, session) {
  rv <- reactiveValues(chat_history = NULL)

  observeEvent(input$chat, {
    req(input$prompt != "")
    response <- chat(input$prompt, input$model, rv$chat_history, Sys.getenv("OPENAI_API_KEY"), speech = input$speech)
    rv$chat_history <- update_history(rv$chat_history, input$prompt, response)

    output$chat_history <- renderUI({
      req(!is.null(rv$chat_history))

      shiny::tagList(
        purrr::map(
          .x = rv$chat_history,
          .f = function(x) {
            alignment <- if (x$role == "user") "text-align: right;" else "text-align: left;"
            box_style <- "background-color: #f1f1f1; border-radius: 10px; padding: 10px; margin: 10px 0; max-width: 80%;"
            role_display <- if (x$role == "assistant") "🤖: " else "👤: "
            role_style <- if (x$role == "user") glue::glue("{box_style} {alignment}; margin-left: auto;") else glue::glue("{box_style} {alignment}; margin-right: auto;")

            if(x$role == "assistant" &
               x$content  == (rv$chat_history)[[length(rv$chat_history)]]$content){
              shiny::div(
                id = "latest_response",
                style = role_style,
                tags$script(HTML(glue::glue("
          setTimeout(function() {{
            typeEffect('latest_response', `{role_display} {x$content}`, 40);
          }}, 100);  // Delay to allow rendering
        ")))
                )
            }else{
              shiny::div(
                style = role_style,
                shiny::markdown(glue::glue("{role_display} {x$content}")))
            }


          }
        ),
        if(input$speech) {
          shiny::tags$audio(src = "output.mp3", type = "audio/mp3", controls = "controls", autoplay = TRUE)
        }
      )
    })

    updateTextAreaInput(session, "prompt", value = "", placeholder = "Ready for more input...")
  })
}

shiny::shinyApp(ui, server)









