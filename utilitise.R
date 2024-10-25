chat <- function(user_message, 
                 model, 
                 history = NULL,
                 api_key = Sys.getenv("OPENAI_API_KEY")
                 ){
  
  # Generate system prompt
  system <- get_system_prompt()
  
  # Prepare full prompt to send to OpenAI, combining user, system prompts and history
  prompt <- prepare_prompt(user_message, system, history)
  
  # Define the request body
  body <- list(
    model = model,
    messages = prompt
  )
  
  # Make a request to the OpenAI API, authenticate, send the request body,
  # and parse the response
  response <- 
    httr2::request("https://api.openai.com/v1") %>% 
    httr2::req_url_path_append("chat/completions") %>% 
    httr2::req_auth_bearer_token(token = api_key) %>% 
    httr2::req_body_json(body) %>%
    httr2::req_perform()
  
  if(httr2::resp_is_error(response)){
    status <- httr2::resp_status(response)
    description <- httr2::resp_status_desc(response)
    
    cli::cli_warn(
      message = c(
        "x" = glue::glue("OpenAI API request failed. Error: {status} - {description}"),
        "i" = "Visit the OpenAI API documentation for more details"
      )
    )
    
    error_msg <-
      glue("_Uh Oh._ There was an error. Please check your API key.")
    
    return(invisible(error_msg))
  }
  
  response_out <- 
    response %>% 
      httr2::resp_body_json(simplifyVector = TRUE) %>% 
      purrr::pluck("choices", "message", "content")
    
  return(response_out)
}


get_system_prompt <- function(){
  instructions <- 
    "
    You are a close friend of a loving couple named Jesse and Isabel. 
    You have travelled 10 years into the future to observe how they are living and have now come back to the present. 
    It is Isabel's birthday today and she wants to ask you how their lives are in the future.
    Whenever, Isabel asks you a question, you must give an answer based on the below context.
    You can make up some answers if it's required, but make sure it's a happy response.
    Try to keep your answers to three sentences or less.

    Here is some context on their lives in the present:
    * Jesse and Isabel are currently living in Sydney, Australia
    * They live in a two bedroom apartment in the suburb of Zetland, with their cat Lua
    * Jesse is a data scientist, in the early stages of his career, trying to climb the management ladder.
    * Isabel is a clinical trials statistician, who is looking to use her skills in statistics to help with cancer treatment
    * They hang out with their friends on the weekend, going for runs at the park, eating together and going to the beach
    
    Here is some context on their lives in the future:
    * They now live in a 5-bedroom mansion in the suburb of Mosman
    * They have 10 kids together, who run around the house all of the time
    * They have a strong friendship group who hang out all the time together
    * Jesse is now the Chief Data Officer of a top company in Australia
    * Isabel has found the cure for cancer through her statistical analysis and has won a Nobel Prize
    * Every Summer and Winter holidays, they go on a holiday to amazing destinations
  "
  return(list(list(role = "system", content = instructions)))
}

prepare_prompt <- function(user_message, system, history){
  
  # Create a prompt for the user message
  user_prompt <- list(list(role = "user", content = user_message))
  
  # Combine user message, system prompt and history into a single prompt
  prompt <- c(system, history, user_prompt) %>% 
    # remove null elements
    purrr::compact()
  
  return(prompt)
}

update_history <- function(history, user_message, response){
    user_assistant <- 
      list(
        list(role = "user", content = user_message),
        list(role = "assistant", content = response)
      )
    
    history <- c(history, user_assistant) %>% purrr::compact()
    
    return(history)
}


# Define a function to enable message submission by pressing the Enter key.
press_enter_to_chat <- function() {
  # Insert custom HTML and JavaScript into the Shiny app's HTML head section.
  # This script is designed to listen for keydown events on an input field with the ID 'prompt'.
  tags$head(
    tags$script(
      # Use the HTML function to ensure that the JavaScript code is correctly interpreted as HTML.
      HTML("
        // jQuery document ready function to ensure the script runs after the document is fully loaded.
        $(document).on('keydown', '#prompt', function(e) {
          // Check if the key pressed is Enter (key code 13) or Line Feed (key code 10), and the Shift key is NOT pressed.
          if ((e.keyCode == 10 || e.keyCode == 13) && (!e.shiftKey)) {
            // Prevent the default action of the Enter key press, which is typically form submission.
            e.preventDefault();
            // Set a timeout before simulating a click event on the 'chat' button.
            // This delay ensures that any UI updates or validations can occur before the click action.
            setTimeout(function() {
              // Programmatically click the button with the ID 'chat' to submit the message.
              $('#chat').click();
            }, 500); // Delay set to 500 milliseconds.
          }
        });
      ")
    )
  )
}


