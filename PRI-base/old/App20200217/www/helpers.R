# Code to display a loading-wheel while code depending on button click is executed.
# Button is disabled in the meantime. To show computation is done: check mark appears 
# and button is enabled again. Both icons show up right next to button. 

# 'withBusyIndicatorUI' wraps button (ui.R) and 'withBusyIndicatorServer' wraps code 
# (server.R).

# Function of ui.R
withBusyIndicatorUI <- function(button) {
  id = button[['attribs']][['id']]
  div(`data-for-btn` = id,
      button,
      span(
        class = "btn-loading-container",
        # Icons hidden until 'withBusyIndicatorServer' is called.
        hidden(
          # Loading wheel
          icon("spinner", class = "btn-loading-indicator fa-spin"),
          # Check mark
          icon("check", class = "btn-done-indicator"))))
}


# Function of server.R
# 'buttonID' of button wraped in 'withBusyIndicatorUI' in ui.R.
withBusyIndicatorServer <- function(buttonID, Rcode) {
  
  loading = sprintf("[data-for-btn=%s] .btn-loading-indicator", buttonID)
  done = sprintf("[data-for-btn=%s] .btn-done-indicator", buttonID)
  
  # Button is disabled and loading-wheel is shown while check mark is disabled.
  shinyjs::disable(buttonID)
  shinyjs::show(selector = loading)
  shinyjs::hide(selector = done)
  
  # When code is executed 'withBusyIndicatorServer' is exited. The button is enabled
  # again and the loading-wheel hidden.
  on.exit({
    shinyjs::enable(buttonID)
    shinyjs::hide(selector = loading)
  })
  
  # When computation is completed check mark shows up and is hidden again after a few time steps.
  tryCatch({
    value = Rcode
    shinyjs::show(selector = done)
    shinyjs::delay(2000, shinyjs::hide(selector = done, anim = TRUE, animType = "fade",
                                       time = 0.5))
    value
  })
}