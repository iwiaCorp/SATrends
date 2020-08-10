withBusyIndicator <- function(buttonId, expr) {
  
  loadingEl <- sprintf("[data-for-button=%s] button-loading-indicator", buttonId)
  doneEl <- sprintf("[data-for-button=%s] .button-done-indicator", buttonId)
  disable(buttonId)
  shinyjs::show(selector = loadingEl)
  hide(selector = doneEl)
  #hide("errorDiv")
  on.exit({
    enable(buttonId)
    hide(selector = loadingEl)
  })
  
  tryCatch({
    value <- expr
    shinyjs::show(selector = doneEl)
    delay(2000, hide(selector = doneEl, anim = TRUE, animType = "fade",
                     time = 0.5))
    value
  }, error = errorFunc)
}

# Error handler that gets used in many tryCatch blocks
errorFunc <- function(err) {
  errMessage <- gsub("^ddpcr: (.*)", "\\1", err$message)
  html("errorMsg", errMessage)
  shinyjs::show("errorDiv", TRUE, "fade")
}

helpPopup2 <- function(content, title = NULL) {
  a(href = "#",
    class = "popover-link",
    `data-toggle` = "popover",
    `data-title` = title,
    `data-content` = content,
    `data-html` = "true",
    `data-trigger` = "hover",
    icon("question-circle")
  )
}