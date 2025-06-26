#' @title Log a message
#' @description  Logs a system message condition suitable for display in a 
#' Shiny console window.
#' @param msg character; The text of the message to display in the log.
#' @returns message condition
#' @export
#'
log_message <- function(msg) {
  message <- message(
    paste(
      format(Sys.time(), usetz = TRUE), "-",
      as.character(msg),
      "<br>")
    )
}