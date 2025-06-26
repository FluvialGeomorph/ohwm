test_that("verify text input", {
  msg_txt <- "This is the message."
  msg_html <- capture.output(log_message(msg_txt), type = "message")
  expect_message(log_message(msg_txt))
})

test_that("verify function output", {
  xs_mapedit <- sf::st_read(system.file("extdata", "shiny", "xs_mapedit.shp",
                                        package = "fluvgeodata"), quiet = TRUE)
  
})