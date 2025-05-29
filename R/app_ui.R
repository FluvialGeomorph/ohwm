#' The application User-Interface
#'
#' @param request Internal parameter for `{shiny}`.
#'     DO NOT REMOVE.
#' @import shiny
#' @importFrom bslib page_navbar nav_panel layout_sidebar sidebar bs_theme
#'                   accordion accordion_panel layout_columns card card_header
#' @importFrom mapedit editModUI
#' @importFrom leaflet leafletOutput
#' @importFrom shinyWidgets autonumericInput noUiSliderInput wNumbFormat
#' prepare_slim_choices
#' @importFrom gt gt_output
#' 
#' @noRd
app_ui <- function(request) {
  mannings_choices <- c(
    "(a) Clean, straight, no deep pools (n = 0.030)" = 0.030,
    "(b) Same as (a), but more stones and weeds (n = 0.035)" = 0.035,
    "(c) Clean, winding, some pools and shoals (n = 0.040)" = 0.040,
    "(d) Same as (c), but some weeds and stones (n = 0.045)" = 0.045,
    "(e) Same as (c), at lower stages, with less effective slopes and sections (n = 0.048)" = 0.048,
    "(f) Same as (d), but more stones (n = 0.050)" = 0.050,
    "(g) Sluggish reaches, weedy, deep pools (n = 0.070)" = 0.070,
    "(h) Very weedy reaches, seep pools or floodways with heavy stands of timber and underbrush (n = 0.100)" = 0.100)
  
  tagList(
    golem_add_external_resources(),
    
    page_navbar(
      title = "FluvialGeomorph Tiered Assessment",
      id = "main",
      theme = bs_theme(bootswatch = "cerulean", version = 5),
      
      nav_panel(title = "Draw XS", layout_sidebar(
        # Display the xs editing module
        editModUI(id = "xs_editor_ui_id"),
        sidebar = sidebar(
          title = "Draw XS Instructions",
          position = "right",
          width = "25%",
          uiOutput("draw_xs_instructions"),
          uiOutput('draw_fl_button')
        )
      )),
      
      nav_panel(title = "Draw Flowline", layout_sidebar(
        # Display fl editing module
        editModUI(id = "fl_editor_ui_id"),
        sidebar = sidebar(
          title = "Draw Flowline Instructions",
          position = "right",
          width = "25%",
          uiOutput("draw_fl_instructions"),
          #actionButton("view_results", "View Results")
          uiOutput('view_results_button')
        )
      )),
      
      nav_panel(title = "Results", layout_sidebar(
        # Display results_map
        leafletOutput("results_map"),
        sidebar = sidebar(
          position = "right",
          width = "50%",
          accordion(
            id = "Results",
            open = c("Cross Sections", "Discharge"),
            accordion_panel(
              title = "Longitudinal Profile", 
              plotOutput("long_profile", height = "250px")),
            accordion_panel(
              title = "Cross Sections",
              selectInput("pick_xs", label = "Select a cross section:", 
                          choices = c(1)),
              splitLayout(
                noUiSliderInput("channel_elevation", "Channel REM:",
                                min = 100, max = 130, value = 103, 
                                format = wNumbFormat(decimals = 1),
                                orientation = "horizontal", 
                                update_on = "end"),
                noUiSliderInput("floodplain_elevation", "Floodplain REM:",
                                min = 100, max = 130, value = 112, 
                                format = wNumbFormat(decimals = 1),
                                orientation = "horizontal", 
                                update_on = "end")),
              plotOutput("xs_plot_channel", height = "250px"),
              plotOutput("xs_plot_floodplain", height = "250px"),
              gt_output("floodplain_volumes")
            ),
            accordion_panel(
              title = "Discharge",
              withMathJax("$$Q = \\frac{1.486}{n} A R ^\\frac{2}{3} S^\\frac{1}{2}$$"),
              layout_columns(
                card(
                  card_header("Channel"),
                  selectInput(
                    inputId = "channel_mannings", 
                    label = "Set Manning's n:",
                    choices = mannings_choices),
                  gt_output("channel_discharge")
                ), 
                card(
                  card_header("Floodplain"),
                  selectInput(
                    inputId = "floodplain_mannings", 
                    label = "Set Manning's n:",
                    choices = mannings_choices),
                  gt_output("floodplain_discharge")
                )
              )
            )
          )
        )
      ))
    )
  )
}
