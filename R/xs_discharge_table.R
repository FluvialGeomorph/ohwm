#' @title Create a Cross Section Discharge Table
#'
#' @description
#' Creates a cross section discharge table for the channel portion of the
#' specified cross section.
#'
#' @export
#' @param xs_pts        sf; A cross section lines feature class.
#' @param xs_number     integer; The cross section `Seq` number of the
#'                      requested cross section.
#' @param bf_estimate   numeric; Detrended bankfull estimate (units:
#'                      detrended feet).
#' @param mannings_n    numeric; The Manning's n coeficient.
#'
#' @return a `gt` object
#'
#' @importFrom fluvgeo slope_sinuosity xs_dimensions
#' @importFrom dplyr group_by slice_min filter .data distinct select mutate 
#'                   rename ungroup left_join join_by relocate recode 
#'                   across arrange
#' @importFrom tidyr pivot_longer
#' @importFrom nhdplusTools discover_nhdplus_id subset_nhdplus
#' @importFrom gt gt fmt_number cols_label_with cols_label tab_options px
#'
xs_discharge_table <- function(xs_pts, xs_number, bf_estimate, mannings_n) {

    # Calculate the slope from adjacent cross sections
  xs_ss <- xs_pts %>%
    group_by(Seq) %>%
    slice_min(DEM_Z, n = 1) %>%
    rename(Z = DEM_Z) %>%
    slope_sinuosity(lead_n = 1, lag_n = 1, use_smoothing = FALSE, 
                    vert_units = "ft") %>%
    ungroup()
  
  # Get reach slope from nhdPlus flowline
  xs <- xs_ss %>%
    filter(.data$Seq == xs_number) 
  
  point_sfc <- sf::st_sfc(sf::st_point(x = c(xs$POINT_X, xs$POINT_Y)), 
                                       crs = 3857)
  start_comid <- discover_nhdplus_id(
    point = point_sfc, 
    nldi_feature = "comid",
    raindrop = TRUE)
  
  output_file <- tempfile(fileext = ".gpkg")
  nhd_flowline <- subset_nhdplus(
    comids = c(start_comid$comid[1]),
    output_file = output_file,
    nhdplus_data = "download",
    overwrite = TRUE, status = FALSE, flowline_only = TRUE)
  unlink(output_file)
  
  nhd_slope <- nhd_flowline[1]$NHDFlowline_Network$slope
  
  # Get the channel portion of the current cross section
  xs_pts_channel <- xs_pts %>%
    filter(.data$Seq == xs_number) %>%
    filter(.data$channel == 1)
  
  # Calculate channel dimensions
  dims <- fluvgeo::xs_dimensions(
    xs_points = xs_pts_channel,
    streams = unique(xs_pts_channel$ReachName),
    regions = c("USA", "Eastern United States"),
    bankfull_elevations = bf_estimate)
  
  # Wrangle the discharge calculations
  dims_table <- dims %>%
    distinct() %>%
    left_join(xs_ss, join_by(cross_section == Seq)) %>%
    select(-c("reach_name", "cross_section", "bankfull_elevation", 
              "discharge", "ID", "ReachName")) %>%
    mutate(xs_type = recode(xs_type, 
                            "DEM derived cross section" = "DEM derived")) %>%
    mutate(nhd_slope = nhd_slope) %>%
    mutate(mannings_n = mannings_n) %>%
    filter(xs_type == "DEM derived") %>%
    mutate(R_proxy = xs_depth) %>%
    mutate(R = xs_depth ^ (2/3)) %>%
    mutate(S_proxy = nhd_slope) %>%
    mutate(S = nhd_slope ^ (1/2)) %>%
    mutate(Q = (1.486 / mannings_n) * xs_area * R * S) %>%
    mutate(V = Q / xs_area) %>%
    select(c(xs_area, xs_width, xs_depth, drainage_area, 
             R_proxy, S_proxy, Q, V))
    
  dims_table_long <- dims_table %>%
    pivot_longer(everything()) %>%
    mutate(units = c("sq ft", "ft", "ft", "sq mi", "ft", "", "cfs", "ft sec")) %>%
    mutate(label = c("XS Area", "XS Width", "XS Mean Depth", "Drainage Area",
                     "XS Hydraulic Radius (R)", "Slope (S)", 
                     "Channel Flow (Q)", "Channel Velocity (V)")) %>%
    relocate(label, .before = name) %>%
    select(-name)
  
  gt_table <- dims_table_long |>
    gt() |>
    cols_label_with(fn = tools::toTitleCase) |>
    cols_label(label = "Variable") |>
    fmt_number(columns = value, decimals = 1) |>
    fmt_number(columns = value, row = c(6), decimals = 4) |>
    tab_options(
      column_labels.font.weight = "bold",
      table.font.size = "small",
      column_labels.padding = px(2),
      data_row.padding = px(1))
  #gt_table
  return(gt_table)
}
