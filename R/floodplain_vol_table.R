floodplain_vol_table <- function(channel_vol, floodplain_vol) {
  assert_that(is.numeric(channel_vol),
              msg = "channel_vol must be numeric")
  assert_that(is.numeric(floodplain_vol),
              msg = "floodplain_vol must be numeric")
  
  vols_tbl <- 
    tibble(
      features = c("Channel", "Floodplain"), 
      volume   = c(channel_vol, floodplain_vol)
    )
  
}