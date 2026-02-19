# CoordConverter - Column Auto-Detection
# Identifies likely latitude and longitude columns by name matching.

#' Find the best matching column name from a list of regex patterns.
#' Returns the original column name (preserving case) or NULL.
find_best_match <- function(col_lower, col_names, patterns) {
  for (pat in patterns) {
    idx <- grep(pat, col_lower)
    if (length(idx) >= 1) return(col_names[idx[1]])
  }
  NULL
}

#' Detect which columns likely contain latitude and longitude.
#' Returns list(lat_col = "Name" or NULL, lon_col = "Name" or NULL).
detect_coord_columns <- function(df) {
  col_names <- names(df)
  col_lower <- tolower(col_names)

  lat_patterns <- c(
    "^lat$", "^latitude$", "^lat_dd$", "^decimallatitude$",
    "^y$", "^lat[_.]", "latitude", "lat"
  )
  lon_patterns <- c(
    "^lon$", "^lng$", "^long$", "^longitude$", "^lon_dd$",
    "^decimallongitude$", "^x$", "^lon[_.]", "^long[_.]",
    "longitude", "long", "lng"
  )

  list(
    lat_col = find_best_match(col_lower, col_names, lat_patterns),
    lon_col = find_best_match(col_lower, col_names, lon_patterns)
  )
}
