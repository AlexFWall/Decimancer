source("../R/coord_parser.R")
source("../R/column_detector.R")

# Test with sample CSV
df <- read.csv("test_data/sample_dms.csv", stringsAsFactors = FALSE, check.names = FALSE)
cat("Loaded", nrow(df), "rows\n\n")

# Test column detection
detected <- detect_coord_columns(df)
cat("Detected lat column:", detected$lat_col, "\n")
cat("Detected lon column:", detected$lon_col, "\n\n")

# Convert
df$Latitude_DD <- parse_coordinates(as.character(df[[detected$lat_col]]))
df$Longitude_DD <- parse_coordinates(as.character(df[[detected$lon_col]]))

# Print results
for (i in seq_len(nrow(df))) {
  cat(sprintf("Row %d (%s): %s -> %.4f | %s -> %.4f\n",
              i, df$Description[i],
              df$Latitude[i],
              ifelse(is.na(df$Latitude_DD[i]), NA, df$Latitude_DD[i]),
              df$Longitude[i],
              ifelse(is.na(df$Longitude_DD[i]), NA, df$Longitude_DD[i])))
}

cat("\nSuccess: lat =", sum(!is.na(df$Latitude_DD)), "/", nrow(df), "\n")
cat("Success: lon =", sum(!is.na(df$Longitude_DD)), "/", nrow(df), "\n")
