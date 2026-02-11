# CoordConverter - Core Coordinate Parsing Functions
# Converts coordinates from DMS, DDM, and DD formats to decimal degrees.

#' Normalize a coordinate string by replacing Unicode symbols with ASCII equivalents.
normalize_coord_string <- function(x) {
  x <- gsub("\u00B0", "d", x)                       # degree symbol -> d
  x <- gsub("[\u2032\u2018\u2019\u02BC]", "'", x)   # prime / smart quotes -> '
  x <- gsub("[\u2033\u201C\u201D]", "\"", x)        # double prime / smart quotes -> "
  x <- gsub(",", "", x)                              # strip commas
  trimws(x)
}

#' Determine hemisphere from prefix and/or suffix indicators.
#' Returns "N", "S", "E", "W", or NA_character_.
determine_hemisphere <- function(prefix, suffix) {
  suffix <- toupper(trimws(suffix))
  prefix <- toupper(trimws(prefix))
  if (nchar(suffix) > 0 && suffix %in% c("N", "S", "E", "W")) return(suffix)
  if (nchar(prefix) > 0 && prefix %in% c("N", "S", "E", "W")) return(prefix)
  NA_character_
}

#' Parse DMS with symbols: 33d51'54"S, 151d12'36.5"E
parse_dms_symbols <- function(x) {
  pat <- paste0(
    "^\\s*([NSEW])?\\s*",
    "(\\d{1,3})\\s*d\\s*",
    "(\\d{1,2})\\s*'\\s*",
    "([\\d.]+)\\s*\"?\\s*",
    "([NSEW])?\\s*$"
  )
  m <- regmatches(x, regexec(pat, x, ignore.case = TRUE, perl = TRUE))[[1]]
  if (length(m) == 0) return(NA_real_)

  hemi <- determine_hemisphere(m[2], m[6])
  deg  <- as.numeric(m[3])
  min  <- as.numeric(m[4])
  sec  <- as.numeric(m[5])

  if (min >= 60 || sec >= 60) return(NA_real_)
  dd <- deg + min / 60 + sec / 3600
  if (!is.na(hemi) && hemi %in% c("S", "W")) dd <- -dd
  dd
}

#' Parse DDM with symbols: 33d51.9'S, 151d12.6'E
parse_ddm_symbols <- function(x) {
  pat <- paste0(
    "^\\s*([NSEW])?\\s*",
    "(\\d{1,3})\\s*d\\s*",
    "([\\d.]+)\\s*'\\s*",
    "([NSEW])?\\s*$"
  )
  m <- regmatches(x, regexec(pat, x, ignore.case = TRUE, perl = TRUE))[[1]]
  if (length(m) == 0) return(NA_real_)

  hemi <- determine_hemisphere(m[2], m[5])
  deg  <- as.numeric(m[3])
  min  <- as.numeric(m[4])

  if (min >= 60) return(NA_real_)
  dd <- deg + min / 60
  if (!is.na(hemi) && hemi %in% c("S", "W")) dd <- -dd
  dd
}

#' Parse DMS space-separated: 33 51 54 S, S 33 51 54
parse_dms_spaces <- function(x) {
  pat <- paste0(
    "^\\s*([NSEW])?\\s*",
    "(\\d{1,3})\\s+",
    "(\\d{1,2})\\s+",
    "([\\d.]+)\\s*",
    "([NSEW])?\\s*$"
  )
  m <- regmatches(x, regexec(pat, x, ignore.case = TRUE, perl = TRUE))[[1]]
  if (length(m) == 0) return(NA_real_)

  hemi <- determine_hemisphere(m[2], m[6])
  deg  <- as.numeric(m[3])
  min  <- as.numeric(m[4])
  sec  <- as.numeric(m[5])

  if (min >= 60 || sec >= 60) return(NA_real_)
  dd <- deg + min / 60 + sec / 3600
  if (!is.na(hemi) && hemi %in% c("S", "W")) dd <- -dd
  dd
}

#' Parse DDM space-separated: 33 51.9 S, S 33 51.9
parse_ddm_spaces <- function(x) {
  pat <- paste0(
    "^\\s*([NSEW])?\\s*",
    "(\\d{1,3})\\s+",
    "([\\d.]+)\\s*",
    "([NSEW])?\\s*$"
  )
  m <- regmatches(x, regexec(pat, x, ignore.case = TRUE, perl = TRUE))[[1]]
  if (length(m) == 0) return(NA_real_)

  hemi <- determine_hemisphere(m[2], m[5])
  deg  <- as.numeric(m[3])
  min  <- as.numeric(m[4])

  if (min >= 60) return(NA_real_)
  dd <- deg + min / 60
  if (!is.na(hemi) && hemi %in% c("S", "W")) dd <- -dd
  dd
}

#' Universal parser: ignores separators entirely, extracts numbers and hemisphere.
#' Handles any encoding mangling (e.g. "28¬∫37‚Äô40‚ÄùS") by focusing on the
#' numeric tokens rather than trying to recognise separator characters.
#'
#' Logic:
#'   1. Extract hemisphere letter (N/S/E/W) from start or end
#'   2. Extract leading sign (-/+)
#'   3. Extract all numeric tokens (digit groups with optional decimal point)
#'   4. Decide format by count and decimal placement:
#'      - 1 number  -> decimal degrees (or integer degrees)
#'      - 2 numbers -> degrees + decimal minutes (DDM)
#'      - 3 numbers -> degrees + minutes + seconds (DMS)
#'   5. Negate if S/W or negative sign
parse_universal <- function(x) {
  s <- trimws(x)
  if (nchar(s) == 0) return(NA_real_)

  # 1. Extract hemisphere from start or end
  hemi_prefix <- ""
  hemi_suffix <- ""
  if (grepl("^[NSEWnsew]", s)) {
    hemi_prefix <- toupper(substr(s, 1, 1))
    s <- trimws(substring(s, 2))
  }
  if (grepl("[NSEWnsew]\\s*$", s)) {
    hemi_suffix <- toupper(sub(".*([NSEWnsew])\\s*$", "\\1", s))
    s <- trimws(sub("[NSEWnsew]\\s*$", "", s))
  }
  hemi <- determine_hemisphere(hemi_prefix, hemi_suffix)

  # 2. Extract leading sign
  negative <- FALSE
  if (grepl("^[-\u2212]", s)) {
    negative <- TRUE
    s <- trimws(substring(s, 2))
  } else if (grepl("^\\+", s)) {
    s <- trimws(substring(s, 2))
  }

  # 3. Extract all numeric tokens (integers or decimals)
  tokens <- regmatches(s, gregexpr("\\d+\\.?\\d*", s, perl = TRUE))[[1]]
  if (length(tokens) == 0) return(NA_real_)

  nums <- as.numeric(tokens)

  # 4. Decide format based on token count
  dd <- NA_real_
  if (length(nums) == 1) {
    # Decimal degrees (e.g. "33.865") or integer degrees (e.g. "33")
    dd <- nums[1]

  } else if (length(nums) == 2) {
    # Degrees + decimal minutes (DDM)
    deg <- nums[1]
    min <- nums[2]
    if (min >= 60) return(NA_real_)
    dd <- deg + min / 60

  } else if (length(nums) >= 3) {
    # Degrees + minutes + seconds (DMS), use first 3 tokens
    deg <- nums[1]
    min <- nums[2]
    sec <- nums[3]
    if (min >= 60 || sec >= 60) return(NA_real_)
    dd <- deg + min / 60 + sec / 3600
  }

  if (is.na(dd)) return(NA_real_)

  # 5. Apply sign/hemisphere
  if (negative) dd <- -dd
  if (!is.na(hemi) && hemi %in% c("S", "W")) dd <- -dd

  dd
}

#' Parse a single coordinate string into decimal degrees.
#' Tries the universal parser first (handles any separator encoding), then
#' falls back to the legacy regex parsers for edge cases.
#' Returns NA_real_ if unparseable.
parse_coordinate <- function(x) {
  if (is.na(x) || is.null(x)) return(NA_real_)

  # Already numeric — pass through
  if (is.numeric(x)) return(as.numeric(x))

  x_str <- trimws(as.character(x))
  if (x_str == "" || toupper(x_str) == "NA") return(NA_real_)

  # Plain numeric string (decimal degrees like -33.865 or 151.21)
  numeric_val <- suppressWarnings(as.numeric(x_str))
  if (!is.na(numeric_val)) return(numeric_val)

  # Try the universal parser (separator-agnostic)
  result <- parse_universal(x_str)
  if (!is.na(result)) return(result)

  # Fall back to legacy regex parsers for anything universal missed
  x_clean <- normalize_coord_string(x_str)

  result <- parse_dms_symbols(x_clean)
  if (!is.na(result)) return(result)

  result <- parse_ddm_symbols(x_clean)
  if (!is.na(result)) return(result)

  result <- parse_dms_spaces(x_clean)
  if (!is.na(result)) return(result)

  result <- parse_ddm_spaces(x_clean)
  if (!is.na(result)) return(result)

  NA_real_
}

#' Parse a vector of coordinate strings into decimal degrees.
parse_coordinates <- function(x) {
  vapply(x, parse_coordinate, numeric(1), USE.NAMES = FALSE)
}
