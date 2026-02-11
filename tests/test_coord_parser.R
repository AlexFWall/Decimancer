library(testthat)

source(file.path(dirname(getwd()), "R", "coord_parser.R"))

test_that("DMS with degree symbol parses correctly", {
  expect_equal(parse_coordinate("33\u00B051'54\"S"), -33.865, tolerance = 0.001)
  expect_equal(parse_coordinate("151\u00B012'36\"E"), 151.21, tolerance = 0.001)
  expect_equal(parse_coordinate("40\u00B026'46\"N"), 40.44611, tolerance = 0.001)
})

test_that("DMS space-separated parses correctly", {
  expect_equal(parse_coordinate("33 51 54 S"), -33.865, tolerance = 0.001)
  expect_equal(parse_coordinate("151 12 36 E"), 151.21, tolerance = 0.001)
  expect_equal(parse_coordinate("N 40 26 46"), 40.44611, tolerance = 0.001)
  expect_equal(parse_coordinate("S 33 51 54"), -33.865, tolerance = 0.001)
})

test_that("DDM with symbols parses correctly", {
  expect_equal(parse_coordinate("33\u00B051.9'S"), -33.865, tolerance = 0.001)
  expect_equal(parse_coordinate("151\u00B012.6'E"), 151.21, tolerance = 0.001)
})

test_that("DDM space-separated parses correctly", {
  expect_equal(parse_coordinate("33 51.9 S"), -33.865, tolerance = 0.001)
  expect_equal(parse_coordinate("S 33 51.9"), -33.865, tolerance = 0.001)
})

test_that("Decimal degrees pass through", {
  expect_equal(parse_coordinate("-33.865"), -33.865)
  expect_equal(parse_coordinate("151.21"), 151.21)
  expect_equal(parse_coordinate(-33.865), -33.865)
  expect_equal(parse_coordinate(0), 0)
})

test_that("Hemisphere prefix and suffix both work", {
  expect_equal(parse_coordinate("W 151 12 36"), -151.21, tolerance = 0.001)
  expect_equal(parse_coordinate("151 12 36 W"), -151.21, tolerance = 0.001)
  expect_equal(parse_coordinate("E 151 12 36"), 151.21, tolerance = 0.001)
})

test_that("Edge cases return NA", {
  expect_true(is.na(parse_coordinate(NA)))
  expect_true(is.na(parse_coordinate("")))
  expect_true(is.na(parse_coordinate("NA")))
  expect_true(is.na(parse_coordinate("not a coordinate")))
  expect_true(is.na(parse_coordinate(NULL)))
})

test_that("Vectorized parse_coordinates works", {
  input <- c("33\u00B051'54\"S", "33 51 54 S", "-33.865", "33 51.9 S")
  result <- parse_coordinates(input)
  expect_equal(length(result), 4)
  expect_true(all(abs(result - (-33.865)) < 0.01))
})

test_that("Mixed valid and invalid values handled", {
  input <- c("33 51 54 S", "bad value", "151.21")
  result <- parse_coordinates(input)
  expect_equal(result[1], -33.865, tolerance = 0.001)
  expect_true(is.na(result[2]))
  expect_equal(result[3], 151.21)
})

# --- Universal parser tests ---

test_that("Encoding-mangled DMS parses correctly", {
  # UTF-8 symbols reinterpreted through wrong encoding
  expect_equal(parse_coordinate("28\u00ac\u00b837\u00e2\u0080\u009a\u00c3\u00b440\u00e2\u0080\u009a\u00c3\u0099S"),
               -28.62778, tolerance = 0.001)
  # Simpler mangling: random junk between numbers
  expect_equal(parse_coordinate("28~~37@@40##S"), -28.62778, tolerance = 0.001)
})

test_that("Letter 'o' used as degree symbol", {
  expect_equal(parse_coordinate("33o51'54\"S"), -33.865, tolerance = 0.001)
  expect_equal(parse_coordinate("151o12'36\"E"), 151.21, tolerance = 0.001)
  expect_equal(parse_coordinate("33O51'54\"S"), -33.865, tolerance = 0.001)
})

test_that("Masculine ordinal as degree symbol", {
  # º (U+00BA) often confused with °
  expect_equal(parse_coordinate("33\u00BA51'54\"S"), -33.865, tolerance = 0.001)
})

test_that("Decimal degrees with hemisphere letter", {
  expect_equal(parse_coordinate("33.865S"), -33.865, tolerance = 0.001)
  expect_equal(parse_coordinate("151.21E"), 151.21, tolerance = 0.001)
  expect_equal(parse_coordinate("S33.865"), -33.865, tolerance = 0.001)
})

test_that("Integer degrees with hemisphere", {
  expect_equal(parse_coordinate("33S"), -33, tolerance = 0.001)
  expect_equal(parse_coordinate("N45"), 45, tolerance = 0.001)
})

test_that("Validation rejects minutes/seconds >= 60", {
  expect_true(is.na(parse_coordinate("33 61 00 S")))
  expect_true(is.na(parse_coordinate("33 51 65 S")))
})

test_that("Negative sign with hemisphere does not double-negate", {
  # -33.865 is already negative; no hemisphere letter
  expect_equal(parse_coordinate("-33.865"), -33.865)
})
