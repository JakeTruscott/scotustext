devtools::install("C:/Users/Jake Truscott/OneDrive - University of Georgia/Active/scotustext")
library(testthat)
library(scotustext)


test_that("docket_search returns expected output", {
  docket_id <- c("19-1392", "22A122", "22M101", "22O145")
  rate <- 5000
  sleep <- 30
  include <- NULL
  exclude <- NULL

  # Run the docket_search function
  result <- docket_search(docket_id, rate, sleep, include, exclude)

  # Perform assertions to check the output
  expect_true(is.data.frame(result))  # Check if the result is a data frame
  expect_equal(nrow(result), length(docket_id))  # Check if the number of rows matches the number of docket IDs

  # Example assertion for a specific docket ID
  expect_equal(result$docket_number[1], "19-1392")

  # Example assertion for a specific variable in the output
  expect_true("petitioner" %in% colnames(result))
}) #Docket Search

test_that("oa_search returns expected output", {

  result <- oa_search(term = "2007", justice = "breyer")
  expect_true(is.data.frame(result))
  expect_equal(result$speaker[1], "JUSTICE BREYER")
  expect_true("text" %in% colnames(result))

}) #Oral Argument Search

test_that("oa_parser returns expected output", {

  dir_path = "C:/Users/Jake Truscott/OneDrive - University of Georgia/Active/Justices Oral Argument Rhetoric/test_OAs"

  result <- oa_parser(dir_path = dir_path)
  expect_true(is.data.frame(result))
  expect_true("text" %in% colnames(result))

}) #Oral Argument Parser

test_that("decision_processor returns expected output", {

  dir_path = "C:/Users/Jake Truscott/OneDrive - University of Georgia/Active/Justices Oral Argument Rhetoric/decision_sample"

  result <- decision_processor(dir_path = dir_path)
  expect_true(is.data.frame(result))
  expect_true("text" %in% colnames(result))

}) #Decision Processor

