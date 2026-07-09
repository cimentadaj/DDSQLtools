# These tests hit the live UN DemoData API. Guard every networked block with
# skip_on_cran() (never run on CRAN) and skip_if_offline() (skip when the host
# is unreachable) so an external-server outage does not fail CI.
skip_if_no_api <- function() {
  testthat::skip_on_cran()
  testthat::skip_if_offline("population.un.org")
}

test_that("The linkGenerator() works fine", {
  skip_if_no_api()
  L <- linkGenerator(
    type = "structureddatarecords",
    locIds = 4,
    indicatorTypeIds = 8,
    dataProcessTypeIds = c(2, 6),
    verbose = FALSE
  )

  expect_output(print(L)) # 1. Always expect an output;
  expect_true(is.character(L)) # 2. The output is of the class "character";
  expect_error(linkGenerator(wrong_argument = 1)) # 3. Does not work with whatever argument in "...";
  expect_error(linkGenerator(type = "countryy")) # 4. Is sensitive to typos;
  expect_equal(length(strsplit(L, split = " ")[[1]]), 1) # 5. Expect no spaces in the string.

  # Emits the new base URL and the canonical camelCase route.
  expect_match(L, "population.un.org/demodata-api/api/", fixed = TRUE)
  expect_match(L, "structuredDataRecords", fixed = TRUE)
})


## Test API functions
validate_read_API <- function(Z) {
  test_that("The read_API works fine", {
    skip_if_no_api()
    expect_output(print(Z)) # 1. Always expect an output;
    expect_true(is.data.frame(Z)) # 2. The output is of the class "data.frame";
    expect_true(ncol(Z) >= 2) # 3. The output has al least 2 columns;
    expect_false(any(is.null(colnames(Z)))) # 4. All columns have names;
    expect_true(nrow(Z) >= 1) # 5. The output has at least 1 rows
  })
}

validate_recordddata <- function(x) {
  validate_read_API(x) # validate
  test_that("record data has no NA in DataCatalogID", {
    skip_if_no_api()
    # Test there aren't any NA in DataCatalogID
    expect_true(!any(is.na(x$DataCatalogID)))
  })
}

# The networked fetches below run at file-source time, i.e. outside any
# test_that() block, so a network failure there would error the whole file
# instead of skipping. Wrap them in a single guarded block that assigns the
# shared objects to the enclosing environment so the later test blocks can
# still reference them; on skip the objects stay NULL and their (also guarded)
# assertions skip too.
D <- NULL
D_datacatalog <- NULL
D_datacatalog_sub <- NULL
D_datacatalog_nat <- NULL
S <- NULL
L <- NULL
P <- NULL
IT <- NULL
II <- NULL
I <- NULL
DS <- NULL
DT <- NULL
DP <- NULL
G <- NULL
X <- NULL
Y <- NULL
mixed <- NULL
mixed_dataid <- NULL
mixed_codes <- NULL
add_chr <- NULL
add_num <- NULL
add_chr2 <- NULL
add_num2 <- NULL
res_extract <- NULL

test_that("live reference and record endpoints fetch without error", {
  skip_if_no_api()

  # ---- Reference endpoints (one live call per accessor) ----
  D <<- get_dataprocesstype()

  D_datacatalog <<- get_datacatalog()
  D_datacatalog_sub <<- get_datacatalog(isSubnational = TRUE)
  D_datacatalog_nat <<- get_datacatalog(isSubnational = FALSE)

  S <<- get_subgroups(
    indicatorTypeIds = 8, # Population by age and sex indicator;
    locIds = 818, # Egypt
    isComplete = 0
  )

  L <<- get_locations(
    includeDependencies = "false",
    includeFormerCountries = "false"
  )

  P <<- get_locationtypes(
    indicatorTypeIds = 8,
    locIds = 818,
    isComplete = 0
  )

  IT <<- get_indicatortypes()
  II <<- get_iitypes()
  I <<- get_indicators()
  DS <<- get_datasources()
  DT <<- get_datatypes()
  DP <<- get_dataprocess()

  G <<- get_seriesdata(
    dataProcessTypeIds = 2,
    indicatorTypeIds = 8,
    isComplete = 0,
    locIds = 4,
    locAreaTypeIds = 2,
    startYear = 1950,
    subGroupIds = 2
  )

  # ---- Record endpoints ----
  X <<- get_recorddata(
    dataProcessTypeIds = 2, # Census
    indicatorTypeIds = 8, # Population by age and sex - abridged
    locIds = 818, # Egypt
    locAreaTypeIds = 2, # Whole area
    subGroupIds = 2, # Total or All groups
    isComplete = 0
  ) # Age Distribution: Abridged

  # Check whether it successfully accepts strings rather than codes
  Y <<- get_recorddata(
    dataProcessTypeIds = "Census",
    indicatorTypeIds = "Population by age and sex",
    locIds = "Egypt",
    locAreaTypeIds = "Whole area",
    subGroupIds = "Total or All groups",
    isComplete = "Abridged"
  )

  # Check whether it successfully accepts mixed cases
  mixed <<- get_recorddata(
    dataProcessTypeIds = "census",
    indicatorTypeIds = "population by age and sex",
    locIds = "egypt",
    locAreaTypeIds = "Whole area",
    subGroupIds = "Total or All groups",
    isComplete = "Abridged"
  )

  # Check whether we can translate with dataProcessIds
  mixed_dataid <<- get_recorddata(
    dataProcessIds = "Population and Housing Census",
    startYear = 1920,
    endYear = 2020,
    indicatorIds = 58,
    isComplete = 0,
    locIds = 4,
    locAreaTypeIds = 2,
    subGroupIds = 2
  )

  # mixed with codes
  mixed_codes <<- get_recorddata(
    dataProcessTypeIds = 2, # Census
    indicatorTypeIds = 8, # Population by age and sex - abridged
    locIds = 818, # Egypt
    locAreaTypeIds = "Whole area", # Whole area
    subGroupIds = "Total or All groups", # Total or All groups
    isComplete = "Abridged"
  )

  ## For dataTypeGroupIds - translate the string
  add_chr <<- get_recorddataadditional(
    dataTypeGroupIds = "Direct",
    indicatorTypeIds = 8,
    isComplete = 0,
    locIds = 818,
    locAreaTypeIds = 2,
    subGroupIds = 2
  )

  ## For dataTypeGroupIds - with an id
  add_num <<- get_recorddataadditional(
    dataTypeGroupIds = 3,
    indicatorTypeIds = 8,
    isComplete = 0,
    locIds = 818,
    locAreaTypeIds = 2,
    subGroupIds = 2
  )

  ## For dataTypeGroupId2s - translate the string
  add_chr2 <<- get_recorddataadditional(
    dataTypeGroupId2s = "Population (sample tabulation)",
    indicatorTypeIds = 8,
    isComplete = 0,
    locIds = 818,
    locAreaTypeIds = 2,
    subGroupIds = 2
  )

  ## For dataTypeGroupId2s - with an id
  add_num2 <<- get_recorddataadditional(
    dataTypeGroupId2s = 11,
    indicatorTypeIds = 8,
    isComplete = 0,
    locIds = 818,
    locAreaTypeIds = 2,
    subGroupIds = 2
  )

  res_extract <<- extract_data("183578537")

  succeed()
})

# ------------------------------------------
# Per-accessor validation. Each block is guarded via validate_read_API()'s
# internal skip_if_no_api(); when the fetch above skipped the object is NULL and
# these blocks skip as well.

validate_read_API(D) # get_dataprocesstype

validate_recordddata(D_datacatalog) # get_datacatalog
validate_recordddata(D_datacatalog_sub) # get_datacatalog(isSubnational = TRUE)
validate_recordddata(D_datacatalog_nat) # get_datacatalog(isSubnational = FALSE)

validate_read_API(S) # get_subgroups
validate_read_API(L) # get_locations
validate_read_API(P) # get_locationtypes
validate_read_API(IT) # get_indicatortypes
validate_read_API(II) # get_iitypes
validate_read_API(I) # get_indicators
validate_read_API(DS) # get_datasources
validate_read_API(DT) # get_datatypes
validate_read_API(DP) # get_dataprocess
validate_read_API(G) # get_seriesdata

validate_recordddata(X) # get_recorddata (codes)
validate_recordddata(Y) # get_recorddata (strings)
validate_recordddata(mixed) # get_recorddata (mixed case)
validate_recordddata(mixed_dataid) # get_recorddata (dataProcessIds)
validate_recordddata(mixed_codes) # get_recorddata (mixed codes/strings)

validate_recordddata(add_chr) # get_recorddataadditional (dataTypeGroupIds string)
validate_recordddata(add_num) # get_recorddataadditional (dataTypeGroupIds id)
validate_recordddata(add_chr2) # get_recorddataadditional (dataTypeGroupId2s string)
validate_recordddata(add_num2) # get_recorddataadditional (dataTypeGroupId2s id)

validate_read_API(res_extract) # extract_data

# ------------------------------------------
test_that("reference endpoints reconstruct their PK_ primary-key columns", {
  skip_if_no_api()
  expect_true("PK_LocID" %in% names(get_locations()))
  expect_true("PK_IndicatorTypeID" %in% names(get_indicatortypes()))
  expect_true("PK_SubGroupID" %in% names(get_subgroups(
    indicatorTypeIds = 8,
    locIds = 818,
    isComplete = 0
  )))
  expect_true("PK_DataProcessTypeID" %in% names(get_dataprocesstype()))
  expect_true("PK_DataProcessID" %in% names(get_dataprocess()))
})

# ------------------------------------------
test_that("get_datacatalog joins reference tables and has non-NA DataCatalogID", {
  skip_if_no_api()
  cat <- get_datacatalog()
  expect_false(any(is.na(cat$DataCatalogID)))
  expect_true(all(c("LocName", "ShortName", "ReferenceYearStart") %in% names(cat)))
})

# ------------------------------------------
test_that("get_iitypes can subset correctly", {
  skip_if_no_api()
  # No need to test each argument separately
  # otherwise the tests run the risk of running for
  # longer and longer.

  # Here I'm testing all argument combined. If they
  # work, it should be the same
  x <- get_iitypes(
    componentIds = 4,
    indicatorTypeIds = 38,
    indicatorIds = 323
  )
  expect_equal(unique(x[["IndicatorTypeComponentId"]]), 4)
  expect_equal(unique(x[["IndicatorTypeId"]]), 38)
  expect_true(323 %in% x[["IndicatorId"]])
})


# ------------------------------------------
test_that("get_recorddata returns error when setting wrong server", {
  skip_if_no_api()
  # After changing the unpd server
  old_server <- getOption("unpd_server")
  options(unpd_server = "http://0.0.0.0/")
  on.exit(options(unpd_server = old_server), add = TRUE)

  expect_error(
    suppressWarnings(
      get_recorddata(
        dataProcessTypeIds = 2, # Census
        indicatorTypeIds = 8, # Population by age and sex - abridged
        locIds = 818, # Egypt
        locAreaTypeIds = "Whole area", # Whole area
        subGroupIds = "Total or All groups", # Total or All groups
        isComplete = "Abridged"
      ) # Age Distribution: Abridged
    )
  )
})

test_that("get_recorddata with codes gives same output with strings", {
  skip_if_no_api()
  Xo <- X[order(X$StructuredDataID), ]
  Yo <- Y[order(Y$StructuredDataID), ]
  mixedo <- mixed[order(mixed$StructuredDataID), ]
  mixed_codeso <- mixed_codes[order(mixed_codes$StructuredDataID), ]

  row.names(Xo) <- NULL
  row.names(Yo) <- NULL
  row.names(mixedo) <- NULL
  row.names(mixed_codeso) <- NULL

  expect_equal(Xo, Yo)
  expect_equal(Xo, mixedo)
  expect_equal(Xo, mixed_codeso)
})


# Called from within a test_that() block, so it does not open its own.
validate_date <- function(res) {
  expect_type(res$TimeStart, "character")
  expect_type(res$TimeEnd, "character")

  # Here I'm testing that days, months and years have 2, 2 and 4
  # digits. The total is 8 plus the two slashes. Here we make sure
  # that we always have 10 characters.
  expect_equal(10, unique(nchar(res$TimeStart)))
  expect_equal(10, unique(nchar(res$TimeEnd)))

  # Test that the structure is 2 digits / 2 digits / 4 digits
  expect_true(all(grepl("^[0-9]{2}/[0-9]{2}/[0-9]{4}$", res$TimeStart)))
  expect_true(all(grepl("^[0-9]{2}/[0-9]{2}/[0-9]{4}$", res$TimeEnd)))

  # The new API returns integer years, synthesized to 01/01/<year>
  expect_true(all(grepl("^01/01/[0-9]{4}$", res$TimeStart)))
  expect_true(all(grepl("^01/01/[0-9]{4}$", res$TimeEnd)))
}

test_that("get_recorddata transforms TimeStart/TimeEnd to DD/MM/YYYY (01/01/YYYY)", {
  skip_if_no_api()
  res <- get_recorddata(dataProcessTypeIds = 9, # Register
                        startYear = 1920,
                        endYear = 2020,
                        indicatorTypeIds = 14, # Births by sex
                        isComplete = 2, # Total
                        locIds = 28, # Antigua and Barbuda
                        locAreaTypeIds = 2, # Whole area
                        subGroupIds = 2) # Total
  validate_date(res)
})


test_that("get_recorddata and get_recorddataadditional transform Name columns to labels", {
  skip_if_no_api()
  res <- get_recorddata(
    dataProcessTypeIds = 2, # Census
    indicatorTypeIds = 8, # Population by age and sex - abridged
    locIds = 818, # Egypt
    locAreaTypeIds = 2, # Whole area
    subGroupIds = 2, # Total or All groups
    isComplete = 0,
    collapse_id_name = TRUE
  ) # Age Distribution: Abridged

  res_additional <- get_recorddataadditional(
    dataProcessTypeIds = 2, # Census
    indicatorTypeIds = 8, # Population by age and sex - abridged
    locIds = 818, # Egypt
    locAreaTypeIds = 2, # Whole area
    subGroupIds = 2, # Total or All groups
    isComplete = 0,
    collapse_id_name = TRUE
  ) # Age Distribution: Abridged


  subset_names <- res[names(values_env$id_to_fact)]
  subset_names_additional <- res_additional[names(values_env$id_to_fact)]

  expect_true(
    all(
      vapply(subset_names, function(x) inherits(x, "haven_labelled"),
        FUN.VALUE = logical(1)
      )
    )
  )

  expect_true(
    all(
      vapply(subset_names_additional, function(x) inherits(x, "haven_labelled"),
        FUN.VALUE = logical(1)
      )
    )
  )


  ## TODO
  ## You might want to add another test that checks that the ID
  ## in the label is a numeric and and value is a character
  ## to make sure you never mix them up.
})

test_that("get_recorddata and get_recorddataadditional keep the correct columns when collapse_id_name is set to different values", {
  skip_if_no_api()

  collapse_opts <- c(TRUE, FALSE)
  cols_available <-
    list(
      setdiff(values_env$col_order, values_env$id_to_fact),
      values_env$col_order
    )

  for (ind in seq_along(cols_available)) {
    res <- get_recorddata(
      dataProcessTypeIds = 2, # Census
      indicatorTypeIds = 8, # Population by age and sex - abridged
      locIds = 818, # Egypt
      locAreaTypeIds = 2, # Whole area
      subGroupIds = 2, # Total or All groups
      isComplete = 0,
      includeUncertainty = TRUE,
      collapse_id_name = collapse_opts[ind]
    ) # Age Distribution: Abridged

    expect_equal(colnames(res), cols_available[[ind]])

    res <- get_recorddataadditional(
      dataProcessTypeIds = 2, # Census
      indicatorTypeIds = 8, # Population by age and sex - abridged
      locIds = 818, # Egypt
      locAreaTypeIds = 2, # Whole area
      subGroupIds = 2, # Total or All groups
      isComplete = 0,
      includeUncertainty = TRUE,
      collapse_id_name = collapse_opts[ind]
    ) # Age Distribution: Abridged

    expect_equal(colnames(res), cols_available[[ind]])
  }

})


test_that("Looking up wrong input throws errors in get_recorddata", {
  skip_if_no_api()
  expect_error(get_recorddata(locIds = "Wrong country"),
    regexp = "Location(s) 'Wrong country' not found. Check get_locations()",
    fixed = TRUE
  )

  expect_error(get_recorddata(indicatorTypeIds = "Wrong"),
    regexp = "Location(s) 'Wrong' not found. Check get_indicatortypes()",
    fixed = TRUE
  )

  expect_error(get_recorddata(subGroupIds = "Wrong"),
    regexp = "Location(s) 'Wrong' not found. Check get_subgroups()",
    fixed = TRUE
  )

  expect_error(get_recorddata(isComplete = "Wrong"),
    regexp = "IsComplete does not accept string 'Wrong'. Only 'abridged', 'complete', 'total'.",
    fixed = TRUE
  )
})

test_that("extract_data returns the requested StructuredDataIDs", {
  skip_if_no_api()
  ids <- "183578537"
  res <- extract_data(ids)
  expect_true(is.data.frame(res))
  expect_true(nrow(res) >= 1)
  expect_true(all(ids %in% as.character(res$StructuredDataID)))
})

test_that("isComplete is set to 'Total' by default", {
  skip_if_no_api()
  myLocations <- 28
  # A request without specifying `isComplete`
  births <- get_recorddata(dataProcessTypeIds = 9,
                           startYear = 1920,
                           endYear = 2020,
                           indicatorTypeIds = 14,
                           locIds = myLocations,
                           locAreaTypeIds = 2,
                           subGroupIds = 2)

  # Same request specifying that it's complete is set to 'Total' (2)
  births_iscomplete <- get_recorddata(dataProcessTypeIds = 9,
                                      startYear = 1920,
                                      endYear = 2020,
                                      indicatorTypeIds = 14,
                                      isComplete = 2,
                                      locIds = myLocations,
                                      locAreaTypeIds = 2,
                                      subGroupIds = 2)

  # Both results are the same. The server does not guarantee row order,
  # so compare the records as a set rather than positionally.
  ord <- function(x) {
    x <- x[order(x$StructuredDataID), , drop = FALSE]
    rownames(x) <- NULL
    x
  }
  expect_identical(ord(births), ord(births_iscomplete))
})

test_that("get_recorddata grabs uncertainty columns when includeUncertainty = TRUE", {
  skip_if_no_api()
  uncertainty_cols <- c(
    "HasUncertaintyRecord",
    "StandardErrorValue",
    "ConfidenceInterval",
    "ConfidenceIntervalLowerBound",
    "ConfidenceIntervalUpperBound"
  )

  X <- get_recorddata(
    dataProcessTypeIds = 2,
    indicatorTypeIds = 8,
    locIds = 818,
    locAreaTypeIds = 2,
    subGroupIds = 2,
    isComplete = 0
  )

  # If includeUncertainty is NULL (default), the columns
  # are NOT included.
  expect_false(all(uncertainty_cols %in% names(X)))

  X <- get_recorddata(
    dataProcessTypeIds = 2,
    indicatorTypeIds = 8,
    locIds = 818,
    locAreaTypeIds = 2,
    subGroupIds = 2,
    isComplete = 0,
    includeUncertainty = FALSE
  )

  # If includeUncertainty is FALSE, the columns
  # are NOT included.
  expect_false(all(uncertainty_cols %in% names(X)))

  X <- get_recorddata(
    dataProcessTypeIds = 2,
    indicatorTypeIds = 8,
    locIds = 818,
    locAreaTypeIds = 2,
    subGroupIds = 2,
    isComplete = 0,
    includeUncertainty = TRUE
  )

  # If includeUncertainty is TRUE, the columns
  # ARE included.
  expect_true(all(uncertainty_cols %in% names(X)))
})

test_that("Checks that SeriesID is a character vector", {
  # This is due to Patrick's request that this should never be a numeric
  # due to loss of precision when grabbing from fromJSON.
  skip_if_no_api()

  X <- get_recorddata(
    dataProcessTypeIds = 2,
    indicatorTypeIds = 8,
    locIds = 818,
    locAreaTypeIds = 2,
    subGroupIds = 2,
    isComplete = 0
  )

  expect_type(X$SeriesID, "character")
})
