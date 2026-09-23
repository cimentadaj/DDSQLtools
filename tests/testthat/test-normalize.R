# Offline tests: no network access. They exercise the field-name normalization
# and URL building that every get_* function relies on.

test_that("normalize_fields restores ID casing on data sources", {
  x <- normalize_fields(
    data.frame(dataSourceId = 1, locId = 2, dataSourceTypeId = 3, name = "a"),
    type = "dataSources"
  )
  expect_equal(names(x), c("DataSourceID", "LocID", "DataSourceTypeID", "Name"))
})

test_that("normalize_fields handles numbered and bare Id suffixes", {
  x <- normalize_fields(
    data.frame(subGroupId1 = 1, dataTypeGroupId2 = 2, id = 3),
    type = "structuredDataSeries"
  )
  expect_equal(names(x), c("SubGroupID1", "DataTypeGroupID2", "ID"))
})

test_that("normalize_fields leaves names without a trailing Id untouched", {
  x <- normalize_fields(
    data.frame(name = 1, ageMid = 2, isDefault = 3, identity = 4),
    type = "open/ages"
  )
  expect_equal(names(x), c("Name", "AgeMid", "IsDefault", "Identity"))
})

test_that("explicit endpoint maps take precedence over the ID rule", {
  loc <- normalize_fields(data.frame(locId = 1, locTypeId = 2), type = "locations")
  expect_equal(names(loc), c("PK_LocID", "LocTypeID"))

  dp <- normalize_fields(
    data.frame(dataProcessId = 1, dataProcessTypeId = 2),
    type = "dataProcesses"
  )
  expect_equal(names(dp), c("PK_DataProcessID", "DataProcessTypeID"))

  dc <- normalize_fields(
    data.frame(dataCatalogId = 1, locId = 2, isSubnational = TRUE),
    type = "dataCatalogs"
  )
  expect_equal(names(dc), c("PK_DataCatalogID", "LocID", "isSubnational"))
})

test_that("records endpoints still expose the full col_order vocabulary", {
  x <- normalize_fields(
    data.frame(structuredDataId = 1, locId = 2, sexId = 3),
    type = "structuredDataRecords"
  )
  expect_true(all(values_env$col_order %in% names(x)))
  expect_false(any(grepl("Id[0-9]*$", names(x))))
})

test_that("normalize_fields returns empty data untouched", {
  x <- normalize_fields(list(), type = "open/ages")
  expect_true(is.data.frame(x))
  expect_equal(nrow(x), 0)
})

test_that("linkGenerator builds the new codelist routes", {
  routes <- c(
    "locationTypes", "subGroupTypes", "dataSourceStatus", "dataSourceTypes",
    "dataStatus", "statisticalConcepts", "sex", "modelPatterns",
    "dataReliability", "periodTypes", "periodGroups"
  )
  for (r in routes) {
    expect_equal(
      linkGenerator(type = tolower(r), verbose = FALSE),
      paste0("https://population.un.org/demodata-api/api/", r, "?")
    )
  }

  expect_equal(
    linkGenerator(type = "open/ages", ageUnit = "Year", verbose = FALSE),
    "https://population.un.org/demodata-api/api/open/ages?ageUnit=Year"
  )
  expect_match(
    linkGenerator(type = "LOCATIONTYPES", verbose = FALSE),
    "/api/locationTypes",
    fixed = TRUE
  )
})
