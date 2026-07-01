#' API Link Generator Function
#'
#' @param server The path to the database. Check if the "unpd_server" option is
#' is set. If not, defaults to \code{"https://popdiv.dfs.un.org/DemoData/api/"}
#'
#' @param type Type of data. Various options are available.
#'
#' @param verbose Logical for whether to print the final translated call
#' to numeric arguments. If the translation of arguments take a lot of time,
#' it can have a substantial reduction in timing. This only makes sense if
#' the user provided values as strings which need translation (such as a
#' country name like 'Haiti' rather than its actually country code).
#'
#' @param ... Other arguments that might define the path to data. All arguments
#' accept a numeric code which is interpreted as the code of the specific
#' product requested. Alternatively, you can supply the equivalent product
#' name as a string which is case insensitive (see examples). Handle with
#' care, this is important! For a list of all options available, see the
#' parameters for each endpoint at https://popdiv.dfs.un.org/Demodata/swagger/ui/index#/
#'
#' @details The link generator is based on the structure of the database
#' created by Dennis Butler (in late 2018). To change the server used to make
#' the requests, set this at the beginning of your script:
#' options(unpd_server = "fill this out").
#'
#' When requesting data from the structured data format (usually called from
#' \code{\link{get_recorddata}}), the columns \code{TimeStart} and \code{TimeEnd}
#' are returned with format \code{DD/MM/YYYY}, where \code{DD} are days, \code{MM}
#' are months and \code{YYYY} are years.
#'
#' @examples
#' \dontrun{
#' # Link to country list
#' L1 <- linkGenerator(
#'   type = "locations",
#'   addDefault = "false",
#'   includeDependencies = "false",
#'   includeFormerCountries = "false"
#' )
#' L1
#'
#' # Link to location types (for Egypt)
#' # With strings rather than codes
#' L2 <- linkGenerator(
#'   type = "locAreaTypes",
#'   indicatorTypeIds = "Population by sex",
#'   locIds = "Egypt",
#'   isComplete = "Abridged"
#' )
#' L2
#'
#' # Link to subgroup types (for Egypt)
#' L3 <- linkGenerator(
#'   type = "subGroups",
#'   indicatorTypeIds = 8,
#'   locIds = 818,
#'   isComplete = 0
#' )
#' L3
#'
#' # Link to indicator list
#' L4 <- linkGenerator(
#'   type = "indicators",
#'   addDefault = "false"
#' )
#' L4
#'
#' # Link to data process type list
#' L5 <- linkGenerator(type = "dataProcessTypes")
#' L5
#' }
#' @keywords internal
linkGenerator <- function(server = getOption("unpd_server", "https://population.un.org/demodata-api/api/"),
                          type,
                          verbose,
                          ...) {
  types <- c(
    "ages",
    "openAges",
    "component",
    "dataCatalogs",
    "dataProcessTypes",
    "dataProcesses",
    "dataReliability",
    "dataSources",
    "dataSourceStatus",
    "dataSourceTypes",
    "dataStatus",
    "dataTypes",
    "defaultKeys",
    "indicators",
    "indicatorTypes",
    "indicatorIndicatorTypes",
    "locAreaTypes",
    "locations",
    "periodGroups",
    "periodTypes",
    "sex",
    "statisticalConcepts",
    "structuredData",
    "structuredDataTable",
    "structuredDataRecords",
    "structuredDataRecordsAdditional",
    "structuredDataSeries",
    "structuredDataCriteria",
    "subGroups",
    "subGroupTypes",
    "timeReferences",
    # These are within UserUtility
    "dataEntryCount"
  )

  idx <- match.arg(tolower(type), choices = tolower(types))
  type <- types[match(idx, tolower(types))] # emit canonical camelCase route

  # The new DemoData API resolves record IDs through the PATH segment
  # (`structuredDataRecords/{ids}`) rather than the legacy `?ids=` query
  # parameter, so pull `ids` out of the filter args and append it to the path.
  dots <- list(...)
  ids <- dots[["ids"]]
  dots[["ids"]] <- NULL

  path <- type
  if (!is.null(ids)) {
    path <- paste0(type, "/", paste(ids, collapse = ","))
  }

  query <- do.call(build_filter, c(dots, list(verbose = verbose)))
  link <- utils::URLencode(paste0(server, path, query))
  link
}


#' Normalize new DemoData API field names to the legacy vocabulary
#'
#' The new DemoData API (\code{population.un.org/demodata-api/api}) returns
#' camelCase field names with no \code{PK_} prefix, whereas the downstream code
#' (\code{col_order}, \code{id_to_fact}, the \code{get_datacatalog} joins, and the
#' lookup helpers) expects the legacy PascalCase / \code{PK_}-prefixed vocabulary.
#' This function is the single normalization point, called from
#' \code{\link{read_API}} immediately after the response envelope is unwrapped.
#'
#' The shim is endpoint-aware (keyed on \code{type}) because the same camelCase
#' field must map differently depending on the endpoint (e.g. \code{locId} becomes
#' \code{PK_LocID} on reference endpoints but \code{LocID} on records). The
#' explicit per-endpoint rename maps are filled in later phases; this scaffold
#' implements the generic leading-character capitalization used as the default.
#'
#' @param data A \code{data.frame} (the unwrapped \code{$data} element of the API
#' response envelope).
#' @param type The canonical camelCase endpoint route the data came from.
#'
#' @return A \code{data.frame} with normalized column names.
#'
#' @keywords internal
normalize_fields <- function(data, type = NULL) {
  data <- as.data.frame(data, stringsAsFactors = FALSE)

  if (ncol(data) == 0) {
    return(data)
  }

  # Generic path: capitalize the leading character of each (dot-separated
  # segment of each) field name. Explicit per-endpoint maps below take
  # precedence over this generic transform.
  cap_lead <- function(nm) {
    parts <- strsplit(nm, ".", fixed = TRUE)[[1]]
    parts <- vapply(
      parts,
      function(p) {
        if (nchar(p) == 0) {
          p
        } else {
          paste0(toupper(substr(p, 1, 1)), substr(p, 2, nchar(p)))
        }
      },
      character(1)
    )
    paste(parts, collapse = ".")
  }

  names(data) <- vapply(names(data), cap_lead, character(1))

  # Endpoint-aware explicit renames. The generic path above has already
  # capitalized the leading character, so the keys here are the
  # already-capitalized names as they appear after that transform
  # (e.g. camelCase `locId` -> `LocId` -> mapped to `PK_LocID`). These
  # reconstruct the legacy `PK_`-prefixed primary keys and preserve `ID`
  # capitalization the downstream lookup helpers depend on.
  explicit_map <- normalize_map(type)

  if (length(explicit_map) > 0) {
    idx <- match(names(data), names(explicit_map))
    has_map <- !is.na(idx)
    names(data)[has_map] <- unname(explicit_map[idx[has_map]])
  }

  # The legacy records payload carried three columns the new API no longer
  # returns (`agesort`, `id`, `FootNoteID`). Downstream `col_order` selection in
  # get_recorddata()/get_recorddataadditional() expects the full vocabulary, so
  # reconstruct any missing `col_order` column as NA on the records endpoints to
  # preserve the column contract (and the exact colnames(res) == col_order tests).
  if (!is.null(type) &&
    tolower(type) %in% c("structureddatarecords", "structureddatarecordsadditional")) {
    missing_cols <- setdiff(values_env$col_order, names(data))
    for (mc in missing_cols) {
      data[[mc]] <- NA
    }
  }

  data
}


#' Per-endpoint field rename maps for \code{\link{normalize_fields}}
#'
#' Returns a named character vector mapping the generically-capitalized field
#' name (the value produced by the leading-character-capitalization step in
#' \code{\link{normalize_fields}}) to the exact legacy vocabulary name the
#' downstream code expects. The map is keyed on the (case-insensitive) endpoint
#' \code{type}. Reference/lookup endpoints reconstruct the \code{PK_}-prefixed
#' primary keys; records endpoints (Phase 4) use non-\code{PK_} foreign keys.
#'
#' @param type The endpoint route the data came from (case-insensitive).
#'
#' @return A named character vector (\code{after-generic -> legacy name}), or
#' an empty character vector when no explicit remapping is required.
#'
#' @keywords internal
normalize_map <- function(type = NULL) {
  empty <- character(0)

  if (is.null(type) || length(type) == 0 || is.na(type)) {
    return(empty)
  }

  key <- tolower(type)

  # Reference / lookup endpoints. The generic capitalization turns camelCase
  # `locId` into `LocId`; here we map that to the legacy `PK_LocID` (and fix
  # the `ID` casing) so lookup helpers keep finding their primary-key column.
  ref_maps <- list(
    locations = c(
      LocId = "PK_LocID",
      LocTypeId = "LocTypeID"
    ),
    indicatortypes = c(
      IndicatorTypeId = "PK_IndicatorTypeID"
    ),
    subgroups = c(
      SubGroupId = "PK_SubGroupID"
    ),
    locareatypes = c(
      LocAreaTypeId = "PK_LocAreaTypeID"
    ),
    dataprocesstypes = c(
      DataProcessTypeId = "PK_DataProcessTypeID"
    ),
    dataprocesses = c(
      DataProcessId = "PK_DataProcessID",
      DataProcessTypeId = "DataProcessTypeID"
    ),
    datatypes = c(
      DataTypeGroupId = "DataTypeGroupID",
      DataTypeGroupId2 = "DataTypeGroupID2"
    ),
    # get_datacatalog joins the reference tables above into the catalog and then
    # selects a fixed PascalCase column set. Reconstruct the PK_ primary key,
    # fix the `ID` casing on the merge keys (LocID, DataProcessID) and the
    # ParentDataCatalogID output column, and keep `isSubnational` lower-cased
    # (it is retained verbatim as an output column downstream).
    datacatalogs = c(
      DataCatalogId = "PK_DataCatalogID",
      LocId = "LocID",
      DataProcessId = "DataProcessID",
      ParentDataCatalogId = "ParentDataCatalogID",
      IsSubnational = "isSubnational"
    )
  )

  if (key %in% names(ref_maps)) {
    return(ref_maps[[key]])
  }

  # Records endpoints. Unlike the reference tables above, the records payload
  # uses NON-`PK_` foreign keys, so the generically-capitalized camelCase names
  # map straight onto the `col_order` / `id_to_fact` PascalCase vocabulary, with
  # the only systematic difference being `Id` -> `ID` casing (plus `Id2` ->
  # `ID2`, `Id1` -> `ID1`). Source of truth: Surya's docx sections 2.11 / 2.12.
  records_map <- c(
    StructuredDataId = "StructuredDataID",
    DataCatalogId = "DataCatalogID",
    LocTypeId = "LocTypeID",
    LocId = "LocID",
    RegId = "RegID",
    AreaId = "AreaID",
    LocAreaTypeId = "LocAreaTypeID",
    SubGroupTypeId = "SubGroupTypeID",
    SubGroupId1 = "SubGroupID1",
    SubGroupCombinationId = "SubGroupCombinationID",
    IndicatorId = "IndicatorID",
    DataProcessTypeId = "DataProcessTypeID",
    DataProcessId = "DataProcessID",
    DataSourceId = "DataSourceID",
    DataStatusId = "DataStatusID",
    StatisticalConceptId = "StatisticalConceptID",
    SexId = "SexID",
    AgeId = "AgeID",
    DataTypeGroupId = "DataTypeGroupID",
    DataTypeGroupId2 = "DataTypeGroupID2",
    DataTypeId = "DataTypeID",
    ModelPatternFamilyId = "ModelPatternFamilyID",
    ModelPatternId = "ModelPatternID",
    DataReliabilityId = "DataReliabilityID",
    PeriodTypeId = "PeriodTypeID",
    PeriodGroupId = "PeriodGroupID",
    TimeReferenceId = "TimeReferenceID",
    SeriesId = "SeriesID"
  )

  if (key %in% c("structureddatarecords", "structureddatarecordsadditional")) {
    return(records_map)
  }

  empty
}


#' Build the section of the path (link) responsible with filtering the data
#'
#' For a description of what each argument represents, see
#' http://24.239.36.16:9654/un3/swagger/ui/index#!/StructuredData/StructuredData_GetStructuredDataRecords
#'
#' @keywords internal
build_filter <- function(dataTypeIds = NULL,
                         dataTypeGroupIds = NULL,
                         dataTypeGroupId2s = NULL,
                         dataProcessIds = NULL,
                         dataProcessTypeIds = NULL,
                         dataSourceShortNames = NULL,
                         dataSourceYears = NULL,
                         startYear = NULL,
                         endYear = NULL,
                         AgeStart = NULL,
                         AgeEnd = NULL,
                         indicatorTypeIds = NULL,
                         indicatorIds = NULL,
                         componentIds = NULL,
                         isComplete = NULL,
                         isActive = NULL,
                         locIds = NULL,
                         ids = NULL,
                         locAreaTypeIds = NULL,
                         subGroupIds = NULL,
                         shortNames = NULL,
                         addDefault = NULL,
                         includeDependencies = NULL,
                         includeFormerCountries = NULL,
                         includeDataIDs = NULL,
                         includeUncertainty = NULL,
                         isSubnational = NULL,
                         years = NULL,
                         verbose) {

  # Keep as list because unlisting multiple ids for a single
  # parameters separates them into different strings
  x <- as.list(environment())

  # Exclude verbose option
  x <- x[!names(x) == "verbose"]

  # For later, to print the translated code query
  # so the user gets the faster request
  translate_vars <- c(
    "locIds", "indicatorTypeIds", "isComplete", "subGroupIds",
    "locAreaTypeIds", "dataProcessIds", "dataProcessTypeIds",
    "dataTypeGroupIds", "dataTypeGroupId2s"
  )

  x_str <- x[translate_vars]

  any_str <- any(vapply(x_str, is.character, FUN.VALUE = logical(1)))

  lookupParams <- list(
    "locIds" = lookupLocIds,
    "indicatorTypeIds" = lookupIndicatorIds,
    "isComplete" = lookupIsCompleteIds,
    "subGroupIds" = lookupSubGroupsIds,
    "dataTypeGroupIds" = lookupDataTypeGroupIds,
    "dataTypeGroupId2s" = lookupDataTypeGroupId2s
  )

  # Iterative over each lookupParams and apply their corresponding lookup
  # function to translate strings such as Germany to the corresponding code.
  # Only available for the names in lookupParams
  x[names(lookupParams)] <- mapply(
    function(fun, vec) fun(vec),
    lookupParams,
    x[names(lookupParams)]
  )

  # Here we need a separate call to the same thing bc
  # I reuse the translated parameters defined above
  # to make queries in the endpoints below
  extraParams <- list(
    "locAreaTypeIds" = lookupAreaTypeIds,
    "dataProcessIds" = lookupDataProcessIds,
    "dataProcessTypeIds" = lookupDataProcessTypeIds
  )

  x[names(extraParams)] <- mapply(
    function(fun, vec, ...) fun(vec, ...),
    extraParams,
    x[names(extraParams)],
    # I pass the already translated parameter list
    # to avoid retranslating stuff like locations, etc...
    # This can save time in querying API
    MoreArgs = list(paramList = x)
  )

  if (length(x) > 0) {
    if (verbose && any_str) {
      # Print call for easier requests
      collapsed_x <- lapply(x, function(i) {
        if (length(i) > 1) paste0("c(", paste0(i, collapse = ", "), ")") else i
      })

      mockup <- unlist(collapsed_x)
      res <- paste0(
        "get_recorddata(",
        paste0(names(mockup), " = ", mockup, collapse = ", "),
        ")"
      )
      cat(
        "If you run the same query again, use the one below (faster): \n ",
        res
      )
    }

    # Turn TRUE/FALSE to true/false
    is_logical <- vapply(x, is.logical, FUN.VALUE = logical(1))
    x[is_logical] <- lapply(x[is_logical], function(x) tolower(as.character(x)))

    # Collapse multiple ids to parameters
    x <- vapply(x, paste0, collapse = ",", FUN.VALUE = character(1))
    # and exclude the empty ones
    x <- x[x != ""]

    S <- paste(paste(names(x), x, sep = "="), collapse = "&")
    out <- paste0("?", S)
  } else {
    out <- ""
  }
  out
}


#' Format data from character to numeric
#' @description When a data is downloaded from web it is saved as a list or
#' data.frame with columns containing strings of information (character format).
#' This function reads the values and if it sees in these columns only numbers
#' will convert the column to class numeric.
#' @param X data.frame
#' @keywords internal
format.numeric.colums <- function(X, exceptions) {
  isNum <- apply(X, 2, FUN = function(w) all(varhandle::check.numeric(w)))
  isNum[colnames(X) %in% exceptions] <- FALSE
  X[isNum] <- lapply(X[, isNum], as.numeric)
  X
}


#' Save downloaded data in a .Rdata file located in the working directory
#' @param data The dataset to be saved;
#' @param file_name Name to be assigned to the data.
#' @keywords internal
save_in_working_dir <- function(data, file_name) {
  assign(file_name, value = data)
  save(list = file_name, file = paste0(file_name, ".Rdata"))

  wd <- getwd()
  n <- nchar(wd)
  wd <- paste0("...", substring(wd, first = n - 45, last = n))
  message(paste0(file_name, ".Rdata is saved in your working directory:\n", wd),
    appendLF = FALSE
  )
  cat("\n   ")
}


lookupDataTypeGroupIds <- function(paramStr) {
  if (is.numeric(paramStr) || is.null(paramStr)) {
    return(paramStr)
  }

  paramStr_low <- tolower(paramStr)

  data_types <- get_datatypes()
  type_code <- data_types[tolower(data_types$DataTypeGroupName) %in% paramStr_low, ]

  # The all statement is in case you provide 2 countries, for example
  if (all(!paramStr_low %in% tolower(type_code$DataTypeGroupName))) {
    stop(
      "DataTypeGroup(s) ",
      paste0("'", paramStr[!paramStr_low %in% type_code$DataTypeGroupName], "'", collapse = ", "),
      " not found. Check get_datatypes()"
    )
  }

  unique(type_code[["DataTypeGroupID"]])
}


lookupDataTypeGroupId2s <- function(paramStr) {
  if (is.numeric(paramStr) || is.null(paramStr)) {
    return(paramStr)
  }

  paramStr_low <- tolower(paramStr)

  data_types <- get_datatypes()
  type_code <- data_types[tolower(data_types$DataTypeGroupName2) %in% paramStr_low, ]

  # The all statement is in case you provide 2 countries, for example
  if (all(!paramStr_low %in% tolower(type_code$DataTypeGroupName2))) {
    stop(
      "DataTypeGroup2(s) ",
      paste0("'", paramStr[!paramStr_low %in% type_code$DataTypeGroupName2], "'", collapse = ", "),
      " not found. Check get_datatypes()"
    )
  }

  unique(type_code[["DataTypeGroupID2"]])
}


lookupLocIds <- function(paramStr) {
  if (is.numeric(paramStr) || is.null(paramStr)) {
    return(paramStr)
  }

  paramStr_low <- tolower(paramStr)

  locs <- get_locations()
  cnt_code <- locs[tolower(locs$Name) %in% paramStr_low, ]

  # The all statement is in case you provide 2 countries, for example
  if (all(!paramStr_low %in% tolower(cnt_code$Name))) {
    stop(
      "Location(s) ",
      paste0("'", paramStr[!paramStr_low %in% cnt_code$Name], "'", collapse = ", "),
      " not found. Check get_locations()"
    )
  }

  cnt_code[["PK_LocID"]]
}

lookupIndicatorIds <- function(paramStr) {
  if (is.numeric(paramStr) || is.null(paramStr)) {
    return(paramStr)
  }
  paramStr_low <- tolower(paramStr)

  inds <- get_indicatortypes()
  inds_code <- inds[tolower(inds$Name) %in% paramStr_low, ]

  # The all statement is in case you provide 2 indicators, for example
  if (all(!tolower(paramStr_low) %in% tolower(inds_code$Name))) {
    stop(
      "Location(s) ",
      paste0("'", paramStr[!paramStr_low %in% inds_code$Name], "'", collapse = ", "),
      " not found. Check get_indicatortypes()"
    )
  }

  inds_code[["PK_IndicatorTypeID"]]
}

lookupSubGroupsIds <- function(paramStr) {
  if (is.numeric(paramStr) || is.null(paramStr)) {
    return(paramStr)
  }
  paramStr_low <- tolower(paramStr)

  inds <- get_subgroups()
  inds_code <- inds[tolower(inds$Name) %in% paramStr_low, ]

  # The all statement is in case you provide 2 indicators, for example
  if (all(!tolower(paramStr) %in% tolower(inds_code$Name))) {
    stop(
      "Location(s) ",
      paste0("'", paramStr[!paramStr_low %in% inds_code$Name], "'", collapse = ", "),
      " not found. Check get_subgroups()"
    )
  }

  inds_code[["PK_SubGroupID"]]
}

lookupAreaTypeIds <- function(paramStr, paramList) {
  if (is.numeric(paramStr) || is.null(paramStr)) {
    return(paramStr)
  }
  paramStr_low <- tolower(paramStr)

  inds <- get_locationtypes(
    locIds = paramList[["locIds"]],
    indicatorTypeIds = paramList[["indicatorTypeIds"]],
    isComplete = paramList[["isComplete"]]
  )

  inds_code <- inds[tolower(inds$Name) %in% paramStr_low, ]
  # The all statement is in case you provide 2 area types, for example
  if (all(!tolower(paramStr) %in% tolower(inds_code$Name))) {
    stop(
      "Area Type(s) ",
      paste0("'", paramStr[!paramStr_low %in% inds_code$Name], "'", collapse = ", "),
      " not found. Check get_locationtypes()"
    )
  }

  inds_code[["PK_LocAreaTypeID"]]
}

lookupDataProcessTypeIds <- function(paramStr, paramList) {
  if (is.numeric(paramStr) || is.null(paramStr)) {
    return(paramStr)
  }
  paramStr_low <- tolower(paramStr)

  # The new DemoData API's `dataProcessTypes` endpoint returns an empty set when
  # the loc/indicator/isComplete filters are supplied, so fetch the full
  # (small, static) reference table and resolve the string against it.
  inds <- get_dataprocesstype()

  inds_code <- inds[tolower(inds$Name) %in% paramStr_low, ]
  # The all statement is in case you provide 2 area types, for example
  if (all(!tolower(paramStr) %in% tolower(inds_code$Name))) {
    stop(
      "Data type(s) ",
      paste0("'", paramStr[!paramStr_low %in% inds_code$Name], "'", collapse = ", "),
      " not found. Check get_dataprocesstype()"
    )
  }

  inds_code[["PK_DataProcessTypeID"]]
}

lookupDataProcessIds <- function(paramStr, paramList) {
  if (is.numeric(paramStr) || is.null(paramStr)) {
    return(paramStr)
  }
  paramStr_low <- tolower(paramStr)

  inds <- get_dataprocess(
    locIds = paramList[["locIds"]],
    indicatorTypeIds = paramList[["indicatorTypeIds"]],
    isComplete = paramList[["isComplete"]]
  )

  inds_code <- inds[tolower(inds$Name) %in% paramStr_low, ]

  # The all statement is in case you provide 2 area types, for example
  if (all(!tolower(paramStr) %in% tolower(inds_code$Name))) {
    stop(
      "Data type(s) ",
      paste0("'", paramStr[!paramStr_low %in% inds_code$Name], "'", collapse = ", "),
      " not found. Check get_dataprocess()"
    )
  }

  inds_code[["PK_DataProcessID"]]
}

lookupIsCompleteIds <- function(paramStr) {
  if (is.numeric(paramStr) || is.null(paramStr)) {
    return(paramStr)
  }

  paramStr_low <- tolower(paramStr)

  res <- switch(paramStr_low,
    "abridged" = 0,
    "complete" = 1,
    "total" = 2,
    stop(
      "IsComplete does not accept string '",
      paramStr, "'",
      ". Only 'abridged', 'complete', 'total'."
    )
  )

  res
}

values_env <- new.env()

# Columns to turn into labelled factors
values_env$id_to_fact <- c(
  AreaName = "AreaID",
  ## DataCatalogName = "DataCatalogID",
  DataReliabilityName = "DataReliabilityID",
  ## SubGroupName = "PK_SubGroupID",
  ## DataSourceName = "DataSourceID",
  DataStatusName = "DataStatusID",
  ## DataTypeName = "DataTypeID",
  DataTypeGroupName = "DataTypeGroupID",
  IndicatorName = "IndicatorID",
  LocName = "LocID",
  LocAreaTypeName = "LocAreaTypeID",
  LocTypeName = "LocTypeID",
  ModelPatternName = "ModelPatternID",
  ModelPatternFamilyName = "ModelPatternFamilyID",
  PeriodGroupName = "PeriodGroupID",
  PeriodTypeName = "PeriodTypeID",
  RegName = "RegID",
  SexName = "SexID",
  StatisticalConceptName = "StatisticalConceptID",
  SubGroupTypeName = "SubGroupTypeID"
  ## DataProcess = "PK_DataProcessID"
)

values_env$col_order <- c(
  "IndicatorName",
  "IndicatorID",
  "LocName",
  "LocID",
  "StructuredDataID",
  "LocTypeName",
  "LocTypeID",
  "RegName",
  "RegID",
  "AreaName",
  "AreaID",
  "LocAreaTypeName",
  "LocAreaTypeID",
  "SubGroupTypeName",
  "SubGroupTypeID",
  "SubGroupID1",
  "SubGroupName",
  "SubGroupCombinationID",
  "SubGroupTypeSortOrder",
  "IndicatorShortName",
  "DataCatalogID",
  "DataProcessTypeID",
  "DataProcessType",
  "DataProcessTypeSort",
  "DataProcess",
  "DataProcessID",
  "DataProcessSort",
  "DataCatalogName",
  "DataCatalogShortName",
  "ReferencePeriod",
  "ReferenceYearStart",
  "ReferenceYearEnd",
  "ReferenceYearMid",
  "FieldWorkStart",
  "FieldWorkEnd",
  "FieldWorkMiddle",
  "DataCatalogNote",
  "DataSourceID",
  "DataSourceAuthor",
  "DataSourceYear",
  "DataSourceName",
  "DataSourceShortName",
  "DataSourceSort",
  "HasUncertaintyRecord",
  "StandardErrorValue",
  "ConfidenceInterval",
  "ConfidenceIntervalLowerBound",
  "ConfidenceIntervalUpperBound",
  "DataStatusName",
  "DataStatusID",
  "DataStatusSort",
  "StatisticalConceptName",
  "StatisticalConceptID",
  "StatisticalConceptSort",
  "SexName",
  "SexID",
  "SexSort",
  "AgeID",
  "AgeUnit",
  "AgeStart",
  "AgeEnd",
  "AgeSpan",
  "AgeMid",
  "AgeLabel",
  "AgeSort",
  "agesort",
  "DataTypeGroupName",
  "DataTypeGroupID",
  "DataTypeName",
  "DataTypeID",
  "DataTypeSort",
  "ModelType",
  "ModelPatternFamilyName",
  "ModelPatternFamilyID",
  "ModelPatternName",
  "ModelPatternID",
  "DataReliabilityName",
  "DataReliabilityID",
  "DataReliabilitySort",
  "PeriodTypeName",
  "PeriodTypeID",
  "PeriodGroupName",
  "PeriodGroupID",
  "PeriodStart",
  "PeriodEnd",
  "PeriodSpan",
  "PeriodMiddle",
  "Weight",
  ## "TimeReferenceID",
  "TimeUnit",
  "TimeStart",
  "TimeEnd",
  "TimeDuration",
  "TimeMid",
  "TimeLabel",
  ## "StaffMemberID",
  "FootNoteID",
  "id",
  "SeriesID",
  "DataValue"
)
