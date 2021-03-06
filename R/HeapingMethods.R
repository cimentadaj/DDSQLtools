#' Wrapper for Age-Heaping Methods
#' 
#' @inheritParams do_splitting
#' @inheritParams DemoTools::check_heaping_myers
#' @seealso
#' 
#' \code{\link[DemoTools]{check_heaping_whipple}},
#' \code{\link[DemoTools]{check_heaping_myers}},
#' \code{\link[DemoTools]{check_heaping_bachi}},
#' \code{\link[DemoTools]{check_heaping_coale_li}},
#' \code{\link[DemoTools]{check_heaping_noumbissi}},
#' \code{\link[DemoTools]{check_heaping_spoorenberg}},
#' \code{\link[DemoTools]{ageRatioScore}},
#' \code{\link[DemoTools]{check_heaping_kannisto}},
#' \code{\link[DemoTools]{check_heaping_jdanov}}.
#' 
#' @examples 
#' P1 <- DDSQLtools.data$Pop1_Egypt_M_DB
#' 
#' H1 <- do_heaping(P1, fn = "Whipple")
#' H2 <- do_heaping(P1, fn = "Myers")
#' H3 <- do_heaping(P1, fn = "Bachi")
#' H4 <- do_heaping(P1, fn = "CoaleLi")
#' H5 <- do_heaping(P1, fn = "Noumbissi")
#' H6 <- do_heaping(P1, fn = "Spoorenberg")
#' H7 <- do_heaping(P1, fn = "ageRatioScore")
#' H8 <- do_heaping(P1, fn = "KannistoHeap")
#' H9 <- do_heaping(P1, fn = "Jdanov")
#' 
#' H <- rbind(H1, H2, H3, H4, H5, H6, H7, H8, H9)
#' select_columns <- c("AgeID", "AgeStart", "AgeMid", "AgeEnd", "AgeLabel",
#'                     "DataTypeName", "DataTypeID", "DataValue")
#' H[, select_columns]
#' 
#' # Silence the function with verbose = FALSE
#' H1 <- do_heaping(P1, fn = "Whipple", verbose = FALSE)
#' # ... or by specifying all arguments
#' H1 <- do_heaping(P1, fn = "Whipple", ageMin = 10, ageMax = 90, digit = 1)
#' @export
do_heaping <- function(X,
                       fn = c("Whipple",
                              "Myers",
                              "Bachi",
                              "CoaleLi",
                              "Noumbissi",
                              "Spoorenberg",
                              "ageRatioScore",
                              "KannistoHeap",
                              "Jdanov"),
                       verbose = TRUE,
                       ...) {

  A   <- X$DataValue
  B   <- X$AgeStart
  C   <- match.call()
  fn  <- match.arg(fn)

  E <- switch(fn,
              Whipple = check_heaping_whipple(A, B, ...),
              Myers = check_heaping_myers(A, B, ...),
              Bachi = check_heaping_bachi(A, B, ...),
              CoaleLi = check_heaping_coale_li(A, B, ...),
              Noumbissi = check_heaping_noumbissi(A, B, ...),
              Spoorenberg = check_heaping_spoorenberg(A, B, ...),
              ageRatioScore = ageRatioScore(A, B, OAG = is_OAG(X), ...),
              KannistoHeap = check_heaping_kannisto(A, B, ...),
              Jdanov = check_heaping_jdanov(A, B, ...)
              )

  fn <- switch(fn,
               Whipple = "check_heaping_whipple",
               Myers = "check_heaping_myers",
               Bachi = "check_heaping_bachi",
               CoaleLi = "check_heaping_coale_li",
               Noumbissi = "check_heaping_noumbissi",
               Spoorenberg = "check_heaping_spoorenberg",
               ageRatioScore = "ageRatioScore",
               KannistoHeap = "check_heaping_kannisto",
               Jdanov = "check_heaping_jdanov"
               )

  G <-
    within(data.frame(DataValue = E), {
      AgeID <- NA_real_
      AgeStart <- min(X$AgeStart)
      AgeEnd <- max(X$AgeEnd)
      AgeMid <- sum(X$AgeMid - X$AgeStart)
      AgeSpan <- AgeEnd - AgeStart
      AgeLabel <- paste0(AgeStart, "-", rev(X$AgeLabel)[1])
      DataTypeName <- paste0("DemoTools::", fn)
      DataTypeID <- paste(deparse(C), collapse = "")
      ReferencePeriod <- unique(X$ReferencePeriod)
    })

  if (verbose) output_msg(fn, names(C))
  out <- format_output(X, G)
  out
}

