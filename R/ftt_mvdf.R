#' The Minimum Viable Data Frame S4 class underpinning `forthetrees`
#'
#' @section Units:
#' For cohesiveness, `forthetrees` assumes all values are in SI units. In
#' practice, this means:
#'
#' * All linear units are in meters.
#' * All measurements of area are in m^2
#' * All measurements of volume are in m^3
#'
#' It is the responsibility of importer functions to correctly convert from
#' non-standard units into these formats.
#'
#' @template mvdf
#'
#' @family classes and related functions
#'
#' @exportClass ftt_mvdf
methods::setClass("ftt_mvdf",
                  slots = c(
                    idx = "character",
                    x = "numeric",
                    y = "numeric",
                    z = "numeric",
                    metadata = "data.frame",
                    appendix = "list"
                  )
)

setValidity("ftt_mvdf", function(object) {

  error <- vector("character")
  n_issue <- 1

  if (length(object@idx) != length(unique(object@idx))) {
    error[n_issue] <- "@idx must be unique."
    n_issue <- n_issue + 1
  }
  if (any(is.null(object@idx) |
          is.na(object@idx))) {
    error[n_issue] <- "@idx must not have any NULL or NA values."
    n_issue <- n_issue + 1
  }
  if (any(is.null(object@x) |
          is.na(object@x) |
          is.infinite(object@x) |
          is.nan(object@x))) {
    error[n_issue] <- "@x must not have any missing values."
    n_issue <- n_issue + 1
  }
  if (any(is.null(object@y) |
          is.na(object@y) |
          is.infinite(object@y) |
          is.nan(object@y))) {
    error[n_issue] <- "@y must not have any missing values."
    n_issue <- n_issue + 1
  }
  if (any(is.null(object@z) |
          is.na(object@z) |
          is.infinite(object@z) |
          is.nan(object@z))) {
    error[n_issue] <- "@z must not have any missing values."
    n_issue <- n_issue + 1
  }
  if (nrow(object@metadata) > 0 && !("idx" %in% names(object@metadata))) {
    error[n_issue] <- "@metadata must have an index column named 'idx'."
  }

  if (n_issue > 1) return(paste0(error, collapse = "\n"))
  return(TRUE)

})

#' Construct a `forthetrees` Minimum Viable Data Frame object
#'
#' @param data Data frame containing columns with names corresponding to the
#' slots listed in TK.
#' @param metadata Data frame: a table containing additional information on the
#' objects to be modeled. Optional, but if this slot is used then the data frame
#' must contain a column named `idx` which should correspond to the `idx` slot.
#' Only the existence of this column is validated.
#' @param appendix List: additional data produced in the generation of the
#' object. Not validated; any additional outputs that don't map to modeled
#' objects may be inserted here.
#'
#' @export
ftt_mvdf <- function(data, metadata = data.frame(), appendix = list()) {
  methods::new("ftt_mvdf",
      idx = as.character(data$idx),
      x = as.double(data$x),
      y = as.double(data$y),
      z = as.double(data$z),
      metadata = as.data.frame(metadata),
      appendix = as.list(appendix)
  )
}

#' Retrieve mvdf values from a `forthetrees` object.
#'
#' @param object The `forthetrees` object to retrieve the mvdf for.
#'
#' @name mvdf
#'
#' @export
setGeneric("mvdf", function(object) standardGeneric("mvdf"))

#' @rdname mvdf
#' @exportMethod mvdf
setMethod("mvdf", "ftt_mvdf", function(object) as.data.frame(object))

#' Set mvdf values for a `forthetrees` object.
#'
#' @param x The `forthetrees` object to set the mvdf within.
#' @param value The data to replace the mvdf with.
#'
#' @rdname mvdfassign
#'
#' @export
setGeneric("mvdf<-", function(x, value) standardGeneric("mvdf<-"))

#' @rdname mvdfassign
#' @exportMethod mvdf<-
setMethod("mvdf<-", "ftt_mvdf", function(x, value) {
  do.call(class(x)[[1]], list(data = value,
                              metadata = x@metadata,
                              appendix = x@appendix))
})

#' Set slot values for `forthetrees` objects.
#'
#' This function returns a new object of the same class as `object` with the
#' same metadata and appendix slots as `object`, but with other slots taking
#' values from `mvdf`. Use it as a convenient, pipe-able way to set slot values
#' for `forthetrees` objects.
#'
#' @param mvdf The minimum viable data frame required by the S4 class
#' @param object The
#' @param metadata The metadata to include in the new object. If `NULL`
#' (the default), uses the metadata from `object`.
#' @param appendix The appendix to include in the new object. If `NULL`
#' (the default), uses the appendix from `object`.
#'
#' @export
ftt_set_mvdf <- function(mvdf, object, metadata = NULL, appendix = NULL) {
  if (is.null(metadata)) metadata <- object@metadata
  if (is.null(appendix)) appendix <- object@appendix
  do.call(class(object)[[1]], list(data = mvdf,
                                   metadata = metadata,
                                   appendix = appendix))
}

#' Retrieve the metadata data frame from a `forthetrees` object
#'
#' @param object The object to retrieve the metadata data frame from.
#'
#' @rdname ftt_get_metadata
#'
#' @export
setGeneric("metadata",
           function(object) standardGeneric("metadata"))

#' @rdname ftt_get_metadata
#' @exportMethod  metadata
setMethod("metadata", "ftt_mvdf", function(object) {
  object@metadata
})

#' Retrieve the appendix from a `forthetrees` object
#'
#' @param object The object to retrieve the appendix from.
#'
#' @rdname ftt_get_appendix
#'
#' @export
setGeneric("appendix",
           function(object) standardGeneric("appendix"))

#' @rdname ftt_get_appendix
#' @exportMethod appendix
setMethod("appendix", "ftt_mvdf", function(object) {
  object@appendix
})

#' Coerce to a data frame
#'
#' Coerce any object inheriting from `ftt_mvdf` to a data frame.
#'
#' @param x An object inheriting from `ftt_mvdf`.
#' @param row.names,optional,... Arguments passed to [base::as.data.frame].
#'
#' @exportMethod as.data.frame
methods::setMethod("as.data.frame", "ftt_mvdf", function(x,
                                                         row.names,
                                                         optional,
                                                         ...) {

  nms <- setdiff(methods::slotNames(x), c("metadata", "appendix"))
  lst <- lapply(nms, function(nm) methods::slot(x, nm))
  return(as.data.frame(stats::setNames(lst, nms),
                       row.names = row.names,
                       optional = optional,
                       ...))

})

#' Show an object inheriting from `ftt_mvdf`
#'
#' Display the object, by printing, plotting or whatever suits its class.
#'
#' @param object Any object inheriting from `ftt_mvdf`
#'
#' @return `show` returns an invisible `NULL`.
#'
#' @exportMethod show
methods::setMethod("show", "ftt_mvdf", function(object) {

  print(as.data.frame(object))
  print(object@metadata)
  print(object@appendix)
  return(NULL)

})
