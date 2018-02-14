## QS class for package reinsureR ##
####################################

## Class definition

#' Quota Share Treaty
#'
#' An S4 class to represent a Quota Share Treaty
#'
#' @slot csn_clm Numeric. Cession rate on claims. Should be between 0 and 1.
#' @slot csn_prm Numeric. Cession rate on premiums. Should be between 0 and 1.
#' @slot com Numeric. Commission rate, applied on the part of premiums given to the reinsurer. Should be between 0 and 1.
#' @slot ptf Vector. List of portfolios on which the treaty is to be applied on.
#' @slot trt Character. Always equal to "QS". Identifier for the type of treaty.
#'
#' @include utils.R
#' @exportClass QS
setClass(Class = "QS",
         representation = representation(
           csn_clm = "numeric",
           csn_prm = "numeric",
           com = "numeric",
           ptf = "vector",
           trt = "character"
         ),
         validity = check_qs
)

## User constructor function

#' Quota Share Treaty: User constructor function
#'
#' \code{qs} defines an object of class \code{QS} (\code{\link{QS-class}}).
#'
#' @param csn_clm Numeric. Cession rate on claims. Should be between 0 and 1.
#' @param csn_prm Numeric. Cession rate on premiums. Should be between 0 and 1. Default value set to \code{csn_clm}.
#' @param com Numeric. Commission rate, applied on the part of premiums given to the reinsurer. Should be between 0 and 1. Default value set to 0.
#' @param ptf Vector. List of portfolios on which the treaty is to be applied on. Default value set to all.
#'
#' @return An object of class \code{QS} (\code{\link{QS-class}}), initialized with the values given in input. Its basic methods are:
#' \itemize{
#'  \item{\code{show}}
#' }
#'
#' @examples
#' treaty_1 <- qs(0.8, com = 0.25)
#' treaty_1
#'
#' @export
qs <- function(csn_clm, csn_prm = "auto", com = 0, ptf = "all"){
  if(csn_prm == "auto") csn_prm <- csn_clm
  new(Class = "QS",
      csn_clm = csn_clm,
      csn_prm = csn_prm,
      com = com,
      ptf = ptf,
      trt = "QS"
  )
}

## Show method

#' @describeIn QS Quota Share Treaty: show method
#' @param object the object to display
#' @export
setMethod("show" ,"QS" ,
          function( object ){
            cat("Quota Share treaty" , "\n          " )
            cat("Cession rate on claims: " , object@csn_clm * 100,  "%\n          " )
            cat("Cession rate on premiums: ", object@csn_prm * 100,  "%\n          " )
            cat("Commission rate: " , object@com * 100,  "%\n          " )
            cat("Portfolios applied on: " , paste(object@ptf, collapse = ", "),  "\n          " )
          }
)
