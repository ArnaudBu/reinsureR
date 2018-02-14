## SL class for package reinsureR ##
####################################

## Class definition

#' Stop Loss Treaty
#'
#' An S4 class to represent a Stop Loss Treaty
#'
#' @slot ded Numeric. Deductible amount of the treaty. Should be superior to 0.
#' @slot lim Numeric. Limit amount for the treaty. Should be superior to 0. May be equal to \code{Inf}.
#' @slot prm Numeric. Premium rate, which represents the proportion of the premium given to the reinsurer as price for the treaty. Should be between 0 and 1.
#' @slot ptf Vector. List of portfolios on which the treaty is to be applied on.
#' @slot trt Character. Always equal to "SL". Identifier for the type of treaty
#'
#' @include utils.R
#' @exportClass SL
setClass(Class = "SL",
         representation = representation(
           ded = "numeric",
           lim = "numeric",
           prm = "numeric",
           ptf = "vector",
           trt = "character"
         ),
         validity = check_sl
)

## User constructor function

#' Stop Loss Treaty: User constructor function
#'
#' \code{sl} defines an object of class \code{SL} (\code{\link{SL-class}}), which represents a Stop Loss treaty.
#'
#' @param ded Numeric. Deductible amount of the treaty. Should be superior to 0.
#' @param lim Numeric. Limit amount for the treaty. Should be superior to 0. May be equal to \code{Inf}.
#' @param prm Numeric. Premium rate, which represents the proportion of the premium given to the reinsurer as price for the treaty. Should be between 0 and 1.
#' @param ptf Vector. List of portfolios on which the treaty is to be applied on. Default value set to all.
#'
#' @return An object of class \code{SL} (\code{\link{SL-class}}), initialized with the values given in input. Its basic methods are:
#' \itemize{
#'  \item{\code{show}}
#' }
#'
#' @examples
#' treaty_1 <- sl(ded = 100000, lim = 20000, prm = 0.01, ptf = "all")
#' treaty_1
#'
#' @export
sl <- function(ded = Inf, lim = Inf, prm = 0, ptf = "all"){
  new(Class = "SL",
      ded = ded,
      lim = lim,
      prm = prm,
      ptf = ptf,
      trt = "SL"
  )
}

## Show method

#' @describeIn SL Stop Loss Treaty: show method
#' @param object the object to display
#' @export
setMethod("show" ,"SL" ,
          function( object ){
            cat("Stop Loss treaty" , "\n          " )
            cat("Deductible: " , object@ded,  "\n          " )
            cat("Limit: ", object@lim,  "\n          " )
            cat("Premium Rate: " , object@prm * 100,  "%\n          " )
            cat("Portfolios applied on: " , paste(object@ptf, collapse = ", "),  "\n          " )
          }
)
