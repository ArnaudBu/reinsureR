## XL class for package reinsureR ##
####################################

## Class definition

#' Excess of Loss Treaty
#'
#' An S4 class to represent an Excess of Loss Treaty
#'
#' @slot ded Numeric. Deductible amount of the treaty. Should be superior to 0.
#' @slot lim Numeric. Limit amount for the treaty. Should be superior to 0. May be equal to \code{Inf}.
#' @slot aal Numeric. Annual Aggregate Deductible amount of the treaty. Should be superior to 0.
#' @slot aad Numeric. Annual Aggregate Limit amount for the treaty. Should be superior to 0. May be equal to \code{Inf}.
#' @slot rns Numeric vector. Reinstatement prices. Vector of lentgh equals to the number of reinstatements with each value equals the price of the reinstatement.
#' @slot prm Numeric. Premium rate, which represents the proportion of the premium given to the reinsurer as price for the treaty. Should be between 0 and 1.
#' @slot ptf Vector. List of portfolios on which the treaty is to be applied on.
#' @slot trt Character. Always equal to "SL". Identifier for the type of treaty
#'
#' @include utils.R
#' @exportClass XL
setClass(Class = "XL",
         representation = representation(
           ded = "numeric",
           lim = "numeric",
           aal = "numeric",
           aad = "numeric",
           prm = "numeric",
           rns = "vector",
           ptf = "vector",
           trt = "character"
         ),
         validity = check_xl
)

## User constructor function

#' Excess of Loss Treaty: User constructor function
#'
#' \code{xl} defines an object of class \code{XL} (\code{\link{XL-class}}), which represents an Excess of Loss treaty.
#'
#' @param ded Numeric. Deductible amount of the treaty. Should be superior to 0.
#' @param lim Numeric. Limit amount for the treaty. Should be superior to 0. May be equal to \code{Inf}.
#' @param aal Numeric. Annual Aggregate Deductible amount of the treaty. Should be superior to 0.
#' @param aad Numeric. Annual Aggregate Limit amount for the treaty. Should be superior to 0. May be equal to \code{Inf}.
#' @param rns Numeric vector. Reinstatement prices. Vector of lentgh equals to the number of reinstatements with each value equals the price of the reinstatement.
#' @param prm Numeric. Premium rate, which represents the proportion of the premium given to the reinsurer as price for the treaty. Should be between 0 and 1.
#' @param ptf Vector. List of portfolios on which the treaty is to be applied on. Default value set to all.
#'
#' @return An object of class \code{XL} (\code{\link{XL-class}}), initialized with the values given in input. Its basic methods are:
#' \itemize{
#'  \item{\code{show}}
#' }
#'
#' @details Reinstatements are the number of time the limit can be reconstructed. The vector given for this parameter will be an indication of the price for each reinstatement. For example, a \code{rns} value of \code{c(0, 1)} will give one free reinstatement and one reinstatement paid 100\% of the premium before totally consuming the limit.
#'
#' @examples
#' treaty_1 <- xl(ded = 200000, lim = 20000, aad = 0,
#'                aal = 10000, prm = 0.01, rns = c(0, 1))
#' treaty_1
#'
#' @export
xl <- function(ded = Inf, lim = Inf, aal = Inf, aad = Inf, prm = 0, rns = "none", ptf = "all"){
  if(rns[1] == "none"){
    rns <- 0
  } else {
    rns <- c(rns, 0)
  }
  new(Class = "XL",
      ded = ded,
      lim = lim,
      aal = aal,
      aad = aad,
      prm = prm,
      rns = rns,
      ptf = ptf,
      trt = "XL"
  )
}

## Show method

#' @describeIn XL Excess of Loss Treaty: show method
#' @param object the object to display
#' @export
setMethod("show" ,"XL" ,
          function( object ){
            cat("Excess of Loss treaty" , "\n          " )
            cat("Deductible: " , object@ded,  "\n          " )
            cat("Limit: ", object@lim,  "\n          " )
            cat("Annual Aggregate Deductible: " , object@aad,  "\n          " )
            cat("Annual Aggregate Limit: " , object@aal,  "\n          " )
            cat("Premium Rate: " , object@prm * 100,  "%\n          " )
            cat("Number of reinstatements: " , length(object@rns) - 1,  "\n          "  )
            if(length(object@rns) > 1){
              cat("   ", "Reinstatements prices (in % of premium): " ,  "\n          " )
              for(i in 1:(length(object@rns)-1)){
                cat("       ", object@rns[i] * 100 ,  "%\n          " )
              }
            }
            cat("Portfolios applied on: " , paste(object@ptf, collapse = ", "),  "\n          " )
          }
)
