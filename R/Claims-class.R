## Claims class for package reinsureR ##
########################################

## Class definition

#' Claims
#'
#' An S4 class to represent Claims and Premiums for reinsurance computations.
#'
#' @slot clm Data.table. Claims table. Contains at least 4 columns:
#' \itemize{
#'  \item{\code{year}: year associated with the considered claim;}
#'  \item{\code{portfolio}: portfolio associated with the considered claim;}
#'  \item{\code{simulId}: simulation id of the considered claim. Useful for stochastic modelling;}
#'  \item{\code{amount}: amount of the considered claim.}
#' }
#' @slot prm Data.table. Premiums table. Contains at least 3 columns:
#' \itemize{
#'  \item{\code{year}: year associated with the considered premium;}
#'  \item{\code{portfolio}: portfolio associated with the considered premium;}
#'  \item{\code{amount}: amount of the considered premium.}
#' }
#' @slot rns Data.table. Reinstatement amount table, that only concerns excess of loss treaties. Contains at least 2 columns:
#' \itemize{
#'  \item{\code{year}: year associated with the considered reinstatements;}
#'  \item{\code{simulId}: simulation id of the considered reinstatement amount. Useful for stochastic modelling.}
#' }
#' @slot com Data.table. Commissions amount table, that only concerns quota share treaties. Contains at least 2 columns:
#' \itemize{
#'  \item{\code{year}: year associated with the considered commission amount;}
#'  \item{\code{portfolio}: portfolio associated with the considered commission.}
#' }
#' @slot trt Vector. List of treaties applied to the Claims object.
#'
#' @include utils.R QS-class.R XL-class.R SL-class.R
#' @exportClass Claims
setClass(Class = "Claims",
         representation = representation(
           clm = "data.table",
           prm = "data.table",
           rns = "data.table",
           com = "data.table",
           trt = "vector"
         ),
         validity = check_claim
)

## User constructor function

#' Claims: User constructor function
#'
#' \code{claims} defines an object of class \code{Claims} (\code{\link{Claims-class}}).
#'
#' @param claims Data.frame. Claims table. Contains at least 2 columns, and may contain 2 optional columns:
#' \itemize{
#'  \item{\code{year}: year associated with the considered claim;}
#'  \item{\code{portfolio}: Optional. Portfolio associated with the considered claim;}
#'  \item{\code{simulId}: Optional. Simulation id of the considered claim. Useful for stochastic modelling;}
#'  \item{\code{amount}: amount of the considered claim.}
#' }
#' @param premiums Data.frame. Premiums table. Contains at least 2 columns, and may contain 1 optional column:
#' \itemize{
#'  \item{\code{year}: year associated with the considered premium;}
#'  \item{\code{portfolio}: Optional. Portfolio associated with the considered premium;}
#'  \item{\code{amount}: amount of the considered premium.}
#' }
#'
#' @details
#'
#' If portfolio or simulId are not given in the input tables, the default value is set to 0 in the \code{Claims} object.
#'
#' Consistency needs to be insured between claims and premiums. For every year where a claim is entered, a premium must be registered. The same goes for the portfolios.
#'
#' Portfolios differenciation is used to apply reinsurance treaties to different part of claims.
#'
#' Simulation Ids are used in the case of stochastic simulations, in order to apply reinsurance treaties over simulated claims. Simulations Id are not used for premiums, which are mapped through year and portfolio. Be careful to have a unique premium for each couple year/portfolio.
#'
#' For a unique combination of year, portfolio and simulId can be associated multiple events that will be taken into account when applying Excess of Loss reinsurance.
#'
#' @return An object of class \code{Claims} (\code{\link{Claims-class}}), initialized with the values given in input. Its basic methods are:
#' \itemize{
#'  \item{\code{show}}
#'  \item{\code{draw}(\link{draw}): plotting function;}
#'  \item{\code{summy}(\link{summy}): plotting function;}
#'  \item{\code{get_claims}(\link{get_claims}): extract claim table from Claims object;}
#'  \item{\code{get_premiums}(\link{get_premiums}): extract premium table from Claims object;}
#'  \item{\code{get_commissions}(\link{get_commissions}): extract commissions table from Claims object;}
#'  \item{\code{get_reinstatements}(\link{get_reinstatements}): extract reinstatements table from Claims object;}
#'  \item{\code{get_treaties}(\link{get_treaties}): extract applied treaties list from Claims object.}
#' }
#'
#' @examples
#' c <- data.frame(year = unlist(sapply(2000:2017, function(x) rep(x, rpois(1,3)))))
#' c$amount <- pmax(rnorm(nrow(c), 200000, 100000), 0)
#'
#' p <- aggregate(amount ~ year, c, sum)
#'
#' claims <- claims(c, p)
#'
#' @import data.table methods
#' @export
claims <- function(claims, premiums){
  if(!("portfolio" %in% colnames(claims))) claims$portfolio <- 0
  if(!("simulId" %in% colnames(claims))) claims$simulId <- 0
  if(!("portfolio" %in% colnames(premiums))) premiums$portfolio <- 0
  com <- unique(premiums[, c("year", "portfolio")])
  rns <- unique(claims[, c("year", "simulId")])
  new(Class = "Claims",
      clm = data.table(claims),
      prm = data.table(premiums),
      rns = data.table(rns),
      com = data.table(com),
      trt = vector()
  )
}

## Apply QS treaty to claims method (internal)

#' @import data.table
setGeneric(
  name = "apply_treaty_qs",
  def = function(object, qs)
  {
    standardGeneric("apply_treaty_qs")
  }
)

setMethod(
  f = "apply_treaty_qs",
  signature = c("Claims", "QS"),
  definition = function(object, qs)
  {
    portfolio <- amount <- amount_after_treaty_1 <- NULL
    c <- ncol(object@clm)
    if(qs@ptf[1] == "all") {
      ptf <- unique(object@clm$portfolio)
    } else{
      ptf <- qs@ptf
      if(! all(ptf %in% unique(object@clm$portfolio))) stop("One of the selected portfolio is not in the claim database.")
    }
    if(c == 4){
      object@clm[portfolio %in% ptf, amount_after_treaty_1 := (1 - qs@csn_clm) * amount ]
      object@clm[!(portfolio %in% ptf), amount_after_treaty_1 :=  amount ]
      object@prm[portfolio %in% ptf, amount_after_treaty_1 := (1 - qs@csn_prm) * amount ]
      object@prm[!(portfolio %in% ptf), amount_after_treaty_1 := amount ]
      object@com <- merge(object@com, object@prm, all.x = TRUE)
      object@com[, c("com_qs_1", "amount", "amount_after_treaty_1") :=
                   list(qs@com * (amount - amount_after_treaty_1), NULL, NULL)]

    } else{
      object@clm[portfolio %in% ptf, paste0("amount_after_treaty_", c - 3) := (1 - qs@csn_clm) * get(paste0("amount_after_treaty_", c - 4)) ]
      object@clm[!(portfolio %in% ptf), paste0("amount_after_treaty_", c - 3) := get(paste0("amount_after_treaty_", c - 4)) ]
      object@prm[portfolio %in% ptf, paste0("amount_after_treaty_", c - 3) := (1 - qs@csn_prm) * get(paste0("amount_after_treaty_", c - 4)) ]
      object@prm[!(portfolio %in% ptf), paste0("amount_after_treaty_", c - 3) := get(paste0("amount_after_treaty_", c - 4)) ]
      object@com <- merge(object@com, object@prm[, c("year", "portfolio", paste0("amount_after_treaty_", c - 4), paste0("amount_after_treaty_", c - 3)), with = FALSE], all.x = TRUE, by = c("year", "portfolio"))
      object@com[, c(paste0("com_qs_", c - 3), paste0("amount_after_treaty_", c - 4), paste0("amount_after_treaty_", c - 3)) :=
                   list(qs@com * (get(paste0("amount_after_treaty_", c - 4)) - get(paste0("amount_after_treaty_", c - 3))), NULL, NULL)]
    }
    object@trt <- c(object@trt, qs)
    return(object)
  }
)


## Apply XL treaty to claims method (internal)

#' @import data.table
#' @importFrom dplyr "%>%" group_by mutate rename rename_ summarise select left_join
setGeneric(
  name = "apply_treaty_xl",
  def = function(object, xl)
  {
    standardGeneric("apply_treaty_xl")
  }
)

setMethod(
  f = "apply_treaty_xl",
  signature = c("Claims", "XL"),
  definition = function(object, xl)
  {
    c <- ncol(object@clm)
    portfolio <- amount <- amount_after_treaty_1 <- simulId <- amount_tot <- amount_reins_tot <-
      amount_reins <- amount_reins_tot_a <- premium_reco<- NULL
    if(xl@ptf[1] == "all") {
      ptf <- unique(object@clm$portfolio)
    } else{
      ptf <- xl@ptf
      if(! all(ptf %in% unique(object@clm$portfolio))) stop("One of the selected portfolio is not in the claim database.")
    }
    if(c == 4){
      basis <- object@clm[portfolio %in% ptf]
      max_rns <- length(xl@rns) * xl@lim
      basis <- basis %>%
        mutate(amount_reins = pmin(pmax(0, amount - xl@ded), xl@lim)) %>%
        group_by(year, simulId) %>%
        mutate(amount_reins_tot = cumsum(amount_reins),
               amount_reins_tot_a = pmin(pmax(0, amount_reins_tot - xl@aad), xl@aal, max_rns),
               amount_reins_a = re_cumsum(amount_reins_tot_a)) %>%
        data.table()
      pr <- object@prm[portfolio %in% ptf] %>%
        group_by(year) %>%
        summarise(amount = sum(amount)) %>%
        data.table()
      reco <- basis %>%
        select(year,simulId, amount_reins_tot_a) %>%
        group_by(year, simulId) %>%
        summarise(reco = max(amount_reins_tot_a) / xl@lim) %>%
        left_join(pr, c("year")) %>%
        mutate(premium_reco = sapply(reco, function(x) prm_rns(x, xl@rns)) * amount * xl@prm) %>%
        data.table()
      object@clm[portfolio %in% ptf, amount_after_treaty_1 := amount - basis$amount_reins_a ]
      object@clm[!(portfolio %in% ptf), amount_after_treaty_1 := amount]
      object@prm[portfolio %in% ptf, amount_after_treaty_1 := amount * (1 - xl@prm) ]
      object@prm[!(portfolio %in% ptf), amount_after_treaty_1 := amount ]
      object@rns <- merge(object@rns, reco, by = c("year", "simulId"), all.x = TRUE )
      object@rns[, c("reinstatement_xl_1", "reco", "amount", "premium_reco") :=
                   list(premium_reco, NULL, NULL, NULL)]
    } else{
      basis <- object@clm[portfolio %in% ptf, c("year", "simulId", "portfolio", paste0("amount_after_treaty_", c - 4)), with = FALSE]
      max_rns <- length(xl@rns) * xl@lim
      basis <- basis %>%
        rename_(.dots = setNames(list(paste0("amount_after_treaty_", c - 4)), "amount")) %>%
        mutate(amount_reins = pmin(pmax(0, amount - xl@ded), xl@lim)) %>%
        group_by(year, simulId) %>%
        mutate(amount_reins_tot = cumsum(amount_reins),
               amount_reins_tot_a = pmin(pmax(0, amount_reins_tot - xl@aad), xl@aal, max_rns),
               amount_reins_a = re_cumsum(amount_reins_tot_a)) %>%
        data.table()
      pr <- object@prm[portfolio %in% ptf, c("year", "portfolio", paste0("amount_after_treaty_", c - 4)), with = FALSE] %>%
        rename_(.dots = setNames(list(paste0("amount_after_treaty_", c - 4)), "amount")) %>%
        group_by(year) %>%
        summarise(amount = sum(amount)) %>%
        data.table()
      reco <- basis %>%
        select(year,simulId, amount_reins_tot_a) %>%
        group_by(year, simulId) %>%
        summarise(reco = max(amount_reins_tot_a) / xl@lim) %>%
        left_join(pr, c("year")) %>%
        mutate(premium_reco = sapply(reco, function(x) prm_rns(x, xl@rns)) * amount * xl@prm) %>%
        data.table()
      object@clm[portfolio %in% ptf, paste0("amount_after_treaty_", c - 3) := get(paste0("amount_after_treaty_", c - 4)) - basis$amount_reins_a ]
      object@clm[!(portfolio %in% ptf), paste0("amount_after_treaty_", c - 3) := get(paste0("amount_after_treaty_", c - 4))]
      object@prm[portfolio %in% ptf, paste0("amount_after_treaty_", c - 3) := get(paste0("amount_after_treaty_", c - 4)) * (1 - xl@prm) ]
      object@prm[!(portfolio %in% ptf), paste0("amount_after_treaty_", c - 3) := get(paste0("amount_after_treaty_", c - 4)) ]
      object@rns <- merge(object@rns, reco, by = c("year", "simulId"), all.x = TRUE )
      object@rns[, c(paste0("reinstatement_xl_", c - 3), "reco", "amount", "premium_reco") :=
                   list(premium_reco, NULL, NULL, NULL)]
    }
    object@trt <- c(object@trt, xl)
    return(object)
  }
)

## Apply SL treaty to claims method (internal)

#' @import data.table
#' @importFrom dplyr "%>%" group_by mutate rename rename_ summarise select left_join
setGeneric(
  name = "apply_treaty_sl",
  def = function(object, sl)
  {
    standardGeneric("apply_treaty_sl")
  }
)

setMethod(
  f = "apply_treaty_sl",
  signature = c("Claims", "SL"),
  definition = function(object, sl)
  {
    c <- ncol(object@clm)
    portfolio <- amount <- amount_after_treaty_1 <- simulId <- amount_tot <- amount_reins_tot <-
      amount_reins <- amount_reins_tot_a <- premium_reco<- NULL
    if(sl@ptf[1] == "all") {
      ptf <- unique(object@clm$portfolio)
    } else{
      ptf <- sl@ptf
      if(! all(ptf %in% unique(object@clm$portfolio))) stop("One of the selected portfolio is not in the claim database.")
    }
    if(c == 4){
      basis <- object@clm[portfolio %in% ptf]
      basis <- basis %>%
        group_by(year, simulId) %>%
        mutate(amount_tot = cumsum(amount),
               amount_reins_tot = pmin(pmax(0, amount_tot - sl@ded), sl@lim),
               amount_reins = re_cumsum(amount_reins_tot)) %>%
        data.table()
      object@clm[portfolio %in% ptf, amount_after_treaty_1 := amount - basis$amount_reins ]
      object@clm[!(portfolio %in% ptf), amount_after_treaty_1 := amount]
      object@prm[portfolio %in% ptf, amount_after_treaty_1 := amount * (1 - sl@prm) ]
      object@prm[!(portfolio %in% ptf), amount_after_treaty_1 := amount ]
    } else{
      basis <- object@clm[portfolio %in% ptf, c("year", "simulId", "portfolio", paste0("amount_after_treaty_", c - 4)), with = FALSE]
      basis <- basis %>%
        rename_(.dots = setNames(list(paste0("amount_after_treaty_", c - 4)), "amount")) %>%
        group_by(year, simulId) %>%
        mutate(amount_tot = cumsum(amount),
               amount_reins_tot = pmin(pmax(0, amount_tot - sl@ded), sl@lim),
               amount_reins = re_cumsum(amount_reins_tot)) %>%
        data.table()
      object@clm[portfolio %in% ptf, paste0("amount_after_treaty_", c - 3) := get(paste0("amount_after_treaty_", c - 4)) - basis$amount_reins ]
      object@clm[!(portfolio %in% ptf), paste0("amount_after_treaty_", c - 3) := get(paste0("amount_after_treaty_", c - 4))]
      object@prm[portfolio %in% ptf, paste0("amount_after_treaty_", c - 3) := get(paste0("amount_after_treaty_", c - 4)) * (1 - sl@prm) ]
      object@prm[!(portfolio %in% ptf), paste0("amount_after_treaty_", c - 3) := get(paste0("amount_after_treaty_", c - 4)) ]
    }
    object@trt <- c(object@trt, sl)
    return(object)
  }
)

## Show method

#' @describeIn Claims show method
#' @param object The object to display
#' @import data.table
#' @importFrom stats setNames
#' @importFrom utils View
#' @importFrom dplyr "%>%" group_by mutate rename rename_ summarise select left_join summarise_at matches
#' @export
setMethod("show" ,"Claims" ,
          function( object ){
            portfolio <- rns_gain <- amount.clm <- simulId <-  amount.prm <-  NULL
            c <- ncol(object@clm)
            if( c == 4){
              clm <- object@clm[, 1:4, with = FALSE]
              prm <- object@prm[, 1:3, with = FALSE]
              out <- clm %>%
                group_by(year, simulId, portfolio) %>%
                summarise_at(vars(matches('amount')), sum) %>%
                left_join(prm, by = c("year", "portfolio"), suffix = c(".clm", ".prm")) %>%
                data.table()
              out <- out %>%
                  group_by(year, portfolio) %>%
                  summarise_at(vars(matches('amount')), mean) %>%
                  data.table()
            } else{
              clm <- object@clm[, c("year", "simulId", "portfolio","amount", paste0("amount_after_treaty_", c - 4)), with = FALSE]
              prm <- object@prm[, c("year", "portfolio","amount", paste0("amount_after_treaty_", c - 4)), with = FALSE]
              out <- clm %>%
                group_by(year, simulId, portfolio) %>%
                summarise_at(vars(matches('amount')), sum) %>%
                left_join(prm, by = c("year", "portfolio"), suffix = c(".clm", ".prm")) %>%
                data.table()
              out[, rns_gain := amount.clm - get(paste0("amount_after_treaty_", c - 4, ".clm")) -
                    amount.prm + get(paste0("amount_after_treaty_", c - 4, ".prm"))]
              out <- out %>%
                  group_by(year, portfolio) %>%
                  summarise_at(vars(matches('amount|rns_gain')), mean) %>%
                  data.table()
            }
            show(out)
            View(out)
          }
)

## Draw method

#' Claims: Plot function
#'
#' \code{draw} produces a graphical representation of an object \code{Claims} (\code{\link{Claims-class}}).
#'
#' @param x The Claims object to represent.
#' @param value Character. The value to consider plot, among:
#' \itemize{
#'  \item{\code{claims}}
#'  \item{\code{premiums}}
#'  \item{\code{reinstatements}}
#'  \item{\code{commissions}}
#'  \item{\code{all}: default value. Compute the profit.}
#' }
#' @param moment Character. Moment for analysis:
#' \itemize{
#'  \item{\code{before}: before reinsurance;}
#'  \item{\code{after}: after reinsurance;}
#'  \item{\code{gain}: default value. Difference in values by application of reinsurance.}
#' }
#'
#' @param output Character. Type of graph to produce:
#' \itemize{
#' \item{\code{boxplot}: default value. Boxplot by year}
#' \item{\code{histogram}: histogram over all years}
#' }
#'
#' @details
#'
#' For boxplots, a red dot represent the mean value for each year.
#'
#' @return a plot
#'
#' @examples
#' c <- data.frame(year = unlist(sapply(2000:2017, function(x) rep(x, rpois(1,3)))))
#' c$amount <- pmax(rnorm(nrow(c), 200000, 100000), 0)
#' p <- aggregate(amount ~ year, c, sum)
#' claims <- claims(c, p)
#' treaty_1 <- xl(ded = 100000, lim = 20000, aad = 5000,
#'                aal = 200000, prm = 0.01, rns = 1)
#' claims <- apply_treaty(claims, treaty_1)
#' draw(claims)
#'
#' @import data.table viridis viridisLite ggplot2
#' @importFrom dplyr "%>%" group_by mutate rename rename_ summarise select left_join summarise_at vars matches
#' @rdname draw-methods
#' @export
setGeneric(
  name = "draw",
  def = function(x, value = "all", moment = "gain", output = "boxplot")
  {
    standardGeneric("draw")
  }
)

#' @rdname draw-methods
setMethod(
  "draw", signature = "Claims",
  definition = function(x, value = "all", moment = "gain", output = "boxplot"){
    simulId <- commission <- rns_gain <- amount.clm <- portfolio <-  amount.prm <- reinstatement <- NULL
    if(!(value %in% c("claims", "premiums", "commissions", "reinstatements", "all"))) stop("Invalid value for value")
    if(!(moment %in% c("before", "after", "gain"))) stop("Invalid value for moment")
    if(!(output %in% c("boxplot", "histogram"))) stop("Invalid value for output")
    c <- ncol(x@clm)
    if( c == 4){
      clm <- x@clm[, 1:4, with = FALSE]
      prm <- x@prm[, 1:3, with = FALSE]
      out <- clm %>%
        group_by(year, simulId, portfolio) %>%
        summarise_at(vars(matches('amount')), sum) %>%
        left_join(prm, by = c("year", "portfolio"), suffix = c(".clm", ".prm")) %>%
        data.table()
      out <- out %>%
        group_by(year, simulId) %>%
        summarise_at(vars(matches('amount')), sum) %>%
        data.table()
    } else{
      clm <- x@clm[, c("year", "simulId", "portfolio","amount", paste0("amount_after_treaty_", c - 4)), with = FALSE]
      prm <- x@prm[, c("year", "portfolio","amount", paste0("amount_after_treaty_", c - 4)), with = FALSE]
      out <- clm %>%
        group_by(year, simulId, portfolio) %>%
        summarise_at(vars(matches('amount')), sum) %>%
        left_join(prm, by = c("year", "portfolio"), suffix = c(".clm", ".prm")) %>%
        data.table()
      out <- out %>%
        group_by(year, simulId) %>%
        summarise_at(vars(matches('amount')), sum) %>%
        data.table()
      c2 <- ncol(x@rns)
      if(c2 > 2){
        rns <- x@rns[, c(1,2, c2), with = FALSE]
        colnames(rns)[3] <- "reinstatement"
        out <- out %>%
          left_join(rns, by = c("year", "simulId")) %>%
          data.table()
      } else {
        out$reinstatement <- 0
      }
      c2 <- ncol(x@com)
      if(c2 >2){
        com <- x@com[, c(1,2, c2), with = FALSE]
        colnames(com)[3] <- "commission"
        com <- com %>%
          group_by(year) %>%
          summarise(commission = sum(commission)) %>%
          data.table()
        out <- out %>%
          left_join(com, by = c("year")) %>%
          data.table()
      } else {
        out$commission <- 0
      }
      out[, rns_gain := amount.clm - get(paste0("amount_after_treaty_", c - 4, ".clm")) -
            amount.prm + get(paste0("amount_after_treaty_", c - 4, ".prm")) +
            commission - reinstatement]
    }
    if(value == "claims"){
      if( moment == "before"){
        dataplot <- data.table(x = out$year,
                               simulId = out$simulId,
                               value = out$amount.clm)
        colnames(dataplot)[3] <- "value"
        dataplot[, mean := mean(value), by = x]
        title <- "Claims amount before reinsurance"
      } else if( moment == "after") {
        dataplot <- data.table(x = out$year,
                               simulId = out$simulId,
                               value = out[, paste0("amount_after_treaty_", c - 4, ".clm"), with = FALSE])
        colnames(dataplot)[3] <- "value"
        dataplot[, mean := mean(value), by = x]
        title <- "Claims amount after reinsurance"
      } else {
        dataplot <- data.table(x = out$year,
                               simulId = out$simulId,
                               value = out$amount.clm - out[, paste0("amount_after_treaty_", c - 4, ".clm"), with = FALSE])
        colnames(dataplot)[3] <- "value"
        dataplot[, mean := mean(value), by = x]
        title <- "Claims taken by the reinsurer"
      }
    } else if(value == "premiums"){
      if( moment == "before"){
        dataplot <- data.table(x = out$year,
                               simulId = out$simulId,
                               value = out$amount.prm)
        colnames(dataplot)[3] <- "value"
        dataplot[, mean := mean(value), by = x]
        title <- "Premium amount before reinsurance"
      } else if( moment == "after") {
        dataplot <- data.table(x = out$year,
                               simulId = out$simulId,
                               value = out[, paste0("amount_after_treaty_", c - 4, ".prm"), with = FALSE])
        colnames(dataplot)[3] <- "value"
        dataplot[, mean := mean(value), by = x]
        title <- "Premium amount after reinsurance"
      } else {
        dataplot <- data.table(x = out$year,
                               simulId = out$simulId,
                               value = out$amount.prm - out[, paste0("amount_after_treaty_", c - 4, ".prm"), with = FALSE])
        colnames(dataplot)[3] <- "value"
        dataplot[, mean := mean(value), by = x]
        title <- "Premium taken by the reinsurer"
      }
    } else if(value == "commissions"){
      dataplot <- data.table(x = out$year,
                             simulId = out$simulId,
                             value = out$commission)
      colnames(dataplot)[3] <- "value"
      dataplot[, mean := mean(value), by = x]
      title <- "Commission given back from the reinsurer"
    } else if(value == "reinstatements"){
      dataplot <- data.table(x = out$year,
                             simulId = out$simulId,
                             value = out$reinstatement)
      colnames(dataplot)[3] <- "value"
      dataplot[, mean := mean(value), by = x]
      title <- "Reinstatement paid to the reinsurer"
    } else{
      if( moment == "before"){
        dataplot <- data.table(x = out$year,
                               simulId = out$simulId,
                               value = out$amount.prm - out$amount.clm)
        colnames(dataplot)[3] <- "value"
        dataplot[, mean := mean(value), by = x]
        title <- "Profit before reinsurance"
      } else if( moment == "after") {
        dataplot <- data.table(x = out$year,
                               simulId = out$simulId,
                               value = out[, paste0("amount_after_treaty_", c - 4, ".prm"), with = FALSE] -
                                 out[, paste0("amount_after_treaty_", c - 4, ".clm"), with = FALSE] +
                                 out$commission - out$reinstatement)
        colnames(dataplot)[3] <- "value"
        dataplot[, mean := mean(value), by = x]
        title <- "Profit after reinsurance"
      } else {
        dataplot <- data.table(x = out$year,
                               simulId = out$simulId,
                               value = out$rns_gain)
        colnames(dataplot)[3] <- "value"
        dataplot[, mean := mean(value), by = x]
        title <- "Profit surplus earned from reinsurance"
      }
    }
    if(output == "boxplot"){
      ggplot(dataplot) +
        geom_boxplot(aes(x = x, y = value, group = x, fill = mean, color = mean)) +
        geom_point(aes(x = x, y = mean), color = "red") +
        scale_fill_viridis(discrete = FALSE, "Mean value") +
        scale_color_viridis(discrete = FALSE, "Mean value") +
        theme_minimal() +
        xlab("year") +
        ylab("value") +
        ggtitle(title)
    } else {
      ggplot(dataplot, aes(value, fill = factor(x))) +
        geom_histogram(bins = sqrt(nrow(dataplot))) +
        scale_fill_viridis(discrete = TRUE, "years") +
        theme_minimal() +
        xlab("value") +
        ylab("") +
        ggtitle(title)
    }
  }
)

## summy method

#' Claims: Summary function
#'
#' \code{summy} summarizes the data contained in the object \code{Claims} (\code{\link{Claims-class}}).
#'
#' @param object The Claims object to represent.
#' @param op Character. The aggregation operation over the simulIds, among:
#' \itemize{
#'  \item{\code{mean}: default value.}
#'  \item{\code{sd}}
#'  \item{\code{median}}
#'  \item{\code{min}}
#'  \item{\code{max}}
#' }
#'
#' @return The summarized data
#'
#' @examples
#' c <- data.frame(year = unlist(sapply(2000:2017, function(x) rep(x, rpois(1,3)))))
#' c$amount <- pmax(rnorm(nrow(c), 200000, 100000), 0)
#' p <- aggregate(amount ~ year, c, sum)
#' claims <- claims(c, p)
#' treaty_1 <- xl(ded = 100000, lim = 20000, aad = 5000,
#'                aal = 200000, prm = 0.01, rns = 1)
#' claims <- apply_treaty(claims, treaty_1)
#' summy(claims)
#'
#' @import data.table
#' @importFrom dplyr "%>%" group_by mutate rename rename_ summarise select left_join summarise_at vars matches
#' @rdname summy-methods
#' @export
setGeneric(
  name = "summy",
  def = function(object, op = "mean")
  {
    standardGeneric("summy")
  }
)

#' @rdname summy-methods
setMethod("summy" ,"Claims" ,
          function( object, op = "mean"){
            c <- ncol(object@clm)
            simulId <- commission <- rns_gain <- amount.clm <- portfolio <-  amount.prm <- reinstatement <- NULL
            if( c == 4){
              clm <- object@clm[, 1:4, with = FALSE]
              prm <- object@prm[, 1:3, with = FALSE]
              out <- clm %>%
                group_by(year, simulId, portfolio) %>%
                summarise_at(vars(matches('amount')), sum) %>%
                left_join(prm, by = c("year", "portfolio"), suffix = c(".clm", ".prm")) %>%
                data.table()
              out <- out %>%
                group_by(year, simulId) %>%
                summarise_at(vars(matches('amount')), sum) %>%
                group_by(year) %>%
                summarise_at(vars(matches('amount')), get(op)) %>%
                data.table()
            } else{
              clm <- object@clm[, c("year", "simulId", "portfolio","amount", paste0("amount_after_treaty_", c - 4)), with = FALSE]
              prm <- object@prm[, c("year", "portfolio","amount", paste0("amount_after_treaty_", c - 4)), with = FALSE]
              out <- clm %>%
                group_by(year, simulId, portfolio) %>%
                summarise_at(vars(matches('amount')), sum) %>%
                left_join(prm, by = c("year", "portfolio"), suffix = c(".clm", ".prm")) %>%
                data.table()
              out <- out %>%
                group_by(year, simulId) %>%
                summarise_at(vars(matches('amount')), sum) %>%
                data.table()
              c2 <- ncol(object@rns)
              if(c2 > 2){
                rns <- object@rns[, c(1,2, c2), with = FALSE]
                colnames(rns)[3] <- "reinstatement"
                out <- out %>%
                  left_join(rns, by = c("year", "simulId")) %>%
                  data.table()
              } else {
                out$reinstatement <- 0
              }
              c2 <- ncol(object@com)
              if(c2 >2){
                com <- object@com[, c(1,2, c2), with = FALSE]
                colnames(com)[3] <- "commission"
                com <- com %>%
                  group_by(year) %>%
                  summarise(commission = sum(commission)) %>%
                  data.table()
                out <- out %>%
                  left_join(com, by = c("year")) %>%
                  data.table()
              } else {
                out$commission <- 0
              }
              out[, rns_gain := amount.clm - get(paste0("amount_after_treaty_", c - 4, ".clm")) -
                    amount.prm + get(paste0("amount_after_treaty_", c - 4, ".prm")) +
                    commission - reinstatement]
              out <- out %>%
                group_by(year) %>%
                summarise_at(vars(matches('amount|reinstatement|commission|rns_gain')), get(op)) %>%
                data.table()
            }
            return(data.frame(out))
            View(out)
          }
)

## Apply treaty function

#' Claims: apply a treaty
#'
#' \code{apply_treaty} applies a treaty on an object of type \code{Claims} (\code{\link{Claims-class}}).
#'
#' @param claims \code{Claims} (\code{\link{Claims-class}}) object
#' @param treaty \code{Treaty} (\code{\link{QS-class}} or \code{\link{XL-class}} or \code{\link{SL-class}}) object
#'
#' @return The updated \code{Claims} object
#'
#' @examples
#' c <- data.frame(year = unlist(sapply(2000:2017, function(x) rep(x, rpois(1,3)))))
#' c$amount <- pmax(rnorm(nrow(c), 200000, 100000), 0)
#' p <- aggregate(amount ~ year, c, sum)
#' claims <- claims(c, p)
#' treaty_1 <- xl(ded = 100000, lim = 20000, aad = 5000,
#'                aal = 200000, prm = 0.01, rns = 1)
#' claims <- apply_treaty(claims, treaty_1)
#'
#' @export
apply_treaty <- function(claims, treaty){
  if(treaty@trt == "XL"){
    return(apply_treaty_xl(claims, treaty))
  } else if (treaty@trt == "SL"){
    return(apply_treaty_sl(claims, treaty))
  } else {
    return(apply_treaty_qs(claims, treaty))
  }
}

## Getters

#' Get claims
#'
#' \code{get_claims} gets the claims table of a \code{Claims} (\code{\link{Claims-class}}) object.
#'
#' @param object The object to display
#'
#' @examples
#' c <- data.frame(year = unlist(sapply(2000:2017, function(x) rep(x, rpois(1,3)))))
#' c$amount <- pmax(rnorm(nrow(c), 200000, 100000), 0)
#' p <- aggregate(amount ~ year, c, sum)
#' claims <- claims(c, p)
#' get_claims(claims)
#'
#' @export
#' @docType methods
#' @rdname get_claims-methods
setGeneric(
  name = "get_claims",
  def = function(object)
  {
    standardGeneric("get_claims")
  }
)

#' @rdname get_claims-methods
setMethod(
  f = "get_claims",
  signature = c("Claims"),
  definition = function(object)
  {
    return(object@clm)
  }
)

#' Get premiums
#'
#' \code{get_premiums} gets the premiums table of a \code{Claims} (\code{\link{Claims-class}}) object.
#'
#' @param object The object to display
#'
#' @examples
#' c <- data.frame(year = unlist(sapply(2000:2017, function(x) rep(x, rpois(1,3)))))
#' c$amount <- pmax(rnorm(nrow(c), 200000, 100000), 0)
#' p <- aggregate(amount ~ year, c, sum)
#' claims <- claims(c, p)
#' get_premiums(claims)
#'
#' @export
#' @rdname get_premiums-methods
#' @docType methods
setGeneric(
  name = "get_premiums",
  def = function(object)
  {
    standardGeneric("get_premiums")
  }
)

#' @rdname get_premiums-methods
setMethod(
  f = "get_premiums",
  signature = c("Claims"),
  definition = function(object)
  {
    return(object@prm)
  }
)

#' Get commissions
#'
#' \code{get_commissions} gets the commissions induced by QS treaties applied to \code{Claims} (\code{\link{Claims-class}}) object.
#'
#' @param object The object to display
#'
#' @examples
#' c <- data.frame(year = unlist(sapply(2000:2017, function(x) rep(x, rpois(1,3)))))
#' c$amount <- pmax(rnorm(nrow(c), 200000, 100000), 0)
#' p <- aggregate(amount ~ year, c, sum)
#' claims <- claims(c, p)
#' treaty_1 <- qs(0.8, com = 0.25)
#' claims <- apply_treaty(claims, treaty_1)
#' get_commissions(claims)
#'
#' @export
#' @rdname get_commissions-methods
#' @docType methods
setGeneric(
  name = "get_commissions",
  def = function(object)
  {
    standardGeneric("get_commissions")
  }
)

#' @rdname get_commissions-methods
setMethod(
  f = "get_commissions",
  signature = c("Claims"),
  definition = function(object)
  {
    return(object@com)
  }
)

#' Get reinstatements
#'
#' \code{get_reinstatements} gets the reinstatements induced by XL treaties applied to \code{Claims} (\code{\link{Claims-class}}) object.
#'
#' @param object The object to display
#'
#' @examples
#' c <- data.frame(year = unlist(sapply(2000:2017, function(x) rep(x, rpois(1,3)))))
#' c$amount <- pmax(rnorm(nrow(c), 200000, 100000), 0)
#' p <- aggregate(amount ~ year, c, sum)
#' claims <- claims(c, p)
#' treaty_1 <- xl(ded = 100000, lim = 20000, aad = 5000,
#'                aal = 200000, prm = 0.01, rns = 1)
#' claims <- apply_treaty(claims, treaty_1)
#' get_reinstatements(claims)
#'
#' @export
#' @rdname get_reinstatements-methods
#' @docType methods
setGeneric(
  name = "get_reinstatements",
  def = function(object)
  {
    standardGeneric("get_reinstatements")
  }
)

#' @rdname get_reinstatements-methods
setMethod(
  f = "get_reinstatements",
  signature = c("Claims"),
  definition = function(object)
  {
    return(object@rns)
  }
)

#' Get treaties
#'
#' \code{get_treaties} gets the characteristics of treaties applied to \code{Claims} (\code{\link{Claims-class}}) object.
#'
#' @param object The object to display
#'
#' @examples
#' c <- data.frame(year = unlist(sapply(2000:2017, function(x) rep(x, rpois(1,3)))))
#' c$amount <- pmax(rnorm(nrow(c), 200000, 100000), 0)
#' p <- aggregate(amount ~ year, c, sum)
#' claims <- claims(c, p)
#' treaty_1 <- qs(0.8, com = 0.25)
#' claims <- apply_treaty(claims, treaty_1)
#' get_treaties(claims)
#'
#' @export
#' @rdname get_treaties-methods
#' @docType methods
setGeneric(
  name = "get_treaties",
  def = function(object)
  {
    standardGeneric("get_treaties")
  }
)

#' @rdname get_treaties-methods
setMethod(
  f = "get_treaties",
  signature = c("Claims"),
  definition = function(object)
  {
    return(object@trt)
  }
)
