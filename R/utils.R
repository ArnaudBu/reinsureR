## Utility function for package reinsureR ##
############################################

## Validity check for Claims class

#' @import data.table
check_claim <- function(object){
  if(!all(c("year", "amount") %in% colnames(object@clm))) stop("Columns year or amount are mandatory for claims.")
  if(!all(colnames(object@clm) %in% c("year", "amount", "portfolio", "simulId"))) stop ("Invalid column name detected in claims. Valid columns are year, portfolio, simulId and amount.")
  if(!all(c("year", "amount") %in% colnames(object@prm))) stop("Columns year or amount are mandatory for premiums.")
  if(!all(colnames(object@prm) %in% c("year", "amount", "portfolio"))) stop ("Invalid column name detected in premiums. Valid column names are year, portfolio and amount.")
  year <- portfolio <- NULL
  dt1 <- setorder(unique(object@prm[, c("year", "portfolio")]), year, portfolio)
  dt2 <- setorder(unique(object@clm[, c("year", "portfolio")]), year, portfolio)
  if(!(nrow(dt1) == nrow(dt2))) stop("Consistency problem between premiums and claims tables. Some combination year x portfolios are not matched between the two tables")
  if(!all(dt1 == dt2)) stop("Consistency problem between premiums and claims tables. Some combination year x portfolios are not matched between the two tables")
}

## Validity check for QS class

check_qs <- function(object){
  if(!(object@csn_clm>= 0 & object@csn_clm <= 1)) stop("Claims cession rate must be a numeric value between 0 and 1.")
  if(!(object@csn_prm >= 0 & object@csn_prm <= 1)) stop("Premiums cession rate must be a numeric value between 0 and 1.")
  if(!(object@com >= 0 & object@com <= 1)) stop("Commission rate must be a numeric value between 0 and 1")
}

## Validity check for XL class

check_xl <- function(object){
  if(!(object@ded >= 0)) stop("Deductible must be a numeric value superior to 0")
  if(!(object@lim >= 0)) stop("Limit must be a numeric value superior to 0")
  if(!(object@aad >= 0)) stop("Annual Aggregate Deductible must be a numeric value superior to 0")
  if(!(object@aal >= 0)) stop("Annual Aggregate Limit must be a numeric value superior to 0")
  if(!(object@prm >= 0 & object@prm <= 1)) stop("Premium rate must be a numeric value between 0 and 1")
  if(!is.numeric(object@rns) | !(all(object@rns >=0))) stop("Reinstatements must be a numeric vector with values superior to 0")
}

## Validity check for SL class

check_sl <- function(object){
  if(!(object@ded >= 0)) stop("Deductible must be a numeric value superior to 0")
  if(!(object@lim >= 0)) stop("Limit must be a numeric value superior to 0")
  if(!(object@prm >= 0 & object@prm <= 1)) stop("Premium rate must be a numeric value between 0 and 1")
}

## Reverse cumsum

re_cumsum <- function(x){
  return(c(x[1],diff(x)))
}

## Reinstatement premium

prm_rns <- function(n_lim, rns){
  n_int = floor(n_lim)
  n_dec = n_lim %% 1
  if(n_int > 0){
    if(n_dec > 0){
      return(sum(rns[1:n_int]) + n_dec * rns[n_int + 1])
    } else {
      return(sum(rns[1:n_int]))
    }
  } else {
    return(n_dec * rns[1])
  }
}
