# reinsureR

Implementation of the burning cost approach in reinsurance as an R package.

## Getting started

The *reinsureR* package implements the traditional burning cost approach for reinsurance by applying reinsurance treaties on claims in order to measure the impact of reinsurance.

### Prerequisites

**reinsureR** can be installed from github.

The package depends on the followings:

* *data.table*
* *dplyr*
* *ggplot2*
* *viridis* and *viridisLite*

### Installation

The package is available from its GitHub repository.

```r
devtools::install_github("arnaudbu/reinsureR", dependencies = TRUE)
```

## Claims class

This document aims at giving an overview of the functionalities of the package through a simple example. All functions will be shown on this use case.

The `Claims` class is at the center of the package, since all computations will be executed within an object of this class. It relies on two datasets, which are claims and premiums history.

### Claims

Let us generate a claim dataset for illustrating the package with the code below

```r
# Claim table construction
set.seed(1) # For reproducibility
c <- expand.grid(year = unlist(sapply(2000:2017, function(x) rep(x, rpois(1,8)))),
                 portfolio = c("ptf1", "ptf2"),
                 simulId = 1:100)
c$amount <- pmax(rnorm(nrow(c), 200000, 100000), 0)
c[sample(1:nrow(c), 5), ]
```

This code produces a table, for which five randomly picked rows are:

```r
year portfolio simulId   amount
2003      ptf1      49 317274.3
2014      ptf2      56 143056.1
2017      ptf2      99 250803.6
2000      ptf2      86 431303.8
2001      ptf1      69 250328.3
```

The idea is to regroup claims amounts by three values:
+ *year*: the year of occurence. Their can be more than one amount by year, that represents the different events that occured;
+ *portfolio*: the associated portfolio. In the case we want to apply a treaty only on a fraction of the claims, the best way is to segment them with different portfolios;
+ *simulId*: a simulation id. In case we want to test reinsurance on model-generated claims, it is possible to use different scenarios.

The columns *portfolio* and *simulId* are optional in order to feed the model with a claim dataset.

### Premiums

Let us construct the associated premiums for each year and portfolio:

```r
# Premium table construction
p <- aggregate(amount ~ year + portfolio + simulId, c, sum)
p <- aggregate(amount ~ year + portfolio, p, mean)
p$amount[p$portfolio == "ptf1"] <- mean(p$amount[p$portfolio == "ptf1"])
p$amount[p$portfolio == "ptf2"] <- mean(p$amount[p$portfolio == "ptf2"])
```
The asked premium is simply the mean amount of claims over the years, without any regard for fees. Thus, our imaginary contract is fairly priced.

Unlike claims, premiums can not be stochastics. Without that, columns are named the same way.

### Construction

An object of class `Claims` is simply constructed by calling the function `claims` on the two previously generated tables:

```r
claims <- claims(c, p)
```
The slots of such an object are:
+ *clm*: the claim table given as an input for the constructor;
+ *prm*: the premium table, also given as an input for the constructor;
+ *rns*: reinstatements premiums paid on Excess of Loss treaties;
+ *com*: commissions from Quota Share treaties;
+ *trt*: list of treaties applied.

All those values can be retrieved from their dedicated getters:

```r
cl <- get_claims(claims)
pm <- get_premiums(claims)
cm <- get_commissions(claims)
rn <- get_reinstatements(claims)
tt <- get_treaties(claims)
```

## Treaties classes

Three types of treaties are available within the package. They can all be applied to the `Claims` object with the function `apply_treaty`:

```r
claims <- apply_treaty(claims, treaty)
```

### Quota Share

### Excess Of Loss

### Stop Loss

## Visualization

### Summarizing

### Ploting

## Authors

* **Arnaud Buzzi** - *reinsureR package* - [View on GitHub](https://github.com/ArnaudBu)
