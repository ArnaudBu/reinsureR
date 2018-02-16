# reinsureR

Implementation of a framework for application of reinsurance treaties on claims datasets, along with statistical and graphical analysis for cost estimation. This package can be used for estimating the impact of reinsurance on several portfolios or for pricing treaties through statistical analysis.

## Getting started

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

This document aims at providing new users with an overview of the functionality brought by the package. For this purpose, a simple example will be developped in order to review all the implemented functions.

The package mainly relies on the `Claims` class, whose purpose is to represent the claims and premiums on which reinsurance will be applied. All computations are executed within objects of this class.

This package is implemented from the perspective of the ceding company. Thus, will be computed claims and premiums kept by the company, and not the amounts paid to the reinsurer or received from it.

### Claims

For the purpose of illustrating the application of reinsurance treaties to a claim dataset, let us create such a dataset.

The objective is to create several claim events per year for a couple of years and two different portfolios.

```r
# Claim table construction
set.seed(1) # For reproducibility
c <- expand.grid(year = unlist(sapply(2000:2017, function(x) rep(x, rpois(1,8)))),
                 portfolio = c("ptf1", "ptf2"),
                 simulId = 1:100)
c$amount <- pmax(rnorm(nrow(c), 200000, 100000), 0)
c[sample(1:nrow(c), 5), ]
```

Five randomly picked rows from the table thus created are:

```r
year portfolio simulId   amount
2003      ptf1      49 317274.3
2014      ptf2      56 143056.1
2017      ptf2      99 250803.6
2000      ptf2      86 431303.8
2001      ptf1      69 250328.3
```

Three dimensions are used in order to group claims:
+ **year**: the occurence year of the event associated to the claim amount. A year can contain more than one claim: it corresponds to the numerous events the treaty may be applied to (in the case of an Excess of Loss treaty).
+ **portfolio**: a segmentation with different portfolios is possible, in order to apply reinsurance treaties on subsets of claims. 
+ **simulId**: a simulation identifier. It is possible to use stochastic modelling in order to simulate claims and test the application of reinsurance over different scenarios.

Note that the there may be several lines associated with a triplet (**year**, **portfolio**, **simulId**), corresponding to different events occuring within a year for a portfolio and a scenario.

### Premiums

A table containing the insurance premiums for each year and portfolio is also needed in order to construct an object of class `Claims`. Note that stochastic aspects are not taken into account for premiums. Except for the column **simulId**, column names are the same as in the table containing the claims. A unique premium amount should be associated to each **year** and **portfolio**.

```r
# Premium table construction
p <- aggregate(amount ~ year + portfolio + simulId, c, sum)
p <- aggregate(amount ~ year + portfolio, p, mean)
p$amount[p$portfolio == "ptf1"] <- mean(p$amount[p$portfolio == "ptf1"])
p$amount[p$portfolio == "ptf2"] <- mean(p$amount[p$portfolio == "ptf2"])
```
For the purpose of the example, we construct the premium table by averaging the yearly claims over the portfolios, without taking into account fee issues.

### Construction

An object of class `Claims` is constructed by calling the function `claims` on the two previously generated tables:

```r
claims <- claims(c, p)
```
The slots of an object of this class are:
+ **clm**: the claim table given as an input for the constructor;
+ **prm**: the premium table, also given as an input for the constructor;
+ **rns**: reinstatements premiums paid to the reinsurer on Excess of Loss treaties;
+ **com**: commissions received from the reinsurer for Quota Share treaties;
+ **trt**: list of treaties applied.

All those values can be retrieved from their dedicated getters:

```r
cl <- get_claims(claims)
pm <- get_premiums(claims)
cm <- get_commissions(claims)
rn <- get_reinstatements(claims)
tt <- get_treaties(claims)
```

## Treaties classes

Three types of treaties are available within the package:
+ Quota Share treaties;
+ Excess of Loss treaties;
+ Stop Loss treaties.

They can all be applied to an object of class `Claims` with the function `apply_treaty`:

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
