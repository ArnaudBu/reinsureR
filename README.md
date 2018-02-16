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

In the examples, treaties are priced such that no gain can be made from reinsurance, which can be verified after the application of each treaty by the code  `mean(summy(claims)$rns_gain)`, explained later.

Reinsurance treaties are applied on the amounts considered after application of **all** previous treaties.

In this example, a quota share treaty will first be applied on the first portfolio, then an excess of loss treaty will be applied on the second, before applying a stop loss treaty on both portfolios.

### Quota Share

Quota Share treaties are constructed from their constructor `qs`.

```r
treaty_qs <- qs(csn_clm = 0.8,
               com = 0,
               ptf = "ptf1")

claims <- apply_treaty(claims, treaty_qs)
```

### Excess Of Loss

Quota Share treaties are constructed from their constructor `xl`.

```r
treaty_xl <- xl(ded = 200000,
                lim = 30000,
                aad = 0,
                aal = Inf,
                prm = rate_prm,
                rns = c(0,0),
                ptf = "ptf2")

claims <- apply_treaty(claims, treaty_xl)
```

### Stop Loss

Stop Loss treaties are constructed from their constructor `sl`.

```r
pp <- aggregate(amount ~ year + simulId + portfolio, c, sum)
pp$amount[pp$portfolio == "ptf1"] <- pp$amount[pp$portfolio == "ptf1"] * 0.2
pp <- aggregate(amount ~ year + simulId, pp, sum)
r$amount_treaty[r$portfolio == "ptf1"] <- 0
r <- aggregate(amount_treaty ~ year + simulId, r, sum)
t <- summy(claims)
rate_prm = mean(pmin(pmax(pp$amount - r$amount_treaty - 3000000, 0), 500000)) / mean(t$amount_after_treaty_2.prm)

treaty_sl <- sl(ded = 3000000,
                lim = 500000,
                prm = rate_prm,
                ptf = "all")

claims <- apply_treaty(claims, treaty_sl)
```

## Visualization

### Summarizing

A summary of the effect of reinsurance on all portfolios can be obtain via the `summy` function:

```r
summy(claims, "mean")
```

The function takes two arguments:
+ **claims**: the claim object;
+ **op**: the operation to be performed for the summary, such as "mean", "sd", "median", "min", "max".

The column **rns_gain** computes the gain from reinsurance for the ceding company, taking into account the difference in claims and premiums, with commissions and reinstatements.

### Ploting

It is also possible to get a graphical representation of the reinsurance effect on all portfolios with the function `draw`:

```r
draw(x, value = "all", moment = "gain", output = "boxplot")
```

The plotting parameters are:
+ **value**: the value to take into account (claims, premiums, everything, ...);
+ **moment**: the moment of ploting (before or after reinsurance, gain from reinsurance);
+ **output**: the type of graph to produce (boxplot or histogram).

## Authors

* **Arnaud Buzzi** - *reinsureR package* - [View on GitHub](https://github.com/ArnaudBu)
