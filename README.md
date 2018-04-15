# ahw

Continuous-time weight<sup>[1](#cont_msm)</sup> estimation based on Aalens additive hazard model. The weights, and weighted additive hazard regressions are consistent<sup>[2](#additive_consistent)</sup>. Moreover, (weighted) additive hazard estimates can be transformed to estimate other parameters consistently<sup>[3](#transforming)</sup>, paving the way for causal survival analysis.

## Getting Started


### Prerequisites

Make sure to have the following libraries installed:

```
timereg
data.table
```
It is an advantage to be familiar with ```timereg::aalen```

### Installing

Make sure to have the ``devtools`` package installed. Then run

```
devtools::install_github("palryalen/ahw")
```

## Versioning

We use [SemVer](http://semver.org/) for versioning.

## Authors

* **Pål Ryalen**

## License

This project is licensed under the Creative Commons Attribution-NonCommercial 4.0 International (CC BY-NC 4.0) licence. You are therefore free to [free to](https://creativecommons.org/licenses/by-nc/4.0/)

*Share — copy and redistribute the material in any medium or format
*Adapt — remix, transform, and build upon the material

<a name="cont_msm">1</a>: Kjetil Røysland (2011), *A martingale approach to continuous-time marginal structural models*, Bernoulli, [link](https://projecteuclid.org/euclid.bj/1310042849)


<a name="additive_consistent">2</a>: Pål Ryalen, Mats Stensrud, Kjetil Røysland, [*The additive hazard estimator is consistent for continuous time marginal structural models*](https://arxiv.org/abs/1802.01946) 


<a name="transforming">3</a>: Pål Ryalen, Mats Stensrud, Kjetil Røysland, [*Transforming cumulative hazard estimates*](https://arxiv.org/abs/1710.07422v3)

<!---
[^fn1]: Kjetil Røysland (2011), *A martingale approach to continuous-time marginal structural models*, Bernoulli, [link](https://projecteuclid.org/euclid.bj/1310042849)
-->

