# ahw

Continuous-time weight[^fn1] estimation based on Aalens additive hazard model. The weights, and weighted additive hazard regressions are consistent[^fn2]. Moreover, (weighted) additive hazard estimates can be transformed to estimate other parameters consistently[^fn3], paving the way for causal survival analysis.

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

## Acknowledgments

* Hat tip to anyone who's code was used
* Inspiration
* etc

[^fn1]: Kjetil Røysland (2011), *A martingale approach to continuous-time marginal structural models*, Bernoulli, [link](https://projecteuclid.org/euclid.bj/1310042849)
[^fn2]: Pål Ryalen, Mats Stensrud, Kjetil Røysland, *The additive hazard estimator is consistent for continuous time marginal structural models*, [arXiv link](https://arxiv.org/abs/1802.01946)
[^fn3]: Pål Ryalen, Mats Stensrud, Kjetil Røysland, *Transforming cumulative hazard estimates*, [arXiv link](https://arxiv.org/abs/1710.07422v3)
