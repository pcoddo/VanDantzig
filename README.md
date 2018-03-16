# VanDantzig #


### Introduction ###
This document contains the code used to generate the analysis found in Oddo et al. (2017), *Deep Uncertainties in Sea-Level Rise and Storm Surge Projections: Implications for Coastal Flood Risk Management.*

Full Citation:
> Oddo, P. C., Lee, B. S., Garner, G. G., Srikrishnan, V., Reed, P. M., Forest, C. E. and Keller, K. (2017), Deep Uncertainties in Sea-Level Rise and Storm Surge Projections: Implications for Coastal Flood Risk Management. Risk Analysis. doi:10.1111/risa.12888. [(View Online)](http://onlinelibrary.wiley.com/doi/10.1111/risa.12888/full)

### Analysis Overview ###
In this analysis we examine the effects of model and parameter uncertainties underlying the decision-analytical model described in Van Dantzig (1956):

> Van Dantzig, David. "Economic decision problems for flood prevention." Econometrica: Journal of the Econometric Society (1956): 276-287.

The Van Dantzig (1956) model uses a cost-benefit framework to determine the optimal dike heightening for a single polder. In this model, the costs of investing into flood protection are weighed against the expected damages from a flood event, given a set of economic and geophysical parameters:

| Parameter      | Value     | Description                                                                      |
|:---------------|-----------|:---------------------------------------------------------------------------------|
| p_zero_p       |   0.0038  | Initial flood frequency (1/yr) with zero height increase                         |
| alpha_p        |    2.6    | Exponential flood frequency constant                                             |
| V_p            | 1e+10 * 2 | Value of goods protected by dike (includes "factor of 2" for cost of human life) |
| delta_prime_p  |    0.02   | Discount rate (percent/year)                                                     |
| k_p            |   4.2e7   | Cost of heightening dikes by 1 meter (Guilders)                                  |
| subs_rate      |   0.002   | Rate of land subsidence (meter/year)                                             |
| sea_level_rate |   0.008   | Rate of sea level rise (meter/year)                                              |


Our analysis is evaluates the effects of increasing parametric and structural model uncertainties using the Van Dantzig (1956) model as a baseline. Each component of the analysis is contained in the following folders:

1. `Baseline_Model`
2. `Parametric_Uncertainty`
3. `Uncertainty_SLR`
4. `Uncertainty_SLR_GEV`

### Model Versions ###
* The `Baseline_Model` recreates the original anylsis described in Van Dantzig (1956), using the best-guess parameter values provided in *Section 6: "The Doubtful Constants."*
* The `Parameteric_Uncertainty` version uses the same structure as the baseline, but introduces uncertainty in the parameter values.
* The `Uncertainty_SLR` version introduces uncertainty in the model structure but implementing an updated sea-level rise model.
* The `Uncertainty_SLR_GEV` version introduces storm surge projections generated using a generalized extreme value (GEV) analysis.

### R Packages ###
* zoo
* lubridate
* lhs
* extrafont
* fExtremes
* extRemes
* timeSeries
* compiler
* fields
* KernSmooth
* RColorBrewer
* MASS
* sfsmisc