# bgmElicit

NOTE: This package is still in development and a paper is in preparation.

The package `bgmElicit` provides tools to elicit prior edge inclusion probabilities 
using Large Language Models (LLMs) for analyzing Markov random field graphical models 
within the Bayesian graphical modeling (BGM) framework. Currently, the package supports
models provided by OpenAI. The elicited
prior can then be used in the **[`bgms`](https://cran.r-project.org/web/packages/bgms/index.html)**,
**[`BDgraph`](http://cran.r-project.org/web/packages/BDgraph/index.html)** and **[`easybgm`](https://cran.r-project.org/web/packages/easybgm/index.html)**
R packages for Bayesian graphical modeling.

The output from the main function, `elicitEdgeProb` (or its lightweight counterpart `elicitEdgeProbLite`), which elicit prior inclusion 
probabilities under the Bernoulli prior, can also be used to estimate the parameters of a Beta-Bernoulli
e Stochastic-Block model (SBM) prior.

This package is inspired by and partly based on the [`theoraizer`](https://github.com/MeikeWaaijers/theoraizer) package by Meike Waaijers.

---

## 🚀 Installation

You can install the development version from GitHub:

```r

if (!requireNamespace("remotes")) { 
  install.packages("remotes")   
}   
remotes::install_github("sekulovskin/bgmElicit")
```

## Usage

### Bernoulli Structure Prior

Using the main function (recommended):

```r
library(bgmElicit)

result_lite <- elicitEdgeProb(
  context = paste(
    "This study examines the relationship between screen time,",
    "physical activity, and cardiovascular health."
  ),
  variable_list = c("Screen Time", "Physical Activity", 
  "Cardiovascular Health"),
  LLM_model = "gpt-5",
  n_perm = 2
)

print(result_lite$relation_df)
```

Using the cheaper lite function

```r
result_lite <- elicitEdgeProbLite(
  context = paste(
    "This study examines the relationship between screen time,",
    "physical activity, and cardiovascular health."
  ),
  variable_list = c("Screen Time", "Physical Activity", 
  "Cardiovascular Health"),
  LLM_model = "gpt-5",
  n_perm = 3
)

print(result_lite$relation_df)
```
### Beta-Bernoulli Structure Prior

```r
llm_out <- llmPriorElicitSimple(
  context = "Exploring cognitive symptoms and mood in depression",
  variable_list = c("Concentration", "Sadness", "Sleep"),
  n_rep = 3
)
beta_params <- betaBinParameters(llm_out)
print(beta_params)
```

### SBM Structure Prior

```r
llm_out <- llmPriorElicitSimple(
  context = "Exploring cognitive symptoms and mood in depression",
  variable_list = c("Concentration", "Sadness", "Sleep"),
  n_rep = 3
)
cl <- sbmClusters(
  llmobject = llm_out,
  algorithm = "louvain",
  threshold = 0.5
)
cl$elicited_no_clusters
```


