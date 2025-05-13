---
output: github_document
---

# llmBgmElicit 

NOTE: This package is in development and a paper is in preparation.

**llmBgmElicit** provides tools to elicit prior edge inclusion probabilities in Bayesian graphical models (BGMs) using large language models (LLMs) like GPT-4o. 

The package includes full and simplified elicitation functions, tools for estimating prior distributions over graph structures, and utilities for parsing LLM outputs and managing API keys.

The elicited prior edge inclusion probabilities can be used directly in the R packages **[`bgm`](https://cran.r-project.org/web/packages/bgms/index.html)** and **[`BDgraph`](http://cran.r-project.org/web/packages/BDgraph/index.html)**.

Additionally, `llmBgmElicit` optionally estimates the parameters of a **Beta-Binomial prior** on the number of edges in the network, which can be used as a structure prior in the **[`bgms`](https://cran.r-project.org/web/packages/bgms/index.html)** package.

The package also provides options for changing the in-built prompts used for elicitation, as well as the ability to precompute the cost of the elicitation.

This package is inspired by and partly based on the [`theoraizer`](https://github.com/MeikeWaaijers/theoraizer) package by Meike Waaijers.

---

## 🚀 Installation

You can install the development version from GitHub:

```r

if (!requireNamespace("remotes")) { 
  install.packages("remotes")   
}   
remotes::install_github("sekulovskin/llmBgmElicit")
```

## Usage

With the standard function (recommended):

```r
library(llmBgmElicit)

result_lite <- elicitEdgeProbLite(
  context = paste(
    "This study examines the relationship between screen time,",
    "physical activity, and cardiovascular health."
  ),
  variable_list = c("Screen Time", "Physical Activity", 
  "Cardiovascular Health"),
  LLM_model = "gpt-4o",
  n_perm = 2
)

print(result_lite$relation_df)
```
With the lite function, which is cheaper

```r
result_lite <- elicitEdgeProbLite(
  context = paste(
    "This study examines the relationship between screen time,",
    "physical activity, and cardiovascular health."
  ),
  variable_list = c("Screen Time", "Physical Activity", 
  "Cardiovascular Health"),
  LLM_model = "gpt-4o",
  n_perm = 3
)

print(result_lite$relation_df)
```
Obtaining the beta-Binomial hyperparameters:

```r
beta_params <- estimateBetaBin(result)

print(beta_params)
```
