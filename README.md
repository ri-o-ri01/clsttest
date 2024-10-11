# Introduction

This project implements a clustering model to classify market regimes and an investment strategy that adjusts portfolio allocations based on the identified regimes. It is organized around key RMarkdown files that generate results, supported by additional files and directories for data processing and analysis.

This project provides a solid framework for managing combinations of market signals, clustering models, and universe (a set of financial instruments) and generates results corresponding to these combinations. Therefore, it enables efficient exploration of different combination settings, allowing users to focus solely on analyzing the most promising combinations while maintaining flexibility to easily switch and test new configurations.



The core of the project consists of two essential RMarkdown files for generating the results:

-   Clustering Model (`clustering_model_generation.Rmd`)

-   Investment Strategy (`clustering_invest_strategy_generation.Rmd`)

Supporting scripts and utilities are integrated into the project to ensure efficient data processing and streamlined analysis. The overall structure is designed to facilitate smooth execution, backtesting, and performance evaluation of the proposed strategies.


# Project Structure

## Directory

The directory structure below organizes the project into essential scripts and RMarkdown files 
used for data generation and analysis.
```
|- README.md 
|- R
   |- run_setup.R          # Setup and initialization script
   |- run_setup_for_user.R # Setup and initialization script for customized cases
   |- cluster              # Functions related to clustering models 
   |- backtest             # Backtesting processes for Investment Strategy
   |- finance              # Finance-related utilities and functions
   |- utils                # Utility functions used across the project
|- rmd
   |- material            # RMarkdown files for data generation
       |- clustering_model_generation.Rmd
       |- clustering_invest_strategy_generation.Rmd
       |- generate.R      # Script to generate required data
   |- analysis            # RMarkdown files for detailed analysis and reporting
   |- test                # Files for testing purposes    
```
To generate the results, simply execute the `generate.R` script.

If you're interested in testing new configurations, 
refer to the **Customized Inputs** section in the README.md.   

## Generation

The generation process relies on parameters defined in the YAML header. For 
details on how these parameters are utilized, refer to the **Loading Parameters 
from YAML Header** section within each file.

Although you can run the two RMarkdown files individually, it's often more 
efficient to generate all necessary results using the `generate.R` script. This 
script streamlines the data generation process, allowing you to obtain results 
without additional configuration.


### 1. **Clustering Model (`clustering_model_generation.Rmd`)**

This RMarkdown file applies a clustering model to historical market data, 
producing market regime classifications. It analyzes various market signals, 
such as volatility and interest rates, to identify patterns and classify 
different market conditions. These classifications are then saved for later use 
in the investment strategy component.

-   **Data Generation**: Automatically generates classified market regimes using 
  clustering algorithms (e.g., Gaussian Mixture Model).
-   **Key Features**:
    -   Customizable market signals
    -   Flexible choice of clustering algorithms (e.g., K-means, GMM)
    -   Parallel processing for efficient computation

### 2. **Investment Strategy (`clustering_invest_strategy_generation.Rmd`)**

TThis component implements a backtesting framework for the investment strategy 
using the results from the clustering model. The strategy determines how the 
portfolio should be rebalanced based on the identified market regimes, performing 
regular rebalancing (e.g., weekly or monthly). The backtest evaluates historical 
performance of multiple financial instruments from periods with the same market 
conditions as the model execution date, classified by the clustering model. This 
ensures robust strategy performance under different market conditions.

-   **Data Usage**: Relies on data generated from the clustering model to adjust 
  portfolio allocations.
-   **Key Features**:
    -   Backtesting framework that simulates historical portfolio rebalancing
    -   Regular rebalancing based on identified market regimes (e.g., weekly or monthly)


### Customized Inputs

This project allows you to customize market signals, clustering models, 
and universe configurations to tailor the analysis to your specific needs. 
By adjusting parameters in `run_setup.R`, you can explore various scenarios 
and fine-tune your investment strategy for optimal performance.

To customize the input parameters, follow these steps:

**1. Market Signals**: 

The available market signals are generated using the `create_market_data_for_clustering2`
function. You can select from the following predefined signals:

```r

"NKY"                   "SPX"                   "USDJPY"                "USGG2YR"              
"USGG10YR"              "VIX"                   "JT7203"                "SPX_ETF"              
"NKY_HV_30D"            "USDJPY_HV_30D"         "JT7203_HV_30D"         "VIX_SMA30D"           
"USGG10YR_SMA30D"       "VIX_SMA30D_DIFF"       "USGG2YR_USGG10YR_DIFF" "USGG10YR_SMA30D_DIFF" 
"USDJPY_SMA5D"          "USDJPY_SMA20D"         "USDJPY_SMA5D20D_RATIO" "NKY_SMA5D"            
"NKY_SMA20D"            "NKY_SMA5D20D_RATIO"  

```

 You can extend these market signals by modifying the 
`create_market_data_for_clustering2` function. By changing the 
`symbol_name_vec` parameter, you can retrieve additional securities from Yahoo 
Finance, enabling more flexible market analysis. 
 
For example, you can define a list of market signals for clustering as follows:

```r
   prod_ref_names_list1 <- list(
     c("VIX", "USGG2YR_USGG10YR_DIFF"),
     c("VIX", "USGG2YR_USGG10YR_DIFF", "USDJPY"),
     c("JT7203_HV_30D", "USDJPY")
   )
```
**2.Clustering Model Parameters**

You can specify the clustering model parameters, including `model_name`, 
`lib_name`, `start_dt`, `seed`, and `clust_num`. The project currently supports 
the K-means model (`model_name="K-means"`, `lib_name="stats"`) and the Gaussian 
Mixture Model (GMM) (`model_name="GMM"`, `lib_name="Mclust"`).

In addition to model selection, you can customize the clustering model by 
setting different random seeds (`seed`) or adjusting the number of clusters 
(`clust_num`). These adjustments allow you to explore various configurations 
and refine the model's performance.

Here's how to set up the parameters for the clustering models in `run_setup.R`:

```r
gmm_mclust_model_params = tribble(
  ~model_name, ~lib_name, ~start_dt,    ~seed, ~clust_num,
  "GMM",       "Mclust",  "2002-01-01", 456,   4
)
```   

**3.Investment Strategy Inputs**

You can customize the market securities and set financial instruments for your 
investment strategy. The default settings include `JT7203` and `SPX_ETF` within 
the `mkt_data_tbl` dataset 

As outlined in the **Calculate Price Return Index** section of the 
`clustering_invest_strategy_generation.Rmd` file, if you require a Total Return 
Index (TRI) for indices like NKY (Nikkei 225) and TPX (TOPIX), you will need to 
manually source dividend information from reliable data providers.

However, in the standard setup, the project will utilize the `mkt_data_tbl` 
dataset generated by the `create_market_data_for_clustering2` function.

For backtesting a portfolio based on clustering model results, set up financial 
instruments using the configuration in `run_setup.R` as follows:


```r
my_clust_elem  = "GMM_PROD_EXPER2"
my_clust_elem2 = "SIMP_PROD_EXPER2"
my_universe_set_name = "SIMP_PROD_UNIVERSE_TBL2"

config_manager <- ClustInvestConfigManager$new()
config_manager$add_universe(my_universe_set_name, "AAPL_LONG_ONLY", 
                            c("AAPL_TRI_LONG", "ZERO"), c("AAPL"))
config_manager$add_universe(my_universe_set_name, "AAPL", 
                            c("AAPL_TRI_LONG", "AAPL_SHORT_LONG","ZERO"), c("AAPL"))                            
``` 

Finally, I have consolidated the R code from the above flow into 
`run_setup_for_user.R`. To apply customized settings, modify the content in 
`run_setup_for_user.R` and then copy it into `run_setup.R`.

# Requirements

-   This project requires Python installation and an active internet connection.

## R

-   **R version**: 4.0 or higher
-   **R packages**:
    -   `tidyverse`
    -   `mclust`
    -   `conflicted`
    -   `here`
    -   `R6`
    -   `zoo`
    -   `rlang`
    -   `reticulate`
    -   `parallel`
    -   `rmarkdown`
    -   `fst`
    -   `PerformanceAnalytics`
    -   `RcppArmadillo`
    -   `assertr`
    -   `lubridate`

To install all required R packages, run the following command in your R environment:

``` r
install.packages(c("tidyverse", "mclust", "conflicted", "here", "R6", "zoo", "rlang", "reticulate", "parallel", "rmarkdown", "fst", "PerformanceAnalytics", "RcppArmadillo", "assertr", "lubridate"))
```

In addition, you will need to install the custom `RcppPerfCalc` package directly from the GitHub release. 

Before installing the custom `RcppPerfCalc` package, please make sure that `Rtools` is installed on your system. `Rtools` is required to compile and build the package from the source.

This package is specifically designed to optimize the execution time when evaluating historical performance in past market regimes, which serves as in-sample data for generating Clustering Investment Strategy results. Utilizing this package will significantly enhance the speed and efficiency of the calculations involved.

To install the `RcppPerfCalc` package, run the following command:

``` r
install.packages("https://github.com/ri-o-ri01/RcppPerfCalc/releases/download/v0.1.0/RcppPerfCalc_0.1.0.tar.gz", 
                 repos = NULL, type = "source")
```



## Python

-   Python Version: 3.8 or higher

-   The `create_price_tbl_from_python_yf` function automatically creates a Python virtual environment and installs the required `yfinance` package for retrieving financial data. Therefore, ensure that Python is installed and properly configured on your system before running the R script.

# Performance and Execution Time

-   This project was executed on the following machine:

    -   Processor: 13th Gen Intel(R) Core(TM) i9-13900KF \@ 3.00 GHz

    -   Memory: 64 GB RAM

    -   Operating System: Windows 11 (64-bit)

-   The computations were performed with `no_cores` = 4, utilizing parallel processing to speed up the execution. Below are the execution times for each major part of the analysis:

    -   clustering_model_generation.Rmd: \~45 minutes for a trial

    -   clustering_invest_strategy_generation.Rmd: \~20 minutes for a trial

# License

This project is licensed under the MIT License. See the LICENSE file for details.

# Others

In addition to this project, I am applying an EDA (Exploratory Data Analysis) approach and developing a trading strategy based on tick data for NKY (Nikkei 225) and TPX (TOPIX). I acquired the data set from the JPX (Japan Exchange Group), and due to compliance and licensing restrictions, I cannot publicly share the data or the source code used for the analysis.

This tick data strategy is expected to enhance the performance of the financial instruments utilized in this project.

If you are interested in learning more about the results or the details of this project, please feel free to [contact me](mailto:ryo.ishi2012@gmail.com)
