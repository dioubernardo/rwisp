# Implementation in R of the WISP Multiple Criteria Sorting Method

[![R-CMD-check](https://github.com/dioubernardo/rwisp/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/dioubernardo/rwisp/actions/workflows/R-CMD-check.yaml)
[![CRAN release](https://www.r-pkg.org/badges/version/rwisp)](https://cran.r-project.org/package=rwisp)

This project consists of the implementation of the method in R, its tests, the CRAN package and a web application written in R Shiny.

If you just want to run the method by reading the data contained in a CSV file, use the application hosted at shinyapps.io.
[Open rwisp on shinyapps.io](https://bernardosilva.shinyapps.io/rwisp/)

## Reference

D. Stanujkic, G. Popovic, D. Karabasevic, I. Meidute-Kavaliauskiene and A. Ulutaş, "An Integrated Simple Weighted Sum Product Method—WISP," in IEEE Transactions on Engineering Management, vol. 70, no. 5, pp. 1933-1944, May 2023, doi: [10.1109/TEM.2021.3075783](https://doi.org/10.1109/TEM.2021.3075783).

Abstract: Until now, many different multiple criteria decision-making methods have been proposed for the facilitation of different business problems such as entrepreneurial. In this article, a new approach for selecting the most acceptable alternative is considered, which is based on the integration of weighted sum and weighted product approaches. The basic idea of the newly proposed approach is partly based on the multiobjective optimization by ratio analysis plus the full multiplicative form method, but also includes some features of the weighted aggregated sum product assessment and combined compromise solution methods. The proposed method is tested by using two illustrative examples that are related to the contractor and industrial robot selection. Additionally, to test its reliability, the obtained results are compared with the ones gained by using the multiple-criteria decision-making methods of the first and second generations. The proposed method predicts the application of a much simpler normalization procedure, involves four utility measures for defining the overall utility of alternatives, and enables a much easier ranking of them. The final conclusion is that the simple weighted sum product method is easy to use and contributes to the increasing of the reliability of decisions that are made. Therefore, the proposal uses a much simpler normalization procedure, i.e., four utility measures for determining the overall utility of alternatives, and allows a much easier ranking of alternatives.

## For Use

### Option 1 - Without installing by entering the data
- git clone https://github.com/dioubernardo/rwisp/
- edit test.r 
- add your data and run

### Option 2 - Without installing reading data from a CSV
- git clone https://github.com/dioubernardo/rwisp/
- edit test-csv.r
- change the CSV file path to your data or edit test.csv and include your data

### Option 3 - Install from github
```
library("devtools");
install_github("dioubernardo/rwisp");
library("rwisp")
...
```

### Option 3 - Install from CRAN
```
install.packages("rwisp")
library("rwisp")
...
```

