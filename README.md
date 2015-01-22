# takiwaR: Methods for "State of the Takiwa" Monitoring

takiwaR is designed to support community-led ecological surveys (citizen science) as part of a "State of the Takiwa" monitoring programme. It provides functions to calculate semi-quantitative environmental assessment metrics, collectively known as Cultural Health Indices (CHI). 

The initial focus of the package is on the Marine Cultural Health Index (MCHI), a survey tool that targets near-shore fish species and the habitats that support them. 

Currently the package is under heavy development, but you can install:

-   the latest released version from CRAN with

    ``` r
    install.packages("takiwaR")
    ```

-   or the latest development version from github with

    ``` r
    if (packageVersion("devtools") < 1.6) {
      install.packages("devtools")
    }
    devtools::install_github("dpritchard/takiwaR")
    ```

This package was developed specifically to analyse State of the Takiwā data collected as part of the Te Rūnanga o Ngāi Tahu monitoring programme, in Aotearoa (New Zealand). For more information about this project, please visit the [Takiwa 3.0 Project][takiwa3] website.

[takiwa3]: http://www.takiwa.org.nz