<!-- README.md is generated from README.Rmd. Please edit that file -->
<!-- rmarkdown::render(input="README.Rmd", output_file = "README.md", output_format = "md_document") -->

# `{ohwm}`

An R Shiny app for remotely identifying stream Ordinary High Water Marks
(OHWM) using publicly available terrain data for small sites.

## Package Status

<!-- badges: start -->

[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
[![packageversion](https://img.shields.io/badge/Package%20version-2025.04.12-orange.svg?style=flat-square)](commits/main)
[![Last-changedate](https://img.shields.io/badge/last%20change-2025--04--12-yellowgreen.svg)](/commits/main)
[![Licence](https://img.shields.io/badge/licence-CC0-blue.svg)](http://choosealicense.com/licenses/cc0-1.0/)
<!-- badges: end -->

## Description

This R Shiny app can be used to remotely identify stream Ordinary High
Water Marks (OHWM) using publicly available terrain data for small
sites. It has the following features:

-   Identify a project site
-   Extract the latest available lidar-derived Digital Elevation Model
    (DEM)
-   Draw stream cross sections
-   Tools for choosing a bankfull elevation for ungaged streams
-   Calculate stream channel dimensions

## Funding

<img src="man/figures/chl.png" width=125 align="right" />

Funding for development and maintenance of FluvialGeomorph has been
provided by the following US Army Corps of Engineers (USACE) programs:

-   [Flood and Coastal Risk
    Management](https://www.erdc.usace.army.mil/Locations/CHL/Flood-Coastal-Risk-Management/)
-   [Ecosystem Management and Restoration Research Program
    (EMRRP)](https://emrrp.el.erdc.dren.mil)
-   [Regional Sediment Management Program
    (RSM)](https://rsm.usace.army.mil/)
-   [Mississippi River Geomorphology and Potamology Program
    (MRG&P)](https://www.mvd.usace.army.mil/Missions/Mississippi-River-Science-Technology/MS-River-Geomorphology-Potamology/)
-   [Flood Risk Management Program
    (FRM)](https://www.iwr.usace.army.mil/Missions/Flood-Risk-Management/Flood-Risk-Management-Program/)
-   [Engineering With Nature (EWN)](https://ewn.el.erdc.dren.mil/)

<p float="left">
<img src="man/figures/chl.png" height=75 />
<img src="man/figures/EMRRP_logo_200.png" height=75 />
<img src="man/figures/RSM_200.png" height=75 />
<img src="man/figures/MRG&P_300.png" height=75 />
<img src="man/figures/FRMP_200.png" height=75 />
<img src="man/figures/SilverJackets_200.png" height=75 />
<img src="man/figures/EWN_200.png" height=75 />
</p>

## Latest Updates

Check out the [NEWS](NEWS.md) for details on the latest updates.

## Authors

-   Christopher Haring, Fluvial Geomorphologist/Research Physical
    Scientist, U.S. Army Corps of Engineers
    <a itemprop="sameAs" content="https://orcid.org/0009-0004-3834-9811" href="https://orcid.org/0009-0004-3834-9811" target="orcid.widget" rel="me noopener noreferrer" style="vertical-align:top;"><img src="https://orcid.org/sites/default/files/images/orcid_16x16.png" alt="ORCID iD icon" style="width:1em;margin-right:.5em;"/>https://orcid.org/0009-0004-3834-9811</a>
-   Michael Dougherty, Geographer, U.S. Army Corps of Engineers
    <a itemprop="sameAs" content="https://orcid.org/0000-0002-1465-5927" href="https://orcid.org/0000-0002-1465-5927" target="orcid.widget" rel="me noopener noreferrer" style="vertical-align:top;"><img src="https://orcid.org/sites/default/files/images/orcid_16x16.png" style="width:1em;margin-right:.5em;" alt="ORCID iD icon">https://orcid.org/0000-0002-1465-5927</a>
-   Thomas Darby, Geographer, U.S. Army Corps of Engineers

## Bug Reports

If you find any bugs using `ohwm`, please open an
[issue](https://github.com/FluvialGeomorph/ohwm/issues).
