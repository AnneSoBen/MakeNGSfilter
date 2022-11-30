# MakeNGSfilter

## About

This is a Shiny application that makes easier the production of ngsfilter files used by the [OBITools](https://pythonhosted.org/OBITools/welcome.html) (Boyer et al. 2016) to demultiplex sequences from metabarcoding experiments and the [metabaR](https://metabarfactory.github.io/metabaR/index.html) package (Zinger et al. 2021).

## Getting started

### Installation

MakeNGSfilter can be installed as follows:

```
install.packages("remotes") # if not already installed
remotes::install_github("AnneSoBen/MakeNGSfilter")
```

### Dependencies

The `shiny`,
`shinydashboard`,
`readxl`,
`stats`,
`stringr` and 
`utils` R packages.

## Usage

Type the following command to open the Shiny app:

```
MakeNGSfilter::MakeNGSfilter()
```

Three files are necessary:

- a 'comments' file: a tab-separated table that lists all samples and controls and their characteristics;

- a 'PCR plates design' file: a tab-separated file that represents the PCR plates design;

- a 'tags plates design' file: a .xlsx files that represents the distribution of sample tags in the PCR plates context.
