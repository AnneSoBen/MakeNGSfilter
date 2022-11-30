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

`shiny`
`shinydashboard`
`readxl`
`stats`
`stringr`
`utils`

## Usage

Type the following command to open the Shiny app:

```
MakeNGSfilter::MakeNGSfilter()
```

