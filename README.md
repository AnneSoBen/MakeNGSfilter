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

### Mandatory files

Three files are necessary to produce a ngsfilter file:

- a 'samples and controls description' file: a tab-separated table that lists all samples and controls and their caracteristics. Accepted file extensions: .txt, .tsv, .tab.
- a 'PCR plates design' file: a tab-separated file that represents the PCR plates design. Accepted file extensions: .txt, .tsv, .tab.
- a 'tags plates design' file: a .xlsx file that represents the distribution of sample tags in the PCR plates context.

### Files structure

Each file must meet specific requirements to create correct ngsfilter files.

#### Samples and controls description

This file lists the IDs of each individual PCR replicate, including controls, included in the experiment, as well as basic characteristics. Three columns are mandatory:

- `id`: column with sample or control IDs. These IDs **MUST BE UNIQUE** and **MUST NOT contain special characters, accents or spaces**.
- `type`: 'sample' for PCR replicates corresponding to samples and 'control' for experimental controls.
- `control_type`: it can take 4 different values for controls ('extraction' for blank extraction controls, 'pcr' for blank PCR controls, 'sequencing' for blank sequencing controls, 'positive' for positive controls (e.g. mock communities)). For more information visit this page). This column must not be completed for samples.

Other columns may be added. Their name and content **MUST NOT contain special characters, accents or spaces**.

#### PCR plates design

This file corresponds to the PCR plates design, showing the sample identity amplified in each well of PCR plates. The current internal design allows for the treatment of up to 12 plates per design. If there exist tag combinations of used forward and reverse tags that were not attributed to samples or controls, unused wells must be indicated by a specific keyword of your choice and will be annotated as sequencing controls in the ngsfilter file (`control_type` attribute). This obligation may disappear in future versions of the app. The keyword **MUST NOT contain special characters, accents or spaces**.

#### Tags plates design

This file indicates the tag combination used in each well of each PCR plates.If there exist tag combinations of used forward and reverse tags that were not attributed to samples or controls, these combinations must be included in the file. This obligation may disappear in future versions of the app.

