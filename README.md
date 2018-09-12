# slhwsurvey

Coding by [Mario Jendrossek](https://github.com/MarioJendrossek) and [Sam Clifford](https://github.com/samclifford) for analysis of a survey of health care workers in Sierra Leone. Work being conducted with Dr Rosalind Eggo.

If you need a copy of the data, ask [Mario Jendrossek](mailto:mario.jendrossek@lshtm.ac.uk)

The `hcw.main.R` script will load the data, complete the analysis and put the results in a folder called `Figures`.

Currently, only the following scripts are used by the main script:

* `hcw.makedata.R` - READ IN AND PROCESS DATA FOR ANALYSIS
* `hcw.minimal.turnover.R` - MODELLING DURATION OF EMPLOYMENT
* `hcw.vaccination.R` - SINGLE EXPLANATORY VARIABLES
* `hcw.sentiment.modelling.R` - MULTIPLE EXPLANATORY VARIABLES
* `hcw.coverage.R` - COVERAGE SIMULATION 