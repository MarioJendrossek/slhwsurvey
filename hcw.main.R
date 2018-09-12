## THE FOLLOWING CODE IS FOR THE ANALYSIS CONTAINED IN...
## Health care worker vaccination against Ebola: vaccine acceptance and employment duration in Sierra Leone
## THE CODE HAS BEEN WRITTEN BY
## MARIO JENDROSSEK and SAM CLIFFORD under ROSALIND EGGO

library(conflicted)
library(tidyverse)
library(RColorBrewer)
library(boot)
library(magrittr)
library(broom)
library(Zelig)

## READ IN AND PROCESS DATA FOR ANALYSIS
source("hcw.makedata.R")

## MODELLING DURATION OF EMPLOYMENT
source("hcw.minimal.turnover.R")

## SINGLE EXPLANATORY VARIABLES
source("hcw.vaccination.R")

## MULTIPLE EXPLANATORY VARIABLES
source("hcw.sentiment.modelling.R")

## COVERAGE SIMULATION 
source("hcw.coverage.R")