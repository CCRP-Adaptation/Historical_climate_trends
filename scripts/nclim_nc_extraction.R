library(stars)
library(dplyr)
library('maptools')
library(ggplot2) #for plotting
library(units) # for dropping units
library(tidyverse)
library(starsdata)
library(viridis)
library(ncdf4)


rm(list=ls())

DataDir <- "C:/Users/achildress/Documents/NOAA-data/nclim_2202/"

name <- "nclimgrid_prcp.nc"

x<-nc_open(paste0(DataDir,name))

x<-read_ncdf(paste0(DataDir,name), var= "prcp")
  