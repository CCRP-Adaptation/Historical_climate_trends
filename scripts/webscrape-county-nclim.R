########### Web scraping ################
# Tutorial: # https://medium.com/geekculture/reading-xml-files-in-r-3122c3a2a8d9

rm(list=ls())
# Load packages
library(rvest)
library(tidyverse)
library(XML)
library(xml2)
library(curl)

url <- "https://www.ncdc.noaa.gov/cag/county/time-series/CO-069/tavg/all/12/1895-2021"
OutDir <- "location save data"

website <- "https://www.ncdc.noaa.gov/cag/county/time-series/CO-069-tavg-all-12-1895-2020.xml"
xml <- read_xml(xml)
xml_parse <- xmlParse(x)
xlm_df <- xmlToDataFrame(nodes=getNodeSet(x_xml, "//data"))

# format for trend analysis

