library(ImmuneSpaceR)
#library(ggplot2)

study <- CreateConnection("SDY112")

hai <- study$getDataset("hai")
study$plot("hai")