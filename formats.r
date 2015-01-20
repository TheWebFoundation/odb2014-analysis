## Analyse the frequency of different formats

## Run format_prep.py first

formats<-read.csv("output/formats.csv")

## Select and formats with more than 2 instances
common_formats<-formats[is.element(formats$Format,c("csv","xml","xls","pdf","json","shp","txt","pc-axis","dbf","px","tsv","gtfs","wms","sdmx","kml","ods","html","doc","dbe","api","mdb","gml","spss","rdf","wfs","rss","navidata","kmz","dwg","atom")),]

common_formats$Format <- factor(common_formats$Format)

table(common_formats$Question,common_formats$Format)

