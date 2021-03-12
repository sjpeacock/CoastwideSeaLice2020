# CoastwideSeaLice2020
Analysis of sea-louse abundance on juvenile salmon in the Discovery Island, Broughton Archipelago, and Clayoquot Sound in spring 2020.

## Contact
This code was written by Stephanie Peacock <stephanie.j.peacock@gmail.com> for Alex Morton, Raincoast Research Society.

## Files and folders

### Data
The folder `data` contains the raw data files for sampling in the Broughton Archipelago ((Salmon Coast Field Station)[www.salmoncoast.org]), Discovery Islands/Nootka/Port Hardy (Raincoast Research Society), and in Clayoquot ((Cedar Coast Field Station)[www.cedarcoastfieldstation.org/].

The file `dataCompilation.R` combines these three sources into a single files `coastwideSeaLice.csv` used in the analyses.

### Results
The file `summaryStats.R` goes through some bootstrapping of the means and 95% CI for the abundance and prevalence of sea lice on juvenile salmon, parsing the data by louse stage, louse species, etc. There is also code to produce the regional map, using the package `PBSmapping`.

`DataSummary.csv` is R output of the average louse abundances per host species and region.

