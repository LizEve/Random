Set up R on Linux

```bash
R --version
R
sudo apt-get install libcurl4-openssl-dev libssl-dev
sudo apt install gfortran
sudo apt install libudunits2-dev
sudo apt-get install libxml2
sudo apt-get install libxml2-dev # gave an error but devtools seems to be working
sudo apt-get install liblapack-dev
sudo apt-get install libopenblas-base # More errors here about not seeing files. 
sudo apt-get install libopenblas-dev
sudo apt install libgsl-dev
sudo apt-get install libgdal-dev
```

/etc/ld.so.conf.d/nvidia_settings.conf -> /etc/alternatives/nvidia_settings_conf
/etc/alternatives/nvidia_settings_conf -> /usr/lib/nvidia-settings-319-updates/ld.so.conf

/usr/lib/nvidia-settings-319-updates/ld.so.conf

```R
install.packages('devtools', dependencies = TRUE)
require('devtools')
install_version('igraph', '1.2.2')
install.packages('devtools', dependencies = TRUE)
devtools::install_github("bcm-uga/LEA")
install.packages("gfortran") # not avaliable for R version 3.5.1
install.packages("strataG") # came up with a bunch of errors. moving on. 
install.packages(c("Cairo","devtools","ggplot2","gridExtra","gtable","tidyr"),dependencies=T)  # more errors, but fuck it
devtools::install_github('royfrancis/pophelper') #
```


bitops
jsonlite
xml2
Rcpp
digest
fs
R6
memoise

maps
colorspace
gtools
munsell
units
gsl
pcaPP
scales

mime
vdiffr
shiny
rgeos
sf