library(googleComputeEngineR)
vm <- gce_vm(template = "rstudio",
             name = "my-rstudio",
             username = "vmarquar", password = "%logME1n$",
             predefined_type = "n1-highmem-2") # CHANGE TO FREE TIER!
