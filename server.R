#!/usr/bin/Rscript
#!/usr/bin/env Rscript

library(ggplot2)
library(plumber)
library(future)
library(svglite)

plan(multisession)

setwd(getwd())

root <- Plumber$new()

##Production endpoints
backend <- Plumber$new("backend.R")

# curl http://localhost:8090/api/v1/rya/not_async_sqrt?n=25 & curl http://localhost:8090/api/v1/rya/sqrt?n=64
# curl http://localhost:8090/api/v1/rya/async_sqrt?n=121 & curl http://localhost:8090/api/v1/rya/sqrt?n=36

root$mount("/api/v1/rya", backend)

root$routes

print(paste("Updated Plumber Development Script started at: ", Sys.time()))
root$run(port = 3000, host = "0.0.0.0", swagger = FALSE)

