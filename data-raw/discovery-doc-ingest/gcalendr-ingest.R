library(tidyverse)

## necessary only during gargle development to get devtools' shim for
## system.file()
devtools::load_all()

source(
  system.file("discovery-doc-ingest", "ingest-functions.R", package = "gcalendr")
)

x <- download_discovery_document("calendar:v3")
dd <- read_discovery_document(x)

methods <- get_raw_methods(dd)

methods <- methods %>% map(groom_properties,  dd)
methods <- methods %>% map(add_schema_params, dd)
methods <- methods %>% map(add_global_params, dd)


.endpoints <- methods
attr(.endpoints, "base_url") <- dd$rootUrl
## View(.endpoints)

# usually you would execute this from *within* the target package,
# but I cannot do so in this example
# please excuse the shenanigans to temporarily target the googledrive project
if (basename(getwd()) == "gcalendr") {
  usethis::use_data(.endpoints, internal = TRUE, overwrite = TRUE)
} else {
  warning("Execute this code only from inside the `gcalendr` project")
}
