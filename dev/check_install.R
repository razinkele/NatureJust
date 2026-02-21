ulib <- "C:/Users/arturas.baziukas/AppData/Local/R/win-library/4.4"
.libPaths(c(ulib, .libPaths()))
cat("Library paths:\n")
cat(paste(.libPaths(), collapse = "\n"), "\n\n")

needed <- c("shiny","golem","bslib","bsicons","config","leaflet","sf",
            "plotly","ggplot2","dplyr","tidyr","purrr","DT","shinyWidgets",
            "rnaturalearth","rnaturalearthdata","rmarkdown","knitr","htmltools")
installed <- installed.packages()[,"Package"]
missing <- setdiff(needed, installed)
cat("Missing packages:", paste(missing, collapse = ", "), "\n")

if (length(missing) > 0) {
  cat("Installing missing packages...\n")
  install.packages(missing, lib = ulib, repos = "https://cloud.r-project.org", quiet = TRUE)
  cat("Installation done.\n")
} else {
  cat("All packages installed.\n")
}
