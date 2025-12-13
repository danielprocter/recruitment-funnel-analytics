
# Render report
rmarkdown::render(
  input = "R/report.Rmd",
  output_file = "report.html",
  output_dir  = "report"
)