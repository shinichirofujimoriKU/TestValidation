setup_python <- function() {
  library(reticulate)
  
  if (nzchar(Sys.getenv("RETICULATE_PYTHON"))) {
    use_python(Sys.getenv("RETICULATE_PYTHON"), required = TRUE)
  } else if ("CONDA_PREFIX" %in% names(Sys.getenv())) {
    use_condaenv(basename(Sys.getenv("CONDA_PREFIX")), required = TRUE)
  } else if (nzchar(Sys.which("python"))) {
    use_python(Sys.which("python"), required = TRUE)
  } else {
    stop("No Python environment detected")
  }
  
  invisible(py_config())
}
