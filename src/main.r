sourceDir <- function(path, pattern = "\\.[rR]$", env = NULL, chdir = TRUE) {
  files <- sort(dir(path, pattern, full.names = TRUE))
  lapply(files, source, chdir = chdir)
}
