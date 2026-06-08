#' Scan CRAN for Filipino Packages
#' @export
scan_metacran <- function() {
  message("Accessing primary CRAN mirror records...")
  
  # Pull live data directly from the official CRAN master mirror matrix
  db_matrix <- available.packages(repos = "https://cloud.r-project.org")
  cran_data <- as.data.frame(db_matrix, stringsAsFactors = FALSE)
  
  # Search terms for local footprint
  ph_terms <- "philippines|manila|psgc|dost|psa|pnp|pagasa|marina|up diliman"
  ph_emails <- "\\.ph\\b"
  
  # Whitelist of known PH developer usernames or names
  whitelist <- "njtalingting|talingting" 
  
  # Use R's fast vectorized logical indices to search rows
  detected_rows <- grep(ph_terms, cran_data$Package, ignore.case = TRUE) |
                   grep(ph_terms, cran_data$Title, ignore.case = TRUE) |
                   grep(ph_terms, cran_data$Description, ignore.case = TRUE) |
                   grep(ph_emails, cran_data$Maintainer, ignore.case = TRUE) |
                   grep(whitelist, cran_data$Maintainer, ignore.case = TRUE)
  
  # Extract clean matching subsets
  result <- cran_data[detected_rows, c("Package", "Maintainer", "Version")]
  rownames(result) <- NULL
  
  return(result)
}
