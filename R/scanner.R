#' Scan METACRAN for Filipino Packages
#' @export
scan_metacran <- function() {
  message("Looking through METACRAN data logs...")
  
  # CHANGED: Switched endpoint to a secondary mirror service
  req <- httr2::request("https://r-pkg.org")
  resp <- httr2::req_perform(req)
  cran_data <- jsonlite::fromJSON(httr2::resp_body_string(resp))
  
  # Search terms for local footprint
  ph_terms <- "philippines|manila|psgc|dost|psa|pnp|pagasa|marina|up diliman"
  ph_emails <- "\\.ph\\b"
  
  # Whitelist of known PH developer usernames or names
  whitelist <- "njtalingting|talingting" 
  
  detected_pkgs <- lapply(cran_data, function(pkg) {
    search_block <- paste(
      pkg$Package, pkg$Title, pkg$Description, 
      pkg$Maintainer, pkg$Author, 
      collapse = " "
    )
    
    # Run the scanner
    if (grepl(ph_terms, search_block, ignore.case = TRUE) || 
        grepl(ph_emails, search_block, ignore.case = TRUE) ||
        grepl(whitelist, search_block, ignore.case = TRUE)) {
      return(data.frame(
        Package = pkg$Package, 
        Maintainer = pkg$Maintainer, 
        Version = pkg$Version,
        stringsAsFactors = FALSE
      ))
    }
    return(NULL)
  })
  
  result <- do.call(rbind, detected_pkgs)
  rownames(result) <- NULL
  return(result)
}
