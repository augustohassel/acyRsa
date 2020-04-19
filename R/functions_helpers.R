# If date is invalid, returns FALSE
.validate_date <- function(date) {
  tryCatch({!is.na(format.Date(x = date, "%Y-%m-%d"))}, error = function(cnd) {FALSE})
}
