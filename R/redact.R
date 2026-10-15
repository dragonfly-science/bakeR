# Credentials must not reach console output or logs. Requests carry
# `Authorization: Bearer <token>`, and legacy job records carry download links
# with `?jwt=<token>`; anything that could echo a request, such as a connection
# error quoting its URL, goes through .redact() first.

# Mask bearer tokens and jwt= query parameters in a character vector.
.redact <- function(x) {
  x <- gsub("(Bearer\\s+)[^\"'[:space:]]+", "\\1***", x, perl = TRUE)
  gsub("([?&]jwt=)[^&\"'[:space:]]+", "\\1***", x, perl = TRUE, ignore.case = TRUE)
}

# Coerce a list to one jsonlite serialises as a JSON object ({}), even when
# empty: an empty unnamed list would become [] and be refused by the server.
.as_json_object <- function(x) {
  x <- as.list(x)
  if (length(x) == 0) x <- structure(list(), names = character(0))
  x
}
