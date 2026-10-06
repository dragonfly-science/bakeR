# Credentials must not reach console output or job logs. Every request this
# package makes is a curl command carrying `Authorization: Bearer <token>`, and
# job records carry download links with `?jwt=<token>`. Two routes leaked them:
# printing the command, and R's own warning when a shell command fails, which
# quotes the command in full. Anything printed goes through .redact(), and every
# command runs through .system(), which replaces that warning with a redacted one.

# Mask bearer tokens and jwt= query parameters in a character vector.
.redact <- function(x) {
  x <- gsub("(Bearer\\s+)[^\"'[:space:]]+", "\\1***", x, perl = TRUE)
  gsub("([?&]jwt=)[^&\"'[:space:]]+", "\\1***", x, perl = TRUE, ignore.case = TRUE)
}

# system() without the credential-bearing warning: a non-zero exit status is
# reported with the command redacted. Returns what system() returns.
.system <- function(command, intern = TRUE) {
  out <- suppressWarnings(system(command, intern = intern))
  status <- if (intern) attr(out, "status") else out
  if (!is.null(status) && !identical(as.integer(status), 0L))
    warning(sprintf("command exited with status %s: %s", status, .redact(command)),
            call. = FALSE)
  out
}
