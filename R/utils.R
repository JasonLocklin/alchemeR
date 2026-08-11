# Alchemer's list-endpoint items are shallow JSON objects whose fields vary
# in shape (scalars, nested objects, arrays) between endpoints and even
# between rows of the *same* endpoint (ADR-003) -- e.g. `referer` is a
# string on one response and null (missing) on another, and `varname` is a
# string on one question and null on the next. This decides each column's
# shape once, across every item, rather than row-by-row (which is what
# dplyr::bind_rows() of per-row tibbles would do, and it errors the moment a
# field's inferred type disagrees between rows). Any field that is nested
# (list/array) on *any* row becomes a list-column for every row; everything
# else becomes an ordinary atomic column with NA for missing/null values.
items_to_tibble <- function(items) {
  if (length(items) == 0) {
    return(tibble::tibble())
  }
  field_names <- unique(unlist(lapply(items, names), use.names = FALSE))
  columns <- purrr::map(field_names, function(nm) {
    values <- purrr::map(items, function(item) item[[nm]])
    is_nested <- purrr::some(values, function(v) is.list(v) || length(v) > 1)
    if (is_nested) {
      values
    } else {
      unlist(purrr::map(values, function(v) if (is.null(v)) NA else v))
    }
  })
  names(columns) <- field_names
  tibble::as_tibble(columns)
}
