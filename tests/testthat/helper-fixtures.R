# Loads a fixture JSON file (built from docs/alchemer-api-reference.md
# examples) as a plain R list, and wraps it in an httr2 mock response with
# the right content type. Shared by every test file that exercises a
# direct-API function against a canned payload instead of the network.
fixture_path <- function(name) {
  system.file("extdata", "fixtures", name, package = "alchemeR", mustWork = TRUE)
}

fixture_response <- function(name, status_code = 200) {
  # Serve the fixture file's exact bytes. Round-tripping the parsed R list
  # back through jsonlite::toJSON() (as httr2::response_json() would) turns
  # JSON `null` into `{}`, which is not what the fixtures are testing for.
  raw <- readBin(fixture_path(name), "raw", file.size(fixture_path(name)))
  httr2::response(
    status_code = status_code,
    headers = list(`Content-Type` = "application/json"),
    body = raw
  )
}

test_client <- function() {
  alchemer_client(token = "TOKEN", secret = "SECRET", domain = "api.alchemer.com", rpm = 100)
}
