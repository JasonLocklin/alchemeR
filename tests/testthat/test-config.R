test_that("alchemer_creds errors with an actionable message when unset", {
  withr::local_envvar(ALCHEMER_API_TOKEN = "", ALCHEMER_API_SECRET = "")
  expect_error(alchemer_creds(), class = "alchemeR_config_error")
  expect_error(alchemer_creds(), "ALCHEMER_API_TOKEN")
})

test_that("alchemer_creds reads environment variables", {
  withr::local_envvar(ALCHEMER_API_TOKEN = "env-token", ALCHEMER_API_SECRET = "env-secret")
  creds <- alchemer_creds()
  expect_equal(creds$token, "env-token")
  expect_equal(creds$secret, "env-secret")
})

test_that("alchemer_domain defaults to the US domain, not the Canadian one", {
  withr::local_envvar(ALCHEMER_DOMAIN = NA)
  expect_equal(alchemer_domain(), "api.alchemer.com")
})

test_that("alchemer_domain is configurable via ALCHEMER_DOMAIN", {
  withr::local_envvar(ALCHEMER_DOMAIN = "api.alchemer.eu")
  expect_equal(alchemer_domain(), "api.alchemer.eu")
})

test_that("alchemer_db_path errors with an actionable message when unset", {
  withr::local_envvar(ALCHEMER_DB = "")
  expect_error(alchemer_db_path(), class = "alchemeR_config_error")
  expect_error(alchemer_db_path(), "ALCHEMER_DB")
})

test_that("alchemer_rpm and alchemer_full_sweep_days have documented defaults", {
  withr::local_envvar(ALCHEMER_RPM = NA, ALCHEMER_FULL_SWEEP_DAYS = NA)
  expect_equal(alchemer_rpm(), 100)
  expect_equal(alchemer_full_sweep_days(), 90)
})

test_that("an optional setting present but blank falls back to its default", {
  # Regression test (code review): Renviron.example ships every optional
  # setting as a `NAME=value` line, so blanking a value out (rather than
  # deleting the line) is the natural way to "unset" one -- but Sys.getenv()'s
  # `unset` default only applies when the variable is absent entirely. The
  # result was a base URL of "https:///v5" and req_throttle() aborting with
  # "capacity must be a whole number, not a numeric NA" -- neither of which
  # names the actual problem.
  withr::local_envvar(
    ALCHEMER_DOMAIN = "", ALCHEMER_RPM = "", ALCHEMER_FULL_SWEEP_DAYS = "",
    ALCHEMER_LANGUAGE = "", ALCHEMER_API_TOKEN = "T", ALCHEMER_API_SECRET = "S"
  )
  expect_equal(alchemer_domain(), "api.alchemer.com")
  expect_equal(alchemer_rpm(), 100)
  expect_equal(alchemer_full_sweep_days(), 90)
  expect_equal(alchemer_language(), "English")
  expect_equal(alchemer_client()$base_url, "https://api.alchemer.com/v5")
})

test_that("alchemer_client takes its whole configuration from the environment", {
  withr::local_envvar(
    ALCHEMER_API_TOKEN = "env-token", ALCHEMER_API_SECRET = "env-secret",
    ALCHEMER_DOMAIN = "api.alchemer.eu", ALCHEMER_RPM = "42"
  )
  client <- alchemer_client()
  expect_equal(client$token, "env-token")
  expect_equal(client$base_url, "https://api.alchemer.eu/v5")
  expect_equal(client$rpm, 42)
})

test_that("alchemer_language defaults to English and rejects SQL-breaking values", {
  # It reaches a generated SQL/JSONPath expression as a bare literal, so it is
  # validated where it is read rather than quoted at each use.
  withr::local_envvar(ALCHEMER_LANGUAGE = NA)
  expect_equal(alchemer_language(), "English")

  withr::local_envvar(ALCHEMER_LANGUAGE = "English\") --")
  expect_error(alchemer_language(), class = "alchemeR_config_error")
  withr::local_envvar(ALCHEMER_LANGUAGE = "en'glish")
  expect_error(alchemer_language(), class = "alchemeR_config_error")
})

test_that("alchemer_tz rejects a value that isn't an IANA name", {
  withr::local_envvar(ALCHEMER_TZ = "Eastern")
  expect_error(alchemer_tz(), class = "alchemeR_config_error")
})
