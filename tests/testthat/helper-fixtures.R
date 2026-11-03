# Synthetic definition and responses in the shapes seen on the real API
# (NOTES.md §5, §13; scratch/rig/fake_api.py). Parsed with
# simplifyVector = FALSE, like the package does.
opt <- \(id, title, value = title) {
  list(id = id, title = list(English = title), value = value)
}
item <- \(id, type, title, options = list(), base_type = "Question", ...) {
  list(
    id = id,
    base_type = base_type,
    type = type,
    title = list(English = title),
    options = options,
    ...
  )
}

definition <- function(sso = FALSE) {
  d <- list(
    id = 123,
    modified_on = "2026-10-01 10:00:00",
    languages = list("English"),
    pages = list(list(
      id = 1,
      questions = list(
        item(
          3,
          "RADIO",
          "<b>Q3</b> colour&nbsp;?",
          list(opt(10001, "Red", "1"), opt(10002, "Blue", "2"))
        ),
        item(
          4,
          "CHECKBOX",
          "Fruit",
          list(opt(10005, "Apple"), opt(10006, "Other"))
        ),
        item(5, "EMAIL", "hook", base_type = "Action"),
        item(6, "TEXTBOX", "Age", properties = list(force_numeric = TRUE)),
        item(7, "TEXTBOX", "Name", properties = list(force_numeric = FALSE)),
        item(
          8,
          "TABLE",
          "Grid",
          sub_questions = list(
            item(9, "RADIO", "Row 1", list(opt(10010, "Yes")))
          )
        ),
        item(
          11,
          "TABLE",
          "Grid2",
          sub_questions = list(
            item(12, "CHECKBOX", "Row A", list(opt(10020, "Opt X")))
          )
        ),
        item(13, "SIGNATURE", "Sign"),
        item(14, "HIDDEN", "h", base_type = "Action"),
        item(
          15,
          "RANK",
          "Rank",
          list(opt(10030, "First"), opt(10031, "Second"))
        ),
        item(16, "SLIDER", "Slide"),
        item(17, "INSTRUCTIONS", "Read this"),
        item(18, "MEDIA", "pic", base_type = "Decorative")
      )
    ))
  )
  if (sso) {
    d$sso <- list(sEntityID = "https://idp.example.org")
  }
  d
}

response <- function(id, survey_data, ...) {
  utils::modifyList(
    list(
      id = as.character(id),
      status = "Complete",
      is_test_data = "0",
      date_started = "2026-11-01 05:30:00 GMT",
      date_submitted = "2026-11-01 05:31:00 GMT",
      date_updated = "2026-11-01 05:31:05",
      response_time = 60,
      language = "English",
      link_id = "300865",
      url_variables = list(),
      data_quality = list(),
      survey_data = survey_data
    ),
    list(...)
  )
}

full_answers <- function(age = "34") {
  list(
    `3` = list(id = 3, type = "RADIO", answer = "1", answer_id = 10001),
    `4` = list(
      id = 4,
      type = "parent",
      options = list(
        `10005` = list(id = 10005, option = "Apple", answer = "Apple")
      )
    ),
    `6` = list(id = 6, type = "TEXTBOX", answer = age),
    `7` = list(id = 7, type = "TEXTBOX", answer = "Pat"),
    `8` = list(id = 8, type = "parent"),
    `9` = list(id = 9, type = "RADIO", parent = 8, answer = "Yes"),
    `11` = list(
      id = 11,
      type = "parent",
      subquestions = list(
        `12` = list(`10020` = list(id = 10020, parent = 12, answer = "Opt X"))
      )
    ),
    `13` = list(
      id = 13,
      type = "SIGNATURE",
      answer = list(
        name = "Pat",
        signature = paste0("data:image/png;base64,", strrep("A", 2000))
      )
    ),
    `14` = list(id = 14, type = "HIDDEN", answer = "abc"),
    `15` = list(
      id = 15,
      type = "RANK",
      answer = list(
        list(id = 10030, option = "First", rank = 2),
        list(id = 10031, option = "Second", rank = 1)
      )
    ),
    `16` = list(id = 16, type = "SLIDER", answer = "7")
  )
}

pages_of <- function(...) list(list(data = list(...)))
