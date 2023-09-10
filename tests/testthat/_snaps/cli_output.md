# success [plain]

    Code
      cli::cli_alert_success("wow")
    Message
      v wow

---

    Code
      cli_check_success()
    Message
      v all data checks passing

# success [ansi]

    Code
      cli::cli_alert_success("wow")
    Message
      [32mv[39m wow

---

    Code
      cli_check_success()
    Message
      [90m[90m[32mv[90m all data checks passing[90m[39m

# success [unicode]

    Code
      cli::cli_alert_success("wow")
    Message
      ✔ wow

---

    Code
      cli_check_success()
    Message
      ✔ all data checks passing

# success [fancy]

    Code
      cli::cli_alert_success("wow")
    Message
      [32m✔[39m wow

---

    Code
      cli_check_success()
    Message
      [90m[90m[32m✔[90m all data checks passing[90m[39m

