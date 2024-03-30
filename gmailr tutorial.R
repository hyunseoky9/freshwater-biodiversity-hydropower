library(gmailr)

gm_auth_configure(
  key = "",
  secret = "",
  path = 'C:/Users/hy324/Documents/gmailr.json',
  appname = "gmailr",
  app = httr::oauth_app(appname, key, secret, ...)
)
use_secret_file("C:/Users/hy324/Documents/gmailr.json")
test_email <-
  gm_mime() %>%
  gm_to("hyoon15@vols.utk.edu") %>%
  gm_from("hyoon15@vols.utk.edu") %>%
  gm_subject("this is just a gmailr test") %>%
  gm_text_body("Can you hear me now?")

# Verify it looks correct
gm_create_draft(test_email)

# If all is good with your draft, then you can send it
gm_send_message(test_email)
