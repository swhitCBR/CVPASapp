install.packages("qrcode")
library(qrcode)

# Replace with your deployed Shiny app URL

# OLD VERSION (September Talk)
# shiny_app_url <- "https://swhit-cbr-cvpassapp.share.connect.posit.cloud/"

# OLD VERSION (October 2026, SacPASS talk)
shiny_app_url <- "https://swhit-cbr-sthdcvpas09.share.connect.posit.cloud/"

# Generate and save
# code <- qr_code(shiny_app_url)
# generate_svg(code, filename = "inst/app/www/shiny_app_qrcode.svg")

# Survey Monkey link for Octoaber 30, 2026  presentation
survey_monkey_url <- "https://www.surveymonkey.com/r/DT96KZ9"
# Generate and save
sm_code <- qr_code(survey_monkey_url)
generate_svg(sm_code, filename = "inst/app/www/survey_monkey_qrcode.svg")

