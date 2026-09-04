install.packages("qrcode")
library(qrcode)

# Replace with your deployed Shiny app URL
shiny_app_url <- "https://swhit-cbr-cvpassapp.share.connect.posit.cloud/"

# Generate and save
code <- qr_code(shiny_app_url)
generate_svg(code, filename = "shiny_app_qrcode.svg")
