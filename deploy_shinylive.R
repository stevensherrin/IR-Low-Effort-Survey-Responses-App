library(shinylive)
library(fs)
#sessionInfo()

# Define the path to your existing Shiny app
existing_app_path <- "C:/Users/sherrins/OneDrive - Wentworth Institute of Technology/Documents/GitHub/IR-Low-Effort-Survey-Responses-App"

shinylive::export(appdir = existing_app_path, destdir = "C:/Users/sherrins/OneDrive - Wentworth Institute of Technology/Documents/GitHub/IR-Low-Effort-Survey-Responses-App/docs")

#httpuv::runStaticServer("docs")

httpuv::runStaticServer("C:/Users/sherrins/OneDrive - Wentworth Institute of Technology/Documents/GitHub/IR-Low-Effort-Survey-Responses-App/docs")

#httpuv::runStaticServer("docs/", port=8008)


