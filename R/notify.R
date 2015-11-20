#' notify command by URL(http://uribo.hatenablog.com/entry/2015/09/22/120815)
#' 
#' @param Undescribe
#' 
#' @return Undescribe
#' @export
notify <- function(msg="Operation complete") {
  
  in.osx <- (Sys.info()['sysname'] == "Darwin")
  in.rstudio <- (Sys.getenv("RSTUDIO") == "1")
  in.rgui <- (Sys.getenv("R_GUI_APP_REVISION") != "")
  dir.notifier <- system("which terminal-notifier", intern = TRUE)
  
  if (in.rstudio) { # hack to see if running in RStudio
    title <- "RStudio"
    sender <- activate <- "org.rstudio.RStudio"
  }
  
  if (in.rgui) { # running in R GUI app?
    title <- "R GUI"
    sender <- activate <- "org.R-project.R"
  }
  
  # if running in RStudio or R GUI app use NotificationCenter otherwise use message()
  if ((in.rstudio | in.rgui) & in.osx) {
    system(sprintf(paste(dir.notifier, "-title '%s' -message '%s' -sender %s -activate %s", sep = " "),
                   title, msg, sender, activate ),
           ignore.stdout=TRUE, ignore.stderr=TRUE, wait=FALSE)
  } else {
    message(msg)      
  }
  
}

# try it!
# library(devtools) # install.packages("devtools")
# source_gist("9c419af5547fde20d2a7")
# system("sleep 10")
# notify("Long op complete")