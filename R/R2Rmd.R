#' Undescribed
#' 
#' @param Undescribed
#' 
#' @return Undescribed
#' 
#' @export
R2Rmd <- function (ScriptFile, username = 'jundoll')
{
  if (file.exists(ScriptFile)) {
    RmdFile = paste0(ScriptFile, "md")
    Lines = readLines(ScriptFile)
    NoLines = length(Lines)
    GoodLines = Lines > ""
    GoodLines2 = c(FALSE, GoodLines[1:(NoLines - 1)])
    ShowLines = GoodLines | GoodLines2
    Lines[Lines == ""] = "```  \n\n```{r }  "
    Lines = Lines[ShowLines]
    cat("# ", "\n## by", username, "\n\n```{r }  ", 
        file = RmdFile)
    cat(paste0("\n", Lines, "  "), file = RmdFile, append = TRUE)
    cat("\n# end of input  \n```  \n\n", file = RmdFile, 
        append = TRUE)
    cat("File", RmdFile, "has been created in your working directory.\n")
  }
  else {
    warning("The specified file does not exist.\n")
  }
  return(invisible(NULL))
}