#' Remove YAML preamble from '.Rmd' files
#'
#' The function \code{yaml_rm} removes the YAML preamble from a '.Rmd' file 
#'
#'
#'  Cria tabela de frequências de acordo com o tipo de variável testada
#'
#'  @param file An Rmarkdown (.Rmd) file
#'
#'  @return An Rmarkdown (.Rmd) file
#'
#'  @examples
#'  
#'  yaml_rm(file)
#'
#' @return An R Markdown output format object in a child file fashion.
#' @export
yaml_rm <- function(doc){
  # if("to chack if it is an .Rmd file")
  content <- readLines(doc)    # read the content of the file and stores
  # if(length() != 2) stop("") else 
    end_yaml <- which(content == "---")[2]
  writeLines(doc.Rmd)
}


# # for a single file OK... 
# # for several files? return several files
# # or if the argument is a path? to return several files in that path
# 
# # to list Rmd files
# rmd <- list.files(pattern = '*.Rmd', recursive = T)     # list of rmd files
  
  
