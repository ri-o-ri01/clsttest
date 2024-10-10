my_env <- new.env()
my_env$my_render <- function(file_name, tgt_params){
  render(paste0(here(), "/rmd/material/", file_name ),
         output_file = paste0(here(), "/rmd/material/html/", 
                              str_replace(file_name, ".Rmd", ".html")),
         params = tgt_params,
         envir = globalenv()
  )
}