
modosin <- function() {
  lfcMODOSIN$new()
}

lfcMODOSIN <- R6::R6Class(

  classname = "lfcMODOSIN",
  inherit = lfcObject_LH,
  cloneable = FALSE,

  public = list(
    get_data = function(table_name){
      res <- private$data_cache[[
        glue::glue("{table_name}")
      ]] %||%
        {  super$get_data(table_name)

        }

      return(res)
    }

  ),

  private = list(

    dbname ="creaf_v4"

  )
)





