
modosin <- function() {
  lfcMODOSIN$new()
}

lfcMODOSIN <- R6::R6Class(

  classname = "lfcMODOSIN",
  inherit = lfcObject_LH,
  cloneable = FALSE,

  public = list(

    # ............ AÑADIR ...........
    # ...............................
    #      .) GET_DATA
    #               .) DATE SELECCION:
    #               .) solo DATA_DAY de la fecha
    #      .) DESCRIVE_TABLE
    #      .) DESCRIVE_VALUE

    # ... GET_DATA....
    # ................

    get_data = function(table_name){
      res <- private$data_cache[[glue::glue("{table_name}")]] %||%
        { super$get_data(table_name) }
      return(res)
    },

    # ... AVAIL TABLES ...
    # ....................

    avail_tables = function() {
      # no tables = Tablas que no se mostraran
      no_tables<- c("geography_columns","geometry_columns","layer","spatial_ref_sys","topology")
      res_1 <- pool::dbListTables(private$pool_conn) %>%
        tolower() %>%
        unique() %>%
        sort()
      # eliminar de los resultados las tablas que el usuario no visualizará
      res_2 <- res_1[!res_1 %in% no_tables]
      return(res_2)
    },

    # ... DESCRIVE TABLE ...
    # ......................

    describe_table = function(tables){
      tables_dict <- nfi_table_dictionary()
      variables_thes <- suppressMessages(self$get_data('variables_thesaurus'))

      tables %>%
        purrr::map(
          modosin_describe_table_cat,
          tables_dict = tables_dict, variables_thes = variables_thes
        )
      return(invisible(self))
    },

    # ... DESCRIVE VAR ...
    # ....................

    describe_var = function(variables) {

      variables_thes <- suppressMessages(self$get_data('variables_thesaurus'))
      numerical_thes <- suppressMessages(self$get_data('variables_numerical'))

      variables %>%
        purrr::map(
          modosin_describe_var_eng,
          variables_thes = variables_thes, numerical_thes = numerical_thes
        )

      invisible(self)

    }

  ),

  private = list(
    dbname ="creaf_v4"
  )
)





