
modosin <- function() {
  lfcMODOSIN$new()
}

lfcMODOSIN <- R6::R6Class(

  classname = "lfcMODOSIN",
  inherit = lfcObject_LH,
  cloneable = FALSE,


  public = list(

    # ............... TIMING GET_DATA R ................
    # ..................................................

    #      .) Es igual al GET_DATA_R, pero:
    #            .) ANULO  Return (RES)
    #            .) ACTIVO Return (DIF)

    get_data_TIMING_R = function(table_name,date_1){

      t1 <- Sys.time()
      res <- private$data_cache[[glue::glue("{table_name}_{date_1}_FALSE")]] %||%
        {
          query_data_spatial <- super$get_data_R_tim(table_name) %>%
            data.frame() %>%
            dplyr::filter(date == date_1) %>%
            head(5)
          private$data_cache[[glue::glue("{table_name}_{date_1}_FALSE")]] <- query_data_spatial
          query_data_spatial
        }
      t2 <- Sys.time()
      dif <- (t2 - t1)
      return(dif)

    },

    # ............... TIMING GET_DATA SQL ................
    # ..................................................

    #      .) Es igual al GET_DATA_SQL, pero:
    #            .) ANULO  Return (RES)
    #            .) ACTIVO Return (DIF)


    get_data_TIMING_SQL = function(table_name,date_1){

      date_2 <- as.Date(date_1, format = "%Y-%m-%d")
      t1 <- Sys.time()
      res <- private$data_cache[[glue::glue("{table_name}_{date_1}_FALSE")]] %||%
        {
          query_data_spatial <- super$get_data_SQL_tim(table_name,date_2) %>%
            data.frame() %>%
            head(5)
          private$data_cache[[glue::glue("{table_name}_{date_1}_FALSE")]] <- query_data_spatial
          query_data_spatial
        }
      t2 <- Sys.time()
      dif <- (t2 - t1)
      return(dif)

    },



    # ................... GET_DATA R ...................
    # ..................................................

    #      .) La Consulta SQL => TODAS las FECHAS
    #      .) En R = seleccion de fecha

    get_data_by_R = function(table_name,date_1){
      date_1 <- as.Date(date_1, format = "%Y-%m-%d")
      t1 <- Sys.time()
      res <- private$data_cache[[glue::glue("{table_name}_{date_1}_FALSE")]] %||%
        {
          query_data_spatial <- super$get_data_R(table_name) %>%
            data.frame() %>%
               dplyr::filter(date == date_1) %>%
                head(5)
          private$data_cache[[glue::glue("{table_name}_{date_1}_FALSE")]] <- query_data_spatial
          query_data_spatial
        }
      t2 <- Sys.time()
      dif <- (t2 - t1)

      cat (crayon::yellow$bold("Processing Time = ",round(dif[[1]], digits = 4)," seg \n") )
      cat ("\n")
      return(res)

    },

    # ................. GET_DATA SQL ..................
    # .................................................

    #      .) La Consulta SQL => SELECCION por FECHA
    #      .) En R = lo pasamos a dataframe

    get_data_by_SQL = function(table_name,date_1){

      date_2 <- as.Date(date_1, format = "%Y-%m-%d")
      t1 <- Sys.time()
      res <- private$data_cache[[glue::glue("{table_name}_{date_1}_FALSE")]] %||%
        {
          query_data_spatial <- super$get_data_SQL(table_name,date_2) %>%
            data.frame() %>%
            head(5)
          private$data_cache[[glue::glue("{table_name}_{date_1}_FALSE")]] <- query_data_spatial
          query_data_spatial
        }
      t2 <- Sys.time()
      dif <- (t2 - t1)

      cat (crayon::yellow$bold("Processing Time = ",round(dif[[1]], digits = 4)," seg \n") )
      cat ("\n")
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
      variables_thes <- suppressMessages(self$get_data_R('variables_thesaurus'))

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

      variables_thes <- suppressMessages(self$get_data_R('variables_thesaurus'))
      numerical_thes <- suppressMessages(self$get_data_R('variables_numerical'))

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





