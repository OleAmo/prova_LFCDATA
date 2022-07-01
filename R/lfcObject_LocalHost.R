

lfcObject_LH <- R6::R6Class(

  classname = 'lfcObject_LH',

  public = list(
     initialize = function(){
     private$pool_conn <- private$pool_conn_create()
    },

  # ............... TIMING GET_DATA R ................
  # ..................................................

  #      .) Es igual al GET_DATA_R, pero:
  #            .) ANULO  MESSAGES
  #            .) ANULO  if (inherits)

  get_data_R_tim = function(table_name){
    private$data_cache[[glue::glue("{table_name}_FALSE")]] %||% {

      query_data <- try(
        dplyr::tbl(private$pool_conn, table_name) %>% dplyr::collect()
      )

      if (inherits(query_data, "try-error")) {
        stop("Can not connect to the database:\n", query_data[1])
      } else {
        private$data_cache[[glue::glue("{table_name}_FALSE")]] <- query_data
        return(query_data)
      }

    }
  },

  # ............... TIMING GET_DATA SQL ................
  # ..................................................

  #      .) Es igual al GET_DATA_SQL, pero:
  #            .) ANULO  MESSAGES
  #            .) ANULO  if (inherits)


  get_data_SQL_tim = function(table_name,date){
    private$data_cache[[glue::glue("{table_name}_{date}_FALSE")]] %||% {

      date_2 <- as.Date(date, format = "%Y-%m-%d")

      sql <- glue:::glue("
        SELECT * FROM public.{table_name}
        WHERE date = '{date_2}' ")

      query_data <- try(pool::dbGetQuery(private$pool_conn,sql))

      if (inherits(query_data, "try-error")) {
        stop("Can not connect to the database:\n", query_data[1])
      } else {
        private$data_cache[[glue::glue("{table_name}_{date}_FALSE")]] <- query_data
        return(query_data)
      }


    }
  },


  # ................... GET_DATA R ...................
  # .................................................

  #      .) La Consulta SQL => TODAS las FECHAS
  #      .) En R = seleccion de fecha

  get_data_R = function(table_name){
    private$data_cache[[glue::glue("{table_name}_FALSE")]] %||% {

      check_args_for(character = list(table_name = table_name))
      check_length_for(table_name, 1)

      message('Querying table from LFC database, this can take a while...')
      query_data <- try(
        dplyr::tbl(private$pool_conn, table_name) %>% dplyr::collect()
      )
      message('Done')

      if (inherits(query_data, "try-error")) {
        stop("Can not connect to the database:\n", query_data[1])
      } else {
        private$data_cache[[glue::glue("{table_name}_FALSE")]] <- query_data
        return(query_data)
      }
    }
  },

  # ................. GET_DATA SQL ..................
  # .................................................

  #      .) La Consulta SQL => SELECCION por FECHA
  #      .)


  get_data_SQL = function(table_name,date){
    private$data_cache[[glue::glue("{table_name}_{date}_FALSE")]] %||% {

      message('Querying table from LFC database, this can take a while...')
      date_2 <- as.Date(date, format = "%Y-%m-%d")

      sql <- glue:::glue("
        SELECT * FROM public.{table_name}
        WHERE date = '{date_2}' ")

      query_data <- try(pool::dbGetQuery(private$pool_conn,sql))

      message('Done')

      if (inherits(query_data, "try-error")) {
        stop("Can not connect to the database:\n", query_data[1])
      } else {
        private$data_cache[[glue::glue("{table_name}_{date}_FALSE")]] <- query_data
        return(query_data)
      }

    }
  }),

  private = list(
    dbname = NULL,
    pool_conn = NA,
    pool_conn_create = function(){
      res <- try({
        pool::dbPool(
          drv = RPostgres::Postgres(),
          dbname = private$dbname,
          host = 'localhost',
          port = 5432,
          idleTimeout = 3600,
          user = 'postgres',
          password = '12345database',
          options = "-c client_min_messages=warning"
        )

      })
      return(res)
    },

    data_cache = list(),

    finalize = function(){
      pool::poolClose(private$pool_conn)

    }

  )





)
