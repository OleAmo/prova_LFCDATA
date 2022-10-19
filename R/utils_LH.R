
# ...... SITEDROUGHT TABLE DICTIONARY .......
# ...........................................

#        .) Tenemos solo UNA TABLA => data_day
#        .) Por lo tanto el diccionario solo saldrà
#              .) DATA
#              .) DAY


siteDrought_table_dictionary <- function() {
  c(
    day = ', every day are generated variables from MEDFATE Model.',
    data = 'Plots eco-physiological variables from NFI version 4 (2013-2016) of Catalunya and
            Parque Nacional Ordesa y Monte Perdido also plots from Scrubland Project '
  )
}


# ........... DESCRIBRE TABLE ...............
# ...........................................

#        .) VARIABLE_NAMES:
#              .) De la tabla VARIABLE THESAURUS nos quedamos
#              .) Con las variable que la COLUMNA VAR_Table es = ala TABLA
#              .) El PULL hace que nos quedamos SOLO con la columna VAR_ID (REW,PET,LAI,...)
#              .) El UNIQUE nos evita las repeticiones

#        .) TABLE_DECONSTRUCTED:
#              .) Hacemos SPLIT (`_') con el nombre de la TABLA
#              .) Después cada parte las compararemos con el diccionario
#              .) Así tendremos una DESCRIPCION de la TABLA

#              .) O sea, para DESCRIVIR la TABLA usamos la palabras de la misma Tabla
#              .) Y el comparar-lo con un diccionario para describir-lo

#              .) Lo podría hacer con una CONSULTA SQL a una SUB TABLA
#              .) Pero esto implicaría conectar con la BBDD y esto tardaría mas tiempo

#        .) CAT:
#              .) TABLES_DICT[table_deconstructed]
#                     .) De la definciones del DICCIONARIO nos quedamos SOLO
#                     .) Con las que coinciden con las palabras del SPLIT TABLE
#              .) SORT(variable_names)
#                     .) Ordenamos la VARIABLE_NAME


siteDrought_describe_table_cat = function(table, tables_dict, variables_thes){

    variable_names <- variables_thes %>%
        dplyr::filter(.data$var_table == table) %>%
        dplyr::pull(.data$var_id) %>%
        unique()

    table_deconstructed <- stringr::str_split(table, '_') %>%
      purrr::flatten_chr()


    cat('\n', crayon::yellow$bold(table), '\n', sep = '')
    # table descriptio
    cat(
      glue::glue("{tables_dict[table_deconstructed] %>% purrr::discard(is.na)}") %>%
        glue::glue_collapse() %>%
        crayon::green() %>%
        strwrap(width = 75),
      # '\n',
      fill = 80, sep = ''
    )

    cat('Variables in table:\n')
    cat(
      glue::glue(" - {sort(variable_names)}") %>%
        glue::glue_collapse(sep = '\n') %>%
        crayon::magenta()
    )
    cat('\n')
}

# .......... DESCRIBRE VARIABLE .............
# ...........................................

#        .) VARIABLE_THES:
#              .) De la tabla VARIABLE THESAURUS nos quedamos
#              .) Con las VAR_ID que es IGUAL a VARIABLE
#                       -) REW    prova_plot_nfi2_genus .....
#                       -) REW    data_day              .....
#              .) Hacemos LEFT_JOIN y AÑADIMOS variables numerical (min, max, var)
#              .) AGRUPAMOS por VAR_DESCRIPTION_ENG

#        .) CAT:
#              .) AMARILLO = Título Variable Inglés = Translation_ENG (Var_ID)
#              .) VERDE = Descripción en inglés de la Variable
#              .) AZUL = Unidades



siteDrought_describe_var_eng <- function(variable, variables_thes, numerical_thes) {

  variables_thes %>%
    dplyr::filter(.data$var_id == variable) %>%
    dplyr::left_join(numerical_thes, by = c("var_id", "var_table")) %>%
    dplyr::group_by(.data$var_description_eng) %>%
    dplyr::group_walk(
      ~ cat(
        "\n",
        # var name
        crayon::yellow$bold(glue::glue(
          "{.x$translation_eng %>% unique()} ({.x$var_id %>% unique()})"
        )),
        "\n",
        # var description
        strwrap(crayon::green(.y$var_description_eng), width = 72),
        "\n",
        # var units
        crayon::blue$bold(
          "Units: [" %+%
            crayon::blue$italic$bold(
              glue::glue("{(.x$var_units %na% ' - ') %>% unique()}")
            ) %+%
            "]"
        ),
        "\n",
        # tables present
        "Present in the following tables:\n",
        crayon::magenta(glue::glue_collapse(
          glue::glue(" - {sort(.x$var_table)}"), sep = '\n'
        )),
        # cat options
        sep = '', fill = 80
      )
    )

  return(invisible(NULL))
}
