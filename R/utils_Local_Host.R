
# ........... DESCRIBRE TABLE ...............
# ...........................................



modosin_describe_table_cat = function(table, tables_dict, variables_thes){
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


modosin_describe_var_eng <- function(variable, variables_thes, numerical_thes) {

  . <- NULL

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
