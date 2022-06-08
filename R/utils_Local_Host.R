

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
