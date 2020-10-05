# Script baseado na rotina do ASDFree
instrucoes_sas <- function(caminho) {
  win1252 <- readr::locale(encoding = 'windows-1252' )

  readr::read_lines(caminho, locale = win1252) %>%
    stringr::str_replace_all( "\t" , " " ) %>%
    str_subset("if reg=.* then do;", negate = TRUE) %>%
    stringr::str_replace_all( "@;" , "") %>%
    stringr::str_replace_all( "/;" , "/") %>%
    stringr::str_subset(stringr::regex("^input$", ignore_case = TRUE),
                        negate = TRUE) %>%
    stringr::str_subset(stringr::fixed("@3 controle 6."),
                        negate = TRUE)
}

ler_sas <- function(files, intrucoes, regex) {
  tf <- tempfile()

  writeLines(instrucoes, tf)

  df <- files %>%
    stringr::str_subset(regex) %>%
    lodown:::read_SAScii(
      sas_path = tf, beginline = str_which(instrucoes, regex),
      skip_decimal_division = TRUE, sas_encoding = "latin1"
    ) %>%
    tibble::as_tibble() %>%
    janitor::clean_names()

  file.remove(tf)

  df
}
