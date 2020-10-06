#' @importFrom magrittr %>%
#' @export
magrittr::`%>%`

listar_ftp <- pnadc:::listar_ftp

criar_link <-  function(ano) {
  stopifnot(ano %in% c(2003, 2009, 2018))

  inicio <- glue::glue(
    "ftp://ftp.ibge.gov.br/Orcamentos_Familiares/",
    "Pesquisa_de_Orcamentos_Familiares_{ano-1}_{ano}/",
    "Microdados/"
  )
  arquivos <- listar_ftp(inicio) %>%
    stringr::str_subset("\\.zip$") %>%
    sort()

  glue::glue(
    "{inicio}{arquivos}"
  )
}
#' Downlaod dos arquivos da POF
#' @importFrom utils download.file
#' @param ano Ano da pesquisa
#' @export
download_pof <- function(ano) {
  stopifnot(ano %in% c(2003, 2009, 2018))
  links <- criar_link(ano)

  if (!dir.exists("dados")) dir.create("dados")

  extdir <- glue::glue("dados/{ano}")

  if (!dir.exists(extdir)) dir.create(extdir)
  arqs <- stringr::str_extract(links, "(?<=/)\\w+\\.zip")
  utils::download.file(links, glue::glue("dados/{ano}/{arqs}"))
}

#' Extrair dados da POF
#' @importFrom utils unzip
#' @param ano Ano da pesquisa
#' @export
unzip_pof <- function(ano) {
  dir(glue::glue("dados/{ano}"), full.names = TRUE) %>%
    stringr::str_subset("\\.zip$") %>%
    lapply(utils::unzip, exdir = glue::glue("dados/{ano}"))
}

