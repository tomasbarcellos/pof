#' Utilidade para ler arquivos de microdados
#' @param arquivo caminho para arquivo
#' @param tamanhos tamanho das colunas
#' @param nomes nomes das colunas
ler_pof <- function(arquivo, tamanhos, nomes) {
  readr::read_fwf(
    arquivo, col_positions = readr::fwf_widths(tamanhos, nomes),
    na = " ", locale = readr::locale(decimal_mark = ".")
  )
}

#' Ler tabelas da POF
#' @param ano Ano da pesquisa
#' @rdname leitura
#' @export
ler_morador <- function(ano) {
  stopifnot(ano %in% c(2003, 2009, 2018))

  regex_file <- stringr::regex("morador(_s)?\\.txt", ignore_case = TRUE)

  files <- dir(path = glue::glue("dados/{ano}/"), recursive = TRUE,
               full.names = TRUE)

  if (ano %in% c(2003, 2009)) {

    instrucoes <- files %>%
      str_subset(stringr::fixed("leitura", ignore_case = TRUE)) %>%
      instrucoes_sas()

    return(ler_sas(files, instrucoes, regex_file))

  }

  tamanhos <- c(2, 4, 1, 9, 2, 1, 2, 2, 1, 2, 2, 4, 3, 1, 1,
                1, 1, 1, 2, 1, 2, 1, 1, 1, 1, 1, 1, 1, 1, 1,
                1, 1, 1, 1, 1, 2, 1, 1, 2, 1, 1, 2, 1, 1, 1,
                2, 1, 2, 14, 14, 10)

  nomes <- c("UF", "ESTRATO_POF", "TIPO_SITUACAO_REG",
             "COD_UPA", "NUM_DOM", "NUM_UC",
             "COD_INFORMANTE", "V0306", "V0401",
             "V04021", "V04022", "V04023", "V0403",
             "V0404", "V0405", "V0406", "V0407",
             "V0408", "V0409", "V0410", "V0411",
             "V0412", "V0413", "V0414", "V0415",
             "V0416", "V041711", "V041712", "V041721",
             "V041722", "V041731", "V041732","V041741",
             "V041742", "V0418", "V0419", "V0420",
             "V0421", "V0422", "V0423", "V0424",
             "V0425", "V0426", "V0427", "V0428",
             "V0429", "V0430", "ANOS_ESTUDO","PESO",
             "PESO_FINAL", "RENDA_TOTAL")

  files %>%
    stringr::str_subset(regex_file) %>%
    ler_pof(tamanhos, nomes)
}
# morad <- ler_morador(2018)

#' @rdname leitura
#' @export
ler_desp_col <- function(ano) {
  stopifnot(ano %in% c(2003, 2009, 2018))

  tamanhos <- c(2,4,1,9,2,1,2,2,7,2,4,10,2,2,1
                ,10,1,12,10,10,1,1,2,14,14,10)

  nomes <- c("UF", "ESTRATO_POF", "TIPO_SITUACAO_REG",
             "COD_UPA", "NUM_DOM", "NUM_UC", "QUADRO",
             "SEQ", "V9001", "V9002", "V9005", "V8000",
             "V9010", "V9011", "V9012", "V1904",
             "V1905", "DEFLATOR", "V8000_DEFLA",
             "V1904_DEFLA", "COD_IMPUT_VALOR",
             "COD_IMPUT_QUANTIDADE", "FATOR_ANUALIZACAO",
             "PESO", "PESO_FINAL", "RENDA_TOTAL")
  ler_pof(glue::glue("dados/{ano}/DESPESA_COLETIVA.txt"),
          tamanhos, nomes)
}
# desp_col <- ler_desp_col(2018)

#' @rdname leitura
#' @export
ler_cad_col <- function(ano) {
  stopifnot(ano %in% c(2003, 2009, 2018))

  tamanhos <- c(2,4,1,9,2,1,2,3,7,2,10,12,
                10,1,2,14,14,10,9,4,5,9)
  nomes <- c("UF", "ESTRATO_POF", "TIPO_SITUACAO_REG",
             "COD_UPA", "NUM_DOM", "NUM_UC", "QUADRO",
             "SEQ", "V9001", "V9002", "V8000", "DEFLATOR",
             "V8000_DEFLA", "COD_IMPUT_VALOR",
             "FATOR_ANUALIZACAO", "PESO", "PESO_FINAL",
             "RENDA_TOTAL",
             "V9005", "V9007", "V9009", "QTD_FINAL"
  )
  ler_pof(glue::glue("dados/{ano}/CADERNETA_COLETIVA.txt"),
          tamanhos, nomes)
}
# cad_col <- ler_cad_col(2018)

#' @rdname leitura
#' @export
ler_desp_ind <- function(ano) {
  stopifnot(ano %in% c(2003, 2009, 2018))

  tamanhos <- c(2,4,1,9,2,1,2,2,2,7,2,10,2,
                2,1,1,1,12,10,1,2,14,14,10)
  nomes <- c("UF", "ESTRATO_POF", "TIPO_SITUACAO_REG",
             "COD_UPA", "NUM_DOM", "NUM_UC",
             "COD_INFORMANTE", "QUADRO", "SEQ", "V9001",
             "V9002", "V8000", "V9010", "V9011", "V9012",
             "V4104", "V4105", "DEFLATOR", "V8000_DEFLA",
             "COD_IMPUT_VALOR", "FATOR_ANUALIZACAO",
             "PESO", "PESO_FINAL", "RENDA_TOTAL"
  )
  ler_pof(glue::glue("dados/{ano}/DESPESA_INDIVIDUAL.txt"),
          tamanhos, nomes)
}
# desp_ind <- ler_desp_ind(2018)

#' @rdname leitura
#' @export
ler_aluguel <- function(ano) {
  stopifnot(ano %in% c(2003, 2009, 2018))

  tamanhos <- c(2,4,1,9,2,1,2,7,2,10,2,2,12,10,1,2,14,14,10)
  nomes <- c("UF", "ESTRATO_POF", "TIPO_SITUACAO_REG",
             "COD_UPA", "NUM_DOM", "NUM_UC", "QUADRO",
             "V9001", "V9002", "V8000", "V9010", "V9011",
             "DEFLATOR", "V8000_DEFLA", "COD_IMPUT_VALOR",
             "FATOR_ANUALIZACAO", "PESO", "PESO_FINAL",
             "RENDA_TOTAL"
  )

  ler_pof(glue::glue("dados/{ano}/ALUGUEL_ESTIMADO.txt"),
          tamanhos, nomes)
}
# alug <- ler_aluguel(2018)

#' @rdname leitura
#' @export
ler_rend_trab <- function(ano) {
  stopifnot(ano %in% c(2003, 2009, 2018))

  regex_file <- stringr::regex("(?<!outros_|SAS/|de )rendim.+(_s)?\\.txt",
                               ignore_case = TRUE)

  files <- dir(path = glue::glue("dados/{ano}/"), recursive = TRUE,
               full.names = TRUE)

  if (ano %in% c(2003, 2009)) {

    instrucoes <- files %>%
      str_subset(stringr::fixed("leitura", ignore_case = TRUE)) %>%
      instrucoes_sas()

    return(ler_sas(files, instrucoes, regex_file))

  }

  tamanhos <- c(2,4,1,9,2,1,2,2,1,1,7,1,1,1,1,1,1,7,7,7
                ,7,2,2,3,1,12,10,10,10,10,1,1,14,14,10,4,5)
  nomes <- c("UF", "ESTRATO_POF", "TIPO_SITUACAO_REG",
             "COD_UPA", "NUM_DOM", "NUM_UC",
             "COD_INFORMANTE", "QUADRO", "SUB_QUADRO",
             "SEQ", "V9001", "V5302", "V53021", "V5303",
             "V5304", "V5305", "V5307", "V8500", "V531112",
             "V531122", "V531132", "V9010", "V9011",
             "V5314", "V5315", "DEFLATOR", "V8500_DEFLA",
             "V531112_DEFLA", "V531122_DEFLA",
             "V531132_DEFLA", "COD_IMPUT_VALOR",
             "FATOR_ANUALIZACAO", "PESO", "PESO_FINAL",
             "RENDA_TOTAL","V53011","V53061"
  )

  files %>%
    str_subset(regex_file) %>%
    ler_pof(tamanhos, nomes)
}
# trab <- ler_rend_trab(2018)

#' @rdname leitura
#' @export
ler_rend_outros <- function(ano) {
  stopifnot(ano %in% c(2003, 2009, 2018))

  regex_file <- stringr::regex("outro.+(_s)?\\.txt", ignore_case = TRUE)

  files <- dir(path = glue::glue("dados/{ano}/"), recursive = TRUE,
               full.names = TRUE)

  if (ano %in% c(2003, 2009)) {

    instrucoes <- files %>%
      str_subset(stringr::fixed("leitura", ignore_case = TRUE)) %>%
      instrucoes_sas()

    return(ler_sas(files, instrucoes, regex_file))

  }

  tamanhos <- c(2, 4, 1, 9, 2, 1, 2, 2, 2, 7, 10, 10, 2,
                2, 12, 10, 10, 1, 1, 14, 14, 10)
  nomes <- c("UF", "ESTRATO_POF", "TIPO_SITUACAO_REG",
             "COD_UPA", "NUM_DOM", "NUM_UC",
             "COD_INFORMANTE", "QUADRO", "SEQ", "V9001",
             "V8500", "V8501", "V9010", "V9011",
             "DEFLATOR", "V8500_DEFLA", "V8501_DEFLA",
             "COD_IMPUT_VALOR", "FATOR_ANUALIZACAO",
             "PESO", "PESO_FINAL", "RENDA_TOTAL")

  files %>%
    str_subset(regex_file) %>%
    ler_pof(tamanhos, nomes)
}
# outros <- ler_rend_outros(2018)

#' @rdname leitura
#' @export
ler_domicilio <- function(ano) {
  stopifnot(ano %in% c(2003, 2009, 2018))

  regex_file <- stringr::regex("domicilio(_s)?\\.txt", ignore_case = TRUE)

  files <- dir(path = glue::glue("dados/{ano}/"), recursive = TRUE,
               full.names = TRUE)

  if (ano %in% c(2003, 2009)) {

    instrucoes <- files %>%
      str_subset(stringr::fixed("leitura", ignore_case = TRUE)) %>%
      instrucoes_sas()

    return(ler_sas(files, instrucoes, regex_file))

  }

  tamanhos <- c(2,4,1,9,2,1,1,1,1,2,1,1,1,1,1,1,1,1,1,2,
                1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,14,14)
  nomes <- c("UF", "ESTRATO_POF", "TIPO_SITUACAO_REG",
             "COD_UPA", "NUM_DOM", "V0201", "V0202",
             "V0203", "V0204", "V0205", "V0206", "V0207",
             "V0208", "V0209", "V02101", "V02102",
             "V02103", "V02104", "V02105", "V02111",
             "V02112", "V02113", "V0212", "V0213",
             "V02141", "V02142", "V0215", "V02161",
             "V02162", "V02163", "V02164", "V0217",
             "V0219", "V0220", "V0221", "PESO",
             "PESO_FINAL"
  )

  files %>%
    str_subset(regex_file) %>%
    ler_pof(tamanhos, nomes)
}

#' Tradutores de rendimento
#'
#' @param ano Ano do tradutor
#' @return Uma tibble com tradutor
#' @rdname tradutores
#' @export
ler_tradutor_rendimento <- function(ano) {
  pasta <- dir(glue::glue("dados/{ano}/"), full.names = TRUE, pattern = "[Tt]radutores.+")
  arq <- dir(glue::glue("{pasta[[1]]}"), pattern = "[Tt]radutor[_ ][Rr]endimento", full.names = T)

  readxl::read_excel(arq) %>%
    janitor::clean_names()
}

#' @rdname tradutores
#' @export
ler_tradutor_despesa <- function(ano) {
  pasta <- dir(glue::glue("dados/{ano}/"), full.names = TRUE, pattern = "[Tt]radutores.+")
  arq <- dir(glue::glue("{pasta[[1]]}"), pattern = "[Tt]radutor[_ ][Dd]espesa_[Gg]eral", full.names = T)

  readxl::read_excel(arq) %>%
    janitor::clean_names()
}

#' @rdname tradutores
#' @export
ler_tradutor_alimentacao <- function(ano) {
  pasta <- dir(glue::glue("dados/{ano}/"), full.names = TRUE, pattern = "[Tt]radutores.+")
  arq <- dir(glue::glue("{pasta[[1]]}"), pattern = "[Tt]radutor[_ ][Aa]liment", full.names = T)

  readxl::read_excel(arq) %>%
    janitor::clean_names()
}

