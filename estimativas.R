library(pof)
library(tidyverse)

morador_uc <- ler_morador(2018) %>%
  select(UF, ESTRATO_POF, TIPO_SITUACAO_REG,
         COD_UPA, NUM_DOM ,NUM_UC, PESO_FINAL) %>%
  unique()

soma_familia <- sum(morador_uc$PESO_FINAL)

dic_rendimento <- ler_tradutor_rendimento(2018)

dic2 <- dic_rendimento %>%
  select(codigo, nivel = nivel_3, desc = descricao_3) %>%
  filter(!is.na(nivel)) %>%
  bind_rows(
    dic_rendimento %>%
      filter(nivel_2 %in% c(13, 14)) %>%
      select(codigo, nivel = nivel_2, desc = descricao_2)
  )

rend_trabalho <- ler_rend_trab(2018) %>%
  filter(!is.na(V8500_DEFLA)) %>%
  transmute(
    V9001 = V9001,
    v5302 = V5302, v5303 = V5303, v5304 = V5304,
    cod_uc = paste0(UF, ESTRATO_POF, TIPO_SITUACAO_REG,
                    COD_UPA, NUM_DOM ,NUM_UC),
    valor_mensal = (V8500_DEFLA*V9011*FATOR_ANUALIZACAO)/12,
    peso_final = PESO_FINAL
  )

outros_rend <- ler_rend_outros(2018) %>%
  transmute(
    V9001 = V9001,
    cod_uc = paste0(UF, ESTRATO_POF, TIPO_SITUACAO_REG,
                    COD_UPA, NUM_DOM ,NUM_UC),
    valor_mensal = ifelse( QUADRO == 54,
                           (V8500_DEFLA*V9011*FATOR_ANUALIZACAO)/12,
                           (V8500_DEFLA*FATOR_ANUALIZACAO)/12
    ),
    peso_final = PESO_FINAL
  )

junta_rendas <- rend_trabalho %>%
  bind_rows(outros_rend) %>%
  mutate(codigo = str_sub(V9001, 1,5)) %>%
  left_join(dic2, by = "codigo") %>%
  # Os casos que caem com o filtro abaixo são movimentações financeiras que não sao renda
  # deposito de poupança, compra de ações, etc...
  filter(!is.na(nivel)) %>%
  mutate(forma = case_when(
    # rendimento de empregado baseado no vínculo
    nivel == 111 & v5302 == 1 ~ "cv",
    nivel == 111 & v5302 == 2 ~ "mv",
    nivel == 111 & v5302 == 3 ~ "cv",
    nivel == 111 & v5302 == 4 & v5303 == 1 ~ "mv", # setor estatutário
    # supoe não ter informalidade no setor publico
    nivel == 111 & v5302 == 4 & v5304 == 1 ~ "cv", # trabalhador de estatal
    # Tem casos (2491) de servidor sem carteria assinada ou estatuto. Pq?
    nivel == 111 & v5302 == 4 ~ "mv", # caso acima
    nivel == 111 & v5302 == 5 ~ "emp", # empregador
    nivel == 111 & v5302 == 6 ~ "cp", # conta própria # não encontrei setor trabalhado
    nivel == 111 & v5302 == 7 ~ "cv", # trab não remunerado ?
    nivel == 111 ~ "cv", # empregado mas sem informação da v5302
    nivel == 112 ~ "emp", # empregador
    nivel == 121 ~ "cv", # INSS
    nivel == 122 ~ "cv", # previdencia pública
    nivel == 123 ~ "mv", # previdencia privada
    nivel == 124 ~ "cv", # programas sociais
    nivel == 13 ~ "mv", # aluguel
    nivel == 14 ~ "mv", # outras rendas (morador ausente, menor de 10,
                        #                indenização judicial, acoes, juros, outros)

    # Deixei esses casos por ultimo por sao casos para pensarmos
    nivel == 113 ~ "cp", # conta própria
    nivel == 125 ~ "cv", # pensao alimenticia, mesada, etc ?
    nivel == 126 ~ "cv", # outras transferências
    TRUE ~ NA_character_,
  ))

junta_rendas %>%
  group_by(forma) %>%
  summarise(s = sum(valor_mensal * peso_final),
            m = weighted.mean(valor_mensal, peso_final)) %>%
  arrange(desc(s))

junta_rendas %>%
  filter(forma == "cp") %>%
  mutate(grupo = kmeans(valor_mensal, c(1e3, 20e3))$cluster) %>%
  ggplot(aes(valor_mensal, col = as.factor(grupo))) +
  geom_density() +
  scale_x_continuous(limits = c(0, 15)*1000, breaks = c(0:15)*1e3) +
  theme(axis.text.x = element_text(angle = 90), axis.text.y = element_blank())

junta_rendas %>%
  filter(forma == "emp") %>%
  mutate(grupo = kmeans(valor_mensal, c(3e3, 5e3, 10e3, 20e3))$cluster) %>%
  ggplot(aes(valor_mensal, col = as.factor(grupo))) +
  geom_density() +
  scale_x_continuous(limits = c(0, 50)*1000, breaks = c(seq(0, 50, 10))*1e3) +
  theme(axis.text.x = element_text(angle = 90), axis.text.y = element_blank())

rendas_m <- junta_rendas %>%
  mutate(forma = ifelse(forma != "cp", forma,
                        ifelse(valor_mensal > 6000, "mv", "cv")))

rendas_m %>%
  group_by(forma) %>%
  summarise(massa = sum(valor_mensal * peso_final),
            media = weighted.mean(valor_mensal, peso_final),
            quantidade = sum(peso_final)) %>%
  arrange(desc(media))

rendas_ucs <- rendas_m %>%
  group_by(cod_uc, forma) %>%
  summarise(renda = sum(valor_mensal)) %>%
  pivot_wider(names_from = forma, values_from = renda, values_fill = list(renda = 0)) %>%
  mutate(total = cv + mv,
         p_cv = cv/total) %>%
  ungroup()

rendas_ucs %>%
  arrange(desc(p_cv))

k_rendas <- kmeans(rendas_ucs$p_cv, c(0.2, 0.5))
k_rendas$centers
ggplot(rendas_ucs, aes(p_cv, fill = factor(k_rendas$cluster))) +
  geom_histogram(binwidth = 0.05)

k_rendas2 <- kmeans(rendas_ucs[, c("total", "p_cv")], 2)
k_rendas2$centers

ggplot(rendas_ucs, aes(p_cv, total, col = factor(k_rendas2$cluster))) +
  geom_point(shape = ".", alpha = 0.5) +
  scale_y_continuous(limits = c(0, 20e3)) +
  theme_classic() +
  theme(legend.position = "none")

ggplot(rendas_ucs, aes(p_cv, total, col = factor(k_rendas$cluster))) +
  geom_point(shape = ".", alpha = 0.5) +
  scale_y_continuous(limits = c(0, 20e3)) +
  theme_classic() +
  theme(legend.position = "none")

ggplot(rendas_ucs, aes(p_cv, col = factor(k_rendas$cluster))) +
  geom_density() #+
  # scale_x_continuous(limits = c(0, 20e3))

ggplot(rendas_ucs, aes(total, col = factor(k_rendas2$cluster))) +
  geom_density() +
  scale_x_continuous(limits = c(0, 20e3))

ggplot(rendas_ucs, aes(total, col = factor( & ))) +
  geom_density() +
  scale_x_continuous(limits = c(0, 20e3))

rendas_ucs %>%
  mutate(esfera = ifelse(p_cv > 0.60, "baixa", "alta")) %>%
  left_join(transmute(morador_uc,
                      cod_uc = paste0(UF, ESTRATO_POF, TIPO_SITUACAO_REG,
                                      COD_UPA, NUM_DOM ,NUM_UC),
                      peso_final = PESO_FINAL)) %>%
  group_by(esfera) %>%
  summarise(n = sum(peso_final))

esferas_ucs <- rendas_ucs %>%
  mutate(esfera = ifelse(p_cv > 0.60, "baixa", "alta")) %>%
  select(cod_uc, esfera)

rendas_ucs %>%
  # mutate(esfera = c("baixa", "alta")[k_rendas2$cluster]) %>%
  mutate(esfera = ifelse(p_cv > 0.60, "baixa", "alta")) %>%
  group_by(esfera) %>%
  summarise(sum(total))
  # select(cod_uc, esfera)


# Despesas
alu_estimado <- ler_aluguel(2018) %>%
  transmute(
    V9001 = V9001,
    cod_uc = paste0(UF, ESTRATO_POF, TIPO_SITUACAO_REG,
                    COD_UPA, NUM_DOM ,NUM_UC),
    valor_mensal = (V8000_DEFLA*V9011*FATOR_ANUALIZACAO*PESO_FINAL)/12
  )

desp_coletiva <- ler_desp_col(2018) %>%
  transmute(V9001 = V9001,
            cod_uc = paste0(UF, ESTRATO_POF, TIPO_SITUACAO_REG,
                            COD_UPA, NUM_DOM ,NUM_UC),
            valor_mensal = ifelse( QUADRO == 10 | QUADRO == 19,
                                   (V8000_DEFLA*V9011*FATOR_ANUALIZACAO*PESO_FINAL)/12,
                                   (V8000_DEFLA*FATOR_ANUALIZACAO*PESO_FINAL)/12
            ) ,
            inss_mensal=(V1904_DEFLA*V9011*FATOR_ANUALIZACAO*PESO_FINAL)/12
  )

cad_coletiva <- ler_cad_col(2018) %>%
  transmute(V9001 = V9001,
            cod_uc = paste0(UF, ESTRATO_POF, TIPO_SITUACAO_REG,
                            COD_UPA, NUM_DOM ,NUM_UC),
            valor_mensal = (V8000_DEFLA*FATOR_ANUALIZACAO*PESO_FINAL)/12
  )

desp_individual <- ler_desp_ind(2018) %>%
  transmute(V9001 = V9001,
            cod_uc = paste0(UF, ESTRATO_POF, TIPO_SITUACAO_REG,
                            COD_UPA, NUM_DOM ,NUM_UC),
            valor_mensal = ifelse( QUADRO %in% c(44, 47, 48, 49, 50),
                                   (V8000_DEFLA*V9011*FATOR_ANUALIZACAO*PESO_FINAL)/12,
                                   (V8000_DEFLA*FATOR_ANUALIZACAO*PESO_FINAL)/12)
  )

rend_trabalho <- ler_rend_trab(2018) %>%
  transmute(V9001 = V9001,
            cod_uc = paste0(UF, ESTRATO_POF, TIPO_SITUACAO_REG,
                            COD_UPA, NUM_DOM ,NUM_UC),
            prev_pub_mensal=(V531112_DEFLA*V9011*FATOR_ANUALIZACAO*PESO_FINAL)/12,
            imp_renda_mensal=(V531122_DEFLA*V9011*FATOR_ANUALIZACAO*PESO_FINAL)/12,
            iss_mensal=(V531132_DEFLA*V9011*FATOR_ANUALIZACAO*PESO_FINAL)/12
  )

outros_rend <- ler_rend_outros(2018) %>%
  transmute(V9001 = V9001,
            cod_uc = paste0(UF, ESTRATO_POF, TIPO_SITUACAO_REG,
                            COD_UPA, NUM_DOM ,NUM_UC),
            deducao_mensal = ifelse( QUADRO == 54,
                                     (V8501_DEFLA*V9011*FATOR_ANUALIZACAO*PESO_FINAL)/12,
                                     (V8501_DEFLA*FATOR_ANUALIZACAO*PESO_FINAL)/12
            )
  )

desp_coletiva_n <- desp_coletiva %>%
  mutate(prev_pub_mensal = NA, imp_renda_mensal = NA,
         iss_mensal = NA , deducao_mensal = NA)

cad_coletiva_n <- cad_coletiva %>%
  mutate(inss_mensal = NA, prev_pub_mensal = NA, imp_renda_mensal = NA,
         iss_mensal = NA, deducao_mensal = NA)

desp_individual_n <- desp_individual %>%
  mutate(inss_mensal = NA, prev_pub_mensal = NA, imp_renda_mensal = NA,
         iss_mensal = NA, deducao_mensal = NA)

alu_estimado_n <- alu_estimado %>%
  mutate(V9001 = as.numeric(V9001), inss_mensal = NA, prev_pub_mensal = NA,
         imp_renda_mensal = NA, iss_mensal = NA, deducao_mensal = NA)

rend_trabalho_n <- rend_trabalho %>%
  mutate(inss_mensal = NA, prev_pub_mensal = NA, imp_renda_mensal = NA,
         iss_mensal = NA, deducao_mensal = NA)

outros_rend_n <- outros_rend %>%
  mutate(valor_mensal = NA, inss_mensal = NA, prev_pub_mensal = NA,
         imp_renda_mensal = NA, iss_mensal = NA) %>%
  select(V9001, valor_mensal, inss_mensal:iss_mensal, deducao_mensal)

junta_despesas <- bind_rows( desp_coletiva_n ,
                    cad_coletiva_n ,
                    desp_individual_n ,
                    alu_estimado_n,
                    rend_trabalho_n ,
                    outros_rend_n ) %>%
  mutate(codigo = round(V9001/100)) %>%
  select(-V9001)

merge1 <- junta_despesas %>%
  left_join(ler_tradutor_despesa(2018) %>%
              select(codigo, variavel, starts_with("nivel")),
            "codigo") %>%
  mutate(
    valor = ifelse( variavel == 'V8000_DEFLA' ,
                    valor_mensal ,
                    ifelse( variavel == 'V1904_DEFLA' ,
                            inss_mensal ,
                            ifelse( variavel == 'V531112_DEFLA' ,
                                    prev_pub_mensal ,
                                    ifelse( variavel == 'V531122_DEFLA' ,
                                            imp_renda_mensal ,
                                            ifelse( variavel == 'V531132_DEFLA' ,
                                                    iss_mensal ,
                                                    ifelse( variavel == 'V8501_DEFLA' ,
                                                            deducao_mensal ,
                                                            NA
                                                    )
                                            )
                                    )
                            )
                    )
    )
  ) %>%
  filter(!is.na(valor))

# Estimativa geral esferas
merge1 %>%
  left_join(esferas_ucs, by = "cod_uc") %>%
  group_by(esfera) %>%
  summarise(soma = sum(valor) * 12 / 1e9)  %>%
  mutate(partic = soma / sum(soma))

# Estimativa um produto: livros
# 110803 - Livros
merge1 %>%
  left_join(esferas_ucs, by = "cod_uc") %>%
  group_by(nivel = nivel_4, esfera) %>%
  summarise(soma = sum(valor) * 12 / 1e6)  %>%
  filter(nivel == 110803) %>%
  mutate(partic = soma / sum(soma))

