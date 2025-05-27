###CODIGO-BASE ISA CAPITAL 2023 SMS-SP V.26052025_16h21m###
###ELABORAÇÂO: José Olimpio Moura de Albuquerque (NADEpi/CIEVS/DEVE/COVISA/SEABEVS), Marcelo Antunes Failla (GISA/CEInfo/CIS/SERMAP)###


install.packages("haven")   # Para ler arquivos .sav
install.packages("survey")  # O pacote survey realiza a análise de amostras complexas
install.packages("srvyr")  # O pacote srvyr baseado no 'survey' realiza a análise no padrão do tidyverse
install.packages("labelled")
install.packages("flextable")
install.packages("officer")

library(tidyverse)
library(haven)
library(survey)
library(srvyr)
library(purrr)
library(tibble)
library(labelled)
library(flextable)
library(officer)

#define a pasta onde estão os arquivos
setwd("c:/ISA_Capital")

#ler o arquivo de dados do SPSS
dados <- read_sav("ISACapital17042025TreinamentoComPesosExcluiVar.sav")

#exclui os registros com o peso final em branco
dados2 <- dados %>%
  mutate(
    FX_ETARIA = cut(
      IDADECALC,
      breaks = c(10, 20, 30, 40, 50, 60, 70, 80, 90, Inf),
      right = FALSE,
      labels = c(
        "10-19", "20-29", "30-39", "40-49",
        "50-59", "60-69", "70-79", "80-89", "90 ou +"
      )
    )
  ) %>%
  filter(!is.na(pesofinal)) 




## gera um data frame com os nomes das variáveis
# Função segura para extrair 1º label (ou NA se não houver)
extrair_label <- function(x) {
  lbl <- attr(x, "label")
  if (is.null(lbl)) return(NA_character_)
  if (length(lbl) > 1) return(paste(lbl, collapse = " | "))
  return(as.character(lbl))
}

# Função segura para extrair val_labels
extrair_val_labels <- function(x) {
  vl <- val_labels(x)
  if (is.null(vl)) return(NA_character_)
  paste(names(vl), "=", vl, collapse = "; ")
}

# Aplica ao banco
variaveis_info <- tibble(
  nome_variavel   = names(dados),
  label_variavel  = map_chr(dados, extrair_label),
  categorias      = map_chr(dados, extrair_val_labels),
  tipo_variavel   = map_chr(dados, ~ class(.)[1])
)


### Gera o arquivo CSV com as informações das variáveis
write.csv2(variaveis_info, "dicionario_variaveis.csv", row.names = FALSE, fileEncoding = "Windows-1252")


################################################################

# Define o desenho do estudo
des <- svydesign(
  id = ~cod_setor,
  strata = ~cod_coord,
  weights = ~pesofinal,
  data = dados2,
  nest = TRUE
)

# Cria versão srvyr do design
des_srvyr <- as_survey_design(des)

###################################################################################
########################## FAZ A TABULAÇÃO DAS VARIÁVEIS ##########################

# Exemplo com sexo
# o comando 'mutate(sexo = as_factor(b08_sexo))' recupera a variável b08_sexo do banco de dados
# e transforma em fator com a descrição do código que já está no arquivo .sav


freq_sexo <- des_srvyr %>%
  filter(!is.na(b07_sexo_geral)) %>%
  mutate(sexo = as_factor(b07_sexo_geral)) %>%
  group_by(sexo) %>%
  summarise(prop = survey_mean(vartype = "ci", proportion = TRUE)) %>%
  mutate(across(c(prop, prop_low, prop_upp), ~ . * 100)) %>%
  rename(Proporção = prop, IC_inf = prop_low, IC_sup = prop_upp)


freq_morbidade2semanas <- des_srvyr %>%
  filter(!is.na(c101_morbidade2semanas)) %>%
  mutate(morbidade2semanas = as_factor(c101_morbidade2semanas)) %>%
  group_by(morbidade2semanas) %>%
  summarise(prop = survey_mean(vartype = "ci", proportion = TRUE)) %>%
  mutate(across(c(prop, prop_low, prop_upp), ~ . * 100)) %>%
  rename(Proporção = prop, IC_inf = prop_low, IC_sup = prop_upp)

freq_hipertensao <- des_srvyr %>%
  filter(!is.na(c201a_hipertensao)) %>%
  mutate(hipertensao = as_factor(c201a_hipertensao)) %>%
  group_by(hipertensao) %>%
  summarise(prop = survey_mean(vartype = "ci", proportion = TRUE)) %>%
  mutate(across(c(prop, prop_low, prop_upp), ~ . * 100)) %>%
  rename(Proporção = prop, IC_inf = prop_low, IC_sup = prop_upp)

freq_diabetes <- des_srvyr %>%
  filter(!is.na(c202a_diabetes)) %>%
  mutate(diabetes = as_factor(c202a_diabetes)) %>%
  group_by(diabetes) %>%
  summarise(prop = survey_mean(vartype = "ci", proportion = TRUE)) %>%
  mutate(across(c(prop, prop_low, prop_upp), ~ . * 100)) %>%
  rename(Proporção = prop, IC_inf = prop_low, IC_sup = prop_upp)

freq_infarto <- des_srvyr %>%
  filter(!is.na(c203a_infarto)) %>%
  mutate(infarto = as_factor(c203a_infarto)) %>%
  group_by(infarto) %>%
  summarise(prop = survey_mean(vartype = "ci", proportion = TRUE)) %>%
  mutate(across(c(prop, prop_low, prop_upp), ~ . * 100)) %>%
  rename(Proporção = prop, IC_inf = prop_low, IC_sup = prop_upp)

freq_avc <- des_srvyr %>%
  filter(!is.na(c205a_avc)) %>%
  mutate(avc = as_factor(c205a_avc)) %>%
  group_by(avc) %>%
  summarise(prop = survey_mean(vartype = "ci", proportion = TRUE)) %>%
  mutate(across(c(prop, prop_low, prop_upp), ~ . * 100)) %>%
  rename(Proporção = prop, IC_inf = prop_low, IC_sup = prop_upp)


freq_cancer <- des_srvyr %>%
  filter(!is.na(c206a_cancer)) %>%
  mutate(cancer = as_factor(c206a_cancer)) %>%
  group_by(cancer) %>%
  summarise(prop = survey_mean(vartype = "ci", proportion = TRUE)) %>%
  mutate(across(c(prop, prop_low, prop_upp), ~ . * 100)) %>%
  rename(Proporção = prop, IC_inf = prop_low, IC_sup = prop_upp)


freq_fx_etaria <- des_srvyr %>%
  filter(!is.na(FX_ETARIA)) %>%
  mutate(hipertensao = as_factor(FX_ETARIA)) %>%
  group_by(FX_ETARIA) %>%
  summarise(prop = survey_mean(vartype = "ci", proportion = TRUE)) %>%
  mutate(across(c(prop, prop_low, prop_upp), ~ . * 100)) %>%
  rename(Proporção = prop, IC_inf = prop_low, IC_sup = prop_upp)


#### GERAR DOCUMENTO COM AS TABELAS DE FREQUÊNCIA


# 🧩 Função robusta para gerar flextable
formatar_tabela <- function(df, titulo) {
  if (!is.data.frame(df) || nrow(df) == 0) {
    warning(paste("Tabela inválida ou vazia:", titulo))
    return(flextable(data.frame(Mensagem = "Tabela não disponível")))
  }
  df <- as.data.frame(df)  # garante compatibilidade
  flextable(df) %>%
    set_caption(caption = titulo) %>%
    autofit() %>%
    set_table_properties(width = 0.66, layout = "autofit")  # ⇐ Reduz largura para 66%
}

# 📄 Cria o documento Word
doc <- read_docx() %>%
  body_add_par("Tabelas de Frequência do Inquérito de Saúde", style = "heading 1") %>%
  body_add_flextable(formatar_tabela(freq_sexo, "Tabela 1 - Frequência por Sexo")) %>%
  body_add_par("", style = "Normal") %>%
  body_add_flextable(formatar_tabela(freq_morbidade2semanas, "Tabela 2 - Morbidade nas Últimas 2 Semanas")) %>%
  body_add_par("", style = "Normal") %>%
  body_add_flextable(formatar_tabela(freq_hipertensao, "Tabela 4 - Hipertensão")) %>%
  body_add_par("", style = "Normal") %>%
  body_add_flextable(formatar_tabela(freq_diabetes, "Tabela 5 - Diabetes")) %>%
  body_add_par("", style = "Normal") %>%
  body_add_flextable(formatar_tabela(freq_infarto, "Tabela 6 - Infarto")) %>%
  body_add_par("", style = "Normal") %>%
  body_add_flextable(formatar_tabela(freq_avc, "Tabela 7 - AVC")) %>%
  body_add_par("", style = "Normal") %>%
  body_add_flextable(formatar_tabela(freq_cancer, "Tabela 8 - Câncer")) %>%
  body_add_par("", style = "Normal") %>%
  body_add_flextable(formatar_tabela(freq_fx_etaria, "Tabela 9 - Frequência por Faixa Etária"))

# 💾 Salva o documento
print(doc, target = "frequencias_ISA_Capital.docx")


#################################################################
################## TABELAS CRUZADAS #############################
################################################################

# Cria tabela cruzada de sexo x morbidade 2 semanas
tab_cross_sexo_morbidade2semanas <- des_srvyr %>%
  filter(!is.na(b07_sexo_geral)) %>%
  filter(!is.na(c101_morbidade2semanas)) %>%
  mutate(
    sexo = as_factor(b07_sexo_geral),
    morbidade2semanas = as_factor(c101_morbidade2semanas)
  ) %>%
  group_by(sexo, morbidade2semanas) %>%
  summarise(
    prop = survey_mean(proportion = TRUE, vartype = "ci"),
    .groups = "drop"
  ) %>%
  mutate(across(c(prop, prop_low, prop_upp), ~ . * 100)) %>%
  rename(
    Proporção = prop,
    IC_inf = prop_low,
    IC_sup = prop_upp
  )


# Formatar tabela no padrão do STATA
tab_cruzada_formatada_sexo_morbidade2semanas <- tab_cross_sexo_morbidade2semanas %>%
  mutate(
    categoria = glue("{round(Proporção, 1)}% [{round(IC_inf, 1)}–{round(IC_sup, 1)}]")
  ) %>%
  select(sexo, morbidade2semanas, categoria) %>%
  pivot_wider(
    names_from = morbidade2semanas,
    values_from = categoria
  )

## Formatar a tabela para o flextable
flextable(tab_cruzada_formatada_sexo_morbidade2semanas) %>%
  set_caption("Tabela cruzada: Sexo x Morbidade nas últimas 2 semanas") %>%
  autofit()



# Cria tabela cruzada de hipertensão x faixa etaria
tab_cross_hipertensao_faixa_etaria <- des_srvyr %>%
  filter(!is.na(FX_ETARIA)) %>%
  filter(!is.na(c201a_hipertensao)) %>%
  mutate(
    hipertensao = as_factor(c201a_hipertensao)
  ) %>%
  group_by(FX_ETARIA, hipertensao) %>%
  summarise(
    prop = survey_mean(proportion = TRUE, vartype = "ci"),
    .groups = "drop"
  ) %>%
  mutate(across(c(prop, prop_low, prop_upp), ~ . * 100)) %>%
  rename(
    Proporção = prop,
    IC_inf = prop_low,
    IC_sup = prop_upp
  )

# Formatar tabela no padrão do STATA
tab_cruzada_formatada_hipertensao_faixa_etaria <- tab_cross_hipertensao_faixa_etaria %>%
  mutate(
    categoria = glue("{round(Proporção, 1)}% [{round(IC_inf, 1)}–{round(IC_sup, 1)}]")
  ) %>%
  select(hipertensao, FX_ETARIA, categoria) %>%
  pivot_wider(
    names_from = hipertensao,
    values_from = categoria
  )

## Formatar a tabela para o flextable
flextable(tab_cruzada_formatada_hipertensao_faixa_etaria) %>%
  set_caption("Tabela cruzada: Hipertensão x Faixa Etária") %>%
  autofit()



# Cria tabela cruzada de infarto x faixa etaria
tab_cross_infarto_faixa_etaria <- des_srvyr %>%
  filter(!is.na(FX_ETARIA)) %>%
  filter(!is.na(c203a_infarto)) %>%
  mutate(
    infarto = as_factor(c203a_infarto)
  ) %>%
  group_by(FX_ETARIA, infarto) %>%
  summarise(
    prop = survey_mean(proportion = TRUE, vartype = "ci"),
    .groups = "drop"
  ) %>%
  mutate(across(c(prop, prop_low, prop_upp), ~ . * 100)) %>%
  rename(
    Proporção = prop,
    IC_inf = prop_low,
    IC_sup = prop_upp
  )

# Formatar tabela no padrão do STATA
tab_cruzada_formatada_infarto_faixa_etaria <- tab_cross_infarto_faixa_etaria %>%
  mutate(
    categoria = glue("{round(Proporção, 1)}% [{round(IC_inf, 1)}–{round(IC_sup, 1)}]")
  ) %>%
  select(infarto, FX_ETARIA, categoria) %>%
  pivot_wider(
    names_from = infarto,
    values_from = categoria
  )

## Formatar a tabela para o flextable
flextable(tab_cruzada_formatada_infarto_faixa_etaria) %>%
  set_caption("Tabela cruzada: Infarto x Faixa Etária") %>%
  autofit()

## 
# Cria tabela cruzada de CRS x morbidade 2 semanas
tab_cross_CRS_morbidade2semanas <- des_srvyr %>%
  filter(!is.na(cod_coord)) %>%
  filter(!is.na(c101_morbidade2semanas)) %>%
  mutate(
    CRS = as_factor(cod_coord),
    morbidade2semanas = as_factor(c101_morbidade2semanas)
  ) %>%
  group_by(CRS, morbidade2semanas) %>%
  summarise(
    prop = survey_mean(proportion = TRUE, vartype = "ci"),
    .groups = "drop"
  ) %>%
  mutate(across(c(prop, prop_low, prop_upp), ~ . * 100)) %>%
  rename(
    Proporção = prop,
    IC_inf = prop_low,
    IC_sup = prop_upp
  )


# Formatar tabela no padrão do STATA
tab_cruzada_formatada_CRS_morbidade2semanas <- tab_cross_CRS_morbidade2semanas %>%
  mutate(
    categoria = glue("{round(Proporção, 1)}% [{round(IC_inf, 1)}–{round(IC_sup, 1)}]")
  ) %>%
  select(CRS, morbidade2semanas, categoria) %>%
  pivot_wider(
    names_from = morbidade2semanas,
    values_from = categoria
  )

## Formatar a tabela para o flextable
flextable(tab_cruzada_formatada_CRS_morbidade2semanas) %>%
  set_caption("Tabela cruzada: CRS x Morbidade nas últimas 2 semanas") %>%
  autofit()



## Salvar no arquivo .DOCX
# 1. Criar flextables com legenda
ftx1 <- flextable(tab_cruzada_formatada_sexo_morbidade2semanas) %>%
  set_caption("Tabela 1. Sexo x Morbidade nas últimas 2 semanas") %>%
  autofit()

ftx2 <- flextable(tab_cruzada_formatada_hipertensao_faixa_etaria) %>%
  set_caption("Tabela 2. Hipertensão x Faixa Etária") %>%
  autofit()

ftx3 <- flextable(tab_cruzada_formatada_infarto_faixa_etaria) %>%
  set_caption("Tabela 3. Infarto x Faixa Etária") %>%
  autofit()

ftx4 <- flextable(tab_cruzada_formatada_CRS_morbidade2semanas) %>%
  set_caption("Tabela 4. CRS x Morbidade nas últimas 2 semanas") %>%
  autofit()

# 2. Criar documento Word
doc <- read_docx()

# 3. Adicionar as tabelas ao documento
doc <- doc %>%
  body_add_par("Tabelas Cruzadas do ISA Capital", style = "heading 1") %>%
  body_add_flextable(ftx1) %>%
  body_add_par("", style = "Normal") %>%
  body_add_flextable(ftx2) %>%
  body_add_par("", style = "Normal") %>%
  body_add_flextable(ftx3) %>%
  body_add_par("", style = "Normal") %>%
  body_add_flextable(ftx4)

# 4. Salvar o documento
print(doc, target = "tabelas_cruzadas_ISA_Capital.docx")
# Fim do script
