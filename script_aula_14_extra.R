##### Atividade aula 14 - extra - banco 2 - equivalente ao SINASC ######
##### Na branch main inserir os comandos e salvar o script com o nome script_aula_14_extra#####

# Tarefa 1: Leitura do banco de dados banco 2 = SINASC.csv com o nome de dados_aula14
# Ler o arquivo, verificar estrutura dos dados e dar uma olhada nos dados
dados_aula14 = read.csv("banco 2 SINASC.csv",
                        header = T,
                        sep = ";",
                        stringsAsFactors = F)
dim(dados_aula14)
names(dados_aula14)
str(dados_aula14)
summary(dados_aula14)
View(dados_aula14)

# Ao terminar a Tarefa 1 commit com a mensagem " script - tarefa 1" e envie para o repositório Aula_14_Extra


# Tarefa 2: Manipulação dos dados
# Padronizar as categorias SEXO_PROPRIETARIO para Masculino e Feminino
# Atribuir legendas para a variável TIPO_VEICULO, sendo 1: Carro e 2: Moto
# Criar uma nova variável em dados_aula14 F_IDADE categorizando as idades em: 22 a 34, 35 a 45
dados_aula14$SEXO_PROPRIETARIO = toupper(dados_aula14$SEXO_PROPRIETARIO)
dados_aula14$SEXO_PROPRIETARIO = factor(dados_aula14$SEXO_PROPRIETARIO,
                                        levels = c("MASCULINO", "FEMININO"),
                                        labels = c("Masculino","Feminino"))

dados_aula14$TIPO_VEICULO = factor(dados_aula14$TIPO_VEICULO,
                                    levels = c(1, 2),
                                    labels = c("Carro", "Moto"))

dados_aula14$F_IDADE = cut(dados_aula14$IDADE_PROPRIETARIO,
                            breaks = c(22, 34, 45),
                            labels = c("22 a 34", "35 a 45"),
                            include.lowest = TRUE)

# Verificando
table(dados_aula14$SEXO_PROPRIETARIO)
table(dados_aula14$TIPO_VEICULO)
table(dados_aula14$F_IDADE)

# Ao terminar a Tarefa 2 commit com a mensagem " script - tarefa 1 a 2" e envie para o repositório Aula_14_Extra


# Tarefa 3: Leitura do banco de dados Tabela_PAM.csv (com o nome tabela_pam) e:
# agregar ao banco dados_aula14 as informações de VALOR_P10 e VALOR_P90
# criar a variável PAM (somente quando TIPO_VEICULO = "Carro"), de acordo com IDADE_PROPRIETARIO e SEXO_PROPRIETARIO, com as seguintes categorias:
# PAM = "PIC", se VALOR_VEICULO < VALOR_P10; "AIC", se VALOR_P10 <= VALOR_VEICULO <= VALOR_P90; "GIC", se VALOR_VEICULO > VALOR_P90
tabela_pam = read.csv("Tabela_PAM.csv",
                      header = T,
                      sep = ";",
                      stringsAsFactors = F)
str(tabela_pam)

# Agregar VALOR_P10 e VALOR_P90 ao banco dados_aula14
# usando IDADE_PROPRIETARIO e SEXO_PROPRIETARIO
chave_dados = paste(dados_aula14$IDADE_PROPRIETARIO,
                     dados_aula14$SEXO_PROPRIETARIO,
                     sep = "_")

chave_pam = paste(tabela_pam$IDADE_PROPRIETARIO,
                   tabela_pam$SEXO_PROPRIETARIO,
                   sep = "_")

posicao = match(chave_dados, chave_pam)

dados_aula14$VALOR_P10 = tabela_pam$VALOR_P10[posicao]
dados_aula14$VALOR_P90 = tabela_pam$VALOR_P90[posicao]

# Criar a variável PAM
dados_aula14$PAM = NA_character_

# PIC: valor do veículo abaixo do P10
dados_aula14$PAM[
  dados_aula14$TIPO_VEICULO == "Carro" &
    !is.na(dados_aula14$VALOR_VEICULO) &
    !is.na(dados_aula14$VALOR_P10) &
    dados_aula14$VALOR_VEICULO < dados_aula14$VALOR_P10
] = "PIC"

# AIC: valor do veículo entre P10 e P90
dados_aula14$PAM[
  dados_aula14$TIPO_VEICULO == "Carro" &
    !is.na(dados_aula14$VALOR_VEICULO) &
    !is.na(dados_aula14$VALOR_P10) &
    !is.na(dados_aula14$VALOR_P90) &
    dados_aula14$VALOR_VEICULO >= dados_aula14$VALOR_P10 &
    dados_aula14$VALOR_VEICULO <= dados_aula14$VALOR_P90
] = "AIC"

# GIC: valor do veículo acima do P90
dados_aula14$PAM[
  dados_aula14$TIPO_VEICULO == "Carro" &
    !is.na(dados_aula14$VALOR_VEICULO) &
    !is.na(dados_aula14$VALOR_P90) &
    dados_aula14$VALOR_VEICULO > dados_aula14$VALOR_P90
] = "GIC"

head(dados_aula14)
table(dados_aula14$PAM, useNA = "ifany")

# Ao terminar a Tarefa 3 commit com a mensagem " script - tarefa 1 a 3" e envie para o repositório Aula_14_Extra

 
# Tarefa 4: Criar o banco de dados BACO_AULA14_RJ, POR MUNICÍPIO, com as seguintes variáveis listadas abaixo. 
# Variáveis que se referem a medidas de posição e de dispersão devem ser calculadas sem considerar NAs

# Atenção: a 1a linha do banco deve ser da UF 33
# ANO: 2025
# NIVEL: UF ou MUNICIPIO
# CODIGO: código do municipio (ou da UF)
# TVV: total de veiculos vendidos
# TVRC: total de vendas com registros completos nas 5 variáveis originais de banco 2 = SINASC
# TVVF: total de veículos vendidos para mulher
# TVVM: total de veículos vendidos para homem
# TVCF: total de carros vendidos para mulheres
# TVCM: total de carros vendidos para homens
# TVMF: total de motos vendidas para mulheres
# TVMM: total de motos vendidas para homens
# TVC_22_34: total de carros vendidos para pessoas na faixa etária de 22 a 34 anos
# TVC_35_45: total de carros vendidos para pessoas na faixa etária de 35 a 45 anos
# IMVCF: idade média das mulheres proprietárias de veículo carro 
# DPVCF: desvio-padrão das idades das mulheres proprietárias de veículo carro
# IVCF_P25: percentil 25 das idades das mulheres proprietárias de veículo carro
# IVCF_P50: percentil 50 das idades das mulheres proprietárias de veículo carro
# IVCF_P75: percentil 75 das idades das mulheres proprietárias de veículo carro
# IMVMM: idade média dos homens proprietários de veículo moto 
# DPVMM: desvio-padrão das idades dos homens proprietários de veículo moto
# IVMM_P25: percentil 25 das idades dos homens proprietários de veículo moto
# IVMM_P50: percentil 50 das idades dos homens proprietários de veículo moto
# IVMM_P75: percentil 75 das idades dos homens proprietários de veículo moto
# TPIC: total de compradores com perfil PIC
# TAIC: total de compradores com perfil AIC
# TGIC: total de compradores com perfil GIC
# Função para calcular as estatísticas
calcula_resumo = function(dados, nivel, codigo) {
  
  # Mulheres + Carro
  mulheres_carro = dados[
    as.character(dados$SEXO_PROPRIETARIO) == "Feminino" &
      as.character(dados$TIPO_VEICULO) == "Carro",
  ]
  
  # Homens + Moto
  homens_moto = dados[
    as.character(dados$SEXO_PROPRIETARIO) == "Masculino" &
      as.character(dados$TIPO_VEICULO) == "Moto",
  ]
  
  # Função para percentis
  p25 = function(x) {
    if (sum(!is.na(x)) == 0) return(NA)
    quantile(x, probs = 0.25, na.rm = TRUE, names = FALSE)
  }
  
  p50 = function(x) {
    if (sum(!is.na(x)) == 0) return(NA)
    quantile(x, probs = 0.50, na.rm = TRUE, names = FALSE)
  }
  
  p75 = function(x) {
    if (sum(!is.na(x)) == 0) return(NA)
    quantile(x, probs = 0.75, na.rm = TRUE, names = FALSE)
  }
  
  data.frame(
    ANO = 2025,
    NIVEL = nivel,
    CODIGO = codigo,
    TVV = nrow(dados),
    TVRC = sum(complete.cases(
      dados[, c("MUNICIPIO",
                "SEXO_PROPRIETARIO",
                "IDADE_PROPRIETARIO",
                "TIPO_VEICULO",
                "VALOR_VEICULO")]
    )),
    TVVF = sum(as.character(dados$SEXO_PROPRIETARIO) == "Feminino",
               na.rm = TRUE),
    TVVM = sum(as.character(dados$SEXO_PROPRIETARIO) == "Masculino",
               na.rm = TRUE),
    
    TVCF = sum(
      as.character(dados$SEXO_PROPRIETARIO) == "Feminino" &
        as.character(dados$TIPO_VEICULO) == "Carro",
      na.rm = TRUE
    ),
    TVCM = sum(
      as.character(dados$SEXO_PROPRIETARIO) == "Masculino" &
        as.character(dados$TIPO_VEICULO) == "Carro",
      na.rm = TRUE
    ),
    TVMF = sum(
      as.character(dados$SEXO_PROPRIETARIO) == "Feminino" &
        as.character(dados$TIPO_VEICULO) == "Moto",
      na.rm = TRUE
    ),
    TVMM = sum(
      as.character(dados$SEXO_PROPRIETARIO) == "Masculino" &
        as.character(dados$TIPO_VEICULO) == "Moto",
      na.rm = TRUE
    ),
    TVC_22_34 = sum(
      as.character(dados$TIPO_VEICULO) == "Carro" &
        as.character(dados$F_IDADE) == "22 a 34",
      na.rm = TRUE
    ),
    TVC_35_45 = sum(
      as.character(dados$TIPO_VEICULO) == "Carro" &
        as.character(dados$F_IDADE) == "35 a 45",
      na.rm = TRUE
    ),
    # Mulheres + Carro
    IMVCF = ifelse(
      sum(!is.na(mulheres_carro$IDADE_PROPRIETARIO)) > 0,
      mean(mulheres_carro$IDADE_PROPRIETARIO, na.rm = TRUE),
      NA
    ),
    DPVCF = ifelse(
      sum(!is.na(mulheres_carro$IDADE_PROPRIETARIO)) > 1,
      sd(mulheres_carro$IDADE_PROPRIETARIO, na.rm = TRUE),
      NA
    ),
    IVCF_P25 = p25(mulheres_carro$IDADE_PROPRIETARIO),
    IVCF_P50 = p50(mulheres_carro$IDADE_PROPRIETARIO),
    IVCF_P75 = p75(mulheres_carro$IDADE_PROPRIETARIO),
    # Homens + Moto
    IMVMM = ifelse(
      sum(!is.na(homens_moto$IDADE_PROPRIETARIO)) > 0,
      mean(homens_moto$IDADE_PROPRIETARIO, na.rm = TRUE),
      NA
    ),
    DPVMM = ifelse(
      sum(!is.na(homens_moto$IDADE_PROPRIETARIO)) > 1,
      sd(homens_moto$IDADE_PROPRIETARIO, na.rm = TRUE),
      NA
    ),
    IVMM_P25 = p25(homens_moto$IDADE_PROPRIETARIO),
    IVMM_P50 = p50(homens_moto$IDADE_PROPRIETARIO),
    IVMM_P75 = p75(homens_moto$IDADE_PROPRIETARIO),
    # Perfil PAM
    TPIC = sum(dados$PAM == "PIC", na.rm = TRUE),
    TAIC = sum(dados$PAM == "AIC", na.rm = TRUE),
    TGIC = sum(dados$PAM == "GIC", na.rm = TRUE)
  )
}

BANCO_UF = calcula_resumo(
  dados_aula14,
  nivel = "UF",
  codigo = 33
)

municipios = unique(dados_aula14$MUNICIPIO)

BANCO_MUNICIPIOS = do.call(
  rbind,
  lapply(municipios, function(m) {
    calcula_resumo(
      dados_aula14[dados_aula14$MUNICIPIO == m, ],
      nivel = "MUNICIPIO",
      codigo = m
    )
  })
)

BANCO_AULA14_RJ = rbind(
  BANCO_UF,
  BANCO_MUNICIPIOS
)

str(BANCO_AULA14_RJ)
head(BANCO_AULA14_RJ)
View(BANCO_AULA14_RJ)

# Ao terminar a Tarefa 4 commit com a mensagem " script - tarefa 1 a 4" e envie para o repositório Aula_14_Extra

# Tarefa 5: Exportar o banco de dados BANCO_AULA14_RJ com o nome BANCO_AULA14_RJ.csv
write.csv(BANCO_AULA14_RJ,
          "BANCO_AULA14_RJ.csv",
          row.names = FALSE)

file.exists("BANCO_AULA14_RJ.csv")

# Ao terminar a Tarefa 5 commit com a mensagem "dados e script - Etapa 2" e envie para o repositório Aula_14_Extra
