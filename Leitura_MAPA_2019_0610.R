########### IMPACTO MAPA
########## Script Leitura REDCap adaptado
######### Jacqueline Silva

library(tidyverse)
library(plyr)

### Load Data
caminho <- "Z:/Nucleo/Pesquisa/Andamento/IMPACTO_MAPA/Gerenciamento/Dado/Banco/"
data <- read.csv(paste(caminho,"IMPACTOMAPA_DATA.csv", sep=""), encoding = "UTF-8")


### Set labels and factors
source("Z:/Nucleo/Pesquisa/Andamento/IMPACTO_MAPA/Gerenciamento/Dado/Script/Codigo_REDCap.R", encoding = "UTF-8")


### Adjusting ID
# Setting as character
data$id <- as.character(data$id)

# Split site and patient id
id <- matrix(unlist(strsplit(data$id,split="-", fixed=TRUE)), byrow = T, ncol=2)

# Add ids in the dataset
data$siteid <- id[,1]
data$patientid <- id[,2]


### Read data dictionary
library(readxl)
dictionary <- read_excel("Z:/Nucleo/Pesquisa/Andamento/IMPACTO_MAPA/CRF/Dicionario_Variavel/Dicionario_2019_0301.xlsx",
                        sheet = 1, col_names = T)



### Split the datasets in three

# List variables of each dataset
var_hospital <- dictionary$variable[dictionary$`Form Name` %in% "Caracterização do hospital"]
var_patient1 <- dictionary$variable[dictionary$`Form Name` %in% c("Admissão", "Alta hospitalar")]
var_patient2 <- dictionary$variable[dictionary$`Form Name` %in% "Dados diários"]

# Split
hospital <- data[data$redcap_event_name %in% "Cadastro do hospital (Arm 1: Hospitais)", 
                 c(var_hospital, 'id', 'siteid','patientid', "caracterizao_do_hospital_complete")]

patient1 <- data[data$redcap_event_name != "Cadastro do hospital (Arm 1: Hospitais)" & 
                    is.na(data$redcap_repeat_instrument), 
                 c(var_patient1, 'id', 'siteid','patientid', 
                   "admisso_complete", "alta_hospitalar_complete")]

patient2 <- data[data$redcap_event_name != "Cadastro do hospital (Arm 1: Hospitais)" & 
                    !is.na(data$redcap_repeat_instrument), 
                 c(var_patient2, 'id', 'siteid','patientid',
                   "redcap_repeat_instance", "dados_dirios_716778_complete")]


### Get data of inclusion from REDCap logging
# Logging Dataset
logging <- read.csv(paste(caminho,"IMPACTOMAPA_Logging.csv", sep=""), sep=",", encoding = "UTF-8")

# Keep only action "Created Record"
logging <- subset(logging, substring(logging$Action, 1, 14) %in% "Created Record")

# Split characters of the column action
action <- matrix(unlist(strsplit(as.character(logging$Action),split=" ", fixed=TRUE)), byrow = T, ncol=10)

# Split the id
id <- matrix(unlist(strsplit(action[ ,3],split="-", fixed=TRUE)), byrow = T, ncol=2)

# Add ids in the logging dataset
logging$siteid <- id[ , 1]
logging$patientid <- id[ , 2]

# Rename date column
colnames(logging)[1] <- "dtinclusao"

# Clean logging dataset
logging <- logging[ , c(1, 5,6)]
logging$dtinclusao <- as.Date(as.character(logging$dtinclusao), "%Y-%m-%d")

logging <- logging %>% group_by(siteid, patientid)  %>% dplyr::mutate(maxima = max(dtinclusao))
logging <- logging[logging$dtinclusao == logging$maxima, ]


# Add date of inclusion in the patient1 dataset
patient1 <- merge(patient1, logging, by = c("siteid", "patientid"), all.x=T)

# Remove logging
rm(logging)

### Format dates
patient1$dtinclusao <- as.Date(as.character(patient1$dtinclusao), "%Y-%m-%d")
patient1$tempoinclusao <- Sys.Date() - patient1$dtinclusao
patient1$dtinter_ad <- as.Date(as.character(patient1$dtinter_ad), "%Y-%m-%d")
patient1$dtalta_ah <- as.Date(as.character(patient1$dtalta_ah), "%Y-%m-%d")
patient2$dt_dd <- as.Date(as.character(patient2$dt_dd))

### Format time
library(lubridate)
patient2$hrdisfun_dd <- hm(as.character(patient2$hrdisfun_dd), 
                           quiet = T, roll = F)
patient2$hratb_dd <- hm(as.character(patient2$hratb_dd), 
                           quiet = T, roll = F)

### Load site data
library(xlsx)
caminho <- "Z:/Nucleo/Pesquisa/Andamento/IMPACTO_MAPA/Gerenciamento/Centro/"
sites <- read.xlsx(paste(caminho,"IMPACTO_MAPA_Gerenciamento_Centros.xlsx", sep=""),
                   sheetName = "Gerenciamento", encoding = "UTF-8")
sites <- sites[, c("siteid", "Instituição", "Investigador.Principal", "Status.do.Centro", "Data.do.Treinamento")]

### Manter somente centros ativos
#sites <- sites[sites$Status.do.Centro %in% "ATIVO", ]

### Dias desde o treinamento
sites$tempo_treinamento <- Sys.Date() - sites$Data.do.Treinamento

# Juntar banco hospital com banco de gerenciamento de centros
hospital <- merge(hospital, sites[sites$Status.do.Centro %in% "ATIVO", ], by="siteid", all.y = T)

### Tempo de internação
patient1$tempointernacao <- as.numeric(patient1$dtalta_ah - patient1$dtinter_ad)

#### ordenando os dados diários, criando a variável "dia" e calculando o número de dias da data anterior 
patient2 <- as.data.frame(patient2 %>% group_by(id) %>% arrange(dt_dd) %>% 
                       dplyr::mutate(dia = dense_rank(dt_dd)) %>% 
                         group_by(id) %>%
                         dplyr::mutate(dt_anterior = dt_dd - lag(dt_dd, default = first(dt_dd)) )) 

### Pacientes com diferença de dias no banco diário maior que 1. Acho que precisa checar  
## patient2[patient2$dt_anterior > 1 | is.na(patient2$dt_anterior), c("id", "dt_dd", "dt_anterior", "dia")]

### Verificar se paciente tem disfuncao organica
patient2$disforganica <- factor(patient2$pasmax_dd < 90 | 
                                  patient2$pam_dd < 65 |
                                  patient2$satmaxox_dd < 90 |
                                  (patient2$consienc_dd != 15 & patient2$consienc_dd != "Não foi possível estimar") |
                                  patient2$ncreat_dd > 2 |
                                  patient2$nbilirr_dd > 2 |
                                  patient2$nplaq_dd < 100000 |
                                  patient2$vexlacta_dd > 18 |
                                  patient2$inrttpa_dd %in% "Sim",
                                levels = c(F,T),
                                labels = c("Não", "Sim"))
patient2$disforganica[is.na(patient2$disforganica)] <- "Não"



#patient2$disforganica[rowSums(is.na(patient2[, c("pasmax_dd", "pam_dd", "satminox_dd",
#                                           "consienc_dd", "ncreat_dd", "nbilirr_dd",
#                                           "nplaq_dd", "vexlacta_dd", "inrttpa_dd")])) == 9] <- NA

# #Conferir disfunção orgânica
# x <- patient2[patient2$disforganica %in% "Sim",
#               c("pasmax_dd", "pam_dd", "satminox_dd",
#                 "consienc_dd", "ncreat_dd", "nbilirr_dd",
#                 "nplaq_dd", "vexlacta_dd", "inrttpa_dd", "hrdisfun_dd",
#                 "redcap_repeat_instance","id")]
# View(x[is.na(x$hrdisfun_dd), ])
# View(x[x$id %in% "98-92", ])


# Pacientes com sepse
patient2$sepse <- factor(patient2$disforganica %in% "Sim" & patient2$qcausa_dd %in% "Piora clínica",levels = c(F,T),
                         labels = c("Não", "Sim") )

patient2$sepse[is.na(patient2$disforganica) | is.na(patient2$qcausa_dd)] <- NA


### Código do Renato para dizer que pacientes tem sepse
aux <- patient2 %>%
  group_by(siteid, patientid) %>%
  dplyr::summarise(sepse = factor(sum(sepse %in% "Sim") >= 1, 
                            levels=c("FALSE", "TRUE"), 
                            labels=c("Não", "Sim")), 
            dias_preenchidos = sum(n()))

dados <- merge(patient1,
               aux, by = c("siteid", "patientid"), all.x = T)


###################################
### Função maravilhosa do Lucas
# cria_lista <- function(x){
#   hup <- "c("
#   for(i in 1:length(x)){
#     if(i<length(x)) hup <- paste(hup, "'", x[i], "', ", sep="")
#     if(i==length(x)) hup <- paste(hup, "'", x[i], "')",sep="")
#   }
#   print(hup)
# }
# x <- scan(what = "char") ou outro vetor de texto.
# cria_lista(x)