#
# Previsão de pacientes estarem com dengue ou não apartir de seus respectivos sintomas, utilizando-se do teorema de Bayes
#

#install.packages("readr")
#install.packages("dplyr")

library(readr)
library(dplyr)

search()

# set path
setwd("/home/bararaq/Documents/Git/R/DataScienceAlgorithms")
path <- file.path("Datasets", "caso-dengue2018.csv")

dengue <- read_csv2(path)

class(dengue)
dim(dengue)
names(dengue)
glimpse(dengue)

head(dengue$tp_classificacao_final, n = 6)
table(dengue$tp_classificacao_final)
summary(dengue)

columnsToEvaluate <- c('notificacao_ano', 'nu_idade', 'tp_sexo', 'tp_gestante', 'tp_raca_cor', 'tp_escolaridade', 'febre', 'mialgia', 'cefaleia', 'vomito', 'nausea', 
                       'dor_costas', 'conjutivite', 'artrite', 'artralgia', 'tp_classificacao_final')
  
dengue2 <- select(dengue, columnsToEvaluate)

glimpse(dengue2)

summary(dengue2)

dengue2$tp_sexo <- ifelse(dengue2$tp_sexo == "M", 0, 1)

dengue3 <- dengue2[complete.cases(dengue2$tp_sexo), ]

dengue3 <- dengue3 %>%
  mutate_each(factor)
  
summary(dengue3)

# renaming the dataset cleaned/munging and creating a new variable (primary key) in the dataset to propose the creation a test data.
dengue_cleaned = dengue3 %>%
  mutate(id = row_number())

# 
rm(dengue, dengue2, dengue3)

train = dengue_cleaned %>%
  sample_frac(size = 0.8)

test = anti_join(dengue_cleaned, train, by = 'id')

print(paste0('train data: ', nrow(train), ' | test data: ', nrow(test)))

# importing NavyBayes
#install.packages("naivebayes")
library(naivebayes)

locmodel <- naive_bayes(tp_classificacao_final ~ artralgia + artrite+ cefaleia+ conjutivite+ dor_costas+ febre+ mialgia+ nausea+ notificacao_ano+ nu_idade+ tp_escolaridade+ tp_gestante+ tp_raca_cor+ tp_sexo+ vomito, data = train)
head(predict(locmodel, test[,0:sum(ncol(test)-2)], type = "prob"), n = 5)
head(predict(locmodel, test[,0:sum(ncol(test)-2)]), n = 5)

