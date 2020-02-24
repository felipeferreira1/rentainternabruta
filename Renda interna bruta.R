#Rotina para calcular a renda interna bruta
#Feito por: Felipe Simplício Ferreira
#última atualização: 24/02/2020


#Definindo diretórios a serem utilizados

getwd()
setwd("C:\\Users\\User\\Documents\\GitHub\\rentainternabruta")

#Carregando pacotes que serão utilizados
library(readxl)

#Carregando arquivo das contas trimestrais
tabela_ct = read_excel("Tab_Compl_CNT_4T18.xls", 
                       sheet = "Valores Correntes")

tabela_ct_v95 = read_excel("Tab_Compl_CNT_4T18.xls", 
                       sheet = "Val encad preços 95 com ajuste")

#Ajustando os dados
tabela_ct[2,2:15] = tabela_ct[3,2:15]
tabela_ct = tabela_ct[-1,]
tabela_ct = tabela_ct[-2,]
colnames(tabela_ct) = tabela_ct[1,]
tabela_ct = tabela_ct[-1,]
tabela_ct[,-1]=apply(tabela_ct[,-1],2,function(x)as.numeric(gsub(",",".",x)))

tabela_ct_v95[2,2:15] = tabela_ct_v95[3,2:15]
tabela_ct_v95 = tabela_ct_v95[-1,]
tabela_ct_v95 = tabela_ct_v95[-2,]
colnames(tabela_ct_v95) = tabela_ct_v95[1,]
tabela_ct_v95 = tabela_ct_v95[-1,]
tabela_ct_v95[,-1]=apply(tabela_ct_v95[,-1],2,function(x)as.numeric(gsub(",",".",x)))

#Filtrando dados anuais
tabela_ca = tabela_ct[FALSE,]
for (i in seq(1,dim(tabela_ct)[1],5)){
  tabela_ca[i,] = tabela_ct[i,] #Coletando os dados anuais, que estão logo após os trimestrais
}
tabela_ca = na.omit(tabela_ca)

tabela_ca_v95 = tabela_ct_v95[FALSE,]
for (i in seq(4,dim(tabela_ct_v95)[1],4)){
  for (j in 2:dim(tabela_ct_v95)[2]){
  tabela_ca_v95[i,j] = (tabela_ct_v95[i-3,j] + tabela_ct_v95[i-2,j] + tabela_ct_v95[i-1,j] + tabela_ct_v95[i,j])/4 #Calculando média dos 4 trimestres do ano
  }
}
tabela_ca_v95$Período = tabela_ct_v95$Período #Usando coluna de data para não excluir as colunas sem data
tabela_ca_v95 = na.omit(tabela_ca_v95)
tabela_ca_v95$Período = tabela_ca$Período[-1] #Ajeitando a data

#Filtrando dados trimestrais
tabela_ct = tabela_ct[-c(seq(1,dim(tabela_ct)[1],5)), ]

#Definindo séries
#Trimestrais a valores correntes
pib_t = tabela_ct[,c('Período', 'PIB')]
consumo_familias_t = tabela_ct[,c('Período', 'Consumo das Famílias')]
consumo_governo_t = tabela_ct[,c('Período', 'Consumo do Governo')]
fbkf_t = tabela_ct[,c('Período', 'Formação Bruta de Capital Fixo')]
export_t = tabela_ct[,c('Período', 'Exportação')]
import_t = tabela_ct[,c('Período', 'Importação')]
absorv_dom_t = tabela_ct[,c('Período', 'PIB')]
absorv_dom_t$PIB = consumo_familias_t$`Consumo das Famílias` + consumo_governo_t$`Consumo do Governo` + fbkf_t$`Formação Bruta de Capital Fixo`
colnames(absorv_dom_t) = c('Período', 'Absorção Doméstica')
var_est_t = tabela_ct[,c('Período', 'Variação de Estoques')]

#Trimestrais a valores de 95
pib_t_v95 = tabela_ct_v95[,c('Período', 'PIB')]
consumo_familias_t_v95 = tabela_ct_v95[,c('Período', 'Consumo das Famílias')]
consumo_governo_t_v95 = tabela_ct_v95[,c('Período', 'Consumo do Governo')]
fbkf_t_v95 = tabela_ct_v95[,c('Período', 'Formação Bruta de Capital Fixo')]
export_t_v95 = tabela_ct_v95[,c('Período', 'Exportação')]
import_t_v95 = tabela_ct_v95[,c('Período', 'Importação')]
absorv_dom_t_v95 = tabela_ct_v95[,c('Período', 'PIB')]
absorv_dom_t_v95$PIB = consumo_familias_t_v95$`Consumo das Famílias` + consumo_governo_t_v95$`Consumo do Governo` + fbkf_t_v95$`Formação Bruta de Capital Fixo`
colnames(absorv_dom_t_v95) = c('Período', 'Absorção Doméstica')

#Anuais a valores de 95
pib_a_v95 = tabela_ca_v95[,c('Período', 'PIB')]
consumo_familias_a_v95 = tabela_ca_v95[,c('Período', 'Consumo das Famílias')]
consumo_governo_a_v95 = tabela_ca_v95[,c('Período', 'Consumo do Governo')]
fbkf_a_v95 = tabela_ca_v95[,c('Período', 'Formação Bruta de Capital Fixo')]
export_a_v95 = tabela_ca_v95[,c('Período', 'Exportação')]
import_a_v95 = tabela_ca_v95[,c('Período', 'Importação')]
absorv_dom_a_v95 = tabela_ca_v95[,c('Período', 'PIB')]
absorv_dom_a_v95$PIB = consumo_familias_a_v95$`Consumo das Famílias` + consumo_governo_a_v95$`Consumo do Governo` + fbkf_a_v95$`Formação Bruta de Capital Fixo`
colnames(absorv_dom_a_v95) = c('Período', 'Absorção Doméstica')

#Calculo da variação real anual
var_r_pib_t = pib_t_v95[FALSE, ]
for (i in 1:dim(pib_t_v95)[1]){
  for (j in 1:dim(tabela_ca)[1]){
    if (i <= 4)
      var_r_pib_t[i,2] = (pib_t_v95[i,2]/(tabela_ca$PIB[j]/4)-1)*100
    else
      var_r_pib_t[i,2] = (pib_t_v95[i,2]/(mean(tabela_ct_v95$PIB[(j-4):(j-1)]))-1)*100
    #Como calcular pra todos?
  print(i)
  print("/")
  print(j)
  break
  }
}

var_r_pib_t = pib_t_v95[FALSE, ]
for (j in 1:dim(tabela_ca)[1]){
  for (i in 1:dim(pib_t_v95)[1]){
    if (i <= 4)
      var_r_pib_t[i,2] = (pib_t_v95[i,2]/(tabela_ca$PIB[j]/4)-1)*100
    else
      var_r_pib_t[i,2] = (pib_t_v95[i,2]/(mean(tabela_ct_v95$PIB[(j-4):(j-1)]))-1)*100
    #Como calcular pra todos?
    print(i)
    print("/")
    print(j)
    break
    }
}
var_r_pib_t$Período = pib_t_v95$Período




