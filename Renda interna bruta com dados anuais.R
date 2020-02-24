#Rotina para calcular a renda interna bruta
#Feito por: Felipe Simplício Ferreira
#última atualização: 06/12/2019


#Definindo diretórios a serem utilizados

getwd()
setwd("C:\\Users\\User\\Documents\\GitHub\\rentainternabruta")

#Carregando pacotes que serão utilizados
library(readxl)

#Carregando arquivo das contas trimestrais
tabela_ca_2000_2017= read_excel("dados.xlsx", sheet = "Anual_2000-2017 (ref2010)")
tabela_ct_1996_2018 = read_excel("dados.xlsx", sheet = "Trimestral_1996-2018 (ref2010)")

#valores correntes
arrumar_tabelas = function(df_entrada ,df_saida, qtd_colunas){
  df_saida = data.frame(df_entrada[,1], df_entrada[,qtd_colunas])
  colnames(df_saida) = df_saida[1,]
  df_saida = df_saida[-1,]
  df_saida[,-1] = apply(df_saida[,-1],2,function(x)as.numeric(gsub(",",".",x)))
  return(df_saida)
}

tabela_ca_2000_2017_vcorr = arrumar_tabelas(tabela_ca_2000_2017, tabela_ca_2000_2017_vcorr, 2:10)

separa_colunas = function(data, nome_coluna, df_entrada){
  df_saida = df_entrada[,c(data, nome_coluna)]
  return(df_saida)
}

pib_a_vcorr = separa_colunas('Período', 'PIB', tabela_ca_2000_2017_vcorr)
consumo_familias_a_vcorr = separa_colunas('Período', 'Consumo das Famílias', tabela_ca_2000_2017_vcorr)
consumo_governo_a_vcorr = separa_colunas('Período', 'Consumo do Governo', tabela_ca_2000_2017_vcorr)
fbkf_a_vcorr = separa_colunas('Período', 'Formação Bruta de Capital Fixo', tabela_ca_2000_2017_vcorr)
export_a_vcorr = separa_colunas('Período', 'Exportação', tabela_ca_2000_2017_vcorr)
import_a_vcorr = separa_colunas('Período', 'Importação', tabela_ca_2000_2017_vcorr)
absorv_dom_a_vcorr = separa_colunas('Período', 'Absorção Doméstica', tabela_ca_2000_2017_vcorr)


#Valor constantes
tabela_ca_2000_2017_vcon = arrumar_tabelas(tabela_ca_2000_2017, tabela_ca_2000_2017_vcon, 11:19)

tabela_ca_2000_2017_vcon = data.frame(tabela_ca_2000_2017[,1], tabela_ca_2000_2017[,11:19])
colnames(tabela_ca_2000_2017_vcon) = tabela_ca_2000_2017_vcon[1,]
tabela_ca_2000_2017_vcon = tabela_ca_2000_2017_vcon[-1,]
tabela_ca_2000_2017_vcon[,-1] = apply(tabela_ca_2000_2017_vcon[,-1],2,function(x)as.numeric(gsub(",",".",x)))

pib_a_vcon = tabela_ca_2000_2017_vcon[,c('Período', 'PIB = PIB a preços do ano anterior')]
consumo_familias_a_vcon = tabela_ca_2000_2017_vcon[,c('Período', 'Consumo das Famílias')]
consumo_governo_a_vcon = tabela_ca_2000_2017_vcon[,c('Período', 'Consumo do Governo')]
fbkf_a_vcon = tabela_ca_2000_2017_vcon[,c('Período', 'Formação Bruta de Capital Fixo')]
export_a_vcon = tabela_ca_2000_2017_vcon[,c('Período', 'Exportação')]
import_a_vcon = tabela_ca_2000_2017_vcon[,c('Período', 'Importação')]
absorv_dom_a_vcon = tabela_ca_2000_2017_vcon[,c('Período', 'Absorção Doméstica')]


#Deflatores
px = export_a_vcorr[,-1] / export_a_vcon[,-1]
px = data.frame(export_a_vcorr[,1], px)

pm = import_a_vcorr[,-1] / import_a_vcon[,-1]
pm = data.frame(import_a_vcorr[,1], pm)

pa = absorv_dom_a_vcorr[,-1] / absorv_dom_a_vcon[,-1]
pa = data.frame(absorv_dom_a_vcorr[,1], pa)

p_pib = pib_a_vcorr[,-1] / pib_a_vcon
p_pib = data.frame(pib_a_vcorr[,1], p_pib)

saa = absorv_dom_a_vcorr[,-1] / pib_a_vcorr[,-1]
saa = data.frame(absorv_dom_a_vcorr[,1], saa)

p_pib_saa = p_pib[,-1] * saa[,-1]
p_pib_saa = data.frame(p_pib[,1], p_pib_saa)

sx = export_a_vcorr[,-1] / pib_a_vcorr[,-1]
sx = data.frame(export_a_vcorr[,1], sx)

sm = (import_a_vcorr[,-1] / pib_a_vcorr[,-1])*-1
sm = data.frame(import_a_vcorr[,1], sm)

sxpx_smpx = (sx[,-1]/px[,-1]) - (sm[,-1]/pm[,-1])
sxpx_smpx = data.frame(sx[,1] , sxpx_smpx)

pa_calc = p_pib_saa[,-1] / (1 - p_pib[,-1] * sxpx_smpx[,-1])
pa_calc = data.frame(p_pib_saa[,1], pa_calc)
colnames(pa_calc) = c("Período", "Pa calculado")

p_tradables_mgeo = data.frame(px$Período ,sqrt(px$Exportação * pm$Importação))
colnames(p_tradables_mgeo) = c("Período", "P_tradables (m.geo)")

p_relativos = p_tradables_mgeo[,-1] / pa[,-1]
p_relativos = data.frame(px[,1], p_relativos)

prt_pa_calc = p_tradables_mgeo[,-1] / pa_calc[,-1]
prt_pa_calc = data.frame(p_tradables_mgeo[,1] , prt_pa_calc)

var_pib_1 = pib_a_vcorr[,1]
for (i in 2:dim(pib_a_vcorr)[1]){
  var_pib_1[i,2] = pib_a_vcon[i,-1] / pib_a_vcorr[i-1,-1]
}

tt = px[,-1] / pm[,-1]
tt = data.frame(px[,1], tt)

x_m = export_a_vcorr[,-1] + import_a_vcorr[,-1]
x_m = data.frame(export_a_vcorr[,1] , x_m)

x_m_pa = x_m[,-1] / pa[,-1]
x_m_pa = data.frame(x_m[,1], x_m_pa)

x_px = export_a_vcorr[,-1] / px[,-1]
x_px = data.frame(export_a_vcorr[,1], x_px)

m_pm = -import_a_vcorr[,-1] / pm[,-1]
m_pm = data.frame(import_a_vcorr[,1], m_pm)

xpx_mpm = x_px[,-1] - m_pm[,-1]
xpx_mpm = data.frame(x_px[,1], xpx_mpm)

gc = x_m_pa[,-1] - xpx_mpm[,-1]
gc = data.frame(x_m_pa[,1], gc)

gc_pib = gc[,-1] / pib_a_vcon[,-1]
gc_pib = data.frame(gc[,1], gc_pib)

rib_p_anoanterior = gc[,-1] + pib_a_vcon[,-1]
rib_p_anoanterior = data.frame(gc[,1], rib_p_anoanterior)

var_rib_1 = pib_a_vcorr[,1]
for (i in 2:dim(pib_a_vcorr)[1]){
  var_rib_1[i,2] = rib_p_anoanterior[i,-1] / pib_a_vcorr[i-1,-1]
}

ind_pib = var_pib_1[,1]
for (i in 2:dim(var_pib_1)[1]){
  ind_pib[1,2] = 100
  ind_pib[i,2] = ind_pib[i-1,2]*var_pib_1[i,2]
}

ind_rib = var_rib_1[,1]
for (i in 2:dim(var_rib_1)[1]){
  ind_rib[1,2] = 100
  ind_rib[i,2] = ind_rib[i-1,2]*var_rib_1[i,2]
}

ind_rib_pib = (ind_rib[,-1] / ind_pib[,-1])*100
ind_rib_pib = data.frame(ind_rib[,1], ind_rib_pib)

#SNA (2008)
p_pib
pib_a_vcon
x_px
m_pm
absorv_dom_a_vcon

x_pa = export_a_vcorr[,-1] / pa[,-1]
x_pa = data.frame(export_a_vcorr[,1], x_pa)

m_pa = - (import_a_vcorr[,-1] / pa[,-1])
m_pa = data.frame(import_a_vcorr[,1], m_pa)

rib_p_anoanterior
gc

var_real_pib = var_pib_1[,-1] - 1
var_real_pib = data.frame(var_pib_1[,1], var_real_pib)

var_real_rib = var_rib_1[,-1] - 1
var_real_rib = data.frame(var_rib_1[,1], var_real_rib)

gc_pib
tt

#índices?


