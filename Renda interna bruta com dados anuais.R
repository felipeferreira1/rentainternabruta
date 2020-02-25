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
tabela_ca_1990_2000 = read_excel("dados.xlsx", sheet = "Anual_1990-2000 (ref1985e2000)")
tabela_ca_1947_1989 = read_excel("dados.xlsx", sheet = "Anual_1947-1989 (ref1987)")

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

pib_a_vcorr_2000_2017 = separa_colunas('Período', 'PIB', tabela_ca_2000_2017_vcorr)
consumo_familias_a_vcorr_2000_2017 = separa_colunas('Período', 'Consumo das Famílias', tabela_ca_2000_2017_vcorr)
consumo_governo_a_vcorr_2000_2017 = separa_colunas('Período', 'Consumo do Governo', tabela_ca_2000_2017_vcorr)
fbkf_a_vcorr_2000_2017 = separa_colunas('Período', 'Formação Bruta de Capital Fixo', tabela_ca_2000_2017_vcorr)
export_a_vcorr_2000_2017 = separa_colunas('Período', 'Exportação', tabela_ca_2000_2017_vcorr)
import_a_vcorr_2000_2017 = separa_colunas('Período', 'Importação', tabela_ca_2000_2017_vcorr)
absorv_dom_a_vcorr_2000_2017 = separa_colunas('Período', 'Absorção Doméstica', tabela_ca_2000_2017_vcorr)


#Valor constantes
tabela_ca_2000_2017_vcon = arrumar_tabelas(tabela_ca_2000_2017, tabela_ca_2000_2017_vcon, 11:19)

pib_a_vcon_2000_2017 = separa_colunas('Período', 'PIB = PIB a preços do ano anterior', tabela_ca_2000_2017_vcon)
consumo_familias_a_vcon_2000_2017 = separa_colunas('Período', 'Consumo das Famílias', tabela_ca_2000_2017_vcon)
consumo_governo_a_vcon_2000_2017 = separa_colunas('Período', 'Consumo do Governo', tabela_ca_2000_2017_vcon)
fbkf_a_vcon_2000_2017 = separa_colunas('Período', 'Formação Bruta de Capital Fixo', tabela_ca_2000_2017_vcon)
export_a_vcon_2000_2017 = separa_colunas('Período', 'Exportação', tabela_ca_2000_2017_vcon)
import_a_vcon_2000_2017 = separa_colunas('Período', 'Importação', tabela_ca_2000_2017_vcon)
absorv_dom_a_vcon_2000_2017 = separa_colunas('Período', 'Absorção Doméstica', tabela_ca_2000_2017_vcon)

#Deflatores com valores constantes

#NOMEAR COLUNAS!!!


px_pc = export_a_vcorr_2000_2017[,-1] / export_a_vcon_2000_2017[,-1]
px_pc = data.frame(export_a_vcorr_2000_2017[,1], px_pc)
colnames(px_pc) = c('Período', 'Px')

pm_pc = import_a_vcorr_2000_2017[,-1] / import_a_vcon_2000_2017[,-1]
pm_pc = data.frame(import_a_vcorr_2000_2017[,1], pm_pc)
colnames(pm_pc) = c('Período', 'Pm')

pa_pc = absorv_dom_a_vcorr_2000_2017[,-1] / absorv_dom_a_vcon_2000_2017[,-1]
pa_pc = data.frame(absorv_dom_a_vcorr_2000_2017[,1], pa_pc)
colnames(pa_pc) = c('Período', 'Pa')

p_pib_pc = pib_a_vcorr_2000_2017[,-1] / pib_a_vcon_2000_2017 [,-1]
p_pib_pc = data.frame(pib_a_vcorr_2000_2017[,1], p_pib_pc)
colnames(p_pib_pc) = c('Período', 'Ppib')

#Cálculo Pa calculado
saa_pc = absorv_dom_a_vcorr_2000_2017[,-1] / pib_a_vcorr_2000_2017[,-1]
saa_pc = data.frame(absorv_dom_a_vcorr_2000_2017[,1], saa_pc)

p_pib_saa_pc = p_pib_pc[,-1] * saa_pc[,-1]
p_pib_saa_pc = data.frame(p_pib_pc[,1], p_pib_saa_pc)

sx_pc = export_a_vcorr_2000_2017[,-1] / pib_a_vcorr_2000_2017[,-1]
sx_pc = data.frame(export_a_vcorr_2000_2017[,1], sx_pc)

sm_pc = (import_a_vcorr_2000_2017[,-1] / pib_a_vcorr_2000_2017[,-1])*-1
sm_pc = data.frame(import_a_vcorr_2000_2017[,1], sm_pc)

sxpx_smpx_pc = (sx_pc[,-1]/px_pc[,-1]) - (sm_pc[,-1]/pm_pc[,-1])
sxpx_smpx_pc = data.frame(sx_pc[,1] , sxpx_smpx_pc)

pa_calc_pc = p_pib_saa_pc[,-1] / (1 - p_pib_pc[,-1] * sxpx_smpx_pc[,-1])
pa_calc_pc = data.frame(p_pib_saa_pc[,1], pa_calc_pc)
colnames(pa_calc_pc) = c("Período", "Pa calculado")

p_tradables_mgeo_pc = data.frame(px_pc$Período ,sqrt(px_pc$Px * pm_pc$Pm))
colnames(p_tradables_mgeo_pc) = c("Período", "P_tradables (m.geo)")

p_relativos_pc = p_tradables_mgeo_pc[,-1] / pa_pc[,-1]
p_relativos_pc = data.frame(px_pc[,1], p_relativos_pc)

prt_pa_calc_pc = p_tradables_mgeo_pc[,-1] / pa_calc_pc[,-1]
prt_pa_calc_pc = data.frame(p_tradables_mgeo_pc[,1] , prt_pa_calc_pc)

var_pib_1_pc = pib_a_vcorr_2000_2017
for (i in 2:dim(pib_a_vcorr_2000_2017)[1]){
  var_pib_1_pc[i,2] = pib_a_vcon_2000_2017[i,-1] / pib_a_vcorr_2000_2017[i-1,-1]
}

tt_pc = px_pc[,-1] / pm_pc[,-1]
tt_pc = data.frame(px_pc[,1], tt_pc)

x_m_pc = export_a_vcorr_2000_2017[,-1] + import_a_vcorr_2000_2017[,-1]
x_m_pc = data.frame(export_a_vcorr_2000_2017[,1] , x_m_pc)

x_m_pa_pc = x_m_pc[,-1] / pa_pc[,-1]
x_m_pa_pc = data.frame(x_m_pc[,1], x_m_pa_pc)

x_px_pc = export_a_vcorr_2000_2017[,-1] / px_pc[,-1]
x_px_pc = data.frame(export_a_vcorr_2000_2017[,1], x_px_pc)

m_pm_pc = -import_a_vcorr_2000_2017[,-1] / pm_pc[,-1]
m_pm_pc = data.frame(import_a_vcorr_2000_2017[,1], m_pm_pc)

xpx_mpm_pc = x_px_pc[,-1] - m_pm_pc[,-1]
xpx_mpm_pc = data.frame(x_px_pc[,1], xpx_mpm_pc)

gc_pc = x_m_pa_pc[,-1] - xpx_mpm_pc[,-1]
gc_pc = data.frame(x_m_pa_pc[,1], gc_pc)

gc_pib_pc = gc_pc[,-1] / pib_a_vcon_2000_2017[,-1]
gc_pib_pc = data.frame(gc_pc[,1], gc_pib_pc)

rib_p_anoanterior_pc = gc_pc[,-1] + pib_a_vcon_2000_2017[,-1]
rib_p_anoanterior_pc = data.frame(gc_pc[,1], rib_p_anoanterior_pc)

var_rib_1_pc = pib_a_vcorr_2000_2017
for (i in 2:dim(pib_a_vcorr_2000_2017)[1]){
  var_rib_1_pc[i,2] = rib_p_anoanterior_pc[i,-1] / pib_a_vcorr_2000_2017[i-1,-1]
}

ind_pib_pc = var_pib_1_pc
for (i in 2:dim(var_pib_1_pc)[1]){
  ind_pib_pc[1,2] = 100
  ind_pib_pc[i,2] = ind_pib_pc[i-1,2]*var_pib_1_pc[i,2]
}

ind_rib_pc = var_rib_1_pc
for (i in 2:dim(var_rib_1_pc)[1]){
  ind_rib_pc[1,2] = 100
  ind_rib_pc[i,2] = ind_rib_pc[i-1,2]*var_rib_1_pc[i,2]
}

ind_rib_pib_pc = (ind_rib_pc[,-1] / ind_pib_pc[,-1])*100
ind_rib_pib_pc = data.frame(ind_rib_pc[,1], ind_rib_pib_pc)

#Deflatores com variação real anual



#SNA (2008)
p_pib
pib_a_vcon_2000_2017
x_px
m_pm
absorv_dom_a_vcon_2000_2017

x_pa = export_a_vcorr_2000_2017[,-1] / pa[,-1]
x_pa = data.frame(export_a_vcorr_2000_2017[,1], x_pa)

m_pa = - (import_a_vcorr_2000_2017[,-1] / pa[,-1])
m_pa = data.frame(import_a_vcorr_2000_2017[,1], m_pa)

rib_p_anoanterior
gc

var_real_pib = var_pib_1[,-1] - 1
var_real_pib = data.frame(var_pib_1[,1], var_real_pib)

var_real_rib = var_rib_1[,-1] - 1
var_real_rib = data.frame(var_rib_1[,1], var_real_rib)

gc_pib
tt

#índices?


