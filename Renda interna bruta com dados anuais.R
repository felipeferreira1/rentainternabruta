#Rotina para calcular a renda interna bruta
#Feito por: Felipe Simplício Ferreira
#última atualização: 02/03/2020


#Definindo diretórios a serem utilizados

getwd()
setwd("C:\\Users\\User\\Documents\\GitHub\\rentainternabruta")

#Carregando pacotes que serão utilizados
library(readxl)

#Funções
arrumar_tabelas = function(df_entrada ,qtd_colunas){
  df_saida = data.frame(df_entrada[,1], df_entrada[,qtd_colunas])
  colnames(df_saida) = df_saida[1,]
  df_saida = df_saida[-1,]
  df_saida[,-1] = apply(df_saida[,-1],2,function(x)as.numeric(gsub(",",".",x)))
  return(df_saida)
}

separar_colunas = function(data, nome_coluna, df_entrada){
  df_saida = df_entrada[,c(data, nome_coluna)]
  return(df_saida)
}

var_nominal = function(df_entrada){
  df_saida = df_entrada
  df_saida[1,2] = NA
  for (i in 2:dim(df_entrada)[1]){
    df_saida[i,2] = df_entrada[i,2]/df_entrada[(i-1),2]
    }
  return(df_saida)
}

deflatores_div_cem_mais_um = function(numerador, denominador, nome_data, nome_dados){
  df_saida = numerador[,-1] / (denominador[,-1]/100+1)
  df_saida = data.frame(numerador[,1], df_saida)
  colnames(df_saida) = c(nome_data, nome_dados)
  return(df_saida)
}

deflatores_div = function(numerador, denominador, nome_data, nome_dados){
  df_saida = numerador[,-1] / denominador[,-1]
  df_saida = data.frame(numerador[,1], df_saida)
  colnames(df_saida) = c(nome_data, nome_dados)
  return(df_saida)
}

deflatores_sub = function(numerador, denominador, nome_data, nome_dados){
  df_saida = numerador[,-1] - denominador[,-1]
  df_saida = data.frame(numerador[,1], df_saida)
  colnames(df_saida) = c(nome_data, nome_dados)
  return(df_saida)
}

deflatores_soma = function(numerador, denominador, nome_data, nome_dados){
  df_saida = numerador[,-1] + denominador[,-1]
  df_saida = data.frame(numerador[,1], df_saida)
  colnames(df_saida) = c(nome_data, nome_dados)
  return(df_saida)
}

deflatores_mult = function(numerador, denominador, nome_data, nome_dados){
  df_saida = numerador[,-1] * denominador[,-1]
  df_saida = data.frame(numerador[,1], df_saida)
  colnames(df_saida) = c(nome_data, nome_dados)
  return(df_saida)
}

######PARTE 1######
#Carregando arquivo das contas anuais 1947-1989 (ref1987)
tabela_ca_1947_1989 = read_excel("dados.xlsx", sheet = "Anual_1947-1989 (ref1987)")
tabela_ca_1947_1989 = arrumar_tabelas(tabela_ca_1947_1989, 2:62)

#Importando deflatores
px_1947_1989 = separar_colunas('Período', 'Px_preço', tabela_ca_1947_1989)
pm_1947_1989 = separar_colunas('Período', 'Pm_preço', tabela_ca_1947_1989)
pa_1947_1989 = separar_colunas('Período', 'Pa - IPC-RJ Média', tabela_ca_1947_1989)
p_pib_1947_1989 = separar_colunas('Período', 'Ppib', tabela_ca_1947_1989)
pa_calc_1947_1989 = separar_colunas('Período', 'Pa calculado', tabela_ca_1947_1989)
p_tradables_mgeo_1947_1989 = separar_colunas('Período', 'P_tradables (m.geo)', tabela_ca_1947_1989)
p_relativos_1947_1989 = separar_colunas('Período', 'Preços relativos (P_tradables/P_ñtradables)', tabela_ca_1947_1989)
prt_pa_calc_1947_1989 = separar_colunas('Período', 'Prt com Pa calculado', tabela_ca_1947_1989)
pib_p_ano_anterior_1947_1989 = separar_colunas('Período', 'PIB a preços do ano anterior', tabela_ca_1947_1989)
tt_1947_1989 = separar_colunas('Período', 'Termos de Troca (Px/Pm)', tabela_ca_1947_1989)
x_m_1947_1989 = separar_colunas('Período', '(X-M)', tabela_ca_1947_1989)
x_m_pa_1947_1989 = separar_colunas('Período', '(X-M)/Pa', tabela_ca_1947_1989)
x_px_1947_1989 = separar_colunas('Período', 'X/Px_preço', tabela_ca_1947_1989)
m_pm_1947_1989 = separar_colunas('Período', 'M/Pm_preço', tabela_ca_1947_1989)
xpx_mpm_1947_1989 = separar_colunas('Período', 'X/Px-M/Pm', tabela_ca_1947_1989)
gc_1947_1989 = separar_colunas('Período', 'GC', tabela_ca_1947_1989)
gc_pib_1947_1989 = separar_colunas('Período', 'GC/PIB', tabela_ca_1947_1989)
rib_p_ano_anterior_1947_1989 = separar_colunas('Período', 'RIB a preços do ano anterior', tabela_ca_1947_1989)
ind_pib_1947_1989 = separar_colunas('Período', 'Índice PIB', tabela_ca_1947_1989)
ind_rib_1947_1989 = separar_colunas('Período', 'Índice RIB Pa', tabela_ca_1947_1989)
ind_rib_pib_1947_1989 = separar_colunas('Período', 'Índice RIB/PIB(Pa)', tabela_ca_1947_1989)


######PARTE 2######
#Carregando arquivo das contas anuais 1990-2000 (ref1985e2000)
tabela_ca_1990_2000 = read_excel("dados.xlsx", sheet = "Anual_1990-2000 (ref1985e2000)")

#valores correntes
tabela_ca_1990_2000_vcorr = arrumar_tabelas(tabela_ca_1990_2000, 2:10)

pib_a_vcorr_1990_2000 = separar_colunas('Período', 'PIB', tabela_ca_1990_2000_vcorr)
consumo_familias_a_vcorr_1990_2000 = separar_colunas('Período', 'Consumo das Famílias', tabela_ca_1990_2000_vcorr)
consumo_governo_a_vcorr_1990_2000 = separar_colunas('Período', 'Consumo do Governo', tabela_ca_1990_2000_vcorr)
fbkf_a_vcorr_1990_2000 = separar_colunas('Período', 'Formação Bruta de Capital Fixo', tabela_ca_1990_2000_vcorr)
var_est_a_vcorr_1996_2018 = separar_colunas('Período', 'Variação de Estoque', tabela_ca_1990_2000_vcorr)
export_a_vcorr_1990_2000 = separar_colunas('Período', 'Exportação', tabela_ca_1990_2000_vcorr)
import_a_vcorr_1990_2000 = separar_colunas('Período', 'Importação', tabela_ca_1990_2000_vcorr)
absorv_dom_a_vcorr_1990_2000 = separar_colunas('Período', 'Absorção Doméstica', tabela_ca_1990_2000_vcorr)
fbk_a_vcorr_1990_2000 = separar_colunas('Período', 'Formação Bruta de Capital', tabela_ca_1990_2000_vcorr)


#Variação real anual
tabela_ca_1990_2000_vra = arrumar_tabelas(tabela_ca_1990_2000, 11:17)

pib_a_vra_1990_2000 = separar_colunas('Período', 'PIB', tabela_ca_1990_2000_vra)
consumo_familias_a_vra_1990_2000 = separar_colunas('Período', 'Consumo das Famílias', tabela_ca_1990_2000_vra)
consumo_governo_a_vra_1990_2000 = separar_colunas('Período', 'Consumo do Governo', tabela_ca_1990_2000_vra)
fbkf_a_vra_1990_2000 = separar_colunas('Período', 'Formação Bruta de Capital Fixo', tabela_ca_1990_2000_vra)
fbk_a_vra_1990_2000 = separar_colunas('Período', 'Formação Bruta de Capital', tabela_ca_1990_2000_vra)
export_a_vra_1990_2000 = separar_colunas('Período', 'Exportação', tabela_ca_1990_2000_vra)
import_a_vra_1990_2000 = separar_colunas('Período', 'Importação', tabela_ca_1990_2000_vra)


#Variação nominal anual
pib_a_vna_1990_2000 = var_nominal(pib_a_vcorr_1990_2000)
consumo_familias_a_vna_1990_2000 = var_nominal(consumo_familias_a_vcorr_1990_2000)
consumo_governo_a_vna_1990_2000 = var_nominal(consumo_governo_a_vcorr_1990_2000)
fbkf_a_vna_1990_2000 = var_nominal(fbkf_a_vcorr_1990_2000)
fbk_a_vna_1990_2000 = var_nominal(fbk_a_vcorr_1990_2000)
export_a_vna_1990_2000 = var_nominal(export_a_vcorr_1990_2000)
import_a_vna_1990_2000 = var_nominal(import_a_vcorr_1990_2000)

#Calculando deflatores
px_vn_1990_2000 = deflatores_div_cem_mais_um(export_a_vna_1990_2000, export_a_vra_1990_2000, "Período", "Px")
pm_vn_1990_2000 = deflatores_div_cem_mais_um(import_a_vna_1990_2000, import_a_vra_1990_2000, "Período", "Pm")
pc_vn_1990_2000 = deflatores_div_cem_mais_um(consumo_familias_a_vna_1990_2000, consumo_familias_a_vra_1990_2000, "Período", "Pc")
pg_vn_1990_2000 = deflatores_div_cem_mais_um(consumo_governo_a_vna_1990_2000, consumo_governo_a_vra_1990_2000, "Período", "Pg")
pfbkf_vn_1990_2000 = deflatores_div_cem_mais_um(fbkf_a_vna_1990_2000, fbkf_a_vra_1990_2000, "Período", "Pfbkf")
pfbk_vn_1990_2000 = deflatores_div_cem_mais_um(fbk_a_vna_1990_2000, fbk_a_vra_1990_2000, "Período", "Pfbk")
pesoc_vn_1990_2000 = deflatores_div(consumo_familias_a_vcorr_1990_2000, absorv_dom_a_vcorr_1990_2000, "Período", "Peso C")
pesog_vn_1990_2000 = deflatores_div(consumo_governo_a_vcorr_1990_2000, absorv_dom_a_vcorr_1990_2000, "Período", "Peso G")
pesofbkf_vn_1990_2000 = deflatores_div(fbkf_a_vcorr_1990_2000, absorv_dom_a_vcorr_1990_2000, "Período", "Peso Fbkf")
pesofbk_vn_1990_2000 = deflatores_div(fbk_a_vcorr_1990_2000, absorv_dom_a_vcorr_1990_2000, "Período", "Peso Fbk")

pa_vn_1990_2000 = 1/((pesoc_vn_1990_2000[,-1])/pc_vn_1990_2000[,-1] + (pesog_vn_1990_2000[,-1]/pg_vn_1990_2000[,-1]) + (pesofbk_vn_1990_2000[,-1]/pfbk_vn_1990_2000[,-1]))
pa_vn_1990_2000 = data.frame(pesoc_vn_1990_2000[,1], pa_vn_1990_2000)
colnames(pa_vn_1990_2000) = c("Período", "Pa")

p_pib_vn_1990_2000 = deflatores_div_cem_mais_um(pib_a_vna_1990_2000, pib_a_vra_1990_2000, "Período", "Ppib")

#Cálculo Pa calculado
saa_vn_1990_2000 = deflatores_div(absorv_dom_a_vcorr_1990_2000, pib_a_vcorr_1990_2000, "Período", "Saa")
p_pib_saa_vn_1990_2000 = deflatores_mult(p_pib_vn_1990_2000, saa_vn_1990_2000, "Período", "Ppib * Saa")
sx_vn_1990_2000 = deflatores_div(export_a_vcorr_1990_2000, pib_a_vcorr_1990_2000, "Período", "Sx")
sm_vn_1990_2000 = deflatores_div(import_a_vcorr_1990_2000, pib_a_vcorr_1990_2000, "Período", "Sm")

sxpx_smpx_vn_1990_2000 = (sx_vn_1990_2000[,-1]/px_vn_1990_2000[,-1]) - (sm_vn_1990_2000[,-1]/pm_vn_1990_2000[,-1])
sxpx_smpx_vn_1990_2000 = data.frame(sx_vn_1990_2000[,1] , sxpx_smpx_vn_1990_2000)
colnames(sxpx_smpx_vn_1990_2000) = c("Período", " (Sx/Px - Sm/Pm)")

pa_calc_vn_1990_2000 = p_pib_saa_vn_1990_2000[,-1] / (1 - p_pib_vn_1990_2000[,-1] * sxpx_smpx_vn_1990_2000[,-1])
pa_calc_vn_1990_2000 = data.frame(p_pib_saa_vn_1990_2000[,1], pa_calc_vn_1990_2000)
colnames(pa_calc_vn_1990_2000) = c("Período", "Pa calculado")

p_tradables_mgeo_vn_1990_2000 = data.frame(px_vn_1990_2000$Período ,sqrt(px_vn_1990_2000$Px * pm_vn_1990_2000$Pm))
colnames(p_tradables_mgeo_vn_1990_2000) = c("Período", "P_tradables (m.geo)")

p_relativos_vn_1990_2000 = deflatores_div(p_tradables_mgeo_vn_1990_2000, pa_vn_1990_2000, "Período", "Prelativos")
prt_pa_calc_vn_1990_2000 = deflatores_div(p_tradables_mgeo_vn_1990_2000, pa_calc_vn_1990_2000, "Período", "Prt com Pa calculado")

pib_p_ano_anterior_1990_2000 = pib_a_vcorr_1990_2000
pib_p_ano_anterior_1990_2000[1,2] = NA
for (i in 2:dim(pib_a_vcorr_1990_2000)[1]){
  pib_p_ano_anterior_1990_2000[i,2] = pib_a_vcorr_1990_2000[(i-1),2] * (pib_a_vra_1990_2000[i,2]/100+1)
}

tt_vn_1990_2000 = deflatores_div(px_vn_1990_2000, pm_vn_1990_2000, "Período", "Termos de troca")
x_m_vn_1990_2000 = deflatores_sub(export_a_vcorr_1990_2000, import_a_vcorr_1990_2000, "Período", "(X-M)")
x_m_pa_vn_1990_2000 = deflatores_div(x_m_vn_1990_2000, pa_vn_1990_2000, "Período", "(X-M)/Pa")
x_px_vn_1990_2000 = deflatores_div(export_a_vcorr_1990_2000, px_vn_1990_2000, "Período", "X/Px")
m_pm_vn_1990_2000 = deflatores_div(import_a_vcorr_1990_2000, pm_vn_1990_2000, "Período", "M/Pm")
xpx_mpm_vn_1990_2000 = deflatores_sub(x_px_vn_1990_2000, m_pm_vn_1990_2000, "Período", "X/Px-M/Pm")
gc_vn_1990_2000 = deflatores_sub(x_m_pa_vn_1990_2000, xpx_mpm_vn_1990_2000, "Período", "GC")
gc_pib_vn_1990_2000 = deflatores_div(gc_vn_1990_2000, pib_p_ano_anterior_1990_2000, "Período", "GC/PIB")
rib_p_ano_anterior_vn_1990_2000 = deflatores_soma(gc_vn_1990_2000, pib_p_ano_anterior_1990_2000, "Período", "RIB a preços do ano anterior")

var_rib_1_vn_1990_2000 = pib_a_vcorr_1990_2000
var_rib_1_vn_1990_2000[1,2] = NA
for (i in 2:dim(pib_a_vcorr_1990_2000)[1]){
  var_rib_1_vn_1990_2000[i,2] = rib_p_ano_anterior_vn_1990_2000[i,-1] / pib_a_vcorr_1990_2000[(i-1),-1]
}

ind_pib_vn_1990_2000 = pib_a_vra_1990_2000
ind_pib_vn_1990_2000[1,2] = 100
for (i in 2:dim(pib_a_vra_1990_2000)[1]){
  ind_pib_vn_1990_2000[i,2] = ind_pib_vn_1990_2000[i-1,2] * (pib_a_vra_1990_2000[i,2] / 100 + 1)
}

ind_rib_vn_1990_2000 = var_rib_1_vn_1990_2000
ind_rib_vn_1990_2000[1,2] = 100
for (i in 2:dim(var_rib_1_vn_1990_2000)[1]){
  ind_rib_vn_1990_2000[i,2] = ind_rib_vn_1990_2000[i-1,2]*var_rib_1_vn_1990_2000[i,2]
}

ind_rib_pib_vn_1990_2000 = (ind_rib_vn_1990_2000[,-1] / ind_pib_vn_1990_2000[,-1])*100
ind_rib_pib_vn_1990_2000 = data.frame(ind_rib_vn_1990_2000[,1], ind_rib_pib_vn_1990_2000)
colnames(ind_rib_pib_vn_1990_2000) = c("Período", "Índice RIB/PIB(Pa)")


######PARTE 3######
#Carregando arquivo das contas trimestrais 1996-2018 (ref 2010)
tabela_ct_1996_2018 = read_excel("dados.xlsx", sheet = "Trimestral_1996-2018 (ref2010)")

#Valores correntes
tabela_ct_1996_2018_vcorr = arrumar_tabelas(tabela_ct_1996_2018, 2:9)

pib_a_vcorr_1996_2018 = separar_colunas('Período', 'PIB', tabela_ct_1996_2018_vcorr)
consumo_familias_a_vcorr_1996_2018 = separar_colunas('Período', 'Consumo das Famílias', tabela_ct_1996_2018_vcorr)
consumo_governo_a_vcorr_1996_2018 = separar_colunas('Período', 'Consumo do Governo', tabela_ct_1996_2018_vcorr)
fbkf_a_vcorr_1996_2018 = separar_colunas('Período', 'Formação Bruta de Capital Fixo', tabela_ct_1996_2018_vcorr)
export_a_vcorr_1996_2018 = separar_colunas('Período', 'Exportação', tabela_ct_1996_2018_vcorr)
import_a_vcorr_1996_2018 = separar_colunas('Período', 'Importação', tabela_ct_1996_2018_vcorr)
absorv_dom_a_vcorr_1996_2018 = separar_colunas('Período', 'Absorção Doméstica', tabela_ct_1996_2018_vcorr)
var_est_a_vcorr_1996_2018 = separar_colunas('Período', 'Variação de Estoques', tabela_ct_1996_2018_vcorr)


#Variação real anual
tabela_ct_1996_2018_vra = arrumar_tabelas(tabela_ct_1996_2018, 10:15)

pib_a_vra_1996_2018 = separar_colunas('Período', 'PIB', tabela_ct_1996_2018_vra)
consumo_familias_a_vra_1996_2018 = separar_colunas('Período', 'Consumo das Famílias', tabela_ct_1996_2018_vra)
consumo_governo_a_vra_1996_2018 = separar_colunas('Período', 'Consumo do Governo', tabela_ct_1996_2018_vra)
fbkf_a_vra_1996_2018 = separar_colunas('Período', 'Formação Bruta de Capital Fixo', tabela_ct_1996_2018_vra)
export_a_vra_1996_2018 = separar_colunas('Período', 'Exportação', tabela_ct_1996_2018_vra)
import_a_vra_1996_2018 = separar_colunas('Período', 'Importação', tabela_ct_1996_2018_vra)


#Variação nominal anual
pib_a_vna_1996_2018 = var_nominal(pib_a_vcorr_1996_2018)
consumo_familias_a_vna_1996_2018 = var_nominal(consumo_familias_a_vcorr_1996_2018)
consumo_governo_a_vna_1996_2018 = var_nominal(consumo_governo_a_vcorr_1996_2018)
fbkf_a_vna_1996_2018 = var_nominal(fbkf_a_vcorr_1996_2018)
export_a_vna_1996_2018 = var_nominal(export_a_vcorr_1996_2018)
import_a_vna_1996_2018 = var_nominal(import_a_vcorr_1996_2018)


#Calculando deflatores
px_vn_1996_2018 = deflatores_div_cem_mais_um(export_a_vna_1996_2018, export_a_vra_1996_2018, "Período", "Px")
pm_vn_1996_2018 = deflatores_div_cem_mais_um(import_a_vna_1996_2018, import_a_vra_1996_2018, "Período", "Pm")
pc_vn_1996_2018 = deflatores_div_cem_mais_um(consumo_familias_a_vna_1996_2018, consumo_familias_a_vra_1996_2018, "Período", "Pc")
pg_vn_1996_2018 = deflatores_div_cem_mais_um(consumo_governo_a_vna_1996_2018, consumo_governo_a_vra_1996_2018, "Período", "Pg")
pfbkf_vn_1996_2018 = deflatores_div_cem_mais_um(fbkf_a_vna_1996_2018, fbkf_a_vra_1996_2018, "Período", "Pfbkf")
pesoc_vn_1996_2018 = deflatores_div(consumo_familias_a_vcorr_1996_2018, absorv_dom_a_vcorr_1996_2018, "Período", "Peso C")
pesog_vn_1996_2018 = deflatores_div(consumo_governo_a_vcorr_1996_2018, absorv_dom_a_vcorr_1996_2018, "Período", "Peso G")
pesofbkf_vn_1996_2018 = deflatores_div(fbkf_a_vcorr_1996_2018, absorv_dom_a_vcorr_1996_2018, "Período", "Peso Fbkf")

pa_vn_1996_2018 = 1/((pesoc_vn_1996_2018[,-1])/pc_vn_1996_2018[,-1] + (pesog_vn_1996_2018[,-1]/pg_vn_1996_2018[,-1]) + (pesofbkf_vn_1996_2018[,-1]/pfbkf_vn_1996_2018[,-1]))
pa_vn_1996_2018 = data.frame(pesoc_vn_1996_2018[,1], pa_vn_1996_2018)
colnames(pa_vn_1996_2018) = c("Período", "Pa")

p_pib_vn_1996_2018 = deflatores_div_cem_mais_um(pib_a_vna_1996_2018, pib_a_vra_1996_2018, "Período", "Ppib")

#Cálculo Pa calculado
saa_vn_1996_2018 = deflatores_div(absorv_dom_a_vcorr_1996_2018, pib_a_vcorr_1996_2018, "Período", "Saa")
p_pib_saa_vn_1996_2018 = deflatores_mult(p_pib_vn_1996_2018, saa_vn_1996_2018, "Período", "Ppib * Saa")
sx_vn_1996_2018 = deflatores_div(export_a_vcorr_1996_2018, pib_a_vcorr_1996_2018, "Período", "Sx")
sm_vn_1996_2018 = deflatores_div(import_a_vcorr_1996_2018, pib_a_vcorr_1996_2018, "Período", "Sm")

sxpx_smpx_vn_1996_2018 = (sx_vn_1996_2018[,-1]/px_vn_1996_2018[,-1]) - (sm_vn_1996_2018[,-1]/pm_vn_1996_2018[,-1])
sxpx_smpx_vn_1996_2018 = data.frame(sx_vn_1996_2018[,1] , sxpx_smpx_vn_1996_2018)
colnames(sxpx_smpx_vn_1996_2018) = c("Período", " (Sx/Px - Sm/Pm)")

pa_calc_vn_1996_2018 = p_pib_saa_vn_1996_2018[,-1] / (1 - p_pib_vn_1996_2018[,-1] * sxpx_smpx_vn_1996_2018[,-1])
pa_calc_vn_1996_2018 = data.frame(p_pib_saa_vn_1996_2018[,1], pa_calc_vn_1996_2018)
colnames(pa_calc_vn_1996_2018) = c("Período", "Pa calculado")

p_tradables_mgeo_vn_1996_2018 = data.frame(px_vn_1996_2018$Período ,sqrt(px_vn_1996_2018$Px * pm_vn_1996_2018$Pm))
colnames(p_tradables_mgeo_vn_1996_2018) = c("Período", "P_tradables (m.geo)")

p_relativos_vn_1996_2018 = deflatores_div(p_tradables_mgeo_vn_1996_2018, pa_vn_1996_2018, "Período", "Prelativos")
prt_pa_calc_vn_1996_2018 = deflatores_div(p_tradables_mgeo_vn_1996_2018, pa_calc_vn_1996_2018, "Período", "Prt com Pa calculado")

pib_p_ano_anterior_1996_2018 = pib_a_vcorr_1996_2018
pib_p_ano_anterior_1996_2018[1,2] = NA
for (i in 2:dim(pib_a_vcorr_1996_2018)[1]){
  pib_p_ano_anterior_1996_2018[i,2] = pib_a_vcorr_1996_2018[(i-1),2] * (pib_a_vra_1996_2018[i,2]/100+1)
}

tt_vn_1996_2018 = deflatores_div(px_vn_1996_2018, pm_vn_1996_2018, "Período", "Termos de troca")
x_m_vn_1996_2018 = deflatores_sub(export_a_vcorr_1996_2018, import_a_vcorr_1996_2018, "Período", "(X-M)")
x_m_pa_vn_1996_2018 = deflatores_div(x_m_vn_1996_2018, pa_vn_1996_2018, "Período", "(X-M)/Pa")
x_px_vn_1996_2018 = deflatores_div(export_a_vcorr_1996_2018, px_vn_1996_2018, "Período", "X/Px")
m_pm_vn_1996_2018 = deflatores_div(import_a_vcorr_1996_2018, pm_vn_1996_2018, "Período", "M/Pm")
xpx_mpm_vn_1996_2018 = deflatores_sub(x_px_vn_1996_2018, m_pm_vn_1996_2018, "Período", "X/Px-M/Pm")
gc_vn_1996_2018 = deflatores_sub(x_m_pa_vn_1996_2018, xpx_mpm_vn_1996_2018, "Período", "GC")
gc_pib_vn_1996_2018 = deflatores_div(gc_vn_1996_2018, pib_p_ano_anterior_1996_2018, "Período", "GC/PIB")
rib_p_ano_anterior_vn_1996_2018 = deflatores_soma(gc_vn_1996_2018, pib_p_ano_anterior_1996_2018, "Período", "RIB a preços do ano anterior")

var_rib_1_vn_1996_2018 = pib_a_vcorr_1996_2018
var_rib_1_vn_1996_2018[1,2] = NA
for (i in 2:dim(pib_a_vcorr_1996_2018)[1]){
  var_rib_1_vn_1996_2018[i,2] = rib_p_ano_anterior_vn_1996_2018[i,-1] / pib_a_vcorr_1996_2018[(i-1),-1]
}

ind_pib_vn_1996_2018 = pib_a_vra_1996_2018
ind_pib_vn_1996_2018[1,2] = 100
for (i in 2:dim(pib_a_vra_1996_2018)[1]){
  ind_pib_vn_1996_2018[i,2] = ind_pib_vn_1996_2018[i-1,2] * (pib_a_vra_1996_2018[i,2] / 100 + 1)
}

ind_rib_vn_1996_2018 = var_rib_1_vn_1996_2018
ind_rib_vn_1996_2018[1,2] = 100
for (i in 2:dim(var_rib_1_vn_1996_2018)[1]){
  ind_rib_vn_1996_2018[i,2] = ind_rib_vn_1996_2018[i-1,2]*var_rib_1_vn_1996_2018[i,2]
}

ind_rib_pib_vn_1996_2018 = (ind_rib_vn_1996_2018[,-1] / ind_pib_vn_1996_2018[,-1])*100
ind_rib_pib_vn_1996_2018 = data.frame(ind_rib_vn_1996_2018[,1], ind_rib_pib_vn_1996_2018)
colnames(ind_rib_pib_vn_1996_2018) = c("Período", "Índice RIB/PIB(Pa)")


######PARTE 4######
#Carregando dados anuais 2000-2017 (ref 2010)
tabela_ca_2000_2017= read_excel("dados.xlsx", sheet = "Anual_2000-2017 (ref2010)")

#valores correntes
tabela_ca_2000_2017_vcorr = arrumar_tabelas(tabela_ca_2000_2017, 2:10)

pib_a_vcorr_2000_2017 = separar_colunas('Período', 'PIB', tabela_ca_2000_2017_vcorr)
consumo_familias_a_vcorr_2000_2017 = separar_colunas('Período', 'Consumo das Famílias', tabela_ca_2000_2017_vcorr)
consumo_governo_a_vcorr_2000_2017 = separar_colunas('Período', 'Consumo do Governo', tabela_ca_2000_2017_vcorr)
fbkf_a_vcorr_2000_2017 = separar_colunas('Período', 'Formação Bruta de Capital Fixo', tabela_ca_2000_2017_vcorr)
export_a_vcorr_2000_2017 = separar_colunas('Período', 'Exportação', tabela_ca_2000_2017_vcorr)
import_a_vcorr_2000_2017 = separar_colunas('Período', 'Importação', tabela_ca_2000_2017_vcorr)
absorv_dom_a_vcorr_2000_2017 = separar_colunas('Período', 'Absorção Doméstica', tabela_ca_2000_2017_vcorr)


#Valores constantes
tabela_ca_2000_2017_vcon = arrumar_tabelas(tabela_ca_2000_2017, 11:19)

pib_a_vcon_2000_2017 = separar_colunas('Período', 'PIB = PIB a preços do ano anterior', tabela_ca_2000_2017_vcon)
consumo_familias_a_vcon_2000_2017 = separar_colunas('Período', 'Consumo das Famílias', tabela_ca_2000_2017_vcon)
consumo_governo_a_vcon_2000_2017 = separar_colunas('Período', 'Consumo do Governo', tabela_ca_2000_2017_vcon)
fbkf_a_vcon_2000_2017 = separar_colunas('Período', 'Formação Bruta de Capital Fixo', tabela_ca_2000_2017_vcon)
export_a_vcon_2000_2017 = separar_colunas('Período', 'Exportação', tabela_ca_2000_2017_vcon)
import_a_vcon_2000_2017 = separar_colunas('Período', 'Importação', tabela_ca_2000_2017_vcon)
absorv_dom_a_vcon_2000_2017 = separar_colunas('Período', 'Absorção Doméstica', tabela_ca_2000_2017_vcon)

#Deflatores com valores constantes

#NOMEAR COLUNAS!!!

#px_pca = deflatores_div(export_a_vcorr_2000_2017, export_a_vcon_2000_2017, "Período", "Px")
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
consolida_series = function(serie1, serie2, serie3, serie4){
  
  
  primeira_juncao = merge(serie1, serie2, by = "Período", all = T)
  segunda_juncao = merge(primeira_juncao, serie3, by = "Período", all = T)
  terceira_juncao = merge(segunda_juncao, serie4, by = "Período", all = T)
  
  terceira_juncao = apply(terceira_juncao,2,function(x)as.numeric(gsub(",",".",x)))
  terceira_juncao = as.data.frame(terceira_juncao)
  colnames(terceira_juncao) = c("Período", "Série 1", "Série 2", "Série 3", "Série 4")
  
  #return(terceira_juncao)}
  
  base_final = data.frame("Período" = terceira_juncao$'Período')
  base_final$'Série'= NA
  
  #return(base_final)}
  
  for (i in 1:dim(base_final)[1]){
    if (terceira_juncao$'Período'[i] < 1991)
      base_final[i,2] = terceira_juncao[i,2]
    if (terceira_juncao$'Período'[i] < 1990 & terceira_juncao$'Período'[i] > 1997)
      base_final[i,2] = terceira_juncao[i,3]
    if (terceira_juncao$'Período'[i] < 1996 & terceira_juncao$'Período'[i] > 2001)
      base_final[i,2] = terceira_juncao[i,4]
    if (terceira_juncao$'Período'[i] < 2000 & terceira_juncao$'Período'[i] > 2018)
      base_final[i,2] = terceira_juncao[i,5]
    if (terceira_juncao$'Período'[i] < 2017)
      base_final[i,2] = terceira_juncao[i,4]
  }
  return(base_final)
}

p_pib_SNA2 = consolida_series(p_pib_1947_1989, p_pib_vn_1990_2000, p_pib_vn_1996_2018, p_pib_pc)

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


