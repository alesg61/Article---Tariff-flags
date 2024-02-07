*Comandos PAINEL
xtset id t, monthly
tis t
iis id

**Manipulando as variáveis**
*Transformar as variáveis de Megawhat/h para Quilowhat/h pois as bandeiras tarifarias são em Kwh
*Transformação do Consumo de energia em Megawhat/h para Quilowhat/h
gen conskwh = (consmgh*1000)

*Criação do Consumo kwh agregado do estado para Consumo kwh por unidade consumidora (per capita)
gen conskwh_pc = (conskwh/ncons)

*Criação do log do Consumo per capita
gen l_conskwh_pc = log(conskwh_pc)

*Criação do Consumo defasado para fazer painel dinâmico
*l.conskwh_pc l.l_conskwh_pc são o consumo per capita e o log do consumo per capita defasados para estimar painel dinâmico.

*Transformação da tarifa média com tributos de Megawhath para Quilowhat/h
*Regra de 3: 1000kwh-----R$334,75... 1kwh-----R$ X... X=334,75/1000
gen tarifakwh = (tarifamgh/1000)

*Deflacionando a tarifa Kwh para valores presentes
*calculado conforme doc da FIPE (https://downloads.fipe.org.br/publicacoes/bif/bif478-18-24.pdf)
gen tarifakwh_def = (0.47/ipca)*tarifakwh

*Criação do log da tarifa kwh deflacionada
gen l_tarifakwh_def = log(tarifakwh_def)

*Deflacionando o Imposto ICMS para valores presentes
*calculado conforme doc da FIPE (https://downloads.fipe.org.br/publicacoes/bif/bif478-18-24.pdf)
gen taxicms_def = (0.47/ipca)*taxicms

*Criando Tax ICMS per capita
gen taxicms_def_pc = (taxicms_def/ncons)

*Criação do log da tarifa kwh deflacionada
gen l_taxicms_def_pc = log(taxicms_def_pc)


*Criação da proxy de renda (Imposto arrecadado pelo Estado mensal) pois só tem dados de PIB trimestral estadual até 2019.
*tax
*Imposto defasado = tax_def
*Imposto defasado por unidade consumidora (per capita) = tax_def_pc
*log do imposto defasado = l_tax_def_pc

*Dummy de horário de verão:
*verao



*Estatistica basica inicial
sum conskwh_pc tax_def taxicms_def tax_def_pc taxicms_def_pc temp ipca ncons tarifakwh_def verde amarela vermelha1 vermelha2 d_escassez

*Matriz de correlação inicial
pwcorr conskwh conskwh_pc tax_def taxicms_def tax_def_pc taxicms_def_pc temp ipca ncons tarifakwh_def verde amarela vermelha1 vermelha2 d_escassez, star(0.1)


*MODELO POOLED
reg l_conskwh_pc l.l_conskwh_pc temp ln_preco ln_renda_def_pc amarela vermelha1 vermelha2 d_escassez

*MODELO DE EFEITOS FIXOS
xtreg l_conskwh_pc l.l_conskwh_pc temp ln_preco ln_renda_def_pc amarela vermelha1 vermelha2 d_escassez, fe

*MODELO DE EFEITOS ALEATÓRIOS
xtreg l_conskwh_pc l.l_conskwh_pc temp ln_preco ln_renda_def_pc amarela vermelha1 vermelha2 d_escassez, re

*ESCOLHA ENTRE MODELOS
*Teste de Chow (H0: Pooled; H1: Efeitos Fixos)
xtreg l_conskwh_pc l.l_conskwh_pc temp ln_preco ln_renda_def_pc amarela vermelha1 vermelha2 d_escassez, fe

*Teste LM de Breusch-Pagan (H0: Pooled; H1:Efeitos Aleatorios)
xtreg l_conskwh_pc l.l_conskwh_pc temp ln_preco ln_renda_def_pc amarela vermelha1 vermelha2 d_escassez, re
xttest0

*Teste de Hausman (H0: Efeitos Aleatorios; H1: Efeitos Fixos)
qui xtreg l_conskwh_pc l.l_conskwh_pc temp ln_preco ln_renda_def_pc amarela vermelha1 vermelha2 d_escassez, fe
estimates store fe
qui xtreg l_conskwh_pc l.l_conskwh_pc temp ln_preco ln_renda_def_pc amarela vermelha1 vermelha2 d_escassez, re
estimates store re
hausman fe re

*Melhor modelo é com Efeitos Fixos (valor-p = 0.0000)

*TESTE PARA IDENTIFICACAO DE PROBLEMAS
*FIV (Multicolinearidade. Maior que 10 tem indÌcios de multicolinearidade)
qui reg l_conskwh_pc l.l_conskwh_pc temp ln_preco ln_renda_def_pc amarela vermelha1 vermelha2 d_escassez
vif
*Não há multicolinearidade

*Teste de autocorrelacao para dados em painel (Teste de Wooldridge; H0: Nao tem autocorrelacao de primeira ordem)
xtserial l_conskwh_pc l.l_conskwh_pc temp ln_preco ln_renda_def_pc amarela vermelha1 vermelha2 d_escassez

*Teste de Wald para heterocedasticidade em grupo (EFEITOS FIXOS) H0: Homocedasticidade
qui xtreg l_conskwh_pc l.l_conskwh_pc temp ln_preco ln_renda_def_pc amarela vermelha1 vermelha2 d_escassez, fe
xttest3
*Há heterocedasticidade pelo teste de Wald

*Teste de White (heterocedasticidade) H0: Homocedasticidade
reg l_conskwh_pc l.l_conskwh_pc temp ln_preco ln_renda_def_pc amarela vermelha1 vermelha2 d_escassez
whitetst
*Há heterocedasticidade pelo teste de White


*Teste de Breush-Pagan Cook-Weisberg test for heteroskedasticity (H0: homocedasticidade)
reg l_conskwh_pc l.l_conskwh_pc temp ln_preco ln_renda_def_pc amarela vermelha1 vermelha2 d_escassez
hettest
*Há heterocedasticidade pelo teste de Breush-Pagan Cook-Weisberg

*Correção do modelo por Erros-padrão robusto
xtreg l_conskwh_pc l.l_conskwh_pc temp ln_preco ln_renda_def_pc amarela vermelha1 vermelha2 d_escassez, fe vce(r)



*So a quick way in order to either check and correct HT is to implement ,vce(robust) after your panel data regression and check if the standart errors associate to your coeficient increases or not. if not, HT wasn't present, if they do, the previously model had HT and it's been corrected. it's not a formal test, but it can help you to see if your model has HT.
*since you have a T>N panel dataset, -xtreg- is not the way to go, wheras -xtgls- is what you need.
*Mínimos Quadrados Generalizados
*xtgls l_conskwh_p temp ibcbr l_tarifakwh_def amarela vermelha1 vermelha2 d_escassez, panels (heteroskedastic) force

*Painel dinâmico por Arellano Bond
*xtabond l_conskwh_pc temp ibcbr l_tarifakwh_def amarela vermelha1 vermelha2 d_escassez, lags(1) artests(2)
*xtdpdsys implements the Arellano and Bover/Blundell and Bond system estimator, which uses the xtabond moment conditions and moment conditions in which lagged first differences of the dependent variable are instruments for the level equation.
*xtdpdsys l_conskwh_pc temp ibcbr l_tarifakwh_def amarela vermelha1 vermelha2 d_escassez, lags(1) artests(2)

