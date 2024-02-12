######################################################
#GLM para os dados de tags e a condição do rio       #
#Mario Thomé de Souza                                #
#Data: 26/ABR/2020                                   #
######################################################
# 1.Limpar os objeto
rm(list=ls())
ls()
dev.off()
# 2.Mudar diretório
setwd("~/Desktop/PUBLICAÇÃO/MOVEMENT")

# 3.Pacotes
library(RT4Bio)

# 4.Chamar os dados da marcação e recaptura
dados<-read.csv("tabela1.csv", h=T, sep=";") 
dim(dados)
summary(dados)
names(dados)
# 5.Verificar graficamente se existe alguma relação entre as variáveis
#plot(dados)

attach(dados)

# 6.elabore uma hipótese
    
    # O deslocamento do tuncunaré (km) é uma função das variáveis explicativas tempo (day), nível do rio (cm), comprimento do peixes (SL), espécie (spp) e o tipo de deslocamento (movimento): modelo 1

    # A velocidade (km/day) é uma função das variáveis explicativas nível do rio (cm), comprimento do peixes (SL), espécie (spp) e o tipo de deslocamento (movimento): modelo 2


# 7.Eleborar o modelo 1 e checar o tipo de distribuição da variável resposta:
hist(km)

#Considerando a distribuição dos dados de km a análise sugere "Poisson"
modelo1<-glm(km~day+cm+SL+spp+movimentacao, family=poisson)
m.n1<-glm(km~1,family=poisson)
anova(m.n1, modelo1, test="Chisq")

#Há diferença entre os modelos p<0.0001, por isso exlui-se o modelo nulo.

#O valor de Deviance/Resid. Deviance é:

416.89/821.69# = 0.51, o que indica o efeito dos preditores no modelo!
anova(modelo1, test="Chisq")

#O resultado indica que a variável espécie e tempo não são significativas, por isso serão excluídas do modelo.

#Modelo1.1
modelo1.1<-glm(km~cm+SL+movimentacao, family=poisson)
m.n1.1<-glm(km~1,family=poisson)
anova(m.n1.1, modelo1.1, test="Chisq")

#Há diferença entre os modelos p<0.0001, por isso exlui-se o modelo nulo.

#O valor de Deviance/Resid. Deviance é:

408.9/821.69# = 0.50, o que indica o efeito dos preditores no modelo!

anova(modelo1.1, test="Chisq")

#Critica ao modelo
rdiagnostic(modelo1.1)

#Ficou claro que a análise não atende os presupostos do modelo.

#Testar o Resdidual deviance/degrees of freedom
summary(modelo1.1)

#Dispersion parameter for poisson family taken to be 1

412.79/43# = 9.599767, o valor é muito maior, indicando sobredispersão e não atende  a premissa e realizaremos a distribuição quasi-poisson!!

#Modelo 1.2 (quasi-poisson)
modelo1.2<-glm(km~cm+SL+movimentacao, family=quasipoisson)
m.n1.2<-glm(km~1,family=quasipoisson)
anova(m.n1.2, modelo1.2, test="F")

#Há diferença entre os modelos p<0.0001, por isso exlui-se o modelo nulo.

#O valor de Deviance/Resid. Deviance é:
408.9/821.69# = 0.50, o que indica o efeito dos preditores no modelo!

anova(modelo1.2, test="F")
#O resultado indica que a variável comprimento do peixes não é significativa, por isso será excluídas do modelo.

#Modelo 1.3
modelo1.3<-glm(km~movimentacao+cm, family=quasipoisson)
m.n1.3<-glm(km~1,family=quasipoisson)
anova(m.n1.3, modelo1.3, test="F")

#Há diferença entre os modelos p<0.0001, por isso exlui-se o modelo nulo.

#O valor de Deviance/Resid. Deviance é:
408.15/821.69# = 0.50, o que indica o efeito dos preditores no modelo!

anova(modelo1.3, test="F")
#O resultado indica que as variáveis cm e tipo de movimentação são significativas a um (P=0.10), com isso temos o modelo final!!!!!!!!!!!!

#Critica ao modelo
#Testar o Resdidual deviance/degrees of freedom
summary(modelo1.3)

#Dispersion parameter for quasipoisson family taken to be 10.64217
413.54/44# = 9.398636, o valor é próximo, e desta forma iremos aceitar a premissa e realizaremos a distribuição quasi-poisson!!

#O gráfico com os resultados
summary(modelo1.3)
dev.off()
plot(km~cm,ylab="Movement (km)", xlab="Water level (cm)",pch=c(8, 16, 2, 15)[unclass(movimentacao)],ylim=c(0,75),las=1,bty="l")
curve(exp(2.411947-0.001121*x),add=T,lty=1)
curve(exp((2.411947+1.281120)-0.001121 *x),add=T,lty=2)
curve(exp((2.411947-0.024383)-0.001121 *x),add=T,lty=3,col=("black"))
curve(exp((2.411947-3.104239)-0.001121 *x),add=T,lty=4)
legend("topright",c("lateral","channel","marginal","no movement"),pch=c(16, 8, 2, 15),lty=c(2,1,3,4),bty="n")

####
10.03 /821.69 # o nível do rio explica 1 % da variação
398.13/821.69 #o comportamento da movimentação explica 48% da variação

#Análise de contraste da variável do comportamento de movimentação dos tucunarés ("movimentacao"):
#Utilize o pacote RT4Bio.....
coms(qvar="movimentacao",mma=modelo1.3)

#----- Final result of the analysis of contrast -----
#| no movement | canalmarginal | lateral |
#       0.282500      7.005556      25.672143 

###############################################################

# 8. Calcular o GLM com a variável VELOCIDADE DO DESLOCAMENTEO!!!
velo=km/day
hist(velo)

#Considerando a distribuição dos dados de velocidade a análise sugere "Poisson"
#Modelo 2
modelo2<-glm(velo~SL+cm+spp+movimentacao, family=poisson) 
m.n2<-glm(velo~1,family=poisson)
anova(m.n2, modelo2, test="Chisq")

#Há diferença entre os modelos p=0.3, por isso não é possível excluir o modelo nulo.

#Modelo2.1
modelo2.1<-glm(velo~SL+spp+movimentacao+cm, family=quasipoisson) 
m.n2.1<-glm(velo~1,family=quasipoisson)
anova(m.n2.1, modelo2.1, test="F")

#Há diferença entre os modelos p=0.009, por isso exlui-se o modelo nulo.
#O valor de Deviance/Resid. Deviance é:
7.0839/20.614# = 0.34, o que indica o efeito dos preditores no modelo!

anova(modelo2.1, test="F")

#O teste indicou que as variáveis comprimento do peixe e nível da água não interferem no deslocamento do peixe.

#Modelo2.2
modelo2.2<-glm(velo~cm+movimentacao, family=quasipoisson) 
m.n2.2<-glm(velo~1,family=quasipoisson)
anova(m.n2.2, modelo2.2, test="F")

#Há diferença entre os modelos p=0.01, por isso exlui-se o modelo nulo.
#O valor de Deviance/Resid. Deviance é:
5.8249/20.614# = 0.28, o que indica o efeito dos preditores no modelo!

anova(modelo2.2, test="F")

#Critica ao modelo
#Testar o Resdidual deviance/degrees of freedom
summary(modelo2.2)
#Dispersion parameter for quasipoisson family taken to be 0.3995594
14.790/44# = 0.3361364, o valor sugere subdispersão,mas como é próximo iremos aceitar a premissa!

coms(qvar="movimentacao",mma=modelo2.1)
#----- Final result of the analysis of contrast -----
#  | no movementmarginalcanal | lateral |
#  Mean by factor levels considering the analysis of contrast

# no movementmarginalcanal            lateral 
#           0.1342112                0.4183105 

#O gráfico com os resultados
summary(modelo2.1)
plot(velo~cm,ylab="Velocity (km/day)", xlab="Water level (cm)",pch=c(8, 16, 2, 15)[unclass(movimentacao)],ylim=c(0,1.5),las=1,bty="l")
curve(exp(-0.841931-0.002055*x),add=T,lty=1)
curve(exp((-0.841931+0.778044)-0.002055*x),add=T,lty=2)
curve(exp((-0.841931-0.234922)-0.002055*x),add=T,lty=3,col=("black"))
curve(exp((-0.841931-2.892404)-0.002055*x),add=T,lty=4)
legend("topright",c("lateral","channel","marginal","no movement"),pch=c(16, 8, 2, 15),lty=c(2,1,3,4),bty="n")

#Figura 5 para publicação

par(mfrow=c(2,1), mar=c(4,4,2,1))
plot(km~cm,ylab="Movement (km)", xlab="",pch=c(8, 16, 2, 15)[unclass(movimentacao)],ylim=c(0,75),las=1,bty="l")
curve(exp(2.411947-0.001121*x),add=T,lty=1)
curve(exp((2.411947+1.281120)-0.001121 *x),add=T,lty=2)
curve(exp((2.411947-0.024383)-0.001121 *x),add=T,lty=3,col=("black"))
curve(exp((2.411947-3.104239)-0.001121 *x),add=T,lty=4)
legend("topright",c("lateral","channel","marginal","no movement"),pch=c(16, 8, 2, 15),lty=c(2,1,3,4),bty="n")

plot(velo~cm,ylab="Velocity (km/day)", xlab="Water level (cm)",pch=c(8, 16, 2, 15)[unclass(movimentacao)],ylim=c(0,1.5),las=1,bty="l")
curve(exp(-0.841931-0.002055*x),add=T,lty=1)
curve(exp((-0.841931+0.778044)-0.002055*x),add=T,lty=2)
curve(exp((-0.841931-0.234922)-0.002055*x),add=T,lty=3,col=("black"))
curve(exp((-0.841931-2.892404)-0.002055*x),add=T,lty=4)
#legend("topright",c("lateral","channel","marginal","no movement"),pch=c(16, 8, 2, 15),lty=c(2,1,3,4),bty="n")
