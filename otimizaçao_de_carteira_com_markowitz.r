
install.packages("quadprog")

install.packages("PerformanceAnalytics")

install.packages("IntroCompFinR", repos="http://R-Forge.R-project.org")

library(IntroCompFinR)

# Inserindo os retornos 2015-2019

ativos <- c("ITUB4", "ABEV3", "VALE3", "PETR4", "B3SA3")

itub_ret <- c(-0.0294, -0.0524, 0.0159, -0.0276, 0.0007, 0.0264, 0.0313, -0.0847, 0.0779, -0.0440)
abev_ret <- c(-0.0038, -0.0424, -0.0690, 0.0544, -0.0465, 0.0502, 0.0136, 0.0079, 0.0199, -0.0342)
vale_ret <- c(0.0149, -0.0043, 0.0360, -0.04, 0.0714, 0.0309, -0.0065, 0.0082, 0.0331, -0.0222)
petr_ret <- c(-0.0053, 0.0266, -0.0233, -0.04, 0.0102, 0.0241, 0.0547, -0.0293, 0.0422, -0.0679)
b3_ret <- c(-0.0317, -0.0140, -0.0292, 0.0229, 0.0657, 0.0611, 0.0571, 0.0480, 0.0231, 0.0159)


# Criando a matriz de retornos

retornos <- cbind(itub_ret, abev_ret, natu_ret, petr_ret)

retornos

# Construindo a matriz de retorno medio

retorno_medio <- rbind(mean(retornos[,1]), mean(retornos[,2]), mean(retornos[,3]), mean(retornos[,4]))

rownames(retorno_medio) <- ativos

retorno_medio

matriz_cov <- cov(retornos)

rownames(matriz_cov) <- ativos
colnames(matriz_cov) <- ativos

matriz_cov

tx_livre_risco <- 0.06

short_selling <- FALSE

# Vamos calcular a nossa carteira mais eficiente - chamado de Tangency Portfolio

# Carteira Eficience
carteira_eficiente <- tangency.portfolio(retorno_medio, matriz_cov, tx_livre_risco, shorts = short_selling)

carteira_eficiente

ativos

# Calculo da carteira com a menor risco possível
carteira_min_risco <- globalMin.portfolio(retorno_medio, matriz_cov, shorts = short_selling)

carteira_min_risco

# compute portfolio frontier
fronteira_eficiente <- efficient.frontier(retorno_medio, matriz_cov, nport = 40, shorts = short_selling)

fronteira_eficiente

attributes(fronteira_eficiente)

# Visualizaçao da saida

plot(fronteira_eficiente, plot.assets=TRUE, col="blue", pch=16)

points(carteira_min_risco$sd, carteira_min_risco$er, col="green", pch=10, cex=2)
points(carteira_eficiente$sd, carteira_eficiente$er, col="red", pch=10, cex=2)

text(carteira_min_risco$sd, carteira_min_risco$er, labels="Risco Minimo", pos=2)
text(carteira_eficiente$sd, carteira_eficiente$er, labels="Carteira Eficiente", pos=2)

tangente <- (carteira_eficiente$er - tx_livre_risco)/carteira_eficiente$sd
abline(a = tx_livre_risco, b=tangente, col="green", lwd=2)
