library(tidyverse)
library(gridExtra)
set.seed(111)



############### variáveis ##########



t <- 300 # quanto tempo levará a simulação. 

num_bancos <- 10 # número de agentes bancários

num_familias <- 1000 # número de agentes domésticos (dá depósitos)

num_mutuarios <- 2000 # número de agentes mutuários (tomam empréstimos)

periodos_emprestimos <- 1

patrimonio <- 40     # patrimônio inicial para cada banco

juros <- 0.0125 # juros 

ind_adeq_capital <- 0.08 # índice de adequação de capital

ind_reserv_min <- 0.035 # índice de reserva mínima

margem_deposito <- 0.1 # margem para os depósitos. (1 + margem_deposito) X juros

margem_emprestimo <- 0.9  # calculado da mesma forma que margem_deposito

tx_recuperacao <- 0.4 # quanto o banco receberá com empréstimos inadimplentes

prob_min_mudar_banco <- 0.01 # probabilidade mínima de família mudar de banco

prob_max_mudar_banco <- 0.08 # probabilidade máxima

pd_min <- 0.04 # probabilidade mínima de um empréstimo entrar em inadimplência durante 1 período

pd_max <- 0.08 # máxima

venda_reserva_emergencia <- 0.8 # para quanto do valor nominal o banco venderá o empréstimo quando precisar de liquidez para satisfazer as reservas

peso_risco <- function(x){0.5 + 5*x} # como é calculado o peso de risco de um empréstimo. x é PD

reservas_metas <- 2 # multiplicador para ind_reserv_min para reservas ótimas mantidas

lim_super_ind_adeq_capital <- 2 # multiplicador para ind_adeq_capital para adequação ideal de capital mantida

efeito_segur <- 0.04

ind_concentracao <- function(x){0.15*x^2} # menos concorrência - spread maior. Para dar spread adicional no início, adicione interceptação à função


############### Matrizes de agentes ############

familias <- list(valor_depositos = matrix(NA,ncol = num_familias, nrow = t),
                 qual_banco = matrix(NA,ncol = num_familias, nrow = t),
                 prob_mudar_banco = matrix(NA,ncol = num_familias, nrow = t))

mutuarios <- list(valor_emprestimo = matrix(NA,ncol = num_mutuarios, nrow = t),
                  qual_banco = matrix(NA,ncol = num_mutuarios, nrow = t),
                  prob_default = matrix(NA,ncol = num_mutuarios, nrow = t),
                  tx_recuperacao = matrix(NA,ncol = num_mutuarios, nrow = t),
                  custo_capital = matrix(NA,ncol = num_mutuarios, nrow = t),
                  peso_risco = matrix(NA,ncol = num_mutuarios, nrow = t),
                  fim_emprestimo = matrix(NA,ncol = num_mutuarios, nrow = t))

bancos <- list(patrimonio = matrix(NA, ncol = num_bancos, nrow = t),
               soma_depositos = matrix(NA,ncol = num_bancos, nrow = t),
               carteira_emprestimo = matrix(NA,ncol = num_bancos, nrow = t),
               reservas = matrix(NA,ncol = num_bancos, nrow = t),
               prov_perdas_esperadas = matrix(NA,ncol = num_bancos, nrow = t),
               receita = matrix(NA,ncol = num_bancos, nrow = t),
               custo = matrix(NA,ncol = num_bancos, nrow = t),
               renda = matrix(NA,ncol = num_bancos, nrow = t),
               ativo_ponderados_risco = matrix(NA,ncol = num_bancos, nrow = t),
               dividendos = matrix(NA,ncol = num_bancos, nrow = t),
               assist_liquidez = matrix(0,ncol = num_bancos, nrow = t),
               falencia = matrix(0,ncol = num_bancos, nrow = t),
               subcapitalizado = matrix(0,ncol = num_bancos, nrow = t),
               market_share = matrix(0,ncol = num_bancos, nrow = t),
               ativo_total = matrix(0,ncol = num_bancos, nrow = t))

# mudar para uma série temporal para incorporar choques exógenos posteriormente
ind_adeq_capital <- rep(ind_adeq_capital, t)
ind_reserv_min <- rep(ind_reserv_min, t)
juros <- rep(juros, t)
margem_deposito <- rep(margem_deposito, t)
margem_emprestimo <- rep(margem_emprestimo, t)

############## Valores Iniciais #############



familias$valor_depositos[1,] <- 1
familias$qual_banco[1,] <- sample(1:num_bancos, size = num_familias, replace = TRUE)
familias$prob_mudar_banco[1,] <- runif(ncol(familias$prob_mudar_banco), prob_min_mudar_banco, prob_max_mudar_banco)
for (o in 2:t) {
  familias$prob_mudar_banco[o,] <- familias$prob_mudar_banco[1,]
}

mutuarios$prob_default[1:t,] <- runif(ncol(mutuarios$prob_default), pd_min, pd_max)
mutuarios$tx_recuperacao[1,] <- tx_recuperacao
mutuarios$peso_risco[1,] <- peso_risco(mutuarios$prob_default[1,]) # 
mutuarios$qual_banco[1,] <- sample(1:num_bancos, size = num_mutuarios, replace = TRUE)
mutuarios$fim_emprestimo[1,] <- sample(1:periodos_emprestimos, size = num_mutuarios, replace = TRUE)

# calculando o custo inicial de capital ------- o juros aqui é a tx de retorno livre de risco
for (q in 1:num_mutuarios) {
  
  mutuarios$custo_capital[1,q] <- (1 + ((margem_emprestimo[1]+1)*juros[1]) - mutuarios$prob_default[1,q] * mutuarios$tx_recuperacao[1,q])/(1-mutuarios$prob_default[1,q])
  
}

bancos$patrimonio[1,] <- patrimonio

for (w in 1:num_bancos) {
  
  # soma de depósitos - Fazendo apenas para linha 1 e para todas as colunas 
          # O processo se repetirar logo mais para p-1 linhas e para todas as colunas (e)
  bancos$soma_depositos[1,w] <- sum(familias$valor_depositos[1,which(familias$qual_banco[1,] == w)])
  
  # inicializar carteira de empréstimos que esteja em conformidade com os regulamentos
  # total de ativos ponderados pelo risco no mercado para um determinado banco ------- 
  risco_potencial_mutuario_cart_emprest <- mutuarios$peso_risco[1,which(mutuarios$qual_banco[1,] == w)]
  
  # número potencial de empréstimos que o banco possui depósitos para emprestar com reserva ------- inicio de 
          # fundos máximos a emprestar (dadas as reservas mínimas e os requisistos de adequação de capital)
  reserva_potencial_Market_Share <- bancos$soma_depositos[1,w] * (1-ind_reserv_min[1])
  
  # número potencial de empréstimos que estarão em conformidade com os regulamentos 
          #-------  no bancos$patrimonio[1,w]+2) < bancos$patrimonio[1,w])[2]
  participacao_potencial_mercado <- table(c(cumsum(risco_potencial_mutuario_cart_emprest * ind_adeq_capital[1]),bancos$patrimonio[1,w]+2) < bancos$patrimonio[1,w])[2] # o valor patrimonial adicionado em c() é porque sempre precisa haver um exemplo falso
  
  # número de empréstimos que atendem a dois requisitos (reservas e requisitos de capital) 
          #-------  - completo "potencial de emprestimos"
  potencial_emprestimos <- floor(min(reserva_potencial_Market_Share, participacao_potencial_mercado))
  
  # conceder empréstimos a esses mutuários sem alguma margem de segurança
  mutuarios$valor_emprestimo[1,which(mutuarios$qual_banco[1,] == w)] <- c(rep(1,(potencial_emprestimos-5)),
                                                                    rep(0,(length(risco_potencial_mutuario_cart_emprest)+5-potencial_emprestimos)))
  # soma dos empréstimos ---- obs: dos mutuarios "mutuarios$valor_emprestimo" para os bancos "bancos$carteira_emprestimo[1,w]"
          # aparece em   
  bancos$carteira_emprestimo[1,w] <- sum(mutuarios$valor_emprestimo[1,which(mutuarios$qual_banco[1,] == w)])
  
  # calcular ativos ponderados pelo risco parte da equação (5) obs: entre parenteses da equação em questão
  bancos$ativo_ponderados_risco[1,w] <- sum(mutuarios$peso_risco[1,which(mutuarios$qual_banco[1,] == w)]*mutuarios$valor_emprestimo[1,which(mutuarios$qual_banco[1,] == w)])
  
  # provisões para perdas esperadas (EL)--------
          # EL = PD * LGD * EAD
          # LGD = 1 - tx de recuperação
  bancos$prov_perdas_esperadas[1,w] <- sum(mutuarios$prob_default[1,which(mutuarios$qual_banco[1,] == w)] * mutuarios$valor_emprestimo[1,which(mutuarios$qual_banco[1,] == w)] * (1-mutuarios$tx_recuperacao[1,which(mutuarios$qual_banco[1,] == w)]))
}

bancos$custo[1,] <- 0
bancos$receita[1,] <- 0
bancos$dividendos[1,] <- 0
bancos$renda[1,] <- 0

# Equação de reservas bancarias ------ 
          # obs: Reservas = ( Somatorio dos depositos * tx de juros dos depositos ) - 
          # Variação da prov. das perdas esperadas - (Somatório conj. emprestimos* tx de juros paga pelo empréstimo)
bancos$reservas[1,] <- bancos$soma_depositos[1,] - bancos$carteira_emprestimo[1,]

bancos$market_share[1, ] <- 1/num_bancos



########### Choque ###########

# choque na taxa de juros

choque_juros <- T
tempo_mudanca_juros <- 100
novo_juros <- 0.04

if(choque_juros == TRUE){
  juros[tempo_mudanca_juros:t] <- juros[tempo_mudanca_juros:t] + novo_juros 
}



########## início da simulação #########



# barra de progresso e cronômetro
progress.bar <- winProgressBar("Simulating banking sector", "0% Done", 0, 1, 0)
ptm <- proc.time()

for (p in 2:t) {
  
  ########### mudança aleatória dos depósitos ##########
  
  mudanca_banco <- rep(0, ncol(familias$qual_banco))# matriz vazia
  
  # calcula o "efeito de segurança" para um determinado banco
  qual_seguro <- ifelse(bancos$renda[p-1,] < 0, efeito_segur,0)
  
  # gera aleatoriamente 1 ou 0 dada a probabilidade de retirada de uma família
  familias$qual_banco[p,] <- familias$qual_banco[p-1,]
  for (u in 1:num_familias) {
    
    nova_prob_mudar_banco <- familias$prob_mudar_banco[1,u] + qual_seguro[familias$qual_banco[p,u]] # adiciona efeito de segurança 
    mudanca_banco[u] <- sample(c(1,0),size = 1,replace = TRUE, prob = c(nova_prob_mudar_banco, 1-nova_prob_mudar_banco))
  }
  
  # gerando aleatoriamente um novo banco para cada depósito mudar (a família pode mudar de banco para o mesmo banco)
  familias$qual_banco[p,which(mudanca_banco == 1)] <- sample(which(bancos$falencia[p-1,] == 0), replace = TRUE, size = length(which(mudanca_banco == 1))) 
  
  # alteração do total de depósitos
            # faz parte das equaçãoes 
  familias$valor_depositos[p,] <- familias$valor_depositos[p-1,]
  for (e in which(bancos$falencia[p-1,] == 0)) {
    bancos$soma_depositos[p,e] <- sum(familias$valor_depositos[p,which(familias$qual_banco[p,] == e)])
  }

  ############ inadimplência de empréstimo ##########
  
  # mesmo status de empréstimo do período anterior
  mutuarios$valor_emprestimo[p,] <- mutuarios$valor_emprestimo[p-1,]
  qual_inadimplencia <- rep(0, ncol(mutuarios$valor_emprestimo)) # matriz vazia
  # gera aleatoriamente 1 ou 0 dado um PD de um empréstimo para cada empréstimo emprestado
  for (i in which(mutuarios$valor_emprestimo[p,] == 1)) {
    qual_inadimplencia[i] <- sample(c(1,0), size = 1,replace = TRUE, prob = c(mutuarios$prob_default[p,i], 1-mutuarios$prob_default[p,i]))
  }
  # empréstimos inadimplentes não valem mais nada (exceto a recuperação inicial, calculada posteriormente)
  mutuarios$valor_emprestimo[p,which(qual_inadimplencia == 1)] <- 0  # Para a linha 1 de "qual_inadimplencia"
  
  # inadimplência de empréstimos anulados
  amortizacao <- mutuarios$valor_emprestimo[p,]
  amortizacao[which(qual_inadimplencia == 1)] <- tx_recuperacao # "*(1-rr)" 
  
  # calculando a perda de crédito -------- 
  
  mutuarios$qual_banco[p,] <- mutuarios$qual_banco[p-1,]
  for (r in which(bancos$falencia[p-1,] == 0)) {
    bancos$carteira_emprestimo[p,r] <- sum(amortizacao[which(mutuarios$qual_banco[p,] == r)])
  }
  
  ## ABBA é CLoss 
  perda_emprestimo_t <- bancos$carteira_emprestimo[p,]-bancos$carteira_emprestimo[p-1,]
  
  # atualização das provisões para perdas esperadas (EL)--------  - obs: Para o restante
            # EL = PD * LGD * EAD
            # LGD = 1 - tx de recuperação
  mutuarios$tx_recuperacao[p,] <- mutuarios$tx_recuperacao[p-1,]
  for (a in which(bancos$falencia[p-1,] == 0)) {
    
    bancos$prov_perdas_esperadas[p,a] <- sum(mutuarios$prob_default[p,which(mutuarios$qual_banco[p,] == a)] * 
                                               mutuarios$valor_emprestimo[p,which(mutuarios$qual_banco[p,] == a)] * 
                                               (1-mutuarios$tx_recuperacao[p,which(mutuarios$qual_banco[p,] == a)]))
    
  }
  
  ############ Complementação da Reserva ##########
  
  # ajustando as reservas à variação dos depósitos  
            # obs: Reservas = ( Somatorio dos depositos * tx de juros dos depositos ) - 
            # Variação da prov. das perdas esperadas - (Somatório conj. emprestimos* tx de juros paga pelo empréstimo)
  bancos$reservas[p,] <- bancos$soma_depositos[p,] - bancos$carteira_emprestimo[p,] + bancos$patrimonio[p-1,] 
  
  
  # índice mininmo de reserva ------- 
            # qual banco deve vender empréstimos por causa das reservas mínimas (1 se cumprirem os requisitos, 0 caso contrário)
  satisfazer_reserva <- ifelse(bancos$reservas[p,] < (bancos$soma_depositos[p,] * ind_reserv_min[p]),0,1) # ------- 
  perda_venda_forcada <- rep(0,num_bancos) # matriz vazia para cálculo adicional de PnL
  
  for (d in which(satisfazer_reserva == 0)) {
    if(bancos$falencia[p-1,d] == 1){next()}
    num_min_emprestimos_venda <- ceiling((bancos$soma_depositos[p,d]*ind_reserv_min[p] - 
                                            bancos$reservas[p,d])/venda_reserva_emergencia) # número mínimo de empréstimos para vender
    perda_venda_forcada[d] <- ceiling(bancos$soma_depositos[p,d]*ind_reserv_min[p] - 
                                        bancos$reservas[p,d]) * (1-venda_reserva_emergencia) # para cálculo adicional de PnL
    
    emprestimos_bancarios <- mutuarios$valor_emprestimo[p,which(mutuarios$qual_banco[p,] == d & mutuarios$valor_emprestimo[p,] == 1)]
    mutuarios$valor_emprestimo[p,which(mutuarios$qual_banco[p,] == d & mutuarios$valor_emprestimo[p,] == 1)] <- c(rep(1,length(emprestimos_bancarios) - 
                                                                                                                        num_min_emprestimos_venda),
                                                                                                      rep(0,num_min_emprestimos_venda))
  }
  # repetindo o cálculo da reserva após o ajuste
        #  bancos$reservas[p,] <- bancos$soma_depositos[p,] - bancos$carteira_emprestimo[p,] + bancos$patrimonio[p-1,] 
  bancos$reservas[p,] <- bancos$soma_depositos[p,] - bancos$carteira_emprestimo[p,] + bancos$patrimonio[p-1,] 
  
  
  ########### calculando PnL e balanço patrimonial ###############
  
  # alteração do spread (ou seja, rentabilidade dos bancos) dada a consolidação/concentração do mercado
  margem_deposito[p] <- margem_deposito[p] - ind_concentracao((num_bancos-sum(bancos$falencia[p]))/num_bancos)
   margem_emprestimo[p] <- margem_emprestimo[p] + ind_concentracao((num_bancos-sum(bancos$falencia[p]))/num_bancos)
  
  ## receita do banco
  # receita de juros
  
  for (l in 1:num_mutuarios) {
    
    mutuarios$custo_capital[p,l] <- ((1 + ((margem_emprestimo[p]+1)*juros[p]) - mutuarios$prob_default[2,l] * mutuarios$tx_recuperacao[p,l])/(1-mutuarios$prob_default[2,l]))
    
  }
  
  for (y in which(bancos$falencia[p-1,] == 0)) {
    
    # custo de capital multiplicado pelo valor do empréstimo ---------- 
    receita_loop <- sum(mutuarios$custo_capital[p-1, mutuarios$qual_banco[p,] == y] * mutuarios$valor_emprestimo[p, mutuarios$qual_banco[p,] == y]) - sum(mutuarios$valor_emprestimo[p,mutuarios$qual_banco[p,] == y])  # menos comprimento porque usamos uma fórmula 1+juros
    # somando os juros das reservas investidas durante o período anterior e menos a venda imediata 
    receita_loop <- receita_loop + (bancos$reservas[p-1,y] * juros[p-1])
    
    bancos$receita[p,y] <- receita_loop
    
  }
  
  ## custos do banco ------- 
  # custo de depósito
  for (f in which(bancos$falencia[p-1,] == 0)) {
    bancos$custo[p,f] <- sum(familias$valor_depositos[p-1, which(familias$qual_banco[p,] == f)] * juros[p] * (1+margem_deposito[p])) + perda_venda_forcada[f]
  }
  # custo de assistência à liquidez
  bancos$custo[p,] <- bancos$custo[p,] + (bancos$assist_liquidez[p-1,] * juros[p])
  
  # lucro líquido dos bancos
  bancos$renda[p,] <- (bancos$receita[p,] - bancos$custo[p,]) + perda_emprestimo_t - (bancos$prov_perdas_esperadas[p-1,] - bancos$prov_perdas_esperadas[p,]) - perda_venda_forcada
  # Colocar o "-perda_emprestimo_t" e "-reservas"
  bancos$patrimonio[p,] <- bancos$patrimonio[p-1,] + bancos$renda[p,]
  
  # dividendos
  mutuarios$peso_risco[p,] <-  peso_risco(mutuarios$prob_default[p,])
  
  for (g in which(bancos$falencia[p-1,] == 0)) {
    # um banco pode dar dividendos se o patrimônio líquido exceder 
            # o índice de adequação de capital em 1,5 do seu mínimo  ----- 
    bancos$ativo_ponderados_risco[p,g] <- sum(mutuarios$valor_emprestimo[p,which(mutuarios$qual_banco[p,] == g)] * mutuarios$peso_risco[p,which(mutuarios$qual_banco[p,] == g)])
    maximo_dividendo <- bancos$patrimonio[p,g] - bancos$ativo_ponderados_risco[p,g]*(ind_adeq_capital[p]*lim_super_ind_adeq_capital)
    
    # segundo requisito é que o banco possa dar dividendos se a reserva mínima permitir
    maximo_dividendo_depositos <- bancos$reservas[p,g] - (bancos$soma_depositos[p,g] * (ind_reserv_min[p] * reservas_metas))
    
    # se um deles não permitir dar dividendos (ou seja, 0), então dê zero
    bancos$dividendos[p,g] <- min(pmax(c(maximo_dividendo, maximo_dividendo_depositos),0))
    bancos$patrimonio[p,g] <- bancos$patrimonio[p,g] - min(pmax(c(maximo_dividendo, maximo_dividendo_depositos),0))
  }
  
  # atualizar reservas após diminuir o patrimônio líquido em um dividendo
  bancos$reservas[p,] <- bancos$patrimonio[p,] + bancos$soma_depositos[p,] - bancos$carteira_emprestimo[p,]
  
  ########### verificação de falência ou subcapitalização e cálculo de assistência de liquidez ########
  
  #verificando falência
  
  bancos$falencia[p,] <- ifelse(bancos$patrimonio[p,] <= 0,1,0)
  
  for (k in which(bancos$falencia[p,] == 1)) {
    
    # deixa patrimônio líquido negativo para o resto da simulação
    bancos$patrimonio[p:t,k] <- bancos$patrimonio[p,k]
    
    # desalavancagem de depósitos
    bancos$soma_depositos[p:t,k] <- 0
    familias$valor_depositos[p, which(familias$qual_banco[p,] == k)] <- 0
    
    # empréstimos para venda imediata
    bancos$carteira_emprestimo[p:t,k] <- 0
    mutuarios$valor_emprestimo[p, which(mutuarios$qual_banco[p,] == k)] <- 0
    
    # permitir que outros bancos concedam empréstimos aos seus clientes
    mutuarios$qual_banco[p, which(mutuarios$qual_banco[p,] == k)] <- sample(which(bancos$falencia[p,] == 0),
                                                                            size = length(mutuarios$qual_banco[p, which(mutuarios$qual_banco[p,] == k)]),
                                                                            replace = TRUE)
    
    #permitir que outros bancos recebam depósitos de seus clientes
    familias$qual_banco[p,which(familias$qual_banco[p,] == k)] <- sample(which(bancos$falencia[p,] == 0),
                                                                             size = length(familias$qual_banco[p,which(familias$qual_banco[p,] == k)]),
                                                                             replace = TRUE)
  }
  
  # verificando a subcapitalização
  bancos$subcapitalizado[p,] <-  ifelse(bancos$patrimonio[p,] < (bancos$ativo_ponderados_risco[p,] * ind_adeq_capital[p]), 1,0)
  
  for (j in which(bancos$subcapitalizado[p,] == 1)) {
    
    if(bancos$falencia[p,j] == 1){next()}
    
    necessid_liquidez <- (bancos$ativo_ponderados_risco[p,j]*ind_adeq_capital[p]) - bancos$patrimonio[p, j]
    
    bancos$assist_liquidez[p,j] <- necessid_liquidez
    
  }
  
  ############# emprestando novos empréstimos ##########
  
  for (h in which(bancos$falencia[p,] == 0)) {
    
    excesso_reservas <- bancos$reservas[p,h] - (bancos$soma_depositos[p,h] * ind_reserv_min[p])
    excesso_capital <- (bancos$patrimonio[p,h] - (bancos$ativo_ponderados_risco[p,h] * ind_adeq_capital[p]))/mean(mutuarios$peso_risco[p,])
    if(any(c(excesso_capital, excesso_reservas) < 0)){next()}
    
    potencial_emprestimos <- min(pmax(c(excesso_reservas, excesso_capital), 0))
    # se o mercado potencial for menor que os empréstimos potenciais, omitir
    if(length(mutuarios$valor_emprestimo[p,mutuarios$valor_emprestimo[p,] == 0 & mutuarios$qual_banco[p,] == h]) < floor(potencial_emprestimos)){
      potencial_emprestimos <- length(mutuarios$valor_emprestimo[p,mutuarios$valor_emprestimo[p,] == 0 & mutuarios$qual_banco[p,] == h])-2
    }
    # substitua os empréstimos do banco que são 0 por 1s dados aleatoriamente.
    mutuarios$valor_emprestimo[p,mutuarios$valor_emprestimo[p,] == 0 & mutuarios$qual_banco[p,] == h] <- sample(c(rep(1, floor(potencial_emprestimos)),
                                                                                                      rep(0,length(mutuarios$valor_emprestimo[p,mutuarios$valor_emprestimo[p,] == 0 & mutuarios$qual_banco[p,] == h]) - floor(potencial_emprestimos))),
                                                                                                    replace = FALSE)
    # alterar carteira de empréstimos
    bancos$carteira_emprestimo[p,h] <- sum(mutuarios$valor_emprestimo[p, which(mutuarios$qual_banco[p,] == h)]) 
    
  }
  
  # atualizar reservas após aumentar a carteira de empréstimos
  bancos$reservas[p,] <- bancos$patrimonio[p,] + bancos$soma_depositos[p,] - bancos$carteira_emprestimo[p,]
  
  
  for (s in 1:num_bancos) {
    
    
    bancos$ativo_total[p, ] <- bancos$patrimonio[p-1,] + bancos$soma_depositos [p-1,] + 
      bancos$carteira_emprestimo [p-1,] + bancos$reservas [p-1,]
    
  }
  
  
  
  for (z in 1:num_bancos) {
    
    
    bancos$market_share[p, ] <- bancos$carteira_emprestimo[p-1,] / sum(bancos$carteira_emprestimo[p,])
    
  }
  
  # fim da barra de progresso
  percentage <- p/t
  setWinProgressBar(progress.bar, percentage, "Simulating banking sector ",
                    sprintf("%d%% Done", round(100 * percentage)))
  
  
  
}


close(progress.bar)
proc.time()-ptm


############ visualização ############

bancos <- lapply(bancos, as.data.frame)
familias <- lapply(familias, as.data.frame)
mutuarios <- lapply(mutuarios, as.data.frame)

#quando foi o choque
choques <- data.frame(conj_choque = c(choque_juros), 
                      ocorrencia_choque = c(tempo_mudanca_juros))                

qual_choque <- which(c(choque_juros))

# total de determinada variável para o agente

market_share  <- bancos$market_share%>%
  mutate(total = rowSums(.[,1:ncol(.)]))%>%
  ggplot(aes(x = 1:nrow(.), y = total))+
  geom_line()+
  labs(title = "Market share")+
  geom_vline(xintercept = c(choques[qual_choque,2]), linetype=2, 
             color = qual_choque+9, size=1)+
  xlab(label = "t")

ativo_total  <- bancos$ativo_total%>%
  mutate(total = rowSums(.[,1:ncol(.)]))%>%
  ggplot(aes(x = 1:nrow(.), y = total))+
  geom_line()+
  labs(title = "ativo_total")+
  geom_vline(xintercept = c(choques[qual_choque,2]), linetype=2, 
             color = qual_choque+9, size=1)+
  xlab(label = "t")

receita  <- bancos$receita%>%
  mutate(total = rowSums(.[,1:ncol(.)]))%>%
  ggplot(aes(x = 1:nrow(.), y = total))+
  geom_line()+
  labs(title = "Receita")+
  geom_vline(xintercept = c(choques[qual_choque,2]), linetype=2, 
             color = qual_choque+9, size=1)+
  xlab(label = "t")

custo  <- bancos$custo%>%
  mutate(total = rowSums(.[,1:ncol(.)]))%>%
  ggplot(aes(x = 1:nrow(.), y = total))+
  geom_line()+
  labs(title = "custo")+
  geom_smooth()+
  geom_vline(xintercept = c(choques[qual_choque,2]), linetype=2, 
             color = qual_choque+9, size=1)+
  xlab(label = "t")

reservas  <- bancos$reservas%>%
  mutate(total = rowSums(.[,1:ncol(.)]))%>%
  ggplot(aes(x = 1:nrow(.), y = total))+
  geom_line()+
  labs(title = "Reservas")+
  geom_vline(xintercept = c(choques[qual_choque,2]), linetype=2, 
             color = qual_choque+9, size=1)+
  xlab(label = "t")

renda  <- bancos$renda%>%
  mutate(total = rowSums(.[,1:ncol(.)]))%>%
  ggplot(aes(x = 1:nrow(.), y = total))+
  geom_line()+
  labs(title = "Renda")+
  geom_smooth(level = 0.9999, method = "lm")+
  geom_vline(xintercept = c(choques[qual_choque,2]), linetype=2, 
             color = qual_choque+9, size=1)+
  xlab(label = "t")

carteira_emprestimo  <- bancos$carteira_emprestimo%>%
  mutate(total = rowSums(.[,1:ncol(.)]))%>%
  ggplot(aes(x = 1:nrow(.), y = total))+
  geom_line()+
  labs(title = "carteira_emprestimo")+
  geom_vline(xintercept = c(choques[qual_choque,2]), linetype=2, 
             color = qual_choque+9, size=1)

patrimonio  <- bancos$patrimonio%>%
  mutate(total = rowSums(.[,1:ncol(.)]))%>%
  ggplot(aes(x = 1:nrow(.), y = total))+
  geom_line()+
  labs(title = "Patrimônio")+
  geom_vline(xintercept = c(choques[qual_choque,2]), linetype=2, 
             color = qual_choque+9, size=1)+
  xlab(label = "t")

lucratividade  <- bancos$renda/bancos$patrimonio
lucratividade  <- lucratividade%>%
  mutate(total = rowSums(.[,1:ncol(.)]))%>%
  ggplot(aes(x = 1:nrow(.), y = total))+
  geom_line()+
  labs(title = "Lucratividade")+
  geom_smooth()+
  geom_vline(xintercept = c(choques[qual_choque,2]), linetype=2, 
             color = qual_choque+9, size=1)+
  xlab(label = "t")

dividendos  <- bancos$dividendos%>%
  mutate(total = rowSums(.[,1:ncol(.)]))%>%
  ggplot(aes(x = 1:nrow(.), y = total))+
  geom_line()+
  labs(title = "Dividendos")+
  geom_smooth()+
  geom_vline(xintercept = c(choques[qual_choque,2]), linetype=2, 
             color = qual_choque+9, size=1)+
  xlab(label = "t")

prov_perdas_esperadas  <- bancos$prov_perdas_esperadas%>%
  mutate(total = rowSums(.[,1:ncol(.)]))%>%
  ggplot(aes(x = 1:nrow(.), y = total))+
  geom_line()+
  labs(title = "Provisão de Perdas Esperadas")+
  geom_smooth()+
  geom_vline(xintercept = c(choques[qual_choque,2]), linetype=2, 
             color = qual_choque+9, size=1)+
  xlab(label = "t")


grid.arrange(patrimonio,lucratividade, dividendos, renda)

grid.arrange(reservas, receita, prov_perdas_esperadas, market_share)


