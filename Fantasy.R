    library(tidyverse)
    library(rvest)
    library(readxl)
    library(writexl)
    library(randomForest)
    library(arules)
    
    link <- read_html("https://landofbasketball.com/year_by_year_stats/2022_2023_double_doubles_rs.htm")
  
    dados <- link %>% html_elements("table") %>% html_table() %>% .[[1]]
    
    colnames(dados) <- dados[2, ]
    
    dados <- dados[-c(1, 2), ]
    
   dados$`Player
			    (Team)`<- dados$`Player
			    (Team)`%>%
      str_remove_all(" ") %>%
      str_remove_all("\n\t\n\t\\(.*\\)") %>%
      str_replace_all("([a-z])([A-Z])", "\\1 \\2")
    
    colnames(dados)[2] <- "Player"
    
    dados <- dados %>% filter(`Double-Doubles` != "Double-Doubles" & `Double-Doubles` != "2,162")
  
    dados <- dados %>% select(-c(1,4,6:9))
    
    dados <- dados %>% mutate(across(c(2,3), as.numeric))
    
    write_xlsx(dados, "dd_td_2022-2023.xlsx")
    
    fantasy <- read_excel("Nba_2022-2023.xlsx")
    
    
    
    fantasy <- fantasy %>% mutate(across(c(8:30), as.numeric))
    
    nomes_errados <- setdiff(dados$Player, fantasy$Player)
    
    nomes_certos <- c("Alperen Sengün", "LeBron James", "LaMelo Ball", "Fred VanVleet",
      "DeMar DeRozan", "CJ McCollum", "T.J. McConnell", "Donte DiVincenzo",
      "Xavier Tillman", "RJ Barrett", "Jalen McDaniels", "P.J. Washington",
      "OG Anunoby", "Willy Hernangómez", "DeAndre Jordan", "Zach LaVine","Dennis Schröder", "Juancho Hernangómez", "Caris LeVert",
      "Théo Maledon")
    
    dados$Player <- ifelse(dados$Player %in% nomes_errados, 
                            nomes_certos[match(dados$Player, nomes_errados)], 
                            dados$Player)
  
  
  fantasy <- full_join(dados, fantasy, by = "Player", relationship = "many-to-many")
  
  pessoas <- subset(fantasy, Tm == "TOT")
  
  fantasy <- subset(fantasy, !(Player %in% pessoas$Player))
  
  fantasy <- rbind(fantasy, pessoas)
  
  fantasy$`Double-Doubles` <- replace(fantasy$`Double-Doubles`, is.na(fantasy$`Double-Doubles`),0)
  fantasy$`Triple-Doubles` <- replace(fantasy$`Triple-Doubles`, is.na(fantasy$`Triple-Doubles`),0)
  
  calcular_pontos <- function(fantasy){
  fantasy <- fantasy %>% mutate(across(-c(1:9), ~round(.,3)))
  pesos <- c(-0.5,-1,-0.5,.5,1.5,2.5,2.5,-1,8,15)
  fantasy_estatisticas <- fantasy %>%
    group_by(Player) %>%
    reframe(Pos, Tm, G, MP, `Double-Doubles`, `Triple-Doubles`, FG, FGA,`FG%`, FT, FTA,
            `FT%`,`3P`, `3PA`, `3P%`, AST, TRB, STL, BLK, TOV, PTS, ORB,
            pontos_media = ((FG) +
                              ((FGA - (FGA * `FG%`)) * pesos[1]) +
                              (FT) +
                              ((FTA - (FTA * `FT%`)) * pesos[2]) +
                              (`3P`) +
                              ((`3PA` - (`3PA` * `3P%`)) * pesos[3]) +
                              (TRB) +
                              (pesos[4] * ORB) + 
                              (AST * pesos[5]) +
                              (STL * pesos[6]) +
                              (BLK * pesos[7]) +
                              (TOV * pesos[8]) +
                              (PTS) +
                              ((`Double-Doubles`/G) * pesos[9]) +
                              ((`Triple-Doubles`/G) * pesos[10]))) %>%
    mutate(ponto_total = pontos_media * G, arremessos_errados = ((FGA - (FGA * `FG%`)) * pesos[1]), lance_livres_errados = ((FTA - (FTA * `FT%`)) * pesos[2]),
           arremessos_de_3_errados = ((`3PA` - (`3PA` * `3P%`)) * pesos[3]), rebotes = TRB, rebotes_ofensivo = pesos[4] * ORB, assistencia = AST * pesos[5],
           roubo_de_bola = STL * pesos[6], bloqueio = BLK * pesos[7], erro = TOV * pesos[8], double_double = (`Double-Doubles`/G) * pesos[9],
           triple_double = (`Triple-Doubles`/G) * pesos[10]) %>%
    arrange(desc(ponto_total)) %>% select(1:6,17,16,18,19,20,21,everything()) %>% select(-c(13:22)) %>% mutate(across(c(7:25), ~ round(., 3)))
  }   
  mean_per_position <- fantasy %>%
    group_by(Pos) %>%
    filter(MP > 10, G > 10 & `3PA` > 0.5) %>% 
    summarise( Players = n(),
     `media_3pt%` = sum(`3P` * G)/sum(`3PA` * G),
      media_3ptA = sum(`3PA` * G) / sum(G),
      media_3ptmade = sum(`3P` * G)/ sum(G),
      field_goal = sum(FG* G)/ sum(FGA * G),
      field_goal_atempt = sum(FGA * G)/ sum(G),
      field_goal_made = sum(FG * G)/ sum(G),
      turnover = sum(TOV * G)/sum(G),
      block = sum(BLK * G)/ sum(G),
      steal = sum(STL * G)/ sum(G),
     `free_throw%`= sum(FT * G)/sum(FTA * G),
      free_throw_atempt = sum(FTA * G)/ sum(G),
      free_throw_made = sum(FT * G)/ sum(G),
      rebound = sum(TRB * G)/ sum(G),
      defensive_rebound = sum(DRB * G)/ sum(G),
      ofensive_rebound = sum(ORB * G)/ sum(G),
      double_double = sum(`Double-Doubles` * G)/ sum(G),
      triple_double = sum(`Triple-Doubles` * G)/ sum(G),
      points = sum(PTS * G)/ sum(G)) %>% mutate(across(-c(1), ~round(.,3)))
      
  points_per_position<- fantasy_estatisticas %>% group_by(Pos) %>% filter(MP > 10) %>% 
    summarise(pontos_media = sum(pontos_media * G)/ sum(G), 
              ponto_total = sum(ponto_total)) %>% mutate(across(c(2,3), ~round(.,3)))    
  
  fantasy_points<- merge(mean_per_position, points_per_position) 
  
  #write_xlsx(fantasy_estatisticas, "fantasy_2023.xlsx")
  
  #write_xlsx(fantasy_points, "fantasy_pontos_medias.xlsx")
  #top_15_pg<- fantasy_estatisticas %>% filter(Pos == "PG") %>% head(15)
  #top_15_sg <- fantasy_estatisticas %>% filter(Pos == "SG")%>% head(15)
  #top_15_sf <- fantasy_estatisticas %>% filter(Pos == "SF") %>% head(15)
  #top_15_pf <- fantasy_estatisticas %>% filter(Pos == "PF")%>% head(15)
  #top_15_C <- fantasy_estatisticas %>% filter(Pos == "C") %>% head(15)
  
  #top_100 <- head(fantasy_estatisticas, 100)
  
  #top_100 <- subset(fantasy, Player %in% top_100$Player)
  
  min_e_max <- function(Pos) {
    min_and_max <- fantasy %>%
      group_by(Pos) %>%
      filter(Pos == Pos & G > 10) %>%
      summarise(players = n(), across(-c(1, 4:10), list(min = min, max = max)))
    return(min_and_max)
  }
  
  posicoes <- c("PG", "SG", "SF", "PF", "C")
  
  lista_dataframes <- lapply(posicoes, function(posicao) {
    # Filtra os dados de acordo com a posição
    fantasy %>%
      filter(MP > 10 & Pos == posicao & G > 10)
  })
  
  
  atributos_fantasy <- colnames(select(fantasy,-c(1,4:11,20,31)))
  
  
  discretizar <- function(filtro, coluna) {
    desvio_padrao <- sd(filtro[[coluna]])
    media <- mean(filtro[[coluna]])
    limite_inferior <- min(filtro[[coluna]])
    limite_superior <- max(filtro[[coluna]])
    cortes <- (limite_superior - limite_inferior)/3
    breaks <- c(limite_inferior - 0.01, 
                limite_inferior + cortes, 
                limite_inferior + 2*cortes, 
                limite_superior)
    
    lev <- cut(filtro[[coluna]], breaks = breaks, ordered = TRUE)
    levels(lev) <- c("ruim", "bom", "ótimo")
    
    resultados_posicao <- data.frame(Player = filtro[["Player"]],Discretizado = lev, x = filtro[[coluna]])
    colnames(resultados_posicao)[3] <- coluna
    colnames(resultados_posicao)[2] <- paste("discretizado",coluna, sep = "_")
    return(resultados_posicao)
  }
  
  
  # Crie uma lista para armazenar os resultados
  
  resultados_posicao <- list()
  # Loop através das posições
  for (i in 1:5) {
    lista_resultados <- list()  # Lista para resultados de cada posição
    
    # Loop através das variáveis a serem discretizadas
    for (j in 1:length(atributos_fantasy)) {
      coluna <- atributos_fantasy[j]
      filtro <- lista_dataframes[[i]]
      
      # Chame a função de discretização e armazene o resultado na lista
      resultado <- discretizar(filtro, coluna)
      lista_resultados[[coluna]] <- resultado
    }
    
    # Armazene os resultados da posição atual na lista principal
    resultados_posicao[[i]] <- lista_resultados
  }
  

  resultado_total <- do.call(bind_rows, unlist(resultados_posicao, recursive = FALSE)) %>%
    group_by(Player) %>%
    summarize_all(function(x) paste(na.omit(x), collapse = ", ")) %>% mutate_at(vars(seq(3, 43, by = 2)), as.numeric)
  
  fantasy_discretizados <- left_join(resultado_total,select(fantasy_estatisticas,1:5,14,15), by = "Player") %>%
    select(1,44:49, everything()) %>%
    arrange(desc(pontos_media))
  
 
  limite_inferior <- min(fantasy_discretizados$pontos_media)
  limite_superior <- max(fantasy_discretizados$pontos_media)
  cortes <- (limite_superior - limite_inferior)/3
  breaks <- c(limite_inferior - 0.01, 
              limite_inferior + cortes, 
              limite_inferior + 2*cortes, 
              limite_superior)
  
  lev <- cut(fantasy_discretizados$pontos_media, breaks = breaks, ordered = TRUE)
  levels(lev) <- c("ruim", "bom", "ótimo")
  
  resultado_total <- left_join(resultado_total, select(fantasy_discretizados,1,6,7), by = "Player", relationship = "many-to-many")
  resultado_total <- resultado_total %>% arrange(desc(pontos_media))
  resultado_total$pontos_media_discretizado <- lev
  #colunas_a_selecionar <- c(6,9, 11, 13, 15, 17, 19, 21, 23, 25, 27, 29, 31, 33, 35, 37, 39, 41, 43, 45, 47, 49)
  
  #fantasy_discretizados <- fantasy_discretizados %>%
    #select(colunas_a_selecionar)
  
  #total_colunas <- ncol(fantasy_discretizados)
  #novos_nomes <- paste("coluna",1:total_colunas, sep = "")
  #colnames(fantasy_discretizados) <- novos_nomes

  
   #Suponha que o dataframe seja fantasy_discretizados e você deseja selecionar colunas de 9 a 49 de 2 em 2
 
 # modelo_rf <- randomForest(coluna1 ~ ., data=fantasy_discretizados, importance=TRUE,
  #                       proximity=TRUE, ntree = 500)
  #print(modelo_rf)
  #importancia_atributos <- importance(modelo_rf)
  #varImpPlot(modelo_rf)
  
  #colunas_a_selecionar <- c(2,4,6,8, 10, 12, 14, 16, 18, 20, 22, 24, 26, 28, 30, 32, 34, 36, 38, 40, 42, 46)

  #resultado_total_discretizado <- resultado_total %>%
    #select(colunas_a_selecionar)
  
  # Exemplo: Convertendo coluna "discretizado_variavel" para fator
  # Transforme todas as colunas em fatores
  #resultado_total_discretizado <- as.data.frame(lapply(resultado_total_discretizado, as.factor))
  

  
  #transacoes <- as(select(resultado_total_discretizado,22,1), "transactions")
  
  # Execute o algoritmo Apriori
  #regras <- apriori(transacoes, 
                    #parameter = list(support = 0.03, confidence = 0.5))
  
  # Examine as regras
  fantasy_cor<- left_join(fantasy %>% filter(Pos == "C"),select(fantasy_estatisticas,1,14,15), by = "Player")
  fantasy_cor<- select(fantasy_cor, 33,34, everything())
  fantasy_c<- cor(select(fantasy_cor, -c(3,6,7,9)), method = "kendall")

  preseason_score <- NULL
  for(ano in 2023:2024){
  for (page in 1:6) {
    
    # Obtém a URL da página atual.
    url <- paste0("https://basketball.realgm.com/nba/preseason/team/NBA/0/stats/",ano,"/Totals/All/points/All/desc/",page,"/Preseason")
    # Lê o HTML da página atual.
    preseason_page <- read_html(url)
    
    # Seleciona a tabela de dados da página atual.
    preseason_table_page <- preseason_page %>% html_elements("table") %>% html_table()
    preseason_table_page <- preseason_table_page[[13]]
    preseason_table_page$ano <- ano
    preseason_score <- bind_rows(preseason_score, preseason_table_page)
  }
  }
  for(ano in 2023:2024){
  url <- "https://basketball.realgm.com/nba/preseason/team/NBA/0/stats/2024/Misc_Stats/All/dbl_dbl/All/desc/1/Preseason"
  double_double <- read_html(url)
  double_double <- double_double %>% html_elements("table") %>% html_table() 
  double_double <- double_double[[13]] %>% select(2,3,4,5)
  double_double$ano <- ano
  preseason_score<- left_join(preseason_score, double_double, by = join_by(Player == Player, ano == ano))  
  }
  preseason_score<- select(preseason_score, -c("Team.y", "Team"))
  preseason_score <- preseason_score %>%
  mutate(`Dbl Dbl.x` = ifelse(!is.na(`Dbl Dbl.y`), `Dbl Dbl.y`, `Dbl Dbl.x`),
         `Tpl Dbl.x` = ifelse(!is.na(`Tpl Dbl.y`), `Tpl Dbl.y`, `Tpl Dbl.x`)) %>% select(-c(27,28)) %>% rename(Team = "Team.x",
                                                                                                               DD = `Dbl Dbl.x`,
                                                                                                               TD = `Tpl Dbl.x`)
  pesos <- c(-0.5,-1,-0.5,.5,1.5,2.5,2.5,-1,8,15)
  
  preseason_fantasy<- reframe(preseason_score,ano, Player, Team, GP, MIN, FGM, 
          FGMI = (FGA - FGM) * pesos[1], 
          FTM, 
          FTMI = (FTA - FTM) * pesos[2],
          `3PM`,
          `3PMI` = (`3PA` - `3PM`) * pesos[3],
          REB,
          ORB = ORB * pesos[4],
          AST = AST * pesos[5],
          STL = STL * pesos[6],
          BLK = BLK * pesos[7],
          TOV = TOV * pesos[8],
          DD = DD * pesos[9],
          TD = TD * pesos[10],
          PTS
          )
  preseason_fantasy$fantasy_total<- rowSums(select(preseason_fantasy, -c(1:5)), na.rm = TRUE)
  preseason_fantasy$min_per_game <- preseason_fantasy$MIN/preseason_fantasy$GP
  preseason_fantasy<- filter(preseason_fantasy, min_per_game > 10)
  preseason_fantasy$pontos_por_minuto <- preseason_fantasy$fantasy_total/preseason_fantasy$MIN
  