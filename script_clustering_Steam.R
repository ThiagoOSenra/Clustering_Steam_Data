# Configuração e Pacotes
options(scipen = 999)
if(!require(pacman)) install.packages("pacman")

pacman::p_load(tidyverse, lubridate, scales, ggplot2, factoextra, cluster, fpc,
               NbClust, janitor, fastDummies, openxlsx, wordcloud, RColorBrewer,
               forcats, knitr, kableExtra, clustMixType, scales)

# Leitura dos dados e Tratamento
dados <- read_csv("dados/steam.csv")
dados <- clean_names(dados)


dados <- dados %>% 
  mutate(owners = recode(owners,
                         "0-20000" = "[10000-400000]",
                         "20000-50000" = "[10000-400000]",
                         "50000-100000" = "[10000-400000]",
                         "100000-200000" = "[10000-400000]",
                         "200000-500000" = "[10000-400000]",
                         "500000-1000000" = "(400000,8000000]",
                         "1000000-2000000" = "(400000,8000000]",
                         "2000000-5000000" = "(400000,8000000]",
                         "5000000-10000000" = "(400000,8000000]",
                         "10000000-20000000" = "(8000000,24000000]",
                         "20000000-50000000" = "(24000000,72000000]",
                         "50000000-100000000" = "(72000000,150000000]",
                         "100000000-200000000" = "(72000000,150000000]")) %>% 
  mutate(popularity = recode(owners,
                             "[10000-400000]" = "Very Low",
                             "(400000,8000000]" = "Low",
                             "(8000000,24000000]" = "Medium",
                             "(24000000,72000000]" = "High",
                             "(72000000,150000000]" = "Very High"))

dados$owners <- factor(dados$owners,
                           levels = c("[10000-400000]", "(400000,8000000]",
                                      "(8000000,24000000]", "(24000000,72000000]",
                                      "(72000000,150000000]"),
                           ordered = TRUE)

dados$popularity <- factor(dados$popularity,
       levels = c("Very Low", "Low", "Medium", "High", "Very High"),
       ordered = TRUE)



steam <- dummy_cols(dados,
                    select_columns = c("platforms", "genres"),
                    split = ";",
                    remove_first_dummy = FALSE,
                    remove_selected_columns = TRUE)

colSums(is.na(steam))

steam <- clean_names(steam)

colnames(steam) <- gsub("^platforms_", "", colnames(steam)) 
colnames(steam) <- gsub("^genres_", "", colnames(steam)) 

steam <- steam %>% 
  mutate(release_year = year(ymd(release_date))) %>%
  select(-all_of(c("appid","name", "required_age", "developer", "publisher",
                   "categories", "windows", "steamspy_tags", "owners", "release_date")))


################################################################################

# Análise Exploratória dos Dados

# Histograma de jogos por ano
ggplot(steam, aes(x = release_year)) +
  geom_histogram(binwidth = 1, fill = "#4C72B0", color = "white") +
  labs(title = "Distribuição de jogos por ano", x = "Ano", y = "Número de jogos") +
  theme_minimal()


# Preço dos jogos (boxplot)
ggplot(dados, aes(x = '', y = price)) +
  geom_boxplot(fill = "#55A868") +
  coord_cartesian(ylim = c(0, 100)) +
  labs(title = "Distribuição de preços dos jogos", y = "Preço (US$)", x = "") +
  theme_minimal()


# Avaliações positivas vs negativas (scatter plot log-log)
ggplot(dados, aes(x = positive_ratings, y = negative_ratings)) +
  geom_point(alpha = 0.4, color = "#C44E52") +
  scale_x_log10() + scale_y_log10() +
  labs(title = "Avaliações positivas vs negativas", x = "Avaliações positivas", y = "Avaliações negativas") +
  theme_minimal()


# Tabela resumo: tempo médio de jogo
dados %>%
  summarise(min_play = min(average_playtime),
            median_play = median(average_playtime),
            mean_play = mean(average_playtime),
            max_play = max(average_playtime)) %>%
  round(2)%>%
  kable("html", caption = "Tempo médio de jogo") %>%
  kable_styling(bootstrap_options = c("striped", "hover", "condensed", "responsive"), full_width = FALSE)

# Correlação entre tempo de jogo e avaliações positivas
ggplot(dados, aes(x = average_playtime, y = positive_ratings)) +
  geom_point(alpha = 0.4, color = "#64B5CD") +
  scale_x_log10() + scale_y_log10() +
  labs(title = "Tempo médio vs Avaliações positivas", x = "Tempo médio de jogo (min)", y = "Avaliações positivas") +
  theme_minimal()


# Proporção de jogos por plataforma
dados %>%
  count(platforms) %>%
  mutate(platforms = fct_reorder(platforms, n)) %>%
  ggplot(aes(x = platforms, y = n)) +
  geom_col(fill = "#8172B3") +
  coord_flip() +
  labs(title = "Distribuição por Suporte", x = "Suporte", y = "Número de jogos") +
  theme_minimal()

dados %>%
  separate_rows(platforms, sep = ";") %>%
  count(platforms, sort = TRUE) %>%
  ggplot(aes(x = fct_reorder(platforms, n), y = n)) +
  geom_col(fill = "#C44E52") +
  coord_flip() +
  labs(title = "Quantidade de jogos por Plataforma", x = "Plataforma", y = "Quantidade") +
  theme_minimal()




# Proporção de jogos por generos
dados %>%
  count(genres) %>%
  mutate(genres = fct_reorder(genres, n)) %>%
  filter(n > 200) %>% 
  ggplot(aes(x = genres, y = n)) +
  geom_col(fill = "#8172B3") +
  coord_flip() +
  labs(title = "Distribuição por Tags", x = "Tags", y = "Número de jogos") +
  theme_minimal()

dados %>%
  separate_rows(genres, sep = ";") %>%
  count(genres, sort = TRUE) %>%
  ggplot(aes(x = fct_reorder(genres, n), y = n)) +
  geom_col(fill = "#C44E52") +
  coord_flip() +
  labs(title = "Quantidade de jogos por gênero", x = "Gênero", y = "Quantidade") +
  theme_minimal()

## Nuvem de palavras: gêneros
genres <- dados$genres %>% 
  str_split(";") %>%
  unlist() %>%
  table() %>%
  sort(decreasing = TRUE)

wordcloud(names(genres), freq = genres, min.freq = 50,
          colors = brewer.pal(8, "Dark2"), random.order = FALSE)

## Tabela de Gêneros
dados |>
  separate_rows(genres, sep = ";") |>
  count(genres, sort = TRUE) |>
  mutate(row = row_number()) |>
  mutate(metade = ceiling(n() / 2)) |>
  mutate(
    coluna = if_else(row <= metade, "esq", "dir"),
    pos = if_else(coluna == "esq", row, row - metade)
  ) |>
  select(-row, -metade) |>
  pivot_wider(names_from = coluna, values_from = c(genres, n), names_glue = "{.value}_{coluna}") |>
  arrange(pos) |>
  select(genres_esq, n_esq, genres_dir, n_dir) |>  # << Aqui está o ajuste
  rename(
    "Gênero" = genres_esq,
    "Total" = n_esq,
    "Gênero " = genres_dir,  # espaço evita duplicata de nome
    "Total " = n_dir
  ) |>
  kable("html", align = "lrrl") |>
  kable_styling(full_width = FALSE, bootstrap_options = c("striped", "hover", "condensed"))

################################################################################

# Selecionando os Gêneros Mais Frequentes
colunas_geners <- steam %>%
  select(11:39) %>% 
  colnames()


steam <- steam %>% 
  mutate(across(where(is.factor), ~ suppressWarnings(as.numeric(.))))

frequencia_geners <- steam %>% 
  select(all_of(colunas_geners)) %>% 
  summarise(across(everything(), sum)) %>% 
  pivot_longer(everything(), names_to = "genero", values_to = "frequencia")


generos <- frequencia_geners %>% 
  filter(frequencia > 150) %>% 
  pull(genero)

steam <- steam %>% 
  select(-all_of(colunas_geners)) %>% 
  bind_cols(steam %>% 
              select(all_of(generos)))


################################################################################

# Análise de Agrupamentos

## Escalonamento
vars_conti <- steam %>% 
  select(where(is.numeric)) %>%
  select(where(~ !all(.x %in% c(0,1)))) %>% 
  select(where(~ !all(.x %in% c(1,2,3,4,5)))) %>%
  colnames()

steam_scaled <- steam
steam_scaled[vars_conti] <- scale(steam_scaled[vars_conti])



## Método Hierárquico
# dist_matrix <- daisy(steam_scaled, metric = "gower")

# hc <- hclust(dist_matrix, method = "ward.D2")

## Dendograma
plot(hc, labels = F, hang = -1, main = "Dendograma")
rect.hclust(hc, k = 3, border = "red")
plot(hc, labels = F, hang = -1, main = "Dendograma")
rect.hclust(hc, k = 4, border = "red")
plot(hc, labels = F, hang = -1, main = "Dendograma")
rect.hclust(hc, k = 5, border = "red")



## Silhueta
freq <- steam_scaled %>% 
  count(popularity, name = "n") %>% 
  mutate(
    n_amostra = pmax(round(0.2 * n), 3)
    )


set.seed(123)
steam_amostra <- steam_scaled %>% 
  inner_join(freq, by = "popularity") %>% 
  group_by(popularity) %>%
  group_modify(~ slice_sample(.x, n = .x$n_amostra[1])) %>% 
  ungroup() %>% 
  select(-all_of(c("n", "n_amostra")))

dist_matrix_amostra <- daisy(steam_amostra, metric = "gower")
hc_amostra <- hclust(dist_matrix_amostra, method = "ward.D2")

# Silhueta
num_grupos <- 2:10
si <- numeric(length(num_grupos))

for (i in seq_along(num_grupos)) {
  k <- num_grupos[i]
  cluster_h_k <- cutree(hc_amostra, k)
  si_k <- silhouette(cluster_h_k, dist_matrix_amostra)
  si[i] <- mean(si_k[, 3])
}


df_silhouette <- data.frame(
  k = num_grupos,
  silhueta_media = si
)


ggplot(df_silhouette, aes(x = k, y = silhueta_media)) +
  geom_line(color = "#0072B2", size = 1) +
  geom_point(color = "#D55E00", size = 3) +
  labs(
    title = "Média do Coeficiente de Silhueta por Número de Grupos",
    x = "Número de Clusters (k)",
    y = "Silhueta Média"
  ) +
  theme_minimal(base_size = 14) +
  scale_x_continuous(breaks = num_grupos) +
  geom_vline(xintercept = num_grupos[which.max(si)], linetype = "dashed", color = "gray50") +
  annotate("text", x = num_grupos[which.max(si)], y = max(si), 
           label = "Melhor k", vjust = -1, color = "gray30")



fviz_silhouette(pam(dist_matrix_amostra, k = 2, diss = TRUE))
fviz_silhouette(pam(dist_matrix_amostra, k = 3, diss = TRUE))
fviz_silhouette(pam(dist_matrix_amostra, k = 4, diss = TRUE))
fviz_silhouette(pam(dist_matrix_amostra, k = 5, diss = TRUE))

steam$cluster <- cutree(hc, k = 4)
dados$cluster <- cutree(hc, k = 4)


################################################################################

# Análise Descritiva dos Clusters

## Tamanho dos Clusters
table(steam$cluster) %>%
  t() %>% 
  kable(format = "html", align = "lcccclcccclcccc") %>%
  kable_styling(full_width = FALSE, bootstrap_options = c("striped", "hover"))



# 1. Jogos de cada Grupo
dados %>%
  filter(cluster == 1) %>%
  mutate(total_avaliacoes = positive_ratings + negative_ratings) %>%
  arrange(desc(total_avaliacoes)) %>%
  select(name, total_avaliacoes) %>%
  head(5) %>%
  kable(format = "html", align = "lc", col.names = c("Nome do Jogo", "Total de Avaliações")) %>%
  kable_styling(full_width = FALSE, bootstrap_options = c("striped", "hover"))

dados %>%
  filter(cluster == 2) %>%
  mutate(total_avaliacoes = positive_ratings + negative_ratings) %>%
  arrange(desc(total_avaliacoes)) %>%
  select(name, total_avaliacoes) %>%
  head(5) %>%
  kable(format = "html", align = "lc", col.names = c("Nome do Jogo", "Total de Avaliações")) %>%
  kable_styling(full_width = FALSE, bootstrap_options = c("striped", "hover"))

dados %>%
  filter(cluster == 3) %>%
  mutate(total_avaliacoes = positive_ratings + negative_ratings) %>%
  arrange(desc(total_avaliacoes)) %>%
  select(name, total_avaliacoes) %>%
  head(5) %>%
  kable(format = "html", align = "lc", col.names = c("Nome do Jogo", "Total de Avaliações")) %>%
  kable_styling(full_width = FALSE, bootstrap_options = c("striped", "hover"))

dados %>%
  filter(cluster == 4) %>%
  mutate(total_avaliacoes = positive_ratings + negative_ratings) %>%
  arrange(desc(total_avaliacoes)) %>%
  select(name, total_avaliacoes) %>%
  head(5) %>%
  kable(format = "html", align = "lc", col.names = c("Nome do Jogo", "Total de Avaliações")) %>%
  kable_styling(full_width = FALSE, bootstrap_options = c("striped", "hover"))


# 2. Estatísticas descritivas por cluster
medias <- steam %>%
  group_by(cluster) %>%
  select(-popularity) %>% 
  summarise(across(where(is.numeric), list(mean = mean), na.rm = TRUE))

preencher_com_valor <- function(tbl, n_linhas, valor = "—") {
  linhas_faltando <- n_linhas - nrow(tbl)
  if (linhas_faltando > 0) {
    # Cria um data.frame com o valor repetido
    preenchido <- as.data.frame(matrix(valor, nrow = linhas_faltando, ncol = ncol(tbl)))
    colnames(preenchido) <- colnames(tbl)
    
    # Garante que tudo seja character para evitar erros na união
    tbl <- mutate(across(everything(), as.character), .data = tbl)
    preenchido <- mutate(across(everything(), as.character), .data = preenchido)
    
    tbl <- bind_rows(tbl, preenchido)
  }
  return(tbl)
}

medias %>%
  pivot_longer(-cluster, names_to = "Variável", values_to = "Média") %>%
  pivot_wider(names_from = cluster, values_from = Média, names_prefix = "Cluster ") %>%
  mutate(across(starts_with("Cluster "), ~ round(.x, digits = 2))) %>%
  arrange(Variável) %>%
  {
    n <- nrow(.)
    por_bloco <- ceiling(n / 2)
    split(., rep(1:2, each = por_bloco, length.out = n))
  } %>%
  map(~ {
    colnames(.x) <- c("Variável", paste0("Cluster ", 1:(ncol(.x) - 1)))
    .x
  }) %>%
  {
    max_linhas <- max(sapply(., nrow))
    map(., preencher_com_valor, n_linhas = max_linhas)
  } %>%
  reduce(cbind) %>%
  kable(format = "html", align = "lcccclcccclcccc") %>%
  kable_styling(full_width = FALSE, bootstrap_options = c("striped", "hover"))

medianas <- steam %>%
  group_by(cluster) %>%
  summarise(across(where(is.numeric), list(median = median), na.rm = TRUE))


medianas %>%
  pivot_longer(-cluster, names_to = "Variável", values_to = "Média") %>%
  pivot_wider(names_from = cluster, values_from = Média, names_prefix = "Cluster ") %>%
  mutate(across(starts_with("Cluster "), ~ round(.x, digits = 2))) %>%
  arrange(Variável) %>%
  {
    n <- nrow(.)
    por_bloco <- ceiling(n / 2)
    split(., rep(1:2, each = por_bloco, length.out = n))
  } %>%
  map(~ {
    colnames(.x) <- c("Variável", paste0("Cluster ", 1:(ncol(.x) - 1)))
    .x
  })  %>%
  {
    max_linhas <- max(sapply(., nrow))
    map(., preencher_com_valor, n_linhas = max_linhas)
  } %>%
  reduce(cbind) %>%
  kable(format = "html", align = "lcccclcccclcccc") %>%
  kable_styling(full_width = FALSE, bootstrap_options = c("striped", "hover"))



# 3. Boxplots das variáveis numéricas por cluster
steam$cluster <- as.factor(steam$cluster)
for (var in vars_conti) {
  p <- ggplot(steam, aes(x = cluster, y = .data[[var]], fill = cluster)) +
    geom_boxplot() +
    labs(title = paste("Boxplot de", var, "por Cluster"), y = var) +
    theme_minimal()
  print(p)
}


# 4. Gráficos de barras para variáveis categóricas
vars_categ <- steam %>% 
  select(-c(vars_conti, cluster)) %>%
  colnames()

for (var in vars_categ) {
  p <- steam %>%
    select(-popularity)
    filter(.data[[var]] != 0) %>% # Filtra apenas valores iguais a 1
    count(cluster) %>%
    group_by(cluster) %>%
    mutate(prop = n / sum(n)) %>%  # essa proporção será 100% agora, pois só há 1s. Ajuste abaixo!
    ungroup() %>%
    # Agora precisamos da base total por cluster (0s + 1s) para calcular a proporção real dos 1s:
    right_join(
      steam %>% count(cluster, name = "total"),
      by = "cluster"
    ) %>%
    mutate(prop = n / total)
  
  p_plot <- ggplot(p, aes(x = as.factor(cluster), y = prop, fill = as.factor(cluster))) +
    geom_col() +
    labs(
      title = paste("Proporção de", var, "por Cluster"),
      x = "Cluster",
      y = "Proporção"
    ) +
    scale_y_continuous(labels = percent_format()) +
    theme_minimal() +
    theme(legend.position = "none")
  
  print(p_plot)
}



df_popularidade <- dados %>%
  mutate(popularidade = positive_ratings + negative_ratings) %>%
  group_by(cluster) %>%
  summarise(popularidade_media = mean(popularidade, na.rm = TRUE))

# Plot com ggplot2
ggplot(df_popularidade, aes(x = factor(cluster), y = popularidade_media, fill = factor(cluster))) +
  geom_col() +
  labs(
    title = "Popularidade Média por Cluster",
    x = "Cluster",
    y = "Popularidade Média (Total de Avaliações)"
  ) +
  theme_minimal(base_size = 14) +
  scale_fill_brewer(palette = "Set2", name = "Cluster") +
  geom_text(aes(label = round(popularidade_media, 1)), vjust = -0.5) +
  theme(legend.position = "none")


# 5. Testes estatísticos para verificar diferenças entre clusters

## ANOVA para variáveis numéricas
aov_results <- list()

for (var in vars_conti) {
  formula <- as.formula(paste(var, "~ cluster"))
  aov_result <- aov(formula, data = steam)
  summary_res <- summary(aov_result)[[1]]
  p_value <- summary_res["Pr(>F)"][1, 1]
  f_value <- summary_res["F value"][1, 1]
  aov_results[[var]] <- data.frame(Variável = var,
                                   `F` = round(f_value, 3),
                                   `p-valor` = format.pval(p_value, digits = 3, eps = 0.001))
}

aov_df <- bind_rows(aov_results)

aov_df %>%
  kable("html", caption = "Resultados dos Testes ANOVA por Cluster") %>%
  kable_styling(bootstrap_options = c("striped", "hover", "condensed", "responsive"), full_width = FALSE)


# Qui-quadrado para variáveis categóricas
chisq_results <- list()
for (var in vars_categ) {
  tbl <- table(steam[[var]], steam$cluster)
  test <- chisq.test(tbl)
  chisq_results[[var]] <- data.frame(Variável = var,
                                     `X²` = round(test$statistic, 3),
                                     `p-valor` = format.pval(test$p.value, digits = 3, eps = 0.001))
}


chisq_df <- bind_rows(chisq_results)

chisq_df %>%
  { # Começa um bloco para criar df1 e df2 e juntar lado a lado
    n <- nrow(.)
    half <- ceiling(n / 2)
    
    df1 <- slice(., 1:half)
    df2 <- slice(., (half + 1):n)
    
    # Completa df2 com NAs se necessário
    if (nrow(df2) < nrow(df1)) {
      n_diff <- nrow(df1) - nrow(df2)
      df2 <- bind_rows(df2, tibble(
        Variável = rep(NA_character_, n_diff),
        X. = rep(NA_real_, n_diff),
        p.valor = rep(NA_character_, n_diff)
      ))
    }
    
    # Renomeia colunas do df2
    colnames(df2) <- paste0(colnames(df2), "_2")
    
    # Junta lado a lado
    bind_cols(df1, df2)
  } %>%
  # Renomeia as colunas finais pra ficar igual você pediu
  setNames(c("Variável", "X²", "p-valor", "Variável", "X²", "p-valor")) %>%
  
  # Gera a tabela com kable
  kable("html",
        caption = "Resultados dos Testes Qui-Quadrado por Cluster",
        row.names = FALSE) %>%
  kable_styling(bootstrap_options = c("striped", "hover", "condensed", "responsive"),
                full_width = FALSE)

################################################################################


write.xlsx(dados, "dados.xlsx")
