# Análise de Agrupamento de Jogos da Steam

[![Status](https://img.shields.io/badge/status-concluído-brightgreen.svg)]() [![License](https://img.shields.io/badge/license-MIT-green.svg)]() [![R](https://img.shields.io/badge/R-%3E=4.2.0-276DC3.svg)]() [![Made with love](https://img.shields.io/badge/made%20with-%E2%9D%A4-red.svg)]()

Repositório do projeto **GET00126 – Análise Multivariada I (UFF, 2025/1)**.\
Foi aplicado **clusterização hierárquica** com **distância de Gower** (dados mistos) para segmentar **27.075 jogos** da Steam (até 2019) por **gênero, avaliações, tempos de jogo, preço, popularidade e compatibilidade**.

## 🧭 Visão Geral

-   **Objetivo:** Identificar **perfis de jogos** na Steam e discutir aplicações em **marketing**, **recomendação** e **estudos de mercado**.\
-   **Abordagem:** Pré-processamento → Métrica **Gower** → **HC aglomerativo (Ward.D2)** → Escolha de **k=4** via dendrograma + silhueta → **Caracterização** dos grupos.

------------------------------------------------------------------------

## 📦 Dados

-   **Fonte:** Kaggle — *Steam Store Games* (Nik Davis).\
-   **Link do dataset:** <https://www.kaggle.com/datasets/nikdavis/steam-store-games>\
-   **Período:** Jogos lançados **até 2019**.\
-   **Variáveis-chave:**
    -   **Numéricas:** `achievements`, `positive_ratings`, `negative_ratings`, `average_playtime`, `median_playtime`, `price`, `release_year`.
    -   **Categóricas:** `geners`(≥ 150 ocorrências), `platforms` (Windows/Mac/Linux), `english` (indicador).
    -   **Derivadas:** `popularity` (a partir de `owners`: Muito Baixa, Baixa, Média, Alta, Muito Alta).

------------------------------------------------------------------------

## 🧪 Metodologia

1.  **Pré-processamento**
    -   `release_date → release_year`
    -   `popularity` a partir de `owners`
    -   Dummies para **gêneros** (frequentes) e **plataformas** (Mac/Linux)
    -   **Padronização** de variáveis numéricas
2.  **Clusterização**
    -   **Dissimilaridade:** `cluster::daisy(..., metric = "gower")`
    -   **Aglomeração:** `hclust(..., method = "ward.D2")`
    -   **k ótimo:** inspeção de **dendrograma** + **média da silhueta** (trade-off entre separação e representatividade)
3.  **Avaliação e Interpretação**
    -   Boxplots e proporções por cluster para variáveis **numéricas** e **categóricas**
    -   Síntese de **perfis** (tabela de caracterização)

------------------------------------------------------------------------

## 📊 Resultados (Resumo)

**k = 4 clusters**:

-   **Cluster 1 — Populares & Multiplataforma**
    -   Maior **popularidade**, **mais avaliações positivas**, **maior preço médio**
    -   **Suporte** a **Mac** e **Linux** mais frequente
    -   Gêneros equilibrados: *indie/action/adventure/casual*
-   **Cluster 2 — Ação & Conteúdo Intenso**
    -   Concentração de **action**, **gore** e **violence**
    -   **Popularidade intermediária**
    -   **Maior mediana de avaliações negativas**
    -   Baixa compatibilidade Mac/Linux
-   **Cluster 3 — Estratégia & Aventura (Nicho Engajado)**
    -   **Tempos de jogo** (médio e mediano) **mais altos**
    -   Boas médias de **conquistas**
    -   Público mais **nichado**
-   **Cluster 4 — Indie/Casual de Baixa Visibilidade**
    -   **Preço baixo/grátis**, **baixa compatibilidade**
    -   **Maior nº de conquistas por jogo** (design orientado a metas)
    -   **Popularidade muito baixa**

> Detalhes completos, gráficos e tabela-resumo: veja o PDF.

------------------------------------------------------------------------

## 🗂 Estrutura do Repositório

📦 movielens-analysis

. ┣ 📂 data

. ┃ ┗ 📄 steam.csv, steam_description_data.csv, steam_media_data.csv...

. ┣ 📂 plots

. ┃ ┗ 📊 Gráficos gerados pela análise

. ┣ 📂 tables

. ┃ ┗ 📊 tabelas geradas pela análise

. ┣ 📜 README.md

. ┣ 📜 script_clustering_Steam.R

## 🛠️ Tecnologias

-   Linguagem: **R**
-   Principais pacotes: `cluster`, `factoextra`, `dplyr`, `ggplot2`.
