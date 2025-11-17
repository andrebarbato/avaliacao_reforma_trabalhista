pacotes <- c("plotly", #plataforma gráfica
             "tidyverse", #carregar outros pacotes do R
             "ggrepel", #geoms de texto e rótulo para 'ggplot2' que ajudam a
             #evitar sobreposição de textos
             "knitr", "kableExtra", #formatação de tabelas
             "reshape2", #função 'melt'
             "misc3d", #gráficos 3D
             "plot3D", #gráficos 3D
             "cluster", #função 'agnes' para elaboração de clusters hierárquicos
             "factoextra", #função 'fviz_dend' para construção de dendrogramas
             "ade4") #função 'ade4' para matriz de distâncias em var. binárias

if(sum(as.numeric(!pacotes %in% installed.packages())) != 0){
  instalador <- pacotes[!pacotes %in% installed.packages()]
  for(i in 1:length(instalador)) {
    install.packages(instalador, dependencies = T)
    break()}
  sapply(pacotes, require, character = T) 
} else {
  sapply(pacotes, require, character = T) 
}

# vetor de indicadores: PIB, Inflação, Tx. Câmbio, Tx. Juros Real,
# Tx. Desemprego, indicadores de governança de corrupção e estabilidade Política e 
# violência e terrorismo
indicators <- c("NY.ADJ.NNTY.PC.KD", "SP.DYN.IMRT.IN", "NE.EXP.GNFS.ZS",
                "NE.IMP.GNFS.ZS", "SH.XPD.CHEX.GD.ZS", "FR.INR.RINR",
                "SP.DYN.LE00.IN", "NY.GDP.PCAP.KD")

raw_indicators <- WDI::WDI(country = "all", 
                           indicator = indicators) |> 
  tidyr::as_tibble()

# Salvando os dados brutos
readr::write_csv(x = raw_indicators,
                 file = "data/raw_indicators_cluster.csv")

# Lendo o arquivo de dados brutos
raw_indicators <- readr::read_csv(file = "data/raw_indicators_cluster.csv", 
                                  col_names = TRUE)

countries <- raw_indicators |> 
  dplyr::rename(income = "NY.ADJ.NNTY.PC.KD",
                child_mort = "SP.DYN.IMRT.IN",
                exports = "NE.EXP.GNFS.ZS",
                imports = "NE.IMP.GNFS.ZS",
                health = "SH.XPD.CHEX.GD.ZS",
                inflation = "FR.INR.RINR",
                life_expec = "SP.DYN.LE00.IN",
                gdp = "NY.GDP.PCAP.KD") |> 
  dplyr:: filter(year %in% c(2014:2024)) |> 
  dplyr::group_by(country) |> 
  dplyr::summarise(
    income = mean(income, na.rm = TRUE),
    child_mort = mean(child_mort, na.rm = TRUE),
    exports = mean(exports, na.rm = TRUE),
    imports = mean(imports, na.rm = TRUE),
    health = mean(health, na.rm = TRUE),
    inflation = mean(inflation, na.rm = TRUE),
    life_expec = mean(life_expec, na.rm = TRUE),
    gdp = mean(gdp, na.rm = TRUE)
  ) |> 
  tidyr::drop_na()



# Estatísticas descritivas

summary(countries)

## As variáveis apresentam unidades de medida e amplitudes muito distintas

# Padronizando as variáveis

pais_padronizado <- as.data.frame(scale(countries[,2:9]))
rownames(pais_padronizado) <- countries$country

## Todas as variáveis passam a ter média = 0 e desvio padrão = 1. Por exemplo:

round(mean(pais_padronizado$exports),3)
round(sd(pais_padronizado$exports))

round(mean(pais_padronizado$gdp),3)
round(sd(pais_padronizado$gdp))

# Vamos iniciar com o esquema de aglomeração hierárquico

# Matriz de dissimilaridades
matriz_D <- pais_padronizado %>% 
  dist(method = "euclidean")

# 1º Teste: Elaboração da clusterização hierárquica como "single linkage"
cluster_hier_single <- agnes(x = matriz_D, method = "single")

# Construção do dendrograma "single linkage"
dev.off()
fviz_dend(x = cluster_hier_single, show_labels = F)

## O método de encadeamento single linkage não permite uma clusterização útil
## Pode-se interpretar que as observações estão muito próximas umas das outras

# 2º Teste: Elaboração da clusterização hierárquica como "complete linkage"
cluster_hier_complete <- agnes(x = matriz_D, method = "complete")

# Construção do dendrograma "complete linkage"
fviz_dend(x = cluster_hier_complete, show_labels = F)

## O método de encadeamento complete linkage melhora significativamente

# 3º Teste: Elaboração da clusterização hierárquica como "average linkage"
cluster_hier_average <- agnes(x = matriz_D, method = "average")

# Construção do dendrograma "average linkage"
fviz_dend(x = cluster_hier_average, show_labels = F)

## Vamos optar pelo complete linkage (average cria clusters com menos observações)

# Dendrograma com visualização dos clusters (selecionando por "altura")
fviz_dend(x = cluster_hier_complete,
          h = 6.5,
          color_labels_by_k = F,
          rect = T,
          rect_fill = T,
          rect_border = "black",
          lwd = 1,
          show_labels = F,
          ggtheme = theme_bw())

## Formam 12 clusters cortando o dendrograma em 5.50

# Vamos detalhar esse esquema hierárquico

coeficientes <- sort(cluster_hier_complete$height, decreasing = FALSE) 
esquema <- as.data.frame(cbind(cluster_hier_complete$merge, coeficientes))
names(esquema) <- c("Cluster1", "Cluster2", "Coeficientes")
esquema

## Portanto, vamos gerar uma variável indicando 12 clusters

countries$cluster_H <- factor(cutree(tree = cluster_hier_complete, k = 8))
pais_padronizado$cluster_H <- factor(cutree(tree = cluster_hier_complete, k = 8))

# A seguir, vamos verificar se todas as variáveis ajudam na formação dos grupos

summary(anova_child_mort <- aov(formula = child_mort ~ cluster_H,
                                data = pais_padronizado))

summary(anova_exports <- aov(formula = exports ~ cluster_H,
                             data = pais_padronizado))

summary(anova_health <- aov(formula = health ~ cluster_H,
                            data = pais_padronizado))

summary(anova_imports <- aov(formula = imports ~ cluster_H,
                             data = pais_padronizado))

summary(anova_income <- aov(formula = income ~ cluster_H,
                            data = pais_padronizado))

summary(anova_inflation <- aov(formula = inflation ~ cluster_H,
                               data = pais_padronizado))

summary(anova_lifeexpec <- aov(formula = life_expec ~ cluster_H,
                               data = pais_padronizado))

summary(anova_gdpp <- aov(formula = gdp ~ cluster_H,
                          data = pais_padronizado))

## Todas auxiliam na formação de pelo menos um cluster

# O que os cluster indicam? Vamos interpretar algumas variáveis médias:

análise <- group_by(countries, cluster_H) %>%
  summarise(income = mean(income, na.rm = TRUE),
            gdp = mean(gdp, na.rm = TRUE),
            mort = mean(child_mort, na.rm = TRUE),
            health = mean(health, na.rm = TRUE),
            expec = mean(life_expec, na.rm = TRUE))

## Por exemplo: os países do cluster 1 e 4 apresentam: 
## Baixa renda média, baixo PIB per capita, 
## Elevada mortalidade infantil, baixa expectativa de vida
## Portanto, são os países em que deve haver ajuda para melhoria das condições

