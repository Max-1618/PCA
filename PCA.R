# PCA - Principal Component Analysis

library(FactoMineR)
library(factoextra)
library(corrplot)

data("decathlon2")
head(decathlon2)

# Terminologia do PCA
# Indivíduos Ativos: usados durante o processo de PCA (linhas 1:23)
# Indivíduos suplementares: as coordenadas desses indivíduos serão estimadas
# usando as informações do PCA e dos parâmetros obtidos dos indivíduos e variáveis ativas
# Variáveis ativas: variáveis que são usadas no processo de PCA (colunas 1:10)
# Variáveis suplementares: as coordenadas dessas variáveis serão estimadas
decathlon2.ativo <- decathlon2[1:23, 1:10]
head(decathlon2.ativo[,1:6],4)

# Padronização dos dados
# O objetivo é colocar todas as variáveis em uma mesma escala
# Obter desvio padrão igual a 1 e média igual a zero.
# Os dados podem ser transformados da seguinte forma: (xi - media(x))/sd(x)
# A função scale() pode ser usada para essa normalização.
# A função PCA() [FactoMineR] normaliza os dados automaticamente durante o PCA.
res.PCA <- PCA(decathlon2.ativo, graph=FALSE)
print(res.PCA)

# Eigenvalues / Variâncias
# Os valores (eigenvalues) medem o total de variação em cada componente principal.
# Os valores são maiores para os primeiros PCs e menores para os subsequentes.
# Ou seja, os primeiros PCs correspondem as direções com o total máximo de variação no 
# conjunto de dados.
# A análise dos valores (eigenvalues) é feita para determinar o número dos componentes
# principais que serão considerados
# Um eigenvalue > 1 indica uma variância maior (apenas para dados normalizados)
# Também é possível limitar o número de PCs por uma fração da variância total, 
# ou seja, se 70% é satisfatório use o número de componentes que atingem esse valor.
eig_val <- get_eigenvalue(res.PCA)
eig_val

# Scree Plot
fviz_eig(res.PCA, addlabels = TRUE, ylim=c(0,50))

# Resultados
var <- get_pca_var(res.PCA)
var

head(var$coord)
head(var$cos2)
head(var$contrib)

# Círculo de correlação
# Variáveis correlacionadas positivamente são agrupadas juntas
# Variáveis correlacionadas negativamente são posicionadas nos lados opostos (quadrantes opostos)
# A distância entre as variáveis e a origem mede a qualidade das variáveis no mapa de fator.
# As variáveis que estão longe da origem são melhores representadas no mapa de fator.
head(var$cor)
fviz_pca_var(res.PCA, col.var = "black")

# Qualidade da representação
# A qualidade da representação das variáveis no mapa de fator é chamado de cos2 (cosseno quadrático)
head(var$cos2)
corrplot(var$cos2, is.corr = FALSE)

# Total do cos2 nas dimensões Dim.1 e Dim.2
# Um valor alto para o cos2 indica uma boa representação da variável no componente principal
# Nesse caso, a variável é posicionada perto da circunferência do círculo de correlação
# Um valor baixo para o cos2 indica que a variável não é perfeitamente representada pelos PCs.
# Nesse caso, a variável está perto do centro do círculo.
# Para uma determinada variável, a soma dos cos2 para todos os PCs é igual a um.
# Se a variável é perfeitamente representada por somente dois PCs (Dim.1 e Dim.2), a soma
# do cos2, nesses dois PCs, é igual a um. Nesse caso, as variáveis serão posicionadas
# no Cículo de correlações.
# Para algumas variáveis, mais do que 2 componentes podem ser necessários para representar
# perfeitamente os dados. Nesse caso, as variáveis são posicionadas dentro do círculo
# de correlações.
fviz_cos2(res.PCA, choice = "var", axes = 1:2)

fviz_pca_var(res.PCA, col.var = "cos2",
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel = TRUE # Evita sobreposição dos textos
             )
fviz_pca_var(res.PCA, alpha.var = "cos2")

# Contribuições das variáveis para os PCs (expressas em percentual)
# As variáveis que são correlacionadas com o PC1 (Dim.1) e PC2 (Dim.2) são as mais
# importantes para explicar a variabilidade no conjunto de dados.
# As variáveis que não estão correlaciondas com qualquer PC ou correlaciondas com as 
# últimas dimensões são variáveis com baixa contribuição e podem ser removidas para 
# simplificar a análise.
head(var$contrib, 4)
corrplot(var$contrib, is.corr = FALSE)

# Contribuições das variáveis para o PC1
fviz_contrib(res.PCA, choice = "var", axes = 1, top = 10)

# Contribuições das variáveis para o PC2
fviz_contrib(res.PCA, choice = "var", axes = 2, top = 10)

# A contribuição total para o PC1 e PC2 
fviz_contrib(res.PCA, choice = "var", axes=1:2, top = 10)

# As variáveis mais importantes podem ser destacadas no gráfico de correlações  
fviz_pca_var(res.PCA, col.var = "contrib", 
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"))
fviz_pca_var(res.PCA, alpha.var = "contrib")

# Descrição da dimensão
# Pode ser usado para identificar as variáveis mais significantes associadas a um 
# dado componente principal.
res.desc <- dimdesc(res.PCA, axes=c(1,2), proba=0.05)
res.desc$Dim.1
res.desc$Dim.2

# Gráfico dos indivíduos
# Resultados
ind <- get_pca_ind(res.PCA)
ind

head(ind$coord)
head(ind$cos2)
head(ind$contrib)

# Qualidade e contribuição (plot)
fviz_pca_ind(res.PCA)

fviz_pca_ind(res.PCA, col.ind = "cos2",
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel = TRUE)
# Pontos
fviz_pca_ind(res.PCA, pointsize = "cos2",
             pointshape = 21, fill="#E7B800",
             repel = TRUE)

fviz_pca_ind(res.PCA, col.ind = "cos2", pointsize = "cos2",
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel = TRUE)

fviz_cos2(res.PCA, choice = "ind")
fviz_contrib(res.PCA, choice = "ind", axes = 1:2)

###------------Usando do dataset Iris
# Colorindo os indivíduos por grupo.
# A coluna "Species" será usada para fazer o agrupamento.
# Executando o PCA
# A variável Species (index = 5) é removida antes de executar a análise PCA
iris.pca <- PCA(iris[,-5], graph=FALSE)

# O parâmetro habillage ou col.ind pode ser usado para especificar a variável fator
# usada para colorir os indivíduos por grupos.
# Para adicionar elipses ao redor de cada grupo, especificar o parâmetro 
# addEllipses = TRUE.
fviz_pca_ind(iris.pca,
             geom.ind = "point",
             col.ind = iris$Species,
             palette = c("#00AFBB", "#E7B800", "#FC4E07"),
             addEllipses = TRUE,
             legend.title = "Grupos")

fviz_pca_ind(iris.pca, geom.ind = "point", col.ind = iris$Species,
             palette = c("#00AFBB", "#E7B800", "#FC4E07"),
             addEllipses = TRUE, ellipse.type = "confidence",
             legend.title = "Grupos")

# Os valores permitidos para o parâmetro palette são:
# "grey" para paleta de cores cinzas;
# paletas de cores "brewer": "RdBu", "Blues".
# Para ver todas: RColorBrewer::display.brewer.all()
# paletas customizadas: c("blue", "red")
# paletas de jornais científicos do pacote "ggsci": “npg”, “aaas”, “lancet”, “jco”, 
# “ucscgb”, “uchicago”, “simpsons” and “rickandmorty”.

# Por exemplo, para usar o jco (journal of clinical oncology)
library(ggsci)
fviz_pca_ind(iris.pca,
             label = "none",
             habillage = iris$Species,
             addEllipses = TRUE,
             palette = "jco")

# Dimensões
# Por default, as variáveis/indivíduos são representados nas dimensões 1 e 2.
# Para visualizar as dimensões 2 e 3, por exemplo, deve ser especificado o 
# parâmetro axes = c(2,3)
fviz_pca_var(res.PCA, axes = c(2,3))
fviz_pca_ind(res.PCA, axes = c(2,3))

# Elementos gráficos: point, text, arrow
# O parâmetro "geom" é usado para especificar os elementos geométricos ou elementos
# gráficos.
# geom.var: um texto para especificar a geometria a ser usada nas variáveis.
# geom.var = "point" : para mostrar apenas pontos
# geom.var = "text" : para mostrar apenas os textos das etiquetas (labels)
# geom.var = c("point", "text") : para mostrar pontos e textos dos labels
# geom.var = c("arrow", "text") : para mostrar setas e textos (default)
fviz_pca_var(res.PCA, geom.var = c("point", "text"))

# geom.ind : um texto para especificar a geometria a ser usada nos individuos.
# geom.ind = "point" : para mostrar apenas pontos
# geom.ind = "text" : para mostrar apenas os textos das etiquetas (labels)
# geom.ind = c("point", "text") : para mostrar pontos e textos dos labels
fviz_pca_ind(res.PCA, geom.ind = "text")

# Tamanho e forma dos elementos gráficos
# labelsize : tamanho da fonte dos textos, exemplo: labelsize = 4.
# pointsize : tamanho dos pontos, exemplo: pointsize = 1.5
# arrowsize : tamanho das setas (espessura), exemplo: arrowsize = 0.5
# pointshape : o formato dos pontos, exemplo : pointshape = 21.
# ggpubr::show_point_shapes() para ver os formatos disponíveis.
fviz_pca_var(res.PCA, arrowsize = 1, labelsize = 5, repel = TRUE)
fviz_pca_var(res.PCA, 
             pointsize = 3, pointshape = 21, fill = "lightblue",
             labelsize = 5, repel = TRUE)

#Elipses
# O parâmetro ellipse.type pode ser usado para mudar o tipo das elipses.
# "convex", "confidence", "t", "norm", "euclid"
fviz_pca_ind(iris.pca, geom.ind = "point", 
             col.ind = iris$Species, 
             palette = c("#00AFBB", "#E7B800", "#FC4E07"),
             addEllipses = TRUE, ellipse.type = "confidence",
             legend.title = "Groups")

fviz_pca_ind(iris.pca, geom.ind = "point",
             col.ind = iris$Species, 
             palette = c("#00AFBB", "#E7B800", "#FC4E07"),
             addEllipses = TRUE, ellipse.type = "convex",
             legend.title = "Groups")

# Pontos de média dos grupos
# Os pontos média dos grupos são mostrados por default
# Para remover as médias, usar o parâmetro mean.point = FALSE
fviz_pca_ind(iris.pca,
             geom.ind = "point", 
             group.ind = iris$Species, 
             legend.title = "Groups",
             mean.point = FALSE)

# Linhas dos eixos
# O parâmetro axes.linetype pode ser usado para especificar o tipo de linhas dos eixos.
# O default é "dashed". Os valores permitidos: "blank", "solid", "dotted", etc.
# Para ver todos os valores possíveis ggpubr::show_line_types()
fviz_pca_var(res.PCA, axes.linetype = "blank")

# Parâmetros gráficos
# Para alterar os gráficos de qualquer ggplot, pode-se usar a função ggpar() [ggpubr]
# Os parâmetros que podem ser alterados com ggpar():
# títulos, labels dos eixos, legenda
# posição da legenda. Valores possíves: "top", "bottom", "left", "right", "none"
# paleta de cores
# Temas: theme_gray(), theme_bw(), theme_minimal(), theme_classic(), theme_void()
library(ggpubr)
ind.p <- fviz_pca_ind(iris.pca, geom = "point", col.ind = iris$Species)
ggpar(ind.p,
      title = "Análise de Componentes Principais",
      subtitle = "dataset Iris",
      caption = "Source: factoextra",
      xlab = "PC1", ylab = "PC2",
      legend.title = "Espécies", legend.position = "top",
      ggtheme = theme_gray(), palette = "jco")

# Biplot
fviz_pca_biplot(res.PCA, repel = TRUE,
                col.var = "#2E9FDF", # cor das variáveis
                col.ind = "#696969"  # cor dos indivíduos
                )

fviz_pca_biplot(iris.pca, 
                col.ind = iris$Species, palette = "jco", 
                addEllipses = TRUE, label = "var",
                col.var = "black", repel = TRUE,
                legend.title = "Espécies")
                

###----------Elementos Suplementares
# O dataset decathlon2 contém variáveis contínuas sumplementares (quanti.sup, colunas 11:12),
# variáveis qualitativas suplementares (quali.sup, coluna 13) e indivíduos suplementares (ind.sup, linhas 24:27)
# As variáveis e indivíduos suplementares não são usadas para determinar os 
# componentes principais. As suas coordenadas são estimadas usando somente a informação
# fornecida pela análise dos componentes principais nas variáveis/indivíduos ativos.
# Para especificar indivíduos e variáveis suplementares na função PCA()
res.PCA <- PCA(decathlon2, ind.sup = 24:27,
               quanti.sup = 11:12, quali.sup = 13, graph = FALSE)

# Variáveis quantitativas
# Estimando (prevendo) os resultados para as variáveis suplementares quantitativas
res.PCA$quanti.sup

# Visualizando todas as variáveis (ativas e suplementares)
fviz_pca_var(res.PCA)

fviz_pca_var(res.PCA, col.var = "black", col.quanti.sup = "red")

# Ocultando as variáveis ativas
fviz_pca_var(res.PCA, invisible = 'var')
# Ocultando as variáveis suplementares
fviz_pca_var(res.PCA, invisible = "quanti.sup")

# Indivíduos
# Estimando (prevendo) os resultados dos indivíduos suplementares
res.PCA$ind.sup

# Visualizando todos os indivíduos (ativos e suplementares)
fviz_pca_ind(res.PCA, col.ind.sup = "blue", repel = TRUE)

# Adicionando as variáveis qualitativas suplementares 
p <- fviz_pca_ind(res.PCA, col.ind.sup = "blue", repel = TRUE)
p <- fviz_add(p, res.PCA$quali.sup$coord, color = "red")
p

# Variáveis qualitativas
res.PCA$quali

fviz_pca_ind(res.PCA, habillage = 13,
             addEllipses =TRUE, ellipse.type = "confidence",
             palette = "jco", repel = TRUE) 

# Filtrando os resultados
# É possível visualizar apenas alguns indivíduos/variáveis usando os parâmetros
# select.ind e select.var
# Os valores permitidos são: NULL ou uma lista contendo os parâmetros: name,cos2 ou contrib.

# Visualizando variáveis com cos2 >= 0.6
fviz_pca_var(res.PCA, select.var = list(cos2 = 0.6))

# As cinco variáveis ativas com os maiores valores de cos2
fviz_pca_var(res.PCA, select.var = list(cos2 = 5))

# Selecionando pelos nomes
nome <- list(name = c("Long.jump", "High.jump", "X100m"))
fviz_pca_var(res.PCA, select.var = nome)

# Os cinco indivíduos e variáveis com maiores contribuições
fviz_pca_biplot(res.PCA, select.ind = list(contrib = 5), 
                select.var = list(contrib = 5),
                ggtheme = theme_minimal())

###--------------- Exportando os resultados

# Exportando os gráficos para arquivos PDF/PNG

# Scree plot
scree.plot <- fviz_eig(res.PCA)
# Indivíduos
ind.plot <- fviz_pca_ind(res.PCA)
# Variáveis 
var.plot <- fviz_pca_var(res.PCA)

# Exportando para um único arquivo PDF
pdf("PCA.pdf") 
print(scree.plot)
print(ind.plot)
print(var.plot)
dev.off()

# Criando arquivos PNG individuais

png("pca-scree-plot.png")
print(scree.plot)
dev.off()

# Print individuals plot to a png file
png("pca-variaveis.png")
print(var.plot)
dev.off()

# Print variables plot to a png file
png("pca-individuos.png")
print(ind.plot)
dev.off()


# Uma alternativa para exportar objetos ggplot, usando a função ggexport()[ggpubr]

# Exportando gráficos individuais para um PDF (um gráfico por página)
ggexport(plotlist = list(scree.plot, ind.plot, var.plot),
         filename = "PCA2.pdf")

# Especificando linhas e colunas para mostrar vários gráficos na mesma página
ggexport(plotlist = list(scree.plot, ind.plot, var.plot), 
         nrow = 2, ncol = 2,
         filename = "PCA3.pdf")

# Arquivos PNG
# Se for passada uma lista de gráficos, serão criados vários arquivos png para cada
# um dos gráficos.
ggexport(plotlist = list(scree.plot, ind.plot, var.plot),
         filename = "PCA.png")

# Exportando os resultados para arquivos TXT/CSV
# Os resultados podem ser exportados usando a função write.infile()[FactoMineR]
write.infile(res.PCA, "pca.txt", sep = "\t")
write.infile(res.PCA, "pca.csv", sep = ";")

# Há outras funções para executar o PCA 

# Usando prcomp()[stats]
res.PCA <- prcomp(iris[,-5], scale. = TRUE)

# Usando princomp()[stats]
res.PCA <- princomp(iris[,-5], cor = TRUE)

# Usando dudi.pca()[ade4]
library(ade4)
res.PCA <- dudi.pca(iris[, -5], scannf = FALSE, nf = 5)

# Usando epPCA()[ExPosition]
library(ExPosition)
res.PCA <- epPCA(iris[, -5], graph = FALSE)
