# JUVENTUDES
# Seleção de regiões nos municípios do interior de MG
# Análise Multicritério
# Neylson Crepalde
########################################################

library(rgdal)
library(descr)
library(xlsx)
library(dplyr)
library(FactoMineR)
options(scipen = 999)
#library(ggmap)

setwd("C:/Users/x6905399/Documents/RASTREIA/Parceria com SEDESE/Seleção/10-Uberlândia/Mapas")
cidade = readOGR("Uberlandia_SetoresCensitariosComDados_UTMSIRGAS2000.shp")
plot(cidade)
crime = read.xlsx("C:/Users/x6905399/Documents/RASTREIA/Parceria com SEDESE/Seleção/10-Uberlândia/Homicidio Consumado - Vitima Fatal - Uberlandia - 2012 a 2015.xlsx",1)
crime = filter(crime, Geo == "SIM")

###########################################################
# Mapa quente
#m = get_map(location = c(lon=-44.891644,lat=-20.145126), zoom=12)
#m2 = ggmap(m)

plot(cidade); plot(cidade_homic, pch=19, col = 'blue', add=T)

#m2 + stat_density2d(aes(x = Longitude, y = Latitude_F, fill = ..level.., alpha=..level..),
#                    bins=15, geom='polygon', data=crime) + 
#  scale_fill_gradient(low = "black", high = "red")

cidade_homic$Latitude_F
cidade_homic$Longitude
###########################################################


###########################################################
# Análise Multicritério por PCA
library(missMDA)

crimes = table(crime$COD..Setor.CensitÃ.rio) %>% as.data.frame
names(crimes) = c("CD_GEOCODI","crimes")

multi = left_join(cidade@data, crimes)
multi_pca = select(multi, PDomicPob, PNAlfabJov, crimes)
dim(multi)

# Trabalhar com imputePCA quando tem NA's
nb = estim_ncpPCA(multi_pca, ncp.min = 0, ncp.max = 3, method.cv = "loo")
imputed <- imputePCA(multi_pca, nb$ncp)
pca_res <- PCA(imputed$completeObs, scale.unit = T)

#pca_res = PCA(multi_pca, ncp=2, scale.unit = T)

pca_res$eig
loadings <- sweep(pca_res$var$coord,2,
                  sqrt(pca_res$eig[1:ncol(pca_res$var$coord),1]),FUN="/"); print(loadings) #Loadings

scores <- matrix(,nrow(multi_pca),ncol(multi_pca))
for(i in 1:nrow(multi_pca)){
  for(j in 1:ncol(multi_pca)) scores[i,j] <- sum(multi_pca[i,]*loadings[,j])
}
print(scores)

multi$scores = scores


#writeOGR(cidade, "multi", "Uberlandia_SetoresCensitarios_Multi_UTMSIRGAS2000.shp", 
#         driver = "ESRI Shapefile")
