
setwd ("~/Desktop/PDJ_UFPA/Clima_Sp02")                                                                          
getwd()


########install devtools
install.packages("testthat")
install.packages('pkgload')
# install.packages("devtools") -  instalei via terminal com esse site 
#(https://stackoverflow.com/questions/48469005/install-devtools-package-in-ubuntu-vm)
library(devtools)

#####OBSERVACAO INSTALACAO DE PACOTES####
#Primeiro precisei fazer uns comandos no terminal. Segundo,Os pacotes - raster, 
#terra, rgeos precisei instalar via terminal e apos isso 
#instalar o biomod2


####################################################

library(ggplot2)                                                                                                    
library(mapdata)                                                                                                    
library(maptools) 
library(maps)                                                                                                       
library(sp)                                                                                                         
library(raster)                                                                                                    
library(XML) 
library(sf) 
library(rgdal) #install package pelo arquivo que baixei                                                                                             
#library(SDMTools)                                                                                                   
library(fields)                                                                                                     
library(reshape2)                                                                                                   
library(rgeos) #install package pelo arquivo que baixei 
#no site (https://github.com/cran/rgeos/releases)                                                                                                   
library(dismo)                                                                                                     
library(biomod2)                                                                                                  
library(rgbif)                                                                                                     
library(virtualspecies) 
library(gridExtra)
library(cowplot)
library("RColorBrewer")
library(lemon)
library(scales)
library(plyr)
library(dplyr)
library(tidyr)
library(tidyverse) 
library(terra) 


##########################################################
#                                                        #
#        remove highly correlated variable               #
#                                                        #
##########################################################

##path
#caminho_planilhas <- "variaveis_selecao"
#dir.create(caminho_planilhas)


### variables
#Current climate data were obtained from the WorldClim database (available at: http://worldclim.org; accessed 24 November 2016) 
#at a resolution of 2.5 arcmin (~5km at the equator) 
#for the Neotropical region (spatial limits of 110° to 30°W; 25°N to 60°S).


#all files
all_variables <- list.files("~/Desktop/PDJ_UFPA/Clima_Sp02/Atual", 
                            pattern = ".tif",
                            full.names = T)

#cleaning
lista_limpa <- grep(".tif$", all_variables, value = T)

#stack
camadas <- stack(lista_limpa)
plot(camadas[[1]])

enviro.data.reduced <- removeCollinearity(camadas, multicollinearity.cutoff = 0.85,
                                          select.variables = TRUE, sample.points = FALSE, plot = TRUE)

#enviro.data.selected<-c(  [1] "BIO_1_Theobroma_crop"  "BIO_8_Theobroma_crop"  "BIO_16_Theobroma_crop" 
#"BIO_17_Theobroma_crop"
#[5] "BIO_15_Theobroma_crop" "BIO_18_Theobroma_crop" "BIO_19_Theobroma_crop" "BIO_2_Theobroma_crop" 
#[9] "BIO_3_Theobroma_crop"  "BIO_7_Theobroma_crop" )                        


#saving
write.csv(enviro.data.reduced, "sel_var_sp02.csv", row.names = F)

dados <- read.csv("~/Desktop/PDJ_UFPA/Clima_Sp02/sel_var_sp02.csv", header = TRUE, sep = ",")
##########################################################
#                                                        #
#                       biomod2                          #
#                                                        #
##########################################################


#path                                                                                                                                               
Current_conditions <- "~/Desktop/PDJ_UFPA/Clima_Sp02/Atual"        
Current_conditions                                                                                                                 


#variables
sel_vars_sp02 <- read.table("sel_var_sp02.csv", sep = ",", header = T, 
                            as.is = T, fill = T)                                                                                                  
sel_vars_sp02                                                                                                                                                                   

#occorrences                                                                                                                                                 
species_data_cupu <- read.csv("ocorrencia_cupu.csv", sep = ",", header = T)                                                                      
table(species_data_cupu$apelido)                                                                                                                                           

species_sp02 <- paste(unique(species_data_cupu$apelido))                                                                                                                      


atuais_sp02 <- stack(list.files(path = paste(Current_conditions, sep =" "),                                                                                                       
                               pattern = ".tif$", full.names = T))                                                                                                      
intersect_mask <- function(x){
  values_x <- getValues(x)
  inter_x <- values_x %*% rep(1,nlayers(x))
  mask <- setValues(subset(x,1),values = (inter_x>0))
  return(mask)
} #garantir que as camadas estejam sobrepostas

atuais_sp02                                                                                                                                                               
nrow(atuais_sp02)
ncol(atuais_sp02)
names(atuais_sp02)

#names (atuais_sp02 [[1]])

myRespName_sp02 <- species_sp02                                                                                                                                           
myRespCoord_sp02 <- species_data_cupu[species_data_cupu$apelido==myRespName_sp02, 
                                    c("Longitude", "Latitude")]
nrow(myRespCoord_sp02)

myResp_sp02 <- rep.int(1, times = nrow(myRespCoord_sp02))#categorizar as presencas                                                                                                                   
myExpl_sp02 <- atuais_sp02 [[grep("BIO_",                                                                                                                  
                                paste(t(sel_vars_sp02)),                                                                                                                           
                                value = T)]]                                                                                                                                

names (myExpl_sp02 [[1]])
#nrow(myExpl)
#ncol(myExpl)
#devtools::install_github("valentinitnelav/geobuffer")
library(geobuffer)
library(stplanr)

#### criando background por convex hull + buffer ####
pts_sp02<-SpatialPoints(species_data_cupu[,c("Longitude", "Latitude")])
pres.bkg_sp02 <- gBuffer(pts_sp02, width = 1)
bkg_sp02 <- gBuffer(pres.bkg_sp02, width = 1, byid = T)

#### cortando as camadas climÃ¡ticas para os limites do background ####
myExpl_crop_sp02 <- crop(myExpl_sp02, extent(bkg_sp02))

myExpl_crop_sp02 <- stack(mask(myExpl_crop_sp02, 
                               intersect_mask (myExpl_crop_sp02)))

plot(myExpl_crop_sp02 [[1]])
points(pts_sp02)

names (myExpl_crop_sp02 [[1]])
writeRaster(myExpl_crop_sp02, "~/Desktop/PDJ_UFPA/Clima_Sp02/atual.grd", 
            overwrite=TRUE, format="raster")

#AQUI PARA SALVAR OS ARQUIVOS CLIMÁTICOS
writeRaster(myExpl_crop_sp02 [[1]], "~/Desktop/PDJ_UFPA/Clima_Sp02/bio1_atual_crop.img", format="raster")
writeRaster(myExpl_crop_sp02 [[10]], "~/Desktop/PDJ_UFPA/Clima_Sp02/bio7_atual_crop.img", format="raster")

library(raster)
library(terra)
library(ggtext)
library(tidyterra)

#### quality threshold
eval_threshold <- 0.7  #limiar de qualidade                                                                                                                                                                    

cat('>>>>>>>>>>>>>>>>Modeling >>>  ', myRespName_sp02, '  <<< now <<<<<<<<<<<<<<<<<<<<<<<<<')                                                                         

#### Formatação dos dados #for presence and ausence
myBiomodData_sp02 <- BIOMOD_FormatingData(resp.var = myResp_sp02,                                                                                                             
                                         expl.var = myExpl_crop_sp02,                                                                                                             
                                         resp.xy = myRespCoord_sp02,                                                                                                         
                                         resp.name = myRespName_sp02,                                                                                                        
                                         PA.nb.rep = 3,                                                                                                                 
                                         PA.nb.absences = 500,                                                                                
                                         PA.strategy = 'random')                                                                                                        

myBiomodData_sp02                                                                                                                                                            
plot(myBiomodData_sp02)
#summary(myBiomodData_sp02)

# Model single models
myBiomodModelOut_sp02 <- BIOMOD_Modeling(bm.format = myBiomodData_sp02,
                                    modeling.id = 'AllModels',
                                    CV.strategy = 'random',
                                    CV.nb.rep = 10,
                                    CV.perc = 0.8,
                                    OPT.strategy = 'bigboss',
                                    var.import = 5,
                                    metric.eval = c('TSS','ROC'))


get_evaluations(myBiomodModelOut_sp02)
get_variables_importance(myBiomodModelOut_sp02)

models.proj <- get_built_models(myBiomodModelOut_sp02, algo = "RF")

##projecting current 
myBiomodProj_sp02 <- BIOMOD_Projection(bm.mod = myBiomodModelOut_sp02,                                                                                               
                                           new.env = myExpl_crop_sp02, #myExpl_crop
                                           proj.name = 'Current',
                                           new.env.xy= NULL, 
                                           models.chosen = 'all',                                                                                                          
                                           metric.binary = 'TSS',                                                                                                              
                                           compress = F,                                                                                                                     
                                           build.clamping.mask = F, 
                                           output.format = '.img') 

#ensemble FUTURE1A                                                                                                                                                
# Model ensemble models
myBiomodEM_algo_sp02 <- BIOMOD_EnsembleModeling(bm.mod = myBiomodModelOut_sp02,
                                                   models.chosen = 'all',
                                                   em.by = 'all',
                                                   em.algo = c('EMmean'),
                                                   metric.select = c('TSS'),
                                                   metric.select.thresh = c(0.7),
                                                   metric.eval = c('TSS', 'ROC'),
                                                   var.import = 3,
                                                   EMci.alpha = 0.05,
                                                   EMwmean.decay = 'proportional')

myBiomodEM_algo_sp02

# saving ensemble evaluations  Current 

# Criando a pasta se ela não existir
if (!dir.exists(myRespName_sp02)) {
  dir.create(myRespName_sp02)
}

capture.output(myBiomodEM_algo_sp02, 
               file = file.path(myRespName_sp02, 
                                paste(myRespName_sp02, "_EM_algo_sp02.txt", sep = "")))

capture.output(get_evaluations(myBiomodEM_algo_sp02),                                                                                                                 
               file = file.path(myRespName_sp02,                                                                                                                         
                                paste(myRespName_sp02, "_eval_EM_algo_sp02.txt", sep = "")))                                                                               


#ensemble projecting Current)
#setwd("~/Desktop/crop_rasters_2")
myBiomodEMProj_EM_algo_sp02 <- BIOMOD_EnsembleForecasting(bm.em = myBiomodEM_algo_sp02,
                                                             proj.name = 'ensemble_Current',
                                                             new.env =myExpl_crop_F1a_sp02,
                                                             models.chosen = 'all',
                                                             metric.binary = 'all',
                                                             metric.filter = 'all', 
                                                             output.format = '.img')
myBiomodEMProj_EM_algo_sp02
plot(myBiomodEMProj_EM_algo_sp02)



rm(list= ls()[(ls() %in% c("myExpl_sp02", "myBiomodProj_sp02", 
                           "myBiomodEM_algo_sp02", 
                           "myBiomodProj_EM_algo_sp02"))])                     



# Get evaluation scores & variables importance
#capture.output(get_variables_importance(myBiomodModelOut), file = paste0(myRespName_fev, "/",myRespName_fev,"_var_importance.txt"))
myBiomodVarImp <- get_variables_importance(myBiomodModelOut_sp02)
write.csv(myBiomodVarImp, "varimp.csv")

# Ler um arquivo .csv e transformar em data frame
dados_clima <- read.csv("varimp.csv", header = TRUE, sep = ",")

# Visualizar os primeiros dados para garantir que foi carregado corretamente
head(dados_clima)

# Se o pacote ggplot2 não estiver instalado, instale-o
# install.packages("ggplot2")

library(ggplot2)

# Supondo que seus dados estão em um data frame chamado 'dados_clima'
ggplot(dados_clima, aes(x = reorder(expl.var, var.imp), y = var.imp)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  coord_flip() +  # Rotaciona o gráfico para melhorar a leitura
  labs(x = "Variáveis Climáticas", y = "Importância", title = "Importância das Variáveis Climáticas") +
  theme_minimal()

#FAZER PLOT PARA VER AS VARIÁVEIS EM GRÁFICO
####BIO 7: Amplitude térmica anual 
#######(Máx. Temperatura do mês mais quente - Min. Temperatura do mês mais frio) 
#BIO1 =Temperatura anual média

################################ FUTURE  ##################################                                                                                         
#modelos com o período de 2021-2040 (MICROC6)

Future_1A <- "~/Desktop/PDJ_UFPA/Clima_Sp02/Futuro_2/MIROC6_1/ssp126"
Future_1B <- "~/Desktop/PDJ_UFPA/Clima_Sp02/Futuro_2/MIROC6_1/ssp245"  
Future_1C <- "~/Desktop/PDJ_UFPA/Clima_Sp02/Futuro_2/MIROC6_1/ssp370"
Future_1D <- "~/Desktop/PDJ_UFPA/Clima_Sp02/Futuro_2/MIROC6_1/ssp585"

################################MIROC6
###MIROC6-ssp126
### FUTURE1a                                                                                                                                                        
futuro1a_sp02 <- stack(list.files(path = paste(Future_1A, sep=''),                                                                                                       
                                  pattern = ".tif$", full.names = T))  
#todas_futuras1a                                                                                                                                                    
intersect_mask <- function(x){
  values_x <- getValues(x)
  inter_x <- values_x %*% rep(1,nlayers(x))
  mask <- setValues(subset(x,1),values = (inter_x>0))
  return(mask)
}   

names(futuro1a_sp02)
names(futuro1a_sp02) <- c('BIO_1_Theobroma_crop', 'BIO_2_Theobroma_crop', 
                          'BIO_3_Theobroma_crop','BIO_4_Theobroma_crop', 
                          'BIO_5_Theobroma_crop', 'BIO_6_Theobroma_crop',
                          'BIO_7_Theobroma_crop', 'BIO_8_Theobroma_crop', 
                          'BIO_9_Theobroma_crop','BIO_10_Theobroma_crop', 
                          'BIO_11_Theobroma_crop', 'BIO_12_Theobroma_crop',
                          'BIO_13_Theobroma_crop', 'BIO_14_Theobroma_crop', 
                          'BIO_15_Theobroma_crop',
                          'BIO_16_Theobroma_crop', 'BIO_17_Theobroma_crop', 
                          'BIO_18_Theobroma_crop','BIO_19_Theobroma_crop')

sel_vars_2 <- read.table("sel_var_sp02.csv", sep = ",", header = T, as.is = T, fill = T)                                                                                                  
sel_vars_2 

myExpl_F1a_sp02 <- futuro1a_sp02 [[grep ("BIO_",  #mudar aqui                                                                                      
                                        paste(t(sel_vars_2)),                                                                                                                  
                                        value = T)]]   

#### criando background por convex hull + buffer ####
pts<-SpatialPoints(species_data_cupu[,c("Longitude", "Latitude")])
pres.bkg <- gBuffer(pts, width = 1)
bkg <- gBuffer(pres.bkg, width = 1, byid = T)

#### cortando as camadas climÃ¡ticas para os limites do background ####
#mudar AQUI
myExpl_crop_F1a_sp02 <- crop(myExpl_F1a_sp02, extent(bkg))

myExpl_crop_F1a_sp02 <- stack(mask(myExpl_crop_F1a_sp02, 
                                   intersect_mask(myExpl_crop_F1a_sp02)))

plot(myExpl_crop_F1a_sp02 [[1]])
points(pts)

names (myExpl_crop_F1a_sp02[[10]]) 
writeRaster(myExpl_crop_F1a_sp02, "~/Desktop/PDJ_UFPA/Clima_Sp02/future_F1A.grd", format="raster")
writeRaster(myExpl_crop_F1a_sp02[[1]], "~/Desktop/PDJ_UFPA/Clima_Sp02/BIO_1_Theobroma_future_crop_ssp126.grd", format="raster")
writeRaster(myExpl_crop_F1a_sp02[[10]], "~/Desktop/PDJ_UFPA/Clima_Sp02/BIO_7__Theobroma_future_crop_ssp126.grd", format="raster")
#myExpl_F1a <- stack(mask(myExpl_F1a, intersect_mask(myExpl_F1a)))

#myExpl_F1a                                                                                                                                                         

rm(list= ls()[(ls() %in% c("futuras1a_sp02"))])                                                                                  


#projecting FUTURE1A                                                                                                                               
myBiomodProj_F1a_sp02 <- BIOMOD_Projection(bm.mod = myBiomodModelOut_sp02,                                                                                               
                                          new.env = myExpl_crop_F1a_sp02, #myExpl_crop
                                          proj.name = 'Futuro1A',
                                          new.env.xy= NULL, 
                                          models.chosen = 'all',                                                                                                          
                                          metric.binary = 'TSS',                                                                                                              
                                          compress = F,                                                                                                                     
                                          build.clamping.mask = F, 
                                          output.format = '.img') 

#ensemble FUTURE1A                                                                                                                                                
# Model ensemble models
myBiomodEM_algoF1A_sp02 <- BIOMOD_EnsembleModeling(bm.mod = myBiomodModelOut_sp02,
                                           models.chosen = 'all',
                                           em.by = 'all',
                                           em.algo = c('EMmean'),
                                           metric.select = c('TSS'),
                                           metric.select.thresh = c(0.7),
                                           metric.eval = c('TSS', 'ROC'),
                                           var.import = 3,
                                           EMci.alpha = 0.05,
                                           EMwmean.decay = 'proportional')

myBiomodEM_algoF1A_sp02

# saving ensemble evaluations  FUTURE1A   

# Criando a pasta se ela não existir
if (!dir.exists(myRespName_sp02)) {
  dir.create(myRespName_sp02)
}

capture.output(myBiomodEM_algoF1A_sp02, 
               file = file.path(myRespName_sp02, 
                                paste(myRespName_sp02, "_EM_algoF1A_sp02.txt", sep = "")))

capture.output(get_evaluations(myBiomodEM_algoF1A_sp02),                                                                                                                 
               file = file.path(myRespName_sp02,                                                                                                                         
                                paste(myRespName_sp02, "_eval_EM_algoF1A_sp02.txt", sep = "")))                                                                               


#ensemble projecting FUTURE1a)
#setwd("~/Desktop/crop_rasters_2")
myBiomodEMProj_EM_algoF1A_sp02 <- BIOMOD_EnsembleForecasting(bm.em = myBiomodEM_algoF1A_sp02,
                                                  proj.name = 'ensemble_Futuro1A',
                                                  new.env =myExpl_crop_F1a_sp02,
                                                  models.chosen = 'all',
                                                  metric.binary = 'all',
                                                  metric.filter = 'all', 
                                                  output.format = '.img')
myBiomodEMProj_EM_algoF1A_sp02
plot(myBiomodEMProj_EM_algoF1A_sp02)

                                                                                           

rm(list= ls()[(ls() %in% c("myExpl_F1A_sp02", "myBiomodProj_F1A_sp02", 
                           "myBiomodEM_algoF1A_sp02", 
                           "myBiomodProj_EM_algoF1A_sp02"))])                     


###MIROC6 - ssp245 
### FUTURE1b                                                                                                                                                        
futuras1B_sp02 <- stack(list.files(path=paste(Future_1B, sep=''), pattern=".tif$", full.names=TRUE))                                                                  
#todas_futuras1b                                                                                                                                                    

names(futuras1B_sp02)
names(futuras1B_sp02) <- c('BIO_1_Theobroma_crop', 'BIO_2_Theobroma_crop', 
                            'BIO_3_Theobroma_crop','BIO_4_Theobroma_crop', 
                            'BIO_5_Theobroma_crop', 'BIO_6_Theobroma_crop',
                            'BIO_7_Theobroma_crop', 'BIO_8_Theobroma_crop', 
                            'BIO_9_Theobroma_crop','BIO_10_Theobroma_crop', 
                            'BIO_11_Theobroma_crop', 'BIO_12_Theobroma_crop',
                            'BIO_13_Theobroma_crop', 'BIO_14_Theobroma_crop', 
                            'BIO_15_Theobroma_crop',
                            'BIO_16_Theobroma_crop', 'BIO_17_Theobroma_crop', 
                            'BIO_18_Theobroma_crop','BIO_19_Theobroma_crop')

myExpl_F1B_sp02 <- futuras1B_sp02 [[grep("BIO_",                                                                                        
                                     paste(t(sel_vars_2)),                                                                                                                  
                                     value = T)]]  

#### criando background por convex hull + buffer ####
pts<-SpatialPoints(species_data_cupu[,c("Longitude", "Latitude")])
pres.bkg <- gBuffer(pts, width = 1)
bkg <- gBuffer(pres.bkg, width = 1, byid = T)

#### cortando as camadas climÃ¡ticas para os limites do background ####
#mudar AQUI
myExpl_crop_F1B_sp02 <- crop(myExpl_F1B_sp02, extent(bkg))

myExpl_crop_F1B_sp02 <- stack(mask(myExpl_crop_F1B_sp02, intersect_mask
                                   (myExpl_crop_F1B_sp02)))

plot(myExpl_crop_F1B_sp02 [[1]])
points(pts)

names (myExpl_crop_F1B_sp02[[1]])
writeRaster(myExpl_crop_F1B_sp02, "~/Desktop/PDJ_UFPA/Clima_Sp02/future_F1B.grd", format="raster")
#writeRaster(myExpl_crop_F1B_out[[5]], "/media/waleria/WALERIA_HD/clima_outubro/Pilocarpus/bio_15_pilocarpus_crop_ssp85.grd", format="raster")
#writeRaster(myExpl_crop_F1B_out[[7]], "/media/waleria/WALERIA_HD/clima_outubro/Pilocarpus/bio_19_pilocarpus_crop_ssp85.grd", format="raster")
#myExpl_F1b <- stack(mask(myExpl_F1b, intersect_mask(myExpl_F1b)))
#myExpl_F1b                                                                                                                                                         

rm(list= ls()[(ls() %in% c("futuras1B_sp02"))]) 


#projecting FUTURE1b 
myBiomodProj_F1b_sp02 <- BIOMOD_Projection(bm.mod = myBiomodModelOut_sp02,                                                                                               
                                           new.env = myExpl_crop_F1B_sp02, #myExpl_crop
                                           proj.name = 'Futuro1B',
                                           new.env.xy= NULL, 
                                           models.chosen = 'all',                                                                                                          
                                           metric.binary = 'TSS',                                                                                                              
                                           compress = F,                                                                                                                     
                                           build.clamping.mask = F, 
                                           output.format = '.img') 


#ensemble FUTURE1b                                                                                                                                                
myBiomodEM_algoF1b_sp02 <- BIOMOD_EnsembleModeling(bm.mod = myBiomodModelOut_sp02,
                                                   models.chosen = 'all',
                                                   em.by = 'all',
                                                   em.algo = c('EMmean'),
                                                   metric.select = c('TSS'),
                                                   metric.select.thresh = c(0.7),
                                                   metric.eval = c('TSS', 'ROC'),
                                                   var.import = 3,
                                                   EMci.alpha = 0.05,
                                                   EMwmean.decay = 'proportional')

                                                                                           
# saving ensemble evaluations FUTURE1b  

# Criando a pasta se ela não existir
if (!dir.exists(myRespName_sp02)) {
  dir.create(myRespName_sp02)
}
#setwd("~/Desktop/crop_rasters_2/Pilocarpus/models")  
capture.output(myBiomodEM_algoF1b_sp02,                                                                                                                                  
               file = file.path(myRespName_sp02,                                                                                                                         
                                paste(myRespName_sp02, "_EM_algoF1B.txt", sep = "")))                                                                                    

capture.output(get_evaluations(myBiomodEM_algoF1b_sp02),                                                                                                                 
               file = file.path(myRespName_sp02,                                                                                                                         
                                paste(myRespName_sp02, "_eval_EM_algoF1B.txt", sep = "")))                                                                               


#projecting ensemble FUTURE1b
#setwd("~/Desktop/crop_rasters_2")
myBiomodEMProj_EM_algoF1B_sp02 <- BIOMOD_EnsembleForecasting(bm.em = myBiomodEM_algoF1b_sp02,
                                                             proj.name = 'ensemble_Futuro1B',
                                                             new.env =myExpl_crop_F1B_sp02,
                                                             models.chosen = 'all',
                                                             metric.binary = 'all',
                                                             metric.filter = 'all', 
                                                             output.format = '.img')
myBiomodEMProj_EM_algoF1B_sp02
plot(myBiomodEMProj_EM_algoF1B_sp02)


rm(list= ls()[(ls() %in% c("myExpl_crop_F1B_sp02", "myBiomodProj_F1b_sp02", 
                           "myBiomodEM_algoF1b_sp02", 
                           "myBiomodEMProj_EM_algoF1B_sp02"))])         

#####MIROC6 - ssp370 
### FUTURE1C                                                                                                                                                        
futuras1C_sp02 <- stack(list.files(path=paste(Future_1C, sep=''), pattern=".tif$", full.names=TRUE))                                                                  
#todas_futuras1C                                                                                                                                                  

names(futuras1C_sp02)
names(futuras1C_sp02) <- c('BIO_1_Theobroma_crop', 'BIO_2_Theobroma_crop', 
                           'BIO_3_Theobroma_crop','BIO_4_Theobroma_crop', 
                           'BIO_5_Theobroma_crop', 'BIO_6_Theobroma_crop',
                           'BIO_7_Theobroma_crop', 'BIO_8_Theobroma_crop', 
                           'BIO_9_Theobroma_crop','BIO_10_Theobroma_crop', 
                           'BIO_11_Theobroma_crop', 'BIO_12_Theobroma_crop',
                           'BIO_13_Theobroma_crop', 'BIO_14_Theobroma_crop', 
                           'BIO_15_Theobroma_crop',
                           'BIO_16_Theobroma_crop', 'BIO_17_Theobroma_crop', 
                           'BIO_18_Theobroma_crop','BIO_19_Theobroma_crop')

myExpl_1C_sp02<-futuras1C_sp02[[grep("BIO_",                                                                                        
                                    paste(t(sel_vars_2)),                                                                                                                    
                                    value = T)]] 

#### criando background por convex hull + buffer ####
pts<-SpatialPoints(species_data_cupu[,c("Longitude", "Latitude")])
pres.bkg <- gBuffer(pts, width = 1)
bkg <- gBuffer(pres.bkg, width = 1, byid = T)

#### cortando as camadas climÃ¡ticas para os limites do background ####
#mudar AQUI
myExpl_crop_F1C_sp02 <- crop(myExpl_1C_sp02, extent(bkg))

myExpl_crop_F1C_sp02 <- stack(mask(myExpl_crop_F1C_sp02, intersect_mask
                                   (myExpl_crop_F1C_sp02)))

plot(myExpl_crop_F1C_sp02 [[1]])
points(pts)
names (myExpl_crop_F1C_sp02[[1]])
writeRaster(myExpl_crop_F1C_sp02, "~/Desktop/PDJ_UFPA/Clima_Sp02/future_F1C.grd", format="raster",overwrite=TRUE)
#myExpl_F1c <- stack(mask(myExpl_F1c, intersect_mask(myExpl_F1c)))

#myExpl_F1c                                                                                                                                                         

rm(list= ls()[(ls() %in% c("futuras1C_sp02"))])


#projecting FUTURE1C                                                                                                                             
myBiomodProj_F1c_sp02 <- BIOMOD_Projection(bm.mod = myBiomodModelOut_sp02,                                                                                               
                                           new.env = myExpl_crop_F1C_sp02, #myExpl_crop
                                           proj.name = 'Futuro1C',
                                           new.env.xy= NULL, 
                                           models.chosen = 'all',                                                                                                          
                                           metric.binary = 'TSS',                                                                                                              
                                           compress = F,                                                                                                                     
                                           build.clamping.mask = F, 
                                           output.format = '.img')


#ensemble FUTURE1C 
myBiomodEM_algoF1c_sp02<- BIOMOD_EnsembleModeling(bm.mod = myBiomodModelOut_sp02,
                                                   models.chosen = 'all',
                                                   em.by = 'all',
                                                   em.algo = c('EMmean'),
                                                   metric.select = c('TSS'),
                                                   metric.select.thresh = c(0.7),
                                                   metric.eval = c('TSS', 'ROC'),
                                                   var.import = 3,
                                                   EMci.alpha = 0.05,
                                                   EMwmean.decay = 'proportional')

myBiomodEM_algoF1c_sp02

                                                                                                        
# Criando a pasta se ela não existir
if (!dir.exists(myRespName_sp02)) {
  dir.create(myRespName_sp02)
}
# saving ensemble evaluations FUTURE1c                                                                                                                          
#setwd("~/Desktop/crop_rasters_2/Pilocarpus/models")  
capture.output(myBiomodEM_algoF1c_sp02,                                                                                                                                  
               file = file.path(myRespName_sp02,                                                                                                                         
                                paste(myRespName_sp02, "_EM_algoF1c.txt", sep = "")))                                                                                    

capture.output(get_evaluations(myBiomodEM_algoF1c_sp02),                                                                                                                 
               file = file.path(myRespName_sp02,                                                                                                                         
                                paste(myRespName_sp02, "_eval_EM_algoF1c.txt", sep = "")))                                                                               


#projecting ensemble FUTURE1c 
#setwd("~/Desktop/crop_rasters_2")
myBiomodEMProj_EM_algoF1C_sp02 <- BIOMOD_EnsembleForecasting(bm.em = myBiomodEM_algoF1c_sp02,
                                                             proj.name = 'ensemble_Futuro1C',
                                                             new.env =myExpl_crop_F1C_sp02,
                                                             models.chosen = 'all',
                                                             metric.binary = 'all',
                                                             metric.filter = 'all', 
                                                             output.format = '.img')

rm(list= ls()[(ls() %in% c("myExpl_crop_F1C_sp02", "myBiomodProj_F1c_sp02", 
                           "myBiomodEM_algoF1c_sp02", 
                           "myBiomodEMProj_EM_algoF1C_sp02"))]) 

#####MIROC6 - ssp585
###FUTURE1D                                                                                                                                                        
futuras1D_sp02<- stack(list.files(path=paste(Future_1D, sep=''), 
                                 pattern=".tif$", full.names=TRUE))                                                                  
#todas_futuras2b                                                                                                                                                    

names(futuras1D_sp02)
names(futuras1D_sp02) <- c('BIO_1_Theobroma_crop', 'BIO_2_Theobroma_crop', 
                           'BIO_3_Theobroma_crop','BIO_4_Theobroma_crop', 
                           'BIO_5_Theobroma_crop', 'BIO_6_Theobroma_crop',
                           'BIO_7_Theobroma_crop', 'BIO_8_Theobroma_crop', 
                           'BIO_9_Theobroma_crop','BIO_10_Theobroma_crop', 
                           'BIO_11_Theobroma_crop', 'BIO_12_Theobroma_crop',
                           'BIO_13_Theobroma_crop', 'BIO_14_Theobroma_crop', 
                           'BIO_15_Theobroma_crop',
                           'BIO_16_Theobroma_crop', 'BIO_17_Theobroma_crop', 
                           'BIO_18_Theobroma_crop','BIO_19_Theobroma_crop')  

myExpl_1D_sp02<-futuras1D_sp02[[grep("BIO_",                                                                                        
                                    paste(t(sel_vars_2)),                                                                                                                    
                                    value = T)]] 

#### criando background por convex hull + buffer ####
pts<-SpatialPoints(species_data_cupu[,c("Longitude", "Latitude")])
pres.bkg <- gBuffer(pts, width = 1)
bkg <- gBuffer(pres.bkg, width = 1, byid = T)

#### cortando as camadas climÃ¡ticas para os limites do background ####
#mudar AQUI
myExpl_crop_1D_sp02 <- crop(myExpl_1D_sp02, extent(bkg))

myExpl_crop_1D_sp02 <- stack(mask(myExpl_crop_1D_sp02, 
                                  intersect_mask (myExpl_crop_1D_sp02)))


plot(myExpl_crop_1D_sp02[[1]])
points(pts)
names (myExpl_crop_1D_sp02 [[1]])
writeRaster(myExpl_crop_1D_sp02, "~/Desktop/PDJ_UFPA/Clima_Sp02/future_F1D.grd", format="raster")
writeRaster(myExpl_crop_1D_sp02[[1]], "~/Desktop/PDJ_UFPA/Clima_Sp02/BIO_1_Theobroma_future_crop_ssp585.grd", format="raster")
writeRaster(myExpl_crop_1D_sp02[[10]], "~/Desktop/PDJ_UFPA/Clima_Sp02/BIO_7__Theobroma_future_crop_ssp585.grd", format="raster")
#myExpl_F1c <- stack(mask(myExpl_F1c, intersect_mask(myExpl_F1c)))
#myExpl_F1c                                                                                                                                                         

rm(list= ls()[(ls() %in% c("futuras1D_sp02"))])



#projecting FUTURE1D                                                                                                                                 
myBiomodProj_F1d_sp02 <- BIOMOD_Projection(bm.mod = myBiomodModelOut_sp02,                                                                                               
                                           new.env = myExpl_crop_1D_sp02, #myExpl_crop
                                           proj.name = 'Futuro1D',
                                           new.env.xy= NULL, 
                                           models.chosen = 'all',                                                                                                          
                                           metric.binary = 'TSS',                                                                                                              
                                           compress = F,                                                                                                                     
                                           build.clamping.mask = F, 
                                           output.format = '.img')


#ensemble  FUTURE1D                                                                                                                                                 
myBiomodEM_algoF1d_sp02<- BIOMOD_EnsembleModeling(bm.mod = myBiomodModelOut_sp02,
                                                  models.chosen = 'all',
                                                  em.by = 'all',
                                                  em.algo = c('EMmean'),
                                                  metric.select = c('TSS'),
                                                  metric.select.thresh = c(0.7),
                                                  metric.eval = c('TSS', 'ROC'),
                                                  var.import = 3,
                                                  EMci.alpha = 0.05,
                                                  EMwmean.decay = 'proportional')



# saving ensemble evaluations  FUTURE1D   

# Criando a pasta se ela não existir
if (!dir.exists(myRespName_sp02)) {
  dir.create(myRespName_sp02)
}

# Capturando a saída e salvando no arquivo
capture.output(myBiomodEM_algoF1d_sp02, 
               file = file.path(myRespName_sp02, 
                                paste(myRespName_sp02, "_EM_algoF1d_sp02.txt", sep = "")))

capture.output(get_evaluations(myBiomodEM_algoF1d_sp02),                                                                                                                 
               file = file.path(myRespName_sp02,                                                                                                                         
                                paste(myRespName_sp02, "_eval_EM_algoF1d_sp02.txt", sep = "")))                                                                               


#projecting ensemble  FUTURE1D    
#setwd("~/Desktop/crop_rasters_2")
myBiomodEMProj_EM_algoF1D_sp02 <- BIOMOD_EnsembleForecasting(bm.em = myBiomodEM_algoF1d_sp02,
                                                             proj.name = 'ensemble_Futuro1D',
                                                             new.env =myExpl_crop_1D_sp02,
                                                             models.chosen = 'all',
                                                             metric.binary = 'all',
                                                             metric.filter = 'all', 
                                                             output.format = '.img')

rm(list= ls()[(ls() %in% c("myExpl_crop_F1D_sp02", "myBiomodProj_F1d_sp02", 
                           "myBiomodEM_algoF1d_sp02", 
                           "myBiomodEMProj_EM_algoF1D_sp02"))])           


#modelos com o período de 2061-2080 (MICROC6)

Future_1E <- "~/Desktop/PDJ_UFPA/Clima_Sp02/Futuro_2/MIROC6_2/ssp126"
Future_1F <- "~/Desktop/PDJ_UFPA/Clima_Sp02/Futuro_2/MIROC6_2/ssp245"  
Future_1G <- "~/Desktop/PDJ_UFPA/Clima_Sp02/Futuro_2/MIROC6_2/ssp370"
Future_1H <- "~/Desktop/PDJ_UFPA/Clima_Sp02/Futuro_2/MIROC6_2/ssp585"

################################MIROC6-2 período 2041-2060
###MIROC6-2/ssp126
### FUTURE1E   

futuras1E_sp02<- stack(list.files(path=paste(Future_1E, sep=''), 
                                  pattern=".tif$", full.names=TRUE))                                                                  
#todas_futuras2b                                                                                                                                                    

names(futuras1E_sp02)
names(futuras1E_sp02) <- c('BIO_1_Theobroma_crop', 'BIO_2_Theobroma_crop', 
                           'BIO_3_Theobroma_crop','BIO_4_Theobroma_crop', 
                           'BIO_5_Theobroma_crop', 'BIO_6_Theobroma_crop',
                           'BIO_7_Theobroma_crop', 'BIO_8_Theobroma_crop', 
                           'BIO_9_Theobroma_crop','BIO_10_Theobroma_crop', 
                           'BIO_11_Theobroma_crop', 'BIO_12_Theobroma_crop',
                           'BIO_13_Theobroma_crop', 'BIO_14_Theobroma_crop', 
                           'BIO_15_Theobroma_crop',
                           'BIO_16_Theobroma_crop', 'BIO_17_Theobroma_crop', 
                           'BIO_18_Theobroma_crop','BIO_19_Theobroma_crop')  

myExpl_1E_sp02<-futuras1E_sp02[[grep("BIO_",                                                                                        
                                     paste(t(sel_vars_2)),                                                                                                                    
                                     value = T)]] 

#### criando background por convex hull + buffer ####
pts<-SpatialPoints(species_data_cupu[,c("Longitude", "Latitude")])
pres.bkg <- gBuffer(pts, width = 1)
bkg <- gBuffer(pres.bkg, width = 1, byid = T)

#### cortando as camadas climÃ¡ticas para os limites do background ####
#mudar AQUI
myExpl_crop_1E_sp02 <- crop(myExpl_1E_sp02, extent(bkg))

myExpl_crop_1E_sp02 <- stack(mask(myExpl_crop_1E_sp02, 
                                  intersect_mask (myExpl_crop_1E_sp02)))


plot(myExpl_crop_1E_sp02[[1]])
points(pts)
names (myExpl_crop_1E_sp02 [[1]])
writeRaster(myExpl_crop_1E_sp02, "~/Desktop/PDJ_UFPA/Clima_Sp02/future_F1E.grd", 
            format="raster", overwrite=TRUE)
#myExpl_F1c <- stack(mask(myExpl_F1c, intersect_mask(myExpl_F1c)))
#myExpl_F1c                                                                                                                                                         

rm(list= ls()[(ls() %in% c("futuras1E_sp02"))])



#projecting FUTURE1E                                                                                                                                
myBiomodProj_F1E_sp02 <- BIOMOD_Projection(bm.mod = myBiomodModelOut_sp02,                                                                                               
                                           new.env = myExpl_crop_1E_sp02, #myExpl_crop
                                           proj.name = 'Futuro1E',
                                           new.env.xy= NULL, 
                                           models.chosen = 'all',                                                                                                          
                                           metric.binary = 'TSS',                                                                                                              
                                           compress = F,                                                                                                                     
                                           build.clamping.mask = F, 
                                           output.format = '.img')


#ensemble  FUTURE1E                                                                                                                                               
myBiomodEM_algoF1E_sp02<- BIOMOD_EnsembleModeling(bm.mod = myBiomodModelOut_sp02,
                                                  models.chosen = 'all',
                                                  em.by = 'all',
                                                  em.algo = c('EMmean'),
                                                  metric.select = c('TSS'),
                                                  metric.select.thresh = c(0.7),
                                                  metric.eval = c('TSS', 'ROC'),
                                                  var.import = 3,
                                                  EMci.alpha = 0.05,
                                                  EMwmean.decay = 'proportional')



# saving ensemble evaluations  FUTURE1E  

# Criando a pasta se ela não existir
if (!dir.exists(myRespName_sp02)) {
  dir.create(myRespName_sp02)
}

# Capturando a saída e salvando no arquivo
capture.output(myBiomodEM_algoF1E_sp02, 
               file = file.path(myRespName_sp02, 
                                paste(myRespName_sp02, "_EM_algoF1E_sp02.txt", sep = "")))

capture.output(get_evaluations(myBiomodEM_algoF1E_sp02),                                                                                                                 
               file = file.path(myRespName_sp02,                                                                                                                         
                                paste(myRespName_sp02, "_eval_EM_algoF1E_sp02.txt", sep = "")))                                                                               


#projecting ensemble  FUTURE1E    
#setwd("~/Desktop/crop_rasters_2")
myBiomodEMProj_EM_algoF1E_sp02 <- BIOMOD_EnsembleForecasting(bm.em = myBiomodEM_algoF1E_sp02,
                                                             proj.name = 'ensemble_Futuro1E',
                                                             new.env =myExpl_crop_1E_sp02,
                                                             models.chosen = 'all',
                                                             metric.binary = 'all',
                                                             metric.filter = 'all', 
                                                             output.format = '.img')

rm(list= ls()[(ls() %in% c("myExpl_crop_F1E_sp02", "myBiomodProj_F1E_sp02", 
                           "myBiomodEM_algoF1E_sp02", 
                           "myBiomodEMProj_EM_algoF1E_sp02"))])           



###MIROC6-2/ssp245
### FUTURE1F   

futuras1F_sp02<- stack(list.files(path=paste(Future_1F, sep=''), 
                                  pattern=".tif$", full.names=TRUE))                                                                  
#todas_futuras2b                                                                                                                                                    

names(futuras1F_sp02)
names(futuras1F_sp02) <- c('BIO_1_Theobroma_crop', 'BIO_2_Theobroma_crop', 
                           'BIO_3_Theobroma_crop','BIO_4_Theobroma_crop', 
                           'BIO_5_Theobroma_crop', 'BIO_6_Theobroma_crop',
                           'BIO_7_Theobroma_crop', 'BIO_8_Theobroma_crop', 
                           'BIO_9_Theobroma_crop','BIO_10_Theobroma_crop', 
                           'BIO_11_Theobroma_crop', 'BIO_12_Theobroma_crop',
                           'BIO_13_Theobroma_crop', 'BIO_14_Theobroma_crop', 
                           'BIO_15_Theobroma_crop',
                           'BIO_16_Theobroma_crop', 'BIO_17_Theobroma_crop', 
                           'BIO_18_Theobroma_crop','BIO_19_Theobroma_crop')  

myExpl_1F_sp02<-futuras1F_sp02[[grep("BIO_",                                                                                        
                                     paste(t(sel_vars_2)),                                                                                                                    
                                     value = T)]] 

#### criando background por convex hull + buffer ####
pts<-SpatialPoints(species_data_cupu[,c("Longitude", "Latitude")])
pres.bkg <- gBuffer(pts, width = 1)
bkg <- gBuffer(pres.bkg, width = 1, byid = T)

#### cortando as camadas climÃ¡ticas para os limites do background ####
#mudar AQUI
myExpl_crop_1F_sp02 <- crop(myExpl_1F_sp02, extent(bkg))

myExpl_crop_1F_sp02 <- stack(mask(myExpl_crop_1F_sp02, 
                                  intersect_mask (myExpl_crop_1F_sp02)))


plot(myExpl_crop_1F_sp02[[1]])
points(pts)
names (myExpl_crop_1F_sp02 [[1]])
writeRaster(myExpl_crop_1F_sp02, "~/Desktop/PDJ_UFPA/Clima_Sp02/future_F1F.grd", 
            format="raster", overwrite=TRUE)
#myExpl_F1c <- stack(mask(myExpl_F1c, intersect_mask(myExpl_F1c)))
#myExpl_F1c                                                                                                                                                         

rm(list= ls()[(ls() %in% c("futuras1F_sp02"))])



#projecting FUTURE1F                                                                                                                               
myBiomodProj_F1F_sp02 <- BIOMOD_Projection(bm.mod = myBiomodModelOut_sp02,                                                                                               
                                           new.env = myExpl_crop_1F_sp02, #myExpl_crop
                                           proj.name = 'Futuro1F',
                                           new.env.xy= NULL, 
                                           models.chosen = 'all',                                                                                                          
                                           metric.binary = 'TSS',                                                                                                              
                                           compress = F,                                                                                                                     
                                           build.clamping.mask = F, 
                                           output.format = '.img')


#ensemble  FUTURE1F                                                                                                                                                 
myBiomodEM_algoF1F_sp02<- BIOMOD_EnsembleModeling(bm.mod = myBiomodModelOut_sp02,
                                                  models.chosen = 'all',
                                                  em.by = 'all',
                                                  em.algo = c('EMmean'),
                                                  metric.select = c('TSS'),
                                                  metric.select.thresh = c(0.7),
                                                  metric.eval = c('TSS', 'ROC'),
                                                  var.import = 3,
                                                  EMci.alpha = 0.05,
                                                  EMwmean.decay = 'proportional')



# saving ensemble evaluations  FUTURE1E  

# Criando a pasta se ela não existir
if (!dir.exists(myRespName_sp02)) {
  dir.create(myRespName_sp02)
}

# Capturando a saída e salvando no arquivo
capture.output(myBiomodEM_algoF1F_sp02, 
               file = file.path(myRespName_sp02, 
                                paste(myRespName_sp02, "_EM_algoF1F_sp02.txt", sep = "")))

capture.output(get_evaluations(myBiomodEM_algoF1F_sp02),                                                                                                                 
               file = file.path(myRespName_sp02,                                                                                                                         
                                paste(myRespName_sp02, "_eval_EM_algoF1F_sp02.txt", sep = "")))                                                                               


#projecting ensemble  FUTURE1E    
#setwd("~/Desktop/crop_rasters_2")
myBiomodEMProj_EM_algoF1F_sp02 <- BIOMOD_EnsembleForecasting(bm.em = myBiomodEM_algoF1F_sp02,
                                                             proj.name = 'ensemble_Futuro1F',
                                                             new.env =myExpl_crop_1F_sp02,
                                                             models.chosen = 'all',
                                                             metric.binary = 'all',
                                                             metric.filter = 'all', 
                                                             output.format = '.img')

rm(list= ls()[(ls() %in% c("myExpl_crop_F1F_sp02", "myBiomodProj_F1F_sp02", 
                           "myBiomodEM_algoF1F_sp02", 
                           "myBiomodEMProj_EM_algoF1F_sp02"))])           



###MIROC6-2/ssp370
### FUTURE1G   

futuras1G_sp02<- stack(list.files(path=paste(Future_1G, sep=''), 
                                  pattern=".tif$", full.names=TRUE))                                                                  
#todas_futuras1G                                                                                                                                                    

names(futuras1G_sp02)
names(futuras1G_sp02) <- c('BIO_1_Theobroma_crop', 'BIO_2_Theobroma_crop', 
                           'BIO_3_Theobroma_crop','BIO_4_Theobroma_crop', 
                           'BIO_5_Theobroma_crop', 'BIO_6_Theobroma_crop',
                           'BIO_7_Theobroma_crop', 'BIO_8_Theobroma_crop', 
                           'BIO_9_Theobroma_crop','BIO_10_Theobroma_crop', 
                           'BIO_11_Theobroma_crop', 'BIO_12_Theobroma_crop',
                           'BIO_13_Theobroma_crop', 'BIO_14_Theobroma_crop', 
                           'BIO_15_Theobroma_crop',
                           'BIO_16_Theobroma_crop', 'BIO_17_Theobroma_crop', 
                           'BIO_18_Theobroma_crop','BIO_19_Theobroma_crop')  

myExpl_1G_sp02<-futuras1G_sp02[[grep("BIO_",                                                                                        
                                     paste(t(sel_vars_2)),                                                                                                                    
                                     value = T)]] 

#### criando background por convex hull + buffer ####
pts<-SpatialPoints(species_data_cupu[,c("Longitude", "Latitude")])
pres.bkg <- gBuffer(pts, width = 1)
bkg <- gBuffer(pres.bkg, width = 1, byid = T)

#### cortando as camadas climÃ¡ticas para os limites do background ####
#mudar AQUI
myExpl_crop_1G_sp02 <- crop(myExpl_1G_sp02, extent(bkg))

myExpl_crop_1G_sp02 <- stack(mask(myExpl_crop_1G_sp02, 
                                  intersect_mask (myExpl_crop_1G_sp02)))


plot(myExpl_crop_1G_sp02[[1]])
points(pts)
names (myExpl_crop_1G_sp02 [[1]])
writeRaster(myExpl_crop_1G_sp02, "~/Desktop/PDJ_UFPA/Clima_Sp02/future_F1G.grd", 
            format="raster", overwrite=TRUE)
#myExpl_F1c <- stack(mask(myExpl_F1c, intersect_mask(myExpl_F1c)))
#myExpl_F1c                                                                                                                                                         

rm(list= ls()[(ls() %in% c("futuras1G_sp02"))])



#projecting FUTURE1G                                                                                                                               
myBiomodProj_F1G_sp02 <- BIOMOD_Projection(bm.mod = myBiomodModelOut_sp02,                                                                                               
                                           new.env = myExpl_crop_1G_sp02, #myExpl_crop
                                           proj.name = 'Futuro1G',
                                           new.env.xy= NULL, 
                                           models.chosen = 'all',                                                                                                          
                                           metric.binary = 'TSS',                                                                                                              
                                           compress = F,                                                                                                                     
                                           build.clamping.mask = F, 
                                           output.format = '.img')


#ensemble  FUTURE1G                                                                                                                                                 
myBiomodEM_algoF1G_sp02<- BIOMOD_EnsembleModeling(bm.mod = myBiomodModelOut_sp02,
                                                  models.chosen = 'all',
                                                  em.by = 'all',
                                                  em.algo = c('EMmean'),
                                                  metric.select = c('TSS'),
                                                  metric.select.thresh = c(0.7),
                                                  metric.eval = c('TSS', 'ROC'),
                                                  var.import = 3,
                                                  EMci.alpha = 0.05,
                                                  EMwmean.decay = 'proportional')



# saving ensemble evaluations  FUTURE1G  

# Criando a pasta se ela não existir
if (!dir.exists(myRespName_sp02)) {
  dir.create(myRespName_sp02)
}

# Capturando a saída e salvando no arquivo
capture.output(myBiomodEM_algoF1G_sp02, 
               file = file.path(myRespName_sp02, 
                                paste(myRespName_sp02, "_EM_algoF1G_sp02.txt", sep = "")))

capture.output(get_evaluations(myBiomodEM_algoF1G_sp02),                                                                                                                 
               file = file.path(myRespName_sp02,                                                                                                                         
                                paste(myRespName_sp02, "_eval_EM_algoF1G_sp02.txt", sep = "")))                                                                               


#projecting ensemble  FUTURE1G    
#setwd("~/Desktop/crop_rasters_2")
myBiomodEMProj_EM_algoF1G_sp02 <- BIOMOD_EnsembleForecasting(bm.em = myBiomodEM_algoF1G_sp02,
                                                             proj.name = 'ensemble_Futuro1G',
                                                             new.env =myExpl_crop_1G_sp02,
                                                             models.chosen = 'all',
                                                             metric.binary = 'all',
                                                             metric.filter = 'all', 
                                                             output.format = '.img')

rm(list= ls()[(ls() %in% c("myExpl_crop_F1G_sp02", "myBiomodProj_F1G_sp02", 
                           "myBiomodEM_algoF1G_sp02", 
                           "myBiomodEMProj_EM_algoF1G_sp02"))])           


###MIROC6-2/ssp585
### FUTURE1H   

futuras1H_sp02<- stack(list.files(path=paste(Future_1H, sep=''), 
                                  pattern=".tif$", full.names=TRUE))                                                                  
#todas_futuras1G                                                                                                                                                    

names(futuras1H_sp02)
names(futuras1H_sp02) <- c('BIO_1_Theobroma_crop', 'BIO_2_Theobroma_crop', 
                           'BIO_3_Theobroma_crop','BIO_4_Theobroma_crop', 
                           'BIO_5_Theobroma_crop', 'BIO_6_Theobroma_crop',
                           'BIO_7_Theobroma_crop', 'BIO_8_Theobroma_crop', 
                           'BIO_9_Theobroma_crop','BIO_10_Theobroma_crop', 
                           'BIO_11_Theobroma_crop', 'BIO_12_Theobroma_crop',
                           'BIO_13_Theobroma_crop', 'BIO_14_Theobroma_crop', 
                           'BIO_15_Theobroma_crop',
                           'BIO_16_Theobroma_crop', 'BIO_17_Theobroma_crop', 
                           'BIO_18_Theobroma_crop','BIO_19_Theobroma_crop')  

myExpl_1H_sp02<-futuras1H_sp02[[grep("BIO_",                                                                                        
                                     paste(t(sel_vars_2)),                                                                                                                    
                                     value = T)]] 

#### criando background por convex hull + buffer ####
pts<-SpatialPoints(species_data_cupu[,c("Longitude", "Latitude")])
pres.bkg <- gBuffer(pts, width = 1)
bkg <- gBuffer(pres.bkg, width = 1, byid = T)

#### cortando as camadas climÃ¡ticas para os limites do background ####
#mudar AQUI
myExpl_crop_1H_sp02 <- crop(myExpl_1H_sp02, extent(bkg))

myExpl_crop_1H_sp02 <- stack(mask(myExpl_crop_1H_sp02, 
                                  intersect_mask (myExpl_crop_1H_sp02)))


plot(myExpl_crop_1H_sp02[[1]])
points(pts)
names (myExpl_crop_1H_sp02 [[1]])
writeRaster(myExpl_crop_1H_sp02, "~/Desktop/PDJ_UFPA/Clima_Sp02/future_F1H.grd", 
            format="raster", overwrite=TRUE)
#myExpl_F1c <- stack(mask(myExpl_F1c, intersect_mask(myExpl_F1c)))
#myExpl_F1c                                                                                                                                                         

rm(list= ls()[(ls() %in% c("futuras1H_sp02"))])



#projecting FUTURE1H                                                                                                                               
myBiomodProj_F1H_sp02 <- BIOMOD_Projection(bm.mod = myBiomodModelOut_sp02,                                                                                               
                                           new.env = myExpl_crop_1H_sp02, #myExpl_crop
                                           proj.name = 'Futuro1H',
                                           new.env.xy= NULL, 
                                           models.chosen = 'all',                                                                                                          
                                           metric.binary = 'TSS',                                                                                                              
                                           compress = F,                                                                                                                     
                                           build.clamping.mask = F, 
                                           output.format = '.img')


#ensemble  FUTURE1G                                                                                                                                                 
myBiomodEM_algoF1H_sp02<- BIOMOD_EnsembleModeling(bm.mod = myBiomodModelOut_sp02,
                                                  models.chosen = 'all',
                                                  em.by = 'all',
                                                  em.algo = c('EMmean'),
                                                  metric.select = c('TSS'),
                                                  metric.select.thresh = c(0.7),
                                                  metric.eval = c('TSS', 'ROC'),
                                                  var.import = 3,
                                                  EMci.alpha = 0.05,
                                                  EMwmean.decay = 'proportional')



# saving ensemble evaluations FUTURE1H 

# Criando a pasta se ela não existir
if (!dir.exists(myRespName_sp02)) {
  dir.create(myRespName_sp02)
}

# Capturando a saída e salvando no arquivo
capture.output(myBiomodEM_algoF1H_sp02, 
               file = file.path(myRespName_sp02, 
                                paste(myRespName_sp02, "_EM_algoF1H_sp02.txt", sep = "")))

capture.output(get_evaluations(myBiomodEM_algoF1H_sp02),                                                                                                                 
               file = file.path(myRespName_sp02,                                                                                                                         
                                paste(myRespName_sp02, "_eval_EM_algoF1H_sp02.txt", sep = "")))                                                                               


#projecting ensemble  FUTURE1H    
#setwd("~/Desktop/crop_rasters_2")
myBiomodEMProj_EM_algoF1H_sp02 <- BIOMOD_EnsembleForecasting(bm.em = myBiomodEM_algoF1H_sp02,
                                                             proj.name = 'ensemble_Futuro1H',
                                                             new.env =myExpl_crop_1H_sp02,
                                                             models.chosen = 'all',
                                                             metric.binary = 'all',
                                                             metric.filter = 'all', 
                                                             output.format = '.img')

rm(list= ls()[(ls() %in% c("myExpl_crop_F1H_sp02", "myBiomodProj_F1H_sp02", 
                           "myBiomodEM_algoF1H_sp02", 
                           "myBiomodEMProj_EM_algoF1H_sp02"))])           

################################################################################
################################MRI-ESM2-0
#modelos com o período de 2021-2040 

Future_2A <- "~/Desktop/PDJ_UFPA/Clima_Sp02/Futuro_2/MRI-ESM2-0.pt01/ssp126" 
Future_2B <- "~/Desktop/PDJ_UFPA/Clima_Sp02/Futuro_2/MRI-ESM2-0.pt01/ssp245" 
Future_2C <- "~/Desktop/PDJ_UFPA/Clima_Sp02/Futuro_2/MRI-ESM2-0.pt01/ssp370" 
Future_2D <- "~/Desktop/PDJ_UFPA/Clima_Sp02/Futuro_2/MRI-ESM2-0.pt01/ssp585"

###MRI-ESM2-0 - ssp126                                                                                                                                                       
futuras2A_sp02<- stack(list.files(path=paste(Future_2A, sep=''), 
                                  pattern=".tif$", full.names=TRUE))                                                     
#todas_futuras2A                                                                                                                                                   


names(futuras2A_sp02)
names(futuras2A_sp02) <- c('BIO_1_Theobroma_crop', 'BIO_2_Theobroma_crop', 
                            'BIO_3_Theobroma_crop','BIO_4_Theobroma_crop', 
                            'BIO_5_Theobroma_crop', 'BIO_6_Theobroma_crop',
                            'BIO_7_Theobroma_crop', 'BIO_8_Theobroma_crop', 
                            'BIO_9_Theobroma_crop','BIO_10_Theobroma_crop', 
                            'BIO_11_Theobroma_crop', 'BIO_12_Theobroma_crop',
                            'BIO_13_Theobroma_crop', 'BIO_14_Theobroma_crop', 
                            'BIO_15_Theobroma_crop',
                            'BIO_16_Theobroma_crop', 'BIO_17_Theobroma_crop', 
                            'BIO_18_Theobroma_crop','BIO_19_Theobroma_crop')  

myExpl_F2A_sp02<<-futuras2A_sp02[[grep("BIO_",                                                                                        
                                     paste(t(sel_vars_2)),                                                                                                                    
                                     value = T)]] 

#### criando background por convex hull + buffer ####
pts<-SpatialPoints(species_data_cupu[,c("Longitude", "Latitude")])
pres.bkg <- gBuffer(pts, width = 1)
bkg <- gBuffer(pres.bkg, width = 1, byid = T)

#### cortando as camadas climÃ¡ticas para os limites do background ####
#mudar AQUI
myExpl_crop_F2A_sp02 <- crop(myExpl_F2A_sp02, extent(bkg))

myExpl_crop_F2A_sp02 <- stack(mask(myExpl_crop_F2A_sp02, 
                                   intersect_mask (myExpl_crop_F2A_sp02)))

plot(myExpl_crop_F2A_sp02 [[1]])
points(pts)
names (myExpl_crop_F2A_sp02[[1]])

writeRaster(myExpl_crop_F2A_sp02, "~/Desktop/PDJ_UFPA/Clima_Sp02/future_F2A.grd", format="raster",overwrite=TRUE)
writeRaster(myExpl_crop_F2A_sp02[[1]], "~/Desktop/PDJ_UFPA/Clima_Sp02/BIO_1_Theobroma_future_crop_MRI-ESM2-0_ssp126.grd", format="raster")
writeRaster(myExpl_crop_F2A_sp02[[10]], "~/Desktop/PDJ_UFPA/Clima_Sp02/BIO_7__Theobroma_future_crop_MRI-ESM2-0_ssp126.grd", format="raster")
#myExpl_F1c <- stack(mask(myExpl_F1c, intersect_mask(myExpl_F1c)))

#myExpl_F1c                                                                                                                                                         

rm(list= ls()[(ls() %in% c("futuras2A_sp02"))])

#############
#projecting FUTURE2a                                                                                                                               
myBiomodProj_F2A_sp02<- BIOMOD_Projection (bm.mod = myBiomodModelOut_sp02,                                                                                               
                                          new.env = myExpl_crop_F2A_sp02, #myExpl_crop
                                          proj.name = 'Futuro2A',
                                          new.env.xy= NULL, 
                                          models.chosen = 'all',                                                                                                          
                                          metric.binary = 'TSS',                                                                                                              
                                          compress = F,                                                                                                                     
                                          build.clamping.mask = F, 
                                          output.format = '.img') 


#ensemble  FUTURE1G                                                                                                                                                 
myBiomodEM_algoF2A_sp02<- BIOMOD_EnsembleModeling(bm.mod = myBiomodModelOut_sp02,
                                                  models.chosen = 'all',
                                                  em.by = 'all',
                                                  em.algo = c('EMmean'),
                                                  metric.select = c('TSS'),
                                                  metric.select.thresh = c(0.7),
                                                  metric.eval = c('TSS', 'ROC'),
                                                  var.import = 3,
                                                  EMci.alpha = 0.05,
                                                  EMwmean.decay = 'proportional')



# saving ensemble evaluations FUTURE1H 

# Criando a pasta se ela não existir
if (!dir.exists(myRespName_sp02)) {
  dir.create(myRespName_sp02)
}

# Capturando a saída e salvando no arquivo
capture.output(myBiomodEM_algoF2A_sp02, 
               file = file.path(myRespName_sp02, 
                                paste(myRespName_sp02, "_EM_algoF2A_sp02.txt", sep = "")))

capture.output(get_evaluations(myBiomodEM_algoF2A_sp02),                                                                                                                 
               file = file.path(myRespName_sp02,                                                                                                                         
                                paste(myRespName_sp02, "_eval_EM_algoF2A_sp02.txt", sep = "")))                                                                               


#projecting ensemble  FUTURE1H    
#setwd("~/Desktop/crop_rasters_2")
myBiomodEMProj_EM_algoF2A_sp02 <- BIOMOD_EnsembleForecasting(bm.em = myBiomodEM_algoF2A_sp02,
                                                             proj.name = 'ensemble_Futuro2A',
                                                             new.env =myExpl_crop_F2A_sp02,
                                                             models.chosen = 'all',
                                                             metric.binary = 'all',
                                                             metric.filter = 'all', 
                                                             output.format = '.img')

rm(list= ls()[(ls() %in% c("myExpl_crop_F2A_sp02", "myBiomodProj_F2A_sp02", 
                           "myBiomodEM_algoF2A_sp02", 
                           "myBiomodEMProj_EM_algoF2A_sp02"))])           
                
#################
### FUTURE2B                                                                                                                                                        
futuras2B_sp02<- stack(list.files(path=paste(Future_2B, sep=''), pattern=".tif$", full.names=TRUE))                                                                  
#todas_futuras3B                                                                                                                                                   

names(futuras2B_sp02)
names(futuras2B_sp02) <- c('BIO_1_Theobroma_crop', 'BIO_2_Theobroma_crop', 
                           'BIO_3_Theobroma_crop','BIO_4_Theobroma_crop', 
                           'BIO_5_Theobroma_crop', 'BIO_6_Theobroma_crop',
                           'BIO_7_Theobroma_crop', 'BIO_8_Theobroma_crop', 
                           'BIO_9_Theobroma_crop','BIO_10_Theobroma_crop', 
                           'BIO_11_Theobroma_crop', 'BIO_12_Theobroma_crop',
                           'BIO_13_Theobroma_crop', 'BIO_14_Theobroma_crop', 
                           'BIO_15_Theobroma_crop',
                           'BIO_16_Theobroma_crop', 'BIO_17_Theobroma_crop', 
                           'BIO_18_Theobroma_crop','BIO_19_Theobroma_crop')  

myExpl_F2B_sp02<<-futuras2B_sp02[[grep("BIO_",                                                                                        
                                       paste(t(sel_vars_2)),                                                                                                                    
                                       value = T)]] 

#### criando background por convex hull + buffer ####
pts<-SpatialPoints(species_data_cupu[,c("Longitude", "Latitude")])
pres.bkg <- gBuffer(pts, width = 1)
bkg <- gBuffer(pres.bkg, width = 1, byid = T)

#### cortando as camadas climÃ¡ticas para os limites do background ####
#mudar AQUI
myExpl_crop_F2B_sp02 <- crop(myExpl_F2B_sp02, extent(bkg))

myExpl_crop_F2B_sp02 <- stack(mask(myExpl_crop_F2B_sp02, 
                                   intersect_mask (myExpl_crop_F2B_sp02)))

plot(myExpl_crop_F2B_sp02 [[1]])
points(pts)
names (myExpl_crop_F2B_sp02 [[1]])
writeRaster(myExpl_crop_F2B_sp02, "~/Desktop/PDJ_UFPA/Clima_Sp02/future_F2B.grd", format="raster",overwrite=TRUE)
#myExpl_F1c <- stack(mask(myExpl_F1c, intersect_mask(myExpl_F1c)))

#myExpl_F1c                                                                                                                                                         

rm(list= ls()[(ls() %in% c("futuras3B_fev"))])  

#projecting FUTURE2B                                                                                                                              
myBiomodProj_F2B_sp02<- BIOMOD_Projection (bm.mod = myBiomodModelOut_sp02,                                                                                               
                                           new.env = myExpl_crop_F2B_sp02, #myExpl_crop
                                           proj.name = 'Futuro2B',
                                           new.env.xy= NULL, 
                                           models.chosen = 'all',                                                                                                          
                                           metric.binary = 'TSS',                                                                                                              
                                           compress = F,                                                                                                                     
                                           build.clamping.mask = F, 
                                           output.format = '.img') 


#ensemble  FUTURE1G                                                                                                                                                 
myBiomodEM_algoF2B_sp02<- BIOMOD_EnsembleModeling(bm.mod = myBiomodModelOut_sp02,
                                                  models.chosen = 'all',
                                                  em.by = 'all',
                                                  em.algo = c('EMmean'),
                                                  metric.select = c('TSS'),
                                                  metric.select.thresh = c(0.7),
                                                  metric.eval = c('TSS', 'ROC'),
                                                  var.import = 3,
                                                  EMci.alpha = 0.05,
                                                  EMwmean.decay = 'proportional')



# saving ensemble evaluations FUTURE2b

# Criando a pasta se ela não existir
if (!dir.exists(myRespName_sp02)) {
  dir.create(myRespName_sp02)
}

# Capturando a saída e salvando no arquivo
capture.output(myBiomodEM_algoF2B_sp02, 
               file = file.path(myRespName_sp02, 
                                paste(myRespName_sp02, "_EM_algoF2B_sp02.txt", sep = "")))

capture.output(get_evaluations(myBiomodEM_algoF2B_sp02),                                                                                                                 
               file = file.path(myRespName_sp02,                                                                                                                         
                                paste(myRespName_sp02, "_eval_EM_algoF2B_sp02.txt", sep = "")))                                                                               


#projecting ensemble  FUTURE1H    
#setwd("~/Desktop/crop_rasters_2")
myBiomodEMProj_EM_algoF2B_sp02 <- BIOMOD_EnsembleForecasting(bm.em = myBiomodEM_algoF2B_sp02,
                                                             proj.name = 'ensemble_Futuro2B',
                                                             new.env =myExpl_crop_F2B_sp02,
                                                             models.chosen = 'all',
                                                             metric.binary = 'all',
                                                             metric.filter = 'all', 
                                                             output.format = '.img')

rm(list= ls()[(ls() %in% c("myExpl_crop_F2B_sp02", "myBiomodProj_F2B_sp02", 
                           "myBiomodEM_algoF2B_sp02", 
                           "myBiomodEMProj_EM_algoF2B_sp02"))])           


#################
### FUTURE2C                                                                                                                                                        
futuras2C_sp02<- stack(list.files(path=paste(Future_2C, sep=''), pattern=".tif$", full.names=TRUE))                                                                  
#todas_futuras3B                                                                                                                                                   

names(futuras2C_sp02)
names(futuras2C_sp02) <- c('BIO_1_Theobroma_crop', 'BIO_2_Theobroma_crop', 
                           'BIO_3_Theobroma_crop','BIO_4_Theobroma_crop', 
                           'BIO_5_Theobroma_crop', 'BIO_6_Theobroma_crop',
                           'BIO_7_Theobroma_crop', 'BIO_8_Theobroma_crop', 
                           'BIO_9_Theobroma_crop','BIO_10_Theobroma_crop', 
                           'BIO_11_Theobroma_crop', 'BIO_12_Theobroma_crop',
                           'BIO_13_Theobroma_crop', 'BIO_14_Theobroma_crop', 
                           'BIO_15_Theobroma_crop',
                           'BIO_16_Theobroma_crop', 'BIO_17_Theobroma_crop', 
                           'BIO_18_Theobroma_crop','BIO_19_Theobroma_crop')  

myExpl_F2C_sp02<<-futuras2C_sp02[[grep("BIO_",                                                                                        
                                       paste(t(sel_vars_2)),                                                                                                                    
                                       value = T)]] 

#### criando background por convex hull + buffer ####
pts<-SpatialPoints(species_data_cupu[,c("Longitude", "Latitude")])
pres.bkg <- gBuffer(pts, width = 1)
bkg <- gBuffer(pres.bkg, width = 1, byid = T)

#### cortando as camadas climÃ¡ticas para os limites do background ####
#mudar AQUI
myExpl_crop_F2C_sp02 <- crop(myExpl_F2C_sp02, extent(bkg))

myExpl_crop_F2C_sp02 <- stack(mask(myExpl_crop_F2C_sp02, 
                                   intersect_mask (myExpl_crop_F2C_sp02)))

plot(myExpl_crop_F2C_sp02 [[1]])
points(pts)
names (myExpl_crop_F2C_sp02 [[1]])
writeRaster(myExpl_crop_F2C_sp02, "~/Desktop/PDJ_UFPA/Clima_Sp02/future_F2C.grd", format="raster",overwrite=TRUE)
#myExpl_F1c <- stack(mask(myExpl_F1c, intersect_mask(myExpl_F1c)))

#myExpl_F1c                                                                                                                                                         

rm(list= ls()[(ls() %in% c("futuras2C_sp02"))])  

#projecting FUTURE2c                                                                                                                              
myBiomodProj_F2C_sp02<- BIOMOD_Projection (bm.mod = myBiomodModelOut_sp02,                                                                                               
                                           new.env = myExpl_crop_F2C_sp02, #myExpl_crop
                                           proj.name = 'Futuro2C',
                                           new.env.xy= NULL, 
                                           models.chosen = 'all',                                                                                                          
                                           metric.binary = 'TSS',                                                                                                              
                                           compress = F,                                                                                                                     
                                           build.clamping.mask = F, 
                                           output.format = '.img') 


#ensemble  FUTURE1G                                                                                                                                                 
myBiomodEM_algoF2C_sp02<- BIOMOD_EnsembleModeling(bm.mod = myBiomodModelOut_sp02,
                                                  models.chosen = 'all',
                                                  em.by = 'all',
                                                  em.algo = c('EMmean'),
                                                  metric.select = c('TSS'),
                                                  metric.select.thresh = c(0.7),
                                                  metric.eval = c('TSS', 'ROC'),
                                                  var.import = 3,
                                                  EMci.alpha = 0.05,
                                                  EMwmean.decay = 'proportional')



# saving ensemble evaluations FUTURE2c

# Criando a pasta se ela não existir
if (!dir.exists(myRespName_sp02)) {
  dir.create(myRespName_sp02)
}

# Capturando a saída e salvando no arquivo
capture.output(myBiomodEM_algoF2C_sp02, 
               file = file.path(myRespName_sp02, 
                                paste(myRespName_sp02, "_EM_algoF2C_sp02.txt", sep = "")))

capture.output(get_evaluations(myBiomodEM_algoF2C_sp02),                                                                                                                 
               file = file.path(myRespName_sp02,                                                                                                                         
                                paste(myRespName_sp02, "_eval_EM_algoF2C_sp02.txt", sep = "")))                                                                               


#projecting ensemble  FUTURE2c    
#setwd("~/Desktop/crop_rasters_2")
myBiomodEMProj_EM_algoF2C_sp02 <- BIOMOD_EnsembleForecasting(bm.em = myBiomodEM_algoF2C_sp02,
                                                             proj.name = 'ensemble_Futuro2C',
                                                             new.env =myExpl_crop_F2C_sp02,
                                                             models.chosen = 'all',
                                                             metric.binary = 'all',
                                                             metric.filter = 'all', 
                                                             output.format = '.img')

rm(list= ls()[(ls() %in% c("myExpl_crop_F2C_sp02", "myBiomodProj_F2C_sp02", 
                           "myBiomodEM_algoF2C_sp02", 
                           "myBiomodEMProj_EM_algoF2C_sp02"))])           


### FUTURE2D                                                                                                                                                      
futuras2D_sp02<- stack(list.files(path=paste(Future_2D, sep=''), pattern=".tif$", full.names=TRUE))                                                                  
#todas_futuras2D                                                                                                                                                   

names(futuras2D_sp02)
names(futuras2D_sp02) <- c('BIO_1_Theobroma_crop', 'BIO_2_Theobroma_crop', 
                           'BIO_3_Theobroma_crop','BIO_4_Theobroma_crop', 
                           'BIO_5_Theobroma_crop', 'BIO_6_Theobroma_crop',
                           'BIO_7_Theobroma_crop', 'BIO_8_Theobroma_crop', 
                           'BIO_9_Theobroma_crop','BIO_10_Theobroma_crop', 
                           'BIO_11_Theobroma_crop', 'BIO_12_Theobroma_crop',
                           'BIO_13_Theobroma_crop', 'BIO_14_Theobroma_crop', 
                           'BIO_15_Theobroma_crop',
                           'BIO_16_Theobroma_crop', 'BIO_17_Theobroma_crop', 
                           'BIO_18_Theobroma_crop','BIO_19_Theobroma_crop')  

myExpl_F2D_sp02<<-futuras2D_sp02[[grep("BIO_",                                                                                        
                                       paste(t(sel_vars_2)),                                                                                                                    
                                       value = T)]] 

#### criando background por convex hull + buffer ####
pts<-SpatialPoints(species_data_cupu[,c("Longitude", "Latitude")])
pres.bkg <- gBuffer(pts, width = 1)
bkg <- gBuffer(pres.bkg, width = 1, byid = T)

#### cortando as camadas climÃ¡ticas para os limites do background ####
#mudar AQUI
myExpl_crop_F2D_sp02 <- crop(myExpl_F2D_sp02, extent(bkg))

myExpl_crop_F2D_sp02 <- stack(mask(myExpl_crop_F2D_sp02, 
                                   intersect_mask (myExpl_crop_F2D_sp02)))

plot(myExpl_crop_F2D_sp02 [[1]])
points(pts)
names (myExpl_crop_F2D_sp02 [[1]])
writeRaster(myExpl_crop_F2D_sp02, "~/Desktop/PDJ_UFPA/Clima_Sp02/future_F2D.grd", format="raster",overwrite=TRUE)
writeRaster(myExpl_crop_F2D_sp02[[1]], "~/Desktop/PDJ_UFPA/Clima_Sp02/BIO_1_Theobroma_future_crop_MRI-ESM2-0_ssp585.grd", format="raster")
writeRaster(myExpl_crop_F2D_sp02[[10]], "~/Desktop/PDJ_UFPA/Clima_Sp02/BIO_7__Theobroma_future_crop_MRI-ESM2-0_ssp585.grd", format="raster")
#myExpl_F1c <- stack(mask(myExpl_F1c, intersect_mask(myExpl_F1c)))

#myExpl_F1c                                                                                                                                                         

rm(list= ls()[(ls() %in% c("futuras2D_sp02"))])  

#projecting FUTURE2D                                                                                                                              
myBiomodProj_F2D_sp02<- BIOMOD_Projection (bm.mod = myBiomodModelOut_sp02,                                                                                               
                                           new.env = myExpl_crop_F2D_sp02, #myExpl_crop
                                           proj.name = 'Futuro2D',
                                           new.env.xy= NULL, 
                                           models.chosen = 'all',                                                                                                          
                                           metric.binary = 'TSS',                                                                                                              
                                           compress = F,                                                                                                                     
                                           build.clamping.mask = F, 
                                           output.format = '.img') 


#ensemble  FUTURE2D                                                                                                                                                 
myBiomodEM_algoF2D_sp02<- BIOMOD_EnsembleModeling(bm.mod = myBiomodModelOut_sp02,
                                                  models.chosen = 'all',
                                                  em.by = 'all',
                                                  em.algo = c('EMmean'),
                                                  metric.select = c('TSS'),
                                                  metric.select.thresh = c(0.7),
                                                  metric.eval = c('TSS', 'ROC'),
                                                  var.import = 3,
                                                  EMci.alpha = 0.05,
                                                  EMwmean.decay = 'proportional')



# saving ensemble evaluations FUTURE2d 

# Criando a pasta se ela não existir
if (!dir.exists(myRespName_sp02)) {
  dir.create(myRespName_sp02)
}

# Capturando a saída e salvando no arquivo
capture.output(myBiomodEM_algoF2D_sp02, 
               file = file.path(myRespName_sp02, 
                                paste(myRespName_sp02, "_EM_algoF2D_sp02.txt", sep = "")))

capture.output(get_evaluations(myBiomodEM_algoF2D_sp02),                                                                                                                 
               file = file.path(myRespName_sp02,                                                                                                                         
                                paste(myRespName_sp02, "_eval_EM_algoF2D_sp02.txt", sep = "")))                                                                               


#projecting ensemble  FUTURE2D    
#setwd("~/Desktop/crop_rasters_2")
myBiomodEMProj_EM_algoF2D_sp02 <- BIOMOD_EnsembleForecasting(bm.em = myBiomodEM_algoF2D_sp02,
                                                             proj.name = 'ensemble_Futuro2D',
                                                             new.env =myExpl_crop_F2D_sp02,
                                                             models.chosen = 'all',
                                                             metric.binary = 'all',
                                                             metric.filter = 'all', 
                                                             output.format = '.img')

rm(list= ls()[(ls() %in% c("myExpl_crop_F2D_sp02", "myBiomodProj_F2D_sp02", 
                           "myBiomodEM_algoF2D_sp02", 
                           "myBiomodEMProj_EM_algoF2D_sp02"))])           


###################################
#########################MRI-ESM2-0 (2041-2060)
Future_2E <- "~/Desktop/PDJ_UFPA/Clima_Sp02/Futuro_2/MRI-ESM2-0.pt02/ssp126" 
Future_2F <- "~/Desktop/PDJ_UFPA/Clima_Sp02/Futuro_2/MRI-ESM2-0.pt02/ssp245" 
Future_2G <- "~/Desktop/PDJ_UFPA/Clima_Sp02/Futuro_2/MRI-ESM2-0.pt02/ssp370" 
Future_2H <- "~/Desktop/PDJ_UFPA/Clima_Sp02/Futuro_2/MRI-ESM2-0.pt02/ssp585"



### FUTURE2E                                                                                                                                                      
futuras2E_sp02<- stack(list.files(path=paste(Future_2E, sep=''), pattern=".tif$", full.names=TRUE))                                                                  
#todas_futuras3B                                                                                                                                                   

names(futuras2E_sp02)
names(futuras2E_sp02) <- c('BIO_1_Theobroma_crop', 'BIO_2_Theobroma_crop', 
                           'BIO_3_Theobroma_crop','BIO_4_Theobroma_crop', 
                           'BIO_5_Theobroma_crop', 'BIO_6_Theobroma_crop',
                           'BIO_7_Theobroma_crop', 'BIO_8_Theobroma_crop', 
                           'BIO_9_Theobroma_crop','BIO_10_Theobroma_crop', 
                           'BIO_11_Theobroma_crop', 'BIO_12_Theobroma_crop',
                           'BIO_13_Theobroma_crop', 'BIO_14_Theobroma_crop', 
                           'BIO_15_Theobroma_crop',
                           'BIO_16_Theobroma_crop', 'BIO_17_Theobroma_crop', 
                           'BIO_18_Theobroma_crop','BIO_19_Theobroma_crop')  

myExpl_F2E_sp02<<-futuras2E_sp02[[grep("BIO_",                                                                                        
                                       paste(t(sel_vars_2)),                                                                                                                    
                                       value = T)]] 

#### criando background por convex hull + buffer ####
pts<-SpatialPoints(species_data_cupu[,c("Longitude", "Latitude")])
pres.bkg <- gBuffer(pts, width = 1)
bkg <- gBuffer(pres.bkg, width = 1, byid = T)

#### cortando as camadas climÃ¡ticas para os limites do background ####
#mudar AQUI
myExpl_crop_F2E_sp02 <- crop(myExpl_F2E_sp02, extent(bkg))

myExpl_crop_F2E_sp02 <- stack(mask(myExpl_crop_F2E_sp02, 
                                   intersect_mask (myExpl_crop_F2E_sp02)))

plot(myExpl_crop_F2E_sp02 [[1]])
points(pts)
names (myExpl_crop_F2E_sp02 [[1]])
writeRaster(myExpl_crop_F2E_sp02, "~/Desktop/PDJ_UFPA/Clima_Sp02/future_F2E.grd", format="raster",overwrite=TRUE)
#myExpl_F1c <- stack(mask(myExpl_F1c, intersect_mask(myExpl_F1c)))

#myExpl_F1c                                                                                                                                                         

rm(list= ls()[(ls() %in% c("futuras2E_sp02"))])  

#projecting FUTURE3B                                                                                                                              
myBiomodProj_F2E_sp02<- BIOMOD_Projection (bm.mod = myBiomodModelOut_sp02,                                                                                               
                                           new.env = myExpl_crop_F2E_sp02, #myExpl_crop
                                           proj.name = 'Futuro2E',
                                           new.env.xy= NULL, 
                                           models.chosen = 'all',                                                                                                          
                                           metric.binary = 'TSS',                                                                                                              
                                           compress = F,                                                                                                                     
                                           build.clamping.mask = F, 
                                           output.format = '.img') 


#ensemble  FUTURE1G                                                                                                                                                 
myBiomodEM_algoF2E_sp02<- BIOMOD_EnsembleModeling(bm.mod = myBiomodModelOut_sp02,
                                                  models.chosen = 'all',
                                                  em.by = 'all',
                                                  em.algo = c('EMmean'),
                                                  metric.select = c('TSS'),
                                                  metric.select.thresh = c(0.7),
                                                  metric.eval = c('TSS', 'ROC'),
                                                  var.import = 3,
                                                  EMci.alpha = 0.05,
                                                  EMwmean.decay = 'proportional')



# saving ensemble evaluations FUTURE1H 

# Criando a pasta se ela não existir
if (!dir.exists(myRespName_sp02)) {
  dir.create(myRespName_sp02)
}

# Capturando a saída e salvando no arquivo
capture.output(myBiomodEM_algoF2E_sp02, 
               file = file.path(myRespName_sp02, 
                                paste(myRespName_sp02, "_EM_algoF2E_sp02.txt", sep = "")))

capture.output(get_evaluations(myBiomodEM_algoF2E_sp02),                                                                                                                 
               file = file.path(myRespName_sp02,                                                                                                                         
                                paste(myRespName_sp02, "_eval_EM_algoF2E_sp02.txt", sep = "")))                                                                               


#projecting ensemble  FUTURE1H    
#setwd("~/Desktop/crop_rasters_2")
myBiomodEMProj_EM_algoF2E_sp02 <- BIOMOD_EnsembleForecasting(bm.em = myBiomodEM_algoF2E_sp02,
                                                             proj.name = 'ensemble_Futuro2E',
                                                             new.env =myExpl_crop_F2E_sp02,
                                                             models.chosen = 'all',
                                                             metric.binary = 'all',
                                                             metric.filter = 'all', 
                                                             output.format = '.img')

rm(list= ls()[(ls() %in% c("myExpl_crop_F2E_sp02", "myBiomodProj_F2E_sp02", 
                           "myBiomodEM_algoF2E_sp02", 
                           "myBiomodEMProj_EM_algoF2E_sp02"))])           


###################
### FUTURE2F                                                                                                                                                      
futuras2F_sp02<- stack(list.files(path=paste(Future_2F, sep=''), pattern=".tif$", full.names=TRUE))                                                                  
#todas_futuras3B                                                                                                                                                   

names(futuras2F_sp02)
names(futuras2F_sp02) <- c('BIO_1_Theobroma_crop', 'BIO_2_Theobroma_crop', 
                           'BIO_3_Theobroma_crop','BIO_4_Theobroma_crop', 
                           'BIO_5_Theobroma_crop', 'BIO_6_Theobroma_crop',
                           'BIO_7_Theobroma_crop', 'BIO_8_Theobroma_crop', 
                           'BIO_9_Theobroma_crop','BIO_10_Theobroma_crop', 
                           'BIO_11_Theobroma_crop', 'BIO_12_Theobroma_crop',
                           'BIO_13_Theobroma_crop', 'BIO_14_Theobroma_crop', 
                           'BIO_15_Theobroma_crop',
                           'BIO_16_Theobroma_crop', 'BIO_17_Theobroma_crop', 
                           'BIO_18_Theobroma_crop','BIO_19_Theobroma_crop')  

myExpl_F2F_sp02<<-futuras2F_sp02[[grep("BIO_",                                                                                        
                                       paste(t(sel_vars_2)),                                                                                                                    
                                       value = T)]] 

#### criando background por convex hull + buffer ####
pts<-SpatialPoints(species_data_cupu[,c("Longitude", "Latitude")])
pres.bkg <- gBuffer(pts, width = 1)
bkg <- gBuffer(pres.bkg, width = 1, byid = T)

#### cortando as camadas climÃ¡ticas para os limites do background ####
#mudar AQUI
myExpl_crop_F2F_sp02 <- crop(myExpl_F2F_sp02, extent(bkg))

myExpl_crop_F2F_sp02 <- stack(mask(myExpl_crop_F2F_sp02, 
                                   intersect_mask (myExpl_crop_F2F_sp02)))

plot(myExpl_crop_F2F_sp02 [[1]])
points(pts)
names (myExpl_crop_F2F_sp02 [[1]])
writeRaster(myExpl_crop_F2F_sp02, "~/Desktop/PDJ_UFPA/Clima_Sp02/future_F2F.grd", format="raster",overwrite=TRUE)
#myExpl_F1c <- stack(mask(myExpl_F1c, intersect_mask(myExpl_F1c)))

#myExpl_F1c                                                                                                                                                         

rm(list= ls()[(ls() %in% c("futuras2F_sp02"))])  

#projecting FUTURE3B                                                                                                                              
myBiomodProj_F2F_sp02<- BIOMOD_Projection (bm.mod = myBiomodModelOut_sp02,                                                                                               
                                           new.env = myExpl_crop_F2F_sp02, #myExpl_crop
                                           proj.name = 'Futuro2F',
                                           new.env.xy= NULL, 
                                           models.chosen = 'all',                                                                                                          
                                           metric.binary = 'TSS',                                                                                                              
                                           compress = F,                                                                                                                     
                                           build.clamping.mask = F, 
                                           output.format = '.img') 


#ensemble  FUTURE1G                                                                                                                                                 
myBiomodEM_algoF2F_sp02<- BIOMOD_EnsembleModeling(bm.mod = myBiomodModelOut_sp02,
                                                  models.chosen = 'all',
                                                  em.by = 'all',
                                                  em.algo = c('EMmean'),
                                                  metric.select = c('TSS'),
                                                  metric.select.thresh = c(0.7),
                                                  metric.eval = c('TSS', 'ROC'),
                                                  var.import = 3,
                                                  EMci.alpha = 0.05,
                                                  EMwmean.decay = 'proportional')



# saving ensemble evaluations FUTURE1H 

# Criando a pasta se ela não existir
if (!dir.exists(myRespName_sp02)) {
  dir.create(myRespName_sp02)
}

# Capturando a saída e salvando no arquivo
capture.output(myBiomodEM_algoF2F_sp02, 
               file = file.path(myRespName_sp02, 
                                paste(myRespName_sp02, "_EM_algoF2F_sp02.txt", sep = "")))

capture.output(get_evaluations(myBiomodEM_algoF2F_sp02),                                                                                                                 
               file = file.path(myRespName_sp02,                                                                                                                         
                                paste(myRespName_sp02, "_eval_EM_algoF2F_sp02.txt", sep = "")))                                                                               


#projecting ensemble  FUTURE1H    
#setwd("~/Desktop/crop_rasters_2")
myBiomodEMProj_EM_algoF2F_sp02 <- BIOMOD_EnsembleForecasting(bm.em = myBiomodEM_algoF2F_sp02,
                                                             proj.name = 'ensemble_Futuro2F',
                                                             new.env =myExpl_crop_F2F_sp02,
                                                             models.chosen = 'all',
                                                             metric.binary = 'all',
                                                             metric.filter = 'all', 
                                                             output.format = '.img')

rm(list= ls()[(ls() %in% c("myExpl_crop_F2F_sp02", "myBiomodProj_F2F_sp02", 
                           "myBiomodEM_algoF2F_sp02", 
                           "myBiomodEMProj_EM_algoF2F_sp02"))])           


######################################

futuras2G_sp02<- stack(list.files(path=paste(Future_2G, sep=''), pattern=".tif$", full.names=TRUE))                                                                  
#todas_futuras3B                                                                                                                                                   

names(futuras2G_sp02)
names(futuras2G_sp02) <- c('BIO_1_Theobroma_crop', 'BIO_2_Theobroma_crop', 
                           'BIO_3_Theobroma_crop','BIO_4_Theobroma_crop', 
                           'BIO_5_Theobroma_crop', 'BIO_6_Theobroma_crop',
                           'BIO_7_Theobroma_crop', 'BIO_8_Theobroma_crop', 
                           'BIO_9_Theobroma_crop','BIO_10_Theobroma_crop', 
                           'BIO_11_Theobroma_crop', 'BIO_12_Theobroma_crop',
                           'BIO_13_Theobroma_crop', 'BIO_14_Theobroma_crop', 
                           'BIO_15_Theobroma_crop',
                           'BIO_16_Theobroma_crop', 'BIO_17_Theobroma_crop', 
                           'BIO_18_Theobroma_crop','BIO_19_Theobroma_crop')  

myExpl_F2G_sp02<<-futuras2G_sp02[[grep("BIO_",                                                                                        
                                       paste(t(sel_vars_2)),                                                                                                                    
                                       value = T)]] 

#### criando background por convex hull + buffer ####
pts<-SpatialPoints(species_data_cupu[,c("Longitude", "Latitude")])
pres.bkg <- gBuffer(pts, width = 1)
bkg <- gBuffer(pres.bkg, width = 1, byid = T)

#### cortando as camadas climÃ¡ticas para os limites do background ####
#mudar AQUI
myExpl_crop_F2G_sp02 <- crop(myExpl_F2G_sp02, extent(bkg))

myExpl_crop_F2G_sp02 <- stack(mask(myExpl_crop_F2G_sp02, 
                                   intersect_mask (myExpl_crop_F2G_sp02)))

plot(myExpl_crop_F2G_sp02 [[1]])
points(pts)
names (myExpl_crop_F2G_sp02 [[1]])
writeRaster(myExpl_crop_F2G_sp02, "~/Desktop/PDJ_UFPA/Clima_Sp02/future_F2G.grd", format="raster",overwrite=TRUE)
#myExpl_F1c <- stack(mask(myExpl_F1c, intersect_mask(myExpl_F1c)))

#myExpl_F1c                                                                                                                                                         

rm(list= ls()[(ls() %in% c("futuras2F_sp02"))])  

#projecting FUTURE3B                                                                                                                              
myBiomodProj_F2G_sp02<- BIOMOD_Projection (bm.mod = myBiomodModelOut_sp02,                                                                                               
                                           new.env = myExpl_crop_F2G_sp02, #myExpl_crop
                                           proj.name = 'Futuro2G',
                                           new.env.xy= NULL, 
                                           models.chosen = 'all',                                                                                                          
                                           metric.binary = 'TSS',                                                                                                              
                                           compress = F,                                                                                                                     
                                           build.clamping.mask = F, 
                                           output.format = '.img') 


#ensemble  FUTURE1G                                                                                                                                                 
myBiomodEM_algoF2G_sp02<- BIOMOD_EnsembleModeling(bm.mod = myBiomodModelOut_sp02,
                                                  models.chosen = 'all',
                                                  em.by = 'all',
                                                  em.algo = c('EMmean'),
                                                  metric.select = c('TSS'),
                                                  metric.select.thresh = c(0.7),
                                                  metric.eval = c('TSS', 'ROC'),
                                                  var.import = 3,
                                                  EMci.alpha = 0.05,
                                                  EMwmean.decay = 'proportional')



# saving ensemble evaluations FUTURE1H 

# Criando a pasta se ela não existir
if (!dir.exists(myRespName_sp02)) {
  dir.create(myRespName_sp02)
}

# Capturando a saída e salvando no arquivo
capture.output(myBiomodEM_algoF2G_sp02, 
               file = file.path(myRespName_sp02, 
                                paste(myRespName_sp02, "_EM_algoF2G_sp02.txt", sep = "")))

capture.output(get_evaluations(myBiomodEM_algoF2G_sp02),                                                                                                                 
               file = file.path(myRespName_sp02,                                                                                                                         
                                paste(myRespName_sp02, "_eval_EM_algoF2G_sp02.txt", sep = "")))                                                                               


#projecting ensemble  FUTURE1H    
#setwd("~/Desktop/crop_rasters_2")
myBiomodEMProj_EM_algoF2G_sp02 <- BIOMOD_EnsembleForecasting(bm.em = myBiomodEM_algoF2G_sp02,
                                                             proj.name = 'ensemble_Futuro2G',
                                                             new.env =myExpl_crop_F2G_sp02,
                                                             models.chosen = 'all',
                                                             metric.binary = 'all',
                                                             metric.filter = 'all', 
                                                             output.format = '.img')

rm(list= ls()[(ls() %in% c("myExpl_crop_F2G_sp02", "myBiomodProj_F2G_sp02", 
                           "myBiomodEM_algoF2G_sp02", 
                           "myBiomodEMProj_EM_algoF2G_sp02"))])  

##########################2H

futuras2H_sp02<- stack(list.files(path=paste(Future_2H, sep=''), pattern=".tif$", full.names=TRUE))                                                                  
#todas_futuras3B                                                                                                                                                   

names(futuras2H_sp02)
names(futuras2H_sp02) <- c('BIO_1_Theobroma_crop', 'BIO_2_Theobroma_crop', 
                           'BIO_3_Theobroma_crop','BIO_4_Theobroma_crop', 
                           'BIO_5_Theobroma_crop', 'BIO_6_Theobroma_crop',
                           'BIO_7_Theobroma_crop', 'BIO_8_Theobroma_crop', 
                           'BIO_9_Theobroma_crop','BIO_10_Theobroma_crop', 
                           'BIO_11_Theobroma_crop', 'BIO_12_Theobroma_crop',
                           'BIO_13_Theobroma_crop', 'BIO_14_Theobroma_crop', 
                           'BIO_15_Theobroma_crop',
                           'BIO_16_Theobroma_crop', 'BIO_17_Theobroma_crop', 
                           'BIO_18_Theobroma_crop','BIO_19_Theobroma_crop')  

myExpl_F2H_sp02<<-futuras2H_sp02[[grep("BIO_",                                                                                        
                                       paste(t(sel_vars_2)),                                                                                                                    
                                       value = T)]] 

#### criando background por convex hull + buffer ####
pts<-SpatialPoints(species_data_cupu[,c("Longitude", "Latitude")])
pres.bkg <- gBuffer(pts, width = 1)
bkg <- gBuffer(pres.bkg, width = 1, byid = T)

#### cortando as camadas climÃ¡ticas para os limites do background ####
#mudar AQUI
myExpl_crop_F2H_sp02 <- crop(myExpl_F2H_sp02, extent(bkg))

myExpl_crop_F2H_sp02 <- stack(mask(myExpl_crop_F2H_sp02, 
                                   intersect_mask (myExpl_crop_F2H_sp02)))

plot(myExpl_crop_F2H_sp02 [[1]])
points(pts)
names (myExpl_crop_F2H_sp02 [[1]])
writeRaster(myExpl_crop_F2H_sp02, "~/Desktop/PDJ_UFPA/Clima_Sp02/future_F2H.grd", format="raster",overwrite=TRUE)
#myExpl_F1c <- stack(mask(myExpl_F1c, intersect_mask(myExpl_F1c)))

#myExpl_F1c                                                                                                                                                         

rm(list= ls()[(ls() %in% c("futuras2H_sp02"))])  

#projecting FUTURE3B                                                                                                                              
myBiomodProj_F2H_sp02<- BIOMOD_Projection (bm.mod = myBiomodModelOut_sp02,                                                                                               
                                           new.env = myExpl_crop_F2H_sp02, #myExpl_crop
                                           proj.name = 'Futuro2H',
                                           new.env.xy= NULL, 
                                           models.chosen = 'all',                                                                                                          
                                           metric.binary = 'TSS',                                                                                                              
                                           compress = F,                                                                                                                     
                                           build.clamping.mask = F, 
                                           output.format = '.img') 


#ensemble  FUTURE1G                                                                                                                                                 
myBiomodEM_algoF2H_sp02<- BIOMOD_EnsembleModeling(bm.mod = myBiomodModelOut_sp02,
                                                  models.chosen = 'all',
                                                  em.by = 'all',
                                                  em.algo = c('EMmean'),
                                                  metric.select = c('TSS'),
                                                  metric.select.thresh = c(0.7),
                                                  metric.eval = c('TSS', 'ROC'),
                                                  var.import = 3,
                                                  EMci.alpha = 0.05,
                                                  EMwmean.decay = 'proportional')



# saving ensemble evaluations FUTURE1H 

# Criando a pasta se ela não existir
if (!dir.exists(myRespName_sp02)) {
  dir.create(myRespName_sp02)
}

# Capturando a saída e salvando no arquivo
capture.output(myBiomodEM_algoF2H_sp02, 
               file = file.path(myRespName_sp02, 
                                paste(myRespName_sp02, "_EM_algoF2H_sp02.txt", sep = "")))

capture.output(get_evaluations(myBiomodEM_algoF2H_sp02),                                                                                                                 
               file = file.path(myRespName_sp02,                                                                                                                         
                                paste(myRespName_sp02, "_eval_EM_algoF2H_sp02.txt", sep = "")))                                                                               


#projecting ensemble  FUTURE1H    
#setwd("~/Desktop/crop_rasters_2")
myBiomodEMProj_EM_algoF2H_sp02 <- BIOMOD_EnsembleForecasting(bm.em = myBiomodEM_algoF2H_sp02,
                                                             proj.name = 'ensemble_Futuro2H',
                                                             new.env =myExpl_crop_F2H_sp02,
                                                             models.chosen = 'all',
                                                             metric.binary = 'all',
                                                             metric.filter = 'all', 
                                                             output.format = '.img')

rm(list= ls()[(ls() %in% c("myExpl_crop_F2H_sp02", "myBiomodProj_F2H_sp02", 
                           "myBiomodEM_algoF2H_sp02", 
                           "myBiomodEMProj_EM_algoF2H_sp02"))]) 

