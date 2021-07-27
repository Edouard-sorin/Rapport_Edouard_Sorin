rm(list=ls())


load("SPIR_Ho.Rdata")
load("SPIR_Po.Rdata")
load("SPIR_Ba.Rdata")
load("SPIR_Go.Rdata")
load("trueP.Rdata")

data_SPIR_Ed <- rbind(SPIR_Ho,SPIR_Po,SPIR_Ba,SPIR_Go)

data_long_Ed <- rbind(data_long_Ho,data_long_Po,data_long_Ba,data_long_Go)

rm (SPIR_Ho,SPIR_Po,SPIR_Ba,SPIR_Go,data_long_Ho,data_long_Po,data_long_Ba,data_long_Go,truep_Ho,truep_Po,truep_Ba,truep_Go)

library(ggplot2)
library(ggdark)

data_long_var <- data_long_Ed
data_SPIR_var <- data_SPIR_Ed
masque_qPCR <- data_SPIR_var$qPCR_36 == 0
data_SPIR_var <- data_SPIR_var[masque_qPCR,]

g3 <- ggplot(data_long_var) +
  aes(x = lambda, y = reflectance, group = code_variete, color = code_variete ) +
  stat_summary(fun = mean, geom = "line" , size = 0.5) +
  scale_color_brewer(palette = "Dark2") +
  labs(x = "Longueur d'onde (en nm)", y = "Réflectance moyenne", color = "Variété") +
  dark_theme_gray() +
  theme(legend.position = "bottom")

g3

data_long_agri <- data_long_Ed

data_long.lambda <- split(data_long_agri, data_long_agri$lambda)

model.lm <- lapply(data_long.lambda, function(x) lm(reflectance ~ code_variete ,data = x))

model.anov<-lapply(model.lm, function(x) anova(x))

# Pour annalyser l'anova en tant que tel, chercher dans l'anova pour la lougueur d'onde souhaité

intermed.anov <- as.data.frame(do.call(rbind, model.anov))

intermed.anov2 <- intermed.anov[,(which(colnames(intermed.anov) == "F value" ):which(colnames(intermed.anov) == "Pr(>F)"))]

Anov_Discrim <- intermed.anov2[ !is.na(intermed.anov2),]

rownames(Anov_Discrim ) = 0:4301

Anov_Discrim  <- Anov_Discrim [(which(rownames(Anov_Discrim ) == 0 ):which(rownames(Anov_Discrim ) == 2150)),]

colnames(Anov_Discrim) <- c("F_value","P_value")

Anov_Discrim $lambda <- as.numeric(350:2500)

## Graphique de présentation ####

gx <- ggplot(Anov_Discrim) +
  
  geom_line(aes(x = lambda , y = F_value) , colour = "#FFFF00") +
  
  labs(x = "Longueur d'onde (en nm)", y = "F_value", color = "") +
  
  dark_theme_gray() +
  
  theme(panel.grid.major.y = element_line(colour = "grey20")) +
  
  annotate(geom = "text", x = Anov_Discrim$lambda[(which.max(Anov_Discrim$F_value))], y = 9+Anov_Discrim$F_value[(which.max(Anov_Discrim$F_value))], label = Anov_Discrim$lambda[(which.max(Anov_Discrim$F_value))], size = 4, colour = "red")+
  
  geom_point(aes(x = Anov_Discrim$lambda[(which.max(Anov_Discrim$F_value))], y = 2+Anov_Discrim$F_value[(which.max(Anov_Discrim$F_value))] ), shape = 124, fill="darkred" ,color = "red" , size = 2) +
  
  labs(caption = "ANOVA à un facteur : (Refléctance ~ Variété)") +
  
  theme(legend.position = "bottom") 

gx

data_long_agri <- data_long_Ed

data_long.lambda <- split(data_long_agri, data_long_agri$lambda)

model.lm <- lapply(data_long.lambda, function(x) lm(reflectance ~ code_agri,data = x))

model.anov<-lapply(model.lm, function(x) anova(x))

# Pour annalyser l'anova en tant que tel, chercher dans l'anova pour la lougueur d'onde souhaité

intermed.anov <- as.data.frame(do.call(rbind, model.anov))

intermed.anov2 <- intermed.anov[,(which(colnames(intermed.anov) == "F value" ):which(colnames(intermed.anov) == "Pr(>F)"))]

Anov_Discrim <- intermed.anov2[ !is.na(intermed.anov2),]

rownames(Anov_Discrim ) = 0:4301

Anov_Discrim  <- Anov_Discrim [(which(rownames(Anov_Discrim ) == 0 ):which(rownames(Anov_Discrim ) == 2150)),]

colnames(Anov_Discrim) <- c("F_value","P_value")

Anov_Discrim $lambda <- as.numeric(350:2500)

## Graphique de présentation ####

gy <- ggplot(Anov_Discrim) +
  
  geom_line(aes(x = lambda , y = F_value), colour = "#228B22") +
  
  labs(x = "Longueur d'onde (en nm)", y = "F_value", color = "") +
  
  dark_theme_gray() +
  
  theme(panel.grid.major.y = element_line(colour = "grey20")) +
  
  annotate(geom = "text", x = Anov_Discrim$lambda[(which.max(Anov_Discrim$F_value))], y = 9+Anov_Discrim$F_value[(which.max(Anov_Discrim$F_value))], label = Anov_Discrim$lambda[(which.max(Anov_Discrim$F_value))], size = 4, colour = "red")+
  
  geom_point(aes(x = Anov_Discrim$lambda[(which.max(Anov_Discrim$F_value))], y = 2+Anov_Discrim$F_value[(which.max(Anov_Discrim$F_value))] ), shape = 124, fill="darkred" ,color = "red" , size = 2) +
  
  labs(caption = "ANOVA à un facteur : (Refléctance ~ Parcelle)") +
  
  theme(legend.position = "bottom") 

gy


g0 <- ggplot(data_long_Ed) +
  aes(x = lambda, y = reflectance, group = qPCR_36, color = factor(qPCR_36) ) +
  stat_summary(fun = mean, geom = "line", size = 0.5) +
  scale_color_brewer(palette = "Dark2", labels = c("Négatif","Positif")) +
  labs(x = "Longueur d'onde (en nm)", y = "Reflectance moyenne", color = "Resultat du test HLB a Ct<36") +
  dark_theme_gray() +
  theme(legend.position = "bottom")

g0

g1 <- ggplot(data_long_Ed[data_long_Ed$lambda >= 400 & data_long_Ed$lambda <= 680,] ) +
  aes(x = lambda, y = reflectance, group = code_ech_arbre, color = qPCR_36 )+
  stat_summary(fun = mean, geom = "line", size = 0.5) +
  scale_color_manual(values = c('0' = "darkgreen", '1' = "brown4")) +
  labs(x = "Longueur d'onde (en nm)", y = "Reflectance moyenne")+
  dark_theme_gray() +
  theme(legend.position = "none")

g1

g2 <- ggplot(data_long_Ed[data_long_Ed$lambda >= 700 & data_long_Ed$lambda <= 1400,] ) +
  aes(x = lambda, y = reflectance, group = code_ech_arbre, color = qPCR_36 )+
  stat_summary(fun = mean, geom = "line", size = 0.5) +
  scale_color_manual(values = c('0' = "darkgreen", '1' = "brown4")) +
  labs(x = "Longueur d'onde (en nm)", y = "Reflectance moyenne") +
  dark_theme_gray()  +
  theme(legend.position = "none")

g2

data_long_agri <- data_long_Ed

data_long.lambda <- split(data_long_agri, data_long_agri$lambda)

model.lm <- lapply(data_long.lambda, function(x) lm(reflectance ~ qPCR_36 * code_variete * code_agri ,data = x))

model.anov<-lapply(model.lm, function(x) anova(x))

# Pour analyser l'anova en tant que tel, chercher dans l'anova pour la lougueur d'onde souhaité

intermed.anov <- as.data.frame(do.call(rbind, model.anov))

intermed.anov2 <- intermed.anov[,(which(colnames(intermed.anov) == "F value" ):which(colnames(intermed.anov) == "Pr(>F)"))]

Anov_Discrim <- intermed.anov2[ !is.na(intermed.anov2),]

nrow_anov <- nrow(Anov_Discrim)

rownames(Anov_Discrim)<- 0:(nrow_anov-1)

Anov_Discrim  <- Anov_Discrim [(which(rownames(Anov_Discrim) == 0 ):which(rownames(Anov_Discrim) == ((2151*6)-1))),]

colnames(Anov_Discrim) <- c("F_value","P_value")

Anov_Discrim$lambda <- as.numeric(rep (350:2500 , each = 6))

Anov_Discrim$Anov_by <- factor(rep(c("Statut seul","Influence variete","Influence parcelle","variété x statut","parcelle x statut","variété x parcelle"),each=1)) 

rm(data_long_agri,data_long.lambda,model.anov,model.lm,intermed.anov,intermed.anov2,trueP)

select.influ_statut <- grep("Statut seul", Anov_Discrim$Anov_by )

select.influ_var <- grep("variété x statut", Anov_Discrim$Anov_by )

select.influ_agri <- grep("parcelle x statut", Anov_Discrim$Anov_by )

Anov_statut <- Anov_Discrim[c(select.influ_statut,select.influ_var,select.influ_agri),]

## Graphique Relation statut seul ###

g5 <- ggplot(Anov_statut) +
  aes(x = lambda, y = F_value, colour = Anov_by) +
  geom_line() + 
  labs(x = "Longueur d'onde (en nm)", y = "F_value", color = "Influence de") +
  
  dark_theme_gray() +
  
  scale_colour_manual(values = c("#228B22","#4876FF","#FFFF00"))+
  
  theme(panel.grid.major.y = element_line(colour = "grey20")) +
  
  annotate(geom = "text", x = Anov_statut$lambda[(which.max(Anov_statut$F_value))], y = 30+Anov_statut$F_value[(which.max(Anov_statut$F_value))], label = Anov_statut$lambda[(which.max(Anov_statut$F_value))], size = 4, colour = "red")+
  
  geom_point(aes(x = Anov_statut$lambda[(which.max(Anov_statut$F_value))], y = 5+Anov_statut$F_value[(which.max(Anov_statut$F_value))] ), shape = 124, fill="darkred" ,color = "red" , size = 2) +
  
  labs(caption = "ANOVA à trois facteurs: (Refléctance ~ Satut x Variété x Parcelle)") +
  
  theme(legend.position = "bottom") 

g5

# RF #### 

library(party)  # Plot l'arbre de decision en Random Forest

library(tidyr) # pivot longer & pivot wider

library(ggplot2)
library(ggparty)
library(ggdark)

seuil.ct = 36

intermed.arbre <- aggregate(reflectance ~ code_ech_feuille + qPCR_36 + lambda, data =  data_long_Ed, mean)

mean.arbre <- pivot_wider(intermed.arbre, names_from = "lambda", values_from = "reflectance", names_prefix = "X")

RF_ct <- mean.arbre
RF_ct <- round(RF_ct[, grep("^X", names(RF_ct))],3)
masque_numeric <- sapply(RF_ct, is.numeric)
RF_ct <- RF_ct[,masque_numeric]
RF_ct[,paste0("qPCR_", seuil.ct)] <- mean.arbre[,paste0("qPCR_", seuil.ct)]


gtree_glob <- ctree( qPCR_36 ~ . , data=RF_ct)


rf.ct <- ggparty(gtree_glob, terminal_space = 0.2 , horizontal = F) +  # terminal_space =  taille aloué aux plots (terminal)
  
  geom_edge(colour = "grey77", size = 0.5) + # mettre reflectance
  
  geom_edge_label(colour = c("black") , size = 4 , alpha = 0.1) +
  
  geom_node_label(# map color of complete label to splitvar
    mapping = aes(),
    # map content to label for each line
    line_list = list(aes(label = splitvar),
                     aes(label = paste("p =",
                                       formatC(p.value,
                                               format = "e",
                                               digits = 2))),
                     aes(label = ""),
                     aes(label = paste0("Nœud ", id," N = ", nodesize))
    ),
    # set graphical parameters for each line in same order
    line_gpar = list(list(size = 12),
                     list(size = 10),
                     list(size = 6),
                     list(size = 8,
                          col = "black",
                          fontface = "bold",
                          alignment = "left")
    ),
    # only inner nodes
    ids = "inner") +
  
  geom_node_label(aes(label = paste0("Nd ", id," N = ", nodesize), size = 2), 
                  fontface = "bold",
                  ids = "terminal",
                  size = 3.7,
                  nudge_y = 0.023)+
  
  geom_node_plot(gglist = list(geom_bar(aes(x = "" , fill = qPCR_36),
                                        position = position_fill(),
                                        alpha = 0.8),
                               theme_classic(base_size = 15) ,
                               scale_fill_brewer(palette = "Dark2"),xlab("")),
                 shared_axis_labels = TRUE, # Met 1 legende pour tous les plots
                 legend_separator = TRUE, # trait entre la legende et les plots
                 size = 1.2,
                 height = 1) +
  
  theme_void() 

rf.ct

# Histo ####


X2000 <- data_SPIR_Ed[,c(which(colnames(data_SPIR_Ed) == "X2000"),which(colnames(data_SPIR_Ed) == "code_variete"),which(colnames(data_SPIR_Ed) == "code_agri"),which(colnames(data_SPIR_Ed) == "qPCR_36"))]

h1 <- ggplot(X2000) +
  aes(x = X2000, fill = qPCR_36, colour = code_variete) +
  labs(x = "Réflectance (sans unité)", y = "Effectif des valeurs de Réflectance", color = "Variété")+
  geom_histogram(bins = 30L) +
  scale_fill_hue(l = 50 , h = c(150,20), labels = c("Négatif","Positif") , name = "Statut HLB") + 
  xlim(0.04, 0.15) +
  scale_colour_manual(values = c("#00EEEE","#FFFF00","#0000CD"))+
  dark_theme_gray() +
  theme(legend.position = "bottom") +
  facet_wrap(vars(code_agri))

h1

# Pred PLS ####


library(caTools) # sample.split

# Usage de l'algorithme partial least square

library(pls) # Fonction Partial Least Square

# Mise en forme des donnees 
library(tidyr) # transformation du format des donnees : pivot_longer
library(tidyverse)#faciliter l'installation et le chargement de plusieurs paquets "tidyverse"

library(ggplot2) # Package ggplot pour graphiques
library(ggdark) # Met un style de graphique ggplot en noir


nb.rep <- 6
seuil.ct <- 36

set.seed(1)  # Pour la reproductibilité

Tirage <- split(data_SPIR_Ed, data_SPIR_Ed$code_ech_feuille, drop = T)


maliste <- lapply(Tirage,function(feuille){ 
  list_pls <- feuille[sample(1:length(feuille$code_ech_feuille), nb.rep),]  
  # on fait ça pour tte les feuilles mais tjrs en choisissant le nombre de rep tire aleatoirement
  code <- grep("^code_ech", names(list_pls))
  qPCR <- grep("^qPCR_", names(list_pls))
  sortie <- cbind.data.frame(unique(list_pls[c(code,qPCR)])
                             , matrix(apply(list_pls[-(which(colnames(list_pls) == "code_variete"):which(colnames(list_pls) == "qPCR_36"))], 2, mean)
                                      , nr = 1, dimnames = list(NULL, names(list_pls)[-(which(colnames(list_pls) == "code_variete"):which(colnames(list_pls) == "qPCR_36"))])))
  sortie
})

test_ed <- do.call(rbind, maliste) # on colle toute les listes créées précédement 

test_ed <- test_ed[-(which(colnames(test_ed) == "qPCR_32"))]

test_ed[[paste0("qPCR_", seuil.ct)]] <- as.numeric(as.character(test_ed[[paste0("qPCR_", seuil.ct)]] ))

decoup <- sample.split(test_ed[,paste0("qPCR_", seuil.ct)], SplitRatio = 0.75) # on decoupe le jeu de donné en training set et test set

train_pls <- test_ed[decoup,]
test_pls <- test_ed[!decoup,] 

model_pls_36 <-  plsr(
  
  train_pls[[paste0("qPCR_", seuil.ct)]] ~ . ,
  data = train_pls[, grep("^X", names(train_pls))], 
  scale = TRUE, 
  validation = "CV"
  
)

# Prediction

pls_pred <- test_pls

# Prediction sur les feuilles de la base d'apprentissage

pls_pred$pls_pred_36 <- predict(model_pls_36, newdata = test_pls[, grep("^X", names(test_pls))], decision.values = T, ncomp=100)

# Conversion des resultats de la prediction en numerique

pls_pred$pls_pred_36 = as.numeric(as.character(pls_pred$pls_pred_36))

# Re arrangemant des donnees

select.lambda <- grep("^X", names(pls_pred))
pls_pred <- pls_pred[,c(names(pls_pred)[-select.lambda], names(pls_pred)[select.lambda] )]

# Moyennage des resultats de la prediction pour chaque arbres

pls_pred_arbres_36 = aggregate(pls_pred_36 ~ code_ech_arbre, data = pls_pred, mean, na.rm = T)

pls_pred_arbres_36$pls_pred_36[pls_pred_arbres_36$pls_pred_36 < 0] = 0   # Pour enlever les valeurs négatives

# Critere choisi sur les observations graphiques

crit.pos <- 0.4
crit.neg <- 0.35

# Parametrage pour presentation graphique et la matrice de confusion

pls_pred_arbres_36$statut_pred <- "Indéterminé"
pls_pred_arbres_36$statut_pred[pls_pred_arbres_36$pls_pred_36 >= crit.pos  ] <- "Positif"
pls_pred_arbres_36$statut_pred[pls_pred_arbres_36$pls_pred_36 <= crit.neg  ] <- "Négatif"

pls_pred_arbres_36$crit <- 0.5
pls_pred_arbres_36$crit[pls_pred_arbres_36$pls_pred_36 >= crit.pos  ] <- 1
pls_pred_arbres_36$crit[pls_pred_arbres_36$pls_pred_36 <= crit.neg  ] <- 0

# Creation d'une nouvelle colonne des resulats issu de la qPCR, pour comparer à la valeurs predite

trueP$seuil.32 <- NULL

pls_pred_arbres_36[paste("qPCR", seuil.ct, sep = "_")] <- lapply(trueP, function(x)
  as.numeric(pls_pred_arbres_36$code_ech_arbre %in% x ))

# Résultats de la prédiction sous forme de matrice de confusion 

pls_pred_arbres_36$crit <- factor(pls_pred_arbres_36$crit ,levels= c(0,1))

pls_confusion_matrix_36 <- ftable( qPCR_36 ~ crit , data = pls_pred_arbres_36 )

pls_confusion_matrix_36 <- as.matrix(pls_confusion_matrix_36)

TN <- pls_confusion_matrix_36[1,1]

TP <- pls_confusion_matrix_36[2,2]

FP <- pls_confusion_matrix_36[2,1]

FN <- pls_confusion_matrix_36[1,2]

#pls_confusion_matrix_36


pls_pred_arbres_36$Pred.correct = "Négatif"
pls_pred_arbres_36$Pred.correct[pls_pred_arbres_36$code_ech_arbre %in% trueP$seuil.36] = "Positif"

g6 <- ggplot(pls_pred_arbres_36)+
  aes(y = pls_pred_36 , x = code_ech_arbre, color = statut_pred) +
  geom_rect(aes(xmin = "A1", xmax = "T7", ymin = crit.neg, ymax = crit.pos), lwd = 1 , fill = "gray50", color = "gray50") +
  geom_point(aes(shape = Pred.correct, size = Pred.correct), fill ="blue") + 
  scale_size_manual(values =c(2.1,2.9) ,name = "Statut Confirmé")+
  scale_shape_manual(values =c(4,21),name = "Statut Confirmé")+
  geom_segment(aes(xend = code_ech_arbre, y = 0, yend = pls_pred_36)) +
  scale_color_manual(values = c(Négatif = "green", Indéterminé = "gray20", Positif = "red")) +
  labs(x = "Code de l'arbre", y = "Résultat moyen des feuilles entre 0 et 1", color = "Statut Prédit") +  dark_theme_gray() +
  theme(legend.position = "bottom") 

g6

# Calcul des parametres pls de la matrice de confusion ####

Accuracy_pls36 <- ((TP+TN) / (TP+TN+FN+FP)*100)

Precision_pls36 <- ((TP / (TP+FP)*100))

Sensitivity_pls36 <- ((TP / (TP+FN)*100))

Parametre_pls_36 <- rbind(Accuracy_pls36,Precision_pls36,Sensitivity_pls36)


rownames(Parametre_pls_36) <- c("Accuracy","Precision","Sensitivity")

# Organiser resultat dans un tableau à 4 cases avec Vrai postif, Vrai négatif , Faux positif, Faut négatif


# Amelio Protocole ####

data_rep_feuille.36 <- read.table(file = "Param_SVM_nb_feuille_ct36_1000_simu.csv"
                                  , header = T
                                  , sep = ";"
                                  , stringsAsFactors = T
                                  , row.names = 1
                                  , na.strings = c("","NA")
                                  , dec = "." )



g7<- ggplot(data = data_rep_feuille.36) +
  aes(x = nb.rep, y = valeurs, color = critere, group = critere)+
  stat_summary(geom = "pointrange", fun.data = function(x) mean_se(x, mult = qt(0.975, length(x) - 1))) +
  stat_summary(geom = "line", fun = mean) +
  scale_x_continuous(breaks=seq(1:10)) +  
  labs(x = "Nombre de feuilles échantillonnées sur chaque arbre", y = "Valeurs moyennes", color = "Paramètres de performance en SVM") +
  dark_theme_gray() +
  theme(legend.position = "bottom")+
  scale_colour_viridis_d() +
  theme(panel.grid.major.y = element_line(colour = "grey20"))

g7


load("SVM_ct36_6rep_100simu_18_02.Rdata")

g8 <- ggplot(data = data_global.36) +
  aes(x = nb.rep, y = valeurs, color = critere, group = critere)+
  stat_summary(geom = "pointrange", fun.data = function(x) mean_se(x, mult = qt(0.975, length(x) - 1))) +
  stat_summary(geom = "line", fun = mean) +
  scale_x_continuous(breaks=seq(1:6)) +  
  labs(x = "Nombre de mesures SPIR effectuées sur chaque feuille", y = "Valeurs moyennes", color = "Paramètres de performance en SVM") +
  dark_theme_gray() +
  theme(legend.position = "bottom")+
  scale_colour_viridis_d() +
  theme(panel.grid.major.y = element_line(colour = "grey20"))

g8