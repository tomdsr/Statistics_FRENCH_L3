# CODE R - Projet statistique - Tests d'hypothèses

fertiproj=read.table("fertiproj11.txt",header=TRUE)
attach(fertiproj) # donc plus besoin de préciser fertiproj$variable

# QUESTION 2
# Recodage de alcool et tabagisme :
alcool2=factor(alcool,labels=c("non","oui")) # oui pour 1 et non pour 0
tabagisme2=factor(tabagisme,labels=c("non","oui"))
# Création de la variable num :
num=vol*conc
# Création de la variable imcc :
summary(imc) # pour obtenir les bornes min et max
imcc=cut(imc,breaks=c(11,24,29,54),include.lowest=TRUE)
# Création de la variable examc :
summary(exam)
exam1=cut(exam,breaks=c(-1,0,6),include.lowest=TRUE)
examc=factor(exam1,labels=c("aucun examen anormal","au moins un examen anormal"))

# QUESTION 3
# Etude descriptive des variables :
summary(alcool2)
summary(tabagisme2)
table(fievre)
summary(imcc)
summary(examc)
summary(vol)
summary(conc)
summary(vit)
summary(age)
summary(dureeinf)
summary(mob)
summary(num)
# Graphes :
plot(alcool2,main="Le patient est-il alcoolique ?")
plot(tabagisme2,main="Le patient fume-t-il ?")
plot(imcc,main="Répartition dans les 3 classes de valeurs de l'IMC")
plot(examc,main="Répartition dans les 2 classes de la variable examc")
hist(vol, freq = FALSE, main="Histogramme : vol")
boxplot(vol,main="Boite à moustache : vol")
hist(conc, freq = FALSE, main="Histogramme : conc")
boxplot(conc,main="Boite à moustache : conc")
hist(vit, freq = FALSE, main="Histogramme : vit")
boxplot(vit,main="Boite à moustache : vit")
hist(age, freq = FALSE, main="Histogramme : age")
boxplot(age,main="Boite à moustache : age")
hist(dureeinf, freq = FALSE, main="Histogramme : dureeinf")
boxplot(dureeinf,main="Boite à moustache : dureeinf")
hist(mob, freq = FALSE, main="Histogramme : mob")
boxplot(mob,main="Boite à moustache : mob")
hist(num, freq = FALSE, main="Histogramme : num")
boxplot(num,main="Boite à moustache : num")
# Test de la normalité de vit :
# Test de Kolmogorov-Smirnov d'adéquation à une loi normale quelconque :
# On met mean(vit) et sd(vit) car on teste que la loi est normale
# mais sans préciser avec quels paramètres comme on ne sait pas
# ce que valent mu et sigma, donc on en prend des estimations sur les données
ks.test(vit,"pnorm",mean(vit),sd(vit))

# QUESTION 4 - A
summary(dureeinf)

# QUESTION 4 - B
table(imcc) # Pour obtenir la répartition empirique (dans notre échantillon)
effempiriques = c(602,325,73)
p0 = c(0.584,0.292,0.124)
# Pour vérifier les conditions de validité du test :
chisq.test(effempiriques,p=p0)$expected # Effectifs attendus
# Test du X² d'adéquation :
chisq.test(effempiriques,p=p0)

# QUESTION 4 - C
table(tabagisme2)
effempiriques = c(607,393)
p0 = c(0.64,0.36)
# Pour vérifier les conditions de validité du test :
chisq.test(effempiriques,p=p0)$expected
# Test du X² d'adéquation :
chisq.test(effempiriques,p=p0)

# QUESTION 4 - D
# Test de Student de comparaison d’une espérance à une valeur donnée :
t.test(num,mu=250)

# QUESTION 5 - A
# Nuage de points :
plot(x=age,y=vol,xlab="Âge",ylab="Volume de l'éjaculat",
     xlim=c(10, 65),ylim=c(0, 15),
     main="Nuage de points : variables age et vol")
# Coefficient de correlation :
cor(age,vol,use="pairwise.complete.obs")
# Test du coefficient de corrélation linéaire de Pearson :
cor.test(age,vol)

# QUESTION 5 - B
# VITALITE :
# Nuage de points :
plot(x=age,y=vit,xlab="Âge (en années)",ylab="Vitalité des spermatozoïdes (%)",
     main="Nuage de points : variables age et vit")
# Coefficient de correlation :
cor(age,vit,use="pairwise.complete.obs")
# Test du coefficient de corrélation linéaire de Pearson :
cor.test(age,vit)
# MOBILITE :
# Nuage de points :
plot(x=age,y=mob,xlab="Âge (en années)",ylab="Mobilité des spermatozoïdes (%)",
     main="Nuage de points : variables age et mob")
# Coefficient de correlation :
cor(age,mob,use="pairwise.complete.obs")
# Test du coefficient de corrélation linéaire de Pearson :
cor.test(age,mob)

# QUESTION 5 - C.
# Moyennes conditionnelles des âges selon le comportement tabagique :
moycondtab=tapply(age,tabagisme2,mean)
# Boites à moustaches :
boxplot(age~tabagisme2)
points(moycondtab,col="magenta")
# Effectifs pour chaque modalités de tabagisme2 :
table(tabagisme2)
# Test de Welch :
t.test(age~tabagisme2,var.equal=FALSE)
# QUESTION 5 - D.
# Moyennes conditionnelles :
moycondage=tapply(age,imcc,mean)
# Boites à moustaches juxtaposées :
boxplot(age~imcc,xlab="IMC",ylab="Âge (en année)")
points(moycondage,col="red",pch=19)
# Vérification des conditions de validité :
# Normalité : Calculons les effectifs par groupe :
table(imcc)
# Homogénéité des variances : Effectuons un test de Brown et Forsythe :
library(car)
leveneTest(age,as.factor(imcc),center=median)
# Anova classique comme on n'a pas rejeté l'homoscédasticité :
oneway.test(age~as.factor(imcc),var.equal=TRUE)
# Si on veut aussi le tableau d'analyse de la variance :
summary(aov(age~as.factor(imcc)))
# Tests de comparaisons multiples :
# Test LSD :
pairwise.t.test(age,imcc,p.adjust.method = "none")
# Test de Bonferroni :
pairwise.t.test(age,imcc,p.adjust.method = "bon")

# QUESTION 6 - A
# Tableau de contingence des 2 variables qualitatives :
tableaucont=table(alcool2,tabagisme2)
print(tableaucont)
# Profils lignes :
round(prop.table(tableaucont,1),digits=2)
barplot(t(prop.table(tableaucont,1)),beside=TRUE,col=c("blue","red"),
        legend.text=TRUE)
# Profils colonnes :
round(prop.table(tableaucont,2),digits=2)
barplot(prop.table(tableaucont,2),beside=TRUE,col=c("green","blue"),
        legend.text=TRUE)
# Effectifs attendus sous H0 :
chisq.test(tableaucont,correct=FALSE)$expected
# Test du X² d'indépendance :
chisq.test(tableaucont,correct=FALSE)

# QUESTION 6 - B
# Tableau de contingence des 2 variables qualitatives :
tableaucont6b=table(tabagisme2,imcc)
print(tableaucont6b)
# Profils lignes :
round(prop.table(tableaucont6b,1),digits=2)
barplot(t(prop.table(tableaucont6b,1)),beside=TRUE,col=c("blue","red","green"),
        legend.text=TRUE)
# Profils colonnes :
round(prop.table(tableaucont6b,2),digits=2)
barplot(prop.table(tableaucont6b,2),beside=TRUE,col=c("green","blue"),
        legend.text=TRUE)
# Effectifs attendus sous H0 :
chisq.test(tableaucont6b,correct=FALSE)$expected
# Test du X² d'indépendance :
chisq.test(tableaucont6b,correct=FALSE)

# QUESTION 6 - C
# Moyennes conditionnelles des vitalités selon la consommation d'alcool :
moycondvit1=tapply(vit,alcool2,mean)
# Boites à moustaches :
boxplot(vit~alcool2)
points(moycondvit1,col="magenta")
# Test de Welch :
t.test(vit~alcool2,var.equal=FALSE)
# Moyennes conditionnelles des vitalités selon le statut tabagique :
moycondvit2=tapply(vit,tabagisme2,mean)
# Boites à moustaches :
boxplot(vit~tabagisme2)
points(moycondvit2,col="magenta")
# Test de Welch :
t.test(vit~tabagisme2,var.equal=FALSE)

# QUESTION 6 - D.
# Moyennes condionnelles des vitalités selon le statut de fièvre :
moycondvit3=tapply(vit,fievre,mean)
# Boites à moustaches :
boxplot(vit~fievre)
points(moycondvit3,col="magenta")
table(fievre)
# Test non paramétrique de Wilcoxon-Mann-Whitney :
wilcox.test(vit~fievre)

# QUESTION 6 - E
# Tableau de contingence des 2 variables :
tableaucont2=table(fievre,examc)
print(tableaucont2)
# Profils lignes :
round(prop.table(tableaucont2,1),digits=2)
barplot(t(prop.table(tableaucont2,1)),beside=TRUE,col=c("blue","red"),
        legend.text=TRUE)
# Profils colonnes :
round(prop.table(tableaucont2,2),digits=2)
barplot(prop.table(tableaucont2,2),beside=TRUE,col=c("green","blue"),
        legend.text=TRUE)
# Effectifs attendus sous H0 :
chisq.test(tableaucont2,correct=FALSE)$expected
# Test :
chisq.test(tableaucont2,correct=FALSE)

# QUESTION 6 - F
# Moyennes conditionnelles :
moycond=tapply(num,imcc,mean)
# Boites à moustaches juxtaposées :
boxplot(num~imcc,xlab="IMC",ylab="Numération (en millions)",ylim=c(0,800))
points(moycond,col="red",pch=19)
# Vérification des conditions de validité :
# Normalité : Calculons les effectifs par groupe :
table(imcc)
# Homogénéité des variances : Effectuons un test de Brown et Forsythe :
library(car)
leveneTest(num,as.factor(imcc),center=median)
# Anova classique comme on n'a pas rejeté l'homoscédasticité :
oneway.test(num~as.factor(imcc),var.equal=TRUE)
# Si on veut aussi le tableau d'analyse de la variance :
summary(aov(num~as.factor(imcc)))

# QUESTION 7
# Régression linéaire multiple :
modele = lm(vit ~ alcool2 + tabagisme2 + fievre + imcc + examc, data=fertiproj)
summary(modele)
modele = lm(vol ~ alcool2 + tabagisme2 + fievre + imcc + examc, data=fertiproj)
summary(modele)
modele = lm(conc ~ alcool2 + tabagisme2 + fievre + imcc + examc, data=fertiproj)
summary(modele)