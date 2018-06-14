##################################################
# INSTRUCTIES
##################################################

# Lees aandachtig het voorbeeld hieronder door,
# wat gebeurt er precies? Begrijp de onderliggende
# concepten!

# Het doel van dit deel van het hands-on lab is
# zelf data te splitsen in training en testing data,
# een random forest te trainen op de training data,
# en het resulterende model te testen met de testing
# data. Dit kan je voor meerdere modellen doen, en
# vervolgens kan je de modellen vergelijken. Welke
# doet het het beste?

# Tip: selecteer code in de editor en druk op
# CTRL + Enter om de geselecteerde code uit te
# voeren!

# Tip: wil je de R help van een bepaalde functie
# of library bekijken? Voer help() uit!
# Bijvoorbeeld: help(randomForest)

ZET EERST DE WORKING DIRECTORY GOED!
Dit moet wijzen naar de directory waar je datasets staan!
setwd("D:/SIGs/Machine Learning/Session2")

##################################################
# VOORBEELD: Training en testing met de Titanic
##################################################

# Bekijk dit voorbeeld aandachtig.

# Load the randomForest package and show the help
library(randomForest)
help(randomForest)

# Loading and inspecting the data
titanic = read.csv("titanic.csv")

# Dataset definition
# Variable  Definition	          Key
# survival 	Survival 	            0 = No, 1 = Yes
# pclass 	  Ticket class 	        1 = 1st, 2 = 2nd, 3 = 3rd
# sex 	    Sex
# Age 	    Age in years
# sibsp 	  # of siblings / spouses aboard the Titanic
# parch 	  # of parents / children aboard the Titanic
# ticket 	  Ticket number
# fare 	    Passenger fare
# cabin 	  Cabin number
# embarked 	Port of Embarkation 	C = Cherbourg, Q = Queenstown, S = Southampton

# Data wrangling:
# Replace missing Age values with median Age
titanic$Age <- sapply(titanic$Age, FUN=function(x) {ifelse(is.na(x),median(titanic$Age, na.rm = TRUE),x)})
# Missing Values for Embarked
which(titanic$Embarked == "")
# Find most common Embarked
table(titanic$Embarked) /sum(titanic$Embarked != "")
# Set missing values to "S"
titanic$Embarked[titanic$Embarked == ""] <- "S"
# Find percentage of missing values in Cabin
1 - (sum(titanic$Cabin != "")/nrow(titanic))
# Find Cabin categories
titanic$Cabin <- substr(titanic$Cabin,1,1)
table(titanic$Cabin)
# Set missing values to "H"
titanic$Cabin[titanic$Cabin == ""] <- "H"
# Factorize factor variables: this way R knows these variables are discrete, not actual numbers
factor_vars <- c('PassengerId','Pclass','Sex','Embarked','Cabin')
titanic[factor_vars] <- lapply(titanic[factor_vars], function(x) as.factor(x))
# End data wrangling

# set seed
set.seed(29032018)
indices = sample(1:nrow(titanic), size=0.3*nrow(titanic))

# Split data
test = titanic[indices,]
dim(test)
train = titanic[-indices,]
dim(train)

# Create a random forest
titanicForest <- randomForest(Survived~ Sex + Embarked + Cabin + Age + SibSp + Parch, data = train)

# Plot de MSE/error rate vs hoeveelheid trees
layout(matrix(c(1,2),nrow=1),width=c(4,1))
par(mar=c(5,4,4,0)) #No margin on the right side
plot(titanicForest, log="y")
# Onderstaande alleen als je een classificatie model hebt!
if(titanicForest$type=='classification'){
  par(mar=c(5,0,4,2)) #No margin on the left side
  plot(c(0,1),type="n", axes=F, xlab="", ylab="")
  legend("top", colnames(titanicForest$err.rate),col=1:4,cex=0.8,fill=1:4)
}

# Gelukkig zijn de schrijvers van de randomForest library
# geniaal, en hebben ze een MSE functie geschreven
mean(titanicForest$mse)

# Als het model een classificatie model is kan de MSE
# niet berekend worden. Dan is er de error rate!
titanicForest$err.rate

##################################################
# IBM Attrition data
# Maak nu een random forest op de attrition data.
# Zorg dat je eerst voor jezelf een goed model
# bedenkt! Dit is de belangrijkste stap van data
# science!
# Volg het stappenplan en kom tot een oplossing!
##################################################

# Data inlezen:
attrition = read.csv("attrition.csv")

# Wranglen:
# Hiermee worden alle numerieke variabelen als numeriek geinterpreteerd
# en alle niet numerieke als categorische variabelen
attrition$Attrition <- as.numeric(as.factor(attrition$Attrition)) - 1

# Stap 1: inspecteer de data! Weet welke variabelen er zijn!
# Denk altijd eerst na voor je gaat handelen!
head(attrition)
summary(attrition)
dim(attrition)

# Stap 2: splits de data op in training en testing.

# Stap 3: maak een random forest model met de training
# data, met dezelfde formule als waar je eerder een decision
# tree mee hebt gemaakt. Is je model regressie of classificatie?

# Stap 4: test je model met de testing data, gebruik predict().

# Stap 5: bereken de MSE of de error rate. Vergelijk dit met de MSE of error rate
# van je eerdere decision tree. Wat valt je op?

# Stap 6: maak een ander random forest model, train en test het,
# en bereken daarvan de MSE of de error rate.

# Stap 7: vergelijk de MSE  of de error rate van de twee modellen. Welke is
# het beste?
# Note: officieel mag je niet zomaar concluderen dat het ene model
# beter is dan het andere model op basis van de MSE of de error rate. Je moet
# een statistische toets uitvoeren om te bekijken of het verschil
# significant is. Dat valt vandaag buiten de leerdoelen.

# Vroeg klaar? Probeer eens een classificatie model te bouwen ipv een regressie
# model. Of andersom als je eerst model een classificatie model was!
