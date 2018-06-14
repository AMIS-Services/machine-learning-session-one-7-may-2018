##################################################
# INSTRUCTIES
##################################################

# Lees aandachtig het voorbeeld hieronder door,
# wat gebeurt er precies? Begrijp de onderliggende
# concepten!

# Het doel van dit deel van het hands-on lab is
# zelf data te splitsen in training en testing data,
# een decision tree te trainen op de training data,
# en het resulterende model te testen met de testing
# data. Dit kan je voor meerdere modellen doen, en
# vervolgens kan je de modellen vergelijken. Welke
# doet het het beste?

# Tip: selecteer code in de editor en druk op
# CTRL + Enter om de geselecteerde code uit te
# voeren!

# Tip: wil je de R help van een bepaalde functie
# of library bekijken? Voer help() uit!
# Bijvoorbeeld: help(tree)

ZET EERST DE WORKING DIRECTORY GOED!
Dit moet wijzen naar de directory waar je datasets staan!
setwd("D:/SIGs/Machine Learning/Session2")

##################################################
# VOORBEELD: Training en testing met de Titanic
##################################################

# Bekijk dit voorbeeld aandachtig.
# Wie het technische deel van Machine Learning SIG 1
# heeft gevolgd zal dit herkennen.

# Load the tree package
library(tree)

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
factor_vars <- c('PassengerId','Pclass','Sex','Embarked','Cabin','Survived')
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

# Create the tree
titanicTree <- tree(Survived~ Sex + Embarked + Cabin + Age + SibSp + Parch, data = train)
plot(titanicTree)
text(titanicTree, pretty = 0)

# Predict test set values using trained model
predicted_values <- predict(titanicTree, test)

# MSE berekenen: ALLEEN voor regressie modellen
mean((test$Survived - predicted_values) ^ 2)

# BELANGRIJK! Predict geeft bij classificatie modellen een kans terug.
# Wanneer je een werkelijke voorspelling wilt, zoals wanneer je
# een confusion matrix wilt maken, moet je predict vertellen dat
# het om een classificatie gaat!
predicted_values <- predict(titanicTree, test, type = "class")
# Merk op! De extra parameter type = "class".

# Confusion matrix maken: ALLEEN voor classificatie modellen
confusionMatrix <- table(predicted_values, test$Survived)
confusionMatrix
# Error rate berekenen: ALLEEN voor classificatie modellen
errorRate <- 1-sum(diag(matrix(confusionMatrix, nrow = nrow(confusionMatrix)))) / sum (confusionMatrix)
# Analyse: de som van de diagonaal van de confusion matrix geeft het totale aantal
# correct voorspelde items. Gedeeld door het totaal is dit het percentage correct voorspeld.
# 1 min dit percentage is het percentage incorrect voorspeld, dus de error rate!

##################################################
# IBM Attrition data
# Jullie beurt om data te verdelen in train en
# test. Maak een prediction en bereken de MSE.
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

# Stap 3: maak een decision tree model met de training
# data. Is je model regressie of classificatie?

# Stap 4: test je model met de testing data, gebruik predict().

# Stap 5: bereken de MSE of de error rate.

# Stap 6: maak een ander decision tree model, train en test het,
# en bereken daarvan de MSE of de error rate.

# Stap 7: vergelijk de MSE of de error rate van de twee modellen. Welke is
# het beste?
# Note: officieel mag je niet zomaar concluderen dat het ene model
# beter is dan het andere model op basis van de MSE. Je moet
# een statistische toets uitvoeren om te bekijken of het verschil
# significant is. Dat valt vandaag buiten de leerdoelen.

