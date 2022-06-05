library(frbs)
#----------- input data ----------------
setwd("C:/Users/MSI/Desktop/ANFIS")
data.train <- read.csv('automobil_train.csv')
data.tst <- read.csv('automobil_test.csv')
#----------- Fuzzy Mamdani----------------
# mendefinisikan fungsi keanggotaan input
varinp.mf <- matrix(c(2, 0, 10, 20, NA,
                       5, 30, 7, NA,NA, 
                       3, 40,50, 60, NA, 
                       2, 50, 135, 250, NA, 
                       3, 135, 275, 350, NA),
                    nrow = 5, byrow = FALSE)

num.fvalinput <- matrix(c(3, 2), nrow=1)

# linguistik variabel
varinput.1 <- c("rendah", "sedang", "tinggi")
varinput.2 <- c("kecil", "besar")
names.varinput <- c(varinput.1, varinput.2)

## interval data
range.data <- matrix(c(0,60, 50, 350,5000,50000), nrow=2)

# mengatur parameter inferensia
type.defuz <- "WAM"
type.tnorm <- "MIN"
type.snorm <- "MAX"
type.implication.func <- "ZADEH"

# nama model
name <- "Sim-0"

#nama variabel
colnames.var <- c("highway.mpg","engine.size","price" )
num.fvaloutput <- matrix(c(3), nrow = 1)

# variabel linguistik output
varoutput.1 <- c("murah", "sedang", "mahal")
names.varoutput <- c(varoutput.1)
# mendefinisikan fungsi keanggotaan output
varout.mf <- matrix(c(2, 5000, 15000, 20000, NA, 
                      5, 25000, 9000, NA, NA, 
                      3, 30000, 40000, 50000, NA),
                    nrow = 5, byrow = FALSE)

# tipe model mamdai
type.model <- "MAMDANI"

## membuat IF-THEN RULES
rule.or <- matrix(c("tinggi", "or", "kecil", "->", "murah",
                     "sedang", "or", "besar", "->", "sedang", 
                     "rendah", "or", "besar", "->", "mahal"), 
                   nrow = 3, byrow = TRUE) 
## melakukan pemodelan mamdani
object.or <- frbs.gen(range.data, num.fvalinput, names.varinput, 
                       num.fvaloutput, varout.mf, names.varoutput, rule.or, ##############
                       varinp.mf, type.model, type.defuz, type.tnorm, 
                       type.snorm, func.tsk = NULL, colnames.var, type.implication.func, name)

## Plot membership function.
plotMF(object.or)

## melakukan prediksi
res.or<- predict(object.or, as.matrix(data.tst[,1:2]))$predicted.val
hasil=data.frame(data.tst[,1:2],Price_aktual=data.tst$price,Price_prediksi=res.or)

## plot pencar (evaluasi model)
library(ggplot2)
library(reshape2)
mdata <- melt(hasil,id=c("highway.mpg", "engine.size"))
colnames(mdata)<-c("highway.mpg", "engine.size","kategori","price")
# hasil fuzzy manual higway mpg
ggplot(mdata, aes(x=highway.mpg , y=price, shape=kategori,color=kategori)) +
  geom_point() +  geom_smooth(method=lm, aes(fill=kategori))
# hasil fuzzy manual engine size
ggplot(mdata, aes(x=engine.size , y=price, shape=kategori,color=kategori)) +
  geom_point() +  geom_smooth(method=lm, aes(fill=kategori))
RMSE(hasil$prediksi,hasil$aktual)

#----------- Fuzzy Takagi Sugeno Kang ----------------
type.model <- "TSK"
## mendefinisikan IF-THEN rules; 
rule.or <- matrix(c("tinggi", "or", "kecil", "->",
                    "sedang", "or", "besar", "->", 
                    "rendah", "or", "besar", "->"), 
                  nrow = 3, byrow = TRUE) 
## memasukkan fungsi linier
func.tsk <- matrix(c(-150, 50,5000,
                     -269.60, 140.91,5000, 
                     -300, 200,5000), 
                   nrow = 3, byrow = TRUE)
## melakukan pemodelan TSK
object.tsk <- frbs.gen(range.data, num.fvalinput, names.varinput, 
                       num.fvaloutput, varout.mf, names.varoutput, rule.or, ##############
                       varinp.mf, type.model, type.defuz, type.tnorm, 
                       type.snorm, func.tsk, colnames.var, type.implication.func, name)

## Plot membership function.
plotMF(object.tsk)

# melakukan prediksi
res.or <- predict(object.tsk, as.matrix(data.tst[,1:2]))$predicted.val
hasil=data.frame(data.tst[,1:2],Price_aktual=data.tst$price,Price_prediksi=res.or)


## plot pencar (evaluasi model)
mdata <- melt(hasil,id=c("highway.mpg", "engine.size"))
colnames(mdata)<-c("highway.mpg", "engine.size","kategori","price")
# hasil fuzzy manual higway mpg
ggplot(mdata, aes(x=highway.mpg , y=price, shape=kategori,color=kategori)) +
  geom_point() +  geom_smooth(method=lm, aes(fill=kategori))

# hasil fuzzy manual engine size
ggplot(mdata, aes(x=engine.size , y=price, shape=kategori,color=kategori)) +
  geom_point() +  geom_smooth(method=lm, aes(fill=kategori))
RMSE(hasil$prediksi,hasil$aktual)
library(caret)
##------------- ANFIS-----------------
## mengatur parameter
dat100<-as.data.frame(dat100)
dat100$ybin<-dat100$ybin+1
dat<-dat100
method.type <- "FRBCS.W"
control <- list(num.labels = 7, 
                type.mf = "GAUSSIAN", 
                type.defuz = "WAM",
                type.tnorm = "MIN", 
                type.snorm = "MAX",
                name = "anfis_kasus_sinkronisasi",
                max.iter=100)
set.seed(13)
range.data_ <-matrix(apply(dat[, -ncol(dat)], 2, range), nrow = 2)
## pemodelan ANFIS menggunakan data training
object.anfis <- frbs.learn(dat100, range.data_, method.type,control)
## prediksi ANFIS mengguanakan data testing
res.anfis <- predict(object.anfis, dat[,-6])

## ringkasan output ANFIS
summary(object.anfis)
## Plot membership functions
plotMF(object.anfis)

## plot pencar (evaluasi model)
aktual=as.factor(dat$ybin)
prediksi=as.factor(res.anfis)
levels(aktual)<-c(1,2,3,4,5,6)
levels(prediksi)<-c(1,2,3,4,5,6)
confusionMatrix(aktual,prediksi)

mdata <- melt(hasil,id=c("highway.mpg", "engine.size"))
colnames(mdata)<-c("highway.mpg", "engine.size","kategori","price")
# hasil fuzzy manual higway mpg
ggplot(mdata, aes(x=highway.mpg , y=price, shape=kategori,color=kategori)) +
  geom_point() +  geom_smooth(method=lm, aes(fill=kategori))
# hasil fuzzy manual engine size
ggplot(mdata, aes(x=engine.size , y=price, shape=kategori,color=kategori)) +
  geom_point() +  geom_smooth(method=lm, aes(fill=kategori))
RMSE(hasil$Price_prediksi,hasil$Price_aktual)





## klasifikasi SINKRONISASI

dat<-data.train
range.data_ <-matrix(apply(dat, 2, range), nrow = 2)



control <- list(num.labels = 3, type.mf = "TRIANGLE", type.tnorm = "MIN", 
                type.implication.func = "ZADEH", name = "sim-0") 

control <- list(num.labels = 7, 
                type.mf = "GAUSSIAN", 
                type.defuz = "WAM",
                type.tnorm = "MIN", 
                type.snorm = "MAX",
                name = "anfis_kasus_sinkronisasi",
                max.iter=10)


object <- frbs.learn(dat, range.data_, method.type, control)


## ringkasan output ANFIS
summary(object)
## Plot membership functions
plotMF(object)


## conduct the prediction process
res.test <- predict(object, dat[,-ncol(dat)])

