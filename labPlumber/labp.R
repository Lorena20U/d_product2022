library(plumber)
library(dplyr)
library(rpart)
library(readr)
library(lubridate)
library(jsonlite)
library(stringr)

#* Log some information about the incoming request
#* @filter logger
function(req){
  if (length(req$args)>0){
    # creando carpetas nuevas
    wd <- getwd()
    d <- Sys.time()
    dir <- paste0(wd,'/year=',year(d),'/day=',day(d),'/hour=',hour(d))
    if (!file.exists(dir)){
      dir.create(dir,recursive=TRUE)
    }
    jdata <- toJSON(list('resp'=req$args, 'query'=req$QUERY_STRING, 'user_a'=req$HTTP_USER_AGENT),auto_unbox=TRUE)
    write(jdata, file = paste0(dir, '/', as.integer(Sys.time()), '.json'))
  }
  plumber::forward()
}


#* Prediccion de sobrevivencia de un pasajero
#* @param Pclass clase en el que viajabe el pasajero
#* @param Sex Sexo del pasajero
#* @param Age edad del pasajero
#* @param SibSp numero de hermanos
#* @param Parch numero de parientes
#* @param Fare precio del boleto
#* @param Embarked puerto del que embarco
#* @post /titanic


function(Pclass, Sex, Age, SibSp, Parch, Fare, Embarked){
  features <- data_frame(Pclass = as.integer(Pclass),
                         Sex,
                         Age=as.integer(Age),
                         SibSp= as.integer(SibSp),
                         Parch = as.integer(Parch),
                         Fare = as.numeric(Fare),
                         Embarked)
  out <- predict(fit,features,type = "class")
  as.character(out)
}