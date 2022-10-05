library(plumber)
library(dplyr)
library(rpart)
library(readr)
library(lubridate)
library(jsonlite)
library(stringr)

#* @apiTitle Modelo del Titanic
#* @apiDescription Este api nos servira para predicir
#* si un pasajero del titanic sobrevive o no
data <- read_delim("data.csv")

data$fecha <- format(as.Date(as.Date("1899-12-30") + as.numeric(data$fecha), "%d-%m-%Y"), "%d-%m-%Y")
data$fecha <- dmy(data$fecha)
data

#* @apiTitle Modelo del Titanic
#* @apiDescription Este api nos servira para predicir
#* si un pasajero del titanic sobrevive o no

fit <- readRDS("modelo_final.rds")
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


users <- data.frame(
  uid=c(1, 2, 3, 4, 5),
  username=c("a", "b", "c", "d", "e")
)
#* Ruteo
#* @get /users/<id>
function(id){
  subset(users, uid %in% id)
}


#* @filter setuser
function(req){
  un <- req$cookies$user
  # Make req$username available to endpoints
  req$username <- un
  
  plumber::forward()
}



#* @get /user/<from>/connect/<to>
function(from, to){
  from <- ymd(as.character(from))
  to <- ymd(as.character(to))
  data %>%
    filter(fecha >= from & fecha <= to)
}

#* @get /type/<id>
function(id){
  list(
    id = id,
    type = typeof(id)
  )
}

#* @get /user/<n:int>
function(n){
  list(
    id = n,
    type = typeof(n)
  )
}

#* @get /user/<valor:double>
function(valor){
  list(
    id = valor,
    type = typeof(valor)
  )
}

#* @get /user/<resp:bool>
function(resp){
  list(
    id = resp,
    type = typeof(resp)
  )
}
     
     
