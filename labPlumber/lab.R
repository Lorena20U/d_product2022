library(plumber)
library(dplyr)
library(rpart)
library(readr)
library(lubridate)

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

users <- data.frame(
  uid=c(1, 2, 3, 4, 5),
  username=c("a", "b", "c", "d", "e")
)
#* ruteo
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
     
     