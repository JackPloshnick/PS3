### PS3 

#Part 1
Monty_Hall <-  as.numeric(sample(1:3,1))

class(Monty_Hall) <- "door"

class(Monty_Hall)


#Part 2

PlayGame <- function(x){
  UseMethod("PlayGame",x)
}

PlayGame.door <- function(x){
  car_door = as.numeric(sample(1:3,1))
  if(Monty_Hall == car_door){
    print("Congrats")
  }else{
    print("Sorry")
  }
}


PlayGame(Monty_Hall)

#Do it in S4

setClass(Class = "door",
         representation = representation(
           door = "numeric"
         ),
         prototype = prototype(
           door = c()
         )
)
new("door")


setValidity("door", function(object){
 test1 = (object@door ==1 | object@door ==2 | object@door == 3)
 test2 = (length(object@door) == 1)
 if(!test1 | !test2){return("@door not valid")}
}
)

setMethod("initialize", "door", function(.Object, ...) {
  value = callNextMethod()
  validObject(value)
  return(value)
})

new("door", door= 1)

setGeneric("PlayGame",
           function(object="door") {
             standardGeneric("PlayGame")
           } )

setMethod("PlayGame", "door",
          function(object){
            car_door = as.numeric(sample(1:3,1))
            if(object@door == car_door){
              print("Congrats")
            }else{
              print("Sorry")
            } 
            
          } )

myObject <- new("door", door= 3)

PlayGame(myObject)


