### PS3 

#Part 1
Monty_Hall <-  as.numeric(sample(1:3,1))

class(Monty_Hall) <- "door"  #makes S3 class called door

class(Monty_Hall)


#Part 2

PlayGame <- function(x){   #creates Generic function called Playgame
  UseMethod("PlayGame",x)
}

PlayGame.door <- function(x){ #plays the game
  car_door = as.numeric(sample(1:3,1))
  if(Monty_Hall == car_door){
    print("Congrats")
  }else{
    print("Sorry")
  }
}


PlayGame(Monty_Hall)

#Do it in S4

setClass(Class = "door(S4)", #creates class in door in S4
         representation = representation(
           door = "numeric"
         ),
         prototype = prototype(
           door = c()
         )
)
new("door(S4)")


setValidity("door(S4)", function(object){ #ensures door is one number between 1-3
 test1 = (object@door ==1 | object@door ==2 | object@door == 3)
 test2 = (length(object@door) == 1)
 if(!test1 | !test2){return("@door not valid")}
}
)

setMethod("initialize", "door(S4)", function(.Object, ...) { #initializes 
  value = callNextMethod()
  validObject(value)
  return(value)
})

new("door(S4)", door= 1)

setGeneric("PlayGame_S4", #sets generic function in S4
           function(object="door(S4)") {
             standardGeneric("PlayGame_S4")
           } )

setMethod("PlayGame_S4", "door(S4)", #plays the game in S4
          function(object){
            car_door = as.numeric(sample(1:3,1))
            if(object@door == car_door){
              print("Congrats")
            }else{
              print("Sorry")
            } 
            
          } )

Monty_Hall_S4 <- new("door(S4)", door= 3)

PlayGame_S4(Monty_Hall_S4)


