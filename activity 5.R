#### Class activity 5

## 2/8/18


make_values <- function(name){ #generic function of make_values
  UseMethod("student", name)
}


make_values.student <- function(name){ #creates random values for the student
  courage = sample(1:100,1)
  ambition = sample(1:100,1)
  intellegence = sample(1:100,1)
  effort = sample(1:100,1)
  a = c(courage, ambition, intellegence, effort) #puts random values in a vector
  class(a) = "student" #Makes vector of class student
  return(a)
}

jacob <- make_values.student() #creates random traits for a student

print(jacob)

class(jacob)

##### Part 2

sorting_matrix <- diag(4) #creates the preferences of the houses. 
                          #griffyndor prefers courage, slytherin ambition, ravenclaw intelegence, hufflepuff effort


?sort

sort.student <-function(x){
  x <- make_values.student() #makes random values for student 
   sorted = t(sorting_matrix) %*% x
  if(max(sorted) == sorted[1]){ #sorts into GRYFFINDOR if first value is highest
    print("GRYFFINDOR")
  }
  if(max(sorted) == sorted[2]){ #sorts into Slytherin if second value is highest
    print("SLYTHERIN")
  }
  if(max(sorted) == sorted[3]){#sorts into ravinclaw if third value is highest
    print("RAVENCLAW")
  }
  if(max(sorted) == sorted[4]){##sorts into hufflepuff if fourth value is highest
    print("HUFFLEPUFF")
  }
}

sort.student("jacob")


### Part 3


sort.student_part3 <-function(x){ #same as above, but it makes a new class for each house
  x <- make_values.student() 
  sorted = t(sorting_matrix) %*% x
  if(max(sorted) == sorted[1]){
    class(x) = c("student", "GRYFFINDOR")
    return(x)
  }
  if(max(sorted) == sorted[2]){
    class(x) = c("student", "SLYTHERIN")
    return(x)
  }
  if(max(sorted) == sorted[3]){
    class(x) = c("student", "RAVENCLAW")
    return(x)
  }
  if(max(sorted) == sorted[4]){
    class(x) = c("student", "HUFFLEPUFF")
    return(x)
  }
}



smith <- sort.student_part3()

class(smith)

###### part 4
 "Gryffindor_Tower" <- new.env() #Creates new enviroments 
"Black_Lake" <- new.env()
"Ravenclaw_Tower" <- new.env()
"Basement" <- new.env()


curfew <- function(x){ #generic curfew function 
  UseMethod("curfew",x)
}

curfew.GRYFFINDOR <- function(x){ #creates method for each house 
Gryffindor_Tower$student <- x
  }

curfew.SLYTHERIN <- function(x){
  Black_Lake$student <- x
}

curfew.RAVENCLAW <- function(x){
  Ravenclaw_Tower$student <- x
}

curfew.HUFFLEPUFF <- function(x){
  Basement$student <- x
}


curfew(smith) #generic uses the correct method to sort person into correct house enviroment 

ls.str(Basement) #checks what is in an enviroment 





