AgeUp = function(population, alldead, lifespan, g){

  if(length(population[population[,9] == 1, 1])>0){
      population <- population %>% 
                      mutate(age = if_else(alive==1, age+1, age), 
                             alive = if_else(age>lifespan, 0, alive), 
                             gendead = if_else(age>lifespan, g, gendead), 
                             yearSpawned = if_else(age>lifespan, NA, yearSpawned), 
                             fate = if_else(age>lifespan, "lifespanMort", fate))
        }else{
            alldead <<- "all dead at age up"
          }
          return(population)
        }
