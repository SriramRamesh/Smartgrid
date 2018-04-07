# Users contain the data for all users and stages
# getdsm is used to get Demand Side management for each user at a given stage
getdsm <- function ( stageNo, userNo ) {
  start <- 1
  temp_dsm <- Users$dsm[Users$Stage %in% c(stageNo)]
  end <- length(temp_dsm)
  temp_index <- end/8

  if(userNo==1)
    start <- 1
  else
    start <- temp_index*(userNo-1)+1

  end <- temp_index*userNo
  temp_sum <- 0
  while ( start<=end ) {
    temp_sum <- temp_sum+temp_dsm[start]
    start <- start+1
  }
  return(temp_sum)
}
# getsoc is used to get State of Charge for each user at a given stage
getsoc <-  function( stageNo, userNo ) {
  start <- 1
  temp_soc <- Users$soc[ Users$Stage %in% c(stageNo) ]
  end <- length( temp_soc )
  temp_index <- end/8
  if ( userNo == 1 )
    start <- 1
  else
    start <- temp_index * ( userNo - 1 ) + 1
  end <- temp_index * userNo
  temp_sum <- 0
  while ( start <= end ) {
    temp_sum <- temp_sum + temp_soc[ start ]
    start <- start + 1
  }
  return( temp_sum )
}
# getSolar,getWind, getmaxCurrent is used to get Solar, Wind and maximum Current for each user at a  given stage
getSolar <-  function ( stageNo, userNo ) {
  start <- 1
  temp_solar <- Users$SolarPercent[Users$Stage %in% c(stageNo)]
  end <- length ( temp_solar )
  temp_index <- end/8
  if(userNo==1)
    start <- 1
  else
    start <- temp_index * ( userNo - 1 ) + 1
  end <- temp_index * userNo
  temp_sum <- 0
  while ( start <= end ) {
    temp_sum <- temp_sum + temp_solar[start]
    start <- start + 1
  }
  return( temp_sum )
}
getWind <-  function(stageNo, userNo) {
  start <- 1
  temp_wind <- Users$WindPercent[Users$Stage %in% c(stageNo)]
  end <- length ( temp_wind )
  temp_index <- end/8
  if(userNo==1)
    start <- 1
  else
    start <- temp_index * (userNo - 1) + 1
  end <- temp_index * userNo
  temp_sum <- 0
  while( start <= end ) {
    temp_sum <- temp_sum + temp_wind[start]
    start <- start + 1
  }
  return (temp_sum)
}
getmaxCurrent <- function(stageNo,userNo){
  start <- 1
  temp_curr <- Users$current[Users$Stage %in% c(stageNo)]
  end <- length(temp_curr)
  temp_index <- end/8
  if(userNo==1)
    start <- 1
  else
    start <- temp_index * (userNo - 1) + 1
  end <- temp_index * userNo
  temp_max_curr <- temp_curr[start]
  start <- start + 1
  while(start <= end) {
    if (temp_max_curr < temp_curr[start]) {
      temp_max_curr=temp_curr[start]
    }
    start <- start+1
  }
  return (temp_max_curr)

}

# i denotes the stage variable( 1 to 144 stages)
i <- 1
#var used: Contribution_User,Stages
#loop for all the stages
while(i <= 144){
  cat("i=",i,'\n')
  cat("No_of_Cars:",Stages$No_of_cars[i],"\n")
  #No_of_cars>0 => peak => Contributed with the least Contribution value in the group
  if(Stages$No_of_cars[i] >= 1) {
    # Find the sorted list of contribution vector
    temp_Contribution <- Contribution_User[i,]
    temp_Contribution <- sort(temp_Contribution)

    #looping until all the cars have contributed
    #Each stage takes around 20% of charging or discharging
    #If No_of_cars is +ve then cars are required to contribute to the grid
    loop_cars <- 1
    curr_index_adder <- 0
    while(loop_cars <= Stages$No_of_cars[i]) {
      NO_CONTRIBUTION <- FALSE
      #Calculating the current stage current and checking for the upper limit of 4A
      if(loop_cars + curr_index_adder == 9) {
        cat("Wrong index 1",'\n');
        NO_CONTRIBUTION <- TRUE
        break
      }
      temp_user <- as.numeric(substring(names(temp_Contribution[loop_cars+curr_index_adder]),5,5))
      cat("temp_user4: ",temp_user,'\n');
      #check for soc(state of charge)
      while(SOC[i,temp_user] < 20) {
        curr_index_adder <- curr_index_adder+1
        if(loop_cars+curr_index_adder == 9) {
          cat("Wrong index 2",'\n');
          NO_CONTRIBUTION <- TRUE
          break
        }
        temp_user <- as.numeric( substring (
        names( temp_Contribution [loop_cars + curr_index_adder] ), 5, 5 ) )
        cat("temp_user5: ",temp_user,'\n');

      }
      if( NO_CONTRIBUTION ) {
        cat("Wrong index 3",'\n');
        break
      }
      while( getmaxCurrent ( i, temp_user) > 4) {
        curr_index_adder <- curr_index_adder + 1
        if (loop_cars + curr_index_adder == 9) {
          cat("Wrong index 4",'\n');
          NO_CONTRIBUTION <- TRUE
          break
        }
        temp_user <- as.numeric( substring (
        names (temp_Contribution[loop_cars + curr_index_adder] ), 5, 5 ) )
        cat("temp_user6: ",temp_user,'\n');

      }
      if(NO_CONTRIBUTION) {
        cat("Wrong index 5",'\n');
        break
      }
      # Updating the value of Contrbution vector
      if(loop_cars + curr_index_adder == 9) {
        cat("Wrong index 6",'\n');
        NO_CONTRIBUTION <- TRUE
        break
      }
      temp_user <- as.numeric( substring (
      names(temp_Contribution[loop_cars + curr_index_adder]), 5, 5))
      cat("temp_user7: ",temp_user,'\n');

      Contribution_User[i,temp_user] <- Contribution_User[ i, temp_user] +
      ( 100 * Stages$Error[i] / max(Stages$Error) )+
      ( 100 * ( getdsm(i, temp_user) + getWind(i, temp_user) + getSolar(i, temp_user) ) )
      SOC[i, temp_user] <- Stages[i, temp_user] - 20
      Charge[i, temp_user] <- (-1)
      loop_cars <- loop_cars + 1
  }

}
# No_of_cars<0 => User with highest contribution factor can charge
else if(Stages$No_of_cars[i] <= -1) {
  # Find the sorted list of contribution vector
  NO_CHARGING <- FALSE
  temp_Contribution <- Contribution_User[i,]
  temp_Contribution <- sort(temp_Contribution, decreasing = TRUE)
  loop_cars <- 1
  curr_index_adder <- 0

  if (loop_cars + curr_index_adder == 9) {
    cat("Wrong index 7",'\n');
    NO_CHARGING <- TRUE
    break
  }
  temp_user <- as.numeric ( substring (
  names ( temp_Contribution[loop_cars+curr_index_adder] ),5 ,5 ))
  cat("temp_user1: ",temp_user,'\n');

  #soc is less than 80


  while ( SOC[i, temp_user] > 80) {
    curr_index_adder <- curr_index_adder + 1
    if ( loop_cars + curr_index_adder == 9 ) {
      cat("Wrong index 8",'\n');
      NO_CHARGING <- TRUE
      break
    }
    temp_user <- as.numeric ( substring (
    names ( temp_Contribution[loop_cars + curr_index_adder] ), 5, 5 ))
    cat("temp_user2: ",temp_user,'\n');

  }
  if(NO_CHARGING) {
    cat("Wrong index 9",'\n');
    break
  }
  while( loop_cars <= ( -1 * Stages$No_of_cars[i] ) ) {
    if ( loop_cars+curr_index_adder == 9 ) {
      cat("Wrong index 10",'\n');
      NO_CHARGING <- TRUE
      break
    }
    temp_user <- as.numeric ( substring (
    names ( temp_Contribution[loop_cars + curr_index_adder] ), 5, 5 ))
    cat("temp_user3: ",temp_user,'\n');
    Contribution_User[i, temp_user] <- Contribution_User[i, temp_user] -
    ( 100 * Stages$Error[i] / min(Stages$Error) ) +
    ( 100 * ( getdsm(i, temp_user) + getWind(i, temp_user) + getSolar(i, temp_user) ) )
    SOC [i, temp_user] <- SOC [i, temp_user] + 20
    Charge [i, temp_user] <- 1
    loop_cars <- loop_cars + 1
    }
  }
  i <- i + 1
}
syncCharge <- function(){
  i <- 1
  while(i <= 144) {
    temp <- user8$Charge[user8$Stage %in% c(i)]
    start <- 1
    end <- length(temp)
    temp <- Charge$user8charge[i]
    while(start <= end){
      user8$Charge[user8$Stage %in% c(i)][start] <- temp
      start <- start + 1
    }
    i <- i + 1
  }
}
