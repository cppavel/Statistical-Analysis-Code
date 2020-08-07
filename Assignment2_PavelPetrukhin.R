#here we initialize variables to count the number of times a person won 
#when they decided to stay on their choice or switch for the other door respectively
n_stay <- 0
n_switch <-0
#initialising a counter for a player, who in the each game randomly chooses to stay or
#switch
n_randomly <-0
#here we have a for loop, so we can run the game 100 times
for(i in 1:100) {
  #this is an array which represents diffent doors
  door <- c(1,2,3)
  #here we choose a random door, behind which we put a car
  cardoor <-sample(door,1)
  #here we make a random player's choice
  choice <-sample(door,1)
  #this is an array which represents two doors, which have goats behind them
  goatdoors<-setdiff(door,cardoor)
  #here we find all the doors the TV host can open
  #if the player initially chose the cardoor, there are two possible choices for the host
  #otherwise there is only one door he can reveal
  reveal_options<-setdiff(goatdoors,choice)
  #if player chose a cardoor, we randomly get the host's choice for the door to reveal
  #otherwise there is only one option, so we just assign it to his choice (reveal variable)
  if(choice==cardoor)
  {
    reveal<-sample(reveal_options,1)
  }else
  {
    reveal<-reveal_options
  }
  #now we find the two doors, which are still closed
  remaining_doors<-setdiff(door,reveal)
  #here we get the door, which represents the player's decision to change his initial choice
  newchoice <- setdiff(remaining_doors,choice)
  #here we get the random choice to stay or not by creating a vector from variables
  #choice and newchoice and sampling it randomly with 1 sample
  possiblechoices <-c(choice,newchoice)
  randomchoice <-sample(possiblechoices,1)
  #if the initial choice is winning, add one to the counter of wins, when player decided to 
  #stay. Similarly, if switch choice is winnnig, add one to the counter of wins, when player
  #decide to change his decision. Finally, we do the same for the random choice
  
  if(choice==cardoor)
  {
    #incrementing n_stay
    n_stay <- n_stay + 1
  }
  if (newchoice == cardoor) 
  {
    #incrementing n_switch
    n_switch <- n_switch +1
  }
  if(randomchoice == cardoor)
  {
    #incrementing n_randomly_stay_switch
    n_randomly <- n_randomly + 1
  }
}
#print the counters after 100 games
#number of wins when player did not change his mind
print(n_stay)
#number of wins when player changed his mind
print(n_switch)
#number of wins when player randomly chose to stay or not
print(n_randomly)

