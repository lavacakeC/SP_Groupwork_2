# n <- 50
# prisoner <- c(1:(2*n))
# card <- sample(prisoner)
# Find_or_not <- 0

# write the function "first_card" to determine the number of the card in the first box each prisoner opened when they enter the room 
# the inputs of function are: n(half of the total number of prisoners, cards and boxes), k(the prisoner's number), strategy(1 is that each prisoner first open the box corresponding to their prisoner number, if the card's number in the box is the prisoner's number, the prisoner succeeds. If the card's number is not the prisoner's number, open the next box corresponding to the card's number; 2 is similar with strategy 1, just the first box opened randomly; 3 is all the boxes opened randomly), card(the boxes'number)
# the output "card_find" is first card's number in the box they first choose
# use "if...else if..." conditional statement to assign value to "card_find", using sample funtion to select 1 number among 2n randomly
first_card <- function(n, k, strategy, card) {
  if (strategy == 1) {
    card_find <- card[k]
  }
  else if (strategy == 2) {
    card_find <- card[sample(c(1:(2*n)), 1)]
  }
  return(card_find)
}

# write the function "strategy_1_2" to determine whether the prisoner go free or not under the strategy 1 and 2
# the inputs of function are: n,k,card(same as function "first_card"); card_find(the output of the function "first_card")
# the output of the function is TRUE of FALSE, which means the prisoner go free or not
# using for loop to implement that each time prisoner try, the prisoner open the box which number is corresponding to the card's number they see in the last box, if statement is to implement when the prisoner find the number in the card corresponding their own number, the output is TRUE, otherwise the output is FALSE
strategy_1_2 <- function(n, k, card, card_find) {
  Find_or_not <- FALSE
  
  for (prisoner_try in 1:n) {
    if (card_find == k) {
      Find_or_not <- TRUE
      break
    }
    card_find <- card[card_find]
  }
  return(Find_or_not)
}

# write a function "strategy_3" simulation strategy 3 that the prisoner randomly chooses to open the box n times.
# the inputs of this funtion are n(the number of times can open the box, half number of total prisoners), k(prisoner's number), card(the box's number)
# the ouput is value 0 or 1, 1 means succeddfuly find a card number in box that mach prisoner's number. 0 means did't find number in the box that mach prisoner's number
# at this funtion, using sample function to randomly assign n numbers from 1 to 2n to simulate the value of opening n box each time. When one of the numbers is the same as the prisoner's number(k), the value corresponding to that array position becomes TRUE. Otherwise the value becomes FALSE. Then useing sum funtion to calculate whether there have TRUE value in the array.
strategy_3 <- function(n, k, card) {
  return(sum(k == card[sample(c(1:(2*n)), n)]))
}

# write the function "Pone" to eatimate the probability of a single prisoner go free successfully under the 3 strategies
# the inputs of function are: n,k,strategy(same as above), nreps(the number of replicate similations to run)
# the output of function is the probability of a single prisoner success under the 3 strategies
# the number of 2n prisoners is assigned successively, and then the escape_num value is set to 0 to calculate the number of successful escapes. for loop is used to simulate the nreps experiment. 
# in each experiment, the card variable was randomly assigned from the prisoner variable. When using strategy 1 or 2, use the first_card function to extract the number of the card each time the box is opened and assign that number to card_find. The strategy_1_2 function is then called to determine if the card number is equal to the prisoner and return TRUE or FALSE. If strategy 3 is used, strategy_3 function is directly called. 
# The escape_num is added to the return value of the different strategy functions to calculate the prisoner's success in finding the box containing his number. The probability of success was calculated by dividing the number of successes by the total number of loop simulations
Pone <- function(n, k, strategy, nreps) {
  
  prisoner <- c(1:(2*n))
  escape_num <- 0
  
  for (rep in 1:nreps) {
    
    card <- sample(prisoner)
    
    if (strategy == 1 | strategy == 2) {
      card_find <- first_card(n, k, strategy, card)
      escape_num <- escape_num + strategy_1_2(n, k, card, card_find)
    }
    
    else if (strategy == 3) {
      escape_num <- escape_num + strategy_3(n, k, card)
    }
    
  }
  
  return(escape_num/nreps)
  
}

# when n=5, which means there are 10 prisoners, estimate the probability of a single prisoner go free successfully under the 3 strategies
Pone(5, 1, 1, 10000)
Pone(5, 1, 2, 10000)
Pone(5, 1, 3, 10000)
system.time(Pone(5, 1, 1, 10000))
system.time(Pone(5, 1, 2, 10000))
system.time(Pone(5, 1, 3, 10000))

# when n=50, which means there are 100 prisoners, estimate the probability of a single prisoner go free successfully under the 3 strategies
Pone(50, 1, 1, 10000)
Pone(50, 1, 2, 10000)
Pone(50, 1, 3, 10000)
system.time(Pone(50, 1, 1, 10000))
system.time(Pone(50, 1, 2, 10000))
system.time(Pone(50, 1, 3, 10000))

# write the function "Pall" to eatimate the probability of all prisoners go free successfully under the 3 strategies
# the inputs of the function are n, strategy and nreps
# the output of function is the probability of all prisoners success under the 3 strategies
# The main idea of Pall function is very similar to that of Pone function. The main difference is that another for loop is embedded in the for loop to simulate that all prisoners enter the room in one of experiments. If one of the prisoners fails in each experiment, the strategy function referenced will return FALSE. 
# Use the if statement to determine when (! If find_or_not) is TRUE (meaning find_or_not is false), the condition fires, using a break statement to jump out of the current loop and making no changes to escape_num. After 2n cycles, determine whether escape_num is equal to 2n (if it is equal to 2n, every prisoner has successfully escaped). This judgment returns true or false, counts the number of successes using success_escape, and returns its success probability
Pall <- function(n, strategy, nreps) {
  prisoner <- c(1:(2*n))
  success_escape <- 0
  
  for (rep in 1:nreps) {
    
    card <- sample(prisoner)
    escape_num <- 0
    
    for (k in 1:(2*n)) {
      
      if (strategy == 1 | strategy == 2) {
        card_find <- first_card(n, k, strategy, card)
        Find_or_not <- strategy_1_2(n, k, card, card_find)
        if (!Find_or_not) {break}
        escape_num <- escape_num + 1
      }
      
      else if (strategy == 3) {
        Find_or_not <- strategy_3(n, k, card)
        if (!Find_or_not) {break}
        escape_num <- escape_num + 1
      }
    }
    success_escape <- success_escape + (escape_num == (2*n))
    
  }
  return(success_escape/nreps)
  
}

# when n=5, which means there are 10 prisoners, estimate the probability of all prisoners go free successfully under the 3 strategies
Pall(5, 1, 10000)
Pall(5, 2, 10000)
Pall(5, 3, 10000)
system.time(Pall(5, 1, 10000))
system.time(Pall(5, 2, 10000))
system.time(Pall(5, 3, 10000))

# when n=50, which means there are 100 prisoners, estimate the probability of all prisoners go free successfully under the 3 strategies
Pall(50, 1, 10000)
Pall(50, 2, 10000)
Pall(50, 3, 10000)
system.time(Pall(50, 1, 10000))
system.time(Pall(50, 2, 10000))
system.time(Pall(50, 3, 10000))

# remarks: when the number of prisoners is large enough, the probability of all escaping should be about zero, for example, when n = 50, the probability of all success should be (1/2)^100, however, when we use strategy 1, the probability of success becomes about 30%.

dloop <- function(n, nreps) {
  occur <- vector(length = 2*n)
  box <- c(1:(2*n))
  
  for (try in 1:nreps) {
    card <- sample(box)
    tmp <- vector(length = 2*n)
    for (k in 1:(2*n)) {
      card_find <- card[k]
      for (try_k in 1:(2*n)) {
        if(card_find == k) {
          tmp[try_k] <- 1
          break
        }
        card_find <- card[card_find]
      } 
      
    }
    occur <- occur + tmp
  }
  prob <- occur/nreps
  return(prob)
}
test <- dloop(50,10000)
sum(test)
sum(test[1:50])
class(test)
barplot(test)
sum(dloop(50,10000))
system.time(dloop(50, 10000))

sum(test[1:50])/(sum(test))

#Q6
no_loop_l_50 <- 0
for (i in 1:10000) {
  tmp <- dloop(50,1)
  no_loop_l_50 <- no_loop_l_50 + (sum(tmp[51:100]) == 0)
}
no_loop_l_50




test <- c(1,23,3)
test1 <- c(2,3,1)
test2 <- vector(length = 3)
test+test1+test2
class(as.vector(test2+test1))