
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
Pone <- function(n, k, strategy, nreps = 10000) {
  
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

# when n=50, which means there are 100 prisoners, estimate the probability of a single prisoner go free successfully under the 3 strategies
Pone(50, 1, 1, 10000)
Pone(50, 1, 2, 10000)
Pone(50, 1, 3, 10000)

# write the function "Pall" to eatimate the probability of all prisoners go free successfully under the 3 strategies
# the inputs of the function are n, strategy and nreps
# the output of function is the probability of all prisoners success under the 3 strategies
# The main idea of Pall function is very similar to that of Pone function. The main difference is that another for loop is embedded in the for loop to simulate that all prisoners enter the room in one of experiments. If one of the prisoners fails in each experiment, the strategy function referenced will return FALSE. 
# Use the if statement to determine when (! If find_or_not) is TRUE (meaning find_or_not is false), the condition fires, using a break statement to jump out of the current loop and making no changes to escape_num. After 2n cycles, determine whether escape_num is equal to 2n (if it is equal to 2n, every prisoner has successfully escaped). This judgment returns true or false, counts the number of successes using success_escape, and returns its success probability
Pall <- function(n, strategy, nreps = 10000) {
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

# when n=50, which means there are 100 prisoners, estimate the probability of all prisoners go free successfully under the 3 strategies
Pall(50, 1, 10000)
Pall(50, 2, 10000)
Pall(50, 3, 10000)

# remarks: when the number of prisoners is large enough, the probability of all escaping should be about zero, for example, when n = 50, the probability of all success should be (1/2)^100, however, when we use strategy 1, the probability of success becomes about 30%.
# remarks: using the Pone function, our results showe that strategy 2 have a success rate of nearly 40%, Strategy 1 and strategy 3 both have a success rate of about 50%; In the Pall function, strategies 2 and 3 have an extremely small probability of success only when n is small, and the success rate approaches zero when n becomes large. However, the success rate of strategy 1 is independent of the size of n, which is the most surprising thing
# remarks: our understanding is that strategy 1 is for the whole population, so the number of individuals in the whole population does not affect the final output, but the strategy 2 and 3 calculate the probability of the whole population on an individual basis, so the number of individuals in the whole population will affect the final output

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

#Q6
no_loop_l_50 <- 0
largest <- vector(length = 100)
prob_of_each_length <- dloop(50,10000)

for (i in 1:10000) {
  tmp <- dloop(50,1)
  index <- c(1:100)
  no_loop_l_50 <- no_loop_l_50 + (sum(tmp[51:100]) == 0)
  index <- index[tmp == 1]
  largest[max(index)] <- largest[max(index)] + 1
}

prob_no_longer_50 <- no_loop_l_50/10000
prob_largest_onetime <- largest/10000
# names(prob_largest_onetime) <- c(0:99)

color1 <- c(rgb(255,220,126,100, maxColorValue = 255), rgb(158,148,182,100, maxColorValue = 255))
color2 <- c(rep(rgb(254,212,152,200, maxColorValue = 255),50), rep(rgb(158,148,182,200, maxColorValue = 255), 50))

par(mfcol = c(1,4))
barplot(c(prob_no_longer_50, 1- prob_no_longer_50), space = 0
        , col = color1, xaxs="i"
        , xlab = "Length of the loop \nlonger than 50 or not"
        , ylab = "Probability"
        , main = "The probability of no loop\n longer than 50 or not")
axis(1, c(0,1,2), c(0,50,100))

barplot(c(prob_no_longer_50, 1- prob_no_longer_50), space = 0, col = color1, xaxs="i")
axis(1, c(0,1,2), c(0,50,100))
par(new = TRUE)
barplot(prob_largest_onetime, col = color2, xaxs = "i", yaxt = "n", xaxt = "n")
axis(4, seq(0,0.025,0.01), seq(0,0.025,0.01))

plot(c(1:100), xaxs = "i", type = "n", xlab = "", ylab = "", yaxt = "n", xaxt = "n"
     , main = "The probability of the \nlargest loop in an experiment")
axis(1, seq(0,100,50), seq(0,100,50))
par(new = TRUE)
barplot(prob_largest_onetime, col = color2, xaxs = "i", yaxt = "n"
        , xlab = "Length of the \nlongest loop in one experiment")
axis(4, seq(0,0.025,0.01), seq(0,0.025,0.01))

plot(c(1:100), xaxs = "i", type = "n", xlab = "", ylab = "", yaxt = "n", xaxt = "n")
axis(1, seq(0,110,10), seq(0,110,10))
par(new = TRUE)
barplot(prob_of_each_length, xaxs = "i", xlab = "Length of the loop"
        , col = rgb(254,212,152,200, maxColorValue = 255)
        , main = "The probability of each loop length occurring \nat least once in an experiment")


