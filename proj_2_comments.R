##### Group member: 
# Sixiang Cheng, s2437109; Ruishuo Cheng, s2305931; Qinxuan Li, s2299101

# github repo's address: https://github.com/lavacakeC/SP_Groupwork_2.git

# The three members of the group reviewed the topic, understood the meaning of the topic, and collected information online. Then they wrote Pone and Pall functions together and calculated the relevant results. Together, they discussed the surprising aspects of this problem. Cheng Sixiang completed the dloop function according to the ideas of the first two questions and calculated the results with examples. Finally, the three people discussed and completed how to visualize the probability of success
# Ruishuo and Qinxuan each undertake about 30% of the work,Sixiang undertake about 40% of the work


# write the function "first_card" to determine the number of the card in the first box each prisoner opened when they enter the room 
# the inputs of function are: n(half of the total number of prisoners, cards and boxes), k(the prisoner's number), strategy(1 is that each prisoner first open the box corresponding to their prisoner number, if the card's number in the box is the prisoner's number, the prisoner succeeds. If the card's number is not the prisoner's number, open the next box corresponding to the card's number; 2 is similar with strategy 1, just the first box opened randomly; 3 is all the boxes opened randomly), card(the boxes'number)
# the output "card_find" is first card's number in the box they first choose
first_card <- function(n, k, strategy, card) {
  # use "if...else if..." conditional statement to assign value to "card_find"
  if (strategy == 1) {
    card_find <- card[k]
  }
  else if (strategy == 2) {
    # using sample funtion to select 1 number among 2n randomly
    card_find <- card[sample(c(1:(2*n)), 1)]
  }
  return(card_find)
}

# write the function "strategy_1_2" to determine whether the prisoner go free or not under the strategy 1 and 2
# the inputs of function are: n,k,card(same as function "first_card"); card_find(the output of the function "first_card")
# the output of the function is TRUE of FALSE, which means the prisoner go free or not
strategy_1_2 <- function(n, k, card, card_find) {
  Find_or_not <- FALSE
  # using for loop to implement that each time prisoner try, the prisoner open the box which number is corresponding to the card's number they see in the last box
  for (prisoner_try in 1:n) {
    # if statement is to implement when the prisoner find the number in the card corresponding their own number
    if (card_find == k) {
      # when if statement execution,Find_or_not changes to TRUE, then use break statement to jump out of the loop
      Find_or_not <- TRUE
      break
    }
    # when if statement not execution, the variable card_find reloads the value of card[card_find] 
    card_find <- card[card_find]
  }
  return(Find_or_not)
}

# write a function "strategy_3" simulation strategy 3 that the prisoner randomly chooses to open the box n times.
# the inputs of this funtion are n(the number of times can open the box, half number of total prisoners), k(prisoner's number), card(the box's number)
# the ouput is value 0 or 1, 1 means succeddfuly find a card number in box that mach prisoner's number. 0 means did't find number in the box that mach prisoner's number
# at this funtion, using sample function to randomly assign n numbers from 1 to 2n to simulate the value of opening n box each time. When one of the numbers is the same as the prisoner's number(k), the value corresponding to that array position becomes TRUE. Otherwise the value becomes FALSE. Then useing sum funtion to calculate whether there have TRUE value in the array
strategy_3 <- function(n, k, card) {
  return(sum(k == card[sample(c(1:(2*n)), n)]))
}

# write the function "Pone" to eatimate the probability of a single prisoner go free successfully under the 3 strategies
# the inputs of function are: n,k,strategy(same as above), nreps(the number of replicate similations to run)
# the output of function is the probability of a single prisoner success under the 3 strategies
Pone <- function(n, k, strategy, nreps = 10000) {
  # the number of 2n prisoners is assigned successively, and then the escape_num value is set to 0 to calculate the number of successful escapes
  prisoner <- c(1:(2*n))
  escape_num <- 0
  
  # for loop is used to simulate the nreps experiment
  for (rep in 1:nreps) {
    # in each experiment, the card variable was randomly assigned from the prisoner variable
    card <- sample(prisoner)
    # When using strategy 1 or 2, use the first_card function to extract the number of the card each time the box is opened and assign that number to card_find
    if (strategy == 1 | strategy == 2) {
      card_find <- first_card(n, k, strategy, card)
      # The strategy_1_2 function is then called to determine if the card number is equal to the prisoner and return TRUE or FALSE. The escape_num is added to the return value of the different strategy functions to calculate the prisoner's success in finding the box containing his number
      escape_num <- escape_num + strategy_1_2(n, k, card, card_find)
    }
    # If strategy 3 is used, strategy_3 function is directly called
    else if (strategy == 3) {
      escape_num <- escape_num + strategy_3(n, k, card)
    }
    
  }
  # The probability of success was calculated by dividing the number of successes by the total number of loop simulations
  return(escape_num/nreps)
  
}

# test when n=5, which means there are 10 prisoners, estimate the probability of a single prisoner go free successfully under the 3 strategies
Pone(5, 1, 1, 10000)
Pone(5, 1, 2, 10000)
Pone(5, 1, 3, 10000)

# test when n=50, which means there are 100 prisoners, estimate the probability of a single prisoner go free successfully under the 3 strategies
Pone(50, 1, 1, 10000)
Pone(50, 1, 2, 10000)
Pone(50, 1, 3, 10000)

# write the function "Pall" to eatimate the probability of all prisoners go free successfully under the 3 strategies
# the inputs of the function are n, strategy and nreps
# the output of function is the probability of all prisoners success under the 3 strategies
# The main idea of Pall function is very similar to that of Pone function
Pall <- function(n, strategy, nreps = 10000) {
  prisoner <- c(1:(2*n))
  success_escape <- 0
  
  for (rep in 1:nreps) {
    
    card <- sample(prisoner)
    escape_num <- 0
    
    # The main difference is that another for loop is embedded in the for loop to simulate that all prisoners enter the room in one of experiments. If one of the prisoners fails in each experiment, the strategy function referenced will return FALSE.
    for (k in 1:(2*n)) {
      
      if (strategy == 1 | strategy == 2) {
        card_find <- first_card(n, k, strategy, card)
        Find_or_not <- strategy_1_2(n, k, card, card_find)
        # Use the if statement to determine when (! If find_or_not) is TRUE (meaning find_or_not is false), the condition fires, using a break statement to jump out of the current loop and making no changes to escape_num
        if (!Find_or_not) {break}
        escape_num <- escape_num + 1
      }
      
      else if (strategy == 3) {
        Find_or_not <- strategy_3(n, k, card)
        if (!Find_or_not) {break}
        escape_num <- escape_num + 1
      }
    }
    # After 2n cycles, determine whether escape_num is equal to 2n (if it is equal to 2n, every prisoner has successfully escaped). This judgment returns true or false, counts the number of successes using success_escape, and returns its success probability
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

# Write a dloop function to calculate and record the probability of each loop length
# the inputs are n and nreps
# the outputs is prob(2n-vector of probabilities)
dloop <- function(n, nreps) {
  
  # First define variable "occur" that create a vector that length is 2n, then create a  array "box" that length is 2n
  occur <- vector(length = 2*n)
  box <- c(1:(2*n))
  
  # In the nreps' loop, we randomly assign the box value to card in each loop, and then create vector tmp to count the number of times each loop length occurs
  for (try in 1:nreps) {
    card <- sample(box)
    tmp <- vector(length = 2*n)
    # Loop k from 1 to 2n, assigning card_find the KTH value of card on each loop.
    for (k in 1:(2*n)) {
      card_find <- card[k]
      # try_k is placed in 2n loops to locate the length of each loop length when the prisoner successfully finds his number card
      for (try_k in 1:(2*n)) {
        # When successful, tmp is added by 1 and the loop is broken out
        if(card_find == k) {
          tmp[try_k] <- 1
          break
        }
        # If it is not found successfully, it reassigns card_find and loops again
        card_find <- card[card_find]
      } 
      
    }
    # Use the occur cycle to record the number of successful nreps, and finally return the probability of success 
    occur <- occur + tmp
  }
  # Calculate the probability
  prob <- occur/nreps
  return(prob)
}

# This part we are trying to using dloop function to calculate the number of occurrences of the longest loop length when n equals 50 and calculate it into probability after loop 10000 times. Finally, the cumulative probability is plotted. We used different ideas to visualize. Finally, we chose a plot that best explains probability. The codes of other plots are attached at the end for reference
# set the vector which is used to contain the largest loop length of each experiment and sum them.
largest <- vector(length = 100)

#try 10000 times
for (i in 1:10000) {
  #use dloop to simulate if or not the length of a certain loop occuring at least once in one experiment.
  tmp <- dloop(50,1)
  #Find the number of loop length occuring in the above dloop function.
  index <- c(1:100)
  index <- index[tmp == 1]
  #Find the largest loop length and accumulate it into the largest vector.
  largest[max(index)] <- largest[max(index)] + 1
}

#calculate the probabilities of each loop length o be the largest loop length in an experiment.
prob_largest_onetime <- largest/10000
#calculate the cumulative probabilities of each loop length to be the largest loop length in an experiment.
prob_largest_onetime_cumulative <- vector(length = 100)
for (i in 1:100) {
  prob_largest_onetime_cumulative[i] <- sum(prob_largest_onetime[1:i])
}

#plot the cumulative probabilities
plot(prob_largest_onetime_cumulative, type = "l"
     , xaxs = "i"
     , yaxs = "i"
     , xlab = "Length of the longest loop in an experiment"
     , ylab = "Probability"
     , main = "The cumulative probability of the \nloop length no longer than x\n(from 1 to 100)")
lines(c(50, 50), c(0, prob_largest_onetime_cumulative[50]), col = "blue")
lines(c(0, 50), c(prob_largest_onetime_cumulative[50], prob_largest_onetime_cumulative[50]), col = "blue")
axis(2, prob_largest_onetime_cumulative[50], prob_largest_onetime_cumulative[50]
     , col = "blue"
     , tck = -0.15
     , line = 1)

#color the region (length <=50)
region_x <- c(1, 1:50, 50)
region_y <- c(0, prob_largest_onetime_cumulative[1:50], 0)
polygon(region_x, region_y, density = -1, col = "yellow")


############################################################################################################################
### Other way of visualising the probabilities ###
# We think the previous plot is enough to visualising the probabilities that there is 
# no loop longer than 50 in a random reshuffling of cards to boxes. 
# The plots below are different ways of showing that probabilities. 
# If the previous plot makes sense, you could stop here and not use the code below

prob_no_longer_50 <- sum(largest[1:50])/10000
prob_of_each_length <- dloop(50,10000)

color1 <- c(rgb(255,220,126,100, maxColorValue = 255), rgb(158,148,182,100, maxColorValue = 255))
color2 <- c(rep(rgb(254,212,152,200, maxColorValue = 255),50), rep(rgb(158,148,182,200, maxColorValue = 255), 50))

par(mfcol = c(1, 4))
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
