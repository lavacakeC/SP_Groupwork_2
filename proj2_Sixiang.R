# n <- 50
# prisoner <- c(1:(2*n))
# card <- sample(prisoner)
# Find_or_not <- 0

first_card <- function(n, k, strategy, card) {
  if (strategy == 1) {
    card_find <- card[k]
  }
  else if (strategy == 2) {
    card_find <- card[sample(c(1:(2*n)), 1)]
  }
  return(card_find)
}

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

strategy_3 <- function(n, k, card) {
  return(sum(k == card[sample(c(1:(2*n)), n)]))
}


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

Pone(50, 1, 1, 10000)
Pone(50, 1, 2, 10000)
Pone(50, 1, 3, 10000)
Pone(5, 1, 1, 10000)
Pone(5, 1, 2, 10000)
Pone(5, 1, 3, 10000)
system.time(Pone(50, 1, 1, 10000))
system.time(Pone(50, 1, 2, 10000))
system.time(Pone(50, 1, 3, 10000))

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

Pall(50, 1, 10000)
Pall(50, 2, 10000)
Pall(50, 3, 10000)
Pall(5, 1, 10000)
Pall(5, 2, 10000)
Pall(5, 3, 10000)
system.time(Pall(50, 1, 10000))
system.time(Pall(50, 2, 10000))
system.time(Pall(50, 3, 10000))

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
largest <- vector(length = 100)

for (i in 1:10000) {
  tmp <- dloop(50,1)
  index <- c(1:100)
  # no_loop_l_50 <- no_loop_l_50 + (sum(tmp[51:100]) == 0)
  index <- index[tmp == 1]
  largest[max(index)] <- largest[max(index)] + 1
}
no_loop_l_50
test <- largest/10000

barplot(test)
sum(test)




test <- c(1,23,3)
test1 <- c(2,3,1)
test2 <- vector(length = 3)
test+test1+test2
class(as.vector(test2+test1))
# 
# 
# Pone <- function(n, k, strategy, nrep) {
#   prisoner <- c(1:(2*n))
#   sum <- 0
#   for (num in 1:nrep) {
#     card <- sample(prisoner)
#     Find_or_not <- 0
#     
#     if (strategy == 3) {
#       for (i in 1:(2*n)) {
#         tmp <- sum(i == card[sample(c(1:(2*n)), n)])
#         Find_or_not <- Find_or_not + tmp
#         
#         # card_find <- card[sample(c(1:(2*n)), n)]
#         # card_find <- card[sample(prisoner, n)]
#         # for (j in 1:n) {
#         #   if (card_find[j] == i) {
#         #     Find_or_not = Find_or_not + 1
#         #     break
#         #   }
#         # }
#       }
#       
#       
#     }
#     
#     else {
#       for(i in 1:(2*n)) {
#         
#         if (strategy == 1) {
#           card_find <- card[i]#1、2策略的区别在这里改
#         }
#         else if (strategy == 2) {
#           card_find <- card[sample(c(1:(2*n)), 1)]
#         }
# 
#         for (j in 1:n) {
# 
#           if (card_find == i) {
#             Find_or_not = Find_or_not + 1
#             break
#           }
#           card_find <- card[card_find]
# 
#         }
#       }
#     }
#     
#     sum <- sum + Find_or_not
#   }
# 
#   prob <- sum / (nrep*100)
#   return(prob)
# }
# 
# Pone(50,3,10000)
# system.time(Pone(50,1,1000))
# system.time(Pone(50,2,1000))
# system.time(Pone(50,3,1000))
# 
# 
# system.time(test1())
# system.time(test2())


