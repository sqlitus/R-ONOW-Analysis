# apriori test category data


# Support: (transactions that include lhs+rhs) / (all transactions)
# supp(xUy) = union of item lists X and Y. == P(Ex ∩ Ey) = probability of X and Y appearing together.
# e.g. 4 of 7 rows contain L1. support = 4/7. (.57)
# e.g. 4 of 7 rows contain Lamar. support = 4/7 (.57)
# e.g. 3 of 7 rows contain L1 AND Lamar. support = 3/7 (.42)


# Confidence: supp(xUy) / supp(x). AKA P(Y|X) "the probability of Y occurring, given X has occurred AKA The Conditional prob of Y.
# e.g. 4 rows contain L1, and 3 of those rows contain Printer. L1 => Printer is .75 confidence
# e.g. only 3 rows contain Printer, and all 3 contain L1. Printer => L1 is 1 confidence (100%)
# e.g. 3 rows contain L1 & Printer. 2 of those also contain Lamar. L1+Printer => Lamar is .66 confidence


# Lift: confidence(x=>y) / supp(Y)
# e.g. L1 => Lamar has 75% confidence, and Lamar happens .57 of the time anyways, so L1 is lifting Lamar a bit (1.33 times normal)


library(arules)
library(tidyverse)

z <- data_frame(id = c(1,2,3,4,5,6,7), team = c('L1', 'L1', 'L1', 'L2', 'L2', 'L2', 'L1'), 
                cat=c('Printer', 'Printer', 'Scanner', 'Scanner', 'Scanner', 'Tax', 'Printer'),
                store = c("Lamar", "Lamar", "Lamar", "Gateway", "Lamar", "Gateway", "Gateway"))
z
z$team <- as.factor(z$team)
z$cat <- as.factor(z$cat)
z$store <- as.factor(z$store)
trans <- z[,-1]

# analysis 1: default
rules <- apriori(data = trans,
                 parameter = list(support = .1, minlen = 1, maxlen = 10, ext = FALSE, confidence=.8, maxtime=5), # default 5 secs
                 appearance = NULL,
                 control = NULL)
rules <- apriori(data = trans)  # should be same as above
inspect(sort(rules, by = "lift")) %>% View()  # view only works on short dfs here

# analysis 2: show everything
rules <- apriori(data = trans,
                 parameter = list(minlen = 1, supp = 0, conf = 0, ext = TRUE)  # arem adds measure but cuts off results?
                 # appearance = list(lhs = c("team=L1", "cat=Printer", "store=Lamar"))
                 # , appearance = list(rhs = c("team=L1"))
                 )

# analysis 3: custom
rules <- apriori(trans, parameter = list(supp = .01, conf = 0, ext = TRUE))

# convert to dataframe
test <- data.frame(lhs = labels(lhs(rules)), rhs = labels(rhs(rules)), rules@quality) %>% 
  arrange(desc(confidence), desc(support)) 





# misc info functions
rules@lhs
rules@rhs
rules@quality
rules@info













# support: % of rows (transactions) w/ lhs => rhs relationship (the association))
# proportion of rows with the entire transaction (both sides)
# e.g. L1 is 4/7 of rows (transactions) (.57), but L1 => Lamar is 3/7 (.42)

# confidence: % of transactions w/ LHS+RHS (association) over just LHS. "this pair appears together instead of separate this % of time"
# P(XUY)
# "if I see lhs, this percent of time I will also see rhs" 
# note: RHS could just appear a lot with other stuff - confidence gives no indication of that. Not necessarily the cause, just the reliability.
# for single elements support = confidence
# e.g. printer is in 3/7 rows, L1 is 4/7 rows, and 3/4 of rows w/ L1 also have printer
# L1 => printer should be 


# e.g. scanner is in 3/7 rows, L1 is 4/7 rows, and 1/4 of rows that have L1, also have scanner.