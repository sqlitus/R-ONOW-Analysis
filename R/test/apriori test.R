# apriori test category data

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
rules <- apriori(trans)
inspect(rules)
inspect(sort(rules, by = "lift"))
inspect(sort(rules, by = "lift")) %>% View()
as.data.frame(inspect(sort(rules, by = "lift"))) %>% str()

# analysis 2: show everything
rules <- apriori(data = trans,
                 parameter = list(minlen = 1, supp = 0, conf = 0),
                 appearance = list(lhs = "team=L1"),
                 # appearance = list(rhs = c("team=L1"), default = "lhs"),
                 control = list(verbose=F)
                 )
inspect(sort(rules, by = "support")) %>% View()
rules.sorted <- sort(rules, by = "lift")
inspect(rules.sorted)

# analysis: show everything for store = ...
# analysis: show stuff with 

# support: % of rows (transactions) w/ lhs => rhs relationship (the association))
# e.g. L1 is 4/7 of rows (transactions) (.57), but L1 => Lamar is 3/7 (.42)

# confidence: % of transactions w/ LHS+RHS (association) over just LHS. "this pair appears together instead of separate this % of time"
# "if I see lhs, this percent of time I will also see rhs" 
# note: RHS could just appear a lot with other stuff - confidence gives no indication of that. Not necessarily the cause, just the reliability.
# for single elements support = confidence
# e.g. printer is in 3/7 rows, L1 is 4/7 rows, and 3/4 of rows w/ L1 also have printer
# e.g. scanner is in 3/7 rows, L1 is 4/7 rows, and 1/4 of rows that have L1, also have scanner.