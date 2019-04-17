install.packages("arules")
setwd("C:/Users/meetr/Desktop/Fall 2018/Data Science/Market Basket Analysis")
#1.	Load the data into R.
student_courses <- read.csv("C:/Users/meetr/Desktop/Fall 2018/Data Science/Market Basket Analysis/market_basket.csv")

View(student_courses)
library(arules)

student_courses$student_id = discretize(student_courses$student_id)
#student_courses$course = discretize(student_courses$course)

#2.	Perform an affinity analysis using support of 0.02 and confidence of 0.3.
t<- read.transactions("market_basket.csv", 
                      rm.duplicates= TRUE, sep=",",
                      format = "single", cols = c(1,2));


inspect(t)
itemFrequencyPlot(t, type="absolute")
rules <- apriori(t, parameter = list(supp = 0.02, conf = 0.3))

#3.	How many rules did the system generate?
#ANS: 40 rules were genereated by system
summary(rules)

'''
Output:

set of 40 rules

rule length distribution (lhs + rhs):sizes
2  3 
34  6 

Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
2.00    2.00    2.00    2.15    2.00    3.00 

summary of quality measures:
support          confidence          lift            count      
Min.   :0.02066   Min.   :0.3014   Min.   : 3.850   Min.   :34.00  
1st Qu.:0.02248   1st Qu.:0.3636   1st Qu.: 4.621   1st Qu.:37.00  
Median :0.02673   Median :0.4568   Median : 5.687   Median :44.00  
Mean   :0.02883   Mean   :0.4911   Mean   : 8.628   Mean   :47.45  
3rd Qu.:0.03326   3rd Qu.:0.5608   3rd Qu.: 6.291   3rd Qu.:54.75  
Max.   :0.04131   Max.   :0.9737   Max.   :41.094   Max.   :68.00  

mining info:
data ntransactions support confidence
t          1646    0.02        0.3
'''


inspect(rules)

#4.	Sort the display by descending order of lift and display the top 5 rules.
rules <- sort(rules, by="lift", decreasing=TRUE)
inspect(rules)
topRules <- rules[1:5]
inspect(topRules)
'''
output:
lhs        rhs     support    confidence lift     count
[1] {AC113} => {AC102} 0.02247874 0.9736842  41.09447 37   
[2] {AC102} => {AC113} 0.02247874 0.9487179  41.09447 37   
[3] {AC111} => {AC115} 0.02187120 0.8181818  22.07750 36   
[4] {AC115} => {AC111} 0.02187120 0.5901639  22.07750 36   
[5] {FI211} => {FI227} 0.02308627 0.6785714  18.93099 38   
'''

#5.	If a student has taken AC104, which course (just one) would you recommend to this student? If you had to recommend two, which two would they be? 
#ANS: I would recommend QA812 for student who has taken AC104. If I had to suggest two courses, I would suggest QA812 and MK601

suggest_rules <- apriori(t, parameter = list(supp = 0.02, conf = 0.3),appearance = list(default="rhs",lhs="AC104"))
inspect(suggest_rules)


suggest_rules <- sort(suggest_rules, by="confidence", decreasing=TRUE)
inspect(suggest_rules)

'''
output:

lhs        rhs     support    confidence lift     count
[1] {AC104} => {QA812} 0.04131227 0.42500    5.687398 68   
[2] {AC104} => {MK601} 0.03827461 0.39375    4.439127 63   
[3] {AC104} => {FI201} 0.03766707 0.38750    4.588669 62   
[4] {AC104} => {MG501} 0.03159174 0.32500    5.143750 52 
'''
