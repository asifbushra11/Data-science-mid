
install.packages("dplyr")
library(dplyr)
library(ggplot2)



mydata<-read.csv("C:/R/Midterm_Dataset_Section(A).csv",header=TRUE,sep=",")
print(mydata)






colSums(is.na(mydata) | mydata == "" )
summary(mydata)

missing_data <- data.frame(
  variable = names(mydata),
  missing_count = sapply(mydata, function(x) sum(is.na(x) | x == ""))
)


ggplot(missing_data, aes(x = reorder(variable, -missing_count), y = missing_count)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  coord_flip() +
  labs(title = "Missing (NA or Empty String) Values per Variable",
       x = "Variables", y = "Number of Missing Values")





mean_age <- mean(mydata$person_age, na.rm = TRUE)
mean_age <-as.integer(round(mean_age))
mydata$person_age[is.na(mydata$person_age)] <- mean_age


mode_gender <- names(sort(table(mydata$person_gender),decreasing = TRUE))[1]
mydata$person_gender[mydata$person_gender == ""] <- mode_gender


mode_education <- names(sort(table(mydata$person_education),decreasing = TRUE))[1]
mydata$person_education[mydata$person_education == ""] <- mode_education


mean_income <- mean(mydata$person_income, na.rm = TRUE)
mean_income <-as.integer(round(mean_income))
mydata$person_income[is.na(mydata$person_income)] <- mean_income



unique(mydata$loan_status)
mode_loan_status <- names(sort(table(mydata$loan_status),decreasing = TRUE))[1]
mydata$loan_status[is.na(mydata$loan_status)] <-mode_loan_status

colSums(is.na(mydata) | mydata == "" )




boxplot(mydata$person_age)
quantile(mydata$person_age)


x <- mydata$person_age
x

iqr <- IQR(x)
iqr

lower_bound_x <-  22 -  1.5 * iqr
upper_bound_x <-  25 + 1.5 * iqr

x[x < lower_bound_x | x > upper_bound_x] <- mean(x)
x <- as.integer(round(x))
mydata$person_age<- x
boxplot(mydata$person_age)



boxplot(mydata$person_emp_exp)
quantile(mydata$person_emp_exp)
exp <- mydata$person_emp_exp
exp
iqr_exp <- IQR(exp)
iqr_exp

lower_bound_exp <-  0 -  1.5 * iqr_exp
upper_bound_exp <-  3 + 1.5 * iqr_exp

exp[exp < lower_bound_exp | exp > upper_bound_exp] <- mean(exp)
exp <- as.integer(round(exp))
mydata$person_emp_exp<- exp
boxplot(mydata$person_emp_exp)




boxplot(mydata$person_income)
quantile(mydata$person_income)

i <- mydata$person_income
i

iqr_i <- IQR(i)
iqr_i

lower_bound_i <-  60747 -  1.5 * iqr_i
upper_bound_i <-  149875 + 1.5 * iqr_i
mean(i)
i[i < lower_bound_i | i > upper_bound_i] <- mean(i)
i <- as.integer(round(i))

boxplot(i)
mydata$person_income<-i







unique(mydata$person_gender)
mydata <- mydata %>%
  mutate(gender = case_when(
    tolower(person_gender) %in% c("male", "malee") ~ "male",
    tolower(person_gender) %in% c("female", "feemale") ~ "female"
  ))

mydata$person_gender<-mydata$gender
mydata$gender <- NULL

unique(mydata$person_gender)



unique(mydata$person_home_ownership)
mydata <- mydata %>%
  mutate(home = case_when(
    toupper(person_home_ownership) %in% c("RENT", "RENTT") ~ "RENT",
    toupper(person_home_ownership) %in% c("OWN", "OOWN") ~ "OWN",
    toupper(person_home_ownership) %in% c("MORTGAGE") ~ "MORTGAGE",
    toupper(person_home_ownership) %in% c("OTHER") ~ "OTHER",
    
    
  ))


mydata$person_home_ownership<-mydata$home
mydata$home <- NULL
unique(mydata$person_home_ownership)





unique(mydata$loan_intent)
unique(mydata$previous_loan_defaults_on_file)



unique(mydata$person_gender)
mydata$person_gender <- factor(mydata$person_gender, 
                         levels = c("female", "male"),labels = c(1,2))

unique(mydata$previous_loan_defaults_on_file)
mydata$previous_loan_defaults_on_file <- factor(mydata$previous_loan_defaults_on_file, 
                               levels = c("No", "Yes"),labels = c(0,1))


unique(mydata$loan_status)
mydata$loan_status <- factor(mydata$loan_status, 
                                                levels = c(0,1),labels = c("No","Yes"))




normalization <- function(n) {
  (n - min(n, na.rm = TRUE)) / (max(n, na.rm = TRUE) - min(n, na.rm = TRUE))
}
mydata$person_income <- normalization(mydata$person_income)









filtered_data1 <- mydata %>%
  filter(loan_amnt > 20000 | person_age < 20)
filtered_data1
str(filtered_data1)



filtered_data2 <- mydata %>%
  filter(loan_status == "Yes" & person_age > 20)
filtered_data2
str(filtered_data2)





table(mydata$person_gender)
  balanced_data_over <- mydata %>%
    group_by(person_gender) %>%
    slice_sample(n = max(table(mydata$person_gender)), replace = TRUE) %>%
    ungroup()
  
  table(balanced_data_over$person_gender)
  class_distribution_balanced <- balanced_data_over %>%
    count(person_gender) %>%
    
    mutate(percentage = n / sum(n) * 100)
  
  
  duplicated(balanced_data_over)
  mydata_unique <- mydata[!duplicated(balanced_data_over), ]
  
  duplicated(mydata_unique)
  
  

set.seed(123)
train_data <- mydata %>% 
    sample_frac(0.8)
  

test_data <- mydata %>% 
    anti_join(train_data)
  
  str(train_data)
  str(test_data)
  
  
  
  
summarize_age_income<- mydata %>%
    group_by(loan_status) %>%
    summarise(
      mean_age    = mean(person_age, na.rm = TRUE),
      median_age  = median(person_age, na.rm = TRUE),
      sd_age      = sd(person_age, na.rm = TRUE),
      min_age     = min(person_age, na.rm = TRUE),
      max_age     = max(person_age, na.rm = TRUE),
      
      mean_income   = mean(person_income, na.rm = TRUE),
      median_income = median(person_income, na.rm = TRUE),
      sd_income     = sd(person_income, na.rm = TRUE),
      min_income    = min(person_income, na.rm = TRUE),
      max_income    = max(person_income, na.rm = TRUE)
    )
summarize_age_income

 
avg_credit <- mydata %>%
  group_by(loan_status) %>%
  summarise(
    mean_credit  = mean(credit_score, na.rm = TRUE),
    median_credit = median(credit_score, na.rm = TRUE),
    sd_credit    = sd(credit_score, na.rm = TRUE),
    count        = n()
  )
avg_credit





emp_exp_stats <- mydata %>%
  group_by(person_education) %>%
  summarise(
    mean_exp   = mean(person_emp_exp, na.rm = TRUE),
    median_exp = median(person_emp_exp, na.rm = TRUE),
    sd_exp     = sd(person_emp_exp, na.rm = TRUE),
    min_exp    = min(person_emp_exp, na.rm = TRUE),
    max_exp    = max(person_emp_exp, na.rm = TRUE),
    IQR_exp    = IQR(person_emp_exp, na.rm = TRUE),
    n          = n()
  )
emp_exp_stats
unique(mydata$person_education)


  
  