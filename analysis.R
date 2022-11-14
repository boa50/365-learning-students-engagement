library(readr)
library(dplyr)
library(ggplot2)

### Questions
# Minutes watched have big influence on how students stay engaged?
# Students usually stay more engaged after a purchase?
# Is there a difference on student engagement based on their country? (maybe poorer countries with less subscriptions will have less engagement)
# Free users engage more in which type of course?
# Which kind of purchase is more common?
### Things that would be useful
# It would be interesting to have more information about the users (if they are single or married, if they have children, if they work or study or nothing, their age)
# We don't have information about the completion of the courses

df_purchases <- read_csv("dataset/365_student_purchases.csv")
df_students <- read_csv("dataset/365_student_info.csv")

length(unique(df_students$student_id))
length(unique(df_purchases$student_id))

### What is student engagement?

df_engagement <- read_csv("dataset/365_student_engagement.csv")
df_learning <- read_csv("dataset/365_student_learning.csv")

# In this case the student watched something but there is no engagement in lessons
# Maybe the engagement in quizzes would count to minutes watched
df_engagement %>% 
  filter(engagement_lessons == 0) %>% 
  inner_join(df_learning, by = c("student_id", "date_engaged" = "date_watched"))

# Plot a histogram of minutes_watched by student_id/date_watched

# Group the data by student_id/date_watched because the same student could watch different courses on the same date
df_learning %>% 
  group_by(student_id, date_watched) %>% 
  summarise(total_minutes_watched = sum(minutes_watched)) %>% 
  ggplot(aes(total_minutes_watched)) +
  geom_histogram()

df_test <- df_learning %>% 
  group_by(student_id, date_watched) %>% 
  summarise(total_minutes_watched = sum(minutes_watched))

# There is a user that watched more than 28 hours of courses on the same day (I think this is a problem)
max(df_test$total_minutes_watched)/60

### Is there a difference on student engagement based on their country? (maybe poorer countries with less subscriptions will have less engagement)

df_join_purchases_students <- inner_join(df_students, df_purchases, by = "student_id")

# Add the column to get how many days are the subscription
df_join_purchases_students %>% 
  mutate(sub_start_date = date_purchased,
         sub_end_date)

df_sample <- df_join_purchases_students[1:5,]

# Check column types
str(df_sample)

# Convert dates
df_sample$date_registered <- as.Date(df_sample$date_registered)
df_sample$date_purchased <- as.Date(df_sample$date_purchased)

sample_date <- as.Date("2022-01-01")
sample_purchase_type <- "Monthly"

get_end_date_subscription <- function(subscription_type, start_date) {
  case_when(
    subscription_type == "Monthly" ~ start_date + 30,
    subscription_type == "Quarterly" ~ start_date + 90,
    subscription_type == "Annual" ~ start_date + 365,
    TRUE ~ start_date
  )
}


df_sample$date_purchased

get_end_date_subscription("Annual", sample_date)

df_sample %>% 
  mutate(end_sub = get_end_date_subscription(purchase_type, date_purchased),
         days_subscribed = end_sub - date_purchased)