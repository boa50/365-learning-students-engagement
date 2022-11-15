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

# The purchase dates ranges from 2022-01-01 to 2022-10-20
min(df_purchases$date_purchased)
max(df_purchases$date_purchased)

### What is student engagement?

df_engagement <- read_csv("dataset/365_student_engagement.csv")
df_learning <- read_csv("dataset/365_student_learning.csv")

# In this case the student watched something but there is no engagement in lessons
# Maybe the engagement in quizzes would count to minutes watched
df_engagement %>% 
  filter(engagement_lessons == 0) %>% 
  inner_join(df_learning, by = c("student_id", "date_engaged" = "date_watched"))

# There are users that only engaged to do exams or quizzes on that day
df_engagement %>% 
  left_join(df_learning, by = c("student_id", "date_engaged" = "date_watched")) %>% 
  filter(is.na(minutes_watched))

# Plot a histogram of minutes_watched by student_id/date_watched

# Group the data by student_id/date_watched because the same student could watch different courses on the same date
df_learning %>% 
  group_by(student_id, date_watched) %>% 
  summarise(total_minutes_watched = sum(minutes_watched)) %>% 
  # Getting only students that watch less than 24h of courses on the same day and at least 0.1 minute
  filter(total_minutes_watched < (24*60) & total_minutes_watched > 0) %>% 
  ggplot(aes(total_minutes_watched)) +
  geom_histogram()

# There is students that watched 0 minutes from a course
df_learning %>% 
  filter(minutes_watched == 0)

df_test <- df_learning %>% 
  group_by(student_id, date_watched) %>% 
  summarise(total_minutes_watched = sum(minutes_watched)) %>%
  filter(total_minutes_watched < (24*60)) %>% 
  filter(total_minutes_watched > 0)

# There is a user that watched more than 28 hours of courses on the same day (I think this is a problem)
summary(df_test$total_minutes_watched)

### Is there a difference on student engagement based on their country? (maybe poorer countries with less subscriptions will have less engagement)
df_join_purchases_students <- inner_join(df_students, df_purchases, by = "student_id")


# Does the same student have purchased more than one time? (renewed subscription)
df_students_more_purchases <- df_purchases %>% 
  group_by(student_id) %>% 
  summarise(purchases = n()) %>% 
  filter(purchases > 1)
df_students_more_purchases$student_id

# Add the column to get how many days are the subscription
df_join_purchases_students <- df_join_purchases_students %>% 
  filter(student_id %in% df_students_more_purchases$student_id) %>% 
  arrange(student_id) %>% 
  mutate(sub_start_date = date_purchased,
         sub_end_date = get_end_date_subscription(purchase_type, date_purchased),
         days_subscribed = sub_end_date - sub_start_date)
  

df_sample <- df_join_purchases_students[1:5,]

get_end_date_subscription <- function(subscription_type, start_date) {
  case_when(
    subscription_type == "Monthly" ~ start_date + 30,
    subscription_type == "Quarterly" ~ start_date + 90,
    subscription_type == "Annual" ~ start_date + 365,
    TRUE ~ start_date
  )
}

df_sample %>% 
  mutate(end_sub = get_end_date_subscription(purchase_type, date_purchased),
         days_subscribed = end_sub - date_purchased)


### Which kind of purchase is more common?
df_purchases %>% 
  group_by(purchase_type) %>% 
  summarise(quantity = n())

### What is the difference between free and paid users engagement?
df_engagement_subscribed_students <- df_join_purchases_students %>% 
  inner_join(df_engagement, by = "student_id") %>% 
  select(student_id, sub_start_date, sub_end_date, engagement_id, date_engaged) %>% 
  mutate(engagement_type = if_else((date_engaged >= sub_start_date) & (date_engaged <= sub_end_date), 1, 0)) %>% 
  group_by(student_id, engagement_id, date_engaged) %>% 
  summarise(engagement_type = if_else(sum(engagement_type) > 0, "Paid", "Free"))


df_engagement_subscribed_students %>% 
  filter(engagement_type == "Paid")
