library(readr)
library(dplyr)
library(ggplot2)

### Base datasets
# Create a student subscription type table
df_student_type <- data.frame(
  type_id = c(0, 1),
  type = c("Free", "Paid")
)

write_csv(df_student_type, "dataset/student_type.csv")

# Getting subscription date from students to use it as filters on all charts
df_purchases <- read_csv("dataset/365_student_purchases.csv")

get_end_date_subscription <- function(subscription_type, start_date) {
  case_when(
    subscription_type == "Monthly" ~ start_date + 30,
    subscription_type == "Quarterly" ~ start_date + 90,
    subscription_type == "Annual" ~ start_date + 365,
    TRUE ~ start_date
  )
}

df_purchases <- df_purchases %>%
  mutate(sub_start_date = date_purchased,
         sub_end_date = get_end_date_subscription(purchase_type, date_purchased)) %>% 
  select(student_id, sub_start_date, sub_end_date)


# Enhancing student info to show if the student have subscribed for at least 1 month
df_student <- read_csv("dataset/365_student_info.csv")

df_student <- df_student %>% 
  left_join(df_purchases, by = "student_id") %>% 
  group_by(student_id, student_country, date_registered) %>%
  summarise(student_type_id = if_else(sum(!is.na(sub_start_date)) > 0, 1, 0))

write_csv(df_student, "dataset/student_info.csv")


### Minutes watched by users by day
df_learning <- read_csv("dataset/365_student_learning.csv")

# As there is no value of student watching more than one course per day
# we don't need to summarise values in the dataset
df_learning %>% 
  group_by(student_id, date_watched) %>% 
  summarise(courses_per_day = n()) %>%
  filter(courses_per_day > 1)

# Removing outliers where students watch more than 24 hours of classes on a single
# day and where they watch 0 minutes
df_learning <- df_learning %>% 
  filter((minutes_watched <= 24*60) & (minutes_watched > 0)) %>% 
# Adding the type of user on the day they watched de course
  left_join(df_purchases, by = "student_id") %>% 
  mutate(student_type = if_else((date_watched >= sub_start_date) & 
                                  (date_watched <= sub_end_date), 
                                1, 0, missing = 0)) %>%
  group_by(student_id, course_id, minutes_watched, date_watched) %>%
  summarise(student_type_id = if_else(sum(student_type) > 0, 1, 0))

write_csv(df_learning, "dataset/student_learning.csv")


### Engaged days in a row
df_engagement <- read_csv("dataset/365_student_engagement.csv")

df_engagement <-df_engagement %>% 
  select(student_id, date_engaged) %>% 
  arrange(student_id, date_engaged) %>% 
  mutate(previous_student_same = if_else(lag(student_id) == student_id,
                                         TRUE, FALSE, missing = FALSE),
         continuous_date = if_else(lag(date_engaged) == date_engaged - 1,
                                   TRUE, FALSE, missing = FALSE),
         increase_count = previous_student_same & continuous_date)

# Creating groups of engagement in a row of days
group_test <- 0  
df_engagement$group <- sapply(df_engagement$increase_count, function(x) { 
  if(x) {
    group_test  
  } else {
    group_test <<- group_test + 1
    group_test  
  }
})

df_engagement <- df_engagement %>% 
  group_by(student_id, group) %>% 
  summarise(days_in_row = n(),
            last_date_engaged = max(date_engaged)) %>% 
  select(student_id, last_date_engaged, days_in_row) %>%
# Only getting students that attended to courses for more than one day
  filter(days_in_row >= 2) %>% 
# Adding the type of user on the last day they engaged on the course
  left_join(df_purchases, by = "student_id") %>% 
  mutate(student_type = if_else((last_date_engaged >= sub_start_date) & 
                                  (last_date_engaged <= sub_end_date), 
                                1, 0, missing = 0)) %>%
  group_by(student_id, last_date_engaged, days_in_row) %>%
  summarise(student_type_id = if_else(sum(student_type) > 0, 1, 0))

write_csv(df_engagement, "dataset/student_engagement_in_row.csv")


### Engaged days per student and per week
df_engagement <- read_csv("dataset/365_student_engagement.csv")

df_engagement <- df_engagement %>% 
  mutate(week_n = strftime(date_engaged, "%V")) %>% 
  group_by(student_id, week_n) %>% 
  summarise(last_date_engaged = max(date_engaged),
            n_engagements = n()) %>% 
# Adding the type of user on the last day they engaged on the course
  left_join(df_purchases, by = "student_id") %>% 
  mutate(student_type = if_else((last_date_engaged >= sub_start_date) & 
                                  (last_date_engaged <= sub_end_date), 
                                1, 0, missing = 0)) %>%
  group_by(student_id, week_n, n_engagements) %>%
  summarise(student_type_id = if_else(sum(student_type) > 0, 1, 0))

write_csv(df_engagement, "dataset/student_engagement_per_week.csv")


### Students that engaged at least 5 times and 30 days or more
df_engagement <- read_csv("dataset/365_student_engagement.csv")

df_engagement <- df_engagement %>% 
  group_by(student_id) %>% 
  summarise(first_engagement = min(date_engaged),
            last_engagement = max(date_engaged),
            n_engagements = n()) %>% 
  mutate(dt_diff_engaged = last_engagement - first_engagement) %>% 
  filter(dt_diff_engaged >= 30 &
           n_engagements >= 5) %>% 
# Adding the type of user on the last day they engaged on the course
  left_join(df_purchases, by = "student_id") %>% 
  mutate(student_type = if_else(((last_engagement >= sub_start_date) & 
                                  (last_engagement <= sub_end_date)) |
                                  ((first_engagement >= sub_start_date) & 
                                     (first_engagement <= sub_end_date)), 
                                1, 0, missing = 0)) %>%
  group_by(student_id, n_engagements, dt_diff_engaged) %>%
  summarise(student_type_id = if_else(sum(student_type) > 0, 1, 0))

write_csv(df_engagement, "dataset/student_engagement_more_time.csv")


df_purchases %>% 
  group_by(student_id) %>% 
  summarise(n = n())
