
library(tidyverse)

# Define the teacher-course mapping
teacher_course_map <- tibble(
  course = c(rep("Mathematics", 3), rep("Biology", 3), rep("Chemistry", 2), rep("Physics", 2), 
             rep("Civics", 6), rep("History", 6), rep("Geography", 6), rep("English", 6), rep("Kiswahili", 6)),
  teacher = c("Ms. Amani", "Mr. Faraji", "Ms. Graceful",          # Mathematics teachers
              "Mr. Baraka", "Ms. Number", "Ms. Zuri",           # Biology teachers
              "Mr. Jingle", "Mr. Chacha",                        # Chemistry teachers
              "Ms. Amani", "Mr. Daudi",                        # Physics teachers (overlap with Mathematics)
              "Mr. Hassan", "Ms. Chiku", "Mr. Juma", "Mr. Baraka", "Ms. Nuru", "Ms. Eliza",  # Civics teachers
              "Ms. Grace", "Mr. Faraji", "Mr. Correct", "Ms. Zuri", "Mr. Jolly", "Mr. Hassan",  # History teachers
              "Ms. Eliza", "Mr. Daudi", "Ms. Chiku", "Mr. Faraji", "Ms. Amani", "Mr. Boom",   # Geography teachers
              "Ms. Clara", "Mr. Adam", "Ms. Howdey", "Mr. Daniel", "Ms. Hello", "Mr. Playa",   # English teachers
              "Mr. Hangtight", "Ms. Nuru", "Ms. Zuri", "Mr. Chacha", "Ms. Grace", "Mr. Juma")   # Kiswahili teachers
)

# Create a teacher availability data frame
teacher_availability <- tibble(
  teacher = unique(teacher_course_map$teacher),
  period_1 = TRUE, period_2 = TRUE, period_3 = TRUE, period_4 = TRUE, period_5 = TRUE,
  period_6 = TRUE, period_7 = TRUE, period_8 = TRUE, period_9 = TRUE
)

# Define the streams
streams <- unlist(map2(
  1:4,
  list(c('A', 'B', 'C', 'D'), c('A', 'B', 'C', 'D'), c('A', 'B', 'C'), c('A', 'B', 'C')),
  ~ paste0('Form ', .x, .y)
))

# Create the schedule tibble with 14 streams and 9 periods
schedule <- expand_grid(
  period = 1:9,            # 9 periods in a day
  stream = streams
) %>%
  mutate(
    teacher = NA_character_, # Placeholder for teacher names
    course = NA_character_   # Placeholder for course names
  )
