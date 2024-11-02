
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

# FIXME Assign teachers and courses to classes in the schedule
for (period in 1:9) {
  # Create the column name for the current period
  period_col <- paste0("period_", period)
  
  # Check if 'period_col' exists in the 'teacher_availability' data frame
  if (period_col %in% colnames(teacher_availability)) {
    
    # Get the list of available teachers for this period
    available <- filter(teacher_availability, .data[[period_col]] == TRUE)
    
    # Proceed only if there are available teachers
    if (nrow(available) > 0) {
      for (stream in unique(schedule$stream)) {
        # Randomly select a course for the stream
        selected_course <- sample(unique(teacher_course_map$course), 1)
        
        # Get teachers for the selected course
        course_teachers <- teacher_course_map %>% filter(course == selected_course) %>% pull(teacher)
        
        # Filter available teachers for the selected course
        available_course_teachers <- available %>% filter(teacher %in% course_teachers)
        
        if (nrow(available_course_teachers) > 0) {
          selected_teacher <- sample(available_course_teachers$teacher, 1)  # Randomly select a teacher from available ones
          
          # Mark the selected teacher as unavailable for this period
          teacher_availability[teacher_availability$teacher == selected_teacher, period_col] <- FALSE
          
          # Assign the selected teacher and course for this period and stream
          schedule <- schedule %>%
            mutate(teacher = ifelse(period == !!period & stream == !!stream & is.na(teacher), selected_teacher, teacher),
                   course = ifelse(period == !!period & stream == !!stream & is.na(course), selected_course, course))
        }
      }
    } else {
      # Handle the case where no teacher is available for the period
      schedule <- schedule %>%
        mutate(teacher = ifelse(period == !!period, NA_character_, teacher),
               course = ifelse(period == !!period, NA_character_, course))
    }
  } else {
    stop(paste("Column", period_col, "does not exist in teacher_availability"))
  }
}

