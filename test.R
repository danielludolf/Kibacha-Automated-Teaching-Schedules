
source('~/Kibacha-Automated-Teaching-Schedules/setup.R')

set.seed(123)
max_retries <- 100

for (period in 1:9) {
  
  period_col <- paste0("period_", period)
  assigned <- FALSE
  
  for (attempt in 1:max_retries) {
    
    # Reset temporary variables
    available_teachers <- teacher_availability %>%
      filter(.data[[period_col]] == TRUE) %>%
      pull(teacher)
    
    assigned_teachers <- c()
    temp_schedule <- schedule
    
    # Shuffle streams
    shuffled_streams <- sample(unique(schedule$stream))
    
    success <- TRUE
    
    for (stream in shuffled_streams) {
      
      # Try to find a course and teacher for this stream
      courses <- sample(unique(teacher_course_map$course))
      assigned <- FALSE
      
      for (course in courses) {
        course_teachers <- teacher_course_map %>%
          filter(course == !!course) %>%
          pull(teacher)
        
        # Eligible and not yet assigned this period
        candidates <- setdiff(intersect(course_teachers, available_teachers), assigned_teachers)
        
        if (length(candidates) > 0) {
          
          selected_teacher <- sample(candidates, 1)
          
          # Assign in temp schedule
          temp_schedule <- temp_schedule %>%
            mutate(
              teacher = ifelse(period == !!period & stream == !!stream, selected_teacher, teacher),
              course = ifelse(period == !!period & stream == !!stream, !!course, course)
            )
          
          assigned_teachers <- c(assigned_teachers, selected_teacher)
          assigned <- TRUE
          break  # stop trying courses
        }
      }
      
      if (!assigned) {
        success <- FALSE
        break  # fail this attempt, retry outer loop
      }
    }
    
    if (success) {
      # All streams assigned for this period
      schedule <- temp_schedule
      teacher_availability[teacher_availability$teacher %in% assigned_teachers, period_col] <- FALSE
      message(paste("Period", period, "assigned successfully on attempt", attempt))
      assigned <- TRUE
      break
    }
  }
  
  if (!assigned) {
    warning(paste("Could not assign all streams in period", period, "after", max_retries, "tries"))
  }
}

View(teacher_availability)

