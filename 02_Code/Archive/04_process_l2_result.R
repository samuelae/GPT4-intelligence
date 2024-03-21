# dependencies -----

library(tidyverse)

# data -----

result <- read_csv("01_Data/Raw/l2_result.csv") %>%
  select(gender, education, cue, n, responses)

# process l2 responses -----

data <- tibble(gender = character(length = 0),
               education = character(length = 0),
               cue = character(length = 0),
               response = character(length = 0),
               response_position = numeric(length = 0),
               resp_id = numeric(length = 0))

for (i in 1:nrow(result)) {

  # participants
  participant_results <- result[i, ]$responses %>% str_split(";") %>% unlist() %>% str_squish()

  # responses
  responses_results <- participant_results[1:10] %>%
    str_split(",") %>%
    lapply((\(x) {str_remove(str_squish(x), "[^\\w\\d]+$")}))

  for (j in 1:length(responses_results)) {

    data <- data %>%
      bind_rows(tibble(gender = result[i, ]$gender,
                       education = result[i, ]$education,
                       cue = result[i, ]$cue,
                       response = responses_results[[j]],
                       response_position = 1:length(responses_results[[j]]),
                       resp_id = (i - 1) * 10 + j))

  }

}

write_csv(data, "01_Data/Processed/l2_responses.csv")
