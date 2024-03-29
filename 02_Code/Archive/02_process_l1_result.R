# dependencies -----

library(tidyverse)

# data -----

result <- read_csv("01_Data/Raw/l1_result.csv") %>%
  select(gender, education, cue, responses)

# process -----

data <- tibble(participant_id = numeric(length = 0),
               gender = character(length = 0),
               education = character(length = 0),
               cue = character(length = 0),
               response = character(length = 0),
               response_position = numeric(length = 0))

for (i in 1:nrow(result)) {

  # participants
  participant_results <- result[i, ]$responses %>% str_split(";") %>% unlist() %>% str_squish()

  # responses
  responses_results <- participant_results[1:10] %>%
    str_split(",") %>%
    lapply((\(x) {str_remove(str_squish(x), "[^\\w\\d]+$")}))

  for (j in 1:length(responses_results)) {

    data <- data %>%
      bind_rows(tibble(participant_id = (i - 1) * 10 + j,
                       gender = result[i, ]$gender,
                       education = result[i, ]$education,
                       cue = result[i, ]$cue,
                       response = responses_results[[j]],
                       response_position = 1:length(responses_results[[j]])))

  }

}

write_csv(data, "01_Data/Processed/l1_responses.csv")
