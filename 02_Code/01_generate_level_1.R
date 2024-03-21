# dependencies -----

library(tidyverse)
library(httr2)
library(progress)

# setup -----

# OpenAI API
openai_token <- Sys.getenv("OPENAI_TOKEN")

# Free Associations
cue <- "intelligence"
gender <- c("male", "female")
education <- c("high school", "university")

# Study design
n_10_trials_per_group <- 25

# pre-flight -----

# conditions
conditions <- expand_grid(cue, gender, education) %>%
  mutate(prompt = paste0("What are the first five associations that come to a ",
                         gender, " ", education,
                         "-educated adult's mind when thinking of the cue '",
                         cue,
                         "'? Produce responses highly typical for a ",
                         gender, " ", education, "-educated adult. Separate the responses by comma. Produce ten separate sets of five responses separated by semicolon."))

conditions_responses <- list()

# generation loop -----

for (j in 1:nrow(conditions)) {

  user_prompt <- conditions$prompt[j]
  i = 1
  responses_list <- list()

  print(paste("cue:", conditions$cue[j], "  gender:", conditions$gender[j], "  education:", conditions$education[j]))
  pb <- progress_bar$new(total = n_10_trials_per_group)
  pb$tick(0)

  while (length(responses_list) < n_10_trials_per_group) {

    # request
    response <- request(base_url = "https://api.openai.com/v1/chat/completions") %>%
      req_auth_bearer_token(openai_token) %>%
      req_body_json(list("model" = "gpt-4-0125-preview",
                         "temperature" = 1,
                         "messages" = list(list("role" = "user",
                                                "content" = user_prompt)))) %>%
      req_retry(max_tries = 5) %>%
      req_perform()

    # fetch response
    responses_list[[i]] <- resp_body_json(response)$choices[[1]]$message$content

    # increment, update pb
    i <- i + 1
    pb$tick()

  }

  conditions_responses[[j]] <- responses_list

}

result <- conditions %>% mutate(responses = "NA") %>%  slice(0)
for (k in 1:nrow(conditions)) {
  c_resp <- unlist(conditions_responses[[k]])
  result <- result %>% bind_rows(conditions[k, ] %>%
    bind_cols(responses = c_resp))
}

write_csv(result, "01_Data/Raw/l1_result.csv")

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
