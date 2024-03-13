# dependencies -----

library(tidyverse)
library(httr2)
library(progress)

# setup -----

# OpenAI API
openai_token <- Sys.getenv("OPENAI_TOKEN") # will cost about $9

# l1 responses
l1 <- read_csv("01_Data/Processed/l1_responses.csv")

# pre-flight -----

# determine number of api calls for each cue and participant group
conditions <- l1 %>%
  group_by(gender, education) %>%
  count(response) %>%
  arrange(gender, education, desc(n)) %>%
  mutate(api_calls = ceiling(n / 10)) %>%
  rename(cue = response) %>%
  mutate(system_prompt = paste0("You are a ", gender, " ", education, "-educated adult.")) %>%
  mutate(prompt = paste0(system_prompt,
                         " What are the first five associations that come to mind when you think of: '",
                         cue,
                         "'? Separate the responses by comma. Produce ten separate sets of five responses separated by semicolon. And remember: ",
                         system_prompt))

conditions_responses <- list()
pb <- progress_bar$new(total = nrow(conditions))
pb$tick(0)

# generation loop -----

for (j in 1:nrow(conditions)) {

  system_prompt <- conditions$system_prompt[j]
  user_prompt <- conditions$prompt[j]
  i = 1
  responses_list <- list()

  while (length(responses_list) < conditions$api_calls[j]) {

    # request
    response <- request(base_url = "https://api.openai.com/v1/chat/completions") %>%
      req_auth_bearer_token(openai_token) %>%
      req_body_json(list("model" = "gpt-4-0125-preview",
                         "temperature" = 1,
                         "messages" = list(list("role" = "system",
                                                "content" = system_prompt),
                                           list("role" = "user",
                                                "content" = user_prompt)))) %>%
      req_retry(max_tries = 5) %>%
      req_perform()

    # fetch response
    responses_list[[i]] <- resp_body_json(response)$choices[[1]]$message$content

    # increment, update pb
    i <- i + 1

  }

  pb$tick()
  conditions_responses[[j]] <- responses_list

}

result <- conditions %>% mutate(responses = "NA") %>% slice(0)
for (k in 1:nrow(conditions)) {
  c_resp <- unlist(conditions_responses[[k]])
  result <- result %>% bind_rows(conditions[k, ] %>%
                                   bind_cols(responses = c_resp))
}

write_csv(result, "01_Data/Raw/l2_result.csv")
