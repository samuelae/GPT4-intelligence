# dependencies -----

library(tidyverse)

# data & setup -----

l1 <- read_csv("01_Data/Processed/l1_responses.csv")
l2 <- read_csv("01_Data/Processed/l2_responses.csv")

# setup
associations_l1 <- l1 %>% mutate(response_level = 1)
associations_l2 <- associations_l1 %>% slice(0)
used_resp_ids <- c()

# combine -----

pb <- progress::progress_bar$new(total = length(unique(l1$participant_id)))

for (p in unique(l1$participant_id)) {

  # get this participant's level_1 responses (clean)
  l1_sel <- l1 %>%
    filter(participant_id == p)
  l1_resp <- l1_sel %>%
    pull(response)

  # get potential level_2 responses for this participant
  l2_sel <- l2 %>%
    filter(gender == l1_sel$gender[1],
           education == l1_sel$education[1],
           cue %in% l1_resp,
           !(resp_id %in% used_resp_ids))

  # sample responses for each cue
  for (c in l1_resp) {

    # get potential responses for this cue
    potential_resp_ids <- l2_sel %>%
      filter(cue == c) %>%
      pull(resp_id) %>%
      unique()

    if (length(potential_resp_ids > 0)) {
      id_sel <- sample(potential_resp_ids, size = 1)
      used_resp_ids <- c(used_resp_ids, id_sel)
    } else {
      id_sel <- NA
      cat(paste("participant", p, "produced no responses for cue:", c, "\n"))
    }

    associations_l2 <- associations_l2 %>%
      bind_rows(l2_sel %>%
                  filter(resp_id == id_sel) %>%
                  mutate(participant_id = p, response_level = 2) %>%
                  select(participant_id, gender, education, cue, response,
                         response_position, response_level))

  }

  pb$tick()

}

associations <- associations_l1 %>%
  bind_rows(associations_l2)

# final cleaning of data ----

# remove numbers and punctuation

associations$cue <- str_replace_all(associations$cue, "[:digit:]", replacement = " ")
associations$cue <- str_replace_all(associations$cue, "[:punct:]", replacement = " ")
associations$cue <- str_squish(associations$cue)

associations$response <- str_replace_all(associations$response, "[:digit:]", replacement = " ")
associations$response <- str_replace_all(associations$response, "[:punct:]", replacement = " ")
associations$response <- str_squish(associations$response)

# output -----

# csv
write_csv(associations, "01_Data/intelligence_associations.csv")

# package
intelligence <- associations
save(intelligence, file = "01_Data/intelligence.rda")

