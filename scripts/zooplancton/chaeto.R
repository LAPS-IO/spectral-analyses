library(tidyverse)
library(data.table)

setwd("~/Desktop/grouped/chaeto_ret")
chaetognata_ret <- list.files(include.dirs = FALSE) %>% 
  as_tibble() %>% 
  mutate(trait = "ret")
setwd("~/Desktop/grouped")
fwrite(chaetognata_ret, "chaetognata_ret.csv")

setwd("~/Desktop/grouped/chaeto_curved")
chaetognata_curved <- list.files(include.dirs = FALSE) %>% 
  as_tibble() %>% 
  mutate(trait = "curved")
setwd("~/Desktop/grouped")
fwrite(chaetognata_curved, "chaetognata_curved.csv")

setwd("~/Desktop/grouped/chaeto_eating")
chaetognata_eating <- list.files(include.dirs = FALSE) %>% 
  as_tibble() %>% 
  mutate(trait = "eating")
setwd("~/Desktop/grouped")
fwrite(chaetognata_eating, "chaetognata_eating.csv")

setwd("~/Desktop/grouped/chaeto_egging")
chaetognata_egging <- list.files(include.dirs = FALSE) %>% 
  as_tibble() %>% 
  mutate(trait = "egging")
setwd("~/Desktop/grouped")
fwrite(chaetognata_egging, "chaetognata_egging.csv")

chaetognatha <- bind_rows(chaetognata_curved, 
                          chaetognata_eating, 
                          chaetognata_egging, 
                          chaetognata_ret)
chaetognatha <- chaetognatha %>% 
  mutate(
    date = as.Date(str_sub(value, 1, 10)),  # Garante que 'date' seja do tipo Date
    time = str_sub(value, 12, 23) %>% 
      paste(date, .) %>% 
      lubridate::ymd_hms() %>% 
      lubridate::ceiling_date("30 min") %>% 
      format("%H:%M:%S"),
    trait = as.factor(trait))

chaeto <- chaetognatha %>% 
  group_by(trait, time) %>% 
  summarise(N = n())

ggplot(chaeto, aes(x = hms::as_hms(time), y = N, color = trait)) +
  geom_smooth() +
  labs(x = "Time", 
       y = "Number of individuals", 
       title = "Chaetognatha sp. behavior traits", 
       color = "Traits") +
  scale_color_manual(values = c("red", "black", "darkblue", "darkgreen")) +
  facet_wrap(~ factor(trait,
                      levels = c("ret", "curved", "eating", "egging"),
                      ordered = TRUE), 
             scales = "free_y")

chaeto2 <- chaetognatha %>% 
  mutate(datetime = paste(date, time) %>% 
           lubridate::ymd_hms() %>% 
           format("%Y-%m-%d %H:%M:%S")) %>% 
  group_by(datetime = ymd_hms(datetime) %>% round_date("30 min")) %>% 
  summarise(N = n()) %>% ungroup()

ggplot(chaeto2, aes(x = datetime,y = N)) +
  geom_smooth() +
  labs(x = "datetime", 
       y = "Number of individuals")
