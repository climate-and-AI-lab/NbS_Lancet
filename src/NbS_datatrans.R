library(tidyverse)
library(dplyr)
library(tidytext)
library(mapview)
library(tmaptools)
library(ggplot2)
library(sf)
library(ggsci)
library(tictoc)
library(tmap)
library(classInt)
library(ggforce)



nbs_df <- readRDS("data/nbs_df.rds")
count <- table(nbs_df$begin_year)


## text analysis

#split unique solutions and for col generation
nbs_df_split <- data.frame(lapply(nbs_df[, c(5:7,12:16)], as.character)) # Convert columns to character type
nbs_df_split[,1:8] <- lapply(nbs_df_split, strsplit, ",\\s*")

#loop through generate binary columns for each column of interest and save unique values
uniq_valdf <- list()
for (i in 1:8) {
  tic()
  
  uniq_val <- unique(unlist(nbs_df_split[,i]))
  if (i == 1) {
    uniq_valdf <- as.data.frame(cbind(uniq_valdf, uniq_val))
    names(uniq_valdf) <- colnames(nbs_df_split)[i]
  }
  else if (i != 1) {
    uniq_valvec <- data.frame(uniq_val)
    names(uniq_valvec) <- colnames(nbs_df_split)[i]
    if (nrow(uniq_valvec) > nrow(uniq_valdf)) {
      
      na_mat <- matrix(NA, nrow = nrow(uniq_valvec) - nrow(uniq_valdf), ncol = ncol(uniq_valdf))
      colnames(na_mat) <- colnames(uniq_valdf)
      uniq_valdf <- rbind(uniq_valdf, na_mat)
      
      uniq_valdf <- cbind(uniq_valdf, uniq_valvec)
    } 
    else if (nrow(uniq_valdf) > nrow(uniq_valvec)) {
      na_mat <- matrix(NA, nrow = nrow(uniq_valdf) - nrow(uniq_valvec), ncol = ncol(uniq_valvec))
      colnames(na_mat) <- colnames(uniq_valvec)
      uniq_valvec <- rbind(uniq_valvec, na_mat)
      
      uniq_valdf <- cbind(uniq_valdf, uniq_valvec)
    }
    else {
      uniq_valdf <- cbind(uniq_valdf, uniq_valvec)
    }
  }
  
  for (j in uniq_val) {
    mat <- matrix(0, nrow = 1012, ncol = 1)
    temp_df <- as.data.frame(mat)
    temp_df$V1 <- ifelse(grepl(j, nbs_df_split[,i], fixed = TRUE), 1, 0)
    
    
    column_name <- paste0(colnames(nbs_df_split)[i], "_", j)
    names(temp_df) <- column_name
    
    nbs_df <- cbind(nbs_df, temp_df)
  }
  print(colnames(nbs_df_split)[i])
  toc()
}

#find single solution pairs 
filtered_sols <- nbs_df_split[lengths(nbs_df_split$key_solutions) == 1, ]
filtered_sols <- filtered_sols %>%
  unnest(solutions)
uniq_sols <- unique(filtered_sols$solutions)
missing_sol <- uniq_valdf$solutions[!(uniq_valdf$solutions %in% uniq_sols)]

filtered_sols <- filtered_sols[filtered_sols$solutions %in% uniq_sols, ]
filtered_sols <- distinct(filtered_sols, solutions, key_solutions)
#find missing sol in solutions House gardens, Atrium, Deltas
pivoted_sols <- nbs_df_split %>% 
  unnest(solutions) %>% 
  filter(solutions == missing_sol[[1]])

#assign missing sol to key solution 
sol_phrases <- c("Parks and urban forests","Community gardens and allotments", 
                 "Blue infrastructure", "Nature on buildings", "Grey infrastructure featuring greens",
                 "Green areas for water management", "Intentionally unmanaged areas",
                 "Nature in buildings" )

# Loop through each element in the all_solutions vector
missing_sol_func <- function(miss_sol) {
  missing_keysol <- NA
  
  for (i in seq_along(pivoted_sols$all_solutions[[1]])) {
    current_phrase <- pivoted_sols$all_solutions[[1]][i]
    
    # Check if "House gardens" is found in the current element
    if (miss_sol %in% current_phrase) {
      for (j in 1:i) {
        preceding_phrase <- pivoted_sols$all_solutions[[1]][j]
        if (preceding_phrase %in% sol_phrases) {
          missing_keysol <- preceding_phrase
          break
        }
      }
    }
  }
  
  new_row <- data.frame(solutions = miss_sol, key_solutions = missing_keysol)
  
  filtered_sols <- rbind(filtered_sols, new_row)
  return(filtered_sols)
}

filtered_sols <- missing_sol_func("House gardens")


### uncomment to save rds
#saveRDS(nbs_df, "nbs_df.rds")

nbs_df_long <- nbs_df %>%
  pivot_longer(cols = starts_with("solutions_"),
               names_to = "solution_type",
               values_to = "value") %>%
  dplyr::select(-name, -location, -socio_impacts) %>%
  pivot_longer(cols = starts_with("begin_year"),
               names_to = "variable",
               values_to = "begin_year") %>% 
  mutate(solution_type = str_remove(solution_type, "solutions_"))

nbs_agg <- nbs_df_long %>% 
  group_by(begin_year, solution_type) %>% 
  summarize(solutions = sum(value))

nbs_agged_type <- nbs_agg %>% 
  group_by(solution_type) %>% 
  summarize(solutions = sum(solutions, na.rm = TRUE))


filter_out <- c("unknown", "in planning stage", "pre-1990")
nbs_agg <- nbs_agg %>% 
  filter(!(begin_year %in% filter_out))

nbs_agg$begin_year <- as.factor(nbs_agg$begin_year)

#remove others from dfs
nbs_agg <- nbs_agg %>% 
  filter(solution_type != "Other")

filtered_sols <- filtered_sols %>% 
  filter(solutions != "Other") %>% 
  rename("solution_type" = solutions)

nbs_agg <- nbs_agg %>%
  left_join(filtered_sols, by = "solution_type")

nbs_agged_type <- nbs_agged_type %>%
  left_join(filtered_sols, by = "solution_type")

nbs_agged_sols <- nbs_agged_type %>% 
  group_by(key_solutions) %>% 
  summarize(solutions = sum(solutions, na.rm = TRUE))

### histogram to see distirbution of values to assign factors
# Filter out zeros
non_zero_values <- nbs_agg$solutions[nbs_agg$solutions != 0]

# Calculate the quantiles using non-zero values
jenks_breaks <- classIntervals(non_zero_values, n = 6, style = "jenks")$brks

# Create a histogram of solutions
p <- ggplot(nbs_agg, aes(x = solutions)) +
  geom_histogram(binwidth = 1, fill = "steelblue", color = "white") +
  geom_vline(xintercept = jenks_breaks, linetype = "dashed", color = "red") +
  labs(x = "Solutions", y = "Frequency", title = "Histogram of Solutions with Jenks Quantiles") 

# Display the histogram
p


# Assign factors to solutions
nbs_agg$solution_factors <- cut(nbs_agg$solutions,
                                       breaks = c(0, 1, 2, 5, 9, 14, 20, 34),
                                       include.lowest = TRUE,
                                       labels = c("0", "1-2", "3-5", "6-9", "10-14", "15-20", "21-34"),
                                       right = FALSE)


# Convert begin_year factor
nbs_agg$begin_year <- as.factor(nbs_agg$begin_year)


### uncomment to save rds
#saveRDS(nbs_agg, "nbs_agg.rds")

