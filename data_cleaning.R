
data_cleaning <- function() {
  # Load the movies data 
  movie_df <- as.data.frame(movie_data) %>% 
    select(id, title, genres, popularity, production_companies,
           release_date, budget, revenue, runtime, status,
           vote_average, vote_count, credits)
  
  movie_cleaned_df <- filter(movie_df, budget > 0, revenue > 0, !(budget %in% c(5000000000, 800000000)))
  
  # Return the preprocessed data frame
  return(movie_cleaned_df)
}






