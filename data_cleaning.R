library(timeDate)
library(stringr)
data_cleaning <- function() {
  
  #Load the movies data 
  movie_df <- as.data.frame(movie_data) %>% 
  select(id, title, genres, popularity, production_companies,
           release_date, budget, revenue, runtime, status,
           vote_average, vote_count, credits)
  
  
  #fill blank release date values with NA
  movie_df$release_date[movie_df$release_date == ""] <- NA
  
  movie_df$budget <- ifelse(is.na(movie_df$budget), 0, movie_df$budget)
  movie_df$revenue <- ifelse(is.na(movie_df$revenue), 0, movie_df$revenue)
  
  movie_cleaned_df <- filter(movie_df, budget > 999, revenue > 999, (status %in% c("Released")),!(budget %in% c(5000000000, 800000000)))
  movie_cleaned_df$genres <- replace(movie_cleaned_df$genres, is.na(movie_cleaned_df$genres), "XXXXX")

  #create a genre map to assign numbers to the categories 
  #genre_map <- c(Action = 1, Adventure = 2, Animation = 3, Comedy = 4, Crime = 5,
  #               Documentary = 6, Drama = 7, Family = 8, Fantasy = 9, History = 10,
  #              Horror = 11, Music = 12, Mystery = 13, Romance = 14, 
  #               `Science Fiction` = 15, Thriller = 16, `TV Movie` = 17,
  #               War = 18, Western = 19, XXXXX = 0)
  
  genre_map <- c(Action = "Action", Adventure = "Adventure", Animation = "Animation", Comedy = "Comedy", Crime = "Crime",
                 Documentary = "Documentary", Drama = "Drama", Family = "Family", Fantasy = "Fantasy", History = "History",
                 Horror = "Horror", Music = "Music", Mystery = "Mystery", Romance = "Romance", 
                 `Science Fiction` = "Science Fiction", Thriller = "Thriller", `TV Movie` = "TV Movie",
                 War = "War", Western = "Western", XXXXX = "XXXXX")
  
  # Replace genre strings with their corresponding numbers
  movie_cleaned_df$genre_category <- case_when(
    grepl("^Action", movie_cleaned_df$genres) ~ genre_map["Action"],
    grepl("^Adventure", movie_cleaned_df$genres) ~ genre_map["Adventure"],
    grepl("^Animation", movie_cleaned_df$genres) ~ genre_map["Animation"],
    grepl("^Comedy", movie_cleaned_df$genres) ~ genre_map["Comedy"],
    grepl("^Crime", movie_cleaned_df$genres) ~ genre_map["Crime"],
    grepl("^Documentary", movie_cleaned_df$genres) ~ genre_map["Documentary"],
    grepl("^Drama", movie_cleaned_df$genres) ~ genre_map["Drama"],
    grepl("^Family", movie_cleaned_df$genres) ~ genre_map["Family"],
    grepl("^Fantasy", movie_cleaned_df$genres) ~ genre_map["Fantasy"],
    grepl("^History", movie_cleaned_df$genres) ~ genre_map["History"],
    grepl("^Horror", movie_cleaned_df$genres) ~ genre_map["Horror"],
    grepl("^Music", movie_cleaned_df$genres) ~ genre_map["Music"],
    grepl("^Mystery", movie_cleaned_df$genres) ~ genre_map["Mystery"],
    grepl("^Romance", movie_cleaned_df$genres) ~ genre_map["Romance"],
    grepl("^Science Fiction", movie_cleaned_df$genres) ~ genre_map["Science Fiction"],
    grepl("^Thriller", movie_cleaned_df$genres) ~ genre_map["Thriller"],
    grepl("^TV Movie", movie_cleaned_df$genres) ~ genre_map["TV Movie"],
    grepl("^War", movie_cleaned_df$genres) ~ genre_map["War"],
    grepl("^Western", movie_cleaned_df$genres) ~ genre_map["Western"],
    TRUE ~ genre_map["XXXXX"]
  )
  
  #creating column called title_length, total_cast,total_production_companies for calculating title length, total cast and crew and number of production companies
  movie_cleaned_df$title_length <- nchar(movie_cleaned_df$title)
  movie_cleaned_df$total_cast <- str_count(movie_cleaned_df$credits, "-") + 1
  movie_cleaned_df$total_production_companies <- str_count(movie_cleaned_df$production_companies, "-") + 1
  
  #Return the preprocessed data frame
  return(movie_cleaned_df)
}






