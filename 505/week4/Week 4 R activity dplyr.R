#Week 4: dplyr package

library(datasets)

#Task: Write the function to get a dataset from Base R: Titanic
#and give the dataframe a new name of your choice
#(hint: you will want your data to be a dataframe. Use the function: as.data.frame(Titanic))

gen_titanic <- function(){
 df_titanic_ <-  as.data.frame(Titanic)
 return(df_titanic_)
}

df_titanic <- gen_titanic()

#See the top rows of the data
#TASK: Write the function to see the top rows of the data

top2_view <- function() {
  df_titanic_ <- gen_titanic()
  return(print(head(df_titanic_,2))
         )
}

top2_view()
# > top2_view()
# Class  Sex   Age Survived Freq
# 1   1st Male Child       No    0
# 2   2nd Male Child       No    0

#Install and call the package dplyr
#TASK: Write the functions to install and call dplyr

library_install_call <- function(library_name){
    if (!require(library_name , character.only = TRUE)) {
      install.packages(library_name)
      library(library_name, character.only = TRUE)
    }
  }

library_install_call("dplyr")

#Let's just see the Survived and Sex columns
#Task: Write the function to 'select' the Survived and Sex columns 
#(hint: use the 'select' function)

survived_sex_select <- function(df, col1, col2) {
  selected_df <- df %>% 
    select(all_of(c(col1, col2)))
  return(selected_df)
}


#Let's name the dataset with just the two columns, Survived and Sex
#TASK: Write the function to save the two columns as one new dataset
#and give it a name
df_titanic <- gen_titanic()
df_selected_cols <- survived_sex_select(df_titanic, 'Survived', 'Sex')

# > df_selected_cols
# Survived    Sex
# 1        No   Male
# 2        No   Male
# 3        No   Male
# 4        No   Male
# 5        No Female
# 6        No Female

#Let's get rid of the Sex column in the new dataset created above
#TASK: Write the function that deselects the sex column
#(hint: use the 'select' function to not select a -column)

remove_cols <- function(df, colname){
  col_removed_df <- df %>% 
    select(-all_of(colname))
  return(col_removed_df)
}

removed_col_df <- remove_cols(df_titanic,'Sex')   

# > removed_col_df
# Class   Age Survived Freq
# 1    1st Child       No    0
# 2    2nd Child       No    0
# 3    3rd Child       No   35
# 4   Crew Child       No    0

#Let's rename a column name
#TASK: Write the function that renames 'Sex' to 'Gender'
df_titanic <- gen_titanic()

new <- c('Gender')
old <- c('Sex')
col_renamer <- function(df, new, old){
  df_titanic <- df_titanic %>% rename_with(~ new, all_of(old))
  return(df_titanic)
}

col_renamer(df_titanic, new, old)

# Output
# Class Gender   Age Survived Freq
# 1    1st   Male Child       No    0
# 2    2nd   Male Child       No    0
# 3    3rd   Male Child       No   35
# 4   Crew   Male Child       No    0

#Let's make a new dataframe with the new column name
#TASK: Write the function that names a new dataset that includes the 'gender' column

df_titanic <- gen_titanic()

new <- c('Gender')
old <- c('Sex')
col_renamer <- function(df, new, old){
  df_renamed <- df_titanic %>% rename_with(~ new, all_of(old))
  return(df_renamed)
}

df_titanic_renamed <- col_renamer(df_titanic, new, old)
# > df_titanic_renamed
# Class Gender   Age Survived Freq
# 1    1st   Male Child       No    0
# 2    2nd   Male Child       No    0
# 3    3rd   Male Child       No   35
# 4   Crew   Male Child       No    0

#Let's 'filter' just the males from our dataset
#TASK: Write the function that includes only rows that are 'male'

df_male_filter <- function(df, col, val){
  col <- rlang::sym(col)
  df_filtered <- df %>% filter(!!col == val)
  return(df_filtered)
}

df_titanic_male <- df_male_filter(df_titanic, 'Sex', 'Male')


#Let's 'arrange' our data by gender (not the data you just filtered)
#TASK: Write the function to group the data by gender (hint: arrange())

arrange_gen <- function(df, arrange_col){
  df_arranged <- df %>% arrange(!!rlang::sym(arrange_col))
}

df_titanic <- gen_titanic()
new <- c('Gender')
old <- c('Sex')

df_titanic_renamed <- col_renamer(df_titanic, new, old)
df_titanic_arranged <- arrange_gen(df_titanic_renamed, 'Gender')

# > df_titanic_arranged
# Class Gender   Age Survived Freq
# 1    1st   Male Child       No    0
# 2    2nd   Male Child       No    0
# 3    3rd   Male Child       No   35
# 4   Crew   Male Child       No    0
# 5    1st   Male Adult       No  118

#Let's see how many people were examined in the dataset (total the frequency in the original dataframe)
#TASK: Sum the Freq column
df_titanic <- gen_titanic()
df_titanic %>%
  summarise(Total_freq = sum(Freq))
#TASK: After you run it, write the total here:2201

#Since we have a males dataset, let's make a females dataset
#TASK: Write the function that includes only rows that are 'female'
df_female_filter <- function(df, col, val){
  col <- rlang::sym(col)
  df_filtered <- df %>% filter(!!col == val)
  return(df_filtered)
}

df_titanic_female <- df_female_filter(df_titanic, 'Sex', 'Female')

# > df_titanic_female
# Class    Sex   Age Survived Freq
# 1    1st Female Child       No    0
# 2    2nd Female Child       No    0
# 3    3rd Female Child       No   17
# 4   Crew Female Child       No    0
# 5    1st Female Adult       No    4

#And now let's join the males and females
#TASK: Write the function that joins the male and female rows 
#(hint: try using 'union' or 'bind_rows')

join_male_female <- function(df_male, df_female){
  df_combined <- bind_rows(df_male, df_female)
  return(df_combined)
}

df_titanic_male <- df_male_filter(df_titanic, 'Sex', 'Male')
df_titanic_female <- df_female_filter(df_titanic, 'Sex', 'Female')

df_titanic_combined <- join_male_female(df_titanic_male, df_titanic_female)

# > df_titanic_combined
# Class    Sex   Age Survived Freq
# 1    1st   Male Child       No    0
# 2    2nd   Male Child       No    0
# 3    3rd   Male Child       No   35
# 4   Crew   Male Child       No    0
# 5    1st   Male Adult       No  118
# 6    2nd   Male Adult       No  154
# 7    3rd   Male Adult       No  387
# 8   Crew   Male Adult       No  670
# 9    1st   Male Child      Yes    5
# 10   2nd   Male Child      Yes   11
# 11   3rd   Male Child      Yes   13
# 12  Crew   Male Child      Yes    0
# 13   1st   Male Adult      Yes   57
# 14   2nd   Male Adult      Yes   14
# 15   3rd   Male Adult      Yes   75
# 16  Crew   Male Adult      Yes  192
# 17   1st Female Child       No    0
# 18   2nd Female Child       No    0
# 19   3rd Female Child       No   17
# 20  Crew Female Child       No    0
# 21   1st Female Adult       No    4


#Optional Task: add any of the other functions 
#you learned about from the dplyr package

# group_by
df_titanic %>% 
  group_by(Survived) %>%
  summarise(sum = sum(Freq), n=n())%>%
  filter(n!= 0)