library(tidyverse)
setwd(normalizePath(dirname(rstudioapi::getActiveDocumentContext()$path), winslash = "/"))

data_csvs <- list.files('../data/', pattern ="\\.csv$")
data_csvs

if (length(data_csvs) == 0) {
  stop("No CSV files found in the directory.")
}

pilot_df <- data.frame()

# Loop through each CSV file and read it into a temporary dataframe
for (file in data_csvs) {
  temp_df <- read.csv(paste0('../data/',file), header = TRUE)  # Assuming CSV files have headers
  pilot_df <- rbind(pilot_df, temp_df)
}
# pilot_df<- pilot_df%>%filter(subject=='a1bqzyb181')

pilot_df%>%group_by(category)%>%summarize(unique_subjects = n_distinct(subject))

ai_trials <- pilot_df%>%filter(trial_type=='html-keyboard-response')%>%filter(response%in%c('arrowright','arrowleft'))
nrow(ai_trials)
ai_trials<- ai_trials%>%filter(practice!='true')
nrow(ai_trials)
ai_trials <- ai_trials%>%mutate(assignment = ifelse(response == 'arrowright', right, left))

result<-ai_trials%>%group_by(texture_pair,concept,assignment)%>%summarize(count = n())
# result%>%complete(concept, assignment, fill = list(count = 0))

# expand(df, nesting(x, y, z))

tmp<- data.frame()
for(this_pair in unique(result$texture_pair)){
 for(this_concept in unique(result$concept)){
   print(this_concept)
   for(this_answer in unlist(strsplit(this_pair,'_'))){

     tmp<-rbind(tmp,cbind(this_pair,this_concept,this_answer))
   }
   
 }
}
colnames(tmp)<-c('texture_pair','concept','assignment')

t<-result%>%right_join(tmp, by = c('texture_pair','concept','assignment'))%>%replace_na(list(count = 0))
t$count<- t$count/(4*length(unique(pilot_df$subject)))

write_csv(t,'../data/visual_texture_assignment_results.csv')


# # Group by A and B, then count occurrences of each value in column C
# result <- df %>%
#   group_by(A, B, C) %>%
#   summarize(count = n()) %>%
#   # Left join with the reference data frame
#   left_join(reference_df, by = c("A", "B", "C")) %>%
#   # Replace NA with 0 in the count column
#   replace_na(list(count = 0))
# 
# # Print the result
# print(result)

ggplot(data=t,aes(x=assignment, y=count))+
  geom_bar(stat='identity')+
  facet_grid(rows=vars(texture_pair), cols=vars(concept),scales='free')+
  ylim(0,.8)

