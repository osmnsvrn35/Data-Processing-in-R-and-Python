import pandas as pd 



def questions_1(Users, Posts):
    # Merge user demographics with posts data and filter for questions (PostType == 1)
    merged_data = pd.merge(Posts[Posts['PostTypeId'] == 1], Users[['Id', 'Location']], left_on='OwnerUserId', right_on='Id', how='left')
 
    # Get the count of each unique location in the 'Location' column
    location_counts = merged_data['Location'].value_counts()
    
    # Create a DataFrame with locations and their counts
    locations_df = pd.DataFrame({'Location': location_counts.index, 'Count': location_counts.values})
 
    return locations_df.head(10)

def questions_2(Users,Comments,Posts):
   # Merge Comments and Users DataFrames on UserId
    merged_comments_df = pd.merge(Comments, Users, left_on='UserId', right_on='Id', how='inner')
    # Merge Posts and Users DataFrames on OwnerUserId to get information about user answers
    merged_answers_df = pd.merge(Posts[Posts['PostTypeId'] == 2], Users, left_on='OwnerUserId', right_on='Id', how='inner')
    # Group by user reputation and calculate the count of comments for each user
    user_comments_count = merged_comments_df.groupby('Reputation')['Id_x'].count().reset_index()
    user_comments_count.columns = ['Reputation', 'CommentsCount']
    # Group by user reputation and calculate the count of answers for each user
    user_answers_count = merged_answers_df.groupby('Reputation')['Id_x'].count().reset_index()
    user_answers_count.columns = ['Reputation', 'AnswersCount']
    # Merge the two DataFrames on Reputation
    user_combined_count = pd.merge(user_comments_count, user_answers_count, on='Reputation', how='outer')
    # Fill NaN values with 0 (users with either comments or answers, not both)
    user_combined_count = user_combined_count.fillna(0)
    # Sort the DataFrame in decreasing order by user reputation
    user_combined_count_sorted = user_combined_count.sort_values(by='Reputation', ascending=False)
    # Reset the index so that it starts from 0
    user_combined_count_sorted = user_combined_count_sorted.reset_index(drop=True)

    return user_combined_count_sorted.head(15)

