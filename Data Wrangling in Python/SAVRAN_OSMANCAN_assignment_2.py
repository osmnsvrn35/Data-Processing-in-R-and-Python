### Data Processing in R and Python 2023Z
### Homework Assignment no. 2
###
### IMPORTANT
### This file should contain only solutions to tasks in the form of a functions
### definitions and comments to the code.
###
#

import pandas as pd
# -----------------------------------------------------------------------------#
# Task 1
# -----------------------------------------------------------------------------#


def solution_1(Posts, Users):

    #Starting with finding the inner part of the query, I staretd applying the inner join
    inner_joined_1 = pd.merge(Posts,Users,how='inner',left_on='OwnerUserId', right_on='Id')
    #and selected the required columns ( I did not rename the Id_x because it will not be in result data frame
    inner_selected_1 = inner_joined_1[['OwnerUserId', 'Id_x', 'Location']]

    #After inner part I applied the filter using query function, simply use the notna()
    filtered_1 = inner_selected_1.query('Location.notna()')
    #Groupped by the Location and chained size function to it in order to calculate the size of
    #each location group's size and reset the indexes renamed the column as Count.
    groupped_by_1 = filtered_1.groupby(by='Location').size().reset_index(name='Count')
    #Ordered the data frame by Count numbers by ascending order
    ordered_by_1 = groupped_by_1.sort_values(by='Count', ascending=False)
    #finally choosing the first 10 element I finished the query.Additionally I reset the indexes of the rows
    #in order to make them sequential
    result_1 = ordered_by_1.head(10).reset_index(drop=True)
    return result_1

# -----------------------------------------------------------------------------#
# Task 2
# -----------------------------------------------------------------------------#

def solution_2(Posts, PostsLinks):

    #Firstly I created the related table dataframe grouping by relatedpostId and calcauted the row count
    RelatedTab_2 = pd.DataFrame(PostsLinks.groupby('RelatedPostId').size().reset_index(name='NumLinks'))

    #After that I applied the inner join
    join_RelatedTab_2 = pd.merge(RelatedTab_2,Posts , left_on='RelatedPostId', right_on='Id', how='inner').reset_index(drop=True)
    #Basically filtered
    filter_2 = join_RelatedTab_2.query('PostTypeId == 1')
    #as last I selected the required columns and sorted with descending order
    result_2 = filter_2[['Title', 'NumLinks']].sort_values(by='NumLinks',ascending=False).reset_index(drop= True)
    return result_2


# -----------------------------------------------------------------------------#
# Task 3
# -----------------------------------------------------------------------------#

def solution_3(Posts, Users, Comments):

    #Firstly,I started creagin CmtToScr Dataframe I groupped by PostId and the take the some of scores to find totalscore
    CmtToScr_group_3 = Comments.groupby('PostId')['Score'].sum().reset_index()
    #I renamed the Score column as CommentsTotalScore
    CmtToScr_3 = CmtToScr_group_3.rename(columns = {'Score':'CommentsTotalScore'})

    #I joined CmtToScore and Posts
    PostBestComments_3 = pd.merge(CmtToScr_3,Posts,left_on='PostId',right_on='Id',how='inner')
    #I applied the filter to the result of the join
    PostBestComments_3 = PostBestComments_3.query('PostTypeId == 1')
    #and selected the necessary columns so that PostsBestComments data frame created
    PostBestComments_3 = PostBestComments_3[['OwnerUserId','Title','CommentCount','ViewCount','CommentsTotalScore']]

    #I joined PostBestComments with Users
    last_join_3 = pd.merge(PostBestComments_3,Users,left_on='OwnerUserId',right_on='Id',how='inner')
    #Then I ordered by commentstotalscore in descreasing order and selected the first 10 elements
    ordered_limited_3 = last_join_3.sort_values(by='CommentsTotalScore',ascending=False)
    #Finally I chose the necessary columns
    result_3 = ordered_limited_3[['Title','CommentCount','ViewCount','CommentsTotalScore','DisplayName','Reputation','Location']].reset_index(drop=True)
    return result_3

# -----------------------------------------------------------------------------#
# Task 4
# -----------------------------------------------------------------------------#

def solution_4(Posts, Users):
    #Firstly I start with creating the Answers DF. Firstly I filtered by 2.
    filter_answers_4 = pd.DataFrame(Posts.query('PostTypeId == 2'))
    #After that I groupped by OwnerUserId and found the count using size() method
    answers_4 = filter_answers_4.groupby('OwnerUserId').size().reset_index(name='AnswersNumber')

    #Exact same things are done for questions table as I did for Answers
    filter_questions_4 = pd.DataFrame(Posts.query('PostTypeId == 1'))
    questions_4 = filter_questions_4.groupby('OwnerUserId').size().reset_index(name='QuestionsNumber')

    #I inner-joined the Answer and Questions on OwnerUserId
    ans_ques_join_4 = pd.merge(answers_4, questions_4, on='OwnerUserId', how='inner')
    #in this line I chained 3 operations firstly applied the filter, ordered by decreasing order and took the first 5 elements
    PostsCounts_4 = ans_ques_join_4.query('AnswersNumber > QuestionsNumber').sort_values(by='AnswersNumber', ascending=False).head(5)

    #Finally I applied the join between the dataframe I create so far with the Users DF.
    result_join_4 = pd.merge(PostsCounts_4,Users,left_on='OwnerUserId', right_on='Id', how='inner')
    #and Selected the required columns
    result_4 = result_join_4[["DisplayName","QuestionsNumber","AnswersNumber","Location","Reputation","UpVotes","DownVotes"]]
    return result_4

# -----------------------------------------------------------------------------#
# Task 5
# -----------------------------------------------------------------------------#

def solution_5(Posts):

    #Firsly I created the bestanswers table applying filter
    filter_best_ans_5 = pd.DataFrame(Posts.query('PostTypeId == 2'))
    #I groupped by ParentId and applied the max method ot score in order to Find MaxScore
    groupped_BestAnswers_5 = filter_best_ans_5.groupby('ParentId')['Score'].max().reset_index()
    #I renamed the Score Column as MaxScore
    BestAnswers_5 = groupped_BestAnswers_5.rename(columns = {'Score' : 'MaxScore'})

    #Then I created the Questions Dataframe only requires filter
    Questions_5 = pd.DataFrame(Posts.query('PostTypeId == 1'))

    #I joined those 2 tables
    ans_ques_join_5 = pd.merge(Questions_5,BestAnswers_5,left_on='Id',right_on='ParentId')

    #Then I joined the result of the previous join with Posts
    other_join_5 = pd.merge(Posts,ans_ques_join_5,left_on='Id',right_on='AcceptedAnswerId',how='inner')
    #I created another column named as Difference using as a diffrence of MaxScore and Posts.Score
    other_join_5['Difference']  = other_join_5['MaxScore'] - other_join_5['Score_x']
    #I filtered the Differences values which are greater than 50 and sorted them in decresasing order and selected the required columns
    almost_result_5 = other_join_5.query('Difference > 50').sort_values(by='Difference',ascending=False)[['Id_y', 'Title_y', 'MaxScore', 'Score_x', 'Difference']]
    #Finally I renamed the column names
    result_5 = almost_result_5.rename(columns = {'Id_y' : 'Id','Title_y' : 'Title','Score_x' : 'AcceptedScore'}).reset_index(drop=True)

    return result_5



