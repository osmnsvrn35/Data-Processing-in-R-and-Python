### Data Processing in R and Python 2023Z
### Homework Assignment no. 1
###
### IMPORTANT
### This file should contain only solutions to tasks in the form of a functions
### definitions and comments to the code.
###
### Report should include:
### * source() of this file at the beggining,
### * reading the data, 
### * attaching the libraries, 
### * execution time measurements (with microbenchmark),
### * and comparing the equivalence of the results,
### * interpretation of queries.


sqldf_1 <- function(Posts, Users){
  # Input the solution here
  #
  
 result_sqldf1<-sqldf(
   "
  SELECT Location, COUNT(*) AS Count
  FROM (
    SELECT Posts.OwnerUserId, Users.Id, Users.Location
    FROM Users
    JOIN Posts ON Users.Id = Posts.OwnerUserId
  ) AS subquery
  WHERE Location NOT IN ('')
  GROUP BY Location
  ORDER BY Count DESC
  LIMIT 10
")
  return(result_sqldf1)
}

base_1 <- function(Posts, Users){
  # Input the solution here
  #
  #I start with realizing the in the inner part of the FROM clause
  user_post_join <- merge(Users,Posts,by.x = "Id",by.y = "OwnerUserId") 
  #filtering out the empty locations
  location_filter <- user_post_join[user_post_join$Location != '',]
  #table will count each occurence of location 
  location_count <-table(location_filter$Location)
  #I am creating the data.frame by the data I had by count and naming the columns
  df<-data.frame(Location = names(location_count),Count = as.numeric(location_count))
  #Finally applying the decreasing order and the choosing the first 10 rows
  df<-df[order(df$Count,decreasing = TRUE),]
  result_base1 <- head(df,10)
  #It will fix the IDs to  sequential
  row.names(result_base1) <- NULL
  ##compare(result_base1,result_sqldf1,allowAll = TRUE)
  return(result_base1)
}

dplyr_1 <- function(Posts, Users){
  # Input the solution here
  # 
  #Firstly I start with applying join in the inner part of FROM clause
  joined_tables_dplyr1 <- inner_join(Posts,Users,by=c("OwnerUserId"="Id"))
  #by using pipelining I select the necessary columns
  joined_tables_dplyr1%>%
  select(OwnerUserId,Id,Location)
  #finally I use the result of the inner part of FROM cluase and applying next steps
  
  result_dplyr1<-joined_tables_dplyr1 %>%
  #I basically use dpylr functions to filter,choosing first 10 elements and groupping by location
    filter(Location != "")%>%
    group_by(Location) %>%
    summarise(Count = n())%>%
    arrange(desc(Count))%>%
    slice_head(n=10)
  #compare(result_dplyr1,result_sqldf1,allowAll = TRUE)
  return(result_dplyr1)
  }

data.table_1 <- function(Posts, Users){
  # Input the solution here
  # 
  # Converting the DFs into data.table
  Posts<-data.table(Posts)
  Users<-data.table(Users)
  #I start by finding the table (inside of the first FROM)
  #I will realize the JOIN
  joined_tables_DT1<-Posts[Users, on = .(OwnerUserId = Id),nomatch=0]
  #now I select the necessary columns and tab will be created
  tab_DT1<-joined_tables_DT1[, .(OwnerUserId,Id,Location)]
  #now I will remove empty locations 
  filtered_DT1<-tab_DT1[!Location %in% c("")]
  #Group by is applied
  groupby_DT1 <- filtered_DT1[ , .(Count = .N),by= Location]
  #applying the descending order by count and the LIMIT 10
  ordered_DT1 <- setorder(groupby_DT1,-Count)
  result_DT1 <- ordered_DT1[1:10]
  #compare(result_DT1,result_sqldf1,allowAll = TRUE)
  return(result_DT1)
}

# -----------------------------------------------------------------------------#
# Task 2
# -----------------------------------------------------------------------------#

sqldf_2 <- function(Posts, PostsLinks){
  # Input the solution here
  # 
  tab<-sqldf("SELECT RelatedPostId AS PostId, COUNT(*) AS NumLinks
        FROM PostsLinks
        GROUP BY RelatedPostId")
  result_sqldf2<-sqldf("
  SELECT Posts.Title, RelatedTab.NumLinks
  FROM(
        tab
      ) AS RelatedTab
  JOIN Posts ON RelatedTab.PostId=Posts.Id
  WHERE Posts.PostTypeId=1
  ORDER BY NumLinks DESC
")
 
  return(result_sqldf2)
}

base_2 <- function(Posts, PostsLinks){
  # Input the solution here
  #
  #I start with making the RelatedTab table
  #Firstly ,I group the data by RelatedPostId
  tab_base2<-table(PostsLinks$RelatedPostId)
  #tab_base2 contains vectors first element is Id second is NumLink I will create dataframe of them
  tab_base2_df <- data.frame(tab_base2)
  colnames(tab_base2_df)<- c("PostId", "NumLinks")
  
  #Performing the inner join
  joined_df_base2 <- merge(tab_base2_df,Posts,by.x="PostId",by.y="Id")
  #applying the Where clause
  filtered_df_base2<-joined_df_base2[joined_df_base2$PostTypeId == 1,]
  #Ordering by descending NumLink order
  order_df_base2 <- filtered_df_base2[order(filtered_df_base2$NumLinks,decreasing = TRUE),]
  #Selecting only numlinks and the titles 
  result_base2 <- data.frame(order_df_base2$Title,order_df_base2$NumLinks)
  colnames(result_base2)<-c("Title","NumLinks")
  return(result_base2)
}

dplyr_2 <- function(Posts, PostsLinks){
  # Input the solution here
  # 
  #I start with creating the RelatedTab Table
  RelatedTab_dplyr2<-PostsLinks%>%
    #creating the tab part
    group_by(PostId = RelatedPostId)%>%
    summarise(NumLinks = n())
  
    #RelatedTab table is created now I can apply inner join
  joined_dplyr2<-inner_join(RelatedTab_dplyr2,Posts,by=c("PostId"="Id"))
  result_dplyr2<-joined_dplyr2 %>%
    #where cluase and order will be applied
    filter(PostTypeId == 1)%>%
    arrange(desc(NumLinks))%>%
    #selecting the title and numlinks
    select(Title,NumLinks)
    return(result_dplyr2)
 
}


data.table_2 <- function(Posts, PostsLinks){
  # Input the solution here
  # 
  
  PostsLinks_DT <- data.table(PostsLinks)
  Posts_DT<- data.table(Posts)
  #Firstly I start with finding the RelatedTab table
  # Group by RelatedPostId and count occurrences
  RelatedTab_DT2 <- PostsLinks_DT[, .(NumLinks = .N), by = RelatedPostId]
  # Rename the columns as per your SELECT statement
  setnames(RelatedTab_DT2, c("PostId", "NumLinks"))
  
  #Now we can realize the join since we have created the RelatedTab
  joined_tables_DT2 <- Posts_DT[RelatedTab_DT2, on = .(Id= PostId ), nomatch = 0]
  #I will filter the PosttypeId==1 
  filtered_DT2<-joined_tables_DT2[PostTypeId %in% c("1")]
  #I will use setorder function to order it decreasingly
  setorder(filtered_DT2,-NumLinks)
  #Finally I will select the necessary columns
  result_DT2<-filtered_DT2[, .(Title,NumLinks)]
  return(result_DT2)
 
  }

# -----------------------------------------------------------------------------#
# Task 3
# -----------------------------------------------------------------------------#

sqldf_3 <- function(Posts, Users, Comments){
  # Input the solution here
  # 
  
sqldf("SELECT Title, CommentCount, ViewCount, CommentsTotalScore, DisplayName, Reputation, Location
      FROM (
          SELECT Posts.OwnerUserId, Posts.Title, Posts.CommentCount, Posts.ViewCount,
          CmtTotScr.CommentsTotalScore
            FROM (
              SELECT PostId, SUM(Score) AS CommentsTotalScore
              FROM Comments
              GROUP BY PostId
              ) AS CmtTotScr
            JOIN Posts ON Posts.Id = CmtTotScr.PostId
            WHERE Posts.PostTypeId=1
            ) AS PostsBestComments
      JOIN Users ON PostsBestComments.OwnerUserId = Users.Id
      ORDER BY CommentsTotalScore DESC
      ")->tab3
 result_sqldf3<-sqldf("SELECT * FROM tab3 LIMIT 10") 
 return(result_sqldf3)
}

base_3 <- function(Posts, Users, Comments){
  # Input the solution here
  # 
  #group by post ID (CmtToScr creation)
  inner_tab_base3<-aggregate(Score ~ PostId,data=Comments,FUN=sum)
  colnames(inner_tab_base3) <- c("PostId","CommentsTotalScore")
  
  #now I will implement next select firstly I will start with join and filtering
  joined_cmttoscr_base3 <- merge(Posts,inner_tab_base3,by.x="Id",by.y="PostId")
  filtered_cmttoscr_base3 <- joined_cmttoscr_base3[joined_cmttoscr_base3$PostTypeId==1,]
  #applying the select clause
  bestcomments_base3 <- filtered_cmttoscr_base3[c("OwnerUserId", "Title", "CommentCount", "ViewCount", "CommentsTotalScore")]
  
  #now PostsBestComments is created and I will apply join and order by
  join_bestcomments_base3 <- merge(Users,bestcomments_base3,by.x = "Id",by.y = "OwnerUserId")
  ordered_bestcomments_base3<- join_bestcomments_base3[order(join_bestcomments_base3$CommentsTotalScore,decreasing = TRUE),]
  limited_bestcomments_base3<- head(ordered_bestcomments_base3,10)
  #now I will select 
  result_base3<-limited_bestcomments_base3[c("Title","CommentCount","ViewCount","CommentsTotalScore","DisplayName","Reputation","Location")]
  #I will fix row IDs to sequential
  row.names(result_base3) <- NULL
  return(result_base3)
  }

dplyr_3 <- function(Posts, Users, Comments){
  # Input the solution here
  # 
  #I start by creating the CmtToScr table 
  CmtToScr_dplyr3<-Comments %>%
    group_by(PostId)%>%
    summarise(CommentsTotalScore = sum(Score))
 
   #As we created the table we can now apply the join
  CmtToScr_joined_dplyr3<-inner_join(Posts,CmtToScr_dplyr3,by=c("Id"="PostId"))
  #here I apply filter and selecting the necessary columns to create PostBestComments table
  PostBestComments_dplyr3<-CmtToScr_joined_dplyr3 %>%
    filter(PostTypeId == 1)%>%
    select(OwnerUserId,Title,CommentCount,ViewCount,CommentsTotalScore)
  
  #As we created PostBestComments we can apply the join
  PostBestComments_joined_dplyr3<- inner_join(PostBestComments_dplyr3,Users,by=c("OwnerUserId"="Id"))
  #Finally I apply descreasing order by CommentsTotalScore, limiting with first 10 element and selecting columns
  result_dplyr3<-PostBestComments_joined_dplyr3 %>%
    arrange(desc(CommentsTotalScore))%>%
    slice_head(n=10)%>%
    select(Title,CommentCount,ViewCount,CommentsTotalScore,DisplayName,Reputation,Location)
  return(result_dplyr3) 
}

data.table_3 <- function(Posts, Users, Comments){
  # Input the solution here
  # 
  Posts_DT<-data.table(Posts)
  Users_DT<-data.table(Users)
  Comments_DT<-data.table(Comments)
  #I start solving it by finding the CmtTotScr table
  # Group by PostId and Sum Scores
  CmtTotScr_DT3 <- Comments_DT[, .(CommentsTotalScore = sum(Score)), by = PostId]
  #We created the CmtTotScr table now we make join
  
  CmtTotScr_join_DT3 <- Posts_DT[CmtTotScr_DT3, on = .(Id= PostId ), nomatch = 0]
  #And filter by postTypeId
  CmtTotScr_filtered_DT3<-CmtTotScr_join_DT3[PostTypeId %in% c("1")]  
  #now we select the necesassry columns and PostBestComments table will be created
  PostBestComments_DT3<-CmtTotScr_filtered_DT3[, .(OwnerUserId,Title,CommentCount,
                                                   ViewCount,CommentsTotalScore)]
  
  #As we created the PostBestComments table we realize the join
  joined_DT3 <- Users_DT[PostBestComments_DT3, on = .( Id = OwnerUserId), nomatch = 0]
  #Now I order it decreasing by commentstotalscore and limit by 10
  setorder(joined_DT3,-CommentsTotalScore)
  filtered_DT3<-joined_DT3[1:10]
  #finally selecting the result table's columns
  result_DT3<-filtered_DT3[, .(Title,CommentCount, ViewCount, 
                               CommentsTotalScore, DisplayName, Reputation, Location)]
  
  return(result_DT3)
  }

# -----------------------------------------------------------------------------#
# Task 4
# -----------------------------------------------------------------------------#

sqldf_4 <- function(Posts, Users){
  # Input the solution here
  # 
  result_sqldf4<-sqldf("
SELECT DisplayName, QuestionsNumber, AnswersNumber, Location, Reputation, UpVotes, DownVotes
FROM (
SELECT *
FROM (
SELECT COUNT(*) as AnswersNumber, OwnerUserId
FROM Posts
WHERE PostTypeId = 2
GROUP BY OwnerUserId
) AS Answers
JOIN
(
SELECT COUNT(*) as QuestionsNumber, OwnerUserId
FROM Posts
WHERE PostTypeId = 1
GROUP BY OwnerUserId
) AS Questions
ON Answers.OwnerUserId = Questions.OwnerUserId
WHERE AnswersNumber > QuestionsNumber
ORDER BY AnswersNumber DESC
LIMIT 5
) AS PostsCounts
JOIN Users
ON PostsCounts.OwnerUserId = Users.Id")
  return(result_sqldf4)
}

base_4 <- function(Posts, Users){
  # Input the solution here
  # 
  #I start by creating Answers table first I apply the filter
  answers_base4_filter<- Posts[Posts$PostTypeId == 2,]
  #I use table in order to get count of ownerUserIds 
  answers_base4_count <- table(answers_base4_filter$OwnerUserId)
  #I put them in a data.frame
  answers_base4_DF<-data.frame(answers_base4_count)
  #as last Im naming the columns
  colnames(answers_base4_DF)<-c("OwnerUserId","AnswersNumber")
  
  #I will apply exact same method for Questions table 
  questions_base4_filter<- Posts[Posts$PostTypeId == 1,]
  questions_base4_count <- table(questions_base4_filter$OwnerUserId)
  questions_base4_DF<-data.frame(questions_base4_count)
  colnames(questions_base4_DF)<-c("OwnerUserId","QuestionsNumber")
  
  #Since we created the tables we can apply the join
  questions_base4_join<-merge(questions_base4_DF,answers_base4_DF,by =("OwnerUserId"))
  #Now I filter it as AnswersNumber > QuestionsNumber
  PostCounts_filtered_base4<-questions_base4_join[questions_base4_join$AnswersNumber > questions_base4_join$QuestionsNumber,]
  #Finally we order it in decreasing order by AnswersNumber and take the first 5 elements so that PostsCount table created
  PostCounts_base4<-PostCounts_filtered_base4[order(PostCounts_filtered_base4$AnswersNumber,decreasing = TRUE),]
  PostsCounts_base4<-head(PostCounts_base4,5)
  
  #Since we created PostsCounts we can apply the last join in query
  result_join_base4 <- merge(Users,PostsCounts_base4,by.x = "Id",by.y = "OwnerUserId")
  #Finally I choose the necessary columns to create result table
  result_base4<-result_join_base4[c("DisplayName","QuestionsNumber","AnswersNumber",
                                    "Location","Reputation","UpVotes","DownVotes")]
  return(result_base4)
  
   }

dplyr_4 <- function(Posts, Users){
  # Input the solution here 
  # 
  #I start with find Answer DF
  Answers_dplyr4 <- Posts %>%
    filter(PostTypeId == 2)%>%
    group_by(OwnerUserId)%>%
    summarise(AnswersNumber = n())
 
   #Now lets find the Questions DF
  Questions_dplyr4<- Posts%>%
    filter(PostTypeId == 1)%>%
    group_by(OwnerUserId)%>%
    summarise(QuestionsNumber = n())
  #Lets make the join
  join_questions_dplyr4<- inner_join(Questions_dplyr4,Answers_dplyr4,by=c("OwnerUserId"="OwnerUserId"))
  PostsCounts_dplyr4<-join_questions_dplyr4%>%
    filter(AnswersNumber > QuestionsNumber)%>%
    arrange(desc(AnswersNumber)) %>%
    filter(!is.na(OwnerUserId)) %>%
    slice_head(n=5)
  join_postcounts_dplyr4<-inner_join(PostsCounts_dplyr4,Users,by=c("OwnerUserId"="Id"))
  result_dplyr4<-join_postcounts_dplyr4%>%
    select(DisplayName,QuestionsNumber,AnswersNumber,Location,Reputation,UpVotes,DownVotes)
  
  return(result_dplyr4)
    
    
  
}

data.table_4 <- function(Posts, Users){
  # Input the solution here
  # 
  Posts_DT<-data.table(Posts)
  Users_DT<-data.table(Users)
  #I start solving this by finding the Answers Table
  #Firstly I will filter it by PostTypeId
  Answers_filtered_DT4<-Posts_DT[PostTypeId %in% c("2")] 
  #Now I will groupping by owneruserId which will also create the table we want
  Answers_DT4 <- Answers_filtered_DT4[, .(AnswersNumber = .N), by = OwnerUserId]

  #Now Questions table will be created
  #Again I will make filtering by PostTypeId and group it by ownerUserId
  Questions_filtered_DT4<-Posts_DT[PostTypeId %in% c("1")] 
  Questions_DT4 <- Questions_filtered_DT4[, .(QuestionsNumber = .N), by = OwnerUserId]
 
  #As we created the both table we can realize the join operation
  Questions_join_DT4 <- Questions_DT4[Answers_DT4, on = .(OwnerUserId= OwnerUserId ), nomatch = 0]
  #Now we will filter it by AnswersNumber > QuestionsNumber
  filtered_DT4<-Questions_join_DT4[AnswersNumber > QuestionsNumber]
  #Now I order it descreasing by AnswerNumber and Limit it to 5 so that 
  #PostsCounts Will be created.
  setorder(filtered_DT4,-AnswersNumber)
  filtered_DT4<-filtered_DT4[!is.na(OwnerUserId)]
  PostsCount_DT4<-filtered_DT4[1:5]
  
  #Now PostsCounts created we can make the join with USER
  PostsCount_join_DT4 <- Users_DT[PostsCount_DT4, on = .(Id= OwnerUserId ), nomatch = 0]
  #Finally we will select the necessary columns and we get the result
  Result_DT4<-PostsCount_join_DT4[, .(DisplayName, QuestionsNumber, 
                                         AnswersNumber, Location, Reputation,
                                         UpVotes, DownVotes)]
 return(Result_DT4)
  }

# -----------------------------------------------------------------------------#
# Task 5
# -----------------------------------------------------------------------------#

sqldf_5 <- function(Posts){
  # Input the solution here
  # 
  result_sqldf5<-sqldf("SELECT
Questions.Id,
Questions.Title,
BestAnswers.MaxScore,
Posts.Score AS AcceptedScore,
BestAnswers.MaxScore-Posts.Score AS Difference
FROM (
SELECT Id, ParentId, MAX(Score) AS MaxScore
FROM Posts
WHERE PostTypeId==2
GROUP BY ParentId
) AS BestAnswers
JOIN (
SELECT * FROM Posts
WHERE PostTypeId==1
) AS Questions
ON Questions.Id=BestAnswers.ParentId
JOIN Posts ON Questions.AcceptedAnswerId=Posts.Id
WHERE Difference>50
ORDER BY Difference DESC")
 
  return(result_sqldf5)
}

base_5 <- function(Posts){
  # Input the solution here

  #I will start finding BestAnswers dataframe
  
  best_answers_base_5 <- aggregate(Score ~ ParentId, data = subset(Posts, PostTypeId == 2), function(x) {
    id <- which.max(x)
    c(Id = Posts$Id[id], MaxScore = max(x))

  })
  
  # Questions part
  questions_base_5 <- subset(Posts, PostTypeId == 1)
  
  # Joining Questions and BestAnswers
  question_best_ans_join_base_5 <- merge(questions_base_5, best_answers_base_5, by.x = "Id", by.y = "ParentId")
  
  # Joining again with Posts
  join_data_base_5 <- merge(Posts, question_best_ans_join_base_5, by.x = "Id", by.y = "AcceptedAnswerId")
  
  # Calculating Difference
  join_data_base_5$Difference <- join_data_base_5$MaxScore - join_data_base_5$Score
  
  # Filtering based on Difference > 50
  filtered_data_base5 <- subset(join_data_base5, Difference > 50)
  
  # Ordering by Difference in descending order
  result_base_5 <- filtered_data_base5[order(filtered_data_base5$Difference, decreasing = TRUE), c("Id.y", "Title.y", "MaxScore", "Score.x", "Difference")]
  
  # Renaming columns
  colnames(result) <- c("Id", "Title", "MaxScore", "AcceptedScore", "Difference")
  
 
 


  
  
 

  }

dplyr_5 <- function(Posts){
  # Input the solution here
  # 
  
  #Lets start by finding the BestAnswers part
    BestAnswers_dplyr5 <- Posts %>%
      #filtering by posttypeid
      filter(PostTypeId == 2) %>%
      #Groupping by parent Id
      group_by(ParentId)%>%
      #Finding the Id which has max score for each parentId and the Max score's itself
      summarise(Id= Id[which.max(Score)],MaxScore = max(Score))%>%
      #relocating the columns as in the sqldf result
      relocate(c("Id","ParentId","MaxScore"))
    
  #Lets find Questions part which is relatively easy
    Questions_dplyr5 <- Posts %>%
      #what I only needed to do is filtering and we will select the all columns
      filter(PostTypeId == 1)
    
  #Lets join the Question and BestAnswers
    Question_BestAns_join_dplyr5<-inner_join(Questions_dplyr5,BestAnswers_dplyr5,by=c("Id"="ParentId"))
  #Id.y for Answers Id
  #Make the other join
    join_dplyr5 <- inner_join(Posts,Question_BestAns_join_dplyr5,by=c("Id"="AcceptedAnswerId"))
    
    result_dplyr5 <-join_dplyr5%>%
    #mutate will add another column mutating from 2 existing columns
      mutate(Difference = MaxScore - Score.x) %>%
    #Since the Difference column mutated I can use Difference column to filter and orderby and finally I will choose the necessary columns
      filter(Difference>50)%>%
      arrange(desc(Difference))%>%
    #Here, since I applied join 2 times some column names were same.that's why they are automatically assigned name for instance Id.x,Id.y
    #Since as last I did JOIN POST with FIRST JOIN'S RESULTING data frame the variables has .x belong to POST Data frame and .y is the other one
    #According to this I chose the variables from the tables I should choose.
      select(Id=Id.y,Title=Title.y,MaxScore,AcceptedScore=Score.x,Difference)
      
    return(result_dplyr5)
 
}

data.table_5 <- function(Posts){
  # Input the solution here
  #
  Posts<-data.table(Posts)
  #I start with creating the BestAnswers
  
  #When I did not use setkey bestanswers was not sorted in ascending order 
  #setkey sorts a data.table and marks it as sorted with an attribute sorted.
  setkey(Posts, ParentId)
  
  # Filtering by posttypeid and creation of the columns
  BestAnswers_DT5 <- Posts[PostTypeId == 2, 
                             .(Id = Id[which.max(Score)], MaxScore = max(Score)),
                           by = ParentId]
  
  # Relocating the columns as in the sqldf result
  BestAnswers_DT5 <- BestAnswers_DT5[, .(Id, ParentId, MaxScore)]
  
  #Now I will create Questions
  # Filtering by posttypeid
  Questions_DT5 <- Posts[PostTypeId %in% c("1")]
  #Now I will join those 2 data frames
  join_ques_ans_DT5<-Questions_DT5[BestAnswers_DT5, on = .(Id= ParentId), nomatch = 0]
  #Now I will join Posts with this data frame
  last_join_DT5 <- Posts[join_ques_ans_DT5, on = .(Id = AcceptedAnswerId),nomatch = 0]
  #Now I will create the Difference column 
  filtered_DT5<-last_join_DT5[, Difference := MaxScore - Score][Difference > 50]
  setorder(filtered_DT5,-Difference)
  result_DT5 <- filtered_DT5[,.(Id=i.Id,Title=i.Title,MaxScore,AcceptedScore = Score,Difference)]
  
  return(result_DT5)
  
  }

plotbench<-function(test){
  
  test_1df <- as.data.frame(test_1)
  ggplot(test_1df, aes(x = expr, y = time)) +
    geom_boxplot(fill = "lightblue", color = "blue") +
    labs(title = "Microbenchmark Results",
         x = "Function",
         y = "Time (Nanoseconds)") +
    scale_x_discrete(labels = c(
      'sqldf_1(Posts, Users)' = 'sqldf',
      'base_1(Posts, Users)' = 'base',
      'dplyr_1(Posts, Users)' = 'dplyr',
      'data.table_1(Posts, Users)' = 'data.table'
    )) +
    theme_minimal()
  
}