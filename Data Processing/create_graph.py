import pandas as pd
import matplotlib.pyplot as plt
import seaborn as sns


def plot_reputation_comments_count(user_comments_count_sorted_1, user_comments_count_sorted_2, user_comments_count_sorted_3):
    # Create subplots with 3 rows and 1 column
    fig, axes = plt.subplots(3, 1, figsize=(10, 18))

    # Plot for the first dataframe
    sns.barplot(x='Reputation', y='CommentsCount', data=user_comments_count_sorted_1, ax=axes[0], orient='h')
    axes[0].set_title("English")
    axes[0].set_xlabel("User Reputation")
    axes[0].set_ylabel("Number of Comments")
    axes[0].invert_yaxis()  # Invert the y-axis

    # Plot for the second dataframe
    sns.barplot(x='Reputation', y='CommentsCount', data=user_comments_count_sorted_2, ax=axes[1], orient='h')
    axes[1].set_title("German")
    axes[1].set_xlabel("User Reputation")
    axes[1].set_ylabel("Number of Comments")
    axes[1].invert_yaxis()  # Invert the y-axis

    # Plot for the third dataframe
    sns.barplot(x='Reputation', y='CommentsCount', data=user_comments_count_sorted_3, ax=axes[2], orient='h')
    axes[2].set_title("Spanish")
    axes[2].set_xlabel("User Reputation")
    axes[2].set_ylabel("Number of Comments")
    axes[2].invert_yaxis()  # Invert the y-axis

    # Adjust layout for better spacing
    plt.tight_layout()

    # Show the plots
    plt.show()

def plot_answers_comments_count(user_comments_count_sorted_1, user_comments_count_sorted_2, user_comments_count_sorted_3):
    # Create subplots with 3 rows and 1 column
    fig, axes = plt.subplots(3, 1, figsize=(10, 18))

    # Plot for the first dataframe
    sns.barplot(x='Reputation', y='AnswersCount', data=user_comments_count_sorted_1, ax=axes[0], orient='h')
    axes[0].set_title("English")
    axes[0].set_xlabel("User Reputation")
    axes[0].set_ylabel("Number of Answers")
    axes[0].invert_yaxis()  # Invert the y-axis

    # Plot for the second dataframe
    sns.barplot(x='Reputation', y='AnswersCount', data=user_comments_count_sorted_2, ax=axes[1], orient='h')
    axes[1].set_title("German")
    axes[1].set_xlabel("User Reputation")
    axes[1].set_ylabel("Number of Answers")
    axes[1].invert_yaxis()  # Invert the y-axis

    # Plot for the third dataframe
    sns.barplot(x='Reputation', y='AnswersCount', data=user_comments_count_sorted_3, ax=axes[2], orient='h')
    axes[2].set_title("Spanish")
    axes[2].set_xlabel("User Reputation")
    axes[2].set_ylabel("Number of Answers")
    axes[2].invert_yaxis()  # Invert the y-axis

    # Adjust layout for better spacing
    plt.tight_layout()

    # Show the plots
    plt.show()

def plot_location_count(df1, df2,df3):
    fig, axes = plt.subplots(3, 1,figsize=(10, 18))

    # Plot for the first dataframe
    sns.barplot(x='Count', y='Location', data=df1, orient='h', ax=axes[0])
    axes[0].set_title("English")
    axes[0].set_xlabel("Count")
    axes[0].set_ylabel("Location")

    # Plot for the second dataframe
    sns.barplot(x='Count', y='Location', data=df2, orient='h', ax=axes[1])
    axes[1].set_title("German")
    axes[1].set_xlabel("Count")
    axes[1].set_ylabel("Location")

    sns.barplot(x='Count', y='Location', data=df3, orient='h', ax=axes[2])
    axes[2].set_title("Spanish")
    axes[2].set_xlabel("Count")
    axes[2].set_ylabel("Location")

    # Adjust layout for better spacing
    plt.tight_layout()

    # Show the plots
    plt.show()    