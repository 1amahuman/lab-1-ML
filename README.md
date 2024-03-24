# Student Performance Prediction Model

This repository contains code for building a predictive model to estimate student performance based on various factors such as study hours, quiz scores, forum participation, and previous grades. The model is implemented in R using supervised learning techniques.

## Dataset Generation

The dataset is simulated to mimic student performance data. It includes features such as study hours, quiz scores, forum participation, previous grades, and final grades. The simulated data is generated with a fixed random seed for reproducibility.

## Model Development

1. **Data Splitting**: The dataset is split into training and testing sets with a ratio of 80:20, respectively. This allows for training the model on a subset of the data and evaluating its performance on unseen data.

2. **Model Specification**: Logistic regression is chosen as the predictive model due to its suitability for binary classification tasks. The model is specified using the `{parsnip}` package, and the engine is set to a generalized linear model (glm) for logistic regression.

3. **Feature Engineering**: A recipe is created to preprocess the data before model training. The recipe specifies the outcome variable (`FinalGrades`) and predictor variables (`StudyHours`, `QuizScores`, `ForumPosts`, `PreviousGrades`).

4. **Model Training**: The model is trained on the training dataset using the specified workflow, which includes both the model and recipe.

5. **Model Evaluation**: The trained model is evaluated on the testing dataset to assess its predictive performance. Evaluation metrics such as accuracy and prediction interval are computed to measure the model's effectiveness in predicting student grades.

## Usage

To run the code:

1. Clone this repository to your local machine.
2. Open the R script or R Markdown file in your preferred R development environment.
3. Install the required packages listed in the script.
4. Run the code sequentially to generate simulated data, train the model, and evaluate its performance.

## Dependencies

The following R packages are required to execute the code:

- `{tidymodels}`: For modeling and preprocessing data.
- `{rsample}`: For data splitting and resampling.
- `{parsnip}`: For specifying machine learning models.
- `{dplyr}`: For data manipulation.
- `{ggplot2}`: For data visualization.

Install these packages using the `install.packages()` function if not already installed.
