# covid-mask-prediction
#### File handling Instructions:
1. Please don't alter the names of the files and the data inside.
2. Please place all the files in a single folder while running the code.
3. Use only the attached excel sheet to import the data. It is personalized for our code.
### Objective: 
This project aims to utilize statistical learning models to predict the frequency of individuals wearing masks when in the presence of people who are not part of their household. This prediction will be based on TAMU survey data.
### Background:
COVID-19, caused by the SARS-CoV-2 virus, is an infectious disease that emerged in early 2020. Since then, communities worldwide have experienced multiple waves of the epidemic. In the initial stages, three primary public health recommendations were emphasized: 1) Wearing masks, 2) Handwashing, and 3) Practicing social distancing or avoiding crowded indoor spaces. Given its airborne nature and impact on the respiratory system, mask wearing played a significant role in preventing the transmission of the disease. However, the effectiveness of mask wearing and its adherence became a subject of extensive discussion and scrutiny, partly due to inconsistent messaging from authorities in the United States. As a result, the propensity of individuals to wear masks varied across different regions.
### Data:
The dataset contains results from a survey about individuals’ attitudes toward mask wearing. The study is conducted among Texas A&M students at College Station during the early stages of the pandemic. The questions aim to measure the different aspects of mask wearing including, but not limited to, its perceived benefits and risks, what individuals think others think about it (norms), what individuals think others are doing, whether they have a choice. The second row in the data file contains the specific question statement and the options given in the survey.

<img width="300" alt="image" src="https://github.com/satyaprakash799/covid-mask-prediction/assets/121471959/afdddcdf-2b0d-4c3c-b7f9-587bdfe1e723">

There are 155 input variables each corresponding to a specific question, and one output variable. The output variable comes from the question: “How often do you wear a mask around people who do not live in your home?” The scale of the answers range from Never (1) to Always (5). There are 479 individuals in the training data. The figure shows the number of responders in each category. There are 100 individuals in the test data holded for testing the trained model.

### Training Models:
Although many statistical learning methods are used to train the model, three models are shortlisted: Forward Subset Selection, Random Forests (Bagging), and LDA. Forward Selection and Random Forests regression models cannot be directly compared to a classification model such as LDA. So, a single parameter, test MSE is calculated for LDA also and compared to the remaining Regression models.

<img width="724" alt="image" src="https://github.com/satyaprakash799/covid-mask-prediction/assets/121471959/5b552e19-d1d1-408e-b4f6-d76e43b1b950">

### Results & Conclusions:
The performance of three different models was assessed to determine the best approach for our dataset. Among these models, the Forward Stepwise Selection showed moderate results, while the Random Forest model demonstrated significantly better performance, making it the preferred choice. Additionally, a classification model called LDA was evaluated, but it did not perform as well as the Random Forest model. In summary, the Random Forest model is the most effective option for our dataset, delivering the highest level of accuracy and reliability for decision-making purposes.
The best-performing model, Random Forest, has a Test Mean Squared Error (MSE) of 0.79, an Adjusted R-squared value for the training model is 0.86. These results indicate that the model is highly accurate and reliable in predicting outcomes, making it the optimal choice for our dataset of all the models.

Upon examining the results of the analyses, it was determined that the survey questions alone do not sufficiently capture the necessary information to predict whether an individual will or will not wear a mask around non-household members.
Although the survey may not capture all the necessary information, the most significant questions address an individual's understanding of the importance of mask-wearing in public, public perception of the COVID threat, public attitudes towards wearing masks, and their confidence and well-being.
