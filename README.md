# covid-mask-prediction
### Objective: 
This project aims to utilize statistical learning models to predict the frequency of individuals wearing masks when in the presence of people who are not part of their household. This prediction will be based on TAMU survey data.
### Background:
COVID-19, caused by the SARS-CoV-2 virus, is an infectious disease that emerged in early 2020. Since then, communities worldwide have experienced multiple waves of the epidemic. In the initial stages, three primary public health recommendations were emphasized: 1) Wearing masks, 2) Handwashing, and 3) Practicing social distancing or avoiding crowded indoor spaces. Given its airborne nature and impact on the respiratory system, mask wearing played a significant role in preventing the transmission of the disease. However, the effectiveness of mask wearing and its adherence became a subject of extensive discussion and scrutiny, partly due to inconsistent messaging from authorities in the United States. As a result, the propensity of individuals to wear masks varied across different regions.
### Data:
The dataset contains results from a survey about individuals’ attitudes toward mask wearing. The study is conducted among Texas A&M students at College Station during the early stages of the pandemic. The questions aim to measure the different aspects of mask wearing including, but not limited to, its perceived benefits and risks, what individuals think others think about it (norms), what individuals think others are doing, whether they have a choice. The second row in the data file contains the specific question statement and the options given in the survey.

<img width="300" alt="image" src="https://github.com/satyaprakash799/covid-mask-prediction/assets/121471959/afdddcdf-2b0d-4c3c-b7f9-587bdfe1e723">

There are 155 input variables each corresponding to a specific question, and one output variable. The output variable comes from the question: “How often do you wear a mask around people who do not live in your home?” The scale of the answers range from Never (1) to Always (5). There are 479 individuals in the training data. The figure shows the number of responders in each category. 
