# Amazon Kindle Reviews Text Analysis

## Project Overview
This project performs descriptive and text analytics on Amazon Kindle reviews to understand customer sentiment, review behavior, and key themes in user feedback. The analysis combines tidytext and tm (text mining) approaches to extract insights from unstructured text data.

## Objectives
- Analyze rating distribution and review patterns  
- Perform tokenization and identify frequently used words  
- Evaluate review length and structure  
- Conduct sentiment analysis on customer reviews  
- Explore word associations and correlations  

## Dataset
- Source: Amazon Kindle Reviews (Kaggle)  
- Data includes review text, ratings, and product identifiers  
- Dataset contains more than 30 reviews  

## Tools and Technologies
- Programming Language: R  
- Libraries: tidytext, dplyr, ggplot2, tm, syuzhet, stringr  
- Techniques:
  - Tokenization and stopword removal  
  - Frequency analysis  
  - Text preprocessing (cleaning, stemming)  
  - Sentiment analysis  
  - Word association and correlation analysis  

## Methodology
1. Data loading and preprocessing  
2. Descriptive analysis of rating distribution  
3. Tokenization using tidytext and removal of stopwords  
4. Frequency analysis of words and tokens  
5. Computation of review-level statistics (word count, sentence count)  
6. Visualization of distributions and patterns  
7. Text cleaning using tm and creation of Term Document Matrix (TDM)  
8. Sentiment analysis using the Syuzhet method  
9. Word association and correlation analysis  

## Key Findings
- Customer ratings are skewed toward higher ratings, indicating generally positive feedback  
- Frequently used words highlight common themes such as reading experience and content quality  
- Review length varies by rating, with longer reviews often associated with stronger opinions  
- Sentiment analysis shows an overall positive sentiment trend across reviews  
- Word association analysis reveals relationships between terms such as "book," "read," and "story"  

## Outputs
- Rating distribution summary  
- Top token frequency tables  
- Review statistics (mean, median, standard deviation)  
- Visualizations:
  - Rating distribution  
  - Top words  
  - Word cloud  
  - Token distribution  
  - Review length by rating  
  - Sentiment distribution and emotional arc  
- Word correlation heatmap  

## Business Impact
- Helps understand customer feedback at scale  
- Identifies key drivers of positive and negative reviews  
- Supports product improvement and marketing strategy  
- Enables data-driven decision-making based on text insights  

## Future Improvements
- Incorporate advanced NLP models (e.g., topic modeling, BERT)  
- Analyze temporal trends in reviews  
- Perform product-level comparison  
- Build a sentiment classification model  

## Author
Sabun Dhital  
MS in Business Analytics  
University of South Dakota  
Email: sabundhital@gmail.com  
