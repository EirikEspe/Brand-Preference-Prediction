# Brand Preference Prediction

Classification of which computer brand Blackwell Electronics' customers prefer 

## Task overview

The sales team in Blackwell Electronics has engaged a market research firm to conduct a survey of their existing customers. 
One of the objectives of the survey was to find out which of two brands of computers the customers prefer. This information 
will help Blackwell Electronics to decide with which manufacturer they should pursue a deeper strategic relationship. 
Unfortunately, the answer to the brand preference question was not properly captured for all of the respondents.

That is where you come in: The Chief Technology Officer (CTO) wants you to investigate if customer responses to some survey 
questions (e.g. income, age, etc.) enable us to predict the answer to the brand preference question. If we can do this with 
confidence, she would like you to make those predictions and provide the sales team with a complete view of what brand the
customers prefer.

The CTO has already set up the data for you in the attached CSV files: the file labelled ***CompleteResponses.csv*** is the dataset 
you will use to train your model and build your predictive model. It includes 10,000 fully-answered surveys and the key to 
the survey can be found in ***survey_key.xlsx***. The file labelled ***SurveyIncomplete.csv*** will be your main test set 
(the data you will apply your optimized model to predict the brand preference). You'll be applying your trained and 
tested model to this data to prepare the model for production. You can find the data files in the ***data*** folder.
