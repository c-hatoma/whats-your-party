# whats-your-party
By Bryce Samwel (MS, Systems Science, Portland State University; BA, Physics, Carroll College) and Chica Morrow (BA, Economics, Middlebury College). Just for fun.


Visit our app here:
https://chica.shinyapps.io/PoliticalPredictions/

To come up with our predictions we used a Naive Bayes Classifier. The priors were taken from the Pew Research Center (see below) and the likelihoods were calculated based on these priors. For those that are unfamiliar with this methodology, the basic premise of the Naive Bayes Classifier is based on Bayes' Theorem which states that p(A|B)=p(B|A)p(A)/p(B). In this project A represents a person's political affiliation and B represents a demographic factor like age or race. We calculate a likelihood (p(A|B)) for each demographic feature and multiply them to come up with a score. The highest score is the most likely political affiliation. We present the prediction as well as the scores scores (normalized to sum to 1) after the user inputs their demographic information and presses the submit button.

Party Identification Data:
2016 Party Identification Detailed Table - Pew Research Center
https://www.pewresearch.org/politics/2016/09/13/2016-party-identification-detailed-tables/
