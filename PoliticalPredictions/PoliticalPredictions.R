library(matrixStats)
library(shiny)
library(tidyverse)

#Probabilities given sex

probRepGivenMale <- 0.51
probDemGivenMale <- 0.41
probIndGivenMale <- 0.08

probRepGivenFemale <- 0.38
probDemGivenFemale <- 0.54
probIndGivenFemale <- 0.08

#Probabilities given race/ethnicity

probRepGivenWhite <- 0.54
probDemGivenWhite <- 0.39
probIndGivenWhite <- 0.07

probRepGivenBlack <- 0.07
probDemGivenBlack <- 0.87
probIndGivenBlack <- 0.06

probRepGivenHispanic <- 0.27
probDemGivenHispanic <- 0.63
probIndGivenHispanic <- 0.10

probRepGivenAsian <- 0.27
probDemGivenAsian <- 0.66
probIndGivenAsian <- 0.07

#Probabilities given generation

probRepGivenMillenial <- 0.36
probDemGivenMillenial <- 0.57
probIndGivenMillenial <- 0.07

probRepGivenGenX <- 0.42
probDemGivenGenX <- 0.48
probIndGivenGenX <- 0.09

probRepGivenBoomer <- 0.49
probDemGivenBoomer <- 0.45
probIndGivenBoomer <- 0.06

probRepGivenSilent <- 0.53
probDemGivenSilent <- 0.40
probIndGivenSilent <- 0.07

#Probabilities given community type

probRepGivenUrban <- 0.33
probDemGivenUrban <- 0.60
probIndGivenUrban <- 0.07

probRepGivenSuburban <- 0.48
probDemGivenSuburban <- 0.44
probIndGivenSuburban <- 0.07

probRepGivenRural <- 0.55
probDemGivenRural <- 0.37
probIndGivenRural <- 0.08

#Probabilities given income

probRepGiven75plus <- 0.49
probDemGiven75plus <- 0.45
probIndGiven75plus <- 0.06

probRepGiven30to75 <- 0.48
probDemGiven30to75 <- 0.45
probIndGiven30to75 <- 0.07

probRepGivenlessthan30 <- 0.32
probDemGivenlessthan30 <- 0.60
probIndGivenlessthan30 <- 0.08

#Probabilities given education

probRepGivenPostgrad <- 0.35
probDemGivenPostgrad <- 0.60
probIndGivenPostgrad <- 0.05

probRepGivenCollegeGrad <- 0.44
probDemGivenCollegeGrad <- 0.50
probIndGivenCollegeGrad <- 0.06

probRepGivenSomeCollege <- 0.46
probDemGivenSomeCollege <- 0.45
probIndGivenSomeCollege <- 0.09

probRepGivenHighSchool <- 0.45
probDemGivenHighSchool <- 0.46
probIndGivenHighSchool <- 0.08

#Probabilities given marital status

probRepGivenMarried <- 0.51
probDemGivenMarried <- 0.44
probIndGivenMarried <- 0.06

probRepGivenUnmarried <- 0.36
probDemGivenUnmarried <- 0.56
probIndGivenUnmarried <- 0.08

#Now, let's see the class priors.

#Class priors

probRep <- 0.44
probDem <- 0.48
probInd <- 0.08

priors <- data.frame(var = "prior",
                     Rep = probRep,
                     Dem = probDem,
                     Ind = probInd)

#And the predictor priors...

#Sex priors

probMale <- 0.492
probFemale <- 0.508

#Race/ethnicity priors

probWhite <- 0.62
probBlack <- 0.123
probHispanic <- 0.173
probAsian <- 0.052

#Generation Priors

probMillenial <- 0.197
probGenX <- 0.23
probBoomer <- 0.408
probSilent <- 0.145

#Community Type priors
probUrban <- 0.336
probSuburban <- 0.466
probRural <- 0.171
probHispanic <- 0.125

#Income priors

prob75plus <- 0.452
prob30to75 <- 0.350
problessthan30 <- 0.198

#Education priors

probPostgrad <- 0.159
probCollegeGrad <- 0.329
probSomeCollege <- 0.278
probHighSchool <- 0.229

#Marital Status priors

probMarried <- 0.574
probUnmarried <- 0.426

# Now that we have all of these, we can used the naive bayes theorem to calculate 
# each of the likelihoods which will be critical in making our predictions.

##Likelihoods given Rep

#by sex
probMaleGivenRep <- probRepGivenMale*probMale/probRep
probFemaleGivenRep <- probRepGivenFemale*probFemale/probRep

#by race
probWhiteGivenRep <- probRepGivenWhite*probWhite/probRep
probBlackGivenRep <- probRepGivenBlack*probBlack/probRep
probHispanicGivenRep <- probRepGivenHispanic*probHispanic/probRep
probAsianGivenRep <- probRepGivenAsian*probAsian/probRep

#by generation
probMillenialGivenRep <- probRepGivenMillenial*probMillenial/probRep
probGenXGivenRep <- probRepGivenGenX*probGenX/probRep
probBoomerGivenRep <- probRepGivenBoomer*probBoomer/probRep
probSilentGivenRep <- probRepGivenSilent*probSilent/probRep

#by community type
probUrbanGivenRep <- probRepGivenUrban*probUrban/probRep
probSuburbanGivenRep <- probRepGivenSuburban*probSuburban/probRep
probRuralGivenRep <- probRepGivenRural*probRural/probRep

#by income
prob75plusGivenRep <- probRepGiven75plus*prob75plus/probRep
prob30to75GivenRep <- probRepGiven30to75*prob30to75/probRep
problessthan30GivenRep <- probRepGivenlessthan30*problessthan30/probRep

#by education
probPostgradGivenRep <- probRepGivenPostgrad*probPostgrad/probRep
probCollegeGradGivenRep <- probRepGivenCollegeGrad*probCollegeGrad/probRep
probSomeCollegeGivenRep <- probRepGivenSomeCollege*probSomeCollege/probRep
probHighSchoolGivenRep <- probRepGivenHighSchool*probHighSchool/probRep

#by marital status
probMarriedGivenRep <- probRepGivenMarried*probMarried/probRep
probUnmarriedGivenRep <- probRepGivenUnmarried*probUnmarried/probRep

##Likelihoods given Dem

#by sex
probMaleGivenDem <- probDemGivenMale*probMale/probDem
probFemaleGivenDem <- probDemGivenFemale*probFemale/probDem

#by race
probWhiteGivenDem <- probDemGivenWhite*probWhite/probDem
probBlackGivenDem <- probDemGivenBlack*probBlack/probDem
probHispanicGivenDem <- probDemGivenHispanic*probHispanic/probDem
probAsianGivenDem <- probDemGivenAsian*probAsian/probDem

#by generation
probMillenialGivenDem <- probDemGivenMillenial*probMillenial/probDem
probGenXGivenDem <- probDemGivenGenX*probGenX/probDem
probBoomerGivenDem <- probDemGivenBoomer*probBoomer/probDem
probSilentGivenDem <- probDemGivenSilent*probSilent/probDem

#by community type
probUrbanGivenDem <- probDemGivenUrban*probUrban/probDem
probSuburbanGivenDem <- probDemGivenSuburban*probSuburban/probDem
probRuralGivenDem <- probDemGivenRural*probRural/probDem

#by income
prob75plusGivenDem <- probDemGiven75plus*prob75plus/probDem
prob30to75GivenDem <- probDemGiven30to75*prob30to75/probDem
problessthan30GivenDem <- probDemGivenlessthan30*problessthan30/probDem

#by education
probPostgradGivenDem <- probDemGivenPostgrad*probPostgrad/probDem
probCollegeGradGivenDem <- probDemGivenCollegeGrad*probCollegeGrad/probDem
probSomeCollegeGivenDem <- probDemGivenSomeCollege*probSomeCollege/probDem
probHighSchoolGivenDem <- probDemGivenHighSchool*probHighSchool/probDem

#by marital status
probMarriedGivenDem <- probDemGivenMarried*probMarried/probDem
probUnmarriedGivenDem <- probDemGivenUnmarried*probUnmarried/probDem

##Likelihoods given Ind

#by sex
probMaleGivenInd <- probIndGivenMale*probMale/probInd
probFemaleGivenInd <- probIndGivenFemale*probFemale/probInd

#by race
probWhiteGivenInd <- probIndGivenWhite*probWhite/probInd
probBlackGivenInd <- probIndGivenBlack*probBlack/probInd
probHispanicGivenInd <- probIndGivenHispanic*probHispanic/probInd
probAsianGivenInd <- probIndGivenAsian*probAsian/probInd

#by generation
probMillenialGivenInd <- probIndGivenMillenial*probMillenial/probInd
probGenXGivenInd <- probIndGivenGenX*probGenX/probInd
probBoomerGivenInd <- probIndGivenBoomer*probBoomer/probInd
probSilentGivenInd <- probIndGivenSilent*probSilent/probInd

#by community type
probUrbanGivenInd <- probIndGivenUrban*probUrban/probInd
probSuburbanGivenInd <- probIndGivenSuburban*probSuburban/probInd
probRuralGivenInd <- probIndGivenRural*probRural/probInd

#by income
prob75plusGivenInd <- probIndGiven75plus*prob75plus/probInd
prob30to75GivenInd <- probIndGiven30to75*prob30to75/probInd
problessthan30GivenInd <- probIndGivenlessthan30*problessthan30/probInd

#by education
probPostgradGivenInd <- probIndGivenPostgrad*probPostgrad/probInd
probCollegeGradGivenInd <- probIndGivenCollegeGrad*probCollegeGrad/probInd
probSomeCollegeGivenInd <- probIndGivenSomeCollege*probSomeCollege/probInd
probHighSchoolGivenInd <- probIndGivenHighSchool*probHighSchool/probInd

#by marital status
probMarriedGivenInd <- probIndGivenMarried*probMarried/probInd
probUnmarriedGivenInd <- probIndGivenUnmarried*probUnmarried/probInd

#Create a table of all results:

# Rep

sexR <- data.frame(var = c("Male", "Female", "Other"),
                   Rep = c(probMaleGivenRep, probFemaleGivenRep, 1))
raceR <- data.frame(var = c("White", "Black", "Hispanic", "Asian", "Other"),
                    Rep = c(probWhiteGivenRep,
                            probBlackGivenRep,
                            probHispanicGivenRep,
                            probAsianGivenRep,
                            1))
generationR <- data.frame(var = c("Millenial", "GenX", "Boomer", "Silent"),
                          Rep = c(probMillenialGivenRep,
                                  probGenXGivenRep,
                                  probBoomerGivenRep,
                                  probSilentGivenRep))
communityR <- data.frame(var = c("Urban", "Suburban", "Rural"),
                         Rep = c(probUrbanGivenRep,
                                 probSuburbanGivenRep,
                                 probRuralGivenRep))
incomeR <- data.frame(var = c("75plus", "30to75", "lessthan30"),
                      Rep = c(prob75plusGivenRep,
                              prob30to75GivenRep,
                              problessthan30GivenRep))
educationR <- data.frame(var = c("Postgrad", "CollegeGrad", "SomeCollege", "HighSchool"),
                         Rep = c(probPostgradGivenRep,
                                 probCollegeGradGivenRep,
                                 probSomeCollegeGivenRep,
                                 probHighSchoolGivenRep))
maritalstatusR <- data.frame(var = c("Married", "Unmarried"),
                             Rep = c(probMarriedGivenRep,
                                     probUnmarriedGivenRep))

# Dem

sexD <- data.frame(var = c("Male", "Female", "Other"),
                   Dem = c(probMaleGivenDem, probFemaleGivenDem, 1))
raceD <- data.frame(var = c("White", "Black", "Hispanic", "Asian", "Other"),
                    Dem = c(probWhiteGivenDem,
                            probBlackGivenDem,
                            probHispanicGivenDem,
                            probAsianGivenDem,
                            1))
generationD <- data.frame(var = c("Millenial", "GenX", "Boomer", "Silent"),
                          Dem = c(probMillenialGivenDem,
                                  probGenXGivenDem,
                                  probBoomerGivenDem,
                                  probSilentGivenDem))
communityD <- data.frame(var = c("Urban", "Suburban", "Rural"),
                         Dem = c(probUrbanGivenDem,
                                 probSuburbanGivenDem,
                                 probRuralGivenDem))
incomeD <- data.frame(var = c("75plus", "30to75", "lessthan30"),
                      Dem = c(prob75plusGivenDem,
                              prob30to75GivenDem,
                              problessthan30GivenDem))
educationD <- data.frame(var = c("Postgrad", "CollegeGrad", "SomeCollege", "HighSchool"),
                         Dem = c(probPostgradGivenDem,
                                 probCollegeGradGivenDem,
                                 probSomeCollegeGivenDem,
                                 probHighSchoolGivenDem))
maritalstatusD <- data.frame(var = c("Married", "Unmarried"),
                             Dem = c(probMarriedGivenDem,
                                     probUnmarriedGivenDem))

# Ind

sexI <- data.frame(var = c("Male", "Female", "Other"),
                   Ind = c(probMaleGivenInd, probFemaleGivenInd, 1))
raceI <- data.frame(var = c("White", "Black", "Hispanic", "Asian", "Other"),
                    Ind = c(probWhiteGivenInd,
                            probBlackGivenInd,
                            probHispanicGivenInd,
                            probAsianGivenInd,
                            1))
generationI <- data.frame(var = c("Millenial", "GenX", "Boomer", "Silent"),
                          Ind = c(probMillenialGivenInd,
                                  probGenXGivenInd,
                                  probBoomerGivenInd,
                                  probSilentGivenInd))
communityI <- data.frame(var = c("Urban", "Suburban", "Rural"),
                         Ind = c(probUrbanGivenInd,
                                 probSuburbanGivenInd,
                                 probRuralGivenInd))
incomeI <- data.frame(var = c("75plus", "30to75", "lessthan30"),
                      Ind = c(prob75plusGivenInd,
                              prob30to75GivenInd,
                              problessthan30GivenInd))
educationI <- data.frame(var = c("Postgrad", "CollegeGrad", "SomeCollege", "HighSchool"),
                         Ind = c(probPostgradGivenInd,
                                 probCollegeGradGivenInd,
                                 probSomeCollegeGivenInd,
                                 probHighSchoolGivenInd))
maritalstatusI <- data.frame(var = c("Married", "Unmarried"),
                             Ind = c(probMarriedGivenInd,
                                     probUnmarriedGivenInd))

sex <- cbind(sexR, Dem = sexD[,2], Ind = sexI[,2])
race <- cbind(raceR, Dem = raceD[,2], Ind = raceI[,2])
generation <- cbind(generationR, Dem = generationD[,2], Ind = generationI[,2])
community <- cbind(communityR, Dem = communityD[,2], Ind = communityI[,2])
income <- cbind(incomeR, Dem = incomeD[,2], Ind = incomeI[,2])
education <- cbind(educationR, Dem = educationD[,2], Ind = educationI[,2])
maritalstatus <- cbind(maritalstatusR, Dem = maritalstatusD[,2], Ind = maritalstatusI[,2])

vecS <- NULL
vecR <- NULL
vecG <- NULL
vecC <- NULL
vecI <- NULL
vecE <- NULL
vecM <- NULL
scores <- NULL


ui <- fluidPage(
  
    titlePanel("Tell us about yourself and we'll guess your political affiliation!"),
    mainPanel(
        tabsetPanel(
            tabPanel("Classifier Tool", 
                     selectInput(inputId = "sex",
                                 label = "Sex",
                                 choices = sex$var),
                     numericInput(inputId = "age",
                                  label = "Age",
                                  min = 18,
                                  max = 88,
                                  step = 1,
                                  value = 40),
                     selectInput(inputId = "race",
                                 label = "Race",
                                 choices = race$var),
                     sliderInput(inputId = "income",
                                 label = "Family Income",
                                 min = 0,
                                 max = 100000,
                                 value = 30000,
                                 animate = TRUE,
                                 sep = ""),
                     selectInput(inputId = "education",
                                 label = "Education Level",
                                 choices = education$var),
                    radioButtons(inputId = "married",
                                label = "Are you married?",
                                choices = maritalstatus$var),
                    radioButtons(inputId = "community",
                                label = "How would you describe your community?",
                                choices = community$var),
                    
                    actionButton(inputId = "submitButton", label = "Submit"),
                    
                    textOutput(outputId = "prediction")
    
))))

server <- function(input, output, session) {
  
  
  Scores <- eventReactive(input$submitButton, {
  
    vecS <- sex[(sex$var == input$sex),]
    
    vecR <- race[(race$var == input$race),]
    
    vecE <- education[(education$var == input$education),]
    
    vecM <- maritalstatus[(maritalstatus$var == input$married),]
    
    vecC <- community[(community$var == input$community),]
    
    
    if(input$age <= 39){
      vecG <- generation[(generation$var == 'Millenial'),]
    }else if(input$age <= 55){
      vecG <- generation[(generation$var == 'GenX'),]
    }else if(input$age <= 74){
      vecG <- generation[(generation$var == 'Boomer'),]
    }else{
      vecG <- generation[(generation$var == 'Silent'),]
    }

    if(input$income >= 75000){
      vecI <- income[(income$var == '75plus'),]
    }else if(input$income >= 30000){
      vecI <- income[(income$var == '30to75'),]
    }else{
      vecI <- income[(income$var == 'lessthan30'),]
    }
    
    scores <- rbind(priors,
                    vecS,
                    vecR,
                    vecE,
                    vecM,
                    vecC,
                    vecG,
                    vecI)
    repScore <- colProds(as.matrix(scores[,c(2)]))
    demScore <- colProds(as.matrix(scores[,c(3)]))
    indScore <- colProds(as.matrix(scores[,c(4)]))
    totalScore<- repScore + demScore + indScore
    repScore <- repScore/totalScore
    demScore <- demScore/totalScore
    indScore <- indScore/totalScore
    maxScore <- max(repScore,demScore,indScore)
    
    if (maxScore == demScore){
      response <- sprintf("We think you lean Democrat.
      \n Republican Leaning = %g, Democrat Leaning = %g, Independent Leaning = %g", repScore, demScore, indScore)
    }else if (maxScore == repScore){
      response <- sprintf("We think you lean Republican.
      \n Republican Leaning = %g, Democrat Leaning = %g, Independent Leaning = %g", repScore, demScore, indScore)
    }else{
      response <- sprintf("We think you lean Independent.
      \n Republican Leaning = %g, Democrat Leaning = %g, Independent Leaning = %g", repScore, demScore, indScore)
    }

  
  })
  
    output$prediction <- renderText({
        
        print(Scores())
      
        
    })
    
}

shinyApp(ui, server)