Big Data Assignment 2- Week 5


Dina Pinchuck 
Odel Fhima    

data.file<-'okcupid_profiles.csv'

source('week5_datingnlp.r')


confusion matrix(cross validation confusion matrix contains the sum of all of the results):

          Reference
Prediction     f     m
         f 26257 19997
         m 46094 87490
                                          
               Accuracy : 0.6325   
       
training times:

$everything
   user  system elapsed 
  68.45    0.00   68.72 

$final
   user  system elapsed 
   2.43    0.00    2.44 



gender words:  "love"   "famili" "friend" "food"   "travel" "movi"   "life"  