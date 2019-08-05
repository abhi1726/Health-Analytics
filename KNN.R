pc <- read.csv("Prostate_Cancer.csv", stringsAsFactors = FALSE)
View(pc)


str(pc)

pc <- pc[-1]
View(pc)

table(pc$diagnosis_result)

pc$diagnosis_result <- factor(pc$diagnosis_result, levels = c("B","M"), 
                              labels = c("Benign", "Malignant"))

round(prop.table(table(pc$diagnosis_result))* 100, digits = 1)

summary(pc)

normalize <- function(x)
{
  return((x- min(x)) / (max(x)- min(x)))
}

pc_n <- as.data.frame(lapply(pc[2:9], normalize)) 

summary(pc_n$radius)


pc_trai <- pc_n[1:65,]
pc_test <- pc_n[66:100,]

View(pc_test)
View(pc_trai)

pc_train_labels <- pc[1:65, 1]
pc_test_labels <- pc[66:100, 1]

View(pc_test_labels)
View(pc_train_labels)

library(class)

pc_test_pred <- knn(train = pc_trai, test = pc_test, 
                    cl = pc_train_labels, k = 10 )

View(pc_test_pred)


library(gmodels)

CrossTable(x =pc_test_labels, y =pc_test_pred, prop.chisq = FALSE)
