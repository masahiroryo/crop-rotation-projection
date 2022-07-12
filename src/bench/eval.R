
eval <- function(model, test_data) {
  pred <- predict(model, data = test_data)

  confusion_matrix <- confusionMatrix(pred$predictions ,test_data$CType)
  x <- confusion_matrix$table %>% confusionMatrix()
  
  plot(model$predictions, las = 2, main="Number of predictions per Class")
  
  var_importance <- model$variable.importance
  
  barplot(var_importance[order(var_importance, decreasing = TRUE)], las = 2, main="Variable importance")
  
  class_accuracy <- data.frame(pred$predictions, test_data$CType) %>%
    group_by(pred.predictions) %>%
    summarize(acc = mean(pred.predictions == test_data.CType)) %>%
    arrange(pred.predictions)
  
  ggplot(data=class_accuracy, aes(x=pred.predictions, y=acc)) +
    geom_bar(stat="identity")+
    ggtitle("Accuracy for each class")
  
}

