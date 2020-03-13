# Read data file from working directory
data <- read.csv(file = "AlmostAll-ClassCSV.csv")
data <- data[!duplicated(data$title),]
data$edit.count <- scale(data$edit.count, center = TRUE, scale= TRUE)
data$editor.count <- scale(data$editor.count, center = TRUE, scale= TRUE)
data$article.size <- scale(data$article.size, center = TRUE, scale= TRUE)

# Ordering the dependent variable
data$class = factor(data$class, levels = c("FA", "GA", "B", "C"), ordered = TRUE)

print(summary(data))


# Dividing data into training and test set
# Random sampling 
samplesize = 0.60 * nrow(data)
set.seed(100)
index = sample(seq_len(nrow(data)), size = samplesize)
# Creating training and test set 
datatrain = data[index,]
datatest = data[-index,]

# Coefficients:
# holding everything else constant, an increase in value of betweenness by one unit increases the 
# expected value of quality in log odds by 4.05.
# Intercepts: 
# The intercepts can be interpreted as the expected odds when others variables assume a value of zero. 
# Build ordinal logistic regression model
model_one = polr(class ~ diameter + closeness + clustering + betweenness + edit.count + article.size,
             data = datatrain, Hess = TRUE)
print(summary(model))


# Inverse Logit of Intercepts:
# FA: 0.06465198
# GA: 0.2245753
# B: 0.5135467
# C: 0.19722602

model_two = polr(class ~ diameter + closeness + clustering + betweenness,
             data = datatrain, Hess = TRUE)


# Compute confusion table and misclassification error
predictclass = predict(model, datatest)
print(table(datatest$class, predictclass))
print(mean(as.character(datatest$class) != as.character(predictclass)))
print("")
# Plotting the effects

# Effect(focal.predictors = "editor.count", model)
# plot(Effect(focal.predictors = "clustering", model))
# plot(Effect(focal.predictors = c("editor.count", "betweenness"), model))

# Likelihood ratio test
anova(model_one, model_two, test ="Chisq")
lrtest(model_one, model_two)

M1 <- logLik(model_one)
M2 <- logLik(model_two)
H <- -2*(M1[1] - M2[1])
print("likelihood ratio test varying independent variables in the model:")
print(H)
print("p-value of ratio test")
print(pchisq(H,3,lower.tail = FALSE))
print("")


# Run a multinomial logit model
# Likelihood ratio test to see if the models are statistically different
multi_model <- multinom(class ~ diameter + closeness + clustering + betweenness + edit.count 
                        + article.size, data = datatrain)
M3 <- logLik(multi_model)

G <- -2*(M1[1] - M3[1])
print("likelihood ratio test between multinomial and ordinal models:")
print(G)
print("p-value of ratio test")
print(pchisq(G,3,lower.tail = FALSE))


