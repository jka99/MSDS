library(nnet)

x <- seq(0, 2*3.1415, by = 0.01)
y = sin(x)
sim = data.frame(x, y)
set.seed(20)
data_used = sim
ctrl = trainControl(method = "cv", number = 5)
fit0 = train(y ~ x,
             data = data_used,
             method = "nnet",
             tuneGrid = expand.grid(size = 0, decay = 0),
             linout = TRUE,
             skip = TRUE,
             trace = FALSE,
             preProc = c("center", "scale"),
             trControl = ctrl)

sim <- sim %>%
  mutate(fit0_pred = predict(fit0))
sim %>%
  gf_point(y ~ x) %>%
  gf_line(fit0_pred ~ x, color = "red")
set.seed(20)
fit1 = train(y ~ x,
             data = data_used,
             method = "nnet",
             tuneGrid = expand.grid(size = 1, decay = 0),
             linout = TRUE,
             #skip = TRUE,
             trace = FALSE,
             preProc = c("center", "scale"),
             trControl = ctrl)

set.seed(20)
fit2 = train(y ~ x,
             data = data_used,
             method = "nnet",
             tuneGrid = expand.grid(size = 2, decay = 0),
             linout = TRUE,
             #skip = TRUE,
             trace = FALSE,
             preProc = c("center", "scale"),
             trControl = ctrl)

sim %>%
  gf_point(y ~ x) %>%
  gf_line(fit1_pred ~ x, color = "blue", lwd = 2, lty = 2) %>%
  gf_line(fit2_pred ~ x, color = "gold", lwd = 2, lty = 3)

fit2$finalModel$convergence

library(ISLR)
data(Default)
Default <- Default %>%
  mutate(student01 = if_else(student == "Yes", 1, 0)) %>%
  select(-student)
  
set.seed(4)  
ctrl = trainControl(method = "cv", number = 5)
fit_default = train(default ~.,
                    data = Default,
                    method = "nnet",
                    tuneGrid = expand.grid(size = 1, decay = 0),
                    trace = FALSE,
                    preProc = c("center", "scale"),
                    maxit = 200,
                    trControl = ctrl)  

library(NeuralNetTools)
par(mar = c(.1,.1,.1,.1))
plotnet(fit_default)  
  
  