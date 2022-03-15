library("tidyverse")
x <- seq(1,10);
y_ql = x^2;
y_mae = abs(x);

d1 = 5;
d2 = 3;

y_hubb_1 = ifelse(abs(x) <= d1, 0.5 * (x^2), d1*x - 0.5*(rep(d1, length(x))^2) )
y_hubb_2 = ifelse(abs(x) <= d2, 0.5 * (x^2), d2*x - 0.5*(rep(d2, length(x))^2) )

df <- data.frame(x,y_ql, y_mae, y_hubb_1, y_hubb_2);

ggplot(df, aes(x)) + geom_line(aes(y=y_ql), color="red") +
  geom_line(aes(y=y_mae), color="blue") +
  geom_line(aes(y=y_hubb_1), color="green") +
  geom_line(aes(y=y_hubb_2), color="gray") +
  labs(x = "e", y = "Loss Function")

optim_fun_batch = function(X, Y, alpha, d=50, cost_func){
  a = runif(1);
  b = runif(1);
  Y_pred = a * X + b;
  n = length(Y);
  error = sum((Y - Y_pred) ^ 2) / n;
  flag = 0;
  i = 0;
  while (flag == 0){
    if (cost_func == 1){
      # Quadratic Loss
      tmp_a = a - alpha * ((2 / n) * (sum((Y - Y_pred) * X)));
      tmp_b = b - alpha * ((2 / n) * (sum(Y - Y_pred)));
    }
    if (cost_func == 2){
      # MAE Loss
      tmp_a = a - alpha * ((2 / n) * (sum(X)));
      tmp_b = b - alpha * (2);
    }
    if (cost_func == 3){
      # Huber Loss
      e = Y - Y_pred;
      y_hubb_a = ifelse(abs(e) <= d, -1 * e * X, -1 * d * X);
      y_hubb_b = ifelse(abs(e) <= d, -1 * e, -1 * d);
      tmp_a = a - alpha * ((1 / n) * sum(y_hubb_a));
      tmp_b = b - alpha * ((1 / n) * sum(y_hubb_b));
    }
    a = tmp_a;
    b = tmp_b;
    Y_pred = a * X + b;optim_fun_stoch = function(x, y, alpha, d=50, cost_func, n){
      idx <- sample(nrow(as.matrix(x)), n)
      Y <- as.matrix(y)[idx,]
      X <- as.matrix(x)[idx,]
      a = runif(1);
      b = runif(1);
      Y_pred = a * X + b;
      n = length(Y);
      error = sum((Y - Y_pred) ^ 2) / n;
      flag = 0;
      i = 0;
      while (flag == 0){
        if (cost_func == 1){
          # Quadratic Loss
          tmp_a = a - alpha * ((2 / n) * (sum((Y - Y_pred) * X)));
          tmp_b = b - alpha * ((2 / n) * (sum(Y - Y_pred)));
        }
        if (cost_func == 2){
          # MAE Loss
          tmp_a = a - alpha * ((2 / n) * (sum(X)));
          tmp_b = b - alpha * (2);
        }
        if (cost_func == 3){
          # Huber Loss
          e = Y - Y_pred;
          y_hubb_a = ifelse(abs(e) <= d, -1 * e * X, -1 * d * X);
          y_hubb_b = ifelse(abs(e) <= d, -1 * e, -1 * d);
          tmp_a = a - alpha * ((1 / n) * sum(y_hubb_a));
          tmp_b = b - alpha * ((1 / n) * sum(y_hubb_b));
        }
        a = tmp_a;
        b = tmp_b;
        Y_pred = a * X + b;
        error_new = sum((Y - Y_pred) ^ 2) / n;
        if (error - error_new <= 0.1) {
          flag = 1;
          return(c(a, b))
        }
        i = i + 1;
        if (i > 500){
          flag = 1;
          return(c(a, b))
        }
      }
    }
    x = mtcars$disp;
    y = mtcars$mpg;
    print ('Optimal Parameters ');
    print (optim_fun_stoch(x, y, 0.00001, 30, 3, 20));
    error_new = sum((Y - Y_pred) ^ 2) / n;
    if (error - error_new <= 0.1) {
      flag = 1;
      return(c(a, b))
    }
    i = i + 1;
    if (i > 500){
      flag = 1;
      return(c(a, b))
    }
  }
}
X = mtcars$disp;
Y = mtcars$mpg;
print ('Optimal Parameters ');
print (optim_fun_batch(X, Y, 0.00001, 30, 1));

