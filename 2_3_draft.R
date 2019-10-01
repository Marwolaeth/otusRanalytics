if (!require(data.table)) install.packages('data.table')
library(data.table)
if (!file.exists('bank/bank-full.csv')) {
  download.file(
    'https://archive.ics.uci.edu/ml/machine-learning-databases/00222/bank.zip',
    destfile = 'bank.zip'
  )
  unzip('bank.zip', exdir = 'bank')
}

bank_data <- fread(
  'bank/bank-full.csv',
  stringsAsFactors = TRUE # Все строковые переменные в датасете — факторы
)

dim(bank_data)
str(bank_data)
summary(bank_data, maxsum = 13)
bank_data
bank_data[(age %in% 25:40) &
            (job %in% c('entrepreneur', 'self-employed')) &
            (loan == 'no') & (housing == 'no')]

bank_data[job == 'unknown' | education == 'unknown']
bank_data[job == 'student', .N]
bank_data[default == 'yes', mean(age)]
bank_data[age >= 60,
          .SD[which.max(balance)],
          .SDcols = c('age', 'job', 'education', 'balance', 'day', 'month')]
bank_data[order(-balance)]

set.seed(5555)
dur_sample <- bank_data[sample(1:.N, 250), duration]
# Раскоментить перед выполнением
hist(dur_sample,
              xlab = "Duration of contact",
              main = "Histogram of contact duration",
              breaks = 20)
bank_data[duration == 0, .N]
hist(log(dur_sample))
hist(dur_sample)
hist(rlnorm(250, mean(log(dur_sample)), sd(log(dur_sample))))
hist(log(rlnorm(250, mean(dur_sample), sd(dur_sample))), breaks = 20)
qqnorm(log(dur_sample), plot.it = T)

summary(dur_sample)
hist(log(dur_sample), breaks = 20)

bank_data[, uniqueN(job)]
bank_data[, .SD[balance > mean(balance),
                .(.N, mean_balance = mean(balance))],
          by = job]

hist(bank_data[, balance])
hist(log(bank_data[, balance]))

bank_data[, .(mean = mean(balance), median = median(balance)), by = job]
bank_data[, .(mean = mean(balance), median = median(balance)), keyby = job]
bank_data[, .(.N, mean = mean(balance), median = median(balance)), keyby = .(job, education)]
