library(tidyverse)
library(lubridate)
library(dplyr)
library(readxl)
library(gridExtra)
install.packages("gridExtra")
bbt<- read_csv("./data/BigBangTheory.csv")
view(bbt)
# Question #1: BigBangTheory
# a. Compute the minimum and the maximum number of viewers.
bbt %>% summarise(
  min=min(`Viewers (millions)`),
  max=max(`Viewers (millions)`)
)

# b. Compute the mean, median, and mode.

  mean(bbt$`Viewers (millions)`)
  median(bbt$`Viewers (millions)`)
  # which.max(table(bbt$`Viewers (millions)`))
  
  freq <- table(bbt$`Viewers (millions)`)
  max_freq <- max(freq)
  modes <- as.numeric(names(freq[freq == max_freq]))
  # 输出众数
  print(modes)

# c. Compute the first and third quartiles.
  quantile(bbt$`Viewers (millions)`,0.25)
  quantile(bbt$`Viewers (millions)`,0.75)

# d. has viewership grown or declined over the 2011–2012 season? Discuss.
  bbt$date <- mdy(bbt$`Air Date`)
  bbt %>%
    arrange(desc(date)) %>%
    ggplot(aes(x = date, y = `Viewers (millions)`)) +
    geom_line() + 
    scale_x_date(date_labels = "%Y-%m-%d", date_breaks = "1 month") +
    theme(axis.text.x = element_text(angle = 90)) 
  
  
  # **Question #2:** NBAPlayerPts. (Attached Data: NBAPlayerPts)
  nba<- read_csv("./data/NBAPlayerPts.csv")
  view(nba)
  # a. Show the frequency distribution.
  breaks <- seq(10, 30, by = 2)  # 从 10 到 30，每 2 分一个区间
  
  # 使用 cut() 函数将 PPG 列分组
  nba$PPG_Group <- cut(nba$PPG, breaks = breaks, right = FALSE)
  nba
  frequency_ds <- table(nba$PPG_Group)
  frequency_ds
  # b. Show the relative frequency distribution.
  relative_frequency_ds <- prop.table(frequency_ds)
  relative_frequency_ds
  # c. Show the cumulative percent frequency distribution.
  cumulative_frequency <- cumsum(relative_frequency_ds)
  cumulative_frequency
  # d. Develop a histogram for the average number of points scored per game.
  

  ggplot(nba,aes(x=PPG))+geom_histogram(binwidth = 5)
  
  # e. Do the data appear to be skewed? Explain.
  # 通过直方图和频率曲线可以看到，数据右偏，使用skewness验证
  e1071::skewness(nba$PPG)
  # 结果验证skewness右偏
  
  # f. What percentage of the players averaged at least 20 points per game?
  goodPlayer <- filter(nba,PPG>=20)
  rate <-  nrow (goodPlayer)/nrow (nba)
  rate
  
  
  # Question 3: A researcher reports survey results by stating that the standard error of the mean is 20. The population standard deviation is 500.
  # a. How large was the sample used in this survey?
  SE=20
  sd=500
  n=(sd/SE)^2
  n
  # b. What is the probability that the point estimate was within ±25 of the population mean?
  # n>30 所有抽样符合正态分布
  
  zu <- 25/SE
  zl <- -25/SE
  pnorm(zu)-pnorm(zl)
  
  
  # Question #4: Young Professional Magazine (Attached Data: Professional)
  # a. Develop appropriate descriptive statistics to summarize the data.
  pd<- read_csv("./data/Professional.csv")
  view(pd)
  mean_age <- mean(pd$Age)  #读者年龄均值
  mean_houseIncome <- mean(pd$`Household Income ($)`) #读者家庭收入均值
  sd_age <- sd(pd$Age)
  sd_houseIncome <- sd(pd$houseIncome)
  male_prop=sum(pd$Gender=='Male',is.na=TRUE)/sum(!is.na(pd$Gender)) #样本中男性读者占比为56.1%
  # b. Develop 95% confidence intervals for the mean age and household income of subscribers.
  # 手动计算数据
  n <- length(pd$Age)  # 样本大小
  # 计算标准误差
  se <- sd_age / sqrt(n)
  ?qnorm
  # 使用qnorm计算置信区间
  confidence_interval <- qnorm(c(0.025, 0.975), mean_age, se)
  confidence_interval
  # 使用置信极值计算计算区间
  z_value <- qnorm(0.975)  # 0.975用于双尾检验（95%置信区间），z分布0对称
  z_value
  error_margin <- z_value * se
  confidence_interval <- c(mean_age - error_margin, mean_age + error_margin)
  confidence_interval
  
  # t检验代码直接获取置信区间
  result_age <- t.test(pd$Age, conf.level = 0.95)
  result_houseIncome <- t.test(pd$`Household Income ($)`, conf.level = 0.95)
  result_age 
  result_houseIncome
  
  # 我们有95%的信心确定读者的平均年龄在30到31岁，读者的平均家庭收入在71078到77840
  
  # c. Develop 95% confidence intervals for the proportion of subscribers who have broadband access at home and the proportion of subscribers who have children.
  hasKid_num=sum(pd$`Have Children?`=='Yes',na.rm = TRUE)
  broadband_num=sum(pd$`Broadband Access?`=='Yes',na.rm = TRUE)
  total_num=sum(!is.na(pd$`Have Children?`))
  result_hasKid=prop.test(hasKid_num,total_num,conf.level = 0.95)
  result_broadband=prop.test(broadband_num,total_num,conf.level = 0.95)
  
  # 我们有95%的信心有孩子的读者占总数的48.46%到58.31%，家庭中装了宽带的占比为57.53%到67.11%
  
  # d. Would Young Professional be a good advertising outlet for online brokers? Justify your conclusion with statistical data.
  networkpd <- pd %>% filter(
    `Broadband Access?`=='Yes'
  ) %>% summarise(
    buy_num=sum(`Real Estate Purchases?`=='Yes',na.rm = TRUE),
    total_num=sum(!is.na(`Real Estate Purchases?`))
  ) 
  result_propInvest=prop.test(networkpd$buy_num,networkpd$total_num,conf.level = 0.95)
  result_propInvest
  

  # 我们有95%的信心家中装了宽带的读者投资占总收入的38.76%到51.24% ,想要购买房地产的占比还是比较高的，所以读者应该是在线经纪商的客户资源
  
  
  # e. Would this magazine be a good place to advertise for companies selling educational software and computer games for young children?
  kidpd <- pd %>% filter(
    `Have Children?`=='Yes'
  ) %>% summarise(
    mean_invest=mean(`Value of Investments ($)`),
    mean_income=mean(`Household Income ($)`),
    has_pc=sum(`Real Estate Purchases?`=='Yes',na.rm = TRUE),
  ) 
  result_propInvest=prop.test(kidpd$mean_invest,kidpd$mean_income,conf.level = 0.95)
  result_propInvest
  
  result_kidwithInternet=prop.test(kidpd$has_pc,hasKid_num,conf.level = 0.95)
  result_kidwithInternet

  # 我们有95%的信心家中有孩子的读者投资占总收入的38.04%到38.74% ,投资占比还是比较高的，而家中有孩子的读者家中联网了的占比大概36.32%到49.78%，所以这个杂志是一个较好的关于孩子的教育软件和电脑游戏的广告位
  
  # f. Comment on the types of articles you believe would be of interest to readers of Young Professional.
  # 鉴于读者的平均年纪在30-31岁，且读者大概率的家中接入了宽带，同时经济的投资占比较高，所以杂志中刊登一些经济股票之类的版块会较受欢迎
  
  
  # Question #5: Quality Associate, Inc. (Attached Data: Quality)
  qua<- read_csv("./data/Quality.csv")
  view(qua)
  
  # a. Conduct a hypothesis test for each sample at the .01 level of significance and determine what action, if any, should be taken. Provide the p-value for each test.

  
  sampleTest <-  function (sampleName) {
    # 在已知方差的时候要使用z检验
    se=0.21/sqrt(length(qua[[sampleName]]))
    mean=mean(qua[[sampleName]])
    result <- (1-pnorm(abs(mean-12)/se))*2
    return (result)
  }
  result_df <- qua %>%
    summarise(across(everything(), ~sampleTest(cur_column()))) 
  result_df
  
  
  # b. compute the standard deviation for each of the four samples. does the assumption of .21 for the population standard deviation appear reasonable?
  sdCalc <- function(sampleName){
    sd <- sd(qua[[sampleName]])
    return (sd)
  }
  result_sd <- qua %>%
    summarise(across(everything(), ~sdCalc(cur_column()))) 
  result_sd[1,]

  t.test( result_sd[1,],mu=0.21)
  # p值为0.4427 在0.5的显著水平下 总体方差当作.21是合理的
  
  # c. compute limits for the sample mean x around μ = 12 such that, as long as a new sample mean is within those limits, the process will be considered to be operating satisfactorily. if x exceeds the upper limit or if x is below the lower limit, corrective action will be taken. these limits are referred to as upper and lower control limits for quality control purposes.
  qnorm(c(0.005,0.995),mean=12,sd=.21)
  
  
  # d. discuss the implications of changing the level of significance to a larger value. what mistake or error could increase if the level of significance is increased?
  # 显著水平提升之后，检测到不合格的次数会变多，会增加客户很多不必要的检测错误和改正的行为
  
  # Question #6:
  ocp<- read_csv("./data/Occupancy.csv")
  view(ocp)
  # a. Estimate the proportion of units rented during the first week of March 2007 and the first week of March 2008.
  yes_mar07 <- sum(ocp$`Mar-07` == "Yes", na.rm = TRUE)
  yes_mar08 <- sum(ocp$`Mar-08` == "Yes", na.rm = TRUE)
  total_mar07 <- sum(!is.na(ocp$`Mar-07`))
  total_mar08 <- sum(!is.na(ocp$`Mar-08`))
  prop_mar07=yes_mar07/total_mar07
  prop_mar08=yes_mar08/total_mar08
  prop_mar07
  prop_mar08
  # b. Provide a 95% confidence interval for the difference in proportions.
  prop.test(c(yes_mar07,yes_mar08), c(total_mar07,total_mar08), conf.level = 0.95,correct = FALSE)   
  # 置信区间是-0.22 -0.01
  # c. On the basis of your findings, does it appear March rental rates for 2008 will be up from those a year earlier?
  # 95%的置信区间没有包括0，置信区间为负，所以可以推断租房比例是逐年下降的
  
  # Question #7: Air Force Training Program (data file: Training)
  aft <- read.csv("./data/Training.csv")
  View(aft)
  
  # a. use appropriate descriptive statistics to summarize the training time data for each method.what similarities or differences do you observe from the sample data?
  mean(aft$Current)
  sd(aft$Current)
  
  mean(aft$Proposed)
  sd(aft$Proposed)
  
  # b. Comment on any difference between the population means for the two methods. Discuss your findings
  t.test(aft$Current,aft$Proposed)
  # p值为0.5 在0.05的显著水平下，拒绝原假设-两个方法是不同的，所以两个方法是相同的
  
  # c. compute the standard deviation and variance for each training method. conduct a hypothesis test about the equality of population variances for the two training methods. Discuss your findings.
  
  s1 <- sd(aft$Current)^2
  s2 <- sd(aft$Proposed)^2
  s1
  s2
  # H0:假设两个方法的总体方差是不等的
  var.test(aft$Current,aft$Proposed)
  # p值为0.000578 无法拒绝原假设 两个方法的总方差是不等的
  
  # d. what conclusion can you reach about any differences between the two methods? what is your
  # recommendation? explain.
  
  # 拟用的替代方案能更好的消除了学员之间的差距，倘若60分是及格分，那么均分差不多的情况下，替代方案能够保证更多的人通过测试，所以替代方案更好。
  
  # e. can you suggest other data or testing that might be desirable before making a final decision
  # on the training program to be used in the future?
  # 需要计算最高值，最低值，数据的左右偏移，来判断方案的极端情况，当遇到只考虑两端人才的时候会有用
  
  
  # Question #8
  cmy <- read.csv("./data/Camry.csv")
  View(cmy)
  # a. Develop a scatter diagram with the car mileage on the horizontal axis and the price on the
  # vertical axis.
  ggplot(cmy,aes(x=Miles..1000s.,y=Price...1000s.))+geom_point()
  
  # b. what does the scatter diagram developed in part (a) indicate about the relationship between the two variables?
  # 从图中可以看出，随着里程数的增加，价格有下降趋势
  # c. Develop the estimated regression equation that could be used to predict the price ($1000s) given the miles (1000s).
  lm_cmy <- lm(Price...1000s. ~ Miles..1000s., data = cmy)
  summary(lm_cmy)
  # 回归方程为 y=-0.0588x+16.47  (x为里程数 y为价格)
  
  # d. Test for a significant relationship at the .05 level of significance.
   # 5%显著性水平下，p值为0.000348
      
      
  # e. Did the estimated regression equation provide a good fit? Explain.
  # Multiple R-squared:  0.5387 具有一定的解释效果
  
  # f. Provide an interpretation for the slope of the estimated regression equation.
  # 回归方程的斜率是-0.0588 意味着没增加1000里程数，价格下降58.8美元
  
  # g. Suppose that you are considering purchasing a previously owned 2007 Camry that has been driven 60,000 miles. Using the estimated regression equation developed in part (c), predict the price for this car. Is this the price you would offer the seller.
  price <- (-0.0588*60+16.47)*1000
  price
  
  
  # Question #9:
  we <-  read_excel('./data/WE.xlsx') %>%
    set_names(
      "id",
      "lost",
      "happy",
      "happy_c",
      "support",
      "support_c",
      "priority",
      "priority_c",
      "login",
      "blog_c",
      "visit_c",
      "limit",
      "gap"
    )
  view(we)
  
  # a. 通过可视化探索流失客户与⾮流失客户的⾏为特点（或特点对⽐），你能发现流失与⾮流失客户⾏为在哪些指标有可能存在显著不同？
  # 添加流失状态标签
  we <- we %>%
    mutate(Churn = ifelse(lost == 1, "Churned", "Not Churned"))
  
  # 计算描述性统计
  desc_stats <- we %>%
    group_by(Churn) %>%
    summarise(
      count = n(),
      happy_mean = mean(happy, na.rm = TRUE),
      support_mean = mean(support, na.rm = TRUE),
      priority_mean = mean(priority, na.rm = TRUE),
      login_mean = mean(login, na.rm = TRUE),
      blog_c_mean = mean(blog_c, na.rm = TRUE),
      visit_c_mean = mean(visit_c, na.rm = TRUE),
      limit_mean = mean(limit, na.rm = TRUE),
      gap_mean = mean(gap, na.rm = TRUE)
    )
  print(desc_stats)
  
  # 选择需要比较的指标
  metrics <- c("happy", "happy_c","support","support_c", "priority","priority_c", "login", "blog_c", "visit_c", "limit", "gap")
  
  # 创建箱线图
  plots <- list()
  for (metric in metrics) {
    p <- ggplot(we, aes(x = Churn, y = .data[[metric]], fill = Churn)) +
      geom_boxplot() +
      labs(title = paste("Boxplot of", metric, "by Churn Status"), x = "Churn Status", y = metric) +
      theme_minimal() +
      theme(legend.position = "none")
    plots[[metric]] <- p
  }
  
  # 显示所有箱线图
  do.call(grid.arrange, c(plots, ncol = 2))
  
  
  # 创建直方图
  plots_hist <- list()
  for (metric in metrics) {
    p <- ggplot(we, aes(x = .data[[metric]], fill = Churn)) +
      geom_histogram(alpha = 0.6, position = "identity", bins = 30) +
      labs(title = paste("Histogram of", metric, "by Churn Status"), x = metric, y = "Count") +
      theme_minimal()
    plots_hist[[metric]] <- p
  }
  
  # 显示所有直方图
  do.call(grid.arrange, c(plots_hist, ncol = 2))
  # b. 通过均值⽐较的⽅式验证上述不同是否显著。
  # t检验方式测试是否显著
  # 定义函数进行t检验
  perform_t_test <- function(data, metric) {
    churned <- data %>% filter(Churn == "Churned") %>% pull(metric)
    not_churned <- data %>% filter(Churn == "Not Churned") %>% pull(metric)
    test <- t.test(churned, not_churned)
    return(test$p.value)
  }
  
  # 计算各指标的p值
  p_values <- sapply(metrics, function(x) perform_t_test(we, x))
  # 结果输出
  p_values_df <- data.frame(
    Metric = metrics,
    P_Value = p_values
  )
  print(p_values_df)
  # 结果发现 ‘客户流失’与 ‘服务优先级相比上月的变化’ 和 ‘客户支持相比上月的变化’无关
  
  # c. 以”流失“为因变量，其他你认为重要的变量为⾃变量（提示：a、b两步的发现），建⽴回归⽅ 程对是否流失进⾏预测。
  # 针对数值和分类数据的比较 使用logit回归
  set.seed(1234)
  we_logit<-glm(lost ~ happy + support + priority+login + visit_c + limit
                + gap + happy_c   +blog_c+visit_c,
                data = we,
                family = binomial(link = "logit"))
  summary(we_logit)
  library(car)
  vif(we_logit)
  
  # d. 根据上⼀步预测的结果，对尚未流失（流失=0）的客户进⾏流失可能性排序，并给出流失可能 性最⼤的前100名⽤户ID列表。

  we$churn_prob <- predict(we_logit, newdata = we %>% select(-id, -lost), type = "response")
  
  top_100_churn_prob <- we %>%
    filter(lost == 0) %>%
    select(id, churn_prob)%>%
    arrange(desc(churn_prob)) %>%
    slice(1:100)
  
  print(top_100_churn_prob)
  