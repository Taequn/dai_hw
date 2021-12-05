library("boot")
data("nodal")
?nodal

nodal <- nodal %>%
  mutate(sev = case_when(xray==1~1,
                         grade==1~1,
                         stage==1~1,
                         TRUE ~ 0))

ggplot(nodal, aes(x=factor(r),
                  fill=factor(sev)))+
  geom_bar(position="dodge")+
  geom_hline(yintercept=0)+
  theme_bw()

table(nodal$sev, nodal$r)
prop.table(table(nodal$sev, nodal$r), 
           margin = 2)
prop.table(table(nodal$sev, nodal$r), 
           margin = 1)

# "success" = nodal involvement

#non severe
x1<-1
n1<-16
(p.hat1<- x1/n1)

library(binom)
binom.confint(x=x1, n=n1)
# 95% CI: 0.0111193448 0.2832874

#severe
x2<-19
n2<-37
(p.hat2<- x2/n2)
library(binom)
binom.confint(x=x2, n=n2)
# 95% CI: 0.3589490 0.6655360

# Two sample approach
prop.test(x=c(x1,x2),
          n=c(n1,n2))
# -0.6957887 -0.2062383

#H0: p1-p2 = 0
#Ha: p1-p2 != 0
# p-value: 0.005094 < 0.05 --> Reject H0 in favor of Ha
#                          --> We have evidence that nodal involvement is different
#                          --> in patients with severe sympotoms (z=2.8, p=0.0051).