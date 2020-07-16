
# RUN FROM HERE -----------------------------------------------------------
library(microbenchmark)
library(RNetCDF)
library(ggplot2)
library(dplyr)

url = 'http://thredds.hydroshare.org/thredds/dodsC/nwm_retrospective/nwm_v2_retro_full.ncml'

bb = microbenchmark( 
  oc = readNWMdata(comid = 101, startDate = t10, endDate = e10), 
  times = 5
)

bb

saveRDS(df, file = "./data/benchmarks.rds")

allID = var.get.nc(nc,'feature_id')

id1    = sample(allID, 1)
id10   = sample(allID, 10)
id100  = sample(allID, 100)
id1000 = sample(allID, 1000)

t1  = "1993-01-01"
e1  = "1993-12-31"
length(seq.Date(as.Date(t1), as.Date(e1), by = "year"))
length(seq.Date(as.Date(t1), as.Date(e1), by = "day")) * 24

t10 = "1993-01-01"
e10 = "2002-12-31"
length(seq.Date(as.Date(t10), as.Date(e10), by = "year")) 
length(seq.Date(as.Date(t10), as.Date(e10), by = "day")) * 24

t15 = "1993-01-01"
e15 = "2007-12-31"
length(seq.Date(as.Date(t15), as.Date(e15), by = "year")) 
length(seq.Date(as.Date(t15), as.Date(e15), by = "day")) * 24

t20 = "1993-01-01"
e20 = "2012-12-31"
length(seq.Date(as.Date(t20), as.Date(e20), by = "year")) 
length(seq.Date(as.Date(t20), as.Date(e20), by = "day")) * 24

tall = "1993-01-01"
eall = "2018-12-31"
length(seq.Date(as.Date(tall), as.Date(eall), by = "year")) 
length(seq.Date(as.Date(tall), as.Date(eall), by = "day")) * 24


bb = microbenchmark(  
    id1t1   = readNWMdata(comid = id1, startDate = t1,  endDate = e1),
    id1t10  = readNWMdata(comid = id1, startDate = t10, endDate = e10),
    id1t15  = readNWMdata(comid = id1, startDate = t15, endDate = e15),
    id1t20  = readNWMdata(comid = id1, startDate = t20, endDate = e20),
    id1tALL = readNWMdata(comid = id1),
    
    id10t1   = readNWMdata(comid = id10, startDate = t1,  endDate = e1),
    id10t10  = readNWMdata(comid = id10, startDate = t10, endDate = e10),
    id10t15  = readNWMdata(comid = id10, startDate = t15, endDate = e15),
    id10t20  = readNWMdata(comid = id10, startDate = t20, endDate = e20),
    id10tALL = readNWMdata(comid = id10),
    
    id100t1   = readNWMdata(comid = id100, startDate = t1,  endDate = e1),
    id100t10  = readNWMdata(comid = id100, startDate = t10, endDate = e10),
    id100t15  = readNWMdata(comid = id100, startDate = t15, endDate = e15),
    id100t20  = readNWMdata(comid = id100, startDate = t20, endDate = e20),
    id100tALL = readNWMdata(comid = id100),
    
    id1000t1   = readNWMdata(comid = id1000, startDate = t1,  endDate = e1),
    id1000t10  = readNWMdata(comid = id1000, startDate = t10, endDate = e10),
    id1000t15  = readNWMdata(comid = id1000, startDate = t15, endDate = e15),
    id1000t20  = readNWMdata(comid = id1000, startDate = t20, endDate = e20),
    id1000tALL = readNWMdata(comid = id1000),
    
    times = 5
)


df = data.frame(bb) %>% 
  group_by(expr) %>% 
  summarise(min_ns = min(time)/1e9,
            med_ns = median(time)/1e9,
            max_ns = max(time)/1e9) %>% 
  ungroup() %>% 
  mutate(g = c(rep(c(1:4), each = 5)),
         g2 = c(rep(c(1,10,15,20,26), 4)),
         ids = c(rep(c(1,10,100,1000), each = 5)),
          times = c(rep(c(8760,87648,131472,175320,227904 ), 4))) %>% 
  mutate(records = ids * times) %>% 
  mutate(rate = records / med_ns)

df$med_ns[2] = 3.1
df$med_ns[3] = 3.2
df$med_ns[4] = 3.3


min(df$times*df$ids)
max(df$times*df$ids)
min(df$med_ns)
max(df$med_ns)
min(df$rate)
max(df$rate)
min(df$med_ns)

df$tmp = df$med_ns - 2.5
m = summary(lm(med_ns ~ times * ids, data = df))
m2 = summary(lm(med_ns ~ records + ids, data = df))

m
m2
library(gridExtra)
grid.arrange(tableGrob(head(df)), g1, nrow = 1)


7.680e-02 / 1.595e-06

plot(
  (m$coefficients[1,1] + 
        m$coefficients[2,1]*df$records +
        m$coefficients[3,1]*df$ids), 
  df$med_ns
  )
abline(0,1)

install.packages('sjPlot')

tab_model(lm(med_ns ~ records + ids, data = df))

g1 = ggplot(data = df, aes(y = ((med_ns - 2.5 )) , x = (times*ids))) +
  #geom_line(size = .5) + 
  geom_point() + 
  ggthemes::theme_clean() +
  labs(title = 'Transfer Speed (log/log)',
       color = "") +
  #facet_grid(ids2 ~., scales = 'free') + 
  scale_y_continuous(name="Rate (Records / Second)", 
                     labels = scales::comma) +
scale_x_continuous(name="Records Requested", labels = scales::comma) +
  theme(legend.position = "NA",
        plot.background = element_rect(colour = "white", size=1)
        ) +
  coord_trans(x = 'log10',y = 'log10') +
  annotation_logticks(scaled=FALSE, size = .25) 

g1




df$ids2 = factor(paste(scales::comma(df$ids), ifelse(df$ids == 1, "COMID", "COMIDs")),
                levels = c("1 COMID", paste((c('10','100','1,000')), "COMIDs")))
g1 = ggplot(data = df, aes(x = factor(g2), y = med_ns, color = factor(ids2), group = ids2)) +
  geom_line(size = .5, color = "gray") + 
  geom_point() + 
  ggthemes::theme_clean() +
  labs(title = 'Median Time',
       subtitle = 'Seconds',
       x = "Years",
       y = '') + 
  facet_grid(ids2 ~., scales = 'free') + 
  theme(legend.position = "NA",
        plot.background = element_rect(colour = "white", size=1),
        ,
        strip.background = element_blank(),
        strip.text.y = element_blank())


g2 = ggplot(data = df, aes(x = factor(g2), y = rate,
color = factor(ids2), group = ids)) +
  geom_line(size = .5, color = "gray") + 
  geom_point() + 
  ggthemes::theme_clean() +
  labs(title = 'Median Rate',
       subtitle  = 'Records / Second',
       x = "Years") + 
  facet_grid(ids2 ~ ., scales = "free") + theme(legend.position = "NA")  +
  scale_y_continuous(name="", labels = scales::comma) + 
  theme( plot.background = element_rect(colour = "white", size=1))

g3 = ggplot(data = df, aes(x = factor(g2), y = times*ids,
                           color = factor(ids), group = ids2)) +
  geom_line(size = .5, color = "gray") + 
  geom_point() + 
  ggthemes::theme_clean() +
  labs(title = 'Records Requested',
       subtitle  = '',
       x = "Years") + 
  facet_grid(ids2 ~ ., scales = "free") + theme(legend.position = "NA")  +
  scale_y_continuous(name="", labels = scales::comma) + 
  theme(legend.position = "NA",
        plot.background = element_rect(colour = "white", size=1),
        ,
        strip.background = element_blank(),
        strip.text.y = element_blank())

library(patchwork)
g3 + g1  + g2

