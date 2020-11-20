
## cleaning simulation result files

library(tidyverse)
library(readr)

## fixed origin - ETS & ARIMA
# level 8
fc.fix.sim.ets.arima.levelNS3048 <- read_csv('fc.fix.sim.ets.arima.levelNS3048.csv') %>%
  mutate(NS = '304', Level = '8')
fc.fix.sim.ets.arima.levelNS6088 <- read_csv('fc.fix.sim.ets.arima.levelNS6088.csv')%>%
  mutate(NS = '608', Level = '8')
fc.fix.sim.ets.arima.levelNS15208 <- read_csv('fc.fix.sim.ets.arima.levelNS15208.csv')%>%
  mutate(NS = '1520', Level = '8')
fc.fix.sim.ets.arima.levelNS30408 <- read_csv('fc.fix.sim.ets.arima.levelNS30408.csv')%>%
  mutate(NS = '3040', Level = '8')

fc.fix.sim.ets.arima.levelNS8 <- bind_rows(fc.fix.sim.ets.arima.levelNS3048, fc.fix.sim.ets.arima.levelNS6088, 
                                           fc.fix.sim.ets.arima.levelNS15208, fc.fix.sim.ets.arima.levelNS30408)
# level 10
fc.fix.sim.ets.arima.levelNS30410 <- read_csv('fc.fix.sim.ets.arima.levelNS30410.csv')%>%
  mutate(NS = '304', Level = '10')
fc.fix.sim.ets.arima.levelNS60810 <- read_csv('fc.fix.sim.ets.arima.levelNS60810.csv')%>%
  mutate(NS = '608', Level = '10')
fc.fix.sim.ets.arima.levelNS152010 <- read_csv('fc.fix.sim.ets.arima.levelNS152010.csv')%>%
  mutate(NS = '1520', Level = '10')
fc.fix.sim.ets.arima.levelNS304010 <- read_csv('fc.fix.sim.ets.arima.levelNS304010.csv')%>%
  mutate(NS = '3040', Level = '10')

fc.fix.sim.ets.arima.levelNS10 <- bind_rows(fc.fix.sim.ets.arima.levelNS30410, fc.fix.sim.ets.arima.levelNS60810, 
                                           fc.fix.sim.ets.arima.levelNS152010, fc.fix.sim.ets.arima.levelNS304010)

# level 12
fc.fix.sim.ets.arima.levelNS30412 <- read_csv('fc.fix.sim.ets.arima.levelNS30412.csv')%>%
  mutate(NS = '304', Level = '12')
fc.fix.sim.ets.arima.levelNS60812 <- read_csv('fc.fix.sim.ets.arima.levelNS60812.csv')%>%
  mutate(NS = '608', Level = '12')
fc.fix.sim.ets.arima.levelNS152012 <- read_csv('fc.fix.sim.ets.arima.levelNS152012.csv')%>%
  mutate(NS = '1520', Level = '12')
fc.fix.sim.ets.arima.levelNS304012 <- read_csv('fc.fix.sim.ets.arima.levelNS304012.csv')%>%
  mutate(NS = '3040', Level = '12')

fc.fix.sim.ets.arima.levelNS12 <- bind_rows(fc.fix.sim.ets.arima.levelNS30412, fc.fix.sim.ets.arima.levelNS60812, 
                                            fc.fix.sim.ets.arima.levelNS152012, fc.fix.sim.ets.arima.levelNS304012)

# level 18
fc.fix.sim.ets.arima.levelNS30418 <- read_csv('fc.fix.sim.ets.arima.levelNS30418.csv')%>%
  mutate(NS = '304', Level = '18')
fc.fix.sim.ets.arima.levelNS60818 <- read_csv('fc.fix.sim.ets.arima.levelNS60818.csv')%>%
  mutate(NS = '608', Level = '18')
fc.fix.sim.ets.arima.levelNS152018 <- read_csv('fc.fix.sim.ets.arima.levelNS152018.csv')%>%
  mutate(NS = '1520', Level = '18')
fc.fix.sim.ets.arima.levelNS304018 <- read_csv('fc.fix.sim.ets.arima.levelNS304018.csv')%>%
  mutate(NS = '3040', Level = '18')

fc.fix.sim.ets.arima.levelNS18 <- bind_rows(fc.fix.sim.ets.arima.levelNS30418, fc.fix.sim.ets.arima.levelNS60818, 
                                            fc.fix.sim.ets.arima.levelNS152018, fc.fix.sim.ets.arima.levelNS304018)


write_csv(fc.fix.sim.ets.arima.levelNS8, 'fc.fix.sim.ets.arima.levelNS8.csv')
write_csv(fc.fix.sim.ets.arima.levelNS10, 'fc.fix.sim.ets.arima.levelNS10.csv')
write_csv(fc.fix.sim.ets.arima.levelNS12, 'fc.fix.sim.ets.arima.levelNS12.csv')
write_csv(fc.fix.sim.ets.arima.levelNS18, 'fc.fix.sim.ets.arima.levelNS18.csv')


### fixed origin - OLS

OLS.fix.NSlevel304.8 <- read_csv('OLS.fix.NSlevel304.8.csv')%>%
  mutate(NS = '304', Level = '8')
OLS.fix.NSlevel608.8 <- read_csv('OLS.fix.NSlevel608.8.csv')%>%
  mutate(NS = '608', Level = '8')
OLS.fix.NSlevel1520.8 <- read_csv('OLS.fix.NSlevel1520.8.csv')%>%
  mutate(NS = '1520', Level = '8')
OLS.fix.NSlevel3040.8 <- read_csv('OLS.fix.NSlevel3040.8.csv')%>%
  mutate(NS = '3040', Level = '8')

OLS.fix.NSlevel304.10 <- read_csv('OLS.fix.NSlevel304.10.csv')%>%
  mutate(NS = '304', Level = '10')
OLS.fix.NSlevel608.10 <- read_csv('OLS.fix.NSlevel608.10.csv')%>%
  mutate(NS = '608', Level = '10')
OLS.fix.NSlevel1520.10 <- read_csv('OLS.fix.NSlevel1520.10.csv')%>%
  mutate(NS = '1520', Level = '10')
OLS.fix.NSlevel3040.10 <- read_csv('OLS.fix.NSlevel3040.10.csv')%>%
  mutate(NS = '3040', Level = '10')

OLS.fix.NSlevel304.12 <- read_csv('OLS.fix.NSlevel304.12.csv')%>%
  mutate(NS = '304', Level = '12')
OLS.fix.NSlevel608.12 <- read_csv('OLS.fix.NSlevel608.12.csv')%>%
  mutate(NS = '608', Level = '12')
OLS.fix.NSlevel1520.12 <- read_csv('OLS.fix.NSlevel1520.12.csv')%>%
  mutate(NS = '1520', Level = '12')
OLS.fix.NSlevel3040.12 <- read_csv('OLS.fix.NSlevel3040.12.csv')%>%
  mutate(NS = '3040', Level = '12')

OLS.fix.NSlevel304.18 <- read_csv('OLS.fix.NSlevel304.18.csv')%>%
  mutate(NS = '304', Level = '18')
OLS.fix.NSlevel608.18 <- read_csv('OLS.fix.NSlevel608.18.csv')%>%
  mutate(NS = '608', Level = '18')
OLS.fix.NSlevel1520.18 <- read_csv('OLS.fix.NSlevel1520.18.csv')%>%
  mutate(NS = '1520', Level = '18')
OLS.fix.NSlevel3040.18 <- read_csv('OLS.fix.NSlevel3040.18.csv')%>%
  mutate(NS = '3040', Level = '18')

OLS.fix.levelNS <- bind_rows(OLS.fix.NSlevel304.8, OLS.fix.NSlevel608.8, OLS.fix.NSlevel1520.8, OLS.fix.NSlevel3040.8,
                             OLS.fix.NSlevel304.10, OLS.fix.NSlevel608.10, OLS.fix.NSlevel1520.10, OLS.fix.NSlevel3040.10, 
                             OLS.fix.NSlevel304.12, OLS.fix.NSlevel608.12, OLS.fix.NSlevel1520.12, OLS.fix.NSlevel3040.12, 
                             OLS.fix.NSlevel304.18, OLS.fix.NSlevel608.18, OLS.fix.NSlevel1520.18, OLS.fix.NSlevel3040.18)
write_csv(OLS.fix.levelNS, 'OLS.fix.levelNS.csv')


## rolling origin - ETS & ARIMA
# level 8
fc.rolling.sim.ets.arima.levelNS3048 <- bind_rows(read_csv('fc.arima.rolling.304.8.csv') , 
                                                  read_csv('fc.ets.rolling.304.8.csv'))%>%
  mutate(NS = '304', Level = '8')
fc.rolling.sim.ets.arima.levelNS6088 <- bind_rows(read_csv('fc.arima.rolling.608.8.csv') , 
                                                  read_csv('fc.ets.rolling.608.8.csv'))%>%
  mutate(NS = '608', Level = '8')
fc.rolling.sim.ets.arima.levelNS15208 <- bind_rows(read_csv('fc.arima.rolling.1520.8.csv') , 
                                                   read_csv('fc.ets.rolling.1520.8.csv'))%>%
  mutate(NS = '1520', Level = '8')
fc.rolling.sim.ets.arima.levelNS30408 <- bind_rows(read_csv('fc.arima.rolling.3040.8.csv') , 
                                                   read_csv('fc.ets.rolling.3040.8.csv'))%>%
  mutate(NS = '3040', Level = '8')

fc.rolling.sim.ets.arima.levelNS8 <- bind_rows(fc.rolling.sim.ets.arima.levelNS3048, fc.rolling.sim.ets.arima.levelNS6088, 
                                           fc.rolling.sim.ets.arima.levelNS15208, fc.rolling.sim.ets.arima.levelNS30408)
# level 10
fc.rolling.sim.ets.arima.levelNS30410 <- bind_rows(read_csv('fc.arima.rolling.304.10.csv') , 
                                                   read_csv('fc.ets.rolling.304.10.csv'))%>%
  mutate(NS = '304', Level = '10')
fc.rolling.sim.ets.arima.levelNS60810 <- bind_rows(read_csv('fc.arima.rolling.608.10.csv') , 
                                                   read_csv('fc.ets.rolling.608.10.csv'))%>%
  mutate(NS = '608', Level = '10')
fc.rolling.sim.ets.arima.levelNS152010 <- bind_rows(read_csv('fc.arima.rolling.1520.10.csv') , 
                                                    read_csv('fc.ets.rolling.1520.10.csv'))%>%
  mutate(NS = '1520', Level = '10')
fc.rolling.sim.ets.arima.levelNS304010 <- bind_rows(read_csv('fc.arima.rolling.3040.10.csv') , 
                                                    read_csv('fc.ets.rolling.3040.10.csv'))%>%
  mutate(NS = '3040', Level = '10')

fc.rolling.sim.ets.arima.levelNS10 <- bind_rows(fc.rolling.sim.ets.arima.levelNS30410, fc.rolling.sim.ets.arima.levelNS60810, 
                                            fc.rolling.sim.ets.arima.levelNS152010, fc.rolling.sim.ets.arima.levelNS304010)

# level 12
fc.rolling.sim.ets.arima.levelNS30412 <- bind_rows(read_csv('fc.arima.rolling.304.12.csv') , 
                                                   read_csv('fc.ets.rolling.304.12.csv'))%>%
  mutate(NS = '304', Level = '12')
fc.rolling.sim.ets.arima.levelNS60812 <- bind_rows(read_csv('fc.arima.rolling.608.12.csv') , 
                                                   read_csv('fc.ets.rolling.608.12.csv'))%>%
  mutate(NS = '608', Level = '12')
fc.rolling.sim.ets.arima.levelNS152012 <- bind_rows(read_csv('fc.arima.rolling.1520.12.csv') , 
                                                    read_csv('fc.ets.rolling.1520.12.csv'))%>%
  mutate(NS = '1520', Level = '12')
fc.rolling.sim.ets.arima.levelNS304012 <- bind_rows(read_csv('fc.arima.rolling.3040.12.csv') , 
                                                    read_csv('fc.ets.rolling.3040.12.csv'))%>%
  mutate(NS = '3040', Level = '12')

fc.rolling.sim.ets.arima.levelNS12 <- bind_rows(fc.rolling.sim.ets.arima.levelNS30412, fc.rolling.sim.ets.arima.levelNS60812, 
                                            fc.rolling.sim.ets.arima.levelNS152012, fc.rolling.sim.ets.arima.levelNS304012)

# level 18
fc.rolling.sim.ets.arima.levelNS30418 <- bind_rows(read_csv('fc.arima.rolling.304.18.csv') , 
                                                   read_csv('fc.ets.rolling.304.18.csv'))%>%
  mutate(NS = '304', Level = '18')
fc.rolling.sim.ets.arima.levelNS60818 <- bind_rows(read_csv('fc.arima.rolling.608.18.csv') , 
                                                   read_csv('fc.ets.rolling.608.18.csv'))%>%
  mutate(NS = '608', Level = '18')
fc.rolling.sim.ets.arima.levelNS152018 <- bind_rows(read_csv('fc.arima.rolling.1520.18.csv') , 
                                                    read_csv('fc.ets.rolling.1520.18.csv'))%>%
  mutate(NS = '1520', Level = '18')
fc.rolling.sim.ets.arima.levelNS304018 <- bind_rows(read_csv('fc.arima.rolling.3040.18.csv') , 
                                                    read_csv('fc.ets.rolling.3040.18.csv'))%>%
  mutate(NS = '3040', Level = '18')

fc.rolling.sim.ets.arima.levelNS18 <- bind_rows(fc.rolling.sim.ets.arima.levelNS30418, fc.rolling.sim.ets.arima.levelNS60818, 
                                            fc.rolling.sim.ets.arima.levelNS152018, fc.rolling.sim.ets.arima.levelNS304018)


write_csv(fc.rolling.sim.ets.arima.levelNS8, 'fc.rolling.sim.ets.arima.levelNS8.csv')
write_csv(fc.rolling.sim.ets.arima.levelNS10, 'fc.rolling.sim.ets.arima.levelNS10.csv')
write_csv(fc.rolling.sim.ets.arima.levelNS12, 'fc.rolling.sim.ets.arima.levelNS12.csv')
write_csv(fc.rolling.sim.ets.arima.levelNS18, 'fc.rolling.sim.ets.arima.levelNS18.csv')


### rolling origin - OLS

OLS.rolling.NSlevel304.8 <- read_csv('OLS.rolling.NSlevel304.8.csv')%>%
  mutate(NS = '304', Level = '8')
OLS.rolling.NSlevel608.8 <- read_csv('OLS.rolling.NSlevel608.8.csv')%>%
  mutate(NS = '608', Level = '8')
OLS.rolling.NSlevel1520.8 <- read_csv('OLS.rolling.NSlevel1520.8.csv')%>%
  mutate(NS = '1520', Level = '8')
OLS.rolling.NSlevel3040.8 <- read_csv('OLS.rolling.NSlevel3040.8.csv')%>%
  mutate(NS = '3040', Level = '8')

OLS.rolling.NSlevel304.10 <- read_csv('OLS.rolling.NSlevel304.10.csv')%>%
  mutate(NS = '304', Level = '10')
OLS.rolling.NSlevel608.10 <- read_csv('OLS.rolling.NSlevel608.10.csv')%>%
  mutate(NS = '608', Level = '10')
OLS.rolling.NSlevel1520.10 <- read_csv('OLS.rolling.NSlevel1520.10.csv')%>%
  mutate(NS = '1520', Level = '10')
OLS.rolling.NSlevel3040.10 <- read_csv('OLS.rolling.NSlevel3040.10.csv')%>%
  mutate(NS = '3040', Level = '10')

OLS.rolling.NSlevel304.12 <- read_csv('OLS.rolling.NSlevel304.12.csv')%>%
  mutate(NS = '304', Level = '12')
OLS.rolling.NSlevel608.12 <- read_csv('OLS.rolling.NSlevel608.12.csv')%>%
  mutate(NS = '608', Level = '12')
OLS.rolling.NSlevel1520.12 <- read_csv('OLS.rolling.NSlevel1520.12.csv')%>%
  mutate(NS = '1520', Level = '12')
OLS.rolling.NSlevel3040.12 <- read_csv('OLS.rolling.NSlevel3040.12.csv')%>%
  mutate(NS = '3040', Level = '12')

OLS.rolling.NSlevel304.18 <- read_csv('OLS.rolling.NSlevel304.18.csv')%>%
  mutate(NS = '304', Level = '18')
OLS.rolling.NSlevel608.18 <- read_csv('OLS.rolling.NSlevel608.18.csv')%>%
  mutate(NS = '608', Level = '18')
OLS.rolling.NSlevel1520.18 <- read_csv('OLS.rolling.NSlevel1520.18.csv')%>%
  mutate(NS = '1520', Level = '18')
OLS.rolling.NSlevel3040.18 <- read_csv('OLS.rolling.NSlevel3040.18.csv')%>%
  mutate(NS = '3040', Level = '18')

OLS.rolling.levelNS <- bind_rows(OLS.rolling.NSlevel304.8, OLS.rolling.NSlevel608.8, OLS.rolling.NSlevel1520.8, OLS.rolling.NSlevel3040.8,
                             OLS.rolling.NSlevel304.10, OLS.rolling.NSlevel608.10, OLS.rolling.NSlevel1520.10, OLS.rolling.NSlevel3040.10, 
                             OLS.rolling.NSlevel304.12, OLS.rolling.NSlevel608.12, OLS.rolling.NSlevel1520.12, OLS.rolling.NSlevel3040.12, 
                             OLS.rolling.NSlevel304.18, OLS.rolling.NSlevel608.18, OLS.rolling.NSlevel1520.18, OLS.rolling.NSlevel3040.18)
write_csv(OLS.rolling.levelNS, 'OLS.rolling.levelNS.csv')

