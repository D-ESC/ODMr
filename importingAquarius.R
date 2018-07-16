VARS <- ODM %>% tbl('Aquarius_TimeSeries') %>% select(LABEL_) %>% distinct() %>% collect()
DAT <- ODM %>% tbl('Aquarius_TimeSeries') %>% filter(LABEL_ == "CB2 HS QR DailyMean") %>% collect()
DAT <- DAT %>% mutate(GRADE_ = case_when(GRADE_ == 2 ~ 105,
                                         GRADE_ == 100 ~ 107,
                                         GRADE_ == -10 ~ 101,
                                         GRADE_ == 60 ~ 107))
SG <- ODMcreate(DAT$TIMESTAMP_, DAT$VALUE_, SiteID = 25, VariableID = 111, MethodID = 25,
                QualifierID = DAT$GRADE_)
SG <- SG %>% filter(!is.na(DataValue))
SG <- distinct(SG)
ODMload(SG, 0)
SG$QualityControlLevelID = 1
ODMload(SG, 1)

DAT <- ODM %>% tbl('Aquarius_TimeSeries') %>% filter(LABEL_ == "BCOF CHT HG DailyMean") %>% collect()
SG <- ODMcreate(DAT$TIMESTAMP_, DAT$VALUE_, SiteID = 21, VariableID = 310, MethodID = 22)
SG <- distinct(SG)
ODMload(SG, 0)
