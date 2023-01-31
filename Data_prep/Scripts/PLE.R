# Calculate total number of psychotic like experiences (PLEs)
# ---------------------------------------------------
# PLE defined as visual OR auditory hallucination OR delusional belief definitely present
# AND occurs >= monthly.
# The only question about delusional belief consistent across all 
# time points is about being spied upon/followed.

# For people that do not have a PLE code FALSE for no or maybe for visual or auditory hallucination or delusion 
# or if they have a visual or auditory or delusion but less than once a month

# We have two ways data was collected: self reported OR by an interviewer
# because people are more likely to endorse a PLE through self reporting 
# we will add a covariate for the method of data collection?

# Child completed ages: 11 (cck), 13 (ccn), 14 (ccr), 16 (ccs), 21 (YPA), 26 (YPF)
# Clinic based completed: 12, 18 (FJPL), 24
# ---------------------------------------------------

library(kableExtra)
# ---------------------------------------------------
# Create col for each time point of whether PLE occured or not (TRUE or FALSE)

dataSmfqPle <-  dataSmfq %>%
  mutate(ple_t01 = ifelse( # time point at ~11 years; freq = once a month or every week
    ( cck360 == 3 ) & ( cck362 %in% c(4,5) ) |  # voices (yes, definitely) & freq 
    ( cck420 == 3 ) & ( cck422 %in% c(4,5) ) |  # seen (yes, definitely) & freq 
    ( cck340 == 3 ) & ( cck342 %in% c(4,5) ) , TRUE, # spied/followed (yes, definitely) & freq 
                   ifelse( 
    ( cck360 %in% c(1:2) ) | # voices: no or maybe
    ( cck360 == 3 ) & ( cck362 %in% c(1:3) ) | # voices: yes definitely and freq not at all, once or twice or < monthly.
    ( cck420 %in% c(1:2) ) | # seen: no or maybe
    ( cck420 == 3 ) & ( cck422 %in% c(1:3) ) | # seen: yes definitely and freq not at all, once or twice or < monthly.
    ( cck340 %in% c(1:2) ) | # followed: no or maybe
    ( cck340 == 3 ) & ( cck342 %in% c(1:3) )  # followed: yes definitely and freq not at all, once or twice or < monthly.
                                             , FALSE, NA)   )) %>% 

    mutate(ple_t02 = ifelse( # time point at ~ 12 years (clinic); freq = Quite often (monthly), Often (almost weekly), Most of the time (most days)
    ( ff5060 == 1 ) & ( ff5062 %in% c(2:4) ) | # Since 12th birthday teenager has had visual illusion/hallucination
    ( ff5030 == 1 ) & ( ff5032 %in% c(2:4) ) | # Since 12th birthday teenager heard voices
    ( ff5100 == 1 ) & ( ff5102 %in% c(2:4) ) , TRUE, # Since 12th birthday teenager has had delusions of being spied on
                     ifelse( 
    ( ff5060 %in% c(2:3) ) | # visuals: no or maybe
    ( ff5060 == 1 ) & ( ff5032 == 1 ) | # visuals: yes but only once or twice
    ( ff5030 %in% c(2:3) ) | # heard: no or maybe
    ( ff5030 == 1 ) & ( ff5032 == 1 ) | # heard: yes but only once or twice
    ( ff5100 %in% c(2:3) ) | # spied: no or maybe
    ( ff5100 == 1 ) & ( ff5102 == 1) # spied: yes but only once or twice
                                             , FALSE, NA )    )) %>% 

  mutate(ple_t03 = ifelse( # time point at ~13 years; freq = Quite often (~once/month), Often (~once/week), Nearly every day
    ( ccn240 == 3 ) & ( ccn242 %in% c(4:6) ) |  # voices (yes, definitely) & freq 
    ( ccn260 == 3 ) & ( ccn262 %in% c(4:6) ) |  # seen (yes, definitely) & freq 
    ( ccn230 == 3 ) & ( ccn232 %in% c(4:6) ) , TRUE, # spied/followed (yes, definitely) & freq 
                   ifelse(
    ( ccn240 %in% c(1:2) ) | # voices: no or maybe
    ( ccn240 == 3 ) & ( ccn242 %in% c(1:3) ) | # voices: yes but not at all, once or twice or < once a month.
    ( ccn260 %in% c(1:2) ) | # heard: no or maybe
    ( ccn260 == 3 ) & ( ccn262 %in% c(1:3) ) | # heard: yes but not at all, once or twice or < once a month.
    ( ccn230 %in% c(1:2) ) | # followed: no or maybe
    ( ccn230 == 3 ) & ( ccn232 %in% c(1:3) ) # followed: yes but not at all, once or twice or < once a month.
                                             , FALSE, NA  )  )) %>% 
  
  mutate(ple_t04 = ifelse( # time point at ~14 years; freq = Quite often (~once/month), Often (~once/week), Nearly every day
    ( ccr360 == 3 ) & ( ccr363 %in% c(4:6) ) |  # voices (yes, definitely) & freq 
    ( ccr380 == 3 ) & ( ccr383 %in% c(4:6) ) |  # seen (yes, definitely) & freq 
    ( ccr340 == 3 ) & ( ccr343 %in% c(4:6) ) , TRUE, # spied/followed (yes, definitely) & freq 
                   ifelse(
    ( ccr360 %in% c(1:2) ) |
    ( ccr360 == 3 ) & ( ccr363 %in% c(1:3) ) |
    ( ccr380 %in% c(1:2) ) |
    ( ccr380 == 3 ) & ( ccr383 %in% c(1:3) ) |
    ( ccr340 %in% c(1:2) ) |
    ( ccr340 == 3 ) & ( ccr343 %in% c(1:3) )
                                             , FALSE, NA  )  )) %>%  
  
  mutate(ple_t05 = ifelse( # time point at ~16 years; freq = more than once a month or nearly every day
    ( ccs2560 == 1 ) & ( ccs2563 %in% c(3,4) ) |  # voices (yes, definitely) & freq 
    ( ccs2600 == 1 ) & ( ccs2603 %in% c(3,4) ) |  # seen (yes, definitely) & freq 
    ( ccs2540 == 1 ) & ( ccs2543 %in% c(3,4) ) , TRUE, # spied/followed (yes, definitely) & freq 
                   ifelse(
    ( ccs2560 %in% c(2:3) ) |
    ( ccs2560 == 1 ) & ( ccs2563 %in% c(1,2) ) |
    ( ccs2600 %in% c(2:3) ) |
    ( ccs2600 == 1 ) & ( ccs2603 %in% c(1,2) ) |
    ( ccs2540 %in% c(2:3) ) |
    ( ccs2540 == 1 ) & ( ccs2543 %in% c(1,2) )   
                                               , FALSE, NA  )  )) %>%  

  mutate(ple_t06 = ifelse( # time point at ~18 years (clinic); freq = Quite often (monthly), Often (almost weekly), Most of the time (most days)
    ( FJPL027 == 3 ) & ( FJPL030 %in% c(2:4) ) |  # has heard the voice (yes, definitely) & freq 
    ( FJPL044 == 3 ) & ( FJPL047 %in% c(2:4) ) |  # seen visions (yes, definitely) & freq 
    ( FJPL056 == 3 ) & ( FJPL059 %in% c(2:4) ) , TRUE, # spied/followed (yes, definitely) & freq 
                    ifelse(
    ( FJPL027 %in% c(1:2) ) |
    ( FJPL027 == 3 ) & ( FJPL030 %in% c(0:1) ) |
    ( FJPL044 %in% c(1:2) ) |
    ( FJPL044 == 3 ) & ( FJPL047 %in% c(0:1) ) |
    ( FJPL056 %in% c(1:2) ) |
    ( FJPL056 == 3 ) & ( FJPL059 %in% c(0:1) )
                                               , FALSE, NA  )  )) %>%  
  
  mutate(ple_t07 = ifelse( # time point at ~21 years; freq = more than once a month or nearly every day
    ( YPA2130 == 1 ) & ( YPA2131 %in% c(3,4) ) |  # voices (yes, definitely) & freq 
    ( YPA2140 == 1 ) & ( YPA2141 %in% c(3,4) ) |  # seen (yes, definitely) & freq 
    ( YPA2150 == 1 ) & ( YPA2151 %in% c(3,4) ) , TRUE, # spied/followed (yes, definitely) & freq 
                  ifelse(
    ( YPA2130 %in% c(2:3) ) |
    ( YPA2130 == 1 ) & ( YPA2131 %in% c(1,2) ) |
    ( YPA2140 %in% c(2:3) ) |
    ( YPA2140 == 1 ) & ( YPA2141 %in% c(1,2) ) |
    ( YPA2150 %in% c(2:3) ) |
    ( YPA2150 == 1 ) & ( YPA2151 %in% c(1,2) )   
                                               , FALSE, NA  )  )) %>% 
  
  mutate(ple_t08 = ifelse( # time point at ~24 years (clinic); freq = Quite often (monthly), Often (almost weekly), Most of the time (most days), Daily
    ( FKPL1300 == 3 ) & ( FKPL1303 %in% c(2:5) ) |  # has heard the voice (yes, definitely) & freq 
    ( FKPL1600 == 3 ) & ( FKPL1603 %in% c(2:5) ) |  # seen visions (yes, definitely) & freq 
    ( FKPL1700 == 3 ) & ( FKPL1703 %in% c(2:5) ) , TRUE, # spied/followed (yes, definitely) & freq 
                  ifelse(
    ( FKPL1300 %in% c(1:2) ) |
    ( FKPL1300 == 3 ) & ( FKPL1303 %in% c(0:1) ) |
    ( FKPL1600 %in% c(1:2) ) |
    ( FKPL1600 == 3 ) & ( FKPL1603 %in% c(0:1) ) |
    ( FKPL1700 %in% c(1:2) ) |
    ( FKPL1700 == 3 ) & ( FKPL1703 %in% c(0:1) )
                                                  , FALSE, NA  )  )) %>% 
  
  mutate(ple_t09 = ifelse( # time point at ~26 years; freq = more than once a month or nearly every day
    ( YPF7010 == 1 ) & ( YPF7030 %in% c(3,4) ) |  # voices (yes, definitely) & freq 
    ( YPF7040 == 1 ) & ( YPF7060 %in% c(3,4) ) |  # seen (yes, definitely) & freq 
    ( YPF7070 == 1 ) & ( YPF7090 %in% c(3,4) ) , TRUE, # spied/followed (yes, definitely) & freq 
                   ifelse(
    ( YPF7010 %in% c(0,2) ) |
    ( YPF7010 == 1 ) & ( YPF7030 %in% c(0:2) ) |
    ( YPF7040 %in% c(0,2) ) |
    ( YPF7040 == 1 ) & ( YPF7060 %in% c(0:2) ) |
    ( YPF7070 %in% c(0,2) ) |
    ( YPF7070 == 1 ) & ( YPF7090 %in% c(0:2) )   
                                               , FALSE, NA  )  ))


# ----------------------------------------
# Calculate n with PLE, n wihtout PLE, total sample size and % with PLE
timePoint <- paste0("ple_t", str_pad(1:9, pad = 0,width = 2 , "left"))

occasions <- c(2,3,4,6,7,8,10,13,15)

PleStats <- lapply(1:length(timePoint), function(i){
  Occasion <- occasions[i]
  vector <- pull(dataSmfqPle, timePoint[i])
  missing <- sum(is.na(vector))
  PleTrue <- sum(vector == TRUE, na.rm = T)
  PleFalse <- sum(vector == FALSE, na.rm = T)
  total <- PleTrue + PleFalse
  percentPle <- round((PleTrue/total)*100 , 2)
  DescStats <- data.frame(Occasion, 
                          "Sample Size" = total, 
                          "PE" = PleTrue,
                          "No PE" = PleFalse,
                          "% PE" = percentPle,
                          "n missing" = missing)
  return(DescStats)
})
PleStats <- do.call(rbind, PleStats)
colnames(PleStats) <- c("Occasion", "Sample Size", "PE", "No PE", "% PE", "n missing")
PleStats$Acquisition <- ifelse(PleStats$Occasion %in% c(3, 8, 13), "Clinic", "Self-report")
PleStats
write.csv(PleStats, "Output/PLE_Descriptive_Statistics.csv", row.names = F)

tab <- PleStats %>%
  kbl() %>%
  kable_classic(full_width = F, html_font = "helvet")
tab
save_kable(tab, "Output/PLE_Descriptive_Statistics.png", zoom = 1.5)
# ----------------------------------------

# ----------------------------------------
# Calculate total number of PLEs and number of appts attended
dataSmfqPle$PLE_total <- rowSums(dplyr::select(dataSmfqPle, timePoint), na.rm = T)

# some of those 0s should be NA though for people that didn't attend ANY appointments 
dataSmfqPle$PLE_total[ rowSums(is.na( dplyr::select(dataSmfqPle, timePoint) )) == 9 ] <- NA

# Also add col of number of appointments participant attended
dataSmfqPle$PLE_appts <- rowSums(!is.na( dplyr::select(dataSmfqPle, timePoint )))

head(dplyr::select(dataSmfqPle, timePoint))
dataSmfqPle$PLE_total %>% summary()
dataSmfqPle$PLE_appts %>% summary()


# ----------------------------------------
# Get ages of participant at each time point, and determine if they were assessed on PLE and depression or just PLE

# age 11 (PLE assessed only)  cck991a
# age 12 (PLE and depression)  ff0011a
# age 13 (PLE assessed only)  ccn991a
# age 14 (PLE assessed only)  ccr991a
# age 16 (PLE and depression)  ccs9991a
# age 18 (PLE assessed only)  FJ003a 
# age 21 (PLE and depression)  YPA9020
# age 24 (PLE only) FKAR0010
# age 26 (PLE assessed only)  YPF9520

# ----------------------------------------
# Change variable to correct name for rest of clean_data script.
dataSmfq <- dataSmfqPle
rm(dataSmfqPle)

# ----------------------------------------




