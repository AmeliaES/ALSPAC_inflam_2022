## Scripts for running data pre-processing

- Please view `Scripts/1_Clean_data.R` for order scripts should be run in. 

#### List of variables used in analysis (also listed in `Variables_used.csv`):

|ALSPAC_ID          |ALSPAC_description                                                              |Variable_Name                 |
|-------------------|--------------------------------------------------------------------------------|------------------------------|
|cidB3421           |Unique pregnancy identifier for Heather Whalley(ACGDEHFB)                       |cidB3421                      |
|qlet               |Birth order (within pregnancy)                                                  |qlet                          |
|kz021              |Participant sex                                                                 |Sex                           |
|c645               |Mums highest ed qualification                                                   |Maternal education at birth   |
|c755               |Social Class - Maternal                                                         |Maternal social class at birth|
|c804               |Child ethnic background                                                         |Ethnicity                     |
|CRP_f9             |C-Reactive protein mg/l, Focus@9                                                |CRP (age 9)                   |
|crp_TF3            |C-reactive Protein mg/l, TF3                                                    |CRP (age 15)                  |
|CRP_TF4            |C-reactive Protein mg/l, TF4                                                    |CRP (age 17)                  |
|CRP_F24            |C-Reactive Protein mg/L, Focus@24                                               |CRP (age 24)                  |
|IL6_f9             |Interleukin 6 pg/ml, Focus@9                                                    |IL-6 (age 9)                  |
|Neutrophils_F24    |Neutrophils 10^9/L, Focus@24                                                    |Neutrophils_F24               |
|Eosinophils_F24    |Eosinophils 10^9/L, Focus@24                                                    |Eosinophils_F24               |
|Basophils_F24      |Basophils 10^9/L, Focus@24                                                      |Basophils_F24                 |
|Lymphocytes_F24    |Lymphocytes 10^9/L, Focus@24                                                    |Lymphocytes_F24               |
|Monocytes_F24      |Monocytes 10^9/L, Focus@24                                                      |Monocytes_F24                 |
|f7003c             |Age (months) at Focus @ 7 visit                                                 |age_F07                       |
|f9003c             |Age (months) at F9 visit                                                        |age_F09                       |
|fd003c             |Age (months) at visit: F10                                                      |age_t01                       |
|ff0011a            |DV: Age of study child at attendance (months)                                   |age_t02                       |
|fg0011a            |DV: Age of study child at attendance (months): TF2                              |age_t03                       |
|ccs9991a           |DV: Age of study child at completion (months)                                   |age_t04                       |
|CCXD006            |DV: Age of study child at completion (months)                                   |age_t05                       |
|cct9991a           |DV: Age of study child at completion (months)                                   |age_t06                       |
|YPA9020            |DV: Age at completion (in months)                                               |age_t07                       |
|YPB9992            |DV: Respondent age at completion (months)                                       |age_t08                       |
|YPC2650            |DV: Age of study young person at completion (months)                            |age_t09                       |
|YPE9660            |Respondent's age in months when questionnaire completed                         |age_t10                       |
|covid4yp_9650      |DV: Age in years on day Q completed: COVID4                                     |age_t11                       |
|cck991a            |DV: Age of study child at completion (months)                                   |PLE_11                        |
|ccn991a            |DV: Age of study child at completion (months)                                   |PLE_13                        |
|ccr991a            |DV: Age of study child at completion (months)                                   |PLE_14                        |
|FJ003a             |Age in months at clinic visit [F17]                                             |PLE_18                        |
|FKAR0010           |Age at clinic visit (in months): F@24                                           |PLE_24                        |
|YPF9520            |Respondent's age in months when questionnaire completed                         |PLE_26                        |
|FKPL2010           |DV: PLIKS (definite): F@24                                                      |PSYCH_24_definite             |
|FKPL2240           |DV: Psychotic disorder ever since 12: F@24                                      |PSYCH_24_disorder             |
|FJPL162            |DV: Interviewer rating of child having >=1 definite psychotic experience: psycho|PSYCH_18_definite             |
|f9ms026a           |BMI: F9                                                                         |BMI_age9                      |
|f7ms026            |Weight (kg): F7                                                                 |weight_kg_age7                |
|f7ms010            |Height (cm): F7                                                                 |height_cm_age7                |
|fh3000             |M5: Height (cms): TF3                                                           |height_cm_age15               |
|fh3010             |M15: Weight (Kgs): TF3                                                          |weight_kg_age15               |
|FJMR022a           |DV: BMI [F17]                                                                   |BMI_age17                     |
|fddp110            |Depression Item 1, Felt miserable: F10                                          |(Depression Time Point 1)     |
|fddp112            |Depression Item 3, Not enjoyed anything: F10                                    |(Depression Time Point 1)     |
|fddp113            |Depression Item 4, Felt tired, sat around: F10                                  |(Depression Time Point 1)     |
|fddp114            |Depression Item 5, Felt very restless: F10                                      |(Depression Time Point 1)     |
|fddp115            |Depression Item 6, Felt I was no good: F10                                      |(Depression Time Point 1)     |
|fddp116            |Depression Item 7, Cried alot: F10                                              |(Depression Time Point 1)     |
|fddp118            |Depression Item 9, Found it hard to think: F10                                  |(Depression Time Point 1)     |
|fddp119            |Depression Item 10, Hated myself: F10                                           |(Depression Time Point 1)     |
|fddp121            |Depression Item 12, Bad person: F10                                             |(Depression Time Point 1)     |
|fddp122            |Depression Item 13, Felt lonely: F10                                            |(Depression Time Point 1)     |
|fddp123            |Depression Item 14, Nobody loved me: F10                                        |(Depression Time Point 1)     |
|fddp124            |Depression Item 15, Not as good as others: F10                                  |(Depression Time Point 1)     |
|fddp125            |Depression Item 16, Did everything wrong: F10                                   |(Depression Time Point 1)     |
|ff6500             |DEP10: Teenager felt miserable or unhappy: depression: TF1                      |(Depression Time Point 2)     |
|ff6502             |DEP30: Teenager didnt enjoy anything at all: depression: TF1                    |(Depression Time Point 2)     |
|ff6503             |DEP40: Teenager felt so tired, just sat around and did nothing: depression: TF1 |(Depression Time Point 2)     |
|ff6504             |DEP50: Teenager was very restless: depression: TF1                              |(Depression Time Point 2)     |
|ff6505             |DEP60: Teenager felt was no good any more: depression: TF1                      |(Depression Time Point 2)     |
|ff6506             |DEP70: Teenager cried a lot: depression: TF1                                    |(Depression Time Point 2)     |
|ff6508             |DEP90: Teenager found it hard to think properly or concentrate: depression: TF1 |(Depression Time Point 2)     |
|ff6509             |DEP100: Teenager hated self: depression: TF1                                    |(Depression Time Point 2)     |
|ff6511             |DEP120: Teenager thought they were a bad person: depression: TF1                |(Depression Time Point 2)     |
|ff6512             |DEP130: Teenager felt lonely: depression: TF1                                   |(Depression Time Point 2)     |
|ff6513             |DEP140: Teenager thought nobody really loved them: depression: TF1              |(Depression Time Point 2)     |
|ff6514             |DEP150: Teenager thought they could never be as good as other kids: depression: |(Depression Time Point 2)     |
|ff6515             |DEP160: Teenager felt did everything wrong: depression: TF1                     |(Depression Time Point 2)     |
|fg7210             |DEP10: Teenager felt miserable or unhappy in the last two weeks: TF2            |(Depression Time Point 3)     |
|fg7212             |DEP30: Teenager didn't enjoy anything at all in the last two weeks: TF2         |(Depression Time Point 3)     |
|fg7213             |DEP40: Teenager felt so tired he/she just sat around and did nothing in the last|(Depression Time Point 3)     |
|fg7214             |DEP50: Teenager was very restless in the last two weeks: TF2                    |(Depression Time Point 3)     |
|fg7215             |DEP60: Teenager felt he/she was no good any more in the last two weeks: TF2     |(Depression Time Point 3)     |
|fg7216             |DEP70: Teenager cried a lot in the last two weeks: TF2                          |(Depression Time Point 3)     |
|fg7218             |DEP90: Teenager found it hard to think properly or concentrate in the last two w|(Depression Time Point 3)     |
|fg7219             |DEP100: Teenager hated him/herself in the last two weeks: TF2                   |(Depression Time Point 3)     |
|fg7221             |DEP120: Teenager was a bad person in the last two weeks: TF2                    |(Depression Time Point 3)     |
|fg7222             |DEP130: Teenager felt lonely in the last two weeks: TF2                         |(Depression Time Point 3)     |
|fg7223             |DEP140: Teenager thought nobody really loved him/her in the last two weeks: TF2 |(Depression Time Point 3)     |
|fg7224             |DEP150: Teenager thought he/she could never be as good as other kids in the last|(Depression Time Point 3)     |
|fg7225             |DEP160: Teenager did everything wrong in the last two weeks: TF2                |(Depression Time Point 3)     |
|ccs4500            |H1: YP has felt unhappy/miserable in the last 2 weeks                           |(Depression Time Point 4)     |
|ccs4502            |H3: YP hasn't enjoyed anything at all in the last 2 weeks                       |(Depression Time Point 4)     |
|ccs4503            |H4: YP has felt so tired they sat around and did nothing in the last 2 weeks    |(Depression Time Point 4)     |
|ccs4504            |H5: YP has felt very restless in the last 2 weeks                               |(Depression Time Point 4)     |
|ccs4505            |H6: YP has felt they were no good anymore in the last 2 weeks                   |(Depression Time Point 4)     |
|ccs4506            |H7: YP has cried a lot in the last 2 weeks                                      |(Depression Time Point 4)     |
|ccs4508            |H9: YP has found it hard to think properly/concentrate in the last 2 weeks      |(Depression Time Point 4)     |
|ccs4509            |H10: YP has hated themselves in the last 2 weeks                                |(Depression Time Point 4)     |
|ccs4511            |H12: YP has felt they were a bad person in the last 2 weeks                     |(Depression Time Point 4)     |
|ccs4512            |H13: YP has felt lonely in the last 2 weeks                                     |(Depression Time Point 4)     |
|ccs4513            |H14: YP has thought nobody really loved them in the last 2 weeks                |(Depression Time Point 4)     |
|ccs4514            |H15: YP has thought they could never be as good as other kids in the last 2 week|(Depression Time Point 4)     |
|ccs4515            |H16: YP has felt they did everything wrong in the last 2 weeks                  |(Depression Time Point 4)     |
|CCXD900            |Q18a: YP felt miserable or unhappy in the past 2 weeks                          |(Depression Time Point 5)     |
|CCXD902            |Q18c: YP did not enjoy anything at all in the past 2 weeks                      |(Depression Time Point 5)     |
|CCXD903            |Q18d: YP felt so tired that they just sat around and did nothing in the past 2 w|(Depression Time Point 5)     |
|CCXD904            |Q18e: YP was very restless in the past 2 weeks                                  |(Depression Time Point 5)     |
|CCXD905            |Q18f: YP felt they were no good any more in the past 2 weeks                    |(Depression Time Point 5)     |
|CCXD906            |Q18g: YP cried a lot in the past 2 weeks                                        |(Depression Time Point 5)     |
|CCXD908            |Q18i: YP found it hard to think properly or concentrate in the past 2 weeks     |(Depression Time Point 5)     |
|CCXD909            |Q18j: YP hated themselves in the past 2 weeks                                   |(Depression Time Point 5)     |
|CCXD911            |Q18l: YP felt they were a bad person in the past 2 weeks                        |(Depression Time Point 5)     |
|CCXD912            |Q18m: YP felt lonely in the past 2 weeks                                        |(Depression Time Point 5)     |
|CCXD913            |Q18n: YP thought nobody really loved them in the past 2 weeks                   |(Depression Time Point 5)     |
|CCXD914            |Q18o: YP thought they could never be as good as other kids in the past 2 weeks  |(Depression Time Point 5)     |
|CCXD915            |Q18p: YP felt they did everything wrong in the past 2 weeks                     |(Depression Time Point 5)     |
|cct2700            |A9A: In the past two weeks, respondent: felt miserable or unhappy               |(Depression Time Point 6)     |
|cct2701            |A9B: In the past two weeks, respondent: didn't enjoy anything at all            |(Depression Time Point 6)     |
|cct2702            |A9C: In the past two weeks, respondent: felt so tired that just sat around and d|(Depression Time Point 6)     |
|cct2703            |A9D: In the past two weeks, respondent: was very restless                       |(Depression Time Point 6)     |
|cct2704            |A9E: In the past two weeks, respondent: felt they were no good any more         |(Depression Time Point 6)     |
|cct2705            |A9F: In the past two weeks, respondent: cried a lot                             |(Depression Time Point 6)     |
|cct2706            |A9G: In the past two weeks, respondent: found it hard to think properly or conce|(Depression Time Point 6)     |
|cct2707            |A9H: In the past two weeks, respondent: hated self                              |(Depression Time Point 6)     |
|cct2708            |A9I: In the past two weeks, respondent: felt was bad person                     |(Depression Time Point 6)     |
|cct2709            |A9J: In the past two weeks, respondent: felt lonely                             |(Depression Time Point 6)     |
|cct2710            |A9K: In the past two weeks, respondent: thought nobody really loved them        |(Depression Time Point 6)     |
|cct2711            |A9L: In the past two weeks, respondent: thought could never be as good as others|(Depression Time Point 6)     |
|cct2712            |A9M: In the past two weeks, respondent: felt did everything wrong               |(Depression Time Point 6)     |
|YPA2000            |b1: In the past two weeks YP has felt miserable or unhappy                      |(Depression Time Point 7)     |
|YPA2010            |b2: In the past two weeks YP has felt that they didn't enjoy anything at all    |(Depression Time Point 7)     |
|YPA2020            |b3: In the past two weeks YP has felt  so tired that they just sat around and di|(Depression Time Point 7)     |
|YPA2030            |b4: In the past two weeks YP was very restless                                  |(Depression Time Point 7)     |
|YPA2040            |b5: In the past two weeks YP felt that they were no good any more               |(Depression Time Point 7)     |
|YPA2050            |b6: In the past two weeks YP thinks that they cried a lot                       |(Depression Time Point 7)     |
|YPA2060            |b7: In the past two weeks YP has found it hard to think properly or concentrate |(Depression Time Point 7)     |
|YPA2070            |b8: In the past two weeks YP has hated themselves                               |(Depression Time Point 7)     |
|YPA2080            |b9: In the past two weeks YP has felt that they are a bad person                |(Depression Time Point 7)     |
|YPA2090            |b10: In the past two weeks YP has felt lonely                                   |(Depression Time Point 7)     |
|YPA2100            |b11: In the past two weeks YP has thought nobody really loved them              |(Depression Time Point 7)     |
|YPA2110            |b12: In the past two weeks YP has thought  that they could never be as good as o|(Depression Time Point 7)     |
|YPA2120            |b13: In the past two weeks YP has felt that they did everything wrong           |(Depression Time Point 7)     |
|YPB5000            |E1: In past two weeks, felt miserable or unhappy                                |(Depression Time Point 8)     |
|YPB5010            |E2: In past two weeks, did not enjoy anything at all                            |(Depression Time Point 8)     |
|YPB5030            |E4: In past two weeks, felt so tired they just sat around and did nothing       |(Depression Time Point 8)     |
|YPB5040            |E5: In past two weeks, was very restless                                        |(Depression Time Point 8)     |
|YPB5050            |E6: In past two weeks, felt they were no good anymore                           |(Depression Time Point 8)     |
|YPB5060            |E7: In past two weeks, cried a lot                                              |(Depression Time Point 8)     |
|YPB5080            |E9: In past two weeks, found it hard to think properly or concentrate           |(Depression Time Point 8)     |
|YPB5090            |E10: In past two weeks, hated themselves                                        |(Depression Time Point 8)     |
|YPB5100            |E11: In past two weeks, felt they were a bad person                             |(Depression Time Point 8)     |
|YPB5120            |E13: In past two weeks, felt lonely                                             |(Depression Time Point 8)     |
|YPB5130            |E14: In past two weeks, thought nobody really loved them                        |(Depression Time Point 8)     |
|YPB5150            |E16: In past two weeks, thought they would never be as good as other people     |(Depression Time Point 8)     |
|YPB5170            |E18: In past two weeks, did everything wrong                                    |(Depression Time Point 8)     |
|YPC1650            |h1a: Respondent has felt miserable or unhappy in the past two weeks             |(Depression Time Point 9)     |
|YPC1651            |h1b: Respondent didn't enjoy anything at all in the past two weeks              |(Depression Time Point 9)     |
|YPC1653            |h1d: Respondent felt so tired they just sat around and did nothing in the past t|(Depression Time Point 9)     |
|YPC1654            |h1e: Respondent was very restless in the past two weeks                         |(Depression Time Point 9)     |
|YPC1655            |h1f: Respondent felt they were no good any more in the past two weeks           |(Depression Time Point 9)     |
|YPC1656            |h1g: Respondent cried a lot in the past two weeks                               |(Depression Time Point 9)     |
|YPC1658            |h1i: Respondent found it hard to think properly or concentrate in the past two w|(Depression Time Point 9)     |
|YPC1659            |h1j: Respondent hated themselves in the past two weeks                          |(Depression Time Point 9)     |
|YPC1660            |h1k: Respondent felt they were a bad person in the past two weeks               |(Depression Time Point 9)     |
|YPC1662            |h1m: Respondent felt lonely in the past two weeks                               |(Depression Time Point 9)     |
|YPC1663            |h1n: Respondent thought nobody really loved them in the past two weeks          |(Depression Time Point 9)     |
|YPC1665            |h1p: Respondent thought they would never be as good as other people in the past |(Depression Time Point 9)     |
|YPC1667            |h1r: Respondent did everything wrong in the past two weeks                      |(Depression Time Point 9)     |
|YPE4080            |f8a: In the past 2 weeks YP felt miserable/unhappy                              |(Depression Time Point 10)    |
|YPE4082            |f8c: In the past 2 weeks YP didn't enjoy anything at all                        |(Depression Time Point 10)    |
|YPE4083            |f8d: In the past 2 weeks YP felt so tired that just sat around and did nothing  |(Depression Time Point 10)    |
|YPE4084            |f8e: In the past 2 weeks YP was very restless                                   |(Depression Time Point 10)    |
|YPE4085            |f8f: In the past 2 weeks YP felt  was no good any more                          |(Depression Time Point 10)    |
|YPE4086            |f8g: In the past 2 weeks YP cried a lot                                         |(Depression Time Point 10)    |
|YPE4088            |f8i: In the past 2 weeks YP found it hard to think properly/concentrate         |(Depression Time Point 10)    |
|YPE4089            |f8j: In the past 2 weeks YP hated themselves                                    |(Depression Time Point 10)    |
|YPE4091            |f8l: In the past 2 weeks YP felt was a bad person                               |(Depression Time Point 10)    |
|YPE4092            |f8m: In the past 2 weeks YP felt lonely                                         |(Depression Time Point 10)    |
|YPE4093            |f8n: In the past 2 weeks YP thought nobody really loved them                    |(Depression Time Point 10)    |
|YPE4094            |f8o: In the past 2 weeks YP  thought  could never be as good as other people    |(Depression Time Point 10)    |
|YPE4095            |f8p: In the past 2 weeks YP felt  did everything wrong                          |(Depression Time Point 10)    |
|covid4yp_4050      |3.1a: In past two weeks participant felt miserable for unhappy: COVID4          |(Depression Time Point 11)    |
|covid4yp_4051      |3.1b: In past two weeks participant didn't enjoy anything at all: COVID4        |(Depression Time Point 11)    |
|covid4yp_4052      |3.1c: In past two weeks pp felt so tired just sat around and did nothing: COVID4|(Depression Time Point 11)    |
|covid4yp_4053      |3.1d: In past two weeks participant was very restless: COVID4                   |(Depression Time Point 11)    |
|covid4yp_4054      |3.1e: In past two weeks participant felt they were no good anymore: COVID4      |(Depression Time Point 11)    |
|covid4yp_4055      |3.1f: In past two weeks participant cried a lot: COVID4                         |(Depression Time Point 11)    |
|covid4yp_4056      |3.1g: In past two weeks pp found it hard to think properly/concentrate: COVID4  |(Depression Time Point 11)    |
|covid4yp_4057      |3.1h: In past two weeks participant hated themselves: COVID4                    |(Depression Time Point 11)    |
|covid4yp_4058      |3.1i: In past two weeks participant was a bad person: COVID4                    |(Depression Time Point 11)    |
|covid4yp_4059      |3.1j: In past two weeks participant felt lonely: COVID4                         |(Depression Time Point 11)    |
|covid4yp_4060      |3.1k: In past two weeks participant thought nobody really loved them: COVID4    |(Depression Time Point 11)    |
|covid4yp_4061      |3.1l: In past two weeks pp thought could never be as good as others: COVID4     |(Depression Time Point 11)    |
|covid4yp_4062      |3.1m: In past two weeks participant did everything wrong: COVID4                |(Depression Time Point 11)    |
|cck360             |B4: Child has heard voices others cannot hear                                   |(PLE Time Point 1)            |
|cck362             |B4a: Frequency since child's 11th birthday they have heard voices others cannot |(PLE Time Point 1)            |
|cck420             |B7: Child has seen something or someone that others could not                   |(PLE Time Point 1)            |
|cck422             |B7a: Frequency since child's 11th birthday they saw something or someone that ot|(PLE Time Point 1)            |
|cck340             |B3: Child has thought they have been spied upon or followed                     |(PLE Time Point 1)            |
|cck342             |B3a:  Frequency child thought they have been spied upon or followed since 11th b|(PLE Time Point 1)            |
|ff5060             |H17: Since 12th birthday teenager has had visual illusion/hallucination: psychos|(PLE Time Point 2)            |
|ff5062             |H19: Since 12th birthday teenager has had visual hallucination - frequency: psyc|(PLE Time Point 2)            |
|ff5030             |H1: Since 12th birthday teenager heard voices: psychosis interview: TF1         |(PLE Time Point 2)            |
|ff5032             |H3: Since 12th birthday teenager heard voices- frequency: psychosis interview: T|(PLE Time Point 2)            |
|ff5100             |D1: Since 12th birthday teenager has had delusions of being spied on: psychosis |(PLE Time Point 2)            |
|ff5102             |D3: Since 12th birthday teenager has had delusions of being spied on- frequency:|(PLE Time Point 2)            |
|ccn240             |B4:Child has heard voices others cannot                                         |(PLE Time Point 3)            |
|ccn242             |B4a: Frequency child has heard voices others cannot since 12th birthday         |(PLE Time Point 3)            |
|ccn260             |B6: Child has seen something nobody else could                                  |(PLE Time Point 3)            |
|ccn262             |B6a: Frequency child has seen something nobody else could since 12th birthday   |(PLE Time Point 3)            |
|ccn230             |B3: Child believes they have been followed or spied on                          |(PLE Time Point 3)            |
|ccn232             |B3a: Frequency child believes they were followed or spied on since 12th birthday|(PLE Time Point 3)            |
|ccr360             |C4: Respondent has heard voices that other people cannot hear                   |(PLE Time Point 4)            |
|ccr363             |C4b: Frequency the respondent has heard voices that other people cannot hear sin|(PLE Time Point 4)            |
|ccr380             |C6: Respondent has seen something other people had not                          |(PLE Time Point 4)            |
|ccr383             |C6b: Frequency at which the respondent had seen something other people had not s|(PLE Time Point 4)            |
|ccr340             |C3: The respondent was thought that they were being followed or spied upon      |(PLE Time Point 4)            |
|ccr343             |C3b: Frequency at which respondent thought they were being followed or spied upo|(PLE Time Point 4)            |
|ccs2560            |D4: YP has heard voices other people couldn't hear                              |(PLE Time Point 5)            |
|ccs2563            |D4a: Frequency YP has heard voices other people couldn't hear since their 15th b|(PLE Time Point 5)            |
|ccs2600            |D6: YP has ever seen something/someone other people could not see               |(PLE Time Point 5)            |
|ccs2603            |D6a: Frequency YP has seen something/someone other people could not see since th|(PLE Time Point 5)            |
|ccs2540            |D3: YP believes they have been followed or spied on                             |(PLE Time Point 5)            |
|ccs2543            |D3a: Frequency YP believes they have been followed or spied on since 15th birthd|(PLE Time Point 5)            |
|FJPL027            |AH1: YP has ever heard voices that other people could not hear [F17]            |(PLE Time Point 6)            |
|FJPL030            |AH3: Frequency YP has heard the voice, in last 6 months [F17]                   |(PLE Time Point 6)            |
|FJPL044            |VH1: YP has ever seen something or someone that other people could not see [F17]|(PLE Time Point 6)            |
|FJPL047            |VH4: Frequency YP has seen visions, in last 6 months [F17]                      |(PLE Time Point 6)            |
|FJPL056            |DL1: YP has ever felt that they were being followed or spied on [F17]           |(PLE Time Point 6)            |
|FJPL059            |DL4: Frequency YP has had thoughts of being followed/spied on, in last 6 months |(PLE Time Point 6)            |
|YPA2130            |b14: YP has heard voices that other people couldn't hear                        |(PLE Time Point 7)            |
|YPA2131            |b14a: Frequency YP has heard voices that other people couldn't hear since their |(PLE Time Point 7)            |
|YPA2140            |b15: YP has seen something or someone that other people could not see           |(PLE Time Point 7)            |
|YPA2141            |b15a: Frequency YP has seen something or someone that other people could not see|(PLE Time Point 7)            |
|YPA2150            |b16: YP has thought they were being followed or spied on                        |(PLE Time Point 7)            |
|YPA2151            |b16a: Frequency YP has thought they were being followed or spied on since their |(PLE Time Point 7)            |
|FKPL1300           |AH1: Since 12yrs, YP heard voices that other people could not hear: F@24        |(PLE Time Point 8)            |
|FKPL1303           |AH4: Frequency YP has heard the voice, in last 6mth: F@24                       |(PLE Time Point 8)            |
|FKPL1600           |VH1: Since 12yrs, YP saw something/someone other people could not see: F@24     |(PLE Time Point 8)            |
|FKPL1603           |VH4: Frequency YP has seen visions, in last 6mth: F@24                          |(PLE Time Point 8)            |
|FKPL1700           |DL1: Since 12yrs, YP felt that they were being followed or spied on: F@24       |(PLE Time Point 8)            |
|FKPL1703           |DL4: Frequency YP has had thoughts of being spied on, in last 6mn: F@24         |(PLE Time Point 8)            |
|YPF7010            |h1: YP has ever heard voices that other people couldn't hear                    |(PLE Time Point 9)            |
|YPF7030            |h1b: Frequency YP heard voices that others couldn't hear in the past year       |(PLE Time Point 9)            |
|YPF7040            |h2: YP has ever seen something or someone that other people couldn't see        |(PLE Time Point 9)            |
|YPF7060            |h2b: Frequency YP saw something/someone that others couldn't see in past year   |(PLE Time Point 9)            |
|YPF7070            |h3: YP thinks they have ever been followed/spied on                             |(PLE Time Point 9)            |
|YPF7090            |h3b: Frequency YP thought were followed/spied on in past year                   |(PLE Time Point 9)            |
|kq348c             |DV: SDQ emotional symptoms score (prorated)                                     |SDQ_emotional_7               |
|ku707b             |DV: SDQ - Emotional symptoms score (prorated)                                   |SDQ_emotional_9               |
|kw6602b            |DV: SDQ - Emotional symptoms score (prorated)                                   |SDQ_emotional_11              |
|ta7025a            |DV: SDQ emotional symptoms score (prorated)                                     |SDQ_emotional_13              |
|tb8618             |DV: 13yr Yes-no anxiety disorder (parent computer prediction, ICD-10 and DSM-IV)|DV_anxiety_dis_13             |
|tb8619             |DV: 13yr Yes-no depressive disorder (parent computer prediction, ICD-10 and DSM-|DV_depressive_dis_13          |
|kr827a             |DV: DAWBA DSM-IV clinical diagnosis - Any anxiety disorder                      |DV_any_anxiety_7              |
|kr830              |DV: DAWBA DSM-IV clinical diagnosis - Major depressive disorder                 |DV_MDD_7                      |
|kv8617             |DV: 10yr Yes-no anxiety disorder (parent computer prediction, ICD-10 and DSM-IV)|DV_anxiety_dis_10             |
|YPB1233            |A22x: Ever been diagnosed with depression                                       |ever_depression_diagnosis_22  |
|YPC0600            |DV: WEMWBS Composite                                                            |WEMWBS_23                     |
|FJCI1001           |DV:Casediag: Participant has ICD-10 diagnosis of depression [F17]               |dep_bin_17                    |
|FKDQ1000           |DV: Mild depressive episode: F@24                                               |mild_dep_dis_24               |
|jan2014imd2010q5_YP|IMD Score 2010, quintiles; at timepoint Jan 2014 (YP)                           |IMD_score                     |
|ff2030             |M13: Weight (Kgs): measuring: TF1                                               |weight_kg_12                  |
|ff2000             |M5: Height (cms): measuring: TF1                                                |height_cm_12                  |
|e391               |EPDS                                                                            |maternal_EDPS                 |
|pe290              |EPDS Score I                                                                    |paternal_EDPS                 |
