# Household Survey data analysis
      # Harmonic Biosphere Company Limited 
      # June, 2020
      
      # Set working directory --------------------------------------------------------
      setwd("~")
      dir.create("RAP", showWarnings = FALSE)
      setwd("~/RAP")
      
      # Load/install required packageslibralies ---------------------------------------
      library(tidyverse)
      library(scales)
      library(ggrepel)
      
      
      options(scipen = 999)
      
      
      df<-read.csv("Household_Survey.csv",check.names = TRUE,stringsAsFactors = FALSE)
      
      
      # Renaming varibales
      df<-df %>% 
        rename(Respondent_Name = Respondent_.Name,
               HoH_N = HoH_.N, HM1_N = HM1_.N, HM7_G = HM6_G.1, HM8_G = HM_G)
      
      
      # POPULATION CHARACTERISTICS -------------------------------------------------
      
      # 1.Percentage of population ratio for PAPS in each of the affected village
      
      df_pop<-df %>%
        select(Village,HoH_N) %>% 
        group_by(Village) %>% 
        summarise(freq = n()) %>% 
        mutate(Percentage_ratio = freq/sum(freq)*100,
               Percentage_ratio = str_c(round(Percentage_ratio,2),"%")) %>% 
        view()
      
      # Population 
      df_pop %>% 
        ggplot(aes(x = "", y = Percentage_ratio, fill = Village))+
        geom_bar(stat = "identity")+
        coord_polar("y", start = 0)+
        geom_text(aes(label = str_c(Percentage_ratio, "%")), position = position_stack(vjust = 0.5))+
        labs(title = "Percentage of Population Ratio in Each Affected  Village", caption = "HBCL RAP 2020")+
        theme_void()+
        theme(plot.title = element_text(size = 15, face = "bold", hjust = 0.5, vjust = -6))+
        theme(plot.caption = element_text(size = 5, hjust = 0.5, vjust = 25))+
        theme(legend.title = element_text(face = "bold"))
      
       # 2.	Gender ratio among the surveyed households -----------------------
            # {in each of the affected villages }
      dfHoH <- df %>% 
        select(Village, Respondent_Gender,contains(c("HM1_G", "HM2_G", "HM3_G", "HM4_G", "HM5_G","HM6_G", "HM7_G", "HM8_G" ))) %>%
        #rename(HM7_G = HM6_G.1, HM8_G = HM_G) %>% 
        mutate_all(na_if,"-") %>% 
        mutate_all(na_if,"") %>% 
        mutate(HM2_G = ifelse(HM2_G=="Male ", "Male",HM2_G))
        # view()
      
      # Household Member - (Combine dataframe)
      dfHoH_gender <- dfHoH %>% 
        pivot_longer(cols = c(2:10), names_to = "HouseMember", values_to = "Gender") %>% 
        na.omit(dfHoH_gender) %>% 
        group_by(Village, Gender) %>% 
        summarise(GendeCount=n()) %>% 
        pivot_wider(names_from = "Gender", values_from = "GendeCount") %>% 
        ungroup() %>% 
        mutate(Total = rowSums(.[2:3])) %>% 
        mutate(FemaleRatio=round(Female/Total*100, digits = 2),
               MaleRatio=round(Male/Total*100, digits = 2)) %>% 
        rownames_to_column("ID") %>% 
        pivot_longer(cols = 6:7, names_to = "Gender", values_to = "Percentage") %>% 
        mutate(Gender = ifelse(Gender == "FemaleRatio", "Female", "Male")) %>% 
        view()
      # Kisewe Pie Chart Gender Ratio ------------------
      dfHoH_gender %>% 
        filter(Village == "Kisewe") %>% 
        ggplot(aes(x = "", y= Percentage , fill = Gender))+
        geom_bar(stat = "identity")+
        coord_polar("y", start = 0)+
        geom_text(aes(label = str_c(Percentage, "%")), position = position_stack(vjust = 0.5))+
        labs(title = "Household Gender Ration - Kisewe Village", caption = "HBCL RAP 2020")+
        theme_void()+
        theme(plot.title = element_text(size = 15, hjust = 0.5, face = "bold", vjust = -5))+
        theme(plot.caption = element_text(size = 5, hjust = 0.5, vjust = 25))+
        theme(legend.title = element_text(face = "bold"))
      
      # Makanga Pie Chart Gender Ratio ------------------
      dfHoH_gender %>% 
        filter(Village == "Makanga") %>% 
        ggplot(aes(x = "", y= Percentage , fill = Gender))+
        geom_bar(stat = "identity")+
        coord_polar("y", start = 0)+
        geom_text(aes(label = str_c(Percentage, "%")), position = position_stack(vjust = 0.5))+
        labs(title = "Household Gender Ration - Makanga Village", caption = "HBCL RAP 2020")+
        theme_void()+
        theme(plot.title = element_text(size = 15, hjust = 0.5, face = "bold", vjust = -5))+
        theme(plot.caption = element_text(size = 5, hjust = 0.5, vjust = 25))+
        theme(legend.title = element_text(face = "bold"))
      
      # Mdindo Pie Chart Gender Ratio ------------------
      dfHoH_gender %>% 
        filter(Village == "Mdindo") %>% 
        ggplot(aes(x = "", y= Percentage , fill = Gender))+
        geom_bar(stat = "identity")+
        coord_polar("y", start = 0)+
        geom_text(aes(label = str_c(Percentage, "%")), position = position_stack(vjust = 0.5))+
        labs(title = "Household Gender Ration - Mdindo Village", caption = "HBCL RAP 2020")+
        theme_void()+
        theme(plot.title = element_text(size = 15, hjust = 0.5, face = "bold", vjust = -5))+
        theme(plot.caption = element_text(size = 5, hjust = 0.5, vjust = 25))+
        theme(legend.title = element_text(face = "bold"))
      
      # NawengePie Chart Gender Ratio ------------------
      dfHoH_gender %>% 
        filter(Village == "Nawenge") %>% 
        ggplot(aes(x = "", y= Percentage , fill = Gender))+
        geom_bar(stat = "identity")+
        coord_polar("y", start = 0)+
        geom_text(aes(label = str_c(Percentage, "%")), position = position_stack(vjust = 0.5))+
        labs(title = "Household Gender Ration - Nawenge Village", caption = "HBCL RAP 2020")+
        theme_void()+
        theme(plot.title = element_text(size = 15, hjust = 0.5, face = "bold", vjust = -5))+
        theme(plot.caption = element_text(size = 5, hjust = 0.5, vjust = 25))+
        theme(legend.title = element_text(face = "bold"))
      
      
    
      # 3.Average size of the household for all of the affected household in each of the affected village  ----------------------- 
      df_AHS <- df %>% 
        select(Village) %>% 
        group_by(Village) %>% 
        summarise(VillageCount = n()) %>% 
        rownames_to_column("ID") %>% 
        view()
        
      # Join two dataframe dfHoH_gender & df_AHS
      dfcombine <- df_AHS %>% 
        left_join(dfHoH_gender, by= c("ID", "Village")) %>% 
        mutate(HouseholdSize=round(Total/VillageCount, digits = 1))%>% 
        view()
      write.csv(dfcombine, "RAPAnalysis.csv")
      
      # 4.Categorize age group for PAPS in to 8 groups from each of the affected population 
      # (0 â 5, 5 â 18, 19 â 24, 25 â 34, 35 â 44, 45 â 54, 55- 64, 65 & above) -------------------------------
      
      Age_group <- c("HM1_A", "HM2_A", "HM3_A", "HM4_A", "HM5_A","HM6_A", "HM7_A", "HM8_A")
      
      df_AGC <- df %>% 
        select(Village,Respondent_Age, HM1_A, HM2_A,HM3_A,  HM4_A, HM5_A, HM6_A,HM7_A, HM8_A) %>% 
        mutate_at(Age_group, as.integer) %>% 
        mutate_all(na_if,"-") %>% 
        mutate_all(na_if,"") %>% 
        pivot_longer(cols = c(2:10), names_to = "Members_Age", values_to = "Age") %>% 
        mutate(AgeGroup = case_when(Age%in%c(0:4)~"0-4",
                                    Age%in%c(5:9)~"5-9",
                                    Age%in%c(10:19)~"10-19",
                                    Age%in%c(20:44)~"20-44",
                                    Age%in%c(45:64)~"45-64",
                                    Age >= 65~"65 & Above")) %>%
        filter(!is.na(Age)) %>% 
        select(-Age,-Members_Age) %>% 
        group_by(Village, AgeGroup) %>% 
        summarise(AgeGroupCount = n()) %>% 
        view()
      
      write.csv(df_AGC, "Age Structure.csv")
     
      # Bar Plot - Age Strucuture ---------------------------------------------------------------------------
      ggplot(data = df_AGC, aes(x = Village, y = AgeGroupCount, fill = AgeGroup))+
        geom_col(position = position_dodge2(preserve = "single",width = 0.9))+
        scale_y_continuous(name = "PAPS Population", limits = c(0,550), seq(0,550,25))+
        labs(title = "Age Structure of Affected Population", fill = "Age Groups", caption = "HBCL RAP 2020")+
        scale_fill_discrete(breaks = c("0-4","5-9", "10-19", "20-44", "45-64", "65 & Above"))+
        theme_light()+
        theme(plot.title = element_text(hjust = 0.5,face = "bold"))+
        theme(plot.caption = element_text(size = 5))+
        theme(axis.title.x = element_text(face = "bold"))+
        theme(axis.title.y = element_text(face = "bold"))+
        theme(legend.title = element_text(face = "bold"))
      
       # Respondent Characteristics ----------------------------
      Respondend <- df %>% 
        select(Village, HofH) %>% 
        group_by(Village) %>% 
        summarise(Total_Household = n()) %>% 
        rownames_to_column("ID") %>% 
        view()
      # a.The surveyed households headed by men in % -----------------------------
      Male_HoH <- df %>% 
        select(Village, HofH, Respondent_Gender) %>% 
        filter(HofH=="Yes" & Respondent_Gender=="Male") %>% 
        group_by(Village, Respondent_Gender) %>% 
        summarise(Male_Resp = n()) %>% 
        mutate(Total_Household = Respondend$Total_Household[match(Village,Respondend$Village)], 
               HofH_Male = round(Male_Resp/Total_Household*100, digits = 1)) %>%
        view()
      
      # b.The surveyed households headed by Female in % -----------------------------
      Female_HoH <- df %>% 
        select(Village, HofH, Respondent_Gender) %>% 
        filter(HofH=="Yes" & Respondent_Gender=="Female") %>% 
        group_by(Village, Respondent_Gender) %>% 
        summarise(Female_Resp = n()) %>% 
        mutate(Total_Household = Respondend$Total_Household[match(Village,Respondend$Village)], 
               HofH_Female = round(Female_Resp/Total_Household*100, digits = 1))%>% 
        view()
      
      # c.The surveyed households headed by None in % -----------------------------
      None_HoH <- df %>% 
        select(Village, HofH) %>% 
        filter(HofH=="No") %>% 
        group_by(Village) %>% 
        summarise(None_Resp = n()) %>% 
        mutate(Total_Household = Respondend$Total_Household[match(Village, Respondend$Village)],
               None_HoH = round(None_Resp/Total_Household*100, digits = 1)) %>% 
        view()
      # Join two dataframe Male_HoH & Female_HoH -----------
      HofH_df <- Male_HoH %>% 
        left_join(Female_HoH, by = c("Village", "Total_Household")) %>% 
        left_join(None_HoH, by = c("Village", "Total_Household")) %>% 
        select(-Respondent_Gender.x, -Respondent_Gender.y) %>% 
        select(Village, Male_Resp, Female_Resp, None_Resp, Total_Household, HofH_Male, HofH_Female, None_HoH) %>% 
        pivot_longer(cols = c(6:8), names_to = "HeadHousehold_Perc", values_to = "Values") %>%
        mutate(HeadHousehold_Perc = ifelse(HeadHousehold_Perc== "HofH_Female", "Male", HeadHousehold_Perc),
               HeadHousehold_Perc = ifelse(HeadHousehold_Perc== "HofH_Male", "Female", HeadHousehold_Perc),
               HeadHousehold_Perc = ifelse(HeadHousehold_Perc== "None_HoH", "Neither", HeadHousehold_Perc)) %>% 
        filter(!is.na(Values)) %>% 
        mutate(HeadHousehold_Perc = fct_reorder(HeadHousehold_Perc,Values)) %>% 
        view()
      write.csv(HofH_df, "Household_Head.csv")
      
      # Bar Chart -----------
      ggplot(data = HofH_df, aes(x = Village, y = Values, fill =  HeadHousehold_Perc))+
        geom_col(position = position_dodge2(preserve = "single", width = 0.9))+
        #geom_text(aes(label = str_c(Values, "%")), position = position_dodge2(preserve = "single", width = 0.9), vjust = 2, size = 4)+
        scale_y_continuous(name = "Percentage (%)", breaks = breaks_width(5))+
        labs(title = "Head of Household", fill ="Gender", caption = "HBCL RAP 2020")+
        theme_light()+
        theme(plot.title = element_text(hjust = 0.5,face = "bold"))+
        theme(plot.caption = element_text(size = 5))+
        theme(axis.title.x = element_text(face = "bold"))+
        theme(axis.title.y = element_text(face = "bold"))+
        theme(legend.title = element_text(face = "bold"))
      
      # LAND TENURE ----------------------------
      LandTenure <- df%>% 
        select(Village, Land.Tenure)%>% 
        mutate(Land.Tenure = ifelse(Land.Tenure == "Customery/Certification Right of Occupancy", "Customary_law", Land.Tenure),
               Land.Tenure = ifelse(Land.Tenure == "1. Leasehold/Certificate of ownership", "Customary_law", Land.Tenure),
               Land.Tenure = ifelse(Land.Tenure == "Customary law", "Customary_law", Land.Tenure),
               Land.Tenure = ifelse(Land.Tenure == "Share cropping", "Share_crop", Land.Tenure),
               Land.Tenure = ifelse(Land.Tenure == "Other form of tenure", "Other_tenure", Land.Tenure)) %>% 
        group_by(Village, Land.Tenure)%>% 
        summarise(Tenure = n())%>% 
        ungroup() %>% 
        pivot_wider(names_from = "Land.Tenure", values_from = "Tenure")%>% 
        replace(is.na(.),0)%>%
        mutate(Total = rowSums(.[2:5]))%>% 
        mutate(Borrow_Perc = round(Borrowed/Total*100, digits = 2),
               Custom_law_Perc= round(Customary_law/Total*100, digits = 2),
               Share_crop_Perc = round(Share_crop/Total*100, digits = 2),
               Other_tenure_Perc = round(Other_tenure/Total*100, digits = 2)) %>% 
        pivot_longer(cols = 7:10, names_to = "LandTenure", values_to = "Percentages") %>% 
        mutate(LandTenure = ifelse(LandTenure == "Borrow_Perc", "Borrowed", LandTenure),
               LandTenure = ifelse(LandTenure == "Custom_law_Perc", "Customer Law", LandTenure),
               LandTenure = ifelse(LandTenure == "Share_crop_Perc", "Shared Crops", LandTenure),
               LandTenure = ifelse(LandTenure == "Other_tenure_Perc", "Other Land Tenure", LandTenure))
        
      # Kisewe Land Tenure Pie Chart
        LandTenure %>% 
        filter(Village == "Kisewe") %>% 
        ggplot(aes(x = "", y = Percentages, fill = LandTenure))+
        geom_bar(stat = "Identity")+
        coord_polar("y", start = 0)+
        geom_text(aes(label = ifelse(Percentages > 3, str_c(Percentages, "%"), "")), position = position_stack(vjust = 0.5), hjust = 0.6, vjust = -6)+
          geom_label_repel(aes(label = ifelse(Percentages < 3, str_c(Percentages, "%"), "")), size = 4, nudge_x = 0.7, show.legend = F)+
          labs(title = "Land Tenure Type - Kisewe Village", caption = "HBCL RAP 2020")+
        theme_void()+
          theme(plot.title = element_text(size = 15, hjust = 0.5, vjust = 1.5, face = "bold"))+
          theme(plot.caption = element_text(size = 5, hjust = 0.5, vjust = 25))+
          theme(legend.title = element_text(face = "bold"))
        
        # Makanga Land Tenure Pie Chart
        LandTenure %>% 
          filter(Village == "Makanga") %>%
          filter(Percentages > 0) %>% 
          ggplot(aes(x = "", y = Percentages, fill = LandTenure))+
          geom_bar(stat = "Identity")+
          coord_polar("y", start = 0)+
          geom_text(aes(label =  str_c(Percentages, "%")), position = position_stack(vjust = 0.5), hjust = 0.5, vjust = -10)+
          labs(title = "Land Tenure Type - Makanga Village", caption = "HBCL RAP 2020")+
          theme_void()+
          theme(plot.title = element_text(size = 15, hjust = 0.5, vjust = 0.5, face = "bold"))+
          theme(plot.caption = element_text(size = 5, hjust = 0.5, vjust = 25))+
          theme(legend.title = element_text(face = "bold"))
        
        # Mdindo Land Tenure Pie Chart
        LandTenure %>% 
          filter(Village == "Mdindo") %>%
          filter(Percentages > 0) %>% 
          mutate(LandTenure = fct_reorder(LandTenure, -Percentages)) %>% 
          ggplot(aes(x = "", y = Percentages, fill = LandTenure))+
          geom_bar(stat = "Identity")+
          coord_polar("y", start = 0)+
          geom_text(aes(label = ifelse(Percentages > 90, str_c(Percentages, "%"), "")), position = position_stack(vjust = 0.5), hjust = 0.6, vjust = -6)+
          geom_label_repel(aes(label = ifelse(Percentages < 90, str_c(Percentages, "%"), "")), size = 4, nudge_x = 0.7, show.legend = F)+
          labs(title = "Land Tenure Type - Mdindo Village", caption = "HBCL RAP 2020")+
          theme_void()+
          theme(plot.title = element_text(size = 15, hjust = 0.5, vjust = 2.5, face = "bold"))+
          theme(plot.caption = element_text(size = 5, hjust = 0.5, vjust = 25))+
          theme(legend.title = element_text(face = "bold"))
        
        # Nawenge Land Tenure Pie Chart
        LandTenure %>% 
          filter(Village == "Nawenge") %>%
          filter(Percentages > 0) %>% 
          #mutate(LandTenure = fct_reorder(LandTenure, -Percentages)) %>% 
          ggplot(aes(x = "", y = Percentages, fill = LandTenure))+
          geom_bar(stat = "Identity")+
          coord_polar("y", start = 0)+
          geom_text(aes(label = str_c(Percentages, "%")), position = position_stack(vjust = 0.5), hjust = 0.5, vjust = -10)+
          #geom_label_repel(aes(label = ifelse(Percentages < 90, str_c(Percentages, "%"), "")), size = 4, nudge_x = 0.7, show.legend = F)+
          labs(title = "Land Tenure Type - Nawenge Village", caption = "HBCL RAP 2020")+
          theme_void()+
          theme(plot.title = element_text(size = 15, hjust = 0.5, vjust = -2.5, face = "bold"))+
          theme(plot.caption = element_text(size = 5, hjust = 0.5, vjust = 25))+
          theme(legend.title = element_text(face = "bold"))
        
     
      # AGRICULTURE ----------------------------------
      # a)	percentage of the PAPS who practice agriculture in each of affected village by the project
      df_Agric <- df %>% 
        select(Village, Activities) %>%
        mutate(Activities, ifelse(Activities == "Actvities", "Agriculuture", Activities)) %>% 
        group_by(Village, Activities) %>% 
        summarise(Agriculture = n()) %>% 
        ungroup()%>% 
        pivot_wider(names_from = "Activities", values_from = "Agriculture")%>% 
        replace(is.na(.),0)%>%
        mutate(Total = rowSums(.[2:3]))%>%
        mutate(No_Perc = round(No/Total*100, digits = 2),
              Yes_Perc = round(Yes/Total*100, digits = 2))%>% 
          pivot_longer(cols = 5:6, names_to = "Agriculture", values_to = "Percentages") %>% 
          mutate(Agriculture = ifelse(Agriculture == "No_Perc", "No Agriculture", "Agriculture"))
        view()
        
        # Kisewe Agriculture Pie Chart ---------------------
        df_Agric %>% 
          filter(Village == "Kisewe") %>% 
          ggplot(aes(x = "", y = Percentages, fill = Agriculture))+
          geom_bar(stat = "identity")+
          coord_polar("y", start = 0)+
          geom_text(aes(label = ifelse(Percentages > 1, str_c(Percentages, "%"), "")), position = position_stack(vjust = 0.5))+
          geom_label_repel(aes(label = ifelse(Percentages < 1, str_c(Percentages, "%"), "")), size = 4,nudge_x = 0.5, hjust= -1,  show.legend = F)+
          labs(title = "Households Practicing Agriculture - Kisewe Village", caption = "HBCL RAP 2020")+
          theme_void()+
          theme(plot.title = element_text(size = 15, vjust = 1, hjust = 0.5, face = "bold"))+
          theme(plot.caption = element_text(size = 5, vjust = 25, hjust = 0.5))+
          theme(legend.title = element_text(face = "bold"))
        
        # Makanga Agriculture Pie Chart ---------------------
        df_Agric %>% 
          filter(Village == "Makanga") %>% 
          filter(Percentages > 0) %>% 
          ggplot(aes(x = "", y = Percentages, fill = Agriculture))+
          geom_bar(stat = "identity")+
          coord_polar("y", start = 0)+
          geom_text(aes(label = str_c(Percentages, "%")), position = position_stack(vjust = 0.5), vjust = -10)+
          labs(title = "Households Practicing Agriculture - Makanga Village", caption = "HBCL RAP 2020")+
          theme_void()+
          theme(plot.title = element_text(size = 15, vjust = -2.5, hjust = 0.5, face = "bold"))+
          theme(plot.caption = element_text(size = 5, vjust = 25, hjust = 0.5))+
          theme(legend.title = element_text(face = "bold"))
      
        # Mdindo Agriculture Pie Chart ---------------------
        df_Agric %>% 
          filter(Village == "Mdindo") %>% 
          ggplot(aes(x = "", y = Percentages, fill = Agriculture))+
          geom_bar(stat = "identity")+
          coord_polar("y", start = 0)+
          geom_text(aes(label =  str_c(Percentages, "%")), position = position_stack(vjust = 0.5), vjust = -6, hjust = 0.2)+
          labs(title = "Households Practicing Agriculture - Mdindo Village", caption = "HBCL RAP 2020")+
          theme_void()+
          theme(plot.title = element_text(size = 15, vjust = -5.5, hjust = 0.5, face = "bold"))+
          theme(plot.caption = element_text(size = 5, vjust = 25, hjust = 0.5))+
          theme(legend.title = element_text(face = "bold"))
        
        # Nawenge Agriculture Pie Chart ---------------------
          df_Agric %>% 
          filter(Village == "Nawenge") %>% 
          filter(Percentages > 0) %>% 
          ggplot(aes(x = "", y = Percentages, fill = Agriculture))+
          geom_bar(stat = "identity")+
          coord_polar("y", start = 0)+
          geom_text(aes(label = str_c(Percentages, "%")), position = position_stack(vjust = 0.5), vjust = -10)+
          labs(title = "Households Practicing Agriculture - Nawenge Village", caption = "HBCL RAP 2020")+
          theme_void()+
          theme(plot.title = element_text(size = 15, vjust = -2.5, hjust = 0.5, face = "bold"))+
          theme(plot.caption = element_text(size = 5, vjust = 25, hjust = 0.5))+
          theme(legend.title = element_text(face = "bold"))
        
        
        # MINING
      # b)	Percentage of the PAPS informal gold mining each of affected village by the project
      df_Mine <- df %>% 
        select(Village, Mining) %>%
        mutate(Mining = ifelse(Mining == "-", "No", Mining)) %>% 
        group_by(Village, Mining) %>% 
        summarise(Mine_Activ = n()) %>%
        ungroup()%>% 
        pivot_wider(names_from = "Mining", values_from = "Mine_Activ")%>%
        replace(is.na(.),0)%>%
        mutate(Total = rowSums(.[2:3]))%>%
        mutate(No_Perc = round(No/Total*100, digits = 2),
               Yes_Perc = round(Yes/Total*100, digits = 2))%>% 
        pivot_longer(cols = 5:6, names_to = "Mining", values_to = "Percentages") %>% 
        mutate(Mining = ifelse(Mining == "No_Perc", "No Mining", "Mining")) %>% 
        view()
      
      # Kisewe Mining Pie Chat
      df_Mine %>% 
        filter(Village == "Kisewe") %>% 
        ggplot(aes(x = "", y = Percentages, fill = Mining))+
        geom_bar(stat = "identity")+
        coord_polar("y", start = 0)+
        geom_text(aes(label =  str_c(Percentages, "%")), position = position_stack(vjust = 0.5), vjust = 2.5, hjust = 1)+
        labs(title = "Households Practicing Mining - Kisewe Village", caption = "HBCL RAP 2020")+
        theme_void()+
        theme(plot.title = element_text(size = 15, vjust = -5.5, hjust = 0.5, face = "bold"))+
        theme(plot.caption = element_text(size = 5, vjust = 25, hjust = 0.5))+
        theme(legend.title = element_text(face = "bold"))
      
      # Makanga Mining Pie Chat
      df_Mine %>% 
        filter(Village == "Makanga") %>% 
        ggplot(aes(x = "", y = Percentages, fill = Mining))+
        geom_bar(stat = "identity")+
        coord_polar("y", start = 0)+
        geom_text(aes(label =  str_c(Percentages, "%")), position = position_stack(vjust = 0.5), vjust = -2.5, hjust = 0.5)+
        labs(title = "Households Practicing Mining - Makanga Village", caption = "HBCL RAP 2020")+
        theme_void()+
        theme(plot.title = element_text(size = 15, vjust = -5.5, hjust = 0.5, face = "bold"))+
        theme(plot.caption = element_text(size = 5, vjust = 25, hjust = 0.5))+
        theme(legend.title = element_text(face = "bold"))
      
      # Mdindo Mining Pie Chat
      df_Mine %>% 
        filter(Village == "Mdindo") %>% 
        ggplot(aes(x = "", y = Percentages, fill = Mining))+
        geom_bar(stat = "identity")+
        coord_polar("y", start = 0)+
        geom_text(aes(label =  str_c(Percentages, "%")), position = position_stack(vjust = 0.5), vjust = -2.5, hjust = 0.5)+
        labs(title = "Households Practicing Mining - Mdindo Village", caption = "HBCL RAP 2020")+
        theme_void()+
        theme(plot.title = element_text(size = 15, vjust = -5.5, hjust = 0.5, face = "bold"))+
        theme(plot.caption = element_text(size = 5, vjust = 25, hjust = 0.5))+
        theme(legend.title = element_text(face = "bold"))
      
      # Nawenge Mining Pie Chat
      df_Mine %>% 
        filter(Village == "Nawenge") %>%
        filter(Percentages > 0) %>% 
        ggplot(aes(x = "", y = Percentages, fill = Mining))+
        geom_bar(stat = "identity")+
        coord_polar("y", start = 0)+
        geom_text(aes(label =  str_c(Percentages, "%")), position = position_stack(vjust = 0.5), vjust = -10, hjust = 0.5)+
        labs(title = "Households Practicing Mining - Nawenge Village", caption = "HBCL RAP 2020")+
        theme_void()+
        theme(plot.title = element_text(size = 15, vjust = -5.5, hjust = 0.5, face = "bold"))+
        theme(plot.caption = element_text(size = 5, vjust = 25, hjust = 0.5))+
        theme(legend.title = element_text(face = "bold"))
      
      
      # c) Percentage of other economic activities [small business activities etc. in each affected village by the project
      df_Business <- df %>% 
        select(Village, Business, Other_sources) %>%
        mutate(Business = ifelse(Business == "-", NA, Business),
               Other_sources = ifelse(Other_sources == "-", NA,Other_sources)) %>%
        filter(!is.na(Business)) %>% 
        view()
      
      # Business
      Busines <- df_Business %>% 
      group_by(Village, Business) %>% 
        summarise(Busines_Activ = n()) %>%
        ungroup()%>% 
        pivot_wider(names_from = "Business", values_from = "Busines_Activ")%>%
        replace(is.na(.),0)%>%
        mutate(Total = rowSums(.[2:3]))%>%
        mutate(No_Perc = round(No/Total*100, digits = 2),
               Yes_Perc = round(Yes/Total*100, digits = 2))%>% 
        pivot_longer(cols = 5:6, names_to = "Business", values_to = "Percentages") %>% 
        mutate(Business = ifelse(Business == "Yes_Perc", "Yes", "No")) %>% 
        view()
      # Kisewe pie chart plot
      Busines %>% 
        filter(Village == "Kisewe") %>% 
        ggplot(aes(x = "", y = Percentages, fill = Business))+
        geom_bar(stat = "identity")+
        geom_text(aes(label = ifelse(Percentages > 90, str_c(Percentages, "%"), "")), position = position_stack(vjust = 0.5), vjust = -6, size = 5)+
        geom_text(aes(label = ifelse(Percentages < 90, str_c(Percentages, "%"), "")), position = position_stack(vjust = 0.5), angle = 90, hjust = -0.5, size = 5)+
        coord_polar("y", start = 0)+
        labs(title = "Household Involved in Business - Kisewe Village", caption = "HBCL RAP 2020")+
        theme_void()+
        theme(plot.title = element_text(hjust = 0.5, size = 15, face = "bold", vjust = -8))+
        theme(plot.caption = element_text(hjust = 0.5, size = 5, vjust = 25))+
        theme(legend.title = element_text(face = "bold", size = 10))
      
      # Makanga pie chart plot
      Busines %>% 
        filter(Village == "Makanga") %>% 
        ggplot(aes(x = "", y = Percentages, fill = Business))+
        geom_bar(stat = "identity")+
        geom_text(aes(label =  str_c(Percentages, "%")), position = position_stack(vjust = 0.5), vjust = -2, size = 5)+
        #geom_text(aes(label = ifelse(Percentages < 90, str_c(Percentages, "%"), "")), position = position_stack(vjust = 0.5), angle = 90, hjust = -0.5, size = 5)+
        coord_polar("y", start = 0)+
        labs(title = "Household Involved in Business - Makanga Village", caption = "HBCL RAP 2020")+
        theme_void()+
        theme(plot.title = element_text(hjust = 0.5, size = 15, face = "bold", vjust = -8))+
        theme(plot.caption = element_text(hjust = 0.5, size = 5, vjust = 25))+
        theme(legend.title = element_text(face = "bold", size = 10))
      
      # Mdindo pie chart plot
      Busines %>% 
        filter(Village == "Mdindo") %>% 
        ggplot(aes(x = "", y = Percentages, fill = Business))+
        geom_bar(stat = "identity")+
        geom_text(aes(label =  str_c(Percentages, "%")), position = position_stack(vjust = 0.5), vjust = -2, size = 5)+
        #geom_text(aes(label = ifelse(Percentages < 90, str_c(Percentages, "%"), "")), position = position_stack(vjust = 0.5), angle = 90, hjust = -0.5, size = 5)+
        coord_polar("y", start = 0)+
        labs(title = "Household Involved in Business - Mdindo Village", caption = "HBCL RAP 2020")+
        theme_void()+
        theme(plot.title = element_text(hjust = 0.5, size = 15, face = "bold", vjust = -8))+
        theme(plot.caption = element_text(hjust = 0.5, size = 5, vjust = 25))+
        theme(legend.title = element_text(face = "bold", size = 10))
      
      # Nawenge pie chart plot
      Busines %>% 
        filter(Village == "Nawenge") %>% 
        filter(Percentages > 0) %>% 
        ggplot(aes(x = "", y = Percentages, fill = Business))+
        geom_bar(stat = "identity")+
        geom_text(aes(label =  str_c(Percentages, "%")), position = position_stack(vjust = 0.5), vjust = -8, size = 5)+
        #geom_text(aes(label = ifelse(Percentages < 90, str_c(Percentages, "%"), "")), position = position_stack(vjust = 0.5), angle = 90, hjust = -0.5, size = 5)+
        coord_polar("y", start = 0)+
        labs(title = "Household Involved in Business - Nawenge Village", caption = "HBCL RAP 2020")+
        theme_void()+
        theme(plot.title = element_text(hjust = 0.5, size = 15, face = "bold", vjust = -8))+
        theme(plot.caption = element_text(hjust = 0.5, size = 5, vjust = 25))+
        theme(legend.title = element_text(face = "bold", size = 10))
      
      # other Busines
      other_Busines <- df_Business %>% 
      group_by(Village, Other_sources) %>% 
        summarise(Other_Activ = n()) %>%
        ungroup()%>% 
        pivot_wider(names_from = "Other_sources", values_from = "Other_Activ")%>%
        replace(is.na(.),0)%>%
        mutate(Total = rowSums(.[2:3]))%>%
        mutate(No_Perc = round(No/Total*100, digits = 2),
               Yes_Perc = round(Yes/Total*100, digits = 2))%>% 
        view()
      
       # EMPLOYMENT -------------------------------
      df_employee <- df %>% 
        select(Village, Employed) %>% 
        mutate_all(na_if,"-") %>% 
        mutate_all(na_if,"") %>% 
        mutate(Employed = ifelse(Employed == "Public servant employee", "Employed", Employed)) %>% 
        group_by(Village, Employed) %>% 
        summarise(Employment = n()) %>%
        ungroup()%>% 
        pivot_wider(names_from = "Employed", values_from = "Employment")%>%
        replace(is.na(.),0)%>%
        rename(Unemployed = "NA") %>% 
        mutate(Total = rowSums(.[2:3]))%>%
        mutate(Unemployed_Perc = round(Unemployed/Total*100, digits = 2),
               Employed_Perc = round(Employed/Total*100, digits = 2))%>% 
        pivot_longer(cols = 5:6, names_to = "Employment", values_to = "Percentages") %>% 
        mutate(Employment = ifelse(Employment == "Unemployed_Perc", "Unemployment", "Employment")) 
        
      # Kisewe Employment Status
        df_employee %>% 
        filter(Village == "Kisewe") %>%
          filter(Percentages > 0) %>% 
        ggplot(aes(x = "", y = Percentages, fill = Employment))+
        geom_bar(stat = "Identity")+
        coord_polar("y", start = 0)+
          geom_text(aes(label = str_c(Percentages, "%")), position = position_stack(vjust = 0.5), vjust = -10)+
          labs(title = "Employment Status - Kisewe Village", caption = "HBCL RAP 2020")+
        theme_void()+
          theme(plot.title = element_text(face = "bold", size = 15, hjust = 0.5, vjust = -7.5))+
          theme(legend.title = element_text(face = "bold"))+
          theme(plot.caption = element_text(vjust = 20, hjust = 0.5, size = 5))
        
        # Makanga Employment Status
        df_employee %>% 
          filter(Village == "Makanga") %>%
          filter(Percentages > 0) %>% 
          ggplot(aes(x = "", y = Percentages, fill = Employment))+
          geom_bar(stat = "Identity")+
          coord_polar("y", start = 0)+
          geom_text(aes(label = str_c(Percentages, "%")), position = position_stack(vjust = 0.5), vjust = -10)+
          labs(title = "Employment Status - Makanga Village", caption = "HBCL RAP 2020")+
          theme_void()+
          theme(plot.title = element_text(face = "bold", size = 15, hjust = 0.5, vjust = -7.5))+
          theme(legend.title = element_text(face = "bold"))+
          theme(plot.caption = element_text(vjust = 20, hjust = 0.5, size = 5))
        
        # Mdindo Employment Status
        df_employee %>% 
          filter(Village == "Mdindo") %>%
          filter(Percentages > 0) %>% 
          ggplot(aes(x = "", y = Percentages, fill = Employment))+
          geom_bar(stat = "Identity")+
          coord_polar("y", start = 0)+
          geom_text(aes(label = ifelse(Percentages > 90, str_c(Percentages, "%"), "")), position = position_stack(vjust = 0.5), vjust = -8)+
          geom_text(aes(label = ifelse(Percentages < 90, str_c(Percentages, "%"), "")), position = position_stack(vjust = 0.5), vjust = 0.2, angle = 90, hjust = -1.5)+
          labs(title = "Employment Status - Mdindo Village", caption = "HBCL RAP 2020")+
          theme_void()+
          theme(plot.title = element_text(face = "bold", size = 15, hjust = 0.5, vjust = -7.5))+
          theme(legend.title = element_text(face = "bold"))+
          theme(plot.caption = element_text(vjust = 20, hjust = 0.5, size = 5))
        
        # Nawenge Employment Status
        df_employee %>% 
          filter(Village == "Nawenge") %>%
          filter(Percentages > 0) %>% 
          ggplot(aes(x = "", y = Percentages, fill = Employment))+
          geom_bar(stat = "Identity")+
          coord_polar("y", start = 0)+
          geom_text(aes(label = str_c(Percentages, "%")), position = position_stack(vjust = 0.5), vjust = -10)+
          labs(title = "Employment Status - Nawenge Village", caption = "HBCL RAP 2020")+
          theme_void()+
          theme(plot.title = element_text(face = "bold", size = 15, hjust = 0.5, vjust = -7.5))+
          theme(legend.title = element_text(face = "bold"))+
          theme(plot.caption = element_text(vjust = 20, hjust = 0.5, size = 5))
        
        
        
      # EXPENSES  --------------------
      df_Expense <- df %>% 
        select(Village, Expenses1,Expenses_other1, Expenses_Amount..TZS.1,Expenses2, Expenses_other2,Expenses_Amount..TZS.2,
               Expenses3,Expenses_other3,Expenses_Amount..TZS.3,Expenses4,Expenses_other4,Expenses_Amount..TZS.4) %>% 
        mutate(Expenses1 = ifelse(Expenses1 == " Other" , Expenses_other1, Expenses1),
               Expenses2 = ifelse(Expenses2 == " Other", Expenses_other2, Expenses2),
               Expenses3 = ifelse(Expenses3 == " Other", Expenses_other3, Expenses3),
               Expenses4 = ifelse(Expenses4 == " Other", Expenses_other4, Expenses4)) %>% 
        select(-contains("other")) %>% 
        view()
      
      df_Kisewe <- df_Expense %>% 
        filter(Village =="Kisewe") %>% 
        view()
      
      df_KS1 <- df_Kisewe %>% 
        select(Expenses1, Expenses_Amount..TZS.1) %>% 
        group_by(Expenses1) %>%
        summarise(ExpensSum1 = sum(Expenses_Amount..TZS.1)) %>% 
        arrange(Expenses1) %>% 
        view()
      
      df_KS2 <- df_Kisewe %>% 
        select(Expenses2, Expenses_Amount..TZS.2) %>% 
        group_by(Expenses2) %>%
        summarise(ExpensSum2 = sum(Expenses_Amount..TZS.2)) %>% 
        arrange(Expenses2) %>% 
        view()
      
      df_KS3 <- df_Kisewe %>% 
        select(Expenses3, Expenses_Amount..TZS.3) %>% 
        group_by(Expenses3) %>%
        summarise(ExpensSum3 = sum(Expenses_Amount..TZS.3)) %>% 
        arrange(Expenses3) %>% 
        view()
      
      df_KS4 <- df_Kisewe %>% 
        select(Expenses4, Expenses_Amount..TZS.4) %>% 
        group_by(Expenses4) %>%
        summarise(ExpensSum4 = sum(Expenses_Amount..TZS.4)) %>% 
        arrange(Expenses4) %>% 
        view()
      
      # Kisewe Join dataframe Expenditure
      df_KSCombine <- df_KS1 %>% 
        mutate(Expenses2 = df_KS2$Expenses2[match(Expenses1, df_KS2$Expenses2)],
               Expenses3 = df_KS3$Expenses3[match(Expenses1, df_KS3$Expenses3)],
               Expenses4 = df_KS4$Expenses4[match(Expenses1, df_KS4$Expenses4)],
               ExpensSum2 = df_KS2$ExpensSum2[match(Expenses1, df_KS2$Expenses2)],
               ExpensSum3 = df_KS3$ExpensSum3[match(Expenses1, df_KS3$Expenses3)],
               ExpensSum4 = df_KS4$ExpensSum4[match(Expenses1, df_KS4$Expenses4)]) %>% 
        select(Expenses1, 3:5, ExpensSum1, 6:8) %>% 
        mutate(Expenses2 = ifelse(is.na(Expenses2), Expenses1, Expenses2), 
               Expenses4 = ifelse(is.na(Expenses4),Expenses1, Expenses4)) %>% 
               replace(is.na(.), 0)%>% 
        mutate(Total = rowSums(.[5:8])) %>% 
        mutate(Percentages = round(Total/sum(Total)*100, digits = 2)) %>% 
        view()
      
      # Kisewe Expense
      df_KSCombine %>% 
        mutate(Expenses1 = fct_reorder(Expenses1, -Percentages)) %>% 
      ggplot(aes(x = "", y = Percentages, fill = Expenses1))+
        geom_bar( stat = "identity")+
        coord_polar("y", start = 0)+
        geom_text(aes(label = ifelse(Percentages > 1, str_c(Percentages, "%"), "")), position = position_stack(vjust = 0.5))+
        geom_label_repel(aes(label = ifelse(Percentages < 1, str_c(Percentages, "%"), "")), size = 5, hjust = 0.5, nudge_x = 0.55, show.legend = F)+
        labs(title = "Households Expenditure variation - Kisewe Village", caption = "HBCL RAP 2020", fill = "Expenses")+
        theme_void()+
        theme(plot.title = element_text(hjust = 0.5, face = "bold", size = 15, vjust = 0.5))+
        theme(plot.caption = element_text(hjust = 0.5, vjust = 25, size = 5))+
        theme(legend.title = element_text(face = "bold"))
      
      # Makanga -----------------------------------------------
      df_Makanga <- df_Expense %>% 
        filter(Village =="Makanga") %>% 
        view()
      
      df_MK1 <- df_Makanga %>% 
        select(Expenses1, Expenses_Amount..TZS.1) %>% 
        group_by(Expenses1) %>%
        summarise(ExpensSum1 = sum(Expenses_Amount..TZS.1)) %>% 
        arrange(Expenses1) %>% 
        view()
      
      df_MK2 <- df_Makanga %>% 
        select(Expenses2, Expenses_Amount..TZS.2) %>% 
        group_by(Expenses2) %>%
        summarise(ExpensSum2 = sum(Expenses_Amount..TZS.2)) %>% 
        arrange(Expenses2) %>% 
        view()
      
      df_MK3 <- df_Makanga %>% 
        select(Expenses3, Expenses_Amount..TZS.3) %>% 
        group_by(Expenses3) %>%
        summarise(ExpensSum3 = sum(Expenses_Amount..TZS.3)) %>% 
        arrange(Expenses3) %>% 
        view()
      
      df_MK4 <- df_Makanga %>% 
        select(Expenses4, Expenses_Amount..TZS.4) %>% 
        group_by(Expenses4) %>%
        summarise(ExpensSum4 = sum(Expenses_Amount..TZS.4)) %>% 
        arrange(Expenses4) %>% 
        view()
      
      # Makanga Join dataframe Expenditure
      df_MKCombine <- df_MK2 %>% 
        mutate(Expenses1 = df_MK1$Expenses1[match(Expenses2, df_MK1$Expenses1)],
               Expenses3 = df_MK3$Expenses3[match(Expenses2, df_MK3$Expenses3)],
               Expenses4 = df_MK4$Expenses4[match(Expenses2, df_MK4$Expenses4)],
               ExpensSum1 = df_MK1$ExpensSum1[match(Expenses2, df_MK1$Expenses1)],
               ExpensSum3 = df_MK3$ExpensSum3[match(Expenses2, df_MK3$Expenses3)],
               ExpensSum4 = df_MK4$ExpensSum4[match(Expenses2, df_MK4$Expenses4)]) %>% 
        select(Expenses1, Expenses2, 4:5, ExpensSum1, ExpensSum2, 7:8) %>% 
        mutate(Expenses1 = ifelse(is.na(Expenses1), Expenses2, Expenses1), 
               Expenses4 = ifelse(is.na(Expenses4),Expenses2, Expenses4),
               Expenses3 = ifelse(is.na(Expenses3), Expenses2, Expenses3)) %>% 
        replace(is.na(.), 0)%>% 
        mutate(Total = rowSums(.[5:8])) %>% 
        mutate(Percentages = round(Total/sum(Total)*100, digits = 2)) %>% 
        view()
      
      # Makanga Expense
      df_MKCombine %>% 
        mutate(Expenses1 = fct_reorder(Expenses1, -Percentages)) %>% 
        ggplot(aes(x = "", y = Percentages, fill = Expenses1))+
        geom_bar( stat = "identity")+
        coord_polar("y", start = 0)+
        geom_text(aes(label = ifelse(Percentages > 3, str_c(Percentages, "%"), "")), position = position_stack(vjust = 0.5))+
        geom_text(aes(label = ifelse(Percentages == 1.71, str_c(Percentages, "%"), "")), position = position_stack(vjust = 0.5), angle = 90, hjust = -2, vjust=1)+
        geom_label_repel(aes(label = ifelse(Percentages < 1, str_c(Percentages, "%"), "")), size = 5, hjust = 0.5, nudge_x = 0.55, show.legend = F)+
        labs(title = "Households Expenditure variation - Makanga Village", caption = "HBCL RAP 2020", fill = "Expenses")+
        theme_void()+
        theme(plot.title = element_text(hjust = 0.5, face = "bold", size = 15, vjust = 0.5))+
        theme(plot.caption = element_text(hjust = 0.5, vjust = 25, size = 5))+
        theme(legend.title = element_text(face = "bold"))
      
      # Mdindo -----------------------------------------------
      df_Mdindo <- df_Expense %>% 
        filter(Village =="Mdindo") %>% 
        view()
      
      df_MD1 <- df_Mdindo %>% 
        select(Expenses1, Expenses_Amount..TZS.1) %>% 
        group_by(Expenses1) %>%
        summarise(ExpensSum1 = sum(Expenses_Amount..TZS.1)) %>% 
        arrange(Expenses1) %>% 
        view()
      
      df_MD2 <- df_Mdindo %>% 
        select(Expenses2, Expenses_Amount..TZS.2) %>% 
        group_by(Expenses2) %>%
        summarise(ExpensSum2 = sum(Expenses_Amount..TZS.2)) %>% 
        arrange(Expenses2) %>% 
        view()
      
      df_MD3 <- df_Mdindo %>% 
        select(Expenses3, Expenses_Amount..TZS.3) %>% 
        group_by(Expenses3) %>%
        summarise(ExpensSum3 = sum(Expenses_Amount..TZS.3)) %>% 
        arrange(Expenses3) %>% 
        view()
      
      df_MD4 <- df_Mdindo %>% 
        select(Expenses4, Expenses_Amount..TZS.4) %>% 
        group_by(Expenses4) %>%
        summarise(ExpensSum4 = sum(Expenses_Amount..TZS.4)) %>% 
        arrange(Expenses4) %>% 
        view()
      
      # Mdindo Join dataframe Expenditure
      df_MDCombine <- df_MD1 %>% 
        mutate(Expenses2 = df_MD2$Expenses2[match(Expenses1, df_MD2$Expenses2)],
               Expenses3 = df_MD3$Expenses3[match(Expenses1, df_MD3$Expenses3)],
               Expenses4 = df_MD4$Expenses4[match(Expenses1, df_MD4$Expenses4)],
               ExpensSum2 = df_MD2$ExpensSum2[match(Expenses1, df_MD2$Expenses2)],
               ExpensSum3 = df_MD3$ExpensSum3[match(Expenses1, df_MD3$Expenses3)],
               ExpensSum4 = df_MD4$ExpensSum4[match(Expenses1, df_MD4$Expenses4)]) %>% 
        select(Expenses1, 3:5, ExpensSum1, 6:8) %>% 
        mutate(Expenses2 = ifelse(is.na(Expenses2), Expenses1, Expenses2), 
               Expenses4 = ifelse(is.na(Expenses4), Expenses1, Expenses4),
               Expenses3 = ifelse(is.na(Expenses3), Expenses1, Expenses3)) %>% 
        replace(is.na(.), 0)%>% 
        mutate(Total = rowSums(.[5:8])) %>% 
        mutate(Percentages = round(Total/sum(Total)*100, digits = 2)) %>% 
        view()
      
      # Mdindo Expense
      df_MDCombine %>% 
        mutate(Expenses1 = fct_reorder(Expenses1, -Percentages)) %>% 
        ggplot(aes(x = "", y = Percentages, fill = Expenses1))+
        geom_bar( stat = "identity")+
        coord_polar("y", start = 0)+
        geom_text(aes(label = ifelse(Percentages > 1, str_c(Percentages, "%"), "")), position = position_stack(vjust = 0.5))+
        geom_label_repel(aes(label = ifelse(Percentages < 1, str_c(Percentages, "%"), "")), size = 5, hjust = 0.5, nudge_x = 0.55, show.legend = F)+
        labs(title = "Households Expenditure variation - Mdindo Village", caption = "HBCL RAP 2020", fill = "Expenses")+
        theme_void()+
        theme(plot.title = element_text(hjust = 0.5, face = "bold", size = 15, vjust = 0.5))+
        theme(plot.caption = element_text(hjust = 0.5, vjust = 25, size = 5))+
        theme(legend.title = element_text(face = "bold"))
      
      
      # Nawenge 
      df_Nawenge <- df_Expense %>% 
        filter(Village =="Nawenge") %>% 
        view()
      
      df_NW1 <- df_Nawenge %>% 
        select(Expenses1, Expenses_Amount..TZS.1) %>% 
        group_by(Expenses1) %>%
        summarise(ExpensSum1 = sum(Expenses_Amount..TZS.1)) %>% 
        arrange(Expenses1) %>% 
        view()
      
      df_NW2 <- df_Nawenge %>% 
        select(Expenses2, Expenses_Amount..TZS.2) %>% 
        group_by(Expenses2) %>%
        summarise(ExpensSum2 = sum(Expenses_Amount..TZS.2)) %>% 
        arrange(Expenses2) %>% 
        view()
      
      df_NW3 <- df_Nawenge %>% 
        select(Expenses3, Expenses_Amount..TZS.3) %>% 
        group_by(Expenses3) %>%
        summarise(ExpensSum3 = sum(Expenses_Amount..TZS.3)) %>% 
        arrange(Expenses3) %>% 
        view()
      
      df_NW4 <- df_Nawenge %>% 
        select(Expenses4, Expenses_Amount..TZS.4) %>% 
        group_by(Expenses4) %>%
        summarise(ExpensSum4 = sum(Expenses_Amount..TZS.4)) %>% 
        arrange(Expenses4) %>% 
        view()
      
      # Nawenge Join dataframe Expenditure
      df_NWCombine <- df_NW1 %>% 
        mutate(Expenses2 = df_NW2$Expenses2[match(Expenses1, df_NW2$Expenses2)],
               Expenses3 = df_NW3$Expenses3[match(Expenses1, df_NW3$Expenses3)],
               Expenses4 = df_NW4$Expenses4[match(Expenses1, df_NW4$Expenses4)],
               ExpensSum2 = df_NW2$ExpensSum2[match(Expenses1, df_NW2$Expenses2)],
               ExpensSum3 = df_NW3$ExpensSum3[match(Expenses1, df_NW3$Expenses3)],
               ExpensSum4 = df_NW4$ExpensSum4[match(Expenses1, df_NW4$Expenses4)]) %>% 
        select(Expenses1, 3:5, ExpensSum1, 6:8) %>% 
        mutate(Expenses2 = ifelse(is.na(Expenses2), Expenses1, Expenses2), 
               Expenses4 = ifelse(is.na(Expenses4), Expenses1, Expenses4),
               Expenses3 = ifelse(is.na(Expenses3), Expenses1, Expenses3)) %>% 
        replace(is.na(.), 0)%>% 
        mutate(Total = rowSums(.[5:8])) %>% 
        mutate(Percentages = round(Total/sum(Total)*100, digits = 2)) %>% 
        view()
      
      # Nawenge Expense
      df_NWCombine %>% 
        mutate(Expenses1 = fct_reorder(Expenses1, -Percentages)) %>% 
        ggplot(aes(x = "", y = Percentages, fill = Expenses1))+
        geom_bar( stat = "identity")+
        coord_polar("y", start = 0)+
        geom_text(aes(label = ifelse(Percentages > 1, str_c(Percentages, "%"), "")), position = position_stack(vjust = 0.5))+
        geom_label_repel(aes(label = ifelse(Percentages < 1, str_c(Percentages, "%"), "")), size = 5, hjust = 0.5, nudge_x = 0.55, show.legend = F)+
        labs(title = "Households Expenditure variation - Nawenge Village", caption = "HBCL RAP 2020", fill = "Expenses")+
        theme_void()+
        theme(plot.title = element_text(hjust = 0.5, face = "bold", size = 15, vjust = -3))+
        theme(plot.caption = element_text(hjust = 0.5, vjust = 25, size = 5))+
        theme(legend.title = element_text(face = "bold"))
      
      
      # Houses Construction Materilas --------------
      
      df_HouseConstruction <- df %>% 
        select(Village,Roof_constr, Walls_constr) %>%
        mutate(Roof_constr = ifelse(Roof_constr == "5. Grass / leaves", "Grass / leaves" , Roof_constr)) %>%
        mutate(Walls_constr = ifelse(Walls_constr == "2. Poles and mud", "Poles and mud", Walls_constr),
               Walls_constr = ifelse(Walls_constr == "8. Other", "Other", Walls_constr)) %>% 
        view()
        
      # 1.Burnt bricks wall, roofed with iron sheets %?
      df_CISB <- df_HouseConstruction %>% 
        filter(Roof_constr == "Iron Sheets" & Walls_constr == "Baled bricks") %>%
        group_by(Village, Walls_constr, Roof_constr) %>% 
        summarise(House_constr = n()) %>% 
        mutate(Total_Household = Respondend$Total_Household[match(Village,Respondend$Village)]) %>%
        ungroup() %>% 
        mutate(Iron_Bailed_Perc = round(House_constr/Total_Household*100, digits = 2)) %>% 
        view()
      
       write.csv(df_CISB, "Burntbricks_IronSheet.csv")
      
       # 2. Burnt bricks wall roofed with grass %?
      df_GRASSB <- df_HouseConstruction %>% 
        filter(Roof_constr == "Grass / leaves" & Walls_constr == "Baled bricks") %>%
        group_by(Village, Walls_constr, Roof_constr) %>% 
        summarise(House_constr = n()) %>% 
        mutate(Total_Household = Respondend$Total_Household[match(Village,Respondend$Village)]) %>%
        ungroup() %>% 
        mutate(Grass_Bailed_Perc = round(House_constr/Total_Household*100, digits = 2)) %>% 
        view()
      write.csv(df_GRASSB, "BurntBricks_Grass.csv")
      
      # 3.Poles and mud Grass roofed %
      df_PolMud <- df_HouseConstruction %>% 
        filter(Roof_constr == "Grass / leaves" & Walls_constr == "Poles and mud") %>%
        group_by(Village, Walls_constr, Roof_constr) %>% 
        summarise(House_constr = n()) %>% 
        mutate(Total_Household = Respondend$Total_Household[match(Village,Respondend$Village)]) %>%
        ungroup() %>% 
        mutate(Mud_Poles_Perc = round(House_constr/Total_Household*100, digits = 2)) %>% 
        view()
      write.csv(df_PolMud, "Poles_Grass.csv")
      
      # 4.Mud bricks wall roofed with grasses %?
      df_MudGrass <- df_HouseConstruction %>% 
        filter(Roof_constr == "Grass / leaves" & Walls_constr == "Sun-dried bricks") %>% 
        group_by(Village, Walls_constr, Roof_constr) %>% 
        summarise(House_constr = n()) %>% 
        mutate(Total_Household = Respondend$Total_Household[match(Village,Respondend$Village)]) %>%
        ungroup() %>% 
        mutate(Mud_Grass_Perc = round(House_constr/Total_Household*100, digits = 2)) %>% 
        view()
      write.csv(df_MudGrass, "Mudbricks_Grass.csv")
      
      # Join data 4 frame to have one dataframe containing all construction material
      df_roof_wall <- df_PolMud %>% 
        mutate(BBricks_IS = df_CISB$House_constr[match(Village,df_CISB$Village)],
               BBricks_Grass = df_GRASSB$House_constr[match(Village, df_GRASSB$Village)],
               MudBricks_Grass = df_MudGrass$House_constr[match(Village, df_MudGrass$Village)]) %>% 
        rename(PoleMud = House_constr) %>% 
        select(-Walls_constr, -Roof_constr, -Total_Household, -Mud_Poles_Perc) %>%
        replace(is.na(.), 0) %>% 
        mutate(Total = rowSums(.[2:5])) %>% 
        mutate(PoleMud_Perc = round(PoleMud/Total*100, digits = 2),
               BBricks_IS_Perc = round(BBricks_IS/Total*100, digits = 2),
               BBricks_Grass_Perc = round(BBricks_Grass/Total*100, digits = 2),
               MudBricks_Grass_Perc = round(MudBricks_Grass/Total*100, digits = 2)) %>% 
        pivot_longer(cols = 7:10, names_to = "Materials", values_to = "Percentages") %>% 
        mutate(Materials = ifelse(Materials == "PoleMud_Perc", "Poles and Mud, Grass/Leaves", Materials),
               Materials = ifelse(Materials == "BBricks_IS_Perc", "Baled Bricks, Iron Sheet", Materials),
               Materials = ifelse(Materials == "BBricks_Grass_Perc", "Baled Bricks, Grass/Leaves", Materials),
               Materials = ifelse(Materials == "MudBricks_Grass_Perc", "Sun-dried bricks, Grass/Leaves", Materials)) %>% 
        view()
      
      # Kisewe - Pie Chat
       df_roof_wall %>% 
        filter(Village == "Kisewe") %>% 
        ggplot( aes(x= "", y= Percentages, fill = Materials))+
        geom_bar(stat = "Identity")+
        coord_polar("y", start = 0)+
         geom_text(aes(label = ifelse(Percentages > 10, str_c(Percentages, "%"), "")), position = position_stack(vjust = 0.5))+
         geom_label_repel(aes(label = ifelse(Percentages < 10, str_c(Percentages, "%"), "")), size = 3, show.legend = F, nudge_x = 0.5, hjust = 2)+
         labs(title = "Houses Walls & Roofs Construction Materials - Kisewe Village", fill = "Construction Materials \n (Walls, Roofs)", 
              caption = "HBCL RAP 2020")+
         theme_void()+
         theme(plot.title = element_text(hjust = 0.5, face = "bold", size = 10, vjust = 0.1))+
         theme(legend.title = element_text(face = "bold"))+
         theme(plot.caption = element_text(vjust = 25, face = "bold", size = 5, hjust = 0.5))
         
       
       # Makanga - Pie Chat
       df_roof_wall %>% 
         filter(Village == "Makanga") %>% 
         ggplot( aes(x= "", y= Percentages, fill = Materials))+
         geom_bar(stat = "Identity")+
         coord_polar("y", start = 0)+
         geom_text(aes(label = ifelse(Percentages > 10, str_c(Percentages, "%"), "")), position = position_stack(vjust = 0.5))+
         # geom_label_repel(aes(label = ifelse(Percentages < 10, str_c(Percentages, "%"), "")), size = 3, show.legend = F, nudge_x = 0.5, hjust = 2)+
         labs(title = "Houses Walls & Roofs Construction Materials - Makanga Village", fill = "Construction Materials \n (Walls, Roofs)", 
              caption = "HBCL RAP 2020")+
         theme_void()+
         theme(plot.title = element_text(hjust = 0.5, face = "bold", size = 10, vjust = 0.1))+
         theme(legend.title = element_text(face = "bold"))+
         theme(plot.caption = element_text(vjust = 25, face = "bold", size = 5, hjust = 0.5))
       
       # Mdindo - Pie Chat
       df_roof_wall %>% 
         filter(Village == "Mdindo") %>% 
         ggplot( aes(x= "", y= Percentages, fill = Materials))+
         geom_bar(stat = "Identity")+
         coord_polar("y", start = 0)+
         geom_text(aes(label = ifelse(Percentages > 10, str_c(Percentages, "%"), "")), position = position_stack(vjust = 0.5))+
         geom_label_repel(aes(label = ifelse(Percentages < 10, str_c(Percentages, "%"), "")), size = 3, show.legend = F, nudge_x = 0.5, hjust = -2)+
         labs(title = "Houses Walls & Roofs Construction Materials - Mdindo Village", fill = "Construction Materials \n (Walls, Roofs)", 
              caption = "HBCL RAP 2020")+
         theme_void()+
         theme(plot.title = element_text(hjust = 0.5, face = "bold", size = 10, vjust = 0.1))+
         theme(legend.title = element_text(face = "bold"))+
         theme(plot.caption = element_text(vjust = 25, face = "bold", size = 5, hjust = 0.5))
       
       # Nawenge - Pie Chat
       df_roof_wall %>% 
         filter(Village == "Nawenge") %>% 
         filter(Percentages > 0) %>% 
         ggplot( aes(x= "", y= Percentages, fill = Materials))+
         geom_bar(stat = "Identity")+
         coord_polar("y", start = 0)+
         geom_text(aes(label = ifelse(Percentages > 10, str_c(Percentages, "%"), "")), position = position_stack(vjust = 0.5), vjust = -10)+
         #geom_label_repel(aes(label = ifelse(Percentages < 10, str_c(Percentages, "%"), "")), size = 3, show.legend = F, nudge_x = 0.5, hjust = -2)+
         labs(title = "Houses Walls & Roofs Construction Materials - Mdindo Village", fill = "Construction Materials \n (Walls, Roofs)", 
              caption = "HBCL RAP 2020")+
         theme_void()+
         theme(plot.title = element_text(hjust = 0.5, face = "bold", size = 10, vjust = 0.1))+
         theme(legend.title = element_text(face = "bold"))+
         theme(plot.caption = element_text(vjust = 25, face = "bold", size = 5, hjust = 0.5))
       
  
      
      # LEVEL OF EDUCATION ATTAINED BY HEAD OF HOUSEHOLD --------------------------
      # 1. Head of household with Primary education Level  ----------------
      df_Primary <- df %>% 
        select(Village,HofH,Education_level) %>%
        filter(HofH == "Yes") %>% 
        filter(Education_level == "Primary") %>% 
        select(-HofH) %>% 
        group_by(Village, Education_level) %>% 
        summarise(Primary = n()) %>% 
        select(-Education_level) %>% 
        view()
      write.csv(df_Primary, "Primary_Education.csv")
      
      # 2. Head of household did not go School  ----------------
      df_NoEdL<- df %>% 
        select(Village,HofH,Education_level) %>%
        filter(HofH == "Yes") %>% 
        filter(Education_level %in% c("Didn't go to school","No formal education","Did not go to school","Did not finish Primary school")) %>% 
        select(-HofH) %>% 
        mutate(Education_level = case_when(Education_level %in% c("Didn't go to school","No formal education",
                                                                  "Did not go to school","Did not finish Primary school")~"No Education")) %>% 
        group_by(Village, Education_level) %>% 
        summarise(No_Education = n()) %>% 
        select(-Education_level) %>% 
        view()
      write.csv(df_NoEdL, "No_Education.csv")
      
      # 3. Head of household with Primary education Level  ---------------- 
      df_HighEd <- df %>% 
        select(Village,HofH,Education_level) %>%
        filter(HofH == "Yes") %>% 
        filter(Education_level %in% c("University","Secondary","Diploma in Community Development and Social Work ")) %>%
        select(-HofH) %>% 
        mutate(Education_level = case_when(Education_level %in% c("University","Secondary",
                                                                  "Diploma in Community Development and Social Work ")~"High Education")) %>% 
        group_by(Village, Education_level) %>% 
        summarise(High_Education = n()) %>% 
        select(-Education_level) %>% 
        view()
      write.csv(df_HighEd, "High_Education.csv")
      
      # Education level dataframe
      df_Education <- df_Primary %>% 
        mutate(No_Education = df_NoEdL$No_Education[match(Village,df_NoEdL$Village)],
               High_Education = df_HighEd$High_Education[match(Village, df_HighEd$Village)],
               High_Education = ifelse(is.na(High_Education), 0, High_Education)) %>% 
        ungroup() %>% 
        mutate(Total = rowSums(.[2:4]))%>% 
        mutate(Primary_Per = round(Primary/Total*100, digits = 2),
               No_Education_Per = round(No_Education/Total*100, digits = 2),
               High_Education_Per = round(High_Education/Total*100, digits = 2)) %>% 
        pivot_longer(cols = 6:8, names_to = "Education", values_to = "Percentages") %>% 
        mutate(Education = ifelse(Education == "Primary_Per", "Primary Education", Education),
               Education = ifelse(Education == "No_Education_Per", "No Education", Education),
               Education = ifelse(Education == "High_Education_Per", "High Education", Education))
    
      # Education Level Kisewe Pie Chat plot
      df_Edu_Kisewe <- df_Education %>% 
        filter(Village == "Kisewe") 
      ggplot(data = df_Edu_Kisewe, aes(x = "", y = Percentages, fill = Education))+
        geom_bar(stat = "identity")+
        coord_polar("y", start = 0)+
        geom_text(aes(label = ifelse(Percentages > 90, str_c(Percentages,"%"), "")), position = position_stack(vjust = 0.5), hjust = 0.5, vjust = -5, size = 5, colour = "black")+
        geom_text(aes(label = ifelse(Percentages < 90, str_c(Percentages,"%"), "")), position = position_stack(vjust = 0.3), size = 5, colour = "black",
                  angle = 100, hjust = -0.5)+
        scale_fill_discrete(breaks = c("No Education", "Primary Education", "High Education"))+
        labs(title = "Respondent Education Level - Kisewe Village", caption = "HBCL RAP 2020", fill = "Education Level")+
        theme_void()+
        theme(plot.title = element_text(hjust = 0.5, size = 16, face = "bold", vjust = -5))+
        theme(legend.title = element_text(face = "bold"))+
        theme(plot.caption = element_text(size = 5, vjust = 35, hjust = 0.5))
      
      # Education level Makanga Pie Chat plot
      df_Education %>% 
        filter(Village == "Makanga") %>% 
      ggplot(aes(x = "", y = Percentages, fill = Education))+
        geom_bar(stat = "identity")+
        coord_polar("y", start = 0)+
        geom_text(aes(label = str_c(Percentages,"%")), position = position_stack(vjust = 0.5), hjust = 0.5, vjust = 0.25, size = 5, colour = "black")+
        scale_fill_discrete(breaks = c("No Education", "Primary Education", "High Education"))+
        labs(title = "Respondent Education Level - Makanga Village", caption = "HBCL RAP 2020", fill = "Education Level")+
        theme_void()+
        theme(plot.title = element_text(hjust = 0.5, size = 16, face = "bold", vjust = -5))+
        theme(legend.title = element_text(face = "bold"))+
        theme(plot.caption = element_text(size = 5, vjust = 35, hjust = 0.5))
      
      # Education level Mdindo Pie Chat plot
      df_Education %>% 
        filter(Village == "Mdindo") %>% 
        ggplot(aes(x = "", y = Percentages, fill = Education))+
        geom_bar(stat = "identity")+
        coord_polar("y", start = 0)+
        geom_text(aes(label = ifelse(Percentages > 80, str_c(Percentages,"%"), "")), position = position_stack(vjust = 0.5), hjust = 0.5, vjust = -5, size = 5, colour = "black")+
        geom_text(aes(label = ifelse(Percentages < 80, str_c(Percentages,"%"), "")), position = position_stack(vjust = 0.3), size = 5, colour = "black",
                  angle = 100, hjust = -0.5)+
        scale_fill_discrete(breaks = c("No Education", "Primary Education", "High Education"))+
        labs(title = "Respondent Education Level - Mdindo Village", caption = "HBCL RAP 2020", fill = "Education Level")+
        theme_void()+
        theme(plot.title = element_text(hjust = 0.5, size = 16, face = "bold", vjust = -5))+
        theme(legend.title = element_text(face = "bold"))+
        theme(plot.caption = element_text(size = 5, vjust = 35, hjust = 0.5))
      
      # Education level Nawenge Pie Chat plot
      df_Education %>% 
        filter(Village == "Nawenge") %>% 
        filter(Percentages > 0) %>% 
        ggplot(aes(x = "", y = Percentages, fill = Education))+
        geom_bar(stat = "identity")+
        coord_polar("y", start = 0)+
        geom_text(aes(label = str_c(Percentages,"%")), position = position_stack(vjust = 0.5), hjust = 0.5, vjust = 0.25, size = 5, colour = "black")+
        scale_fill_discrete(breaks = c("No Education", "Primary Education", "High Education"))+
        labs(title = "Respondent Education Level - Nawenge Village", caption = "HBCL RAP 2020", fill = "Education Level")+
        theme_void()+
        theme(plot.title = element_text(hjust = 0.5, size = 16, face = "bold", vjust = -5))+
        theme(legend.title = element_text(face = "bold"))+
        theme(plot.caption = element_text(size = 5, vjust = 35, hjust = 0.5))
  
      write.csv(df_Education, "Education.csv")
      
      
      
      # WATER SERVICES AND DISTANCE 
      # 1 (a).Show percentage of each sources from each of the affected village - Dry Season
      df_Water_dryseason <- df %>% 
        select(Village, Water_Dryseason, Dryseason_dist_km) %>%
        mutate_all(na_if,"-") %>% 
        separate(Water_Dryseason,into = c("No_code","Water_Dryseason"),sep = "[0-9].") %>%
        mutate(Water_Dryseason = ifelse(is.na(Water_Dryseason), No_code, Water_Dryseason)) %>%
        select(-No_code) %>% 
        filter(!is.na(Water_Dryseason)) %>% 
        group_by(Village, Water_Dryseason) %>% 
        summarise(Dry_Season = n()) %>% 
        mutate(Dry_Season_Perc = round(Dry_Season/sum(Dry_Season)*100, digits = 2)) %>%
        mutate(Water_Dryseason = ifelse(Water_Dryseason ==" Unprotected spring", "Unprotected spring", Water_Dryseason),
               Water_Dryseason = ifelse(Water_Dryseason =="Surface water (lake/dam/river/stream)", "Surface Water", Water_Dryseason),
               Water_Dryseason = ifelse(Water_Dryseason ==" Surface water (lake/dam/river/stream)", "Surface Water", Water_Dryseason)) %>% 
        view()
   
      # Bar Chart Water Sources Dry Season
     ggplot(data = df_Water_dryseason, aes(x = Village, y = Dry_Season_Perc, fill = Water_Dryseason))+
        geom_col(position = position_dodge2(preserve = "single", width = 0.9))+
        #geom_text(aes(y = -1, label = Water_Dryseason), position = position_dodge(width = 0.9), size = 3, angle = 90, va = "top")+
        geom_text(aes(label = ifelse(Dry_Season_Perc > 2, str_c(Dry_Season_Perc, "%"), "")), position = position_dodge2(width= 0.9, preserve = "single"),
                   angle = 90, hjust = 1, size = 4)+
        geom_text(aes(label = ifelse(Dry_Season_Perc < 2, str_c(Dry_Season_Perc, "%"), "")), position = position_dodge2(width= 0.9, preserve = "single"), 
                   angle = 90, hjust = -0.1, size = 4)+
        scale_y_continuous(name = "Percentages", breaks = breaks_width(5))+
        labs(title = "Water Sources - Dry Season", caption = "HBCL RAP 2020", fill = "Water Sources")+
        theme_light()+
        theme(plot.title = element_text(size = 15, face = "bold", hjust = 0.5))+
        theme(plot.caption = element_text(size = 5, hjust = 1))+
        theme(legend.title = element_text(face = "bold"))+
        theme(axis.title.x = element_text(face = "bold"), axis.title.y = element_text(face = "bold"))
       
       
  
      
      # (i). Pie Chat plot Kisewe water Sources - Dry Season -------------------------------------------------------
       df_Water_dryseason %>% 
        filter(Village == "Kisewe") %>% 
          ggplot(aes(x = "", y = Dry_Season_Perc, fill = Water_Dryseason))+
            geom_bar(stat = "identity")+
            coord_polar("y", start = 0)+
            geom_text(aes(label = str_c(Dry_Season_Perc, "%")), position = position_stack(vjust = 0.1), size = 5, colour = "black")+
            labs(title = "Household Water Sources During Dry season - Kisewe Village", caption = "HBCL RAP 2020", fill = "Water Sources")+
            theme_void()+
            theme(plot.title = element_text(hjust = 0.5, size = 16, face = "bold", vjust = -5))+
            theme(legend.title = element_text(face = "bold"))+
            theme(plot.caption = element_text(size = 5, vjust = 35, hjust = 0.5))
          
          # (ii). Makanga Pie Chat plot Makanga water Sources - Dry Season -------------------------------------------------------
          df_Water_dryseason %>% 
            filter(Village == "Makanga") %>% 
          ggplot(aes(x = "", y = Dry_Season_Perc, fill = Water_Dryseason))+
            geom_bar(stat = "identity")+
            coord_polar("y", start = 0)+
            geom_text(aes(label = str_c(Dry_Season_Perc, "%")), position = position_stack(vjust = 0.5), size = 5, colour = "black")+
            labs(title = "Household Water Sources During Dry season - Makanga Village", caption = "HBCL RAP 2020", fill = "Water Sources")+
            theme_void()+
            theme(plot.title = element_text(hjust = 0.5, size = 16, face = "bold", vjust = -8))+
            theme(legend.title = element_text(face = "bold"))+
            theme(plot.caption = element_text(size = 5, vjust = 35, hjust = 0.5))
          
          # (ii). Mdindo Pie Chat plot Mdindo water Sources - Dry Season -------------------------------------------------------
          df_Water_dryseason %>% 
            filter(Village == "Mdindo") %>% 
            mutate(Water_Dryseason = fct_reorder(Water_Dryseason, -Dry_Season_Perc)) %>% 
          ggplot( aes(x = "", y = Dry_Season_Perc, fill =Water_Dryseason))+
            geom_bar(stat = "identity")+
            coord_polar("y", start = 0)+
            geom_text(aes(label = ifelse(Dry_Season_Perc > 2, str_c(Dry_Season_Perc, "%"), "")), position = position_stack(vjust = 0.5))+
            geom_label_repel(aes(label = ifelse(Dry_Season_Perc < 2, str_c(Dry_Season_Perc, "%"), ""), y = Dry_Season_Perc), size = 4, show.legend = F, nudge_x = 0.6)+
            labs(title = "Household Water Sources During Dry season - Mdindo Village", caption = "HBCL RAP 2020", fill = "Water Sources")+
            theme_void()+
            theme(plot.title = element_text(hjust = 0.5, size = 16, face = "bold", vjust = 0.5))+
            theme(legend.title = element_text(face = "bold"))+
            theme(plot.caption = element_text(size = 5, vjust = 35, hjust = 0.5))
          
          # (iv). Pie Chat plot Nawenge water Sources - Dry Season -------------------------------------------------------
         df_Water_dryseason %>% 
            filter(Village == "Nawenge") %>% 
          ggplot(aes(x = "", y = Dry_Season_Perc, fill = Water_Dryseason))+
            geom_bar(stat = "identity")+
            coord_polar("y", start = 0)+
            geom_text(aes(label = str_c(Dry_Season_Perc, "%")), position = position_stack(vjust = 0.5), size = 5, colour = "black")+
            labs(title = "Household Water Sources During Dry season - Nawenge Village", caption = "HBCL RAP 2020", fill = "Water Sources")+
            theme_void()+
            theme(plot.title = element_text(hjust = 0.5, size = 16, face = "bold", vjust = -8))+
            theme(legend.title = element_text(face = "bold"))+
            theme(plot.caption = element_text(size = 5, vjust = 35, hjust = 0.5))
      
      write.csv(df_Water, "water_dryseason.csv")
      
      # 1 (b).Distance variation to the houses - Dry Season --------------------------------- 
      df_Water <- df %>% 
        select(Village, Water_Dryseason, Dryseason_dist_km) %>%
        mutate_all(na_if,"-") %>% 
        separate(Water_Dryseason,into = c("No_code","Water_Dryseason"),sep = "[0-9].") %>%
        mutate(Water_Dryseason = ifelse(is.na(Water_Dryseason), No_code, Water_Dryseason)) %>%
        select(-No_code) %>% 
        mutate(Dryseason_dist_km = as.numeric(Dryseason_dist_km)) %>% 
        filter(!is.na(Water_Dryseason)) %>% 
        group_by(Village, Water_Dryseason) %>% 
        summarise(Ave_dist = mean(Dryseason_dist_km, na.rm = TRUE)) %>% 
        view()
      
      
      # 2 (a).Show percentage of each sources from each of the affected village - Wet Season
      df_Water_wetseason <- df %>% 
        select(Village, Water_Wetseason, Wetseason_dist_km) %>%
        mutate_all(na_if,"-") %>% 
        separate(Water_Wetseason,into = c("No_code","Water_Wetseason"),sep = "[0-9].") %>%
        mutate(Water_Wetseason = ifelse(is.na(Water_Wetseason), No_code, Water_Wetseason)) %>%
        select(-No_code) %>% 
        filter(!is.na(Water_Wetseason)) %>% 
        group_by(Village, Water_Wetseason) %>% 
        summarise(Wet_Season = n()) %>% 
        mutate(Wet_Season_Perc = round(Wet_Season/sum(Wet_Season)*100, digits = 2)) %>%
        mutate(Water_Wetseason = ifelse(Water_Wetseason ==" Unprotected spring", "Unprotected spring", Water_Wetseason),
               Water_Wetseason = ifelse(Water_Wetseason =="Surface water (lake/dam/river/stream)", "Surface Water", Water_Wetseason),
               Water_Wetseason = ifelse(Water_Wetseason ==" Surface water (lake/dam/river/stream)", "Surface Water", Water_Wetseason)) %>% 
        view()
      
      
      # Bar Chart Water Sources Wet Season
      ggplot(data = df_Water_wetseason, aes(x = Village, y = Wet_Season_Perc, fill = Water_Wetseason))+
        geom_col(position = position_dodge2(preserve = "single", width = 0.9))+
        #geom_text(aes(y = -1, label = Water_Dryseason), position = position_dodge(width = 0.9), size = 3, angle = 90, va = "top")+
        geom_text(aes(label = ifelse(Wet_Season_Perc > 2, str_c(Wet_Season_Perc, "%"), "")), position = position_dodge2(width= 0.9, preserve = "single"), 
                  angle = 90, hjust = 1, size = 4)+
        geom_text(aes(label = ifelse(Wet_Season_Perc < 2, str_c(Wet_Season_Perc, "%"), "")), position = position_dodge2(width= 0.9, preserve = "single"), 
                   angle = 90, hjust = -0.1, size = 4)+
        scale_y_continuous(name = "Percentages", breaks = breaks_width(5))+
        labs(title = "Water Sources - Wet Season", caption = "HBCL RAP 2020", fill = "Water Sources")+
        theme_light()+
        theme(plot.title = element_text(size = 15, face = "bold", hjust = 0.5))+
        theme(plot.caption = element_text(size = 5, hjust = 1))+
        theme(legend.title = element_text(face = "bold"))+
        theme(axis.title.x = element_text(face = "bold"), axis.title.y = element_text(face = "bold"))
      
      
      # (i). Pie Chat plot Kisewe water Sources - Wet Season -------------------------------------------------------
      df_Water_wetseason %>% 
        filter(Village == "Kisewe") %>%
        mutate(Water_Wetseason = fct_reorder(Water_Wetseason, -Wet_Season_Perc)) %>% 
      ggplot(aes(x = "", y = Wet_Season_Perc, fill = Water_Wetseason))+
        geom_bar(stat = "identity")+
        coord_polar("y", start = 0)+
        geom_text(aes(label = ifelse(Wet_Season_Perc >1, str_c( Wet_Season_Perc, "%"),"")), position = position_stack(vjust = 0.5), size = 5, colour = "black")+
        geom_label_repel(aes(label = ifelse(Wet_Season_Perc < 1, str_c(Wet_Season_Perc, "%"), "")), size = 4, nudge_x = 0.55, show.legend = F)+
        labs(title = "Household Water Sources During Wet season - Kisewe Village", caption = "HBCL RAP 2020", fill = "Water Sources")+
        theme_void()+
        theme(plot.title = element_text(hjust = 0.5, size = 15, face = "bold", vjust = -2))+
        theme(legend.title = element_text(face = "bold"))+
        theme(plot.caption = element_text(size = 5, vjust = 35, hjust = 0.5))
      
      # (ii). Pie Chat plot Makanga water Sources - Wet Season -------------------------------------------------------
      df_Water_wetseason %>% 
        filter(Village == "Makanga") %>% 
      ggplot(aes(x = "", y = Wet_Season_Perc, fill = Water_Wetseason))+
        geom_bar(stat = "identity")+
        coord_polar("y", start = 0)+
        geom_text(aes(label = str_c( Wet_Season_Perc, "%")), position = position_stack(vjust = 0.5), size = 5, colour = "black")+
        labs(title = "Household Water Sources During Wet season - Makanga Village", caption = "HBCL RAP 2020", fill = "Water Sources")+
        theme_void()+
        theme(plot.title = element_text(hjust = 0.5, size = 16, face = "bold", vjust = -8))+
        theme(legend.title = element_text(face = "bold"))+
        theme(plot.caption = element_text(size = 5, vjust = 35, hjust = 0.5))
      
    
      # (iii). Mdindo Pie Chat plot Mdindo water Sources - Wet Season -------------------------------------------------------
     df_Water_wetseason %>% 
        filter(Village == "Mdindo") %>% 
      ggplot(aes(x = "", y = Wet_Season_Perc, fill = reorder(Water_Wetseason, -Wet_Season_Perc)))+
        geom_bar(stat = "identity")+
        coord_polar("y", start = 0)+
        geom_text(aes(label = ifelse (Wet_Season_Perc > 14, str_c( Wet_Season_Perc, "%"), "")), position = position_stack(vjust = 0.5), size = 5, colour = "black")+
        geom_label_repel(aes(label = ifelse(Wet_Season_Perc < 14, str_c(Wet_Season_Perc, "%"), "")), size = 3, nudge_x = 1.5, show.legend = F)+
        labs(title = "Household Water Sources During Wet season - Mdindo Village", caption = "HBCL RAP 2020", fill = "Water Sources")+
        theme_void()+
        theme(plot.title = element_text(hjust = 0.5, size = 15, face = "bold", vjust = 2))+
        theme(legend.title = element_text(face = "bold"))+
        theme(plot.caption = element_text(size = 5, vjust = 35, hjust = 0.5))
      
      
      # (iiv).Pie Chat plot Nawenge water Sources - Wet Season -------------------------------------------------------
     df_Water_wetseason %>% 
        filter(Village == "Nawenge") %>% 
      ggplot(aes(x = "", y = Wet_Season_Perc, fill = Water_Wetseason))+
        geom_bar(stat = "identity")+
        coord_polar("y", start = 0)+
        geom_text(aes(label = str_c( Wet_Season_Perc, "%")), position = position_stack(vjust = 0.5), size = 5, colour = "black")+
        labs(title = "Household Water Sources During Wet season - Nawenge Village", caption = "HBCL RAP 2020", fill = "Water Sources")+
        theme_void()+
        theme(plot.title = element_text(hjust = 0.5, size = 16, face = "bold", vjust = -8))+
        theme(legend.title = element_text(face = "bold"))+
        theme(plot.caption = element_text(size = 5, vjust = 35, hjust = 0.5))
      
        # write.csv(df_Water, "water_wetseason.csv")
      
      
      # b.(i). Pie Chat plot Nawenge water Sources - Wet Season -------------------------------------------------------
      df_Nawenge_Drywater <- df_Water %>% 
        filter(Village == "Nawenge") %>% 
      ggplot(data = df_Nawenge_Drywater, aes(x = "", y = Dry_Season_Perc, fill = Water_Dryseason))+
        geom_bar(stat = "identity")+
        coord_polar("y", start = 0)+
        geom_text(aes(label = str_c(Dry_Season_Perc, "%")), position = position_stack(vjust = 0.5), size = 5, colour = "white")+
        labs(title = "Household Water Sources During Dry season - Nawenge Village", caption = "HBCL RAP 2020", fill = "Water Sources")+
        theme_void()+
        theme(plot.title = element_text(hjust = 0.5, size = 16, face = "bold", vjust = -5))+
        theme(legend.title = element_text(face = "bold"))+
        theme(plot.caption = element_text(size = 5, vjust = 35, hjust = 0.5))
      
      # 2 (b).Distance variation to the houses - Wet Season  
      df_Water_wetseason <- df %>% 
        select(Village, Water_Wetseason, Wetseason_dist_km) %>%
        mutate_all(na_if,"-") %>% 
        separate(Water_Wetseason,into = c("No_code","Water_Wetseason"),sep = "[0-9].") %>%
        mutate(Water_Wetseason = ifelse(is.na(Water_Wetseason), No_code, Water_Wetseason)) %>%
        select(-No_code) %>% 
        mutate( Wetseason_dist_km = as.numeric(Wetseason_dist_km)) %>% 
        filter(!is.na(Water_Wetseason)) %>% 
        group_by(Village, Water_Wetseason) %>% 
        summarise(Ave_dist = mean(Wetseason_dist_km, na.rm = TRUE)) %>% 
        view()
      
      
      # MAIN SOURCES OF ENERGY 
      # 1. Main	sources of energy for lighting 
      df_LightEnergy <- df %>% 
        select(Village, Lighting_enrg) %>% 
        separate(Lighting_enrg, into = c("Number", "Lighting_enrg"), sep = "[0-9]. ") %>% 
        mutate(Lighting_enrg = ifelse(is.na(Lighting_enrg), Number, Lighting_enrg)) %>%
        select(-Number) %>% 
        view()
      
      df_lumbesa <- df_LightEnergy %>% 
        filter(str_detect(Lighting_enrg, pattern = "Lumbesa")| str_detect(Lighting_enrg, pattern = "lumbesa")|str_detect(Lighting_enrg, pattern = "Local")) %>%
        mutate(Lighting_enrg = ifelse(str_detect(Lighting_enrg, pattern = "Lumbesa")| str_detect(Lighting_enrg, pattern = "lumbesa")|str_detect(Lighting_enrg, pattern = "Local"),
                                      "Lumbesa", Lighting_enrg)) %>% 
        group_by(Village) %>% 
        summarise(Lumbesa = n()) %>% 
        view()
      
      df_flashlight <- df_LightEnergy %>% 
        filter(str_detect(Lighting_enrg, pattern = "Flash light")| str_detect(Lighting_enrg, pattern = "Tourch")|str_detect(Lighting_enrg, pattern = "Torch")|
                 str_detect(Lighting_enrg, pattern = "Charg")|str_detect(Lighting_enrg, pattern = "flash light")|str_detect(Lighting_enrg, pattern = "Battery powered lights")|
                 str_detect(Lighting_enrg, pattern = "Flash lamp")|str_detect(Lighting_enrg, pattern = "Flashlight")) %>% 
        mutate(Lighting_enrg = ifelse(str_detect(Lighting_enrg, pattern = "Flash light")| str_detect(Lighting_enrg, pattern = "Tourch")|str_detect(Lighting_enrg, pattern = "Torch")|
                 str_detect(Lighting_enrg, pattern = "Charg")|str_detect(Lighting_enrg, pattern = "flash light")|str_detect(Lighting_enrg, pattern = "Battery powered lights")|
                 str_detect(Lighting_enrg, pattern = "Flash lamp")|str_detect(Lighting_enrg, pattern = "Flashlight"), "Flash light", Lighting_enrg)) %>% 
        group_by(Village) %>% 
        summarise(Flash_Light = n()) %>% 
        view()
      
      df_solar <- df_LightEnergy %>% 
        filter(str_detect(Lighting_enrg, pattern = "Solar")) %>% 
        mutate(Lighting_enrg = ifelse(str_detect(Lighting_enrg, pattern = "Solar"), "Solar", Lighting_enrg)) %>% 
        group_by(Village) %>% 
        summarise(Solar = n()) %>% 
        view()
      
      df_wicklamp <- df_LightEnergy %>% 
        filter(str_detect(Lighting_enrg, pattern = "Wick lamp")|str_detect(Lighting_enrg, pattern = "Wicklamp")) %>% 
        mutate(Lighting_enrg= ifelse(str_detect(Lighting_enrg, pattern = "Wick lamp")|str_detect(Lighting_enrg, pattern = "Wicklamp"), "Wick Lamp", Lighting_enrg)) %>% 
        group_by(Village) %>% 
        summarise(Wicklamp = n()) %>% 
        view()
      
      
      df_pressurelamp <- df_LightEnergy %>% 
        filter(str_detect(Lighting_enrg, pattern = "Pressure lamp")|str_detect(Lighting_enrg, pattern = "Hurricane lamp")) %>% 
        mutate(Lighting_enrg = ifelse(str_detect(Lighting_enrg, pattern = "Pressure lamp")|str_detect(Lighting_enrg, pattern = "Hurricane lamp"), "Pressure Lamp", Lighting_enrg)) %>%
        group_by(Village) %>% 
        summarise(Pressurelamp= n()) %>% 
        view()
      
      df_candle <- df_LightEnergy %>% 
        filter(Lighting_enrg == "Candles") %>%
        group_by(Village) %>% 
        summarise(Candle = n()) %>% 
        view()
      
      df_generator <- df_LightEnergy %>% 
        filter(str_detect(Lighting_enrg, pattern = "generator")) %>% 
        mutate(Lighting_enrg = ifelse(str_detect(Lighting_enrg, pattern = "generator"), "Generator", Lighting_enrg)) %>%
        group_by(Village) %>% 
        summarise(generator = n()) %>% 
        view()
      
      
      df_biomass <- df_LightEnergy %>% 
      filter(str_detect(Lighting_enrg, pattern = "biomass")) %>% 
        mutate(Lighting_enrg = ifelse(str_detect(Lighting_enrg, pattern = "biomass"), "Firewood", Lighting_enrg)) %>% 
        group_by(Village) %>% 
        summarise(Firewood = n()) %>% 
        view()
      
      
      # Join all Energy Source for Lighting dataframe
      df_lighting <- df_biomass %>% 
        mutate(Lumbesa = df_lumbesa$Lumbesa[match(Village,df_lumbesa$Village)],
               Flash_Light = df_flashlight$Flash_Light[match(Village,df_flashlight$Village)],
               Solar = df_solar$Solar[match(Village,df_solar$Village)],
               Wicklamp= df_wicklamp$Wicklamp[match(Village,df_wicklamp$Village)],
               Pressulamp = df_pressurelamp$Pressurelamp[match(Village,df_pressurelamp$Village)],
               Candle = df_candle$Candle[match(Village,df_candle$Village)],
               generator = df_generator$generator[match(Village,df_generator$Village)],
               Firewood = df_biomass$Firewood[match(Village, df_biomass$Village)])%>% 
        replace(is.na(.),0) %>% 
        mutate(Total = rowSums(.[2:9])) %>% 
        mutate(Firewood_P = round(Firewood/Total*100, digits = 2),
               Lumbesa_P = round(Lumbesa/Total*100, digits = 2),
               Flash_light_P = round(Flash_Light/Total*100, digits = 2),
               Solar_P = round(Solar/Total*100, digits = 2),
               Wicklamp_P = round(Wicklamp/Total*100, digits = 2),
               Pressurelamp_P = round(Pressulamp/Total*100, digits = 2),
               Candle_p = round(Candle/Total*100, digits = 2),
               Generator_P = round(generator/Total*100, digits = 2)) %>% 
        pivot_longer(cols = 11:18, names_to = "Energy", values_to = "Percentages") %>% 
        mutate(Energy = ifelse(Energy == "Firewood_P", "Firewood(biomass)", Energy),
               Energy= ifelse(Energy == "Lumbesa_P", "Local Made (Lumbesa)", Energy),
               Energy = ifelse(Energy == "Flash_light_P", "Flash Light", Energy),
               Energy = ifelse(Energy == "Solar_P", "Solar", Energy),
               Energy = ifelse(Energy == "Wicklamp_P", "Wick lamp", Energy),
               Energy = ifelse(Energy == "Pressurelamp_P", "Pressure Lamp", Energy),
               Energy = ifelse(Energy == "Candle_p", "Candle", Energy),
               Energy = ifelse(Energy == "Generator_P", "Generator", Energy)) %>% 
       
        
        view()
      df_lighting %>% 
        filter(Percentages > 0) %>% 
        mutate(Energy = as.factor(Energy)) %>% 
        
      # BAR CHART - Energy for Lighting
      ggplot(aes(x = Village, y = Percentages, fill = Energy))+
        geom_col(position = position_dodge2(width = 0.9, preserve = "single"))+
        #geom_text(aes(label = ifelse(Percentages >= 5, str_c(Percentages, "%"), "")), position = position_dodge2(width = 0.9, preserve = "single"), 
                  #size =4, angle = 90, hjust = 1)+
        #geom_text(aes(label = ifelse(Percentages < 5, str_c(Percentages, "%"), "")), position = position_dodge2(width = 0.9, preserve = "single"), 
                  #size =4, angle = 90, hjust = -0.1)+
        scale_y_continuous(breaks = breaks_width(5))+
        labs(title = "Energy Sources for Lighting",caption = "HBCL RAP 2020")+
        theme_light()+
        theme(plot.title = element_text(size = 15, hjust = 0.5, face = "bold"))+
        theme(plot.caption = element_text(size = 5))+
        theme(legend.title = element_text(face = "bold"))+
        theme(axis.title.x = element_text(face = "bold"), axis.title.y = element_text(face = "bold"))
        
      
      # Kisewe Lighting Energy Sources 
       df_lighting %>% 
         filter(Village == "Kisewe") %>% 
         filter(Percentages > 0) %>% 
         ggplot(aes(x = "", y = Percentages, fill = Energy))+
         geom_bar(stat = "identity")+
         coord_polar("y", start = 0)+
         geom_text(aes(label = ifelse(Percentages > 4, str_c(Percentages, "%"), "")), position = position_stack(vjust = 0.5))+
         geom_text(aes(label = ifelse(Percentages == 2.1, str_c(Percentages, "%"), "")), position = position_stack(vjust = 0.5), angle = 90, hjust = -1.5)+
         geom_text(aes(label = ifelse(Percentages < 2, str_c(Percentages, "%"), "")), position = position_stack(vjust = 0.5), angle = 70, hjust = 2, vjust = -0.25)+
         labs(title = "Energy Source for Lighting - Kisewe Village", caption = "HBCL RAP 2020")+
         theme_void()+
         theme(plot.title = element_text(size = 15, hjust = 0.5, vjust = -10, face = "bold"))+
         theme(plot.caption = element_text(size = 5, hjust = 0.5, vjust = 25))+
         theme(legend.title = element_text(face = "bold", size = 10))
       
       # Makanga Lighting Energy Sources
       df_lighting %>% 
         filter(Village == "Makanga") %>% 
         filter(Percentages > 0) %>% 
         ggplot(aes(x = "", y = Percentages, fill = Energy))+
         geom_bar(stat = "identity")+
         coord_polar("y", start = 0)+
         geom_text(aes(label = ifelse(Percentages > 4, str_c(Percentages, "%"), "")), position = position_stack(vjust = 0.5))+
         geom_text(aes(label = ifelse(Percentages == 2.1, str_c(Percentages, "%"), "")), position = position_stack(vjust = 0.5), angle = 90, hjust = -1.5)+
         geom_text(aes(label = ifelse(Percentages < 2, str_c(Percentages, "%"), "")), position = position_stack(vjust = 0.5), angle = 70, hjust = 2, vjust = -0.25)+
         labs(title = "Energy Source for Lighting - Makanga Village", caption = "HBCL RAP 2020")+
         theme_void()+
         theme(plot.title = element_text(size = 15, hjust = 0.5, vjust = -10, face = "bold"))+
         theme(plot.caption = element_text(size = 5, hjust = 0.5, vjust = 25))+
         theme(legend.title = element_text(face = "bold", size = 10))
       
       # Mdindo Lighting Energy Sources 
       df_lighting %>% 
         filter(Village == "Mdindo") %>% 
         filter(Percentages > 0) %>% 
         ggplot(aes(x = "", y = Percentages, fill = Energy))+
         geom_bar(stat = "identity")+
         coord_polar("y", start = 0)+
         geom_text(aes(label = ifelse(Percentages > 4, str_c(Percentages, "%"), "")), position = position_stack(vjust = 0.5), vjust = -2)+
         geom_text(aes(label = ifelse(Percentages == 2.54, str_c(Percentages, "%"), "")), position = position_stack(vjust = 0.5), hjust = 0.5, angle = 100)+
         geom_text(aes(label = ifelse(Percentages < 2, str_c(Percentages, "%"), "")), position = position_stack(vjust = 0.5), hjust = -1.5, vjust = 0.5, angle = 90)+
         labs(title = "Energy Source for Lighting - Mdindo Village", caption = "HBCL RAP 2020")+
         theme_void()+
         theme(plot.title = element_text(size = 15, hjust = 0.5, vjust = -10, face = "bold"))+
         theme(plot.caption = element_text(size = 5, hjust = 0.5, vjust = 25))+
         theme(legend.title = element_text(face = "bold", size = 10))
       
       # Nawenge Lighting Energy Sources
       df_lighting %>% 
         filter(Village == "Nawenge") %>% 
         filter(Percentages > 0) %>% 
         ggplot(aes(x = "", y = Percentages, fill = Energy))+
         geom_bar(stat = "identity")+
         coord_polar("y", start = 0)+
         geom_text(aes(label =str_c(Percentages, "%")), position = position_stack(vjust = 0.5), vjust = -15)+
         labs(title = "Energy Source for Lighting - Nawenge Village", caption = "HBCL RAP 2020")+
         theme_void()+
         theme(plot.title = element_text(size = 15, hjust = 0.5, vjust = -10, face = "bold"))+
         theme(plot.caption = element_text(size = 5, hjust = 0.5, vjust = 25))+
         theme(legend.title = element_text(face = "bold", size = 10))
    
      
      # 2. MAIN SOURCE OF ENERGY FOR COOKING--------------------------------------------------------
      df_CookingEnergy <- df %>% 
        select(Village, Cooking_enrg) %>% 
        mutate(Cooking_enrg = ifelse(Cooking_enrg == "7. Firewood (biomass)", "Firewood (biomass)", Cooking_enrg),
               Cooking_enrg = ifelse(Cooking_enrg == "Torch" ,"Firewood (biomass)",  Cooking_enrg),
               Cooking_enrg = ifelse(Cooking_enrg == "Lumbesa (battery powered) and flashlight", "Firewood (biomass)", Cooking_enrg)) %>%
        mutate_all(na_if,"-") %>%
        group_by(Village, Cooking_enrg) %>% 
        summarise(CookE_Per = n()) %>% 
        filter(!is.na(Cooking_enrg)) %>%
        mutate(CookE_Perc = round(CookE_Per/sum(CookE_Per)*100, digits = 2)) %>% 
         
        view()
        
       df_CookingEnergy %>% 
         #filter(Percentages > 0) %>% 
         #mutate(Energy = as.factor(Energy)) %>% 
         
         # BAR CHART - Energy for Cooking
         ggplot(aes(x = Village, y = CookE_Perc, fill = Cooking_enrg))+
         geom_col(position = position_dodge2(width = 0.9, preserve = "single"))+
         #geom_text(aes(label = ifelse(CookE_Perc >= 5, str_c(CookE_Perc, "%"), "")), position = position_dodge2(width = 0.9, preserve = "single"), 
                   #size =4, angle = 90, hjust = 1)+
         #geom_text(aes(label = ifelse(CookE_Perc < 5, str_c(CookE_Perc, "%"), "")), position = position_dodge2(width = 0.9, preserve = "single"), 
                   #size =4, angle = 90, hjust = -0.1)+
         scale_y_continuous(name = "Percentages", breaks = breaks_width(5))+
         labs(title = "Energy Sources for Cooking",caption = "HBCL RAP 2020", fill = "Energy Sources")+
         theme_light()+
         theme(plot.title = element_text(size = 15, hjust = 0.5, face = "bold"))+
         theme(plot.caption = element_text(size = 5))+
         theme(legend.title = element_text(face = "bold"))+
         theme(axis.title.x = element_text(face = "bold"), axis.title.y = element_text(face = "bold"))
       
      # 1.Kisewe Energy Source
      df_CookingEnergy %>% 
        filter(Village == "Kisewe") %>% 
        mutate(Cooking_enrg = fct_reorder(Cooking_enrg, -CookE_Perc)) %>% 
        ggplot(aes(x = "", y = CookE_Perc, fill = Cooking_enrg))+
        geom_bar(stat = "identity")+
        coord_polar("y", start = 0)+
        geom_text(aes(label = ifelse(CookE_Perc > 2, str_c(CookE_Perc, "%"), "")), position = position_stack(vjust = 0.5), size = 5, vjust = -8, hjust = 0.1, colour = "black")+
        geom_label_repel(aes(label = ifelse(CookE_Perc < 2, str_c(CookE_Perc, "%"), "")), size = 5,vjust = -8, hjust = 0.7, nudge_x = -0.05, show.legend = F)+
        labs(title = "Household Energy Sources for Cooking - Kisewe Village", caption = "HBCL RAP 2020", fill = "Energy Sources")+
        theme_void()+
        theme(plot.title = element_text(hjust = 0.5, size = 16, face = "bold", vjust = -5))+
        theme(legend.title = element_text(face = "bold"))+
        theme(plot.caption = element_text(size = 5, vjust = 35, hjust = 0.5))
       
       
      # 2.Makanga Energy Source
      df_CookingEnergy %>% 
        filter(Village == "Makanga") %>% 
        ggplot(aes(x = "", y = CookE_Perc, fill = Cooking_enrg))+
        geom_bar(stat = "identity")+
        coord_polar("y", start = 0)+
        geom_text(aes(label = str_c(CookE_Perc, "%")), position = position_stack(vjust = 0.5), size = 5, colour = "white")+
        labs(title = "Household Energy Sources for Cooking - Makanga Village", caption = "HBCL RAP 2020", fill = "Energy Sources")+
        theme_void()+
        theme(plot.title = element_text(hjust = 0.5, size = 16, face = "bold", vjust = -5))+
        theme(legend.title = element_text(face = "bold"))+
        theme(plot.caption = element_text(size = 5, vjust = 35, hjust = 0.5))
        
        
      # 3.Mdindo Energy Source for Cooking
      df_CookingEnergy %>% 
        filter(Village == "Mdindo") %>% 
        mutate(Cooking_enrg = fct_reorder(Cooking_enrg, -CookE_Perc)) %>% 
        ggplot(aes(x = "", y = CookE_Perc, fill = Cooking_enrg))+
        geom_bar(stat = "identity")+
        coord_polar("y", start = 0)+
        geom_text(aes(label = ifelse(CookE_Perc == 2.04, str_c(CookE_Perc, "%"), "")), position = position_stack(vjust = 0.5), size = 5, vjust = 1, hjust =-1, colour = "black",
                  angle = 85)+
        geom_text(aes(label = ifelse(CookE_Perc > 4, str_c(CookE_Perc, "%"), "")), position = position_stack(vjust = 0.5), size = 5, vjust = -7, hjust =- 0.3, colour = "black")+
        geom_label_repel(aes(label = ifelse(CookE_Perc < 2, str_c(CookE_Perc, "%"), "")), size = 5,vjust = -8, hjust = 1.5, nudge_x = -0.45, show.legend = F)+
        labs(title = "Household Energy Sources for Cooking - Mdindo Village", caption = "HBCL RAP 2020", fill = "Energy Sources")+
        theme_void()+
        theme(plot.title = element_text(hjust = 0.5, size = 15, face = "bold", vjust = -10))+
        theme(legend.title = element_text(face = "bold"))+
        theme(plot.caption = element_text(size = 5, vjust = 35, hjust = 0.5))
      
        
        # 4.Nawenge Energy Source for Cooking
        df_CookingEnergy %>% 
          filter(Village == "Nawenge") %>% 
        ggplot(aes(x = "", y = CookE_Perc, fill = Cooking_enrg))+
          geom_bar(stat = "identity")+
          coord_polar("y", start = 0)+
          geom_text(aes(label = str_c(CookE_Perc, "%")), position = position_stack(vjust = 0.5), vjust = -10,size = 5, colour = "black")+
          labs(title = "Household Energy Sources for Cooking - Nawenge Village", caption = "HBCL RAP 2020", fill = "Energy Sources")+
          theme_void()+
          theme(plot.title = element_text(hjust = 0.5, size = 16, face = "bold", vjust = -5))+
          theme(legend.title = element_text(face = "bold"))+
          theme(plot.caption = element_text(size = 5, vjust = 35, hjust = 0.5))
        
      
      # MODE OD COMPENSATION PAYMENT -------------------------------
      df_payment <- df %>% 
        select(Village, Mode.of.Compersation) %>%
        mutate(Mode.of.Compersation = ifelse(Mode.of.Compersation == "", NA, Mode.of.Compersation)) %>% 
        group_by(Village, Mode.of.Compersation) %>% 
        summarise(Payment_Mode = n()) %>% 
        filter(!is.na(Mode.of.Compersation)) %>% 
        mutate(Payment_Mode_Per = round(Payment_Mode/sum(Payment_Mode)*100, digits = 2)) %>% 
        view()
      
      
      # Kisewe Village Compesation payment mode Pie Chart Plot
      df_Kisewe <- df_payment %>% 
        filter(Village == "Kisewe")
      ggplot(data = df_Kisewe, aes(x = "", y = Payment_Mode_Per, fill = Mode.of.Compersation))+
        geom_bar(stat = "identity")+
        coord_polar("y", start = 0)+
        geom_text(aes(label = str_c(Payment_Mode_Per, "%")), position = position_stack(vjust = 0.5), size = 5, colour = "white")+
        labs(title = "Household Prefered Compensation Mode of Payment - Kisewe Village", caption = "HBCL RAP 2020", fill = "Mode of Payment")+
        theme_void()+
        theme(plot.title = element_text(hjust = 0.5, size = 16, face = "bold", vjust = -5))+
        theme(legend.title = element_text(face = "bold"))+
        theme(plot.caption = element_text(size = 5, vjust = 35, hjust = 0.5))
      
      # Makanga Village Compesation payment mode Pie Chart Plot
      df_Makanga <- df_payment %>% 
        filter(Village == "Makanga")
        ggplot(data = df_Makanga, aes(x = "", y = Payment_Mode_Per, fill = Mode.of.Compersation))+
        geom_bar(stat = "identity")+
        coord_polar("y", start = 0)+
        geom_text(aes(label = str_c(Payment_Mode_Per, "%")), position = position_stack(vjust = 0.5), size = 5, colour = "white")+
        labs(title = "Household Prefered Compensation Mode of Payment - Makanga Village", caption = "HBCL RAP 2020", fill = "Mode of Payment")+
        theme_void()+
        theme(plot.title = element_text(hjust = 0.5, size = 16, face = "bold", vjust = -5))+
        theme(legend.title = element_text(face = "bold"))+
        theme(plot.caption = element_text(size = 5, vjust = 35, hjust = 0.5))
        
      # Mdindo Village Compesation payment mode Pie Chart Plot
        df_Mdindo <- df_payment %>% 
          filter(Village == "Mdindo")
        ggplot(data = df_Mdindo, aes(x = "", y = Payment_Mode_Per, fill = Mode.of.Compersation))+
          geom_bar(stat = "identity")+
          coord_polar("y", start = 0)+
          geom_text(aes(label = str_c(Payment_Mode_Per, "%")), position = position_stack(vjust = 0.5), size = 5, colour = "white")+
          labs(title = "Household Prefered Compensation Mode of Payment - Mdindo Village", caption = "HBCL RAP 2020", fill = "Mode of Payment")+
          theme_void()+
          theme(plot.title = element_text(hjust = 0.5, size = 16, face = "bold", vjust = -5))+
          theme(legend.title = element_text(face = "bold"))+
          theme(plot.caption = element_text(size = 5, vjust = 35, hjust = 0.5))
        
       
        # Nawenge Village Compesation payment mode Pie Chart Plot 
        df_Nawenge <- df_payment %>% 
          filter(Village == "Nawenge")
        ggplot(data = df_Nawenge, aes(x = "", y = Payment_Mode_Per, fill = Mode.of.Compersation))+
          geom_bar(stat = "identity")+
          coord_polar("y", start = 0)+
          geom_text(aes(label = str_c(Payment_Mode_Per, "%")), position = position_stack(vjust = 0.5),vjust = -8, size = 5, colour = "white")+
          labs(title = "Household Prefered Compensation Mode of Payment - Nawenge Village", caption = "HBCL RAP 2020", fill = "Mode of Payment")+
          theme_void()+
          theme(plot.title = element_text(hjust = 0.5, size = 16, face = "bold", vjust = -5))+
          theme(legend.title = element_text(face = "bold"))+
          theme(plot.caption = element_text(size = 5, vjust = 35, hjust = 0.5))
      
        
        # HEAS HOUSEHOLD HEALTH VULNERABILITY ----------------------
        df_Health <- df %>% 
          select(Village,Chron_sickness, Sickness) %>% 
          filter(Chron_sickness == "Yes") %>% 
          mutate(Sickness = ifelse(str_detect(Sickness, pattern = "epilepcy")|str_detect(Sickness, pattern = "eclipsy")|str_detect(Sickness, pattern = "Epilepsy")|
                                     str_detect(Sickness, pattern = "epilepsy")|str_detect(Sickness, pattern = "eclipy")|str_detect(Sickness, pattern = "epileptic")|
                                     str_detect(Sickness, pattern = "Kifafa"),"Epilepsy", Sickness),
                 Sickness = ifelse(str_detect(Sickness, pattern = "Mental sickness")|str_detect(Sickness, pattern = "Mental illness")|
                                     str_detect(Sickness, pattern = "Mental sickness")|str_detect(Sickness, pattern = "Has mental health issues"), "Mental Sickness", Sickness),
                 Sickness = ifelse(str_detect(Sickness, pattern = "pressure")|str_detect(Sickness, pattern = "Heart")|str_detect(Sickness, pattern = "Pressure "), "Blood Pressure", Sickness),
                 Sickness = ifelse(str_detect(Sickness, pattern = "aged")|str_detect(Sickness, pattern = "Old age")|
                                     str_detect(Sickness, pattern = "Aged")|str_detect(Sickness, pattern = "Aging")|str_detect(Sickness, pattern = "elderly"), "Old Age", Sickness),
                 Sickness = ifelse(str_detect(Sickness, pattern = "Blindness")|str_detect(Sickness, pattern = "blind")|str_detect(Sickness, pattern = "Seeing")|
                                     str_detect(Sickness, pattern = "Eyesight weakness")|str_detect(Sickness, pattern = "Low vision"),"Blindness", Sickness),
                 Sickness = ifelse(str_detect(Sickness, pattern = "rippled")|str_detect(Sickness, pattern = "Legs muscles")|str_detect(Sickness, pattern = "Fingers")|
                                     str_detect(Sickness, pattern = "back pain")|str_detect(Sickness, pattern = "Leg")|str_detect(Sickness, pattern = "legs")|
                                     str_detect(Sickness, pattern = "Weekness of born"), "Crippled", Sickness),
                 Sickness = ifelse(str_detect(Sickness, pattern = "deaf")|str_detect(Sickness, pattern = "Deaf")|str_detect(Sickness, pattern = "Earling problems"), "Deaf", Sickness),
                 Sickness = ifelse(str_detect(Sickness, pattern = "Dumb"), "Dumb", Sickness),
                 Sickness = ifelse(str_detect(Sickness, pattern = "cancer"), "Cancer", Sickness),
                 Sickness = ifelse(str_detect(Sickness, pattern = "intesti-l"), "Intestinal Problem", Sickness),
                 Sickness = ifelse(str_detect(Sickness, pattern = "iver"), "Liver Disease", Sickness),
                 Sickness = ifelse(str_detect(Sickness, pattern = "hernia"), "Hernia", Sickness))%>% 
          group_by(Village, Sickness) %>% 
          summarise(SicknessCount = n()) %>% 
          mutate(Percentages = round(SicknessCount/sum(SicknessCount)*100, digits = 2)) %>%
          view()
        
        # HEAD OF HOUSEHOLD HEALTH VULNERABILITY
        # Kisewe Head of Household Health
        df_Health %>% 
          filter(Village == "Kisewe") %>% 
          ggplot(aes(x = "", y = Percentages, fill = Sickness))+
          geom_bar(stat = "identity")+
          coord_polar("y", start = 0)+
          geom_text(aes(label = str_c(Percentages, "%")), position = position_stack(vjust = 0.5))+
          labs(title = "Head of Household Health Status- Kisewe Village", caption = "HBCL RAP 2020", fill = "Chronic Sickness")+
          theme_void()+
          theme(plot.title = element_text(size = 15, vjust = -8, hjust = 0.5, face = "bold"))+
          theme(plot.caption = element_text(size = 5, vjust = 25, hjust = 0.5))+
          theme(legend.title = element_text(face = "bold"))
        
        
        # Makanga Head of Household Health
        df_Health %>% 
          filter(Village == "Makanga") %>% 
          ggplot(aes(x = "", y = Percentages, fill = Sickness))+
          geom_bar(stat = "identity")+
          coord_polar("y", start = 0)+
          geom_text(aes(label = str_c(Percentages, "%")), position = position_stack(vjust = 0.5))+
          labs(title = "Head of Household Health Status - Makanga Village", caption = "HBCL RAP 2020", fill = "Chronic Sickness")+
          theme_void()+
          theme(plot.title = element_text(size = 15, vjust = -8, hjust = 0.5, face = "bold"))+
          theme(plot.caption = element_text(size = 5, vjust = 25, hjust = 0.5))+
          theme(legend.title = element_text(face = "bold"))
        
        # Mdindo Head of Household Health
        df_Health %>% 
          filter(Village == "Mdindo") %>% 
          ggplot(aes(x = "", y = Percentages, fill = Sickness))+
          geom_bar(stat = "identity")+
          coord_polar("y", start = 0)+
          geom_text(aes(label = str_c(Percentages, "%")), position = position_stack(vjust = 0.5))+
          labs(title = "Head of Household Health Status - Mdindo Village", caption = "HBCL RAP 2020", fill = "Chronic Sickness")+
          theme_void()+
          theme(plot.title = element_text(size = 15, vjust = -8, hjust = 0.5, face = "bold"))+
          theme(plot.caption = element_text(size = 5, vjust = 25, hjust = 0.5))+
          theme(legend.title = element_text(face = "bold"))
        
        # Nawenge Head of Household Health
        df_Health %>% 
          filter(Village == "Nawenge") %>% 
          ggplot(aes(x = "", y = Percentages, fill = Sickness))+
          geom_bar(stat = "identity")+
          coord_polar("y", start = 0)+
          geom_text(aes(label = str_c(Percentages, "%")), position = position_stack(vjust = 0.5), vjust = -15)+
          labs(title = "Head of Household Health Status - Nawenge Village", caption = "HBCL RAP 2020", fill = "Chronic Sickness")+
          theme_void()+
          theme(plot.title = element_text(size = 15, vjust = -8, hjust = 0.5, face = "bold"))+
          theme(plot.caption = element_text(size = 5, vjust = 25, hjust = 0.5))+
          theme(legend.title = element_text(face = "bold"))
       
        
        # FAMILY MEMBER HEALTH VULNERABILITY
        df_HealthMember <- df %>% 
          select(Village,Chron_sickness_Other_member,Handicap) %>% 
          filter(Chron_sickness_Other_member == "Yes") %>%
          mutate(Handicap = ifelse(str_detect(Handicap, pattern = "epilepcy")|str_detect(Handicap, pattern = "eclipsy")|
                                     str_detect(Handicap, pattern = "Epileptic")|str_detect(Handicap, pattern = "epilepsy")|
                                     str_detect(Handicap, pattern = "Epilepsy")|str_detect(Handicap, pattern = "Eliclpsy")|
                                     str_detect(Handicap, pattern = "His sons has being experiencing  kifafa")|str_detect(Handicap, pattern = "Seizures seaso-lly " ),"Epilepsy",Handicap),
                 Handicap = ifelse(str_detect(Handicap, pattern = "Mental sickness")|str_detect(Handicap, pattern = "Madness ")|
                                     str_detect(Handicap, pattern = "Craziness")|str_detect(Handicap, pattern = "Kaka yake ha- akili vizuri")|
                                     str_detect(Handicap, pattern = "Mentally ill daughter. "), "Mental Sicknes", Handicap),
                 Handicap = ifelse(str_detect(Handicap, pattern = "Blood pressure")|str_detect(Handicap, pattern = "Wife has heart problem ")|
                                     str_detect(Handicap, pattern = "Stroke"), "Blood Pressure", Handicap),
                 Handicap = ifelse(str_detect(Handicap, pattern = "rippled")|str_detect(Handicap, pattern = "Their daughter  has health issue problems")|
                                     str_detect(Handicap, pattern = "shorter than normal")|str_detect(Handicap, pattern = "Wife with weak legs")|
                                     str_detect(Handicap, pattern = "He fall down ")|str_detect(Handicap, pattern = "Legs illness")|
                                     str_detect(Handicap, pattern = "Handicaped hand,"), "Crippled", Handicap),
                 Handicap = ifelse(str_detect(Handicap, pattern = "Blindness ")|str_detect(Handicap, pattern = "blind")|
                                     str_detect(Handicap, pattern = "Short sightness"), "Blindness", Handicap),
                 Handicap = ifelse(str_detect(Handicap, pattern = "Old aged" ), "Old Age", Handicap),
                 Handicap = ifelse(str_detect(Handicap, pattern = "dumb"), "Dumb", Handicap),
                 Handicap = ifelse(str_detect(Handicap, pattern = "flaria"), "Flaria", Handicap),
                 Handicap = ifelse(str_detect(Handicap, pattern = "sicklecell"), "Sickle Cell", Handicap),
                 Handicap = ifelse(str_detect(Handicap, pattern = "cancerous."), "Cancer", Handicap),
                 Handicap = ifelse(str_detect(Handicap, pattern = "Skin infection "), "Skin Infection", Handicap)) %>% 
          group_by(Village, Handicap) %>% 
          summarise(HandicapCount = n()) %>% 
          mutate(Percentages = round(HandicapCount/sum(HandicapCount)*100, digits = 2)) %>% 
          view()
        
        
       
        # Kisewe Health
        df_HealthMember %>% 
          filter(Village == "Kisewe") %>% 
          ggplot(aes(x = "", y = Percentages, fill = Handicap))+
          geom_bar(stat = "identity")+
          coord_polar("y", start = 0)+
          geom_text(aes(label = str_c(Percentages, "%")), position = position_stack(vjust = 0.5))+
          labs(title = "Family Members Health Status - Kisewe Village", caption = "HBCL RAP 2020", fill = "Chronic Sickness")+
          theme_void()+
          theme(plot.title = element_text(size = 15, vjust = -8, hjust = 0.5, face = "bold"))+
          theme(plot.caption = element_text(size = 5, vjust = 25, hjust = 0.5))+
          theme(legend.title = element_text(face = "bold"))
        
        
        # Makanga Health
        df_HealthMember %>% 
          filter(Village == "Makanga") %>% 
          ggplot(aes(x = "", y = Percentages, fill = Handicap))+
          geom_bar(stat = "identity")+
          coord_polar("y", start = 0)+
          geom_text(aes(label = str_c(Percentages, "%")), position = position_stack(vjust = 0.5))+
          labs(title = "Family Members Health Status - Makanga Village", caption = "HBCL RAP 2020", fill = "Chronic Sickness")+
          theme_void()+
          theme(plot.title = element_text(size = 15, vjust = -8, hjust = 0.5, face = "bold"))+
          theme(plot.caption = element_text(size = 5, vjust = 25, hjust = 0.5))+
          theme(legend.title = element_text(face = "bold"))
        
        # Mdindo Health
        df_HealthMember %>% 
          filter(Village == "Mdindo") %>% 
          ggplot(aes(x = "", y = Percentages, fill = Handicap))+
          geom_bar(stat = "identity")+
          coord_polar("y", start = 0)+
          geom_text(aes(label = str_c(Percentages, "%")), position = position_stack(vjust = 0.5))+
          labs(title = "Family Members Health Status - Mdindo Village", caption = "HBCL RAP 2020", fill = "Chronic Sickness")+
          theme_void()+
          theme(plot.title = element_text(size = 15, vjust = -8, hjust = 0.5, face = "bold"))+
          theme(plot.caption = element_text(size = 5, vjust = 25, hjust = 0.5))+
          theme(legend.title = element_text(face = "bold"))
        
        # Nawenge Health
        df_HealthMember %>% 
          filter(Village == "Nawenge") %>% 
          ggplot(aes(x = "", y = Percentages, fill = Handicap))+
          geom_bar(stat = "identity")+
          coord_polar("y", start = 0)+
          geom_text(aes(label = str_c(Percentages, "%")), position = position_stack(vjust = 0.5), vjust = -15)+
          labs(title = "Family Members Health Status - Nawenge Village", caption = "HBCL RAP 2020", fill = "Chronic Sickness")+
          theme_void()+
          theme(plot.title = element_text(size = 15, vjust = -8, hjust = 0.5, face = "bold"))+
          theme(plot.caption = element_text(size = 5, vjust = 25, hjust = 0.5))+
          theme(legend.title = element_text(face = "bold"))
  
