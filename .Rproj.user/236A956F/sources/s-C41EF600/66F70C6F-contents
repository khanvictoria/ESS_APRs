X2019_07_08_APR_Dataset <- read_excel("C:/Users/vkhan/Desktop/2019_07_08_APR_Dataset.xlsx", 
                                     sheet = "Worksheet")
library(magrittr)
library(dplyr)
library(reshape2)

### Activity status across ESS categories 
X2019_07_08_APR_Dataset %>% 
  select(`Funding proposal number`, 
         `Total number of activities`:`Activity Completed`) -> Activity_status
substr(Activity_status$`Funding proposal number`, 1,5) -> Activity_status$FP
X2019_02_28_Approved_FPs %>% 
  rename(FP = FP_number) -> X2019_02_28_Approved_FPs
merge(Activity_status, X2019_02_28_Approved_FPs, by="FP") -> Activity_status

Activity_status %>% 
  select(FP, `Total number of activities`:`Activity Completed`, 
         Region, Access_modality, Focus, ESS_Categoty) -> Activity_status

Activity_status %>% 
mutate("Activity on track" = `Activity started-ahead of schedule` + `Activity started-progress on track`,
       "Activity delayed" = Activity_status$`Activity started but progress delayed` + Activity_status$`Activity start is delayed`) -> Activity_status 

Activity_status %>% 
    mutate(ESS_Category = case_when(ESS_Categoty == "Intermediation 2" ~ "Category B",
                                  ESS_Categoty == "Intermediation 3" ~ "Category C",
                                  TRUE ~ ESS_Categoty))  -> Activity_status

write.csv(Activity_status,"Activity status.csv")

Activity_status %>% 
  filter(Activity_status$Region == "Africa") -> Total
colSums(Total[ , 2:14])



### ESS challenges across ESS categories
X2019_07_08_APR_Dataset %>% 
  select(`Funding proposal number`, 
         starts_with("Impact of the")) -> ESS_challenges

substr(ESS_challenges$`Funding proposal number`, 1,5) -> ESS_challenges$FP
ESS_challenges[ESS_challenges == "NA"] = NA


  melt(ESS_challenges[, -1], id.vars = "FP", value.name = "Severity", na.rm = T ) -> ESS_challenges
# na.omit(ESS_challenges) -> ESS_challenges

merge(ESS_challenges, X2019_02_28_Approved_FPs, by="FP") -> ESS_challenges

ESS_challenges %>% 
  select(FP, variable, Severity, 
         Region, Access_modality, Focus, ESS_Categoty) -> ESS_challenges

ESS_challenges %>% 
  mutate(ESS_Category = case_when(ESS_Categoty == "Intermediation 2" ~ "Category B",
                                  ESS_Categoty == "Intermediation 3" ~ "Category C",
                                  TRUE ~ ESS_Categoty))  -> ESS_challenges
write.csv(ESS_challenges,"ESS_challenges.csv")

ESS_challenges %>% 
  na_if("NA") -> ESS_challenges

# Graph 3. Types of challenges and their number (count)
X2019_07_08_APR_Dataset %>% 
  select(`Funding proposal number`, 
         Legal_1, Financial_1, `Environmental/Social`, Implementation_1, Political_1, Procurement, Other ) -> ESS_challenges
# Changes during implementation

X2019_07_08_APR_Dataset %>% 
  select(`Funding proposal number`, `Project ESS category`,
         `Management changes of NDA`:Others) -> Changes

Changes[Changes == "NR"] = 0
substr(Changes$`Funding proposal number`, 1,5) -> Changes$FP

Changes %>% 
  mutate(ESS_Category = case_when(`Project ESS category` == "Intermediation 2" ~ "Category B",
                                  `Project ESS category` == "Intermediation 3" ~ "Category C",
                                  TRUE ~ `Project ESS category`))  -> Changes
Changes$ESS_Category[Changes$ESS_Category == "NA"] = "Category C"

write.csv(Changes,"Changes.csv")


